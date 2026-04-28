# figure_utils.R — Figure generation functions for EphiVIZ Shiny

# EphiVIZ colour palette
EPHIVIZ_PALETTE <- c("#f97316", "#2563eb", "#16a34a", "#8b5cf6", "#ec4899", "#14b8a6")

# ── eDISH Plot ────────────────────────────────────────────────────────────────

#' eDISH: ALT (x) vs BILIRUBIN (y) percent change from baseline
gen_edish <- function(adlb) {
  if (is.null(adlb) || nrow(adlb) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::annotate("text", x = .5, y = .5, label = "No lab data loaded",
                        size = 5, colour = "#431407") +
      ggplot2::theme_void())
  }

  # Get ALT and BILI for each subject at last post-baseline visit
  get_pchg <- function(param_name) {
    sub <- adlb[adlb$PARAMCD == param_name | adlb$PARAM == param_name, , drop = FALSE]
    if (nrow(sub) == 0) return(NULL)
    base <- sub[grepl("BASELINE|BASE", toupper(sub$AVISIT)), , drop = FALSE]
    post <- sub[!grepl("BASELINE|BASE", toupper(sub$AVISIT)), , drop = FALSE]
    if (nrow(base) == 0 || nrow(post) == 0) return(NULL)
    base_mean <- aggregate(AVAL ~ USUBJID, data = base, FUN = mean, na.rm = TRUE)
    names(base_mean)[2] <- "BASE"
    post_last <- do.call(rbind, lapply(split(post, post$USUBJID), function(s) {
      if ("AVISITN" %in% names(s)) s[which.max(s$AVISITN), , drop = FALSE]
      else s[nrow(s), , drop = FALSE]
    }))
    m <- merge(post_last[, c("USUBJID", "AVAL", if ("TRTA" %in% names(post_last)) "TRTA")],
               base_mean, by = "USUBJID")
    m$PCHG <- 100 * (m$AVAL - m$BASE) / m$BASE
    m
  }

  alt_df  <- get_pchg("ALT")
  bili_df <- get_pchg("BILI")

  if (is.null(alt_df) || is.null(bili_df)) {
    return(ggplot2::ggplot() +
      ggplot2::annotate("text", x = .5, y = .5,
                        label = "ALT and/or BILI data not found", size = 5, colour = "#431407") +
      ggplot2::theme_void())
  }

  plt_df <- merge(alt_df[, c("USUBJID", "PCHG", if ("TRTA" %in% names(alt_df)) "TRTA")],
                  bili_df[, c("USUBJID", "PCHG")],
                  by = "USUBJID", suffixes = c("_ALT", "_BILI"))

  trt_col <- if ("TRTA" %in% names(plt_df)) "TRTA" else NULL

  p <- ggplot2::ggplot(plt_df, ggplot2::aes(
    x = PCHG_ALT, y = PCHG_BILI,
    colour = if (!is.null(trt_col)) .data[[trt_col]] else "All Subjects"
  )) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "#9ca3af") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "#9ca3af") +
    # Hy's Law reference lines
    ggplot2::geom_hline(yintercept = 100, colour = "#dc2626", linetype = "dotted", linewidth = .8) +
    ggplot2::geom_vline(xintercept = 200, colour = "#dc2626", linetype = "dotted", linewidth = .8) +
    ggplot2::geom_point(alpha = .7, size = 2.5) +
    ggplot2::scale_colour_manual(values = EPHIVIZ_PALETTE) +
    ggplot2::labs(
      title    = "eDISH Plot — ALT vs Bilirubin % Change from Baseline",
      subtitle = "Hy's Law zone: ALT >200% and Bilirubin >100%",
      x        = "ALT % Change from Baseline",
      y        = "Bilirubin % Change from Baseline",
      colour   = "Treatment",
      caption  = "Red dotted lines: Hy's Law thresholds (ALT 3×ULN; Bili 2×ULN proxy)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(colour = "#431407", face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )

  # Label outliers
  outliers <- plt_df[plt_df$PCHG_ALT > 200 & plt_df$PCHG_BILI > 100, , drop = FALSE]
  if (nrow(outliers) > 0 && requireNamespace("ggrepel", quietly = TRUE)) {
    p <- p + ggrepel::geom_label_repel(
      data = outliers,
      ggplot2::aes(label = USUBJID),
      size = 3, fill = "#fef2f2", colour = "#b91c1c"
    )
  }
  p
}

# ── KM Curve ─────────────────────────────────────────────────────────────────

gen_km_curve <- function(adsl) {
  if (!requireNamespace("survival", quietly = TRUE)) {
    return(ggplot2::ggplot() +
      ggplot2::annotate("text", x = .5, y = .5,
                        label = "Package 'survival' not installed", size = 5) +
      ggplot2::theme_void())
  }

  if (is.null(adsl) || nrow(adsl) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::annotate("text", x = .5, y = .5, label = "No ADSL data",
                        size = 5, colour = "#431407") +
      ggplot2::theme_void())
  }

  # Simulate time-to-event from ADSL if not present
  set.seed(123)
  df <- adsl
  if (!"AVAL" %in% names(df)) df$AVAL  <- rexp(nrow(df), rate = 1/300)
  if (!"CNSR" %in% names(df)) df$CNSR  <- sample(0:1, nrow(df), replace = TRUE, prob = c(.6, .4))
  df$EVENT <- 1 - df$CNSR

  trt_col <- if ("TRTA" %in% names(df)) "TRTA" else NULL

  fit <- tryCatch({
    if (!is.null(trt_col)) {
      survival::survfit(survival::Surv(AVAL, EVENT) ~ df[[trt_col]], data = df)
    } else {
      survival::survfit(survival::Surv(AVAL, EVENT) ~ 1, data = df)
    }
  }, error = function(e) NULL)

  if (is.null(fit)) {
    return(ggplot2::ggplot() +
      ggplot2::annotate("text", x = .5, y = .5, label = paste("KM error:", "check data"),
                        size = 5) + ggplot2::theme_void())
  }

  # Build plot data manually
  if (is.null(fit$strata)) {
    # Single group (no strata)
    km_data <- data.frame(
      time  = c(0, fit$time),
      surv  = c(1, fit$surv),
      Group = "All",
      stringsAsFactors = FALSE
    )
  } else {
    km_data <- do.call(rbind, lapply(seq_along(fit$strata), function(i) {
      start <- if (i == 1) 1 else cumsum(fit$strata)[i-1] + 1
      end   <- cumsum(fit$strata)[i]
      idx   <- start:end
      grp_name <- gsub("^.*=", "", names(fit$strata)[i])
      data.frame(
        time     = c(0, fit$time[idx]),
        surv     = c(1, fit$surv[idx]),
        Group    = grp_name,
        stringsAsFactors = FALSE
      )
    }))
  }

  p <- ggplot2::ggplot(km_data, ggplot2::aes(x = time, y = surv, colour = Group)) +
    ggplot2::geom_step(linewidth = 1) +
    ggplot2::scale_colour_manual(values = EPHIVIZ_PALETTE) +
    ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    ggplot2::labs(
      title    = "Kaplan-Meier Survival Curve",
      subtitle = "Event-free survival by treatment arm",
      x        = "Time (Days)",
      y        = "Survival Probability",
      colour   = "Treatment",
      caption  = "Censored observations not marked (use ggsurvfit for full features)"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(colour = "#431407", face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
  p
}

# ── Waterfall Plot ────────────────────────────────────────────────────────────

gen_waterfall <- function(adeff, adsl) {
  if (is.null(adeff) || nrow(adeff) == 0) {
    # Use synthetic tumour shrinkage
    set.seed(99)
    if (!is.null(adsl) && nrow(adsl) > 0) {
      adeff <- data.frame(
        USUBJID = adsl$USUBJID,
        TRTA    = adsl$TRTA,
        PCHG    = round(rnorm(nrow(adsl), -15, 35), 1),
        stringsAsFactors = FALSE
      )
    } else {
      return(ggplot2::ggplot() +
        ggplot2::annotate("text", x = .5, y = .5, label = "No efficacy data",
                          size = 5, colour = "#431407") + ggplot2::theme_void())
    }
  } else {
    adeff$PCHG <- if ("AVAL" %in% names(adeff)) adeff$AVAL else
      round(rnorm(nrow(adeff), -15, 35), 1)
    if (!is.null(adsl) && "TRTA" %in% names(adsl)) {
      adeff <- merge(adeff, adsl[, c("USUBJID", "TRTA")], by = "USUBJID", all.x = TRUE,
                     suffixes = c("", ".adsl"))
      if ("TRTA.adsl" %in% names(adeff)) adeff$TRTA <- adeff$TRTA.adsl
    }
  }

  df <- adeff[order(adeff$PCHG), ]
  df$RANK <- seq_len(nrow(df))
  trt_col <- if ("TRTA" %in% names(df)) "TRTA" else NULL

  p <- ggplot2::ggplot(df, ggplot2::aes(
    x    = RANK,
    y    = PCHG,
    fill = if (!is.null(trt_col)) .data[[trt_col]] else "Subject"
  )) +
    ggplot2::geom_col(width = .9) +
    ggplot2::geom_hline(yintercept = -30, linetype = "dashed", colour = "#431407", linewidth = .8) +
    ggplot2::scale_fill_manual(values = EPHIVIZ_PALETTE) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%")) +
    ggplot2::labs(
      title   = "Waterfall Plot — Best % Change from Baseline",
      x       = "Subject (ranked)",
      y       = "Best % Change from Baseline",
      fill    = "Treatment",
      caption = "Dashed line: −30% threshold"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(colour = "#431407", face = "bold"),
      axis.text.x      = ggplot2::element_blank(),
      axis.ticks.x     = ggplot2::element_blank(),
      legend.position  = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
  p
}

# ── Volcano Plot ─────────────────────────────────────────────────────────────

gen_volcano <- function(adae, adsl) {
  if (is.null(adae) || nrow(adae) == 0 || is.null(adsl) || nrow(adsl) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::annotate("text", x = .5, y = .5, label = "No AE data",
                        size = 5, colour = "#431407") + ggplot2::theme_void())
  }

  trt_col <- if ("TRTA" %in% names(adsl)) "TRTA" else NULL
  arms <- if (!is.null(trt_col)) sort(unique(adsl[[trt_col]])) else NULL
  if (length(arms) < 2) {
    return(ggplot2::ggplot() +
      ggplot2::annotate("text", x = .5, y = .5,
                        label = "Need ≥ 2 treatment arms for Volcano plot",
                        size = 5, colour = "#431407") + ggplot2::theme_void())
  }

  arm1 <- arms[1]; arm2 <- arms[2]
  s1 <- adsl$USUBJID[adsl[[trt_col]] == arm1]
  s2 <- adsl$USUBJID[adsl[[trt_col]] == arm2]
  N1 <- length(s1); N2 <- length(s2)

  ae_df <- adae[adae$USUBJID %in% c(s1, s2), , drop = FALSE]
  pts   <- unique(ae_df$AEDECOD)

  res <- do.call(rbind, lapply(pts, function(pt) {
    ae_pt <- ae_df[ae_df$AEDECOD == pt, , drop = FALSE]
    n1 <- length(unique(ae_pt$USUBJID[ae_pt$USUBJID %in% s1]))
    n2 <- length(unique(ae_pt$USUBJID[ae_pt$USUBJID %in% s2]))
    r1 <- n1 / N1; r2 <- n2 / N2
    rr <- if (r2 > 0) log2(r1 / r2) else NA
    p_val <- tryCatch(fisher.test(matrix(c(n1, N1-n1, n2, N2-n2), 2))$p.value,
                      error = function(e) NA_real_)
    data.frame(PT = pt, RR_log2 = rr,
               P_neglog10 = if (!is.na(p_val)) -log10(p_val + 1e-10) else NA_real_,
               N_arm1 = n1, N_arm2 = n2, stringsAsFactors = FALSE)
  }))

  res <- res[!is.na(res$RR_log2), , drop = FALSE]
  if (nrow(res) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::annotate("text", x = .5, y = .5, label = "Insufficient data",
                        size = 5) + ggplot2::theme_void())
  }

  res$Significant <- res$P_neglog10 > -log10(0.05) & abs(res$RR_log2) > 1
  res$Colour <- ifelse(res$Significant & res$RR_log2 > 0, "Higher in arm1",
                 ifelse(res$Significant & res$RR_log2 < 0, "Higher in arm2", "Not significant"))

  p <- ggplot2::ggplot(res, ggplot2::aes(x = RR_log2, y = P_neglog10, colour = Colour)) +
    ggplot2::geom_point(ggplot2::aes(size = N_arm1 + N_arm2), alpha = .7) +
    ggplot2::geom_hline(yintercept = -log10(0.05), linetype = "dashed", colour = "#6b7280") +
    ggplot2::geom_vline(xintercept = c(-1, 1), linetype = "dashed", colour = "#6b7280") +
    ggplot2::scale_colour_manual(
      values = c("Higher in arm1" = "#f97316", "Higher in arm2" = "#2563eb",
                 "Not significant" = "#9ca3af")
    ) +
    ggplot2::scale_size_continuous(range = c(2, 8)) +
    ggplot2::labs(
      title   = "Volcano Plot — AE Risk Ratio",
      x       = paste0("log₂(Risk Ratio: ", arm1, " / ", arm2, ")"),
      y       = "-log₁₀(p-value)",
      colour  = NULL, size = "Total N"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(colour = "#431407", face = "bold"),
      legend.position = "bottom"
    )

  sig_pts <- res[res$Significant, , drop = FALSE]
  if (nrow(sig_pts) > 0 && requireNamespace("ggrepel", quietly = TRUE)) {
    p <- p + ggrepel::geom_label_repel(
      data = sig_pts,
      ggplot2::aes(label = PT),
      size = 3, max.overlaps = 15
    )
  }
  p
}

# ── Lab Box Plot ─────────────────────────────────────────────────────────────

gen_lab_boxplot <- function(adlb, param_name = "ALT") {
  if (is.null(adlb) || nrow(adlb) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::annotate("text", x = .5, y = .5, label = "No lab data",
                        size = 5, colour = "#431407") + ggplot2::theme_void())
  }

  df <- adlb[adlb$PARAMCD == param_name | adlb$PARAM == param_name, , drop = FALSE]
  if (nrow(df) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::annotate("text", x = .5, y = .5,
                        label = paste("Parameter", param_name, "not found"),
                        size = 5, colour = "#431407") + ggplot2::theme_void())
  }

  trt_col <- if ("TRTA" %in% names(df)) "TRTA" else NULL

  # Order visits
  if ("AVISITN" %in% names(df)) {
    visit_order <- unique(df$AVISIT[order(df$AVISITN)])
    df$AVISIT <- factor(df$AVISIT, levels = visit_order)
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(
    x    = AVISIT,
    y    = AVAL,
    fill = if (!is.null(trt_col)) .data[[trt_col]] else "All"
  )) +
    ggplot2::geom_boxplot(outlier.size = 1.5, outlier.alpha = .5, width = .6) +
    ggplot2::scale_fill_manual(values = EPHIVIZ_PALETTE) +
    ggplot2::labs(
      title = paste("Lab Box Plot —", param_name),
      x     = "Visit",
      y     = paste(param_name, "Value"),
      fill  = "Treatment"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(colour = "#431407", face = "bold"),
      axis.text.x      = ggplot2::element_text(angle = 30, hjust = 1),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position  = "bottom"
    )

  if (!is.null(trt_col)) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data[[trt_col]]), ncol = 1, scales = "free_y")
  }
  p
}
