# table_utils.R — Table generation functions for EphiVIZ Shiny

#' Format n (%) string
fmt_pct <- function(n, N) {
  if (N == 0) return("0 (0.0%)")
  sprintf("%d (%.1f%%)", n, 100 * n / N)
}

#' Format mean (SD)
fmt_mean_sd <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return("—")
  sprintf("%.1f (%.2f)", mean(x), sd(x))
}

#' Format median (Q1, Q3)
fmt_med_q <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return("—")
  q <- quantile(x, c(.25, .75))
  sprintf("%.1f (%.1f, %.1f)", median(x), q[1], q[2])
}

#' Format min, max
fmt_min_max <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return("—")
  sprintf("%.1f, %.1f", min(x), max(x))
}

#' Apply population filter to dataset
apply_pop_filter <- function(df, pop_flag = "SAFFL") {
  if (is.null(df) || nrow(df) == 0) return(df)
  if (pop_flag == "ALL" || !pop_flag %in% names(df)) return(df)
  df[!is.na(df[[pop_flag]]) & df[[pop_flag]] == "Y", , drop = FALSE]
}

#' Get treatment arms + Total from dataset
get_arms <- function(df, trt_col = "TRTA") {
  if (!trt_col %in% names(df)) return(c("Total"))
  arms <- sort(unique(df[[trt_col]]))
  c(arms, "Total")
}

# ── 14.1.1 Demographics Table ────────────────────────────────────────────────

#' Generate Demographics summary table
#' @param adsl ADSL data.frame
#' @param pop_flag population flag column name
#' @return list(data=data.frame, caption=character, footnote=character)
gen_demographics <- function(adsl, pop_flag = "SAFFL") {
  df <- apply_pop_filter(adsl, pop_flag)
  if (is.null(df) || nrow(df) == 0) {
    return(list(data = data.frame(Characteristic = "No data available"),
                caption = "Table 14.1.1 Demographics",
                footnote = "No data."))
  }

  trt_col <- if ("TRTA" %in% names(df)) "TRTA" else if ("TRT01A" %in% names(df)) "TRT01A" else NULL
  arms <- if (!is.null(trt_col)) sort(unique(df[[trt_col]])) else character(0)
  all_arms <- c(arms, "Total")

  get_n <- function(sub_df) nrow(sub_df)
  get_sub <- function(arm) {
    if (arm == "Total") df
    else if (!is.null(trt_col)) df[df[[trt_col]] == arm, , drop = FALSE]
    else df
  }

  Ns <- sapply(all_arms, function(a) get_n(get_sub(a)))

  rows <- list()

  # Header row: N
  hdr <- c("N", sapply(all_arms, function(a) as.character(Ns[[a]])))
  rows[[length(rows) + 1]] <- setNames(hdr, c("Characteristic", all_arms))

  # AGE
  rows[[length(rows) + 1]] <- setNames(c("Age (years)", rep("", length(all_arms))),
                                        c("Characteristic", all_arms))
  if ("AGE" %in% names(df)) {
    stats_age <- list(
      "  n"           = sapply(all_arms, function(a) sum(!is.na(get_sub(a)$AGE))),
      "  Mean (SD)"   = sapply(all_arms, function(a) fmt_mean_sd(get_sub(a)$AGE)),
      "  Median (Q1, Q3)" = sapply(all_arms, function(a) fmt_med_q(get_sub(a)$AGE)),
      "  Min, Max"    = sapply(all_arms, function(a) fmt_min_max(get_sub(a)$AGE))
    )
    for (nm in names(stats_age)) {
      rows[[length(rows) + 1]] <- setNames(c(nm, as.character(stats_age[[nm]])),
                                            c("Characteristic", all_arms))
    }
  }

  # SEX
  if ("SEX" %in% names(df)) {
    rows[[length(rows) + 1]] <- setNames(c("Sex, n (%)", rep("", length(all_arms))),
                                          c("Characteristic", all_arms))
    for (val in sort(unique(df$SEX))) {
      r <- c(paste0("  ", val),
             sapply(all_arms, function(a) {
               sub_df <- get_sub(a)
               fmt_pct(sum(sub_df$SEX == val, na.rm = TRUE), nrow(sub_df))
             }))
      rows[[length(rows) + 1]] <- setNames(r, c("Characteristic", all_arms))
    }
  }

  # RACE
  if ("RACE" %in% names(df)) {
    rows[[length(rows) + 1]] <- setNames(c("Race, n (%)", rep("", length(all_arms))),
                                          c("Characteristic", all_arms))
    for (val in sort(unique(df$RACE))) {
      r <- c(paste0("  ", val),
             sapply(all_arms, function(a) {
               sub_df <- get_sub(a)
               fmt_pct(sum(sub_df$RACE == val, na.rm = TRUE), nrow(sub_df))
             }))
      rows[[length(rows) + 1]] <- setNames(r, c("Characteristic", all_arms))
    }
  }

  # WEIGHTBL
  if ("WEIGHTBL" %in% names(df)) {
    rows[[length(rows) + 1]] <- setNames(c("Weight at Baseline (kg)", rep("", length(all_arms))),
                                          c("Characteristic", all_arms))
    stats_wt <- list(
      "  n"           = sapply(all_arms, function(a) sum(!is.na(get_sub(a)$WEIGHTBL))),
      "  Mean (SD)"   = sapply(all_arms, function(a) fmt_mean_sd(get_sub(a)$WEIGHTBL)),
      "  Median (Q1, Q3)" = sapply(all_arms, function(a) fmt_med_q(get_sub(a)$WEIGHTBL)),
      "  Min, Max"    = sapply(all_arms, function(a) fmt_min_max(get_sub(a)$WEIGHTBL))
    )
    for (nm in names(stats_wt)) {
      rows[[length(rows) + 1]] <- setNames(c(nm, as.character(stats_wt[[nm]])),
                                            c("Characteristic", all_arms))
    }
  }

  out <- do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
  list(
    data     = out,
    caption  = "Table 14.1.1 — Summary of Demographic and Baseline Characteristics",
    footnote = paste0("Abbreviations: n = number of subjects; SD = standard deviation; ",
                      "Q1 = first quartile; Q3 = third quartile. ",
                      "Population: ", pop_flag, "=Y.")
  )
}

# ── 14.3.1 AE Summary Table ──────────────────────────────────────────────────

gen_ae_summary <- function(adae, adsl, pop_flag = "SAFFL") {
  adsl_f <- apply_pop_filter(adsl, pop_flag)
  if (is.null(adsl_f) || nrow(adsl_f) == 0) {
    return(list(data = data.frame(Category = "No data"), caption = "AE Summary", footnote = ""))
  }

  trt_col <- if ("TRTA" %in% names(adsl_f)) "TRTA" else NULL
  arms    <- if (!is.null(trt_col)) sort(unique(adsl_f[[trt_col]])) else character(0)
  all_arms <- c(arms, "Total")

  Ns <- sapply(all_arms, function(a) {
    if (a == "Total") nrow(adsl_f)
    else sum(adsl_f[[trt_col]] == a, na.rm = TRUE)
  })

  # Subjects in ADAE
  ae_subjs <- unique(adae$USUBJID[adae$USUBJID %in% adsl_f$USUBJID])

  get_subj_count <- function(subjs, arm) {
    if (arm == "Total") {
      length(intersect(subjs, adsl_f$USUBJID))
    } else {
      arm_subjs <- adsl_f$USUBJID[adsl_f[[trt_col]] == arm]
      length(intersect(subjs, arm_subjs))
    }
  }

  categories <- list(
    "Any Adverse Event"              = unique(adae$USUBJID),
    "Any Serious Adverse Event"      = unique(adae$USUBJID[adae$AESER == "Y"]),
    "Any Grade >= 3 Event"           = unique(adae$USUBJID[!is.na(adae$AETOXGR) & adae$AETOXGR >= 3]),
    "AE Leading to Discontinuation"  = unique(adae$USUBJID[adae$AEACN == "DRUG WITHDRAWN"])
  )

  rows <- lapply(names(categories), function(nm) {
    subjs <- categories[[nm]]
    vals  <- sapply(all_arms, function(a) {
      n <- get_subj_count(subjs, a)
      N <- Ns[[a]]
      fmt_pct(n, N)
    })
    setNames(c(nm, vals), c("Category", all_arms))
  })

  out <- do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
  list(
    data     = out,
    caption  = "Table 14.3.1 — Summary of Adverse Events",
    footnote = paste0("n (%) = number and percentage of subjects with at least one event. ",
                      "Subjects counted once per category. Population: ", pop_flag, "=Y.")
  )
}

# ── 14.3.2 AE by SOC/PT ──────────────────────────────────────────────────────

gen_ae_by_soc <- function(adae, adsl, pop_flag = "SAFFL") {
  adsl_f <- apply_pop_filter(adsl, pop_flag)
  if (is.null(adsl_f) || nrow(adsl_f) == 0 || is.null(adae) || nrow(adae) == 0) {
    return(list(data = data.frame(SOC = "No data"), caption = "AE by SOC/PT", footnote = ""))
  }

  trt_col  <- if ("TRTA" %in% names(adsl_f)) "TRTA" else NULL
  arms     <- if (!is.null(trt_col)) sort(unique(adsl_f[[trt_col]])) else character(0)
  all_arms <- c(arms, "Total")
  Ns <- sapply(all_arms, function(a) {
    if (a == "Total") nrow(adsl_f)
    else sum(adsl_f[[trt_col]] == a, na.rm = TRUE)
  })

  adae_f <- adae[adae$USUBJID %in% adsl_f$USUBJID, , drop = FALSE]
  if (!is.null(trt_col) && "TRTA" %in% names(adsl_f)) {
    adae_f <- merge(adae_f, adsl_f[, c("USUBJID", trt_col)], by = "USUBJID", all.x = TRUE,
                    suffixes = c("", ".adsl"))
    if (paste0(trt_col, ".adsl") %in% names(adae_f)) {
      adae_f[[trt_col]] <- adae_f[[paste0(trt_col, ".adsl")]]
    }
  }

  count_subj <- function(sub_ae, arm) {
    if (arm == "Total") sub_ae
    else if (!is.null(trt_col)) sub_ae[sub_ae[[trt_col]] == arm, , drop = FALSE]
    else sub_ae
  }

  socs <- sort(unique(adae_f$AESOC))
  rows <- list()

  for (soc in socs) {
    soc_ae <- adae_f[adae_f$AESOC == soc, , drop = FALSE]
    soc_counts <- sapply(all_arms, function(a) {
      sub <- count_subj(soc_ae, a)
      n   <- length(unique(sub$USUBJID))
      fmt_pct(n, Ns[[a]])
    })
    rows[[length(rows) + 1]] <- setNames(c(soc, soc_counts), c("SOC / Preferred Term", all_arms))

    pts <- sort(unique(soc_ae$AEDECOD))
    for (pt in pts) {
      pt_ae <- soc_ae[soc_ae$AEDECOD == pt, , drop = FALSE]
      pt_counts <- sapply(all_arms, function(a) {
        sub <- count_subj(pt_ae, a)
        n   <- length(unique(sub$USUBJID))
        fmt_pct(n, Ns[[a]])
      })
      rows[[length(rows) + 1]] <- setNames(c(paste0("  ", pt), pt_counts),
                                            c("SOC / Preferred Term", all_arms))
    }
  }

  out <- do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
  list(
    data     = out,
    caption  = "Table 14.3.2 — Adverse Events by System Organ Class and Preferred Term",
    footnote = paste0("n (%) = number and percentage of subjects with at least one event. ",
                      "Sorted by SOC frequency (descending). Population: ", pop_flag, "=Y.")
  )
}

# ── 14.4.1 Lab Summary ───────────────────────────────────────────────────────

gen_lab_summary <- function(adlb, pop_flag = "SAFFL") {
  if (is.null(adlb) || nrow(adlb) == 0) {
    return(list(data = data.frame(Parameter = "No data"), caption = "Lab Summary", footnote = ""))
  }

  df <- if ("SAFFL" %in% names(adlb) && pop_flag != "ALL") {
    adlb[!is.na(adlb$SAFFL) & adlb$SAFFL == "Y", , drop = FALSE]
  } else adlb

  trt_col  <- if ("TRTA" %in% names(df)) "TRTA" else NULL
  arms     <- if (!is.null(trt_col)) sort(unique(df[[trt_col]])) else character(0)
  all_arms <- c(arms, "Total")

  params <- if ("PARAM" %in% names(df)) sort(unique(df$PARAM)) else character(0)
  visits <- if ("AVISIT" %in% names(df)) {
    if ("AVISITN" %in% names(df)) {
      vmap <- unique(df[, c("AVISIT", "AVISITN")])
      vmap <- vmap[order(vmap$AVISITN), ]
      vmap$AVISIT
    } else sort(unique(df$AVISIT))
  } else character(0)

  rows <- list()
  for (param in params) {
    for (vis in visits) {
      sub <- df[df$PARAM == param & df$AVISIT == vis, , drop = FALSE]
      stats <- sapply(all_arms, function(a) {
        s <- if (a == "Total") sub else sub[sub[[trt_col]] == a, , drop = FALSE]
        v <- s$AVAL
        v <- v[!is.na(v)]
        if (length(v) == 0) return("—")
        sprintf("%.2f (%.2f)", mean(v), sd(v))
      })
      rows[[length(rows) + 1]] <- setNames(
        c(param, vis, stats),
        c("Parameter", "Visit", all_arms)
      )
    }
  }

  if (length(rows) == 0) {
    return(list(data = data.frame(Parameter = "No data available"),
                caption = "Table 14.4.1 — Laboratory Parameters Summary",
                footnote = ""))
  }

  out <- do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
  list(
    data     = out,
    caption  = "Table 14.4.1 — Summary of Laboratory Parameters",
    footnote = "Values are Mean (SD). Population: Safety (SAFFL=Y)."
  )
}

# ── 14.2.1 Primary Endpoint ──────────────────────────────────────────────────

gen_primary_endpoint <- function(adeff, adsl, pop_flag = "ITTFL") {
  adsl_f <- apply_pop_filter(adsl, pop_flag)
  if (is.null(adsl_f) || nrow(adsl_f) == 0 || is.null(adeff) || nrow(adeff) == 0) {
    return(list(data = data.frame(Category = "No data"),
                caption = "Table 14.2.1 — Primary Endpoint",
                footnote = ""))
  }

  trt_col  <- if ("TRTA" %in% names(adsl_f)) "TRTA" else NULL
  arms     <- if (!is.null(trt_col)) sort(unique(adsl_f[[trt_col]])) else character(0)
  all_arms <- c(arms, "Total")

  Ns <- sapply(all_arms, function(a) {
    if (a == "Total") nrow(adsl_f)
    else sum(adsl_f[[trt_col]] == a, na.rm = TRUE)
  })

  eff_f <- adeff[adeff$USUBJID %in% adsl_f$USUBJID, , drop = FALSE]
  if (!is.null(trt_col)) {
    eff_f <- merge(eff_f, adsl_f[, c("USUBJID", trt_col)], by = "USUBJID", all.x = TRUE,
                   suffixes = c("", ".adsl"))
    if (paste0(trt_col, ".adsl") %in% names(eff_f)) {
      eff_f[[trt_col]] <- eff_f[[paste0(trt_col, ".adsl")]]
    }
  }

  resp_col <- if ("AVALC" %in% names(eff_f)) "AVALC" else NULL

  rows <- list()
  if (!is.null(resp_col)) {
    for (val in sort(unique(eff_f[[resp_col]]))) {
      counts <- sapply(all_arms, function(a) {
        sub <- if (a == "Total") eff_f else eff_f[eff_f[[trt_col]] == a, , drop = FALSE]
        n   <- sum(sub[[resp_col]] == val, na.rm = TRUE)
        N   <- Ns[[a]]
        ci  <- tryCatch({
          if (n > 0 && N > 0) {
            # Wilson score interval (handles edge proportions correctly)
            p  <- n / N
            z  <- 1.96
            centre <- (p + z^2/(2*N)) / (1 + z^2/N)
            margin <- z * sqrt(p*(1-p)/N + z^2/(4*N^2)) / (1 + z^2/N)
            lo <- max(0, centre - margin) * 100
            hi <- min(100, centre + margin) * 100
            sprintf("%s | 95%%CI: (%.1f%%, %.1f%%)", fmt_pct(n, N), lo, hi)
          } else fmt_pct(n, N)
        }, error = function(e) fmt_pct(n, N))
        ci
      })
      rows[[length(rows) + 1]] <- setNames(c(val, counts), c("Response", all_arms))
    }
  }

  if (length(rows) == 0) {
    return(list(data = data.frame(Category = "No data"),
                caption = "Table 14.2.1 — Primary Endpoint",
                footnote = ""))
  }

  out <- do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
  list(
    data     = out,
    caption  = "Table 14.2.1 — Primary Efficacy Endpoint: Overall Response Rate",
    footnote = paste0("n (%) with exact 95% Confidence Interval (Wilson). ",
                      "Population: ", pop_flag, "=Y.")
  )
}

#' Generate R code for the currently selected table
gen_table_rcode <- function(tbl_type, dataset, pop_flag) {
  sprintf(
'# EphiVIZ — Auto-generated R code
# Table: %s | Dataset: %s | Population: %s

library(dplyr)

# Load data
adsl <- load_adam_data("ADSL")
%s

# Apply population filter
pop_data <- adsl[adsl$%s == "Y", ]

# Generate table
result <- gen_%s(adsl = pop_data%s)',
    tbl_type, dataset, pop_flag,
    if (dataset != "ADSL") sprintf('%s <- load_adam_data("%s")', tolower(dataset), dataset) else "",
    pop_flag,
    switch(tbl_type,
      "14.1.1 — Demographics"      = "demographics",
      "14.3.1 — AE Summary"        = "ae_summary",
      "14.3.2 — AE by SOC/PT"      = "ae_by_soc",
      "14.4.1 — Lab Summary"       = "lab_summary",
      "14.2.1 — Primary Endpoint"  = "primary_endpoint",
      "demographics"),
    if (dataset != "ADSL") sprintf(", %s = %s", tolower(dataset), tolower(dataset)) else ""
  )
}
