# summary_utils.R — Summary Dashboard helpers for EphiVIZ Shiny

# EphiVIZ palette (consistent with figure_utils)
EPHIVIZ_PALETTE <- c("#f97316", "#2563eb", "#16a34a", "#8b5cf6", "#ec4899", "#14b8a6")

#' Overall summary statistics for overview cards
#' @param datasets named list of data.frames
#' @param selected_ds name of primary selected dataset
#' @return named list: n_subjects, n_records, n_variables, pct_missing
summary_overview <- function(datasets, selected_ds = "ADSL") {
  ds_list <- datasets[!sapply(datasets, is.null)]
  if (length(ds_list) == 0) {
    return(list(n_subjects = 0, n_records = 0, n_variables = 0, pct_missing = 0))
  }

  df <- if (!is.null(selected_ds) && selected_ds %in% names(ds_list)) {
    ds_list[[selected_ds]]
  } else ds_list[[1]]

  adsl <- if ("ADSL" %in% names(ds_list)) ds_list[["ADSL"]] else NULL
  n_subjects <- if (!is.null(adsl) && "USUBJID" %in% names(adsl)) {
    length(unique(adsl$USUBJID))
  } else if ("USUBJID" %in% names(df)) {
    length(unique(df$USUBJID))
  } else nrow(df)

  n_records   <- nrow(df)
  n_variables <- ncol(df)
  pct_missing <- if (n_records > 0 && n_variables > 0) {
    round(100 * sum(is.na(df)) / (n_records * n_variables), 1)
  } else 0

  list(
    n_subjects  = n_subjects,
    n_records   = n_records,
    n_variables = n_variables,
    pct_missing = pct_missing
  )
}

#' Variable distribution plot
#' @param df data.frame
#' @param col_name column to plot
#' @return ggplot object
plot_variable_dist <- function(df, col_name) {
  if (is.null(df) || !col_name %in% names(df)) {
    return(ggplot2::ggplot() +
      ggplot2::annotate("text", x = .5, y = .5, label = "Select a variable",
                        size = 5, colour = "#431407") +
      ggplot2::theme_void())
  }

  x    <- df[[col_name]]
  type <- col_type_category(x)
  trt_col <- if ("TRTA" %in% names(df)) "TRTA" else NULL

  if (type == "NUM") {
    plot_df <- data.frame(x = x, stringsAsFactors = FALSE)
    if (!is.null(trt_col)) plot_df$Group <- df[[trt_col]]

    p <- ggplot2::ggplot(plot_df, ggplot2::aes(
      x    = x,
      fill = if (!is.null(trt_col)) Group else "All",
      colour = if (!is.null(trt_col)) Group else "All"
    )) +
      ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                               alpha = .5, bins = 30, position = "identity") +
      ggplot2::geom_density(alpha = .2) +
      ggplot2::geom_vline(xintercept = mean(x, na.rm = TRUE),
                           linetype = "dashed", colour = "#431407", linewidth = .8) +
      ggplot2::geom_vline(xintercept = median(x, na.rm = TRUE),
                           linetype = "dotted", colour = "#2563eb", linewidth = .8) +
      ggplot2::scale_fill_manual(values = EPHIVIZ_PALETTE) +
      ggplot2::scale_colour_manual(values = EPHIVIZ_PALETTE) +
      ggplot2::labs(
        title    = paste("Distribution of", col_name),
        subtitle = sprintf("Mean = %.2f (dashed), Median = %.2f (dotted)",
                           mean(x, na.rm = TRUE), median(x, na.rm = TRUE)),
        x        = col_name,
        y        = "Density",
        fill     = "Treatment",
        colour   = "Treatment"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(colour = "#431407", face = "bold"),
        legend.position = "bottom",
        panel.grid.minor = ggplot2::element_blank()
      )
  } else if (type %in% c("CHR", "FLAG", "LOGI")) {
    tbl <- sort(table(x[!is.na(x)]), decreasing = TRUE)
    tbl <- tbl[seq_len(min(20, length(tbl)))]
    plot_df <- data.frame(Value = names(tbl), N = as.integer(tbl), stringsAsFactors = FALSE)
    plot_df$Value <- factor(plot_df$Value, levels = rev(plot_df$Value))

    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = N, y = Value)) +
      ggplot2::geom_col(fill = "#f97316", alpha = .85) +
      ggplot2::geom_text(ggplot2::aes(label = N), hjust = -.2, size = 3.5) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, .15))) +
      ggplot2::labs(
        title = paste("Frequency of", col_name),
        x     = "Count",
        y     = col_name
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(colour = "#431407", face = "bold"),
        panel.grid.minor = ggplot2::element_blank()
      )
  } else if (type == "DATE") {
    dates <- as.Date(x[!is.na(x)])
    plot_df <- data.frame(Date = dates)
    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = Date)) +
      ggplot2::geom_density(fill = "#f97316", alpha = .6, colour = "#431407") +
      ggplot2::labs(
        title = paste("Timeline Density of", col_name),
        x     = col_name,
        y     = "Density"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(colour = "#431407", face = "bold")
      )
  } else {
    p <- ggplot2::ggplot() +
      ggplot2::annotate("text", x = .5, y = .5, label = "Cannot plot this variable type",
                        size = 5) +
      ggplot2::theme_void()
  }
  p
}

#' Treatment arm overview bar charts
#' @param adsl ADSL data.frame
#' @return ggplot object
plot_treatment_overview <- function(adsl) {
  if (is.null(adsl) || nrow(adsl) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::annotate("text", x = .5, y = .5, label = "No ADSL data",
                        size = 5, colour = "#431407") +
      ggplot2::theme_void())
  }

  trt_col <- if ("TRTA" %in% names(adsl)) "TRTA" else NULL
  if (is.null(trt_col)) {
    return(ggplot2::ggplot() +
      ggplot2::annotate("text", x = .5, y = .5,
                        label = "No treatment column (TRTA) found",
                        size = 5) +
      ggplot2::theme_void())
  }

  vars_to_plot <- intersect(c("SEX", "RACE", "ETHNIC"), names(adsl))
  if (length(vars_to_plot) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::annotate("text", x = .5, y = .5,
                        label = "No demographic columns found",
                        size = 5) +
      ggplot2::theme_void())
  }

  # N per arm subtitle
  n_per_arm <- table(adsl[[trt_col]])
  subtitle_txt <- paste(
    paste0(names(n_per_arm), " (N=", n_per_arm, ")"),
    collapse = " | "
  )

  plot_list <- lapply(vars_to_plot, function(v) {
    cnt <- as.data.frame(table(adsl[[trt_col]], adsl[[v]]), stringsAsFactors = FALSE)
    names(cnt) <- c("Treatment", "Category", "N")
    ggplot2::ggplot(cnt, ggplot2::aes(x = Treatment, y = N, fill = Category)) +
      ggplot2::geom_col(position = "fill") +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::scale_fill_manual(values = EPHIVIZ_PALETTE) +
      ggplot2::labs(title = v, x = NULL, y = "Proportion", fill = v) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        plot.title       = ggplot2::element_text(colour = "#431407", face = "bold", size = 12),
        legend.position  = "right",
        axis.text.x      = ggplot2::element_text(angle = 20, hjust = 1),
        panel.grid.minor = ggplot2::element_blank()
      )
  })

  p <- if (requireNamespace("patchwork", quietly = TRUE)) {
    base_p <- Reduce(`+`, plot_list)
    base_p + patchwork::plot_annotation(
      title    = "Treatment Arm — Demographic Breakdown",
      subtitle = subtitle_txt,
      theme = ggplot2::theme(
        plot.title    = ggplot2::element_text(colour = "#431407", face = "bold", size = 13),
        plot.subtitle = ggplot2::element_text(colour = "#92400e", size = 11)
      )
    )
  } else {
    plot_list[[1]] + ggplot2::labs(title = "Treatment Arm Overview", subtitle = subtitle_txt)
  }
  p
}

#' Missing data heatmap
#' @param datasets named list of data.frames
#' @return ggplot object
plot_missing_heatmap <- function(datasets) {
  ds_list <- datasets[!sapply(datasets, is.null)]
  if (length(ds_list) == 0) {
    return(ggplot2::ggplot() +
      ggplot2::annotate("text", x = .5, y = .5, label = "No datasets loaded",
                        size = 5, colour = "#431407") +
      ggplot2::theme_void())
  }

  rows <- do.call(rbind, lapply(names(ds_list), function(dsn) {
    df   <- ds_list[[dsn]]
    cols <- names(df)
    data.frame(
      Dataset  = dsn,
      Variable = cols,
      Pct_Miss = sapply(cols, function(c) 100 * mean(is.na(df[[c]]))),
      stringsAsFactors = FALSE
    )
  }))

  ggplot2::ggplot(rows, ggplot2::aes(x = Variable, y = Dataset, fill = Pct_Miss)) +
    ggplot2::geom_tile(colour = "#fff", linewidth = .4) +
    ggplot2::geom_text(ggplot2::aes(
      label = ifelse(Pct_Miss > 0, paste0(round(Pct_Miss, 0), "%"), "")
    ), size = 2.5, colour = "#431407") +
    ggplot2::scale_fill_gradient(low = "#fff7ed", high = "#f97316",
                                  limits = c(0, 100), name = "% Missing") +
    ggplot2::labs(
      title = "Missing Data Profile",
      x     = "Variable",
      y     = "Dataset"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title   = ggplot2::element_text(colour = "#431407", face = "bold"),
      axis.text.x  = ggplot2::element_text(angle = 60, hjust = 1, size = 8),
      axis.text.y  = ggplot2::element_text(size = 10),
      panel.grid   = ggplot2::element_blank()
    )
}

#' Correlation matrix heatmap
#' @param df data.frame
#' @return ggplot object
plot_corr_matrix <- function(df) {
  if (is.null(df) || nrow(df) < 5) {
    return(ggplot2::ggplot() +
      ggplot2::annotate("text", x = .5, y = .5, label = "Insufficient data for correlation",
                        size = 5, colour = "#431407") +
      ggplot2::theme_void())
  }

  num_cols <- names(df)[sapply(df, is.numeric)]
  if (length(num_cols) < 2) {
    return(ggplot2::ggplot() +
      ggplot2::annotate("text", x = .5, y = .5,
                        label = "Need ≥ 2 numeric columns for correlation",
                        size = 5, colour = "#431407") +
      ggplot2::theme_void())
  }

  num_cols <- num_cols[seq_len(min(25, length(num_cols)))]
  corr_mat <- cor(df[, num_cols, drop = FALSE], use = "pairwise.complete.obs")

  # Melt correlation matrix to long format (vectorised)
  corr_long <- data.frame(
    Var1 = rep(rownames(corr_mat), times = ncol(corr_mat)),
    Var2 = rep(colnames(corr_mat), each  = nrow(corr_mat)),
    Corr = as.vector(corr_mat),
    stringsAsFactors = FALSE
  )

  show_labels <- length(num_cols) <= 20

  p <- ggplot2::ggplot(corr_long, ggplot2::aes(x = Var1, y = Var2, fill = Corr)) +
    ggplot2::geom_tile(colour = "#fff", linewidth = .4) +
    ggplot2::scale_fill_gradient2(
      low = "#1e3a8a", mid = "#ffffff", high = "#f97316",
      midpoint = 0, limits = c(-1, 1), name = "Pearson r"
    ) +
    ggplot2::labs(
      title = "Correlation Matrix (Pearson)",
      x     = NULL,
      y     = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title  = ggplot2::element_text(colour = "#431407", face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
      axis.text.y = ggplot2::element_text(size = 9),
      panel.grid  = ggplot2::element_blank()
    )

  if (show_labels) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = round(Corr, 2)),
      size = 3,
      colour = ifelse(abs(corr_long$Corr) > .6, "white", "#431407")
    )
  }
  p
}

#' Data quality flags
#' @param datasets named list of loaded data.frames
#' @return data.frame with columns: Check, Variable, Issue, Count, Pct, Severity
compute_dq_flags <- function(datasets) {
  ds_list <- datasets[!sapply(datasets, is.null)]
  if (length(ds_list) == 0) {
    return(data.frame(Check = "No datasets loaded", Variable = "", Issue = "",
                      Count = 0, Pct = "—", Severity = "⚠ Warning",
                      stringsAsFactors = FALSE))
  }

  flags <- list()

  adsl <- ds_list[["ADSL"]]
  adae <- ds_list[["ADAE"]]

  # 1. Duplicate USUBJIDs in ADSL
  if (!is.null(adsl) && "USUBJID" %in% names(adsl)) {
    dup <- sum(duplicated(adsl$USUBJID))
    flags[[length(flags) + 1]] <- data.frame(
      Check    = "Duplicate USUBJID",
      Variable = "USUBJID",
      Issue    = "Duplicated subject IDs in ADSL",
      Count    = dup,
      Pct      = paste0(round(100 * dup / nrow(adsl), 1), "%"),
      Severity = if (dup == 0) "✓ OK" else "✗ Error",
      stringsAsFactors = FALSE
    )
  }

  # 2. Subjects in ADAE not in ADSL
  if (!is.null(adae) && !is.null(adsl) &&
      "USUBJID" %in% names(adae) && "USUBJID" %in% names(adsl)) {
    orphan <- length(setdiff(unique(adae$USUBJID), unique(adsl$USUBJID)))
    flags[[length(flags) + 1]] <- data.frame(
      Check    = "ADAE subjects not in ADSL",
      Variable = "USUBJID",
      Issue    = "ADAE subjects missing from ADSL",
      Count    = orphan,
      Pct      = paste0(round(100 * orphan / length(unique(adae$USUBJID)), 1), "%"),
      Severity = if (orphan == 0) "✓ OK" else "⚠ Warning",
      stringsAsFactors = FALSE
    )
  }

  # 3. Missing SAFFL / ITTFL
  for (flag_col in c("SAFFL", "ITTFL")) {
    if (!is.null(adsl) && flag_col %in% names(adsl)) {
      n_miss <- sum(is.na(adsl[[flag_col]]) | adsl[[flag_col]] == "")
      flags[[length(flags) + 1]] <- data.frame(
        Check    = paste("Missing", flag_col),
        Variable = flag_col,
        Issue    = paste(flag_col, "is blank or NA"),
        Count    = n_miss,
        Pct      = paste0(round(100 * n_miss / nrow(adsl), 1), "%"),
        Severity = if (n_miss == 0) "✓ OK" else "⚠ Warning",
        stringsAsFactors = FALSE
      )
    }
  }

  # 4. AVAL < 0 in ADLB where unexpected
  adlb <- ds_list[["ADLB"]]
  if (!is.null(adlb) && "AVAL" %in% names(adlb)) {
    neg <- sum(adlb$AVAL < 0, na.rm = TRUE)
    flags[[length(flags) + 1]] <- data.frame(
      Check    = "Negative AVAL in ADLB",
      Variable = "AVAL",
      Issue    = "AVAL < 0 (unexpected for lab values)",
      Count    = neg,
      Pct      = paste0(round(100 * neg / nrow(adlb), 1), "%"),
      Severity = if (neg == 0) "✓ OK" else "⚠ Warning",
      stringsAsFactors = FALSE
    )
  }

  # 5. Date anomalies AENDT < ASTDT
  if (!is.null(adae) && "ASTDT" %in% names(adae) && "AENDT" %in% names(adae)) {
    date_err <- sum(as.Date(adae$AENDT) < as.Date(adae$ASTDT), na.rm = TRUE)
    flags[[length(flags) + 1]] <- data.frame(
      Check    = "AENDT before ASTDT",
      Variable = "ASTDT / AENDT",
      Issue    = "End date before start date",
      Count    = date_err,
      Pct      = paste0(round(100 * date_err / nrow(adae), 1), "%"),
      Severity = if (date_err == 0) "✓ OK" else "✗ Error",
      stringsAsFactors = FALSE
    )
  }

  if (length(flags) == 0) {
    return(data.frame(Check = "No checks performed", Variable = "", Issue = "",
                      Count = 0, Pct = "—", Severity = "⚠ Warning",
                      stringsAsFactors = FALSE))
  }
  do.call(rbind, flags)
}

#' Helper: column_type_category re-exported for use in summary_utils
col_type_category <- function(x) {
  if (is.logical(x))                                    return("LOGI")
  if (inherits(x, c("Date", "POSIXct", "POSIXlt")))    return("DATE")
  if (is.numeric(x))                                    return("NUM")
  vals <- unique(x[!is.na(x)])
  if (length(vals) <= 2 && all(vals %in% c("Y", "N", "", "0", "1"))) return("FLAG")
  return("CHR")
}
