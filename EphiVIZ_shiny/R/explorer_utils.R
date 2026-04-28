# explorer_utils.R — Data Explorer helpers for EphiVIZ Shiny

#' Determine column type category
#' @return one of "NUM", "CHR", "DATE", "FLAG", "LOGI"
col_type_category <- function(x) {
  if (is.logical(x))                                    return("LOGI")
  if (inherits(x, c("Date", "POSIXct", "POSIXlt")))    return("DATE")
  if (is.numeric(x))                                    return("NUM")
  # Flag columns: 2 unique non-NA values that look like Y/N or 0/1
  vals <- unique(x[!is.na(x)])
  if (length(vals) <= 2 && all(vals %in% c("Y", "N", "", "0", "1"))) return("FLAG")
  return("CHR")
}

#' Build a summary of columns for a data.frame
#' @return data.frame with one row per column
build_col_summary <- function(df) {
  if (is.null(df) || ncol(df) == 0) return(data.frame())
  do.call(rbind, lapply(names(df), function(col) {
    x    <- df[[col]]
    type <- col_type_category(x)
    n    <- sum(!is.na(x))
    n_miss <- sum(is.na(x))
    n_uniq <- length(unique(x[!is.na(x)]))
    min_max_top <- if (type == "NUM") {
      paste0("Min: ", round(min(x, na.rm = TRUE), 3),
             " | Max: ", round(max(x, na.rm = TRUE), 3))
    } else {
      top <- names(sort(table(x), decreasing = TRUE))[seq_len(min(3, n_uniq))]
      paste(top, collapse = ", ")
    }
    data.frame(
      Variable      = col,
      Type          = type,
      n             = n,
      n_missing     = n_miss,
      pct_missing   = if (nrow(df) > 0) round(100 * n_miss / nrow(df), 1) else NA,
      n_unique      = n_uniq,
      Min_Max_Top   = min_max_top,
      stringsAsFactors = FALSE
    )
  }))
}

#' Apply a list of filter values to a data.frame
#' @param df data.frame to filter
#' @param filters named list; each element corresponds to a column name
#'   - For CHR/FLAG: character vector of selected values
#'   - For NUM: numeric vector of length 2 (min, max)
#'   - For DATE: Date vector of length 2 (start, end)
#' @return filtered data.frame
apply_explorer_filters <- function(df, filters) {
  if (is.null(df) || nrow(df) == 0 || length(filters) == 0) return(df)

  for (col in names(filters)) {
    if (!col %in% names(df)) next
    val <- filters[[col]]
    if (is.null(val) || length(val) == 0) next

    x    <- df[[col]]
    type <- col_type_category(x)

    keep <- switch(type,
      CHR  = ,
      FLAG = ,
      LOGI = x %in% val,
      NUM  = {
        if (length(val) == 2 && is.numeric(val)) {
          !is.na(x) & x >= val[1] & x <= val[2]
        } else rep(TRUE, nrow(df))
      },
      DATE = {
        if (length(val) == 2) {
          d <- as.Date(x)
          !is.na(d) & d >= as.Date(val[1]) & d <= as.Date(val[2])
        } else rep(TRUE, nrow(df))
      },
      rep(TRUE, nrow(df))
    )
    keep[is.na(keep)] <- FALSE
    df <- df[keep, , drop = FALSE]
  }
  df
}

#' Count how many filters are non-default (active)
#' @param filters named list of filter values
#' @param df original data.frame (used to check defaults)
count_active_filters <- function(filters, df) {
  if (is.null(filters) || length(filters) == 0) return(0)
  n <- 0
  for (col in names(filters)) {
    if (!col %in% names(df)) next
    val  <- filters[[col]]
    x    <- df[[col]]
    type <- col_type_category(x)

    active <- switch(type,
      CHR  = ,
      FLAG = ,
      LOGI = {
        all_vals <- unique(x[!is.na(x)])
        !setequal(val, all_vals)
      },
      NUM  = {
        if (length(val) == 2) {
          val[1] > min(x, na.rm = TRUE) || val[2] < max(x, na.rm = TRUE)
        } else FALSE
      },
      DATE = FALSE,
      FALSE
    )
    if (isTRUE(active)) n <- n + 1
  }
  n
}

#' Build UI for a single filter widget (used inside renderUI)
#' @param col  column name
#' @param x    column vector
#' @param ns   namespace function
build_filter_widget_ui <- function(col, x, ns) {
  type <- col_type_category(x)
  input_id <- ns(paste0("flt_", col))
  note_id  <- ns(paste0("flt_note_", col))

  body <- switch(type,
    CHR  = ,
    FLAG = {
      vals <- sort(unique(x[!is.na(x)]))
      if (length(vals) > 20) {
        shiny::textInput(input_id, label = NULL, placeholder = "Type to filter (comma-separated)")
      } else {
        shiny::checkboxGroupInput(input_id, label = NULL, choices = vals, selected = vals,
                                  inline = FALSE)
      }
    },
    NUM  = {
      mn <- floor(min(x, na.rm = TRUE) * 10) / 10
      mx <- ceiling(max(x, na.rm = TRUE) * 10) / 10
      if (!is.finite(mn)) mn <- 0
      if (!is.finite(mx)) mx <- 1
      if (mn == mx) mx <- mn + 1
      shiny::sliderInput(input_id, label = NULL, min = mn, max = mx, value = c(mn, mx),
                         step = round((mx - mn) / 100, 3))
    },
    DATE = {
      dts <- as.Date(x[!is.na(x)])
      shiny::dateRangeInput(input_id, label = NULL,
                             start = min(dts), end = max(dts))
    },
    LOGI = {
      shiny::checkboxGroupInput(input_id, label = NULL, choices = c("TRUE", "FALSE"),
                                selected = c("TRUE", "FALSE"))
    },
    shiny::tags$span("—", class = "text-muted")
  )

  shiny::div(
    class = "filter-widget",
    shiny::div(
      class = "filter-widget-header",
      shiny::span(col),
      shiny::span(
        class = paste0("type-chip type-", tolower(type)),
        type
      )
    ),
    shiny::div(class = "filter-widget-body", body,
      shiny::div(class = "filter-note",
        shiny::textInput(note_id, label = "Note / comment", placeholder = "Rationale...")
      )
    )
  )
}
