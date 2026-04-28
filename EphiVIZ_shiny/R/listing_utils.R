# listing_utils.R — Listing generation functions for EphiVIZ Shiny

#' Generate a listing as a data.frame
#' All listings return list(data, caption, footnote)

# ── Listing 1: All Adverse Events ────────────────────────────────────────────

gen_listing_ae_all <- function(adae, adsl, pop_flag = "SAFFL") {
  adsl_f <- if (!is.null(adsl)) {
    if (pop_flag != "ALL" && pop_flag %in% names(adsl))
      adsl[!is.na(adsl[[pop_flag]]) & adsl[[pop_flag]] == "Y", , drop = FALSE]
    else adsl
  } else NULL

  if (is.null(adae) || nrow(adae) == 0) {
    return(list(data = data.frame(Note = "No AE data available"),
                caption = "Listing 16.2.1 — Listing of All Adverse Events",
                footnote = "No data loaded."))
  }

  df <- adae
  if (!is.null(adsl_f)) df <- df[df$USUBJID %in% adsl_f$USUBJID, , drop = FALSE]

  # Select display columns
  keep_cols <- intersect(
    c("USUBJID", "TRTA", "AESOC", "AEDECOD", "AESEV", "AESER", "AETOXGR",
      "AEACN", "ASTDT"),
    names(df)
  )
  out <- df[order(df$USUBJID), keep_cols, drop = FALSE]

  list(
    data     = out,
    caption  = "Listing 16.2.1 — Listing of All Adverse Events",
    footnote = paste0("Sorted by USUBJID. AESEV = severity; AESER = serious flag; ",
                      "AETOXGR = toxicity grade; AEACN = action taken. Population: ",
                      pop_flag, "=Y.")
  )
}

# ── Listing 2: Serious Adverse Events ────────────────────────────────────────

gen_listing_ae_serious <- function(adae, adsl, pop_flag = "SAFFL") {
  adsl_f <- if (!is.null(adsl)) {
    if (pop_flag != "ALL" && pop_flag %in% names(adsl))
      adsl[!is.na(adsl[[pop_flag]]) & adsl[[pop_flag]] == "Y", , drop = FALSE]
    else adsl
  } else NULL

  if (is.null(adae) || nrow(adae) == 0) {
    return(list(data = data.frame(Note = "No AE data available"),
                caption = "Listing 16.2.2 — Serious Adverse Events",
                footnote = "No data loaded."))
  }

  df <- adae
  if (!is.null(adsl_f)) df <- df[df$USUBJID %in% adsl_f$USUBJID, , drop = FALSE]
  df <- df[!is.na(df$AESER) & df$AESER == "Y", , drop = FALSE]

  keep_cols <- intersect(
    c("USUBJID", "TRTA", "AESOC", "AEDECOD", "AESEV", "AETOXGR", "AEACN", "ASTDT"),
    names(df)
  )
  out <- df[order(df$USUBJID), keep_cols, drop = FALSE]

  list(
    data     = out,
    caption  = "Listing 16.2.2 — Listing of Serious Adverse Events",
    footnote = "AESER=Y. Sorted by USUBJID."
  )
}

# ── Listing 3: Lab Abnormalities ─────────────────────────────────────────────

gen_listing_lab_abnorm <- function(adlb, pop_flag = "SAFFL") {
  if (is.null(adlb) || nrow(adlb) == 0) {
    return(list(data = data.frame(Note = "No lab data available"),
                caption = "Listing 16.3.1 — Laboratory Abnormalities",
                footnote = "No data loaded."))
  }

  df <- if (pop_flag != "ALL" && "SAFFL" %in% names(adlb))
    adlb[!is.na(adlb$SAFFL) & adlb$SAFFL == "Y", , drop = FALSE]
  else adlb

  # Flag high values (> 2x baseline — simplified)
  if ("AVAL" %in% names(df) && "PARAM" %in% names(df)) {
    # Simple filter: top 10% per param
    df <- do.call(rbind, lapply(split(df, df$PARAM), function(sub) {
      thresh <- quantile(sub$AVAL, .90, na.rm = TRUE)
      sub[!is.na(sub$AVAL) & sub$AVAL >= thresh, , drop = FALSE]
    }))
  }

  keep_cols <- intersect(
    c("USUBJID", "TRTA", "PARAM", "PARAMCD", "AVISIT", "AVAL", "SAFFL"),
    names(df)
  )
  out <- df[order(df$USUBJID, df$PARAM, df$AVISIT), keep_cols, drop = FALSE]

  list(
    data     = out,
    caption  = "Listing 16.3.1 — Listing of Notable Laboratory Abnormalities",
    footnote = "Values at or above the 90th percentile per parameter shown."
  )
}

# ── Listing 4: Concomitant Medications ───────────────────────────────────────

gen_listing_conmeds <- function(adcm, adsl, pop_flag = "SAFFL") {
  adsl_f <- if (!is.null(adsl)) {
    if (pop_flag != "ALL" && pop_flag %in% names(adsl))
      adsl[!is.na(adsl[[pop_flag]]) & adsl[[pop_flag]] == "Y", , drop = FALSE]
    else adsl
  } else NULL

  if (is.null(adcm) || nrow(adcm) == 0) {
    return(list(data = data.frame(Note = "No conmed data available"),
                caption = "Listing 16.4.1 — Concomitant Medications",
                footnote = "No data loaded."))
  }

  df <- adcm
  if (!is.null(adsl_f)) df <- df[df$USUBJID %in% adsl_f$USUBJID, , drop = FALSE]

  keep_cols <- intersect(
    c("USUBJID", "TRTA", "CMTRT", "CMDECOD", "CMSTDTC"),
    names(df)
  )
  out <- df[order(df$USUBJID), keep_cols, drop = FALSE]

  list(
    data     = out,
    caption  = "Listing 16.4.1 — Listing of Concomitant Medications",
    footnote = "Sorted by USUBJID. CMTRT = medication reported; CMDECOD = standardised term."
  )
}

# ── Listing 5: Subject Disposition ───────────────────────────────────────────

gen_listing_disposition <- function(adsl, pop_flag = "ALL") {
  if (is.null(adsl) || nrow(adsl) == 0) {
    return(list(data = data.frame(Note = "No subject data available"),
                caption = "Listing 16.1.1 — Subject Disposition",
                footnote = "No data loaded."))
  }

  df <- if (pop_flag != "ALL" && pop_flag %in% names(adsl))
    adsl[!is.na(adsl[[pop_flag]]) & adsl[[pop_flag]] == "Y", , drop = FALSE]
  else adsl

  keep_cols <- intersect(
    c("USUBJID", "TRTA", "SAFFL", "ITTFL", "DISCONFL", "DTHFL"),
    names(df)
  )
  out <- df[order(df$USUBJID), keep_cols, drop = FALSE]

  list(
    data     = out,
    caption  = "Listing 16.1.1 — Subject Disposition",
    footnote = "SAFFL = Safety; ITTFL = ITT; DISCONFL = discontinued; DTHFL = death."
  )
}

# ── Listing 6: Vital Signs ────────────────────────────────────────────────────

gen_listing_vs <- function(advs, pop_flag = "SAFFL") {
  if (is.null(advs) || nrow(advs) == 0) {
    return(list(data = data.frame(Note = "No vital signs data available"),
                caption = "Listing 16.5.1 — Vital Signs",
                footnote = "No data loaded."))
  }

  df <- if (pop_flag != "ALL" && "SAFFL" %in% names(advs))
    advs[!is.na(advs$SAFFL) & advs$SAFFL == "Y", , drop = FALSE]
  else advs

  keep_cols <- intersect(
    c("USUBJID", "TRTA", "PARAM", "PARAMCD", "AVISIT", "AVAL"),
    names(df)
  )
  out <- df[order(df$USUBJID, df$PARAM, df$AVISIT), keep_cols, drop = FALSE]

  list(
    data     = out,
    caption  = "Listing 16.5.1 — Listing of Vital Signs",
    footnote = "AVAL = analysis value. Sorted by USUBJID, Parameter, Visit."
  )
}

#' Generate R code for a listing
gen_listing_rcode <- function(lst_type, dataset, pop_flag) {
  sprintf(
'# EphiVIZ — Auto-generated R code
# Listing: %s | Dataset: %s | Population: %s

library(dplyr)
source("R/data_utils.R")
source("R/listing_utils.R")

# Load data
%s

# Generate listing
result <- gen_listing_%s(%s, pop_flag = "%s")
DT::datatable(result$data)',
    lst_type, dataset, pop_flag,
    paste0(tolower(dataset), ' <- load_adam_data("', dataset, '")'),
    switch(lst_type,
      "All Adverse Events"          = "ae_all",
      "Serious Adverse Events"      = "ae_serious",
      "Lab Abnormalities"           = "lab_abnorm",
      "Concomitant Medications"     = "conmeds",
      "Subject Disposition"         = "disposition",
      "Vital Signs"                 = "vs",
      "ae_all"),
    tolower(dataset),
    pop_flag
  )
}
