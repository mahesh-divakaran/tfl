# data_utils.R — Data loading utilities for EphiVIZ Shiny app

#' Load ADaM dataset from pharmaverseadam or generate synthetic fallback
#' @param ds_name Dataset name (e.g. "ADSL", "ADAE")
#' @return data.frame with UPPER CASE column names
load_adam_data <- function(ds_name) {
  if (requireNamespace("pharmaverseadam", quietly = TRUE)) {
    tryCatch({
      e <- new.env()
      data(list = tolower(ds_name), package = "pharmaverseadam", envir = e)
      df <- get(tolower(ds_name), envir = e)
      names(df) <- toupper(names(df))
      return(as.data.frame(df))
    }, error = function(err) {
      message("pharmaverseadam load failed for ", ds_name, ": ", err$message)
      return(generate_synthetic_adam(ds_name))
    })
  } else {
    return(generate_synthetic_adam(ds_name))
  }
}

#' Handle file upload for a dataset
#' @param file_info fileInput info list (name, datapath, type)
#' @return data.frame with UPPER CASE column names
handle_upload <- function(file_info) {
  if (is.null(file_info)) return(NULL)
  path <- file_info$datapath
  ext  <- tolower(tools::file_ext(file_info$name))
  df <- tryCatch({
    if (ext == "csv") {
      readr::read_csv(path, show_col_types = FALSE)
    } else if (ext %in% c("sas7bdat", "xpt")) {
      haven::read_sas(path)
    } else {
      stop("Unsupported file type: ", ext)
    }
  }, error = function(e) {
    stop("File read error: ", e$message)
  })
  df <- as.data.frame(df)
  names(df) <- toupper(names(df))
  df
}

#' Generate synthetic ADaM data for a given dataset name
#' @param ds_name One of ADSL, ADAE, ADLB, ADVS, ADCM, ADEFF
#' @return data.frame
generate_synthetic_adam <- function(ds_name) {
  set.seed(42)
  n_subj <- 254
  trta_vals <- c("Treatment A", "Treatment B", "Placebo")

  usubjids <- sprintf("STUDY-001-%04d", seq_len(n_subj))
  trta      <- rep(trta_vals, length.out = n_subj)
  trtpn     <- as.integer(factor(trta, levels = trta_vals))
  ages      <- round(rnorm(n_subj, 55, 12))
  ages      <- pmax(18, pmin(85, ages))
  sexes     <- sample(c("M", "F"), n_subj, replace = TRUE, prob = c(.52, .48))
  races     <- sample(c("WHITE", "BLACK OR AFRICAN AMERICAN", "ASIAN", "OTHER"),
                      n_subj, replace = TRUE, prob = c(.6, .2, .15, .05))
  ethnic    <- sample(c("NOT HISPANIC OR LATINO", "HISPANIC OR LATINO"),
                      n_subj, replace = TRUE, prob = c(.8, .2))
  saffl     <- sample(c("Y", "N"), n_subj, replace = TRUE, prob = c(.95, .05))
  ittfl     <- sample(c("Y", "N"), n_subj, replace = TRUE, prob = c(.93, .07))
  weightbl  <- round(rnorm(n_subj, 75, 14), 1)

  switch(toupper(ds_name),
    ADSL = data.frame(
      USUBJID = usubjids, STUDYID = "STUDY-001", SUBJID = sub("STUDY-001-", "", usubjids),
      TRTA = trta, TRTAN = trtpn,
      AGE = ages, SEX = sexes, RACE = races, ETHNIC = ethnic,
      SAFFL = saffl, ITTFL = ittfl, WEIGHTBL = weightbl,
      DTHFL = sample(c("Y", ""), n_subj, replace = TRUE, prob = c(.03, .97)),
      DISCONFL = sample(c("Y", ""), n_subj, replace = TRUE, prob = c(.15, .85)),
      stringsAsFactors = FALSE
    ),
    ADAE = {
      n_ae <- 1500
      ae_subjs <- sample(usubjids, n_ae, replace = TRUE)
      ae_trta  <- trta[match(ae_subjs, usubjids)]
      socs <- c("GASTROINTESTINAL DISORDERS", "NERVOUS SYSTEM DISORDERS",
                "INFECTIONS AND INFESTATIONS", "GENERAL DISORDERS",
                "SKIN AND SUBCUTANEOUS TISSUE DISORDERS")
      pts_by_soc <- list(
        "GASTROINTESTINAL DISORDERS" = c("NAUSEA", "DIARRHOEA", "VOMITING", "ABDOMINAL PAIN"),
        "NERVOUS SYSTEM DISORDERS"   = c("HEADACHE", "DIZZINESS", "FATIGUE"),
        "INFECTIONS AND INFESTATIONS"= c("NASOPHARYNGITIS", "UPPER RESPIRATORY TRACT INFECTION"),
        "GENERAL DISORDERS"          = c("PYREXIA", "OEDEMA PERIPHERAL", "ASTHENIA"),
        "SKIN AND SUBCUTANEOUS TISSUE DISORDERS" = c("RASH", "PRURITUS")
      )
      ae_soc <- sample(socs, n_ae, replace = TRUE)
      # Ensure only valid SOC keys are used
      ae_soc <- socs[match(ae_soc, socs)]
      ae_pt  <- sapply(ae_soc, function(s) {
        pts <- pts_by_soc[[s]]
        if (is.null(pts) || length(pts) == 0) return("UNKNOWN")
        sample(pts, 1)
      })
      data.frame(
        USUBJID = ae_subjs, STUDYID = "STUDY-001",
        TRTA = ae_trta, TRTAN = trtpn[match(ae_subjs, usubjids)],
        AESOC = ae_soc, AEDECOD = ae_pt,
        AESEV = sample(c("MILD", "MODERATE", "SEVERE"), n_ae, replace = TRUE, prob = c(.5, .35, .15)),
        AESER = sample(c("Y", "N"), n_ae, replace = TRUE, prob = c(.08, .92)),
        AETOXGR = sample(1:4, n_ae, replace = TRUE, prob = c(.4, .3, .2, .1)),
        AEACN  = sample(c("DOSE REDUCED", "DRUG WITHDRAWN", "NONE"), n_ae, replace = TRUE, prob = c(.1, .05, .85)),
        ASTDT  = as.Date("2020-01-01") + sample(0:365, n_ae, replace = TRUE),
        stringsAsFactors = FALSE
      )
    },
    ADLB = {
      params <- c("ALT", "AST", "BILI", "ALB", "CREAT", "GLUC", "HGB", "PLT", "WBC")
      visits  <- c("BASELINE", "WEEK 4", "WEEK 8", "WEEK 12", "WEEK 24", "END OF TREATMENT")
      rows <- expand.grid(USUBJID = usubjids, PARAM = params, AVISIT = visits,
                          stringsAsFactors = FALSE)
      n_lb <- nrow(rows)
      baseline_vals <- list(ALT=30, AST=28, BILI=0.6, ALB=4.2, CREAT=0.9,
                            GLUC=95, HGB=13.5, PLT=230, WBC=6.5)
      rows$AVAL <- mapply(function(p) {
        bv <- baseline_vals[[p]]
        round(rnorm(1, bv, bv * 0.15), 2)
      }, rows$PARAM)
      rows$TRTA   <- trta[match(rows$USUBJID, usubjids)]
      rows$TRTAN  <- trtpn[match(rows$USUBJID, usubjids)]
      rows$AVISITN <- as.integer(factor(rows$AVISIT,
                       levels = c("BASELINE","WEEK 4","WEEK 8","WEEK 12","WEEK 24","END OF TREATMENT")))
      rows$PARAMCD <- rows$PARAM
      rows$DTYPE   <- ""
      rows$SAFFL   <- saffl[match(rows$USUBJID, usubjids)]
      rows
    },
    ADVS = {
      params <- c("SYSBP", "DIABP", "WEIGHT", "HEIGHT", "BMI", "PULSE")
      visits  <- c("BASELINE", "WEEK 4", "WEEK 8", "WEEK 12")
      rows <- expand.grid(USUBJID = usubjids, PARAM = params, AVISIT = visits,
                          stringsAsFactors = FALSE)
      baseline_vals <- list(SYSBP=120, DIABP=78, WEIGHT=75, HEIGHT=170, BMI=26, PULSE=72)
      rows$AVAL <- mapply(function(p) {
        bv <- baseline_vals[[p]]
        round(rnorm(1, bv, bv * 0.1), 1)
      }, rows$PARAM)
      rows$TRTA  <- trta[match(rows$USUBJID, usubjids)]
      rows$TRTAN <- trtpn[match(rows$USUBJID, usubjids)]
      rows$PARAMCD <- rows$PARAM
      rows$SAFFL   <- saffl[match(rows$USUBJID, usubjids)]
      rows
    },
    ADCM = {
      n_cm <- 800
      cm_subjs <- sample(usubjids, n_cm, replace = TRUE)
      data.frame(
        USUBJID = cm_subjs, STUDYID = "STUDY-001",
        TRTA  = trta[match(cm_subjs, usubjids)],
        CMTRT = sample(c("ASPIRIN","METFORMIN","LISINOPRIL","ATORVASTATIN",
                          "OMEPRAZOLE","IBUPROFEN","PARACETAMOL"), n_cm, replace = TRUE),
        CMDECOD = sample(c("ACETYLSALICYLIC ACID","METFORMIN","LISINOPRIL",
                            "ATORVASTATIN","OMEPRAZOLE","IBUPROFEN","PARACETAMOL"),
                          n_cm, replace = TRUE),
        CMSTDTC = as.character(as.Date("2019-01-01") + sample(0:365, n_cm, replace = TRUE)),
        stringsAsFactors = FALSE
      )
    },
    # Default / ADEFF
    {
      data.frame(
        USUBJID = usubjids, STUDYID = "STUDY-001",
        TRTA = trta, TRTAN = trtpn,
        AVAL  = round(rnorm(n_subj, 50, 20), 1),
        AVALC = sample(c("RESPONDER", "NON-RESPONDER"), n_subj,
                        replace = TRUE, prob = c(.45, .55)),
        PARAMCD = "OVRESP", PARAM = "Overall Response",
        AVISIT = "END OF TREATMENT",
        stringsAsFactors = FALSE
      )
    }
  )
}
