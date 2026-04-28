# EphiVIZ v3 — Clinical TLF Builder
# A standalone R Shiny application
# Run with: shiny::runApp("EphiVIZ_shiny")

library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(DT)
library(scales)

# Provide `%||%` for R < 4.4
`%||%` <- function(x, y) if (length(x) > 0 && !is.null(x) && !all(is.na(x))) x else y

# Source utilities
source("R/data_utils.R")
source("R/table_utils.R")
source("R/listing_utils.R")
source("R/figure_utils.R")
source("R/explorer_utils.R")
source("R/summary_utils.R")

# ── Helper: styled DT datatable ──────────────────────────────────────────────
ephiviz_dt <- function(df, caption = NULL, ...) {
  DT::datatable(
    df,
    caption    = caption,
    rownames   = FALSE,
    escape     = FALSE,
    extensions = c("Buttons", "ColReorder", "Scroller"),
    options    = list(
      dom          = "Bfrtip",
      pageLength   = 25,
      lengthMenu   = list(c(25, 50, 100, -1), c("25", "50", "100", "All")),
      scrollX      = TRUE,
      colReorder   = TRUE,
      deferRender  = TRUE,
      scroller     = TRUE,
      buttons      = list(
        "copy",
        list(extend = "csv",   text = "CSV"),
        list(extend = "excel", text = "Excel"),
        list(extend = "pdf",   text = "PDF"),
        "colvis"
      ),
      initComplete = DT::JS(
        "function(settings, json) {",
        "  $(this.api().table().header()).css({'background-color':'#431407','color':'#ffffff'});",
        "}"
      )
    ),
    class = "stripe hover cell-border",
    ...
  ) |>
    DT::formatStyle(
      columns    = seq_len(ncol(df)),
      target     = "row",
      backgroundColor = DT::styleRow(
        which(seq_len(nrow(df)) %% 2 == 0), "#fff7ed"
      )
    )
}

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(rel = "stylesheet", href = "style.css"),
    tags$title("EphiVIZ v3 — Clinical TLF Builder")
  ),

  tags$div(id = "app",

    # ── Top Navigation ─────────────────────────────────────────────────────
    tags$div(class = "nav-bar",
      tags$span(class = "nav-logo", "⚕ EphiVIZ"),
      actionButton("nav_data",     "📂 Data",      class = "nav-btn active"),
      actionButton("nav_tables",   "📊 Tables",    class = "nav-btn"),
      actionButton("nav_listings", "📋 Listings",  class = "nav-btn"),
      actionButton("nav_figures",  "📈 Figures",   class = "nav-btn"),
      actionButton("nav_about",    "ℹ About",      class = "nav-btn")
    ),

    # ════════════════════════════════════════════════════════════════════════
    # SCREEN 1 — DATA
    # ════════════════════════════════════════════════════════════════════════
    tags$div(id = "screen_data", class = "screen active",
      style = "flex-direction: column;",

      # Sub-tab bar
      tags$div(class = "sub-tab-bar",
        actionButton("sub_loader",   "📂 Dataset Loader",  class = "sub-tab-btn active"),
        actionButton("sub_explorer", "🔍 Data Explorer",   class = "sub-tab-btn"),
        actionButton("sub_summary",  "📊 Summary Dashboard", class = "sub-tab-btn")
      ),

      # ── Sub-screen 1A: Dataset Loader ──────────────────────────────────
      tags$div(id = "sub_loader_screen", class = "sub-screen active",

        # Status bar
        tags$div(id = "load_status",
          uiOutput("load_status_ui")
        ),

        # DS Cards grid
        tags$div(class = "ds-grid",

          # ADSL
          tags$div(id = "card_adsl", class = "ds-card sel",
            tags$div(class = "ds-card-header",
              tags$span(class = "ds-name", "ADSL"),
              uiOutput("badge_adsl")
            ),
            tags$p(class = "ds-desc", "Subject-level analysis dataset (required)"),
            tags$div(class = "ds-actions",
              actionButton("load_adsl", "⬇ Load", class = "btn-sm primary"),
              fileInput("upload_adsl", NULL, accept = c(".csv",".sas7bdat",".xpt"),
                        buttonLabel = "📁 Upload", placeholder = "No file", width = "140px")
            )
          ),

          # ADAE
          tags$div(id = "card_adae", class = "ds-card",
            tags$div(class = "ds-card-header",
              tags$span(class = "ds-name", "ADAE"),
              uiOutput("badge_adae")
            ),
            tags$p(class = "ds-desc", "Adverse events analysis dataset"),
            tags$div(class = "ds-actions",
              actionButton("load_adae", "⬇ Load", class = "btn-sm primary"),
              fileInput("upload_adae", NULL, accept = c(".csv",".sas7bdat",".xpt"),
                        buttonLabel = "📁 Upload", placeholder = "No file", width = "140px")
            )
          ),

          # ADLB
          tags$div(id = "card_adlb", class = "ds-card",
            tags$div(class = "ds-card-header",
              tags$span(class = "ds-name", "ADLB"),
              uiOutput("badge_adlb")
            ),
            tags$p(class = "ds-desc", "Laboratory results analysis dataset"),
            tags$div(class = "ds-actions",
              actionButton("load_adlb", "⬇ Load", class = "btn-sm primary"),
              fileInput("upload_adlb", NULL, accept = c(".csv",".sas7bdat",".xpt"),
                        buttonLabel = "📁 Upload", placeholder = "No file", width = "140px")
            )
          ),

          # ADVS
          tags$div(id = "card_advs", class = "ds-card",
            tags$div(class = "ds-card-header",
              tags$span(class = "ds-name", "ADVS"),
              uiOutput("badge_advs")
            ),
            tags$p(class = "ds-desc", "Vital signs analysis dataset"),
            tags$div(class = "ds-actions",
              actionButton("load_advs", "⬇ Load", class = "btn-sm primary"),
              fileInput("upload_advs", NULL, accept = c(".csv",".sas7bdat",".xpt"),
                        buttonLabel = "📁 Upload", placeholder = "No file", width = "140px")
            )
          ),

          # ADCM
          tags$div(id = "card_adcm", class = "ds-card",
            tags$div(class = "ds-card-header",
              tags$span(class = "ds-name", "ADCM"),
              uiOutput("badge_adcm")
            ),
            tags$p(class = "ds-desc", "Concomitant medications dataset"),
            tags$div(class = "ds-actions",
              actionButton("load_adcm", "⬇ Load", class = "btn-sm primary"),
              fileInput("upload_adcm", NULL, accept = c(".csv",".sas7bdat",".xpt"),
                        buttonLabel = "📁 Upload", placeholder = "No file", width = "140px")
            )
          ),

          # ADEFF
          tags$div(id = "card_adeff", class = "ds-card",
            tags$div(class = "ds-card-header",
              tags$span(class = "ds-name", "ADEFF"),
              uiOutput("badge_adeff")
            ),
            tags$p(class = "ds-desc", "Efficacy analysis dataset"),
            tags$div(class = "ds-actions",
              actionButton("load_adeff", "⬇ Load", class = "btn-sm primary"),
              fileInput("upload_adeff", NULL, accept = c(".csv",".sas7bdat",".xpt"),
                        buttonLabel = "📁 Upload", placeholder = "No file", width = "140px")
            )
          )

        ) # end ds-grid
      ), # end sub_loader_screen

      # ── Sub-screen 1B: Data Explorer ────────────────────────────────────
      tags$div(id = "sub_explorer_screen", class = "sub-screen",

        # Top toolbar
        tags$div(class = "explorer-toolbar",
          tags$div(style = "min-width: 160px;",
            selectInput("exp_ds", NULL,
                        choices  = c("ADSL","ADAE","ADLB","ADVS","ADCM","ADEFF"),
                        selected = "ADSL",
                        width    = "100%")
          ),
          tags$div(style = "flex:1; max-width: 280px;",
            textInput("exp_search", NULL, placeholder = "Quick search across all columns...",
                      width = "100%")
          ),
          uiOutput("exp_active_badge"),
          actionButton("exp_apply",  "✔ Apply Filters", class = "btn-sm primary"),
          actionButton("exp_clear",  "✖ Clear All",     class = "btn-sm"),
          downloadButton("exp_dl_csv",  "CSV",  class = "btn-sm"),
          downloadButton("exp_dl_xlsx", "XLSX", class = "btn-sm")
        ),

        # Main area
        tags$div(class = "explorer-layout",

          # Filter panel
          tags$div(class = "explorer-filter-panel",
            uiOutput("exp_filters_ui")
          ),

          # Table area
          tags$div(class = "explorer-main",
            uiOutput("exp_row_badge"),
            DT::dataTableOutput("exp_table")
          )
        )
      ), # end sub_explorer_screen

      # ── Sub-screen 1C: Summary Dashboard ────────────────────────────────
      tags$div(id = "sub_summary_screen", class = "sub-screen",

        tags$div(class = "summary-layout",

          # Dataset selector for summary
          tags$div(style = "display:flex; align-items:center; gap:10px; flex-shrink:0;",
            tags$span(style = "font-weight:700; color:#431407;", "Dataset:"),
            selectInput("sum_ds", NULL,
                        choices  = c("ADSL","ADAE","ADLB","ADVS","ADCM","ADEFF"),
                        selected = "ADSL",
                        width    = "180px")
          ),

          # Overview cards
          tags$div(class = "summary-cards-row",
            tags$div(class = "summary-card",
              tags$div(class = "summary-card-header", "SUBJECTS"),
              tags$div(class = "summary-card-body",
                uiOutput("sum_subjects"),
                tags$div(class = "summary-card-subtitle", "(ADSL unique)")
              )
            ),
            tags$div(class = "summary-card",
              tags$div(class = "summary-card-header", "RECORDS"),
              tags$div(class = "summary-card-body",
                uiOutput("sum_records"),
                tags$div(class = "summary-card-subtitle", "(selected dataset)")
              )
            ),
            tags$div(class = "summary-card",
              tags$div(class = "summary-card-header", "VARIABLES"),
              tags$div(class = "summary-card-body",
                uiOutput("sum_variables"),
                tags$div(class = "summary-card-subtitle", "(columns)")
              )
            ),
            tags$div(class = "summary-card",
              tags$div(class = "summary-card-header", "MISSING %"),
              tags$div(class = "summary-card-body",
                uiOutput("sum_missing"),
                tags$div(class = "summary-card-subtitle", "overall")
              )
            )
          ),

          # Data Specs table
          tags$div(class = "summary-section",
            tags$div(class = "summary-section-header",
              "Data Specifications",
              downloadButton("sum_specs_dl", "CSV", class = "btn-sm")
            ),
            tags$div(class = "summary-section-body",
              DT::dataTableOutput("sum_specs_table")
            )
          ),

          # Distribution plots (tabbed)
          tags$div(class = "summary-section",
            tags$div(class = "summary-section-header", "Distribution Plots"),
            tags$div(class = "dist-tabs",
              tags$button(class = "dist-tab-btn active", id = "dist_tab_var",
                          onclick = "switchDistTab('var')", "Variable Distributions"),
              tags$button(class = "dist-tab-btn", id = "dist_tab_trt",
                          onclick = "switchDistTab('trt')", "Treatment Arm Overview"),
              tags$button(class = "dist-tab-btn", id = "dist_tab_miss",
                          onclick = "switchDistTab('miss')", "Missing Data Heatmap")
            ),
            tags$div(id = "dist_content_var", class = "dist-tab-content active",
              fluidRow(
                column(3,
                  selectInput("sum_var_pick", "Select Variable:", choices = NULL, width = "100%")
                ),
                column(9,
                  plotOutput("sum_var_dist_plot", height = "300px")
                )
              )
            ),
            tags$div(id = "dist_content_trt", class = "dist-tab-content",
              plotOutput("sum_trt_plot", height = "320px")
            ),
            tags$div(id = "dist_content_miss", class = "dist-tab-content",
              plotOutput("sum_miss_plot", height = "320px")
            )
          ),

          # Correlation matrix (collapsible)
          tags$div(class = "corr-panel",
            tags$div(class = "corr-panel-header",
              tags$span(style = "font-weight:700; color:#431407;", "Correlation Matrix"),
              tags$button(class = "collapse-btn", id = "corr_toggle_btn",
                          onclick = "toggleCorr()", "▼ Show")
            ),
            tags$div(id = "corr_panel_body", class = "corr-panel-body",
                     style = "display:none;",
              plotOutput("sum_corr_plot", height = "340px")
            )
          ),

          # DQ Flags
          tags$div(class = "dq-section",
            tags$div(class = "summary-section-header", "Data Quality Flags"),
            tags$div(class = "summary-section-body",
              DT::dataTableOutput("sum_dq_table")
            )
          )

        ) # end summary-layout
      ) # end sub_summary_screen

    ), # end screen_data

    # ════════════════════════════════════════════════════════════════════════
    # SCREEN 2 — TABLES
    # ════════════════════════════════════════════════════════════════════════
    tags$div(id = "screen_tables", class = "screen",
      tags$div(class = "tlf-layout",

        # Sidebar
        tags$div(class = "tlf-sidebar",
          tags$div(class = "sb-label", "Table Type"),
          selectInput("tbl_type", NULL,
                      choices = c(
                        "Demographics"        = "14.1.1 — Demographics",
                        "Primary Endpoint"    = "14.2.1 — Primary Endpoint",
                        "AE Summary"          = "14.3.1 — AE Summary",
                        "AE by SOC/PT"        = "14.3.2 — AE by SOC/PT",
                        "Lab Summary"         = "14.4.1 — Lab Summary"
                      ),
                      width = "100%"),

          tags$div(class = "sb-label", "Population"),
          radioButtons("tbl_pop", NULL,
                       choices = c("Safety (SAFFL)" = "SAFFL",
                                   "ITT (ITTFL)"    = "ITTFL",
                                   "All"             = "ALL"),
                       selected = "SAFFL"),

          tags$div(class = "sb-label", "Merge with ADSL"),
          tags$div(class = "toggle-wrap", id = "tbl_merge_wrap",
            tags$div(id = "tbl_merge_track", class = "toggle-track",
              tags$div(class = "toggle-thumb")
            ),
            tags$span("Merge ADSL", style = "font-size:12px;")
          ),

          tags$hr(style = "border-color:#fed7aa;"),
          tags$div(class = "action-btn-group",
            actionButton("tbl_generate", "▶ Generate Table",  class = "btn-primary"),
            actionButton("tbl_rcode",    "{ } Show R Code",   class = "btn-secondary"),
            actionButton("tbl_config",   "⚙ Configure",       class = "btn-secondary"),
            downloadButton("tbl_dl_rtf", "⬇ Download RTF",    class = "btn-secondary")
          )
        ),

        # Main area
        tags$div(class = "tlf-main",
          uiOutput("tbl_caption_ui"),
          DT::dataTableOutput("tbl_output"),
          uiOutput("tbl_footnote_ui")
        )
      )
    ),

    # ════════════════════════════════════════════════════════════════════════
    # SCREEN 3 — LISTINGS
    # ════════════════════════════════════════════════════════════════════════
    tags$div(id = "screen_listings", class = "screen",
      tags$div(class = "tlf-layout",

        tags$div(class = "tlf-sidebar",
          tags$div(class = "sb-label", "Listing Type"),
          selectInput("lst_type", NULL,
                      choices = c(
                        "All Adverse Events",
                        "Serious Adverse Events",
                        "Lab Abnormalities",
                        "Concomitant Medications",
                        "Subject Disposition",
                        "Vital Signs"
                      ),
                      width = "100%"),

          tags$div(class = "sb-label", "Population"),
          radioButtons("lst_pop", NULL,
                       choices = c("Safety (SAFFL)" = "SAFFL",
                                   "ITT (ITTFL)"    = "ITTFL",
                                   "All"             = "ALL"),
                       selected = "SAFFL"),

          tags$hr(style = "border-color:#fed7aa;"),
          tags$div(class = "action-btn-group",
            actionButton("lst_generate", "▶ Generate Listing", class = "btn-primary"),
            actionButton("lst_rcode",    "{ } Show R Code",    class = "btn-secondary"),
            downloadButton("lst_dl_rtf", "⬇ Download RTF",     class = "btn-secondary")
          )
        ),

        tags$div(class = "tlf-main",
          uiOutput("lst_caption_ui"),
          DT::dataTableOutput("lst_output"),
          uiOutput("lst_footnote_ui")
        )
      )
    ),

    # ════════════════════════════════════════════════════════════════════════
    # SCREEN 4 — FIGURES
    # ════════════════════════════════════════════════════════════════════════
    tags$div(id = "screen_figures", class = "screen",
      tags$div(class = "tlf-layout",

        tags$div(class = "tlf-sidebar",
          tags$div(class = "sb-label", "Figure Type"),
          selectInput("fig_type", NULL,
                      choices = c("eDISH Plot", "KM Curve", "Waterfall Plot",
                                  "Volcano Plot", "Lab Box Plot"),
                      width = "100%"),

          conditionalPanel(
            condition = "input.fig_type == 'Lab Box Plot'",
            tags$div(class = "sb-label", "Parameter"),
            selectInput("fig_lab_param", NULL,
                        choices = c("ALT","AST","BILI","ALB","CREAT","GLUC","HGB","PLT","WBC"),
                        width = "100%")
          ),

          tags$hr(style = "border-color:#fed7aa;"),
          tags$div(class = "action-btn-group",
            actionButton("fig_generate", "▶ Generate Figure",  class = "btn-primary"),
            actionButton("fig_rcode",    "{ } Show R Code",    class = "btn-secondary"),
            downloadButton("fig_dl_png", "⬇ Download PNG",     class = "btn-secondary")
          )
        ),

        tags$div(class = "tlf-main",
          tags$div(class = "fig-container", style = "flex:1;",
            plotOutput("fig_output", height = "100%", width = "100%")
          )
        )
      )
    ),

    # ════════════════════════════════════════════════════════════════════════
    # SCREEN 5 — ABOUT
    # ════════════════════════════════════════════════════════════════════════
    tags$div(id = "screen_about", class = "screen",
      tags$div(class = "about-container",
        tags$h1("EphiVIZ v3 — Clinical TLF Builder"),
        tags$p(
          "A comprehensive R Shiny application for generating clinical Tables, Listings, and ",
          "Figures (TLF) from ADaM datasets. Supports pharmaverseadam package data, ",
          "SAS/CSV file uploads, and synthetic data generation."
        ),
        tags$h2("Features"),
        tags$ul(
          tags$li("📂 Dataset Loader — Load from pharmaverseadam or upload SAS/CSV files"),
          tags$li("🔍 Data Explorer — Interactive multi-filter data browsing"),
          tags$li("📊 Summary Dashboard — Data profiling, distributions, missing data, DQ flags"),
          tags$li("📊 Tables — Demographics, AE Summary, AE by SOC/PT, Lab Summary, Efficacy"),
          tags$li("📋 Listings — AEs, Serious AEs, Lab Abnormalities, ConMeds, Disposition, VS"),
          tags$li("📈 Figures — eDISH, KM Curve, Waterfall, Volcano, Lab Box Plot")
        ),
        tags$h2("Datasets Supported"),
        tags$ul(
          tags$li("ADSL — Subject-level analysis dataset"),
          tags$li("ADAE — Adverse events"),
          tags$li("ADLB — Laboratory results"),
          tags$li("ADVS — Vital signs"),
          tags$li("ADCM — Concomitant medications"),
          tags$li("ADEFF — Efficacy")
        ),
        tags$h2("Technology"),
        tags$p("Built with R Shiny, ggplot2, DT, shinyjs, dplyr, survival, flextable, and officer."),
        tags$hr(style = "border-color:#fed7aa;"),
        tags$p(style = "color:#92400e; font-size:11px;",
               "EphiVIZ v3.0.0 | Clinical TLF Builder | For research use only")
      )
    )

  ), # end #app

  # ── JavaScript ──────────────────────────────────────────────────────────────
  tags$script(HTML("
    // ── Navigation ────────────────────────────────────────────────────────
    function showScreen(name) {
      ['data','tables','listings','figures','about'].forEach(function(s) {
        document.getElementById('screen_' + s).classList.remove('active');
        document.getElementById('nav_' + s).classList.remove('active');
      });
      document.getElementById('screen_' + name).classList.add('active');
      document.getElementById('nav_' + name).classList.add('active');
    }

    // Bind nav buttons (Shiny actionButtons fire input events; we also handle click)
    document.getElementById('nav_data').addEventListener('click',
      function(){ showScreen('data'); });
    document.getElementById('nav_tables').addEventListener('click',
      function(){ showScreen('tables'); });
    document.getElementById('nav_listings').addEventListener('click',
      function(){ showScreen('listings'); });
    document.getElementById('nav_figures').addEventListener('click',
      function(){ showScreen('figures'); });
    document.getElementById('nav_about').addEventListener('click',
      function(){ showScreen('about'); });

    // ── Sub-tabs (Data screen) ─────────────────────────────────────────────
    function showSubTab(name) {
      ['loader','explorer','summary'].forEach(function(t) {
        document.getElementById('sub_' + t + '_screen').classList.remove('active');
        document.getElementById('sub_' + t).classList.remove('active');
      });
      document.getElementById('sub_' + name + '_screen').classList.add('active');
      document.getElementById('sub_' + name).classList.add('active');
    }

    document.getElementById('sub_loader').addEventListener('click',
      function(){ showSubTab('loader'); });
    document.getElementById('sub_explorer').addEventListener('click',
      function(){ showSubTab('explorer'); });
    document.getElementById('sub_summary').addEventListener('click',
      function(){ showSubTab('summary'); });

    // ── Distribution plot tabs ─────────────────────────────────────────────
    function switchDistTab(name) {
      ['var','trt','miss'].forEach(function(t) {
        document.getElementById('dist_content_' + t).classList.remove('active');
        document.getElementById('dist_tab_' + t).classList.remove('active');
      });
      document.getElementById('dist_content_' + name).classList.add('active');
      document.getElementById('dist_tab_' + name).classList.add('active');
    }

    // ── Correlation matrix toggle ──────────────────────────────────────────
    var corrOpen = false;
    function toggleCorr() {
      corrOpen = !corrOpen;
      var body = document.getElementById('corr_panel_body');
      var btn  = document.getElementById('corr_toggle_btn');
      body.style.display = corrOpen ? 'block' : 'none';
      btn.textContent    = corrOpen ? '▲ Hide' : '▼ Show';
      if (corrOpen) {
        Shiny.setInputValue('corr_toggle', Math.random());
      }
    }

    // ── CSS toggle for merge ADSL ──────────────────────────────────────────
    var mergeOn = false;
    document.getElementById('tbl_merge_track').addEventListener('click', function() {
      mergeOn = !mergeOn;
      this.classList.toggle('on', mergeOn);
      Shiny.setInputValue('tbl_merge_adsl', mergeOn);
    });
  "))
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # ── Reactive values ────────────────────────────────────────────────────────
  rv <- reactiveValues(
    ADSL   = NULL, ADAE = NULL, ADLB = NULL,
    ADVS   = NULL, ADCM = NULL, ADEFF = NULL,
    loaded = character(0),
    # Explorer state
    explorer_ds      = "ADSL",
    explorer_filters = list(),
    filter_notes     = list(),
    filtered_data    = NULL,
    # Config state
    cfg_title1 = "Study XYZ-123: A Phase 3 Randomized Controlled Trial",
    cfg_title2 = "", cfg_title3 = "",
    cfg_fn1    = "Abbreviations: n = number of subjects; SD = standard deviation.",
    cfg_fn2    = "", cfg_fn3    = ""
  )

  # ── Helper: get a dataset by name ─────────────────────────────────────────
  get_ds <- function(name) rv[[name]]

  loaded_ds_names <- reactive({
    rv$loaded
  })

  # ── Dataset status badges ──────────────────────────────────────────────────
  make_badge <- function(ds_name) {
    renderUI({
      if (ds_name %in% rv$loaded) {
        tags$span(class = "ds-badge ok", "✓ Loaded")
      } else {
        tags$span(class = "ds-badge", "Not loaded")
      }
    })
  }

  output$badge_adsl <- make_badge("ADSL")
  output$badge_adae <- make_badge("ADAE")
  output$badge_adlb <- make_badge("ADLB")
  output$badge_advs <- make_badge("ADVS")
  output$badge_adcm <- make_badge("ADCM")
  output$badge_adeff <- make_badge("ADEFF")

  # ── Load status ────────────────────────────────────────────────────────────
  output$load_status_ui <- renderUI({
    if (length(rv$loaded) == 0) {
      tags$div(class = "status-msg info",
               "ℹ No datasets loaded. Click ⬇ Load to load from pharmaverseadam or 📁 Upload to use your own file.")
    } else {
      tags$div(class = "status-msg ok",
               paste0("✓ Loaded: ", paste(rv$loaded, collapse = ", ")))
    }
  })

  # ── Load buttons ───────────────────────────────────────────────────────────
  load_ds <- function(ds_name) {
    withProgress(message = paste("Loading", ds_name, "..."), {
      tryCatch({
        df <- load_adam_data(ds_name)
        rv[[ds_name]] <- df
        if (!ds_name %in% rv$loaded) rv$loaded <- c(rv$loaded, ds_name)
        runjs(sprintf(
          "document.getElementById('card_%s').classList.add('loaded');",
          tolower(ds_name)
        ))
      }, error = function(e) {
        showNotification(paste("Error loading", ds_name, ":", e$message),
                         type = "error", duration = 8)
      })
    })
  }

  observeEvent(input$load_adsl,  { load_ds("ADSL") })
  observeEvent(input$load_adae,  { load_ds("ADAE") })
  observeEvent(input$load_adlb,  { load_ds("ADLB") })
  observeEvent(input$load_advs,  { load_ds("ADVS") })
  observeEvent(input$load_adcm,  { load_ds("ADCM") })
  observeEvent(input$load_adeff, { load_ds("ADEFF") })

  # ── Upload handlers ─────────────────────────────────────────────────────────
  do_upload <- function(ds_name, file_info) {
    req(file_info)
    withProgress(message = paste("Uploading", ds_name, "..."), {
      tryCatch({
        df <- handle_upload(file_info)
        rv[[ds_name]] <- df
        if (!ds_name %in% rv$loaded) rv$loaded <- c(rv$loaded, ds_name)
        runjs(sprintf(
          "document.getElementById('card_%s').classList.add('loaded');",
          tolower(ds_name)
        ))
        showNotification(paste("✓", ds_name, "uploaded successfully"),
                         type = "message", duration = 4)
      }, error = function(e) {
        showNotification(paste("Upload error:", e$message), type = "error", duration = 8)
      })
    })
  }

  observeEvent(input$upload_adsl,  { do_upload("ADSL",  input$upload_adsl) })
  observeEvent(input$upload_adae,  { do_upload("ADAE",  input$upload_adae) })
  observeEvent(input$upload_adlb,  { do_upload("ADLB",  input$upload_adlb) })
  observeEvent(input$upload_advs,  { do_upload("ADVS",  input$upload_advs) })
  observeEvent(input$upload_adcm,  { do_upload("ADCM",  input$upload_adcm) })
  observeEvent(input$upload_adeff, { do_upload("ADEFF", input$upload_adeff) })

  # ════════════════════════════════════════════════════════════════════════════
  # DATA EXPLORER
  # ════════════════════════════════════════════════════════════════════════════

  # Update dataset selector with loaded datasets
  observe({
    opts <- if (length(rv$loaded) > 0) rv$loaded else c("ADSL","ADAE","ADLB","ADVS","ADCM","ADEFF")
    updateSelectInput(session, "exp_ds", choices = opts,
                      selected = if ("ADSL" %in% opts) "ADSL" else opts[1])
  })

  # Current explorer dataset
  exp_data_raw <- reactive({
    req(input$exp_ds)
    df <- get_ds(input$exp_ds)
    if (is.null(df)) {
      showNotification(paste(input$exp_ds, "not loaded yet. Loading now..."),
                       type = "warning", duration = 3)
      load_ds(input$exp_ds)
      df <- get_ds(input$exp_ds)
    }
    df
  })

  # Filter panel UI
  output$exp_filters_ui <- renderUI({
    df <- exp_data_raw()
    req(df)
    ns <- session$ns

    cols_to_show <- names(df)
    # Limit to first 30 columns for performance
    if (length(cols_to_show) > 30) cols_to_show <- cols_to_show[1:30]

    widgets <- lapply(cols_to_show, function(col) {
      build_filter_widget_ui(col, df[[col]], ns)
    })
    do.call(tagList, widgets)
  })

  # Collect active filter values
  active_filters <- reactive({
    df <- exp_data_raw()
    req(df)
    cols <- names(df)
    if (length(cols) > 30) cols <- cols[1:30]

    filters <- list()
    for (col in cols) {
      id  <- paste0("flt_", col)
      val <- input[[id]]
      if (!is.null(val) && length(val) > 0) {
        filters[[col]] <- val
      }
    }
    filters
  })

  # Active filter count badge
  output$exp_active_badge <- renderUI({
    df  <- exp_data_raw()
    req(df)
    flt <- active_filters()
    n   <- count_active_filters(flt, df)
    if (n > 0) {
      tags$span(class = "row-count-badge",
        "Active filters:",
        tags$span(class = "active-filter-badge", n)
      )
    } else {
      tags$span(class = "row-count-badge", "No active filters")
    }
  })

  # Apply filters reactively (on button click)
  filtered_data <- eventReactive(input$exp_apply, {
    df  <- exp_data_raw()
    flt <- active_filters()
    result <- apply_explorer_filters(df, flt)

    # Quick text search
    search <- trimws(input$exp_search %||% "")
    if (nchar(search) > 0) {
      chr_cols <- names(result)[sapply(result, function(x) is.character(x) || is.factor(x))]
      if (length(chr_cols) > 0) {
        mask <- Reduce(`|`, lapply(chr_cols, function(c) {
          grepl(search, as.character(result[[c]]), ignore.case = TRUE)
        }))
        result <- result[mask, , drop = FALSE]
      }
    }
    result
  }, ignoreNULL = FALSE)

  # Also update when dataset changes
  observeEvent(input$exp_ds, {
    df <- exp_data_raw()
    rv$filtered_data <- df
  })

  # Show unfiltered data initially, filtered after Apply
  exp_display_data <- reactive({
    if (!is.null(filtered_data())) filtered_data()
    else exp_data_raw()
  })

  # Row count badge
  output$exp_row_badge <- renderUI({
    raw_df <- exp_data_raw()
    flt_df <- exp_display_data()
    req(raw_df)
    total <- nrow(raw_df)
    shown <- if (!is.null(flt_df)) nrow(flt_df) else total
    removed <- total - shown
    tags$div(
      class = "row-count-badge",
      style = "margin-bottom:8px;",
      sprintf("Showing %s of %s rows", format(shown, big.mark=","), format(total, big.mark=",")),
      if (removed > 0) tags$span(class = "filtered-out",
                                   sprintf("| %s filtered out", format(removed, big.mark=",")))
    )
  })

  # Data table output
  output$exp_table <- DT::renderDataTable({
    df <- exp_display_data()
    req(df)
    if (nrow(df) == 0) {
      return(DT::datatable(data.frame(Message = "No rows match current filters"),
                           rownames = FALSE, options = list(dom = "t")))
    }
    ephiviz_dt(df)
  })

  # Clear all filters
  observeEvent(input$exp_clear, {
    df <- exp_data_raw()
    req(df)
    cols <- names(df)
    if (length(cols) > 30) cols <- cols[1:30]
    for (col in cols) {
      x    <- df[[col]]
      type <- col_type_category(x)
      id   <- paste0("flt_", col)
      switch(type,
        CHR  = ,
        FLAG = ,
        LOGI = {
          vals <- sort(unique(x[!is.na(x)]))
          updateCheckboxGroupInput(session, id, selected = vals)
        },
        NUM  = {
          mn <- floor(min(x, na.rm = TRUE) * 10) / 10
          mx <- ceiling(max(x, na.rm = TRUE) * 10) / 10
          if (!is.finite(mn)) mn <- 0
          if (!is.finite(mx)) mx <- 1
          if (mn == mx) mx <- mn + 1
          updateSliderInput(session, id, value = c(mn, mx))
        },
        DATE = {
          dts <- as.Date(x[!is.na(x)])
          updateDateRangeInput(session, id, start = min(dts), end = max(dts))
        }
      )
    }
    # Reset text search
    updateTextInput(session, "exp_search", value = "")
    showNotification("✓ All filters cleared", type = "message", duration = 2)
  })

  # Downloads
  output$exp_dl_csv <- downloadHandler(
    filename = function() paste0(input$exp_ds, "_filtered_", Sys.Date(), ".csv"),
    content  = function(file) {
      df <- exp_display_data()
      readr::write_csv(df, file)
    }
  )

  output$exp_dl_xlsx <- downloadHandler(
    filename = function() paste0(input$exp_ds, "_filtered_", Sys.Date(), ".xlsx"),
    content  = function(file) {
      df <- exp_display_data()
      openxlsx::write.xlsx(df, file)
    }
  )

  # ════════════════════════════════════════════════════════════════════════════
  # SUMMARY DASHBOARD
  # ════════════════════════════════════════════════════════════════════════════

  # Update dataset selector for summary
  observe({
    opts <- if (length(rv$loaded) > 0) rv$loaded else c("ADSL","ADAE","ADLB","ADVS","ADCM","ADEFF")
    updateSelectInput(session, "sum_ds", choices = opts,
                      selected = if ("ADSL" %in% opts) "ADSL" else opts[1])
  })

  sum_datasets <- reactive({
    list(ADSL = rv$ADSL, ADAE = rv$ADAE, ADLB = rv$ADLB,
         ADVS = rv$ADVS, ADCM = rv$ADCM, ADEFF = rv$ADEFF)
  })

  sum_selected_df <- reactive({
    req(input$sum_ds)
    df <- get_ds(input$sum_ds)
    if (is.null(df)) load_ds(input$sum_ds)
    get_ds(input$sum_ds)
  })

  sum_overview <- reactive({
    summary_overview(sum_datasets(), input$sum_ds)
  })

  output$sum_subjects  <- renderUI({
    tags$div(class = "summary-card-value",
             format(sum_overview()$n_subjects, big.mark = ","))
  })
  output$sum_records   <- renderUI({
    tags$div(class = "summary-card-value",
             format(sum_overview()$n_records, big.mark = ","))
  })
  output$sum_variables <- renderUI({
    tags$div(class = "summary-card-value",
             format(sum_overview()$n_variables, big.mark = ","))
  })
  output$sum_missing   <- renderUI({
    tags$div(class = "summary-card-value",
             paste0(sum_overview()$pct_missing, "%"))
  })

  # Data specs table
  output$sum_specs_table <- DT::renderDataTable({
    df <- sum_selected_df()
    req(df)
    specs <- build_col_summary(df)
    if (is.null(specs) || nrow(specs) == 0) return(NULL)

    # Colour-code type column
    specs$Type <- sapply(specs$Type, function(t) {
      cls <- switch(t, NUM = "type-num", CHR = "type-chr", DATE = "type-date",
                    FLAG = "type-flag", LOGI = "type-logi", "")
      sprintf('<span class="type-chip %s">%s</span>', cls, t)
    })

    DT::datatable(
      specs,
      escape    = FALSE,
      rownames  = FALSE,
      options   = list(
        dom        = "Bfrtip",
        pageLength = 25,
        scrollX    = TRUE,
        buttons    = list("copy", list(extend = "csv", text = "CSV")),
        initComplete = DT::JS(
          "function(settings, json) {",
          "  $(this.api().table().header()).css({'background-color':'#431407','color':'#fff'});",
          "}"
        )
      )
    )
  })

  output$sum_specs_dl <- downloadHandler(
    filename = function() paste0(input$sum_ds, "_specs_", Sys.Date(), ".csv"),
    content  = function(file) {
      df <- sum_selected_df()
      specs <- build_col_summary(df)
      readr::write_csv(specs, file)
    }
  )

  # Update variable picker for distribution
  observe({
    df <- sum_selected_df()
    req(df)
    updateSelectInput(session, "sum_var_pick", choices = names(df), selected = names(df)[1])
  })

  # Variable distribution plot
  output$sum_var_dist_plot <- renderPlot({
    df <- sum_selected_df()
    req(df, input$sum_var_pick)
    plot_variable_dist(df, input$sum_var_pick)
  }, res = 96)

  # Treatment overview plot
  output$sum_trt_plot <- renderPlot({
    adsl <- rv$ADSL
    if (is.null(adsl)) adsl <- load_adam_data("ADSL")
    plot_treatment_overview(adsl)
  }, res = 96)

  # Missing data heatmap
  output$sum_miss_plot <- renderPlot({
    plot_missing_heatmap(sum_datasets())
  }, res = 96)

  # Correlation matrix
  output$sum_corr_plot <- renderPlot({
    req(input$corr_toggle)
    df <- sum_selected_df()
    req(df)
    plot_corr_matrix(df)
  }, res = 96)

  # DQ flags table
  output$sum_dq_table <- DT::renderDataTable({
    flags <- compute_dq_flags(sum_datasets())
    req(flags)

    flags$Severity <- sapply(flags$Severity, function(s) {
      cls <- if (grepl("OK", s)) "dq-ok"
             else if (grepl("Error", s)) "dq-error"
             else "dq-warn"
      sprintf('<span class="%s">%s</span>', cls, s)
    })

    DT::datatable(
      flags,
      escape   = FALSE,
      rownames = FALSE,
      options  = list(
        dom      = "t",
        pageLength = 20,
        initComplete = DT::JS(
          "function(settings, json) {",
          "  $(this.api().table().header()).css({'background-color':'#431407','color':'#fff'});",
          "}"
        )
      )
    )
  })

  # ════════════════════════════════════════════════════════════════════════════
  # TABLES
  # ════════════════════════════════════════════════════════════════════════════

  tbl_result <- reactiveVal(NULL)

  observeEvent(input$tbl_generate, {
    adsl  <- rv$ADSL
    adae  <- rv$ADAE
    adlb  <- rv$ADLB
    adeff <- rv$ADEFF
    pop   <- input$tbl_pop

    if (is.null(adsl)) {
      adsl <- load_adam_data("ADSL")
      rv$ADSL <- adsl
      if (!"ADSL" %in% rv$loaded) rv$loaded <- c(rv$loaded, "ADSL")
    }

    result <- tryCatch({
      switch(input$tbl_type,
        "14.1.1 — Demographics"     = gen_demographics(adsl, pop),
        "14.2.1 — Primary Endpoint" = {
          if (is.null(adeff)) {
            adeff <- load_adam_data("ADEFF"); rv$ADEFF <- adeff
            if (!"ADEFF" %in% rv$loaded) rv$loaded <- c(rv$loaded, "ADEFF")
          }
          gen_primary_endpoint(adeff, adsl, pop)
        },
        "14.3.1 — AE Summary"       = {
          if (is.null(adae)) {
            adae <- load_adam_data("ADAE"); rv$ADAE <- adae
            if (!"ADAE" %in% rv$loaded) rv$loaded <- c(rv$loaded, "ADAE")
          }
          gen_ae_summary(adae, adsl, pop)
        },
        "14.3.2 — AE by SOC/PT"     = {
          if (is.null(adae)) {
            adae <- load_adam_data("ADAE"); rv$ADAE <- adae
            if (!"ADAE" %in% rv$loaded) rv$loaded <- c(rv$loaded, "ADAE")
          }
          gen_ae_by_soc(adae, adsl, pop)
        },
        "14.4.1 — Lab Summary"      = {
          if (is.null(adlb)) {
            adlb <- load_adam_data("ADLB"); rv$ADLB <- adlb
            if (!"ADLB" %in% rv$loaded) rv$loaded <- c(rv$loaded, "ADLB")
          }
          gen_lab_summary(adlb, pop)
        },
        NULL
      )
    }, error = function(e) {
      showNotification(paste("Table error:", e$message), type = "error", duration = 8)
      NULL
    })

    tbl_result(result)
  })

  output$tbl_caption_ui <- renderUI({
    res <- tbl_result()
    if (!is.null(res)) tags$p(class = "tbl-caption", res$caption)
  })

  output$tbl_footnote_ui <- renderUI({
    res <- tbl_result()
    if (!is.null(res)) tags$p(class = "tbl-footnote", res$footnote)
  })

  output$tbl_output <- DT::renderDataTable({
    res <- tbl_result()
    if (is.null(res)) {
      return(DT::datatable(
        data.frame(Message = "Click '▶ Generate Table' to generate output"),
        rownames = FALSE, options = list(dom = "t")
      ))
    }
    ephiviz_dt(res$data)
  })

  # R Code modal
  observeEvent(input$tbl_rcode, {
    code <- gen_table_rcode(input$tbl_type,
                            switch(input$tbl_type,
                              "14.3.1 — AE Summary"   = "ADAE",
                              "14.3.2 — AE by SOC/PT" = "ADAE",
                              "14.4.1 — Lab Summary"  = "ADLB",
                              "14.2.1 — Primary Endpoint" = "ADEFF",
                              "ADSL"),
                            input$tbl_pop)
    showModal(modalDialog(
      title  = "Generated R Code",
      size   = "l",
      footer = tagList(
        actionButton("copy_rcode", "📋 Copy", class = "btn-sm"),
        modalButton("Close")
      ),
      tags$pre(class = "rcode-block", id = "rcode_pre", code)
    ))
  })

  # Copy R code to clipboard
  observeEvent(input$copy_rcode, {
    runjs("
      var codeEl = document.getElementById('rcode_pre');
      if (codeEl) {
        navigator.clipboard.writeText(codeEl.textContent).then(function() {
          Shiny.setInputValue('copy_done', Math.random());
        }).catch(function() {
          // Fallback: select text for manual copy
          var range = document.createRange();
          range.selectNode(codeEl);
          window.getSelection().removeAllRanges();
          window.getSelection().addRange(range);
          Shiny.setInputValue('copy_fallback', Math.random());
        });
      }
    ")
    showNotification("✓ Code copied to clipboard", type = "message", duration = 2)
  })

  # Table configuration modal
  observeEvent(input$tbl_config, {
    showModal(modalDialog(
      title  = "Table Configuration",
      size   = "l",
      footer = tagList(
        actionButton("cfg_save", "💾 Save", class = "btn-sm primary"),
        modalButton("Close")
      ),
      textInput("cfg_title1_input", "Title Line 1:", value = rv$cfg_title1, width = "100%"),
      textInput("cfg_title2_input", "Title Line 2:", value = rv$cfg_title2, width = "100%"),
      textInput("cfg_title3_input", "Title Line 3:", value = rv$cfg_title3, width = "100%"),
      tags$hr(),
      textInput("cfg_fn1_input", "Footnote Line 1:", value = rv$cfg_fn1, width = "100%"),
      textInput("cfg_fn2_input", "Footnote Line 2:", value = rv$cfg_fn2, width = "100%"),
      textInput("cfg_fn3_input", "Footnote Line 3:", value = rv$cfg_fn3, width = "100%")
    ))
  })

  observeEvent(input$cfg_save, {
    rv$cfg_title1 <- input$cfg_title1_input %||% ""
    rv$cfg_title2 <- input$cfg_title2_input %||% ""
    rv$cfg_title3 <- input$cfg_title3_input %||% ""
    rv$cfg_fn1    <- input$cfg_fn1_input %||% ""
    rv$cfg_fn2    <- input$cfg_fn2_input %||% ""
    rv$cfg_fn3    <- input$cfg_fn3_input %||% ""
    removeModal()
    showNotification("✓ Configuration saved", type = "message", duration = 2)
  })

  # Table download RTF
  output$tbl_dl_rtf <- downloadHandler(
    filename = function() paste0(gsub("[^A-Za-z0-9]", "_", input$tbl_type), ".rtf"),
    content  = function(file) {
      res <- tbl_result()
      if (is.null(res)) {
        writeLines("No table generated.", file)
        return()
      }
      tryCatch({
        ft <- flextable::flextable(res$data)
        ft <- flextable::set_caption(ft, caption = res$caption)
        ft <- flextable::bg(ft, bg = "#431407", part = "header")
        ft <- flextable::color(ft, color = "white", part = "header")
        flextable::save_as_rtf(ft, path = file)
      }, error = function(e) {
        writeLines(paste("RTF export error:", e$message), file)
      })
    }
  )

  # ════════════════════════════════════════════════════════════════════════════
  # LISTINGS
  # ════════════════════════════════════════════════════════════════════════════

  lst_result <- reactiveVal(NULL)

  observeEvent(input$lst_generate, {
    adsl <- rv$ADSL; adae <- rv$ADAE; adlb <- rv$ADLB
    advs <- rv$ADVS; adcm <- rv$ADCM
    pop  <- input$lst_pop

    auto_load <- function(ds_name) {
      if (is.null(rv[[ds_name]])) {
        df <- load_adam_data(ds_name)
        rv[[ds_name]] <- df
        if (!ds_name %in% rv$loaded) rv$loaded <- c(rv$loaded, ds_name)
        df
      } else rv[[ds_name]]
    }

    result <- tryCatch({
      switch(input$lst_type,
        "All Adverse Events"      = {
          gen_listing_ae_all(auto_load("ADAE"), auto_load("ADSL"), pop)
        },
        "Serious Adverse Events"  = {
          gen_listing_ae_serious(auto_load("ADAE"), auto_load("ADSL"), pop)
        },
        "Lab Abnormalities"       = gen_listing_lab_abnorm(auto_load("ADLB"), pop),
        "Concomitant Medications" = {
          gen_listing_conmeds(auto_load("ADCM"), auto_load("ADSL"), pop)
        },
        "Subject Disposition"     = gen_listing_disposition(auto_load("ADSL"), pop),
        "Vital Signs"             = gen_listing_vs(auto_load("ADVS"), pop),
        NULL
      )
    }, error = function(e) {
      showNotification(paste("Listing error:", e$message), type = "error", duration = 8)
      NULL
    })

    lst_result(result)
  })

  output$lst_caption_ui <- renderUI({
    res <- lst_result()
    if (!is.null(res)) tags$p(class = "tbl-caption", res$caption)
  })

  output$lst_footnote_ui <- renderUI({
    res <- lst_result()
    if (!is.null(res)) tags$p(class = "tbl-footnote", res$footnote)
  })

  output$lst_output <- DT::renderDataTable({
    res <- lst_result()
    if (is.null(res)) {
      return(DT::datatable(
        data.frame(Message = "Click '▶ Generate Listing' to generate output"),
        rownames = FALSE, options = list(dom = "t")
      ))
    }
    ephiviz_dt(res$data)
  })

  # Listing R code
  observeEvent(input$lst_rcode, {
    ds <- switch(input$lst_type,
      "All Adverse Events"      = "ADAE",
      "Serious Adverse Events"  = "ADAE",
      "Lab Abnormalities"       = "ADLB",
      "Concomitant Medications" = "ADCM",
      "Subject Disposition"     = "ADSL",
      "Vital Signs"             = "ADVS",
      "ADSL"
    )
    code <- gen_listing_rcode(input$lst_type, ds, input$lst_pop)
    showModal(modalDialog(
      title  = "Generated R Code",
      size   = "l",
      footer = modalButton("Close"),
      tags$pre(class = "rcode-block", code)
    ))
  })

  output$lst_dl_rtf <- downloadHandler(
    filename = function() paste0(gsub("[^A-Za-z0-9]", "_", input$lst_type), ".rtf"),
    content  = function(file) {
      res <- lst_result()
      if (is.null(res)) { writeLines("No listing generated.", file); return() }
      tryCatch({
        ft <- flextable::flextable(res$data)
        ft <- flextable::set_caption(ft, caption = res$caption)
        ft <- flextable::bg(ft, bg = "#431407", part = "header")
        ft <- flextable::color(ft, color = "white", part = "header")
        flextable::save_as_rtf(ft, path = file)
      }, error = function(e) writeLines(paste("RTF export error:", e$message), file))
    }
  )

  # ════════════════════════════════════════════════════════════════════════════
  # FIGURES
  # ════════════════════════════════════════════════════════════════════════════

  fig_plot <- reactiveVal(NULL)

  observeEvent(input$fig_generate, {
    adsl <- rv$ADSL; adae <- rv$ADAE; adlb <- rv$ADLB; adeff <- rv$ADEFF

    auto_load <- function(ds_name) {
      if (is.null(rv[[ds_name]])) {
        df <- load_adam_data(ds_name)
        rv[[ds_name]] <- df
        if (!ds_name %in% rv$loaded) rv$loaded <- c(rv$loaded, ds_name)
        df
      } else rv[[ds_name]]
    }

    p <- tryCatch({
      switch(input$fig_type,
        "eDISH Plot"    = gen_edish(auto_load("ADLB")),
        "KM Curve"      = gen_km_curve(auto_load("ADSL")),
        "Waterfall Plot" = gen_waterfall(auto_load("ADEFF"), auto_load("ADSL")),
        "Volcano Plot"  = gen_volcano(auto_load("ADAE"), auto_load("ADSL")),
        "Lab Box Plot"  = gen_lab_boxplot(auto_load("ADLB"), input$fig_lab_param),
        NULL
      )
    }, error = function(e) {
      showNotification(paste("Figure error:", e$message), type = "error", duration = 8)
      NULL
    })

    fig_plot(p)
  })

  output$fig_output <- renderPlot({
    p <- fig_plot()
    if (is.null(p)) {
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = .5, y = .5,
                          label = "Click '▶ Generate Figure' to render",
                          size = 5, colour = "#92400e") +
        ggplot2::theme_void() +
        ggplot2::theme(plot.background = ggplot2::element_rect(fill = "#fff7ed", colour = NA))
    } else p
  }, height = function() {
    h <- session$clientData$output_fig_output_height
    if (is.null(h) || h < 200) 500 else h
  }, width = function() session$clientData$output_fig_output_width %||% 700)

  # Figure R code
  observeEvent(input$fig_rcode, {
    code <- sprintf(
'# EphiVIZ — Auto-generated R code
# Figure: %s

library(ggplot2)
source("R/data_utils.R")
source("R/figure_utils.R")

# Load required datasets
adsl <- load_adam_data("ADSL")
adae <- load_adam_data("ADAE")
adlb <- load_adam_data("ADLB")

# Generate figure
p <- gen_%s(%s)
print(p)

# Save
ggsave("output/%s.png", p, width = 10, height = 6, dpi = 150)',
      input$fig_type,
      switch(input$fig_type,
        "eDISH Plot"     = "edish",
        "KM Curve"       = "km_curve",
        "Waterfall Plot" = "waterfall",
        "Volcano Plot"   = "volcano",
        "Lab Box Plot"   = "lab_boxplot",
        "edish"),
      switch(input$fig_type,
        "eDISH Plot"     = "adlb",
        "KM Curve"       = "adsl",
        "Waterfall Plot" = "adeff = NULL, adsl = adsl",
        "Volcano Plot"   = "adae, adsl",
        "Lab Box Plot"   = sprintf('adlb, param_name = "%s"', input$fig_lab_param %||% "ALT"),
        ""),
      gsub("[^A-Za-z0-9]", "_", tolower(input$fig_type))
    )

    showModal(modalDialog(
      title  = "Generated R Code",
      size   = "l",
      footer = modalButton("Close"),
      tags$pre(class = "rcode-block", code)
    ))
  })

  output$fig_dl_png <- downloadHandler(
    filename = function() paste0(gsub("[^A-Za-z0-9]", "_", tolower(input$fig_type)), ".png"),
    content  = function(file) {
      p <- fig_plot()
      if (is.null(p)) {
        p <- ggplot2::ggplot() +
          ggplot2::annotate("text", x = .5, y = .5, label = "No figure generated", size = 5) +
          ggplot2::theme_void()
      }
      ggplot2::ggsave(file, p, width = 10, height = 6, dpi = 150)
    }
  )
}  # end server

shinyApp(ui = ui, server = server)

