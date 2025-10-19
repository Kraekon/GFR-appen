# Updated ui.R with extended navbar padding and "Filip Landgren" below title
library(bslib)
library(htmltools)
library(shiny)
library(shinyjs) # Import shinyjs

# Shared formula choices (sync with global.R)
BSA_FORMULAS <- list(
  "Du Bois" = "dubois",
  "Du Bois (Tweaked)" = "dubois_tweaked",
  "Haycock" = "haycock",
  "Boyd" = "boyd",
  "Gehan" = "gehan",
  "Fujimoto" = "fujimoto",
  "Schlich" = "schlich"
)

EGFR_ADULT_FORMULAS <- list(
  "EKFC" = "EKFC",
  "FAS" = "FAS",
  "r-LMR" = "r-LMR",
  "CKD-EPI (2021)" = "CKD-EPI (2021)",
  "LMR18" = "LMR18",
  "CAPA" = "CAPA",
  "MDRD" = "MDRD",
  "BIS (≥70 år)" = "BIS"
)

EGFR_CHILD_FORMULAS <- list(
  "Schwartz (0 - 18 år)" = "Schwartz",
  "CKiD U25 (≥1 år)" = "CKiD_U25",
  "EKFC (≥2 år)" = "EKFC_child",
  "LMR18 (2-17 år)" = "LMR18_child",
  "CAPA (≥1 år)" = "CAPA_child",
  "Zappitelli (8-17 år)" = "Zappitelli"
)

# Omnipaque iodine concentrations
OMNIPAQUE_IODINE <- list(
  "Omnipaque 140" = 140,
  "Omnipaque 180" = 180,
  "Omnipaque 200" = 200,
  "Omnipaque 240" = 240,
  "Omnipaque 300" = 300,
  "Omnipaque 350" = 350
)

# Choices for the number of points selector
NUM_POINTS_CHOICES <- list(
  "1-punkt" = 1,
  "2-punkt" = 2,
  "4-punkt" = 4
)

# Consolidated Iohexol tab UI
iohexol_tab_ui <- function() {
  sidebarLayout(
    sidebarPanel(
      width = 4,
      div(class = "input-section",
          h5("Patient", style = "color: #333;"),
          tags$div(class = "form-floating mb-3",
                   tags$input(id = "io_age", type = "number", class = "form-control", placeholder = " ", min = 18, max = 120, step = 1),
                   tags$label("Ålder (år)", `for` = "io_age")
          ),
          tags$div(class = "form-floating mb-3",
                   tags$select(id = "io_sex", class = "form-select",
                               tags$option("Man", value = "Man"),
                               tags$option("Kvinna", value = "Kvinna")),
                   tags$label("Kön", `for` = "io_sex")
          ),
          tags$div(class = "form-floating mb-3",
                   tags$input(id = "io_height", type = "number", class = "form-control", placeholder = " ", min = 100, max = 230, step = 1),
                   tags$label("Längd (cm)", `for` = "io_height")
          ),
          tags$div(class = "form-floating mb-3",
                   tags$input(id = "io_weight", type = "number", class = "form-control", placeholder = " ", min = 30, max = 200, step = 1),
                   tags$label("Vikt (kg)", `for` = "io_weight")
          ),
          tags$div(class = "form-floating mb-3",
                   tags$select(id = "io_bsa_formula", class = "form-select",
                               mapply(function(name, value) tags$option(name, value = value), names(BSA_FORMULAS), BSA_FORMULAS, SIMPLIFY = FALSE)),
                   tags$label("BSA-formel", `for` = "io_bsa_formula")
          )
      ),
      div(class = "input-section",
          h5("Injektion", style = "color: #333;"),
          tags$div(class = "form-floating mb-3",
                   tags$select(id = "io_conc_inj", class = "form-select",
                               mapply(function(name, value) tags$option(name, value = value, selected = if(value == 300) "selected"), names(OMNIPAQUE_IODINE), OMNIPAQUE_IODINE, SIMPLIFY = FALSE)),
                   tags$label("Iohexol koncentration (mg jod/mL)", `for` = "io_conc_inj")
          ),
          tags$div(class = "form-floating mb-3",
                   tags$input(id = "io_vol_inj", type = "number", class = "form-control", placeholder = " ", value = 4, min = 0, step = 1),
                   tags$label("Volym iohexol (mL)", `for` = "io_vol_inj")
          ),
          tags$div(class = "form-floating mb-3",
                   tags$input(id = "io_inj_time", type = "text", class = "form-control", placeholder = "HH:MM"),
                   tags$label("Tidpunkt för injektion", `for` = "io_inj_time")
          )
      ),
      div(class = "input-section",
          h5("Provtagning", style = "color: #333;"),
          tags$div(class = "form-floating mb-3",
                   tags$select(id = "num_points", class = "form-select",
                               mapply(function(name, value) tags$option(name, value = value), names(NUM_POINTS_CHOICES), NUM_POINTS_CHOICES, SIMPLIFY = FALSE)),
                   tags$label("Antal mätpunkter", `for` = "num_points")
          ),
          
          # --- Point 1 (side-by-side) ---
          div(class = "row g-2 mb-3",
              div(class = "col",
                  div(class = "form-floating",
                      tags$input(id = "io_sample_time1", type = "text", class = "form-control", placeholder = "HH:MM"),
                      tags$label("Tidpunkt 1", `for` = "io_sample_time1")
                  )
              ),
              div(class = "col",
                  div(class = "form-floating",
                      tags$input(id = "io_conc_p1", type = "number", class = "form-control", placeholder = " ", min = 0, step = 1),
                      tags$label("Konc. 1 (mg/L)", `for` = "io_conc_p1")
                  )
              )
          ),
          # --- Point 2 (side-by-side, conditional) ---
          conditionalPanel(
            condition = "input.num_points >= 2",
            div(class = "row g-2 mb-3",
                div(class = "col",
                    div(class = "form-floating",
                        tags$input(id = "io_sample_time2", type = "text", class = "form-control", placeholder = "HH:MM"),
                        tags$label("Tidpunkt 2", `for` = "io_sample_time2")
                    )
                ),
                div(class = "col",
                    div(class = "form-floating",
                        tags$input(id = "io_conc_p2", type = "number", class = "form-control", placeholder = " ", min = 0, step = 1),
                        tags$label("Konc. 2 (mg/L)", `for` = "io_conc_p2")
                    )
                )
            )
          ),
          # --- Point 3 (side-by-side, conditional) ---
          conditionalPanel(
            condition = "input.num_points == 4",
            div(class = "row g-2 mb-3",
                div(class = "col",
                    div(class = "form-floating",
                        tags$input(id = "io_sample_time3", type = "text", class = "form-control", placeholder = "HH:MM"),
                        tags$label("Tidpunkt 3", `for` = "io_sample_time3")
                    )
                ),
                div(class = "col",
                    div(class = "form-floating",
                        tags$input(id = "io_conc_p3", type = "number", class = "form-control", placeholder = " ", min = 0, step = 1),
                        tags$label("Konc. 3 (mg/L)", `for` = "io_conc_p3")
                    )
                )
            )
          ),
          # --- Point 4 (side-by-side, conditional) ---
          conditionalPanel(
            condition = "input.num_points == 4",
            div(class = "row g-2 mb-3",
                div(class = "col",
                    div(class = "form-floating",
                        tags$input(id = "io_sample_time4", type = "text", class = "form-control", placeholder = "HH:MM"),
                        tags$label("Tidpunkt 4", `for` = "io_sample_time4")
                    )
                ),
                div(class = "col",
                    div(class = "form-floating",
                        tags$input(id = "io_conc_p4", type = "number", class = "form-control", placeholder = " ", min = 0, step = 1),
                        tags$label("Konc. 4 (mg/L)", `for` = "io_conc_p4")
                    )
                )
            )
          ),
          actionButton("io_clear", "Rensa alla fält", class = "action-button btn w-100")
      )
    ),
    mainPanel(
      width = 8,
      uiOutput("io_result_ui"),
      plotOutput("io_plot")
    )
  )
}

# eGFR Adult tab UI
egfr_adult_tab_ui <- function() {
  sidebarLayout(
    sidebarPanel(
      width = 4,
      div(class = "input-section",
          tags$div(class = "form-floating mb-3",
                   tags$input(id = "age", type = "number", class = "form-control", placeholder = " ", min = 18, max = 120, step = 1),
                   tags$label("Ålder (år)", `for` = "age")
          ),
          tags$div(class = "form-floating mb-3",
                   tags$select(id = "sex", class = "form-select",
                               tags$option("Man", value = "Man"),
                               tags$option("Kvinna", value = "Kvinna")),
                   tags$label("Kön", `for` = "sex")
          ),
          tags$div(class = "form-floating mb-3",
                   tags$input(id = "creatinine", type = "number", class = "form-control", placeholder = " ", min = 1, max = 5000, step = 1),
                   tags$label("Kreatinin (µmol/L)", `for` = "creatinine")
          ),
          tags$div(class = "form-floating mb-3",
                   tags$input(id = "cysc", type = "number", class = "form-control", placeholder = " ", min = 0.1, max = 10, step = 0.01),
                   tags$label("Cystatin C (mg/L)", `for` = "cysc")
          ),
          tags$div(class = "mb-3",
                   tags$label("Formler"),
                   checkboxGroupInput("formulas_adult", label = NULL, choices = EGFR_ADULT_FORMULAS, selected = "EKFC")
          ),
          actionButton("clear_adult", "Rensa alla fält", class = "action-button btn w-100")
      )
    ),
    mainPanel(
      width = 8,
      uiOutput("adult_results_ui")
    )
  )
}

# eGFR Child tab UI
egfr_child_tab_ui <- function() {
  sidebarLayout(
    sidebarPanel(
      width = 4,
      div(class = "input-section",
          tags$div(class = "form-floating mb-3",
                   tags$input(id = "age_child", type = "number", class = "form-control", placeholder = " ", min = 0, max = 18, step = 1),
                   tags$label("Ålder (år)", `for` = "age_child")
          ),
          tags$div(class = "form-floating mb-3",
                   tags$select(id = "sex_child", class = "form-select",
                               tags$option("Man", value = "Man"),
                               tags$option("Kvinna", value = "Kvinna")),
                   tags$label("Kön", `for` = "sex_child")
          ),
          tags$div(class = "form-floating mb-3",
                   tags$input(id = "height_child", type = "number", class = "form-control", placeholder = " ", min = 30, max = 200, step = 1),
                   tags$label("Längd (cm)", `for` = "height_child")
          ),
          tags$div(class = "form-floating mb-3",
                   tags$input(id = "creatinine_child", type = "number", class = "form-control", placeholder = " ", min = 1, max = 5000, step = 1),
                   tags$label("Kreatinin (µmol/L)", `for` = "creatinine_child")
          ),
          tags$div(class = "form-floating mb-3",
                   tags$input(id = "cysc_child", type = "number", class = "form-control", placeholder = " ", min = 0.1, max = 10, step = 0.01),
                   tags$label("Cystatin C (mg/L)", `for` = "cysc_child")
          ),
          tags$div(class = "mb-3",
                   tags$label("Formler"),
                   checkboxGroupInput("formulas_child", label = NULL, choices = EGFR_CHILD_FORMULAS, selected = "Schwartz")
          ),
          actionButton("clear_child", "Rensa alla fält", class = "action-button btn w-100")
      )
    ),
    mainPanel(
      width = 8,
      uiOutput("child_results_ui")
    )
  )
}

# "Other" tab UI
other_tab_ui <- function() {
  fluidPage(
    fluidRow(
      # --- Left Column: Cockcroft-Gault and Optimal provtagningstid ---
      column(6,
             card(
               style = "background-color: #f8f9fa;",
               card_header("Cockcroft-Gault"),
               card_body(
                 padding = "0.5rem",
                 div(class = "input-section",
                     tags$div(class = "form-floating mb-3",
                              tags$input(id = "cg_age", type = "number", class = "form-control", placeholder = " ", min = 0, max = 120, step = 1),
                              tags$label("Ålder (år)", `for` = "cg_age")
                     ),
                     tags$div(class = "form-floating mb-3",
                              tags$select(id = "cg_sex", class = "form-select",
                                          tags$option("Man", value = "Man"),
                                          tags$option("Kvinna", value = "Kvinna")),
                              tags$label("Kön", `for` = "cg_sex")
                     ),
                     tags$div(class = "form-floating mb-3",
                              tags$input(id = "cg_weight", type = "number", class = "form-control", placeholder = " ", min = 30, max = 200, step = 1),
                              tags$label("Vikt (kg)", `for` = "cg_weight")
                     ),
                     tags$div(class = "form-floating mb-3",
                              tags$input(id = "cg_creatinine", type = "number", class = "form-control", placeholder = " ", min = 1, max = 5000, step = 1),
                              tags$label("Kreatinin (µmol/L)", `for` = "cg_creatinine")
                     )
                 ),
                 uiOutput("cg_result_ui"),
                 actionButton("clear_cg", "Återställ", class = "action-button btn w-100")
               )
             ),
             card(
               style = "background-color: #f8f9fa;",
               card_header("Optimal provtagningstid (enpunkt)"),
               card_body(
                 padding = "0.5rem",
                 div(class = "input-section",
                     tags$div(class = "form-floating mb-3",
                              tags$input(id = "opt_age", type = "number", class = "form-control", placeholder = " ", min = 0, max = 120, step = 1),
                              tags$label("Ålder (år)", `for` = "opt_age")
                     ),
                     tags$div(class = "form-floating mb-3",
                              tags$select(id = "opt_sex", class = "form-select",
                                          tags$option("Man", value = "Man"),
                                          tags$option("Kvinna", value = "Kvinna")),
                              tags$label("Kön", `for` = "opt_sex")
                     ),
                     tags$div(class = "form-floating mb-3",
                              tags$input(id = "opt_weight", type = "number", class = "form-control", placeholder = " ", min = 30, max = 200, step = 1),
                              tags$label("Vikt (kg)", `for` = "opt_weight")
                     ),
                     tags$div(class = "form-floating mb-3",
                              tags$input(id = "opt_height", type = "number", class = "form-control", placeholder = " ", min = 100, max = 230, step = 1),
                              tags$label("Längd (cm)", `for` = "opt_height")
                     ),
                     tags$div(class = "form-floating mb-3",
                              tags$input(id = "opt_egfr", type = "number", class = "form-control", placeholder = " ", min = 0, step = 1),
                              tags$label("eGFR (mL/min/1.73m²)", `for` = "opt_egfr")
                     ),
                     tags$div(class = "form-floating mb-3",
                              tags$select(id = "opt_bsa_formula", class = "form-select",
                                          mapply(function(name, value) tags$option(name, value = value), names(BSA_FORMULAS), BSA_FORMULAS, SIMPLIFY = FALSE)),
                              tags$label("BSA-formel", `for` = "opt_bsa_formula")
                     )
                 ),
                 uiOutput("opt_result_ui"),
                 actionButton("clear_opt", "Återställ", class = "action-button btn w-100")
               )
             )
      ),
      
      # --- Right Column: GFR Conversion ---
      column(6,
             card(
               style = "background-color: #f8f9fa;",
               card_header("GFR-konvertering"),
               card_body(
                 padding = "0.5rem",
                 div(class = "input-section",
                     tags$div(class = "form-floating mb-3",
                              tags$select(id = "gfr_conv_direction", class = "form-select",
                                          tags$option("Absolut till relativ", value = "abs_to_rel"),
                                          tags$option("Relativ till absolut", value = "rel_to_abs")),
                              tags$label("Konverteringsriktning", `for` = "gfr_conv_direction")
                     ),
                     
                     conditionalPanel(
                       condition = "input.gfr_conv_direction == 'abs_to_rel'",
                       tags$div(class = "form-floating mb-3",
                                tags$input(id = "gfr_abs", type = "number", class = "form-control", placeholder = " ", min = 0),
                                tags$label("Absolut GFR (mL/min)", `for` = "gfr_abs")
                       )
                     ),
                     conditionalPanel(
                       condition = "input.gfr_conv_direction == 'rel_to_abs'",
                       tags$div(class = "form-floating mb-3",
                                tags$input(id = "gfr_rel", type = "number", class = "form-control", placeholder = " ", min = 0),
                                tags$label("Relativ GFR (mL/min/1.73m²)", `for` = "gfr_rel")
                       )
                     ),
                     
                     tags$div(class = "form-floating mb-3",
                              tags$input(id = "gfr_conv_height", type = "number", class = "form-control", placeholder = " ", min = 100, max = 230, step = 1),
                              tags$label("Längd (cm)", `for` = "gfr_conv_height")
                     ),
                     tags$div(class = "form-floating mb-3",
                              tags$input(id = "gfr_conv_weight", type = "number", class = "form-control", placeholder = " ", min = 30, max = 200, step = 1),
                              tags$label("Vikt (kg)", `for` = "gfr_conv_weight")
                     ),
                     tags$div(class = "form-floating mb-3",
                              tags$select(id = "gfr_conv_sex", class = "form-select",
                                          tags$option("Man", value = "Man"),
                                          tags$option("Kvinna", value = "Kvinna")),
                              tags$label("Kön", `for` = "gfr_conv_sex")
                     ),
                     tags$div(class = "form-floating mb-3",
                              tags$select(id = "gfr_conv_bsa_formula", class = "form-select",
                                          mapply(function(name, value) tags$option(name, value = value), names(BSA_FORMULAS), BSA_FORMULAS, SIMPLIFY = FALSE)),
                              tags$label("BSA-formel", `for` = "gfr_conv_bsa_formula")
                     )
                 ),
                 uiOutput("gfr_conv_result_ui"),
                 actionButton("clear_gfr_conv", "Återställ", class = "action-button btn w-100")
               )
             )
      )
    )
  )
}

# Main UI
ui <- fluidPage(
  theme = bs_theme(version = 5, primary = "#007bff", "navbar-bg" = "#007bff", "navbar-fg" = "#ffffff"),
  useShinyjs(), # Initialize shinyjs
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$style(HTML("
  /* Hide number input spinners */
  input[type=number]::-webkit-inner-spin-button, 
  input[type=number]::-webkit-outer-spin-button { 
    -webkit-appearance: none; 
    margin: 0; 
  }
  input[type=number] {
    -moz-appearance: textfield;
  }
  /* Navbar text and active tab styling */
  .navbar .navbar-nav .nav-link {
    color: #ffffff !important;
  }
  .navbar .navbar-nav .nav-link.active, .navbar .navbar-nav .nav-link:hover {
    color: #ffffff !important;
    border-bottom: 2px solid #ffffff !important;
    border-bottom-width: 2px !important;
    border-bottom-style: solid !important;
    margin-bottom: 2px;
    padding-bottom: 0.25rem;
  }
  .navbar {
    padding-bottom: 0.125rem;
  }
  /* MonoLisa-Bold font for navbar title */
  @font-face {
    font-family: 'MonoLisa-Bold';
    src: url('MonoLisa-Bold.ttf') format('truetype');
  }
  .navbar-brand {
    font-family: 'MonoLisa-Bold', sans-serif;
    color: #ffffff !important;
  }
  /* Checkbox checkmark styling for eGFR tabs */
  input[type='checkbox']:checked {
    background-image: url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 20 20'%3E%3Cpath fill='none' stroke='%23ffffff' stroke-linecap='round' stroke-linejoin='round' stroke-width='3' d='M6 10l3 3l6-6'/%3E%3C/svg%3E\") !important;
  }
"))
  ),
  div(class = "app-container",
      navbarPage(
        title = "GFR-appen",
        bg = "primary",
        tabPanel("Iohexolberäkning", iohexol_tab_ui()),
        tabPanel("eGFR (vuxna)", egfr_adult_tab_ui()),
        tabPanel("eGFR (barn)", egfr_child_tab_ui()),
        tabPanel("Övrigt", other_tab_ui())
      )
  )
)