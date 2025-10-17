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

EGFR_FORMULAS <- list(
  "EKFC" = "EKFC",
  "FAS" = "FAS",
  "r-LMR" = "r-LMR",
  "CKD-EPI (2021)" = "CKD-EPI (2021)",
  "LMR18" = "LMR18",
  "CAPA" = "CAPA",
  "MDRD" = "MDRD",
  "BIS (endast ≥70 år)" = "BIS"
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
          # --- Points 3 & 4 (side-by-side, conditional) ---
          conditionalPanel(
            condition = "input.num_points == 4",
            tagList(
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
              ),
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
            )
          )
      ),
      div(class = "input-section",
          tags$div(class = "mb-3",
                   actionButton("io_clear", "Rensa alla fält", class = "action-button btn w-100")
          )
      )
    ),
    mainPanel(
      width = 8,
      div(class = "panel-header",
          h4("Iohexolberäkning", style = "color: white; margin: 0;")
      ),
      div(class = "results-section",
          uiOutput("io_result_ui"),
          plotOutput("io_plot", height = "400px")
      )
    )
  )
}

# eGFR (Adults) tab UI
egfr_adult_tab_ui <- function() {
  sidebarLayout(
    sidebarPanel(
      width = 4,
      div(class = "input-section",
          h5("Demografi", style = "color: #333;"),
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
                   tags$input(id = "cysc", type = "number", class = "form-control", placeholder = " ", min = 0.2, max = 10, step = 0.01),
                   tags$label("Cystatin C (mg/L)", `for` = "cysc")
          )
      ),
      div(class = "input-section",
          h5("Formler", style = "color: #333;"),
          # --- REPLACED checkboxGroupInput with custom UI ---
          div(id="formulas", class="shiny-input-checkboxgroup shiny-input-container",
              lapply(names(EGFR_FORMULAS), function(label) {
                value <- EGFR_FORMULAS[[label]]
                
                # Create the checkbox tag
                checkbox_tag <- tags$div(class="checkbox",
                                         tags$label(
                                           tags$input(type="checkbox", name="formulas", value=value),
                                           tags$span(label)
                                         )
                )
                
                # Set the ID for the BIS checkbox specifically
                if (value == "BIS") {
                  checkbox_tag$children[[1]]$children[[1]]$attribs$id <- "formula_bis"
                }
                
                # Set the default checked state
                if (value == "EKFC") {
                  checkbox_tag$children[[1]]$children[[1]]$attribs$checked <- "checked"
                }
                
                return(checkbox_tag)
              })
          )
      ),
      div(class = "input-section",
          class = "mb-3",
          actionButton("clear", "Rensa alla fält", class = "action-button btn w-100")
      )
    ),
    mainPanel(
      width = 8,
      div(class = "panel-header",
          h4("eGFR (vuxna)", style = "color: white; margin: 0;")
      ),
      div(class = "results-section",
          DT::dataTableOutput("resultsTable"),
          uiOutput("selectedMean"),
          uiOutput("asteriskNote"),
          conditionalPanel(
            condition = "input.resultsTable_rows_selected && input.resultsTable_rows_selected.length > 0",
            actionButton("clear_selection", "Rensa val", class = "action-button-mini")
          )
      )
    )
  )
}

# Main UI
ui <- fluidPage(
  theme = bs_theme(version = 5),
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
    "))
  ),
  div(class = "app-container",
      navbarPage(
        "GFR-appen",
        tabPanel("Iohexolberäkning", iohexol_tab_ui()),
        tabPanel("eGFR (vuxna)", egfr_adult_tab_ui())
      )
  )
)