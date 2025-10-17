source("global.R")

server <- function(input, output, session) {
  # Run once after session is flushed to set UI defaults where necessary.
  session$onFlushed(function() {
    updateSelectInput(session, "io_sex", selected = "Man")
    updateSelectInput(session, "io_bsa_formula", selected = "dubois")
    updateSelectInput(session, "io_conc_inj", selected = "300")
    updateSelectInput(session, "sex", selected = "Man")
    updateSelectInput(session, "sex_child", selected = "Man")
    updateSelectInput(session, "cg_sex", selected = "Man")
    updateSelectInput(session, "gfr_conv_sex", selected = "Man")
    updateSelectInput(session, "gfr_conv_bsa_formula", selected = "dubois")
    
    shinyjs::disable("formula_bis")
    
    # Initially disable all conditional child formulas
    shinyjs::disable("formula_child_CKiD_U25")
    shinyjs::disable("formula_child_EKFC_child")
    shinyjs::disable("formula_child_LMR18_child")
    shinyjs::disable("formula_child_CAPA_child")
    shinyjs::disable("formula_child_Zappitelli")
    
  }, once = TRUE)
  
  # --- Observer to conditionally enable/disable BIS formula ---
  observeEvent(input$age, {
    age_val <- input$age
    if (is.na(age_val) || age_val < 70) {
      shinyjs::disable("formula_bis")
      updateCheckboxGroupInput(session, "formulas_adult", selected = setdiff(input$formulas_adult, "BIS"))
    } else {
      shinyjs::enable("formula_bis")
    }
  }, ignoreNULL = FALSE)
  
  # --- Observer to conditionally enable/disable Pediatric formulas ---
  observeEvent(input$age_child, {
    age <- input$age_child
    
    manage_checkbox <- function(id, condition, value) {
      if (is.na(age) || !condition) {
        shinyjs::disable(id)
        updateCheckboxGroupInput(session, "formulas_child", selected = setdiff(input$formulas_child, value))
      } else {
        shinyjs::enable(id)
      }
    }
    
    manage_checkbox("formula_child_CKiD_U25", age >= 1, "CKiD_U25")
    manage_checkbox("formula_child_EKFC_child", age >= 2, "EKFC_child")
    manage_checkbox("formula_child_LMR18_child", age >= 2 && age < 18, "LMR18_child")
    manage_checkbox("formula_child_CAPA_child", age >= 1, "CAPA_child")
    manage_checkbox("formula_child_Zappitelli", age >= 8 && age <= 17, "Zappitelli")
    
  }, ignoreNULL = FALSE)
  
  # --- eGFR (Adults) Section ---
  
  output$adult_results_ui <- renderUI({
    age_val <- input$age
    if (is.na(age_val)) {
      tagList(
        DT::dataTableOutput("resultsTable_adult"),
        uiOutput("selectedMean_adult"),
        uiOutput("asteriskNote_adult"),
        conditionalPanel(
          condition = "input.resultsTable_adult_rows_selected && input.resultsTable_adult_rows_selected.length > 0",
          actionButton("clear_selection_adult", "Rensa val", class = "action-button-mini")
        )
      )
    } else if (age_val < 18) {
      tags$div(class = "alert alert-warning", role = "alert", "Använd beräkning för barn")
    } else {
      tagList(
        DT::dataTableOutput("resultsTable_adult"),
        uiOutput("selectedMean_adult"),
        uiOutput("asteriskNote_adult"),
        conditionalPanel(
          condition = "input.resultsTable_adult_rows_selected && input.resultsTable_adult_rows_selected.length > 0",
          actionButton("clear_selection_adult", "Rensa val", class = "action-button-mini")
        )
      )
    }
  })
  
  calc_egfr_adult <- function(scr_umol, scys, sex, age, formulas) {
    rows <- data.frame(Resultat = character(), Value = character(), stringsAsFactors = FALSE)
    scr_mgdl <- if (is_valid_num(scr_umol)) scr_umol / 88.4 else NA
    
    # Centralized Q-value calculations
    q_sex_age_adj <- calculate_q_cysc(age, sex)
    q_age_adj_rlmr <- calculate_q_cysc(age)
    
    if ("EKFC" %in% formulas) {
      if (is_valid_num(scr_mgdl)) {
        gfr_cr <- ekfc_gfr(scr_mgdl, ifelse(sex == "Man", 0.9, 0.7), -0.322, -1.132)
        rows <- rbind(rows, data.frame(Resultat = "EKFC (kreatinin)", Value = round(gfr_cr, 0)))
      }
      if (is_valid_num(scys)) {
        gfr_cys <- ekfc_gfr(scys, 0.83, -0.322, -1.132)
        rows <- rbind(rows, data.frame(Resultat = "EKFC (cystatin C)", Value = round(gfr_cys, 0)))
        if (!is.na(q_sex_age_adj)) {
          gfr_cys_sexadj <- ekfc_gfr(scys, q_sex_age_adj, -0.322, -1.132)
          rows <- rbind(rows, data.frame(Resultat = "EKFC (cystatin C)*", Value = round(gfr_cys_sexadj, 0)))
        }
      }
      if (is_valid_num(scr_mgdl) && is_valid_num(scys)) {
        gfr_cr <- ekfc_gfr(scr_mgdl, ifelse(sex == "Man", 0.9, 0.7), -0.322, -1.132)
        gfr_cys <- ekfc_gfr(scys, 0.83, -0.322, -1.132)
        rows <- rbind(rows, data.frame(Resultat = "EKFC (kreatinin & cystatin C)", Value = round((gfr_cr + gfr_cys) / 2, 0)))
        if (!is.na(q_sex_age_adj)) {
          gfr_cys_sexadj <- ekfc_gfr(scys, q_sex_age_adj, -0.322, -1.132)
          rows <- rbind(rows, data.frame(Resultat = "EKFC (kreatinin & cystatin C)*", Value = round((gfr_cr + gfr_cys_sexadj) / 2, 0)))
        }
      }
    }
    if ("FAS" %in% formulas) {
      if (is_valid_num(scr_mgdl)) rows <- rbind(rows, data.frame(Resultat = "FAS (kreatinin)", Value = round(fas_gfr_cr(scr_mgdl, age), 0)))
      if (is_valid_num(scys)) {
        rows <- rbind(rows, data.frame(Resultat = "FAS (cystatin C)", Value = round(fas_gfr_cys(scys, age), 0)))
        if(!is.na(q_sex_age_adj)) {
          gfr_fas_sex_adj <- 107.3 / (scys / q_sex_age_adj)
          if (age > 40) gfr_fas_sex_adj <- gfr_fas_sex_adj * (0.988^(age - 40))
          rows <- rbind(rows, data.frame(Resultat = "FAS (cystatin C)*", Value = round(gfr_fas_sex_adj, 0)))
        }
      }
      if (is_valid_num(scr_mgdl) && is_valid_num(scys)) {
        gfr_fas_cr <- fas_gfr_cr(scr_mgdl, age)
        gfr_fas_cys <- fas_gfr_cys(scys, age)
        rows <- rbind(rows, data.frame(Resultat = "FAS (kreatinin & cystatin C)", Value = round((gfr_fas_cr + gfr_fas_cys) / 2, 0)))
        if(!is.na(q_sex_age_adj)) {
          gfr_fas_sex_adj <- 107.3 / (scys / q_sex_age_adj)
          if (age > 40) gfr_fas_sex_adj <- gfr_fas_sex_adj * (0.988^(age - 40))
          rows <- rbind(rows, data.frame(Resultat = "FAS (kreatinin & cystatin C)*", Value = round((gfr_fas_cr + gfr_fas_sex_adj) / 2, 0)))
        }
      }
    }
    if ("r-LMR" %in% formulas) {
      if (is_valid_num(scr_umol)) {
        rows <- rbind(rows, data.frame(Resultat = "r-LMR (kreatinin)", Value = round(r_lmr_gfr_cr(scr_umol, sex, age), 0)))
      }
      if (is_valid_num(scys)) {
        rows <- rbind(rows, data.frame(Resultat = "r-LMR (cystatin C)", Value = round(r_lmr_gfr_cys(scys, age, q_age_adj_rlmr), 0)))
        rows <- rbind(rows, data.frame(Resultat = "r-LMR (cystatin C)*", Value = round(r_lmr_gfr_cys(scys, age, q_sex_age_adj), 0)))
      }
      if (is_valid_num(scr_umol) && is_valid_num(scys)) {
        gfr_cr <- r_lmr_gfr_cr(scr_umol, sex, age)
        gfr_cys_age <- r_lmr_gfr_cys(scys, age, q_age_adj_rlmr)
        gfr_cys_sex_age <- r_lmr_gfr_cys(scys, age, q_sex_age_adj)
        
        rows <- rbind(rows, data.frame(Resultat = "r-LMR (kreatinin & cystatin C)", Value = round((gfr_cr + gfr_cys_age) / 2, 0)))
        rows <- rbind(rows, data.frame(Resultat = "r-LMR (kreatinin & cystatin C)*", Value = round((gfr_cr + gfr_cys_sex_age) / 2, 0)))
      }
    }
    if ("CKD-EPI (2021)" %in% formulas) {
      if (is_valid_num(scr_umol)) rows <- rbind(rows, data.frame(Resultat = "CKD-EPI (kreatinin)", Value = round(ckdepi2021_gfr_cr(scr_umol, sex, age), 0)))
      if (is_valid_num(scys)) rows <- rbind(rows, data.frame(Resultat = "CKD-EPI (cystatin C)", Value = round(ckdepi2021_gfr_cys(scys, sex, age), 0)))
      if (is_valid_num(scr_umol) && is_valid_num(scys)) rows <- rbind(rows, data.frame(Resultat = "CKD-EPI (kreatinin & cystatin C)", Value = round(ckdepi2021_gfr_cr_cys(scr_umol, scys, sex, age), 0)))
    }
    if ("LMR18" %in% formulas && is_valid_num(scr_umol)) rows <- rbind(rows, data.frame(Resultat = "LMR18 (kreatinin)", Value = round(lmr18_gfr_cr(scr_umol, sex, age), 0)))
    if ("CAPA" %in% formulas && is_valid_num(scys)) rows <- rbind(rows, data.frame(Resultat = "CAPA (cystatin C)", Value = round(capa_gfr_cys(scys, age), 0)))
    if ("MDRD" %in% formulas && is_valid_num(scr_umol)) rows <- rbind(rows, data.frame(Resultat = "MDRD (kreatinin)", Value = round(mdrd_gfr(scr_umol, sex, age), 0)))
    if ("BIS" %in% formulas && is_valid_num(age) && age >= 70 && is_valid_num(scr_umol)) rows <- rbind(rows, data.frame(Resultat = "BIS (kreatinin)", Value = round(bis_gfr(scr_umol, sex, age), 0)))
    
    return(rows)
  }
  
  egfr_results_adult <- reactive({
    req(input$age, input$age >= 18, input$sex, input$formulas_adult)
    calc_egfr_adult(input$creatinine, input$cysc, input$sex, input$age, input$formulas_adult)
  })
  
  output$resultsTable_adult <- DT::renderDataTable({
    result <- egfr_results_adult()
    if (nrow(result) == 0) return(NULL)
    datatable(result, options = list(paging = FALSE, searching = FALSE, ordering = FALSE, info = FALSE, columnDefs = list(list(className = 'dt-center', targets = '_all'))), rownames = FALSE)
  })
  
  output$selectedMean_adult <- renderUI({
    selected <- input$resultsTable_adult_rows_selected
    if (length(selected) > 0) {
      results <- egfr_results_adult()
      mean_val <- mean(as.numeric(results$Value[selected]), na.rm = TRUE)
      tags$div(style = "margin-top: 10px;", paste("Medelvärde av valda rader:", round(mean_val, 0), "mL/min/1.73m²"))
    }
  })
  
  output$asteriskNote_adult <- renderUI({
    results <- egfr_results_adult()
    if (nrow(results) > 0 && any(grepl("\\*", results$Resultat))) {
      tags$div(style = "font-size: 0.8em; color: #666; margin-top: 10px;", "* Beräknat med köns- och åldersjusterad Q-faktor för cystatin C")
    }
  })
  
  # --- eGFR (Children) Section ---
  
  output$child_results_ui <- renderUI({
    age_val <- input$age_child
    if (is.na(age_val)) {
      tagList(
        DT::dataTableOutput("resultsTable_child"),
        uiOutput("selectedMean_child"),
        uiOutput("footnote_child"),
        conditionalPanel(
          condition = "input.resultsTable_child_rows_selected && input.resultsTable_child_rows_selected.length > 0",
          actionButton("clear_selection_child", "Rensa val", class = "action-button-mini")
        )
      )
    } else if (age_val > 18) {
      tags$div(class = "alert alert-warning", role = "alert", "Använd beräkning för vuxna")
    } else {
      tagList(
        DT::dataTableOutput("resultsTable_child"),
        uiOutput("selectedMean_child"),
        uiOutput("footnote_child"),
        conditionalPanel(
          condition = "input.resultsTable_child_rows_selected && input.resultsTable_child_rows_selected.length > 0",
          actionButton("clear_selection_child", "Rensa val", class = "action-button-mini")
        )
      )
    }
  })
  
  calc_egfr_child <- function(scr_umol, scys, sex, age, height, formulas) {
    rows <- data.frame(Resultat = character(), Value = character(), stringsAsFactors = FALSE)
    scr_mgdl <- if (is_valid_num(scr_umol)) scr_umol / 88.4 else NA
    height_m <- if (is_valid_num(height)) height / 100 else NA
    
    if ("Schwartz" %in% formulas && is_valid_num(scr_mgdl) && is_valid_num(height)) {
      rows <- rbind(rows, data.frame(Resultat = "Schwartz (0 - 18 år)", Value = round(schwartz_gfr(height, scr_mgdl), 0)))
    }
    if ("CKiD_U25" %in% formulas && age >= 1) {
      gfr_scr <- if (is_valid_num(scr_mgdl) && is_valid_num(height_m)) ckid_u25_scr(sex, age, height_m, scr_mgdl) else NA
      gfr_cys <- if (is_valid_num(scys)) ckid_u25_cys(sex, age, scys) else NA
      if(!is.na(gfr_scr)) rows <- rbind(rows, data.frame(Resultat = "CKiD U25 (kreatinin)", Value = round(gfr_scr, 0)))
      if(!is.na(gfr_cys)) rows <- rbind(rows, data.frame(Resultat = "CKiD U25 (cystatin C)", Value = round(gfr_cys, 0)))
      if(!is.na(gfr_scr) && !is.na(gfr_cys)) rows <- rbind(rows, data.frame(Resultat = "CKiD U25 (medel)", Value = round((gfr_scr + gfr_cys) / 2, 0)))
    }
    if ("EKFC_child" %in% formulas && age >= 2) {
      gfr_cr <- if(is_valid_num(scr_umol)) ekfc_gfr(scr_umol, q_cr_child(sex, age), -0.322, -1.132) else NA
      gfr_cys <- if(is_valid_num(scys)) ekfc_gfr(scys, 0.83, -0.322, -1.132) else NA
      if(!is.na(gfr_cr)) rows <- rbind(rows, data.frame(Resultat = "EKFC (kreatinin)", Value = round(gfr_cr, 0)))
      if(!is.na(gfr_cys)) rows <- rbind(rows, data.frame(Resultat = "EKFC (cystatin C)", Value = round(gfr_cys, 0)))
      if(!is.na(gfr_cr) && !is.na(gfr_cys)) rows <- rbind(rows, data.frame(Resultat = "EKFC (kreatinin & cystatin C)", Value = round((gfr_cr + gfr_cys) / 2, 0)))
    }
    if ("LMR18_child" %in% formulas && is_valid_num(scr_umol) && age >= 2 && age < 18) {
      rows <- rbind(rows, data.frame(Resultat = "LMR18", Value = round(lmr18_gfr_child_cr(scr_umol, sex, age), 0)))
    }
    if ("CAPA_child" %in% formulas && is_valid_num(scys) && age >= 1) {
      rows <- rbind(rows, data.frame(Resultat = "CAPA", Value = round(capa_gfr_cys(scys, age), 0)))
    }
    if ("Zappitelli" %in% formulas && age >= 8 && age <= 17) {
      if (is_valid_num(scys)) {
        gfr_cys_base <- zappitelli_gfr_cys(scys)
        rows <- rbind(rows, data.frame(Resultat = "Zappitelli (cystatin C)", Value = round(gfr_cys_base, 0)))
        rows <- rbind(rows, data.frame(Resultat = "Zappitelli (cystatin C)¹", Value = round(gfr_cys_base * 1.2, 0)))
      }
      if (is_valid_num(scys) && is_valid_num(scr_umol) && is_valid_num(height)) {
        gfr_comb_base <- zappitelli_gfr_cys_cr(scr_umol, scys, height)
        rows <- rbind(rows, data.frame(Resultat = "Zappitelli (kreatinin & cystatin C)", Value = round(gfr_comb_base, 0)))
        rows <- rbind(rows, data.frame(Resultat = "Zappitelli (kreatinin & cystatin C)¹", Value = round(gfr_comb_base * 1.165, 0)))
        gfr_comb_sb <- gfr_comb_base * (scr_umol^0.925 / 40.45)
        rows <- rbind(rows, data.frame(Resultat = "Zappitelli (kreatinin & cystatin C)²", Value = round(gfr_comb_sb, 0)))
      }
    }
    return(rows)
  }
  
  egfr_results_child <- reactive({
    req(input$age_child, input$age_child <= 18, input$sex_child, input$formulas_child)
    calc_egfr_child(input$creatinine_child, input$cysc_child, input$sex_child, input$age_child, input$height_child, input$formulas_child)
  })
  
  output$resultsTable_child <- DT::renderDataTable({
    result <- egfr_results_child()
    if (nrow(result) == 0) return(NULL)
    datatable(result, options = list(paging = FALSE, searching = FALSE, ordering = FALSE, info = FALSE, columnDefs = list(list(className = 'dt-center', targets = '_all'))), rownames = FALSE)
  })
  
  output$selectedMean_child <- renderUI({
    selected <- input$resultsTable_child_rows_selected
    if (length(selected) > 0) {
      results <- egfr_results_child()
      mean_val <- mean(as.numeric(results$Value[selected]), na.rm = TRUE)
      tags$div(style = "margin-top: 10px;", paste("Medelvärde av valda rader:", round(mean_val, 0), "mL/min/1.73m²"))
    }
  })
  
  output$footnote_child <- renderUI({
    results <- egfr_results_child()
    if (nrow(results) > 0 && any(grepl("Zappitelli", results$Resultat))) {
      tags$div(style = "font-size: 0.8em; color: #666; margin-top: 10px;", HTML("¹ Njurtransplanterad<br>² Ryggmärgsbråck"))
    }
  })
  
  # --- Iohexol Calculation Section ---
  
  observe({
    req(input$io_age, input$io_sex, input$io_weight, input$io_height, input$io_conc_inj,
        input$io_vol_inj, input$io_inj_time, input$io_bsa_formula, input$num_points)
    
    num_points <- as.numeric(input$num_points)
    sex <- input$io_sex
    weight <- as.numeric(input$io_weight)
    height <- as.numeric(input$io_height)
    iodine_mg_ml <- as.numeric(input$io_conc_inj)
    iohexol_mg_ml <- iodine_to_iohexol(iodine_mg_ml)
    dose_mg <- iohexol_mg_ml * as.numeric(input$io_vol_inj)
    bsa_formula <- input$io_bsa_formula
    
    show_error <- function(message) {
      output$io_result_ui <- renderUI({ tags$div(style = "color: red;", message) })
      output$io_plot <- renderPlot(NULL)
    }
    
    if (is.na(iohexol_mg_ml)) {
      show_error("Ogiltig iohexolkoncentration.")
      return()
    }
    
    if (num_points == 1) {
      req(input$io_sample_time1, input$io_conc_p1)
      t_diff_min <- calc_time_difference_min(input$io_inj_time, input$io_sample_time1)
      plasma_mg_L <- as.numeric(input$io_conc_p1)
      
      if (is.na(t_diff_min) || t_diff_min < 120) {
        show_error("Ogiltig tidsdifferens, provtagning före 2 timmar.")
        return()
      }
      
      result <- iohexol_one_point_gfr(sex, weight, height, dose_mg, plasma_mg_L, t_diff_min, bsa_formula)
      
      output$io_result_ui <- renderUI({
        if (any(is.na(result))) {
          tags$div(style = "color: red;", "Ogiltiga indata eller beräkning misslyckades.")
        } else {
          tags$div(
            style = "background: #f8f9fa; border-radius: 12px; padding: 24px; margin-bottom: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.04);",
            tags$div(style = "font-size: 1.8em; font-weight: bold;", "Absolut mGFR: ", tags$span(style="color: #0d6efd;", sprintf("%.1f", result$gfr), " mL/min")),
            tags$div(style = "font-size: 1.8em; font-weight: bold;", "Relativt mGFR: ", tags$span(style="color: #0d6efd;", sprintf("%.1f", result$gfr_adj), " mL/min/1.73m²")),
            tags$div(style = "font-size: 1.2em; color: #888; margin-top: 10px;", sprintf("Optimal provtagningstid: %s", format_hours_minutes(result$t_opt_hours)))
          )
        }
      })
      
      output$io_plot <- renderPlot({
        if (is.na(result$gfr)) return(NULL)
        eecv_ml <- jacobsson_eecv(sex, weight)
        
        df_sample <- data.frame(time = t_diff_min / 60, conc = plasma_mg_L)
        df_c0 <- data.frame(time = 0, conc = dose_mg / (eecv_ml / 1000))
        df_line <- rbind(df_c0, df_sample)
        
        fit <- lm(log(conc) ~ time, data = df_line)
        slope <- coef(fit)[2]
        intercept <- coef(fit)[1]
        
        t_end <- max(df_sample$time, result$t_opt_hours, na.rm = TRUE) + 0.5
        conc_end <- exp(intercept + slope * t_end)
        df_dashed <- data.frame(time = c(df_sample$time, t_end), conc = c(df_sample$conc, conc_end))
        
        t_opt <- result$t_opt_hours
        conc_opt <- exp(intercept + slope * t_opt)
        df_opt <- data.frame(time = t_opt, conc = conc_opt, label = sprintf("Optimal provtagningstid:\n%s", format_hours_minutes(result$t_opt_hours)))
        
        ggplot() +
          geom_line(data = df_line, aes(time, conc), color = "black") +
          geom_line(data = df_dashed, aes(time, conc), linetype = "dashed", color = "black") +
          geom_point(data = df_c0, aes(time, conc), size = 5, color = "black") +
          geom_point(data = df_sample, aes(time, conc), size = 5, color = "#0d6efd") +
          geom_point(data = df_opt, aes(time, conc), shape = 4, size = 4, color = "#0d6efd") +
          ggrepel::geom_label_repel(
            data = df_opt, aes(time, conc, label = label), direction = "y", nudge_y = 0.05,
            segment.color = "#0d6efd", segment.size = 0.5, min.segment.length = 0, label.padding = unit(0.4, "lines"),
            color = "#0d6efd", size = 5, label.r = unit(0.25, "lines")
          ) +
          scale_y_log10(labels = scales::number_format()) +
          scale_x_continuous(limits = c(0, max(df_sample$time, result$t_opt_hours, na.rm = TRUE) + 0.5)) +
          labs(x = "Tid (timmar)", y = "Koncentration (mg/L)") +
          theme_minimal(base_size = 14)
      })
      
    } else if (num_points %in% c(2, 4)) {
      
      format_sub_result_multiline <- function(label, result) {
        abs_gfr <- if (is_valid_num(result$gfr)) sprintf("%.1f", result$gfr) else "N/A"
        rel_gfr <- if (is_valid_num(result$gfr_adj)) sprintf("%.1f", result$gfr_adj) else "N/A"
        tags$div(
          tags$b(label),
          tags$div(
            style = "display: grid; grid-template-columns: 50px auto; align-items: center; margin-left: 10px; line-height: 1.2;",
            tags$span(style = "text-align: right; padding-right: 5px;", abs_gfr), tags$span("mL/min"),
            tags$span(style = "text-align: right; padding-right: 5px;", rel_gfr), tags$span("mL/min/1.73m²")
          )
        )
      }
      
      times_str <- sapply(1:num_points, function(i) input[[paste0("io_sample_time", i)]])
      concs_num <- sapply(1:num_points, function(i) as.numeric(input[[paste0("io_conc_p", i)]]))
      
      req(all(sapply(times_str, is_valid_hhmm)), all(is_valid_num(concs_num, min = 0, inclusive = FALSE)))
      
      times_min <- sapply(times_str, function(t) calc_time_difference_min(input$io_inj_time, t))
      
      if (any(is.na(times_min)) || any(times_min < 120)) {
        show_error("Ogiltig tidsdifferens, provtagning före 2 timmar.")
        return()
      }
      
      bsa <- bsa_select(weight, height, sex, bsa_formula)
      main_result <- slope_intercept_gfr(times_min, concs_num, dose_mg, bsa)
      
      results_1pt <- lapply(1:num_points, function(i) {
        iohexol_one_point_gfr(sex, weight, height, dose_mg, concs_num[i], times_min[i], bsa_formula)
      })
      
      output$io_result_ui <- renderUI({
        if (any(is.na(main_result))) {
          tags$div(style = "color: red;", "Ogiltiga indata eller beräkning misslyckades.")
        } else {
          sub_calcs_ui <- if (num_points == 2) {
            tagList(
              tags$button("Visa enpunktsberäkningar", class="btn btn-outline-primary btn-sm", type="button", `data-bs-toggle`="collapse", `data-bs-target`="#collapse_sub_2pt"),
              div(class="collapse", id="collapse_sub_2pt", style="margin-top: 15px;",
                  fluidRow(
                    column(6, format_sub_result_multiline("Prov 1:", results_1pt[[1]])),
                    column(6, format_sub_result_multiline("Prov 2:", results_1pt[[2]]))
                  )
              )
            )
          } else { # 4-point
            point_pairs <- list(c(1,2), c(1,3), c(1,4), c(2,3), c(2,4), c(3,4))
            results_2pt <- lapply(point_pairs, function(pair) {
              slope_intercept_gfr(times_min[pair], concs_num[pair], dose_mg, bsa)
            })
            tagList(
              div(style="display: flex; gap: 10px; margin-bottom: 15px;",
                  tags$button("Visa tvåpunktsberäkningar", class="btn btn-outline-primary btn-sm", type="button", `data-bs-toggle`="collapse", `data-bs-target`="#collapse_2pt_calcs"),
                  tags$button("Visa enpunktsberäkningar", class="btn btn-outline-primary btn-sm", type="button", `data-bs-toggle`="collapse", `data-bs-target`="#collapse_1pt_calcs")
              ),
              div(class="collapse", id="collapse_2pt_calcs",
                  h5("Tvåpunktsberäkningar", style="font-weight: bold; margin-bottom: 10px;"),
                  fluidRow(
                    column(6, format_sub_result_multiline("Prov 1 & 2:", results_2pt[[1]])),
                    column(6, format_sub_result_multiline("Prov 2 & 3:", results_2pt[[4]]))
                  ),
                  fluidRow(
                    column(6, format_sub_result_multiline("Prov 1 & 3:", results_2pt[[2]])),
                    column(6, format_sub_result_multiline("Prov 2 & 4:", results_2pt[[5]]))
                  ),
                  fluidRow(
                    column(6, format_sub_result_multiline("Prov 1 & 4:", results_2pt[[3]])),
                    column(6, format_sub_result_multiline("Prov 3 & 4:", results_2pt[[6]]))
                  ),
                  hr(style = "margin-top: 20px; margin-bottom: 20px;")
              ),
              div(class="collapse", id="collapse_1pt_calcs",
                  h5("Enpunktsberäkningar", style="font-weight: bold; margin-bottom: 10px;"),
                  fluidRow(
                    column(6, format_sub_result_multiline("Prov 1:", results_1pt[[1]])),
                    column(6, format_sub_result_multiline("Prov 3:", results_1pt[[3]]))
                  ),
                  fluidRow(
                    column(6, format_sub_result_multiline("Prov 2:", results_1pt[[2]])),
                    column(6, format_sub_result_multiline("Prov 4:", results_1pt[[4]]))
                  )
              )
            )
          }
          
          tagList(
            tags$div(
              style = "background: #f8f9fa; border-radius: 12px; padding: 24px; margin-bottom: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.04);",
              tags$div(style = "font-size: 1.8em; font-weight: bold;", "Absolut mGFR: ", tags$span(style="color: #0d6efd;", sprintf("%.1f", main_result$gfr), " mL/min")),
              tags$div(style = "font-size: 1.8em; font-weight: bold;", "Relativt mGFR: ", tags$span(style="color: #0d6efd;", sprintf("%.1f", main_result$gfr_adj), " mL/min/1.73m²")),
              hr(style = "margin-top: 20px; margin-bottom: 20px;"),
              sub_calcs_ui
            )
          )
        }
      })
      
      output$io_plot <- renderPlot({
        if (any(is.na(main_result))) return(NULL)
        df_obs <- data.frame(time_h = times_min / 60, conc = concs_num, point_num = 1:num_points)
        fit <- lm(log(conc) ~ time_h, data = df_obs)
        
        t_end_h <- max(df_obs$time_h) + 0.5
        pred_times_h <- seq(0, t_end_h, length.out = 200)
        pred_log <- predict(fit, newdata = data.frame(time_h = pred_times_h))
        df_pred <- data.frame(time_h = pred_times_h, conc = exp(pred_log))
        
        ggplot() +
          geom_line(data = df_pred, aes(x = time_h, y = conc), color = "black") +
          geom_point(data = df_obs, aes(x = time_h, y = conc), size = 5, color = "#0d6efd") +
          geom_text(data = df_obs, aes(x = time_h, y = conc, label = point_num), color = "white", fontface = "bold", size = 4) +
          scale_y_log10(labels = scales::number_format()) +
          labs(x = "Tid (timmar)", y = "Koncentration (mg/L)") +
          theme_minimal(base_size = 14)
      })
    }
  })
  
  # --- Other Calculations Section ---
  
  cg_result <- reactive({
    req(input$cg_age, input$cg_weight, input$cg_creatinine, input$cg_sex)
    calc_cockcroft_gault(input$cg_age, input$cg_weight, input$cg_creatinine, input$cg_sex)
  })
  
  output$cg_result_ui <- renderUI({
    res <- cg_result()
    if (is_valid_num(res)) {
      tags$div(
        style = "background: #f8f9fa; border-radius: 12px; padding: 24px; box-shadow: 0 2px 8px rgba(0,0,0,0.04);",
        tags$div(style = "font-size: 1.5em; font-weight: bold;", "Kreatininclearance: ", tags$span(style="color: #0d6efd;", sprintf("%.1f", res), tags$span(style = "font-size: 0.6em; font-weight: normal; margin-left: 2px;", "mL/min")))
      )
    }
  })
  
  gfr_conv_result <- reactive({
    req(input$gfr_conv_direction, input$gfr_conv_height, input$gfr_conv_weight, input$gfr_conv_sex, input$gfr_conv_bsa_formula)
    bsa <- bsa_select(input$gfr_conv_weight, input$gfr_conv_height, input$gfr_conv_sex, input$gfr_conv_bsa_formula)
    if (!is_valid_num(bsa)) return(list(value = NA, label = NA, unit = NA))
    
    if (input$gfr_conv_direction == "abs_to_rel") {
      req(input$gfr_abs)
      val <- input$gfr_abs * (1.73 / bsa)
      list(value = val, label = "Relativ GFR", unit = "mL/min/1.73m²")
    } else {
      req(input$gfr_rel)
      val <- input$gfr_rel * bsa / 1.73
      list(value = val, label = "Absolut GFR", unit = "mL/min")
    }
  })
  
  output$gfr_conv_result_ui <- renderUI({
    res <- gfr_conv_result()
    if (is_valid_num(res$value)) {
      tags$div(
        style = "background: #f8f9fa; border-radius: 12px; padding: 24px; box-shadow: 0 2px 8px rgba(0,0,0,0.04);",
        tags$div(style = "font-size: 1.5em; font-weight: bold;", paste0(res$label, ": "), tags$span(style="color: #0d6efd;", sprintf("%.1f", res$value), tags$span(style = "font-size: 0.6em; font-weight: normal; margin-left: 2px;", res$unit)))
      )
    }
  })
  
  # --- Controls / clear buttons ---
  
  observeEvent(input$io_clear, {
    updateNumericInput(session, "io_age", value = NA)
    updateSelectInput(session, "io_sex", selected = "Man")
    updateNumericInput(session, "io_weight", value = NA)
    updateNumericInput(session, "io_height", value = NA)
    updateSelectInput(session, "io_conc_inj", selected = "300")
    updateNumericInput(session, "io_vol_inj", value = 4)
    updateTextInput(session, "io_inj_time", value = "")
    updateSelectInput(session, "io_bsa_formula", selected = "dubois")
    for (i in 1:4) {
      updateTextInput(session, paste0("io_sample_time", i), value = "")
      updateNumericInput(session, paste0("io_conc_p", i), value = NA)
    }
    output$io_result_ui <- renderUI({ NULL })
    output$io_plot <- renderPlot({ NULL })
  })
  
  observeEvent(input$clear_adult, {
    updateNumericInput(session, "age", value = NA)
    updateSelectInput(session, "sex", selected = "Man")
    updateNumericInput(session, "creatinine", value = NA)
    updateNumericInput(session, "cysc", value = NA)
    updateCheckboxGroupInput(session, "formulas_adult", selected = c("EKFC"))
    DT::selectRows(proxy = DT::dataTableProxy("resultsTable_adult"), selected = NULL)
  })
  
  observeEvent(input$clear_selection_adult, {
    DT::selectRows(proxy = DT::dataTableProxy("resultsTable_adult"), selected = NULL)
  })
  
  observeEvent(input$clear_child, {
    updateNumericInput(session, "age_child", value = NA)
    updateSelectInput(session, "sex_child", selected = "Man")
    updateNumericInput(session, "height_child", value = NA)
    updateNumericInput(session, "creatinine_child", value = NA)
    updateNumericInput(session, "cysc_child", value = NA)
    updateCheckboxGroupInput(session, "formulas_child", selected = "Schwartz")
    DT::selectRows(proxy = DT::dataTableProxy("resultsTable_child"), selected = NULL)
  })
  
  observeEvent(input$clear_selection_child, {
    DT::selectRows(proxy = DT::dataTableProxy("resultsTable_child"), selected = NULL)
  })
  
  observeEvent(input$clear_cg, {
    updateNumericInput(session, "cg_age", value = NA)
    updateSelectInput(session, "cg_sex", selected = "Man")
    updateNumericInput(session, "cg_weight", value = NA)
    updateNumericInput(session, "cg_creatinine", value = NA)
  })
  
  observeEvent(input$clear_gfr_conv, {
    updateSelectInput(session, "gfr_conv_direction", selected = "abs_to_rel")
    updateNumericInput(session, "gfr_abs", value = NA)
    updateNumericInput(session, "gfr_rel", value = NA)
    updateNumericInput(session, "gfr_conv_height", value = NA)
    updateNumericInput(session, "gfr_conv_weight", value = NA)
    updateSelectInput(session, "gfr_conv_sex", selected = "Man")
    updateSelectInput(session, "gfr_conv_bsa_formula", selected = "dubois")
  })
}