# Updated server.R with new calculator logic and updated clear button behavior
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
    updateSelectInput(session, "opt_sex", selected = "Man")
    updateSelectInput(session, "opt_bsa_formula", selected = "dubois")
    
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
      if (is_valid_num(scys)) rows <- rbind(rows, data.frame(Resultat = "FAS (cystatin C)", Value = round(fas_gfr_cys(scys, age), 0)))
      if (is_valid_num(scr_mgdl) && is_valid_num(scys)) rows <- rbind(rows, data.frame(Resultat = "FAS (kreatinin & cystatin C)", Value = round((fas_gfr_cr(scr_mgdl, age) + fas_gfr_cys(scys, age)) / 2, 0)))
    }
    if ("r-LMR" %in% formulas) {
      if (is_valid_num(scr_umol)) rows <- rbind(rows, data.frame(Resultat = "r-LMR (kreatinin)", Value = round(r_lmr_gfr_cr(scr_umol, sex, age), 0)))
      if (is_valid_num(scys)) {
        rows <- rbind(rows, data.frame(Resultat = "r-LMR (cystatin C)", Value = round(r_lmr_gfr_cys(scys, age, 0.83), 0)))
        if (!is.na(q_age_adj_rlmr)) rows <- rbind(rows, data.frame(Resultat = "r-LMR (cystatin C)*", Value = round(r_lmr_gfr_cys(scys, age, q_age_adj_rlmr), 0)))
      }
      if (is_valid_num(scr_umol) && is_valid_num(scys)) rows <- rbind(rows, data.frame(Resultat = "r-LMR (kreatinin & cystatin C)", Value = round((r_lmr_gfr_cr(scr_umol, sex, age) + r_lmr_gfr_cys(scys, age, 0.83)) / 2, 0)))
    }
    if ("CKD-EPI (2021)" %in% formulas) {
      if (is_valid_num(scr_umol)) rows <- rbind(rows, data.frame(Resultat = "CKD-EPI (kreatinin)", Value = round(ckdepi2021_gfr_cr(scr_umol, sex, age), 0)))
      if (is_valid_num(scys)) rows <- rbind(rows, data.frame(Resultat = "CKD-EPI (cystatin C)", Value = round(ckdepi2021_gfr_cys(scys, sex, age), 0)))
      if (is_valid_num(scr_umol) && is_valid_num(scys)) rows <- rbind(rows, data.frame(Resultat = "CKD-EPI (kreatinin & cystatin C)", Value = round(ckdepi2021_gfr_cr_cys(scr_umol, scys, sex, age), 0)))
    }
    if ("LMR18" %in% formulas) {
      if (is_valid_num(scr_umol)) rows <- rbind(rows, data.frame(Resultat = "LMR18 (kreatinin)", Value = round(lmr18_gfr_cr(scr_umol, sex, age), 0)))
    }
    if ("CAPA" %in% formulas) {
      if (is_valid_num(scys)) rows <- rbind(rows, data.frame(Resultat = "CAPA (cystatin C)", Value = round(capa_gfr_cys(scys, age), 0)))
    }
    if ("MDRD" %in% formulas) {
      if (is_valid_num(scr_umol)) rows <- rbind(rows, data.frame(Resultat = "MDRD (kreatinin)", Value = round(mdrd_gfr(scr_umol, sex, age), 0)))
    }
    if ("BIS" %in% formulas) {
      if (is_valid_num(scr_umol)) rows <- rbind(rows, data.frame(Resultat = "BIS (kreatinin)", Value = round(bis_gfr(scr_umol, sex, age), 0)))
    }
    rows
  }
  
  results_adult <- reactive({
    req(input$age >= 18)
    calc_egfr_adult(input$creatinine, input$cysc, input$sex, input$age, input$formulas_adult)
  })
  
  output$resultsTable_adult <- DT::renderDataTable({
    datatable(results_adult(), options = list(pageLength = 20, searching = FALSE, paging = FALSE, info = FALSE, ordering = FALSE), rownames = FALSE, selection = 'multiple')
  })
  
  output$selectedMean_adult <- renderUI({
    selected_rows <- input$resultsTable_adult_rows_selected
    if (length(selected_rows) > 0) {
      selected_values <- as.numeric(results_adult()$Value[selected_rows])
      mean_val <- mean(selected_values, na.rm = TRUE)
      tags$div(style = "margin-top: 10px; font-weight: bold;", sprintf("Medelvärde av valda: %.1f", mean_val))
    }
  })
  
  output$asteriskNote_adult <- renderUI({
    if (any(grepl("\\*", results_adult()$Resultat))) {
      tags$p(style = "font-size: 0.8em; color: #666;", "* Ålders- och/eller könskorrigerad Q-värde för cystatin C")
    }
  })
  
  # --- eGFR (Children) Section ---
  
  output$child_results_ui <- renderUI({
    age_val <- input$age_child
    if (is.na(age_val)) {
      tagList(
        DT::dataTableOutput("resultsTable_child"),
        uiOutput("selectedMean_child"),
        uiOutput("asteriskNote_child"),
        conditionalPanel(
          condition = "input.resultsTable_child_rows_selected && input.resultsTable_child_rows_selected.length > 0",
          actionButton("clear_selection_child", "Rensa val", class = "action-button-mini")
        )
      )
    } else if (age_val >= 18) {
      tags$div(class = "alert alert-warning", role = "alert", "Använd beräkning för vuxna")
    } else {
      tagList(
        DT::dataTableOutput("resultsTable_child"),
        uiOutput("selectedMean_child"),
        uiOutput("asteriskNote_child"),
        conditionalPanel(
          condition = "input.resultsTable_child_rows_selected && input.resultsTable_child_rows_selected.length > 0",
          actionButton("clear_selection_child", "Rensa val", class = "action-button-mini")
        )
      )
    }
  })
  
  calc_egfr_child <- function(scr_umol, scys, sex, age, height_cm, formulas) {
    rows <- data.frame(Resultat = character(), Value = character(), stringsAsFactors = FALSE)
    scr_mgdl <- if (is_valid_num(scr_umol)) scr_umol / 88.4 else NA
    height_m <- if (is_valid_num(height_cm)) height_cm / 100 else NA
    
    if ("Schwartz" %in% formulas) {
      if (is_valid_num(height_cm) && is_valid_num(scr_mgdl)) rows <- rbind(rows, data.frame(Resultat = "Schwartz (kreatinin)", Value = round(schwartz_gfr(height_cm, scr_mgdl), 0)))
    }
    if ("CKiD_U25" %in% formulas) {
      if (is_valid_num(height_m) && is_valid_num(scr_mgdl)) rows <- rbind(rows, data.frame(Resultat = "CKiD U25 (kreatinin)", Value = round(ckid_u25_scr(sex, age, height_m, scr_mgdl), 0)))
      if (is_valid_num(scys)) rows <- rbind(rows, data.frame(Resultat = "CKiD U25 (cystatin C)", Value = round(ckid_u25_cys(sex, age, scys), 0)))
      if (is_valid_num(height_m) && is_valid_num(scr_mgdl) && is_valid_num(scys)) rows <- rbind(rows, data.frame(Resultat = "CKiD U25 (kreatinin & cystatin C)", Value = round(0.5 * ckid_u25_scr(sex, age, height_m, scr_mgdl) + 0.5 * ckid_u25_cys(sex, age, scys), 0)))
    }
    if ("EKFC_child" %in% formulas) {
      if (is_valid_num(scr_mgdl)) rows <- rbind(rows, data.frame(Resultat = "EKFC (kreatinin)", Value = round(ekfc_gfr(scr_mgdl, q_cr_child(sex, age) / 88.4, -0.322, -1.132), 0)))
      if (is_valid_num(scys)) rows <- rbind(rows, data.frame(Resultat = "EKFC (cystatin C)", Value = round(ekfc_gfr(scys, 0.83, -0.322, -1.132), 0)))
      if (is_valid_num(scr_mgdl) && is_valid_num(scys)) rows <- rbind(rows, data.frame(Resultat = "EKFC (kreatinin & cystatin C)", Value = round((ekfc_gfr(scr_mgdl, q_cr_child(sex, age) / 88.4, -0.322, -1.132) + ekfc_gfr(scys, 0.83, -0.322, -1.132)) / 2, 0)))
    }
    if ("LMR18_child" %in% formulas) {
      if (is_valid_num(scr_umol)) rows <- rbind(rows, data.frame(Resultat = "LMR18 (kreatinin)", Value = round(lmr18_gfr_child_cr(scr_umol, sex, age), 0)))
    }
    if ("CAPA_child" %in% formulas) {
      if (is_valid_num(scys)) rows <- rbind(rows, data.frame(Resultat = "CAPA (cystatin C)", Value = round(capa_gfr_cys(scys, age), 0)))
    }
    if ("Zappitelli" %in% formulas) {
      if (is_valid_num(scys)) rows <- rbind(rows, data.frame(Resultat = "Zappitelli (cystatin C)", Value = round(zappitelli_gfr_cys(scys), 0)))
      if (is_valid_num(scr_umol) && is_valid_num(scys) && is_valid_num(height_cm)) rows <- rbind(rows, data.frame(Resultat = "Zappitelli (kreatinin & cystatin C)", Value = round(zappitelli_gfr_cys_cr(scr_umol, scys, height_cm), 0)))
    }
    rows
  }
  
  results_child <- reactive({
    req(input$age_child < 18)
    calc_egfr_child(input$creatinine_child, input$cysc_child, input$sex_child, input$age_child, input$height_child, input$formulas_child)
  })
  
  output$resultsTable_child <- DT::renderDataTable({
    datatable(results_child(), options = list(pageLength = 20, searching = FALSE, paging = FALSE, info = FALSE, ordering = FALSE), rownames = FALSE, selection = 'multiple')
  })
  
  output$selectedMean_child <- renderUI({
    selected_rows <- input$resultsTable_child_rows_selected
    if (length(selected_rows) > 0) {
      selected_values <- as.numeric(results_child()$Value[selected_rows])
      mean_val <- mean(selected_values, na.rm = TRUE)
      tags$div(style = "margin-top: 10px; font-weight: bold;", sprintf("Medelvärde av valda: %.1f", mean_val))
    }
  })
  
  output$asteriskNote_child <- renderUI({
    if (any(grepl("\\*", results_child()$Resultat))) {
      tags$p(style = "font-size: 0.8em; color: #666;", "* Ålders- och/eller könskorrigerad Q-värde för cystatin C")
    }
  })
  
  # --- Iohexol Section ---
  
  io_calc_reactive <- reactive({
    req(input$io_age >= 18, input$io_sex, input$io_weight, input$io_height, input$io_conc_inj, input$io_vol_inj, input$io_inj_time, input$io_bsa_formula)
    
    iodine_conc <- as.numeric(input$io_conc_inj)
    iohexol_conc <- iodine_to_iohexol(iodine_conc)
    dose_mg <- iohexol_conc * input$io_vol_inj
    
    num_points <- as.numeric(input$num_points)
    
    times_str <- paste0("io_sample_time", 1:num_points)
    concs_str <- paste0("io_conc_p", 1:num_points)
    
    times_min <- sapply(1:num_points, function(i) calc_time_difference_min(input$io_inj_time, input[[times_str[i]]]))
    concs_num <- sapply(1:num_points, function(i) input[[concs_str[i]]])
    
    if (any(is.na(times_min)) || any(times_min <= 0) || any(is.na(concs_num)) || any(concs_num <= 0)) return(list(gfr = NA, gfr_adj = NA, t_opt_hours = NA))
    
    bsa <- bsa_select(input$io_weight, input$io_height, input$io_sex, input$io_bsa_formula)
    
    if (num_points == 1) {
      iohexol_one_point_gfr(input$io_sex, input$io_weight, input$io_height, dose_mg, concs_num[1], times_min[1], input$io_bsa_formula)
    } else {
      slope_intercept_gfr(times_min, concs_num, dose_mg, bsa)
    }
  })
  
  output$io_result_ui <- renderUI({
    res <- io_calc_reactive()
    if (is_valid_num(res$gfr)) {
      tags$div(
        style = "background: #f8f9fa; border-radius: 12px; padding: 24px; box-shadow: 0 2px 8px rgba(0,0,0,0.04);",
        tags$div(style = "font-size: 1.5em; font-weight: bold;", "Absolut GFR: ", tags$span(style="color: #0d6efd;", sprintf("%.1f", res$gfr), tags$span(style = "font-size: 0.6em; font-weight: normal; margin-left: 2px;", "mL/min"))),
        tags$div(style = "font-size: 1.5em; font-weight: bold;", "Relativ GFR: ", tags$span(style="color: #0d6efd;", sprintf("%.1f", res$gfr_adj), tags$span(style = "font-size: 0.6em; font-weight: normal; margin-left: 2px;", "mL/min/1.73m²"))),
        if (input$num_points == 1 && is_valid_num(res$t_opt_hours)) {
          tags$div(style = "font-size: 1.2em; margin-top: 10px;", "Optimal tid: ", tags$span(style="color: #0d6efd;", format_hours_minutes(res$t_opt_hours)))
        }
      )
    }
  })
  
  output$io_plot <- renderPlot({
    if (input$num_points == 1) return(NULL)
    
    res <- io_calc_reactive()
    if (!is_valid_num(res$gfr)) return(NULL)
    
    num_points <- as.numeric(input$num_points)
    times_str <- paste0("io_sample_time", 1:num_points)
    concs_str <- paste0("io_conc_p", 1:num_points)
    
    times_min <- sapply(1:num_points, function(i) calc_time_difference_min(input$io_inj_time, input[[times_str[i]]]))
    concs_num <- sapply(1:num_points, function(i) input[[concs_str[i]]])
    
    times_h <- times_min / 60
    
    df_obs <- data.frame(time_h = times_h, conc = concs_num, point_num = 1:num_points)
    
    fit <- lm(log(conc) ~ time_h, data = df_obs)
    pred_line <- data.frame(time_h = seq(min(times_h), max(times_h), length.out = 100))
    pred_line$conc <- exp(predict(fit, newdata = pred_line))
    
    ggplot() +
      geom_line(data = pred_line, aes(x = time_h, y = conc), color = "blue") +
      geom_point(data = df_obs, aes(x = time_h, y = conc), color = "red", size = 3) +
      geom_text_repel(data = df_obs, aes(x = time_h, y = conc, label = point_num), color = "white", fontface = "bold", size = 4) +
      scale_y_log10(labels = scales::number_format()) +
      labs(x = "Tid (timmar)", y = "Koncentration (mg/L)") +
      theme_minimal(base_size = 14)
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
  
  opt_result <- reactive({
    req(input$opt_age, input$opt_sex, input$opt_weight, input$opt_height, input$opt_egfr, input$opt_bsa_formula)
    eecv <- jacobsson_eecv(input$opt_sex, input$opt_weight)
    bsa <- bsa_select(input$opt_weight, input$opt_height, input$opt_sex, input$opt_bsa_formula)
    if (!is_valid_num(eecv) || !is_valid_num(bsa)) return(NA)
    abs_gfr <- input$opt_egfr * bsa / 1.73
    if (!is_valid_num(abs_gfr) || abs_gfr <= 0) return(NA)
    t_opt_min <- eecv / abs_gfr
    t_opt_hours <- t_opt_min / 60
    format_hours_minutes(t_opt_hours)
  })
  
  output$opt_result_ui <- renderUI({
    res <- opt_result()
    if (!is.na(res)) {
      tags$div(
        style = "background: #f8f9fa; border-radius: 12px; padding: 24px; box-shadow: 0 2px 8px rgba(0,0,0,0.04);",
        tags$div(style = "font-size: 1.5em; font-weight: bold;", "Optimal tid: ", tags$span(style="color: #0d6efd;", res))
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
    output$cg_result_ui <- renderUI({ NULL })
  })
  
  observeEvent(input$clear_opt, {
    updateNumericInput(session, "opt_age", value = NA)
    updateSelectInput(session, "opt_sex", selected = "Man")
    updateNumericInput(session, "opt_weight", value = NA)
    updateNumericInput(session, "opt_height", value = NA)
    updateNumericInput(session, "opt_egfr", value = NA)
    updateSelectInput(session, "opt_bsa_formula", selected = "dubois")
    output$opt_result_ui <- renderUI({ NULL })
  })
  
  observeEvent(input$clear_gfr_conv, {
    updateSelectInput(session, "gfr_conv_direction", selected = "abs_to_rel")
    updateNumericInput(session, "gfr_abs", value = NA)
    updateNumericInput(session, "gfr_rel", value = NA)
    updateNumericInput(session, "gfr_conv_height", value = NA)
    updateNumericInput(session, "gfr_conv_weight", value = NA)
    updateSelectInput(session, "gfr_conv_sex", selected = "Man")
    updateSelectInput(session, "gfr_conv_bsa_formula", selected = "dubois")
    output$gfr_conv_result_ui <- renderUI({ NULL })
  })
}