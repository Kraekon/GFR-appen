# Updated server.R with Övrigt calculations in an observe block to ensure re-computation after clear.

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
    
    # Initially disable BIS for adults
    runjs("$('#formulas_adult input[value=\"BIS\"]').prop('disabled', true);")
    
    # Initially disable all conditional child formulas
    runjs("$('#formulas_child input[value=\"CKiD_U25\"]').prop('disabled', true);")
    runjs("$('#formulas_child input[value=\"EKFC_child\"]').prop('disabled', true);")
    runjs("$('#formulas_child input[value=\"LMR18_child\"]').prop('disabled', true);")
    runjs("$('#formulas_child input[value=\"CAPA_child\"]').prop('disabled', true);")
    runjs("$('#formulas_child input[value=\"Zappitelli\"]').prop('disabled', true);")
    
  }, once = TRUE)
  
  # --- Observer to conditionally enable/disable BIS formula ---
  observeEvent(input$age, {
    age_val <- input$age
    if (is.na(age_val) || age_val < 70) {
      runjs("$('#formulas_adult input[value=\"BIS\"]').prop('disabled', true);")
      updateCheckboxGroupInput(session, "formulas_adult", selected = setdiff(input$formulas_adult, "BIS"))
    } else {
      runjs("$('#formulas_adult input[value=\"BIS\"]').prop('disabled', false);")
    }
  }, ignoreNULL = FALSE)
  
  # --- Observer to conditionally enable/disable Pediatric formulas ---
  observeEvent(input$age_child, {
    age <- input$age_child
    
    manage_checkbox <- function(value, condition) {
      if (is.na(age) || !condition) {
        runjs(sprintf("$('#formulas_child input[value=\"%s\"]').prop('disabled', true);", value))
        updateCheckboxGroupInput(session, "formulas_child", selected = setdiff(input$formulas_child, value))
      } else {
        runjs(sprintf("$('#formulas_child input[value=\"%s\"]').prop('disabled', false);", value))
      }
    }
    
    manage_checkbox("CKiD_U25", age >= 1)
    manage_checkbox("EKFC_child", age >= 2)
    manage_checkbox("LMR18_child", age >= 2 && age < 18)
    manage_checkbox("CAPA_child", age >= 1)
    manage_checkbox("Zappitelli", age >= 8 && age <= 17)
    
  }, ignoreNULL = FALSE)
  
  # --- eGFR (Adults) Section ---
  
  output$adult_results_ui <- renderUI({
    age_val <- input$age
    results <- tryCatch(results_adult(), error = function(e) NULL)
    
    if (is.na(age_val)) {
      return(NULL)
    } else if (age_val < 18) {
      tags$div(class = "alert alert-warning", role = "alert", "Använd beräkning för barn")
    } else if (!is.null(results) && nrow(results) > 0 && any(is_valid_num(results$Resultat))) {
      tagList(
        DT::dataTableOutput("resultsTable_adult"),
        uiOutput("selectedMean_adult"),
        uiOutput("asteriskNote_adult"),
        conditionalPanel(
          condition = "input.resultsTable_adult_rows_selected && input.resultsTable_adult_rows_selected.length > 0",
          actionButton("clear_selection_adult", "Rensa val", class = "action-button-mini")
        )
      )
    } else {
      return(NULL)
    }
  })
  
  calc_egfr_adult <- function(scr_umol, scys, sex, age, formulas) {
    rows <- data.frame(Formel = character(), Resultat = character(), stringsAsFactors = FALSE)
    scr_mgdl <- if (is_valid_num(scr_umol)) scr_umol / 88.4 else NA
    
    # Centralized Q-value calculations
    q_sex_age_adj <- calculate_q_cysc(age, sex)
    q_age_adj_rlmr <- calculate_q_cysc(age)
    
    if ("EKFC" %in% formulas) {
      if (is_valid_num(scr_mgdl)) {
        gfr_cr <- ekfc_gfr(scr_mgdl, ifelse(sex == "Man", 0.9, 0.7), -0.322, -1.132)
        rows <- rbind(rows, data.frame(Formel = "EKFC (kreatinin)", Resultat = round(gfr_cr, 0)))
      }
      if (is_valid_num(scys)) {
        gfr_cys <- ekfc_gfr(scys, 0.83, -0.322, -1.132)
        rows <- rbind(rows, data.frame(Formel = "EKFC (cystatin C)", Resultat = round(gfr_cys, 0)))
        if (!is.na(q_sex_age_adj)) {
          gfr_cys_sexadj <- ekfc_gfr(scys, q_sex_age_adj, -0.322, -1.132)
          rows <- rbind(rows, data.frame(Formel = "EKFC (cystatin C)*", Resultat = round(gfr_cys_sexadj, 0)))
        }
      }
      if (is_valid_num(scr_mgdl) && is_valid_num(scys)) {
        gfr_cr <- ekfc_gfr(scr_mgdl, ifelse(sex == "Man", 0.9, 0.7), -0.322, -1.132)
        gfr_cys <- ekfc_gfr(scys, 0.83, -0.322, -1.132)
        rows <- rbind(rows, data.frame(Formel = "EKFC (kreatinin & cystatin C)", Resultat = round((gfr_cr + gfr_cys) / 2, 0)))
        if (!is.na(q_sex_age_adj)) {
          gfr_cys_sexadj <- ekfc_gfr(scys, q_sex_age_adj, -0.322, -1.132)
          rows <- rbind(rows, data.frame(Formel = "EKFC (kreatinin & cystatin C)*", Resultat = round((gfr_cr + gfr_cys_sexadj) / 2, 0)))
        }
      }
    }
    if ("FAS" %in% formulas) {
      if (is_valid_num(scr_mgdl)) rows <- rbind(rows, data.frame(Formel = "FAS (kreatinin)", Resultat = round(fas_gfr_cr(scr_mgdl, age), 0)))
      if (is_valid_num(scys)) rows <- rbind(rows, data.frame(Formel = "FAS (cystatin C)", Resultat = round(fas_gfr_cys(scys, age), 0)))
      if (is_valid_num(scr_mgdl) && is_valid_num(scys)) rows <- rbind(rows, data.frame(Formel = "FAS (kreatinin & cystatin C)", Resultat = round((fas_gfr_cr(scr_mgdl, age) + fas_gfr_cys(scys, age)) / 2, 0)))
    }
    if ("r-LMR" %in% formulas) {
      if (is_valid_num(scr_umol)) rows <- rbind(rows, data.frame(Formel = "r-LMR (kreatinin)", Resultat = round(r_lmr_gfr_cr(scr_umol, sex, age), 0)))
      if (is_valid_num(scys)) rows <- rbind(rows, data.frame(Formel = "r-LMR (cystatin C)", Resultat = round(r_lmr_gfr_cys(scys, age, q_age_adj_rlmr), 0)))
      if (is_valid_num(scr_umol) && is_valid_num(scys)) rows <- rbind(rows, data.frame(Formel = "r-LMR (kreatinin & cystatin C)", Resultat = round((r_lmr_gfr_cr(scr_umol, sex, age) + r_lmr_gfr_cys(scys, age, q_age_adj_rlmr)) / 2, 0)))
    }
    if ("CKD-EPI (2021)" %in% formulas) {
      if (is_valid_num(scr_umol)) rows <- rbind(rows, data.frame(Formel = "CKD-EPI (kreatinin)", Resultat = round(ckdepi2021_gfr_cr(scr_umol, sex, age), 0)))
      if (is_valid_num(scys)) rows <- rbind(rows, data.frame(Formel = "CKD-EPI (cystatin C)", Resultat = round(ckdepi2021_gfr_cys(scys, sex, age), 0)))
      if (is_valid_num(scr_umol) && is_valid_num(scys)) rows <- rbind(rows, data.frame(Formel = "CKD-EPI (kreatinin & cystatin C)", Resultat = round(ckdepi2021_gfr_cr_cys(scr_umol, scys, sex, age), 0)))
    }
    if ("LMR18" %in% formulas) {
      if (is_valid_num(scr_umol)) rows <- rbind(rows, data.frame(Formel = "LMR18 (kreatinin)", Resultat = round(lmr18_gfr_cr(scr_umol, sex, age), 0)))
    }
    if ("CAPA" %in% formulas) {
      if (is_valid_num(scys)) rows <- rbind(rows, data.frame(Formel = "CAPA (cystatin C)", Resultat = round(capa_gfr_cys(scys, age), 0)))
    }
    if ("MDRD" %in% formulas) {
      if (is_valid_num(scr_umol)) rows <- rbind(rows, data.frame(Formel = "MDRD (kreatinin)", Resultat = round(mdrd_gfr(scr_umol, sex, age), 0)))
    }
    if ("BIS" %in% formulas) {
      if (is_valid_num(scr_umol)) rows <- rbind(rows, data.frame(Formel = "BIS (kreatinin)", Resultat = round(bis_gfr(scr_umol, sex, age), 0)))
    }
    return(rows)
  }
  
  results_adult <- reactive({
    scr_umol <- input$creatinine
    scys <- input$cysc
    age <- input$age
    sex <- input$sex
    formulas <- input$formulas_adult
    if (is.null(age) || is.null(sex) || is.null(formulas)) return(data.frame(Formel = character(), Resultat = character()))
    calc_egfr_adult(scr_umol, scys, sex, age, formulas)
  })
  
  output$resultsTable_adult <- DT::renderDataTable({
    results <- results_adult()
    if (nrow(results) == 0) return(NULL)
    datatable(results, options = list(paging = FALSE, searching = FALSE, ordering = FALSE, info = FALSE, columnDefs = list(list(className = 'dt-center', targets = '_all'))), rownames = FALSE, colnames = c("Formel", "Resultat (mL/min/1.73m²)"))
  })
  
  output$selectedMean_adult <- renderUI({
    selected <- input$resultsTable_adult_rows_selected
    if (length(selected) > 0) {
      results <- results_adult()
      mean_val <- mean(as.numeric(results$Resultat[selected]), na.rm = TRUE)
      tags$div(style = "margin-top: 10px;", paste("Medelvärde av valda rader:", round(mean_val, 0), "mL/min/1.73m²"))
    }
  })
  
  output$asteriskNote_adult <- renderUI({
    results <- results_adult()
    if (nrow(results) > 0 && any(grepl("\\*", results$Formel))) {
      tags$div(style = "font-size: 0.8em; color: #666; margin-top: 10px;", "* Beräknat med köns- och åldersjusterad Q-faktor för cystatin C")
    }
  })
  
  observeEvent(input$clear_selection_adult, {
    DT::selectRows(proxy = DT::dataTableProxy("resultsTable_adult"), selected = NULL)
  })
  
  # --- eGFR (Children) Section ---
  
  output$child_results_ui <- renderUI({
    age_val <- input$age_child
    results <- tryCatch(results_child(), error = function(e) NULL)
    
    if (is.na(age_val)) {
      return(NULL)
    } else if (age_val > 18) {
      tags$div(class = "alert alert-warning", role = "alert", "Använd beräkning för vuxna")
    } else if (!is.null(results) && nrow(results) > 0 && any(is_valid_num(results$Resultat))) {
      tagList(
        DT::dataTableOutput("resultsTable_child"),
        uiOutput("selectedMean_child"),
        uiOutput("footnote_child"),
        conditionalPanel(
          condition = "input.resultsTable_child_rows_selected && input.resultsTable_child_rows_selected.length > 0",
          actionButton("clear_selection_child", "Rensa val", class = "action-button-mini")
        )
      )
    } else {
      return(NULL)
    }
  })
  
  calc_egfr_child <- function(scr_umol, scys, sex, age, height_cm, formulas) {
    rows <- data.frame(Formel = character(), Resultat = character(), stringsAsFactors = FALSE)
    scr_mgdl <- if (is_valid_num(scr_umol)) scr_umol / 88.4 else NA
    height_m <- if (is_valid_num(height_cm)) height_cm / 100 else NA
    
    if ("Schwartz" %in% formulas) {
      if (is_valid_num(scr_mgdl) && is_valid_num(height_cm)) {
        rows <- rbind(rows, data.frame(Formel = "Schwartz (0 - 18 år)", Resultat = round(schwartz_gfr(height_cm, scr_mgdl), 0)))
      }
    }
    if ("CKiD_U25" %in% formulas && age >= 1) {
      if (is_valid_num(scr_mgdl) && is_valid_num(height_m)) {
        rows <- rbind(rows, data.frame(Formel = "CKiD U25 (kreatinin)", Resultat = round(ckid_u25_scr(sex, age, height_m, scr_mgdl), 0)))
      }
      if (is_valid_num(scys)) {
        rows <- rbind(rows, data.frame(Formel = "CKiD U25 (cystatin C)", Resultat = round(ckid_u25_cys(sex, age, scys), 0)))
      }
      if (is_valid_num(scr_mgdl) && is_valid_num(height_m) && is_valid_num(scys)) {
        rows <- rbind(rows, data.frame(Formel = "CKiD U25 (medel)", Resultat = round((ckid_u25_scr(sex, age, height_m, scr_mgdl) + ckid_u25_cys(sex, age, scys)) / 2, 0)))
      }
    }
    if ("EKFC_child" %in% formulas && age >= 2) {
      if (is_valid_num(scr_umol)) {
        rows <- rbind(rows, data.frame(Formel = "EKFC (kreatinin)", Resultat = round(ekfc_gfr(scr_umol / 88.4, q_cr_child(sex, age) / 88.4, -0.322, -1.132), 0)))
      }
      if (is_valid_num(scys)) {
        rows <- rbind(rows, data.frame(Formel = "EKFC (cystatin C)", Resultat = round(ekfc_gfr(scys, 0.83, -0.322, -1.132), 0)))
      }
      if (is_valid_num(scr_umol) && is_valid_num(scys)) {
        rows <- rbind(rows, data.frame(Formel = "EKFC (kreatinin & cystatin C)", Resultat = round((ekfc_gfr(scr_umol / 88.4, q_cr_child(sex, age) / 88.4, -0.322, -1.132) + ekfc_gfr(scys, 0.83, -0.322, -1.132)) / 2, 0)))
      }
    }
    if ("LMR18_child" %in% formulas && age >= 2 && age < 18) {
      if (is_valid_num(scr_umol)) {
        rows <- rbind(rows, data.frame(Formel = "LMR18 (2-17 år)", Resultat = round(lmr18_gfr_child_cr(scr_umol, sex, age), 0)))
      }
    }
    if ("CAPA_child" %in% formulas && age >= 1) {
      if (is_valid_num(scys)) {
        rows <- rbind(rows, data.frame(Formel = "CAPA (≥1 år)", Resultat = round(capa_gfr_cys(scys, age), 0)))
      }
    }
    if ("Zappitelli" %in% formulas && age >= 8 && age <= 17) {
      if (is_valid_num(scys)) {
        rows <- rbind(rows, data.frame(Formel = "Zappitelli (cystatin C)", Resultat = round(zappitelli_gfr_cys(scys), 0)))
      }
      if (is_valid_num(scys) && is_valid_num(scr_umol) && is_valid_num(height_cm)) {
        rows <- rbind(rows, data.frame(Formel = "Zappitelli (kreatinin & cystatin C)", Resultat = round(zappitelli_gfr_cys_cr(scr_umol, scys, height_cm), 0)))
      }
    }
    return(rows)
  }
  
  results_child <- reactive({
    scr_umol <- input$creatinine_child
    scys <- input$cysc_child
    sex <- input$sex_child
    age <- input$age_child
    height_cm <- input$height_child
    formulas <- input$formulas_child
    if (is.null(age) || is.null(sex) || is.null(formulas)) return(data.frame(Formel = character(), Resultat = character()))
    calc_egfr_child(scr_umol, scys, sex, age, height_cm, formulas)
  })
  
  output$resultsTable_child <- DT::renderDataTable({
    results <- results_child()
    if (nrow(results) == 0) return(NULL)
    datatable(results, options = list(paging = FALSE, searching = FALSE, ordering = FALSE, info = FALSE, columnDefs = list(list(className = 'dt-center', targets = '_all'))), rownames = FALSE, colnames = c("Formel", "Resultat (mL/min/1.73m²)"))
  })
  
  output$selectedMean_child <- renderUI({
    selected <- input$resultsTable_child_rows_selected
    if (length(selected) > 0) {
      results <- results_child()
      mean_val <- mean(as.numeric(results$Resultat[selected]), na.rm = TRUE)
      tags$div(style = "margin-top: 10px;", paste("Medelvärde av valda rader:", round(mean_val, 0), "mL/min/1.73m²"))
    }
  })
  
  output$footnote_child <- renderUI({
    results <- results_child()
    if (nrow(results) > 0 && any(grepl("Zappitelli", results$Formel))) {
      tags$div(style = "font-size: 0.8em; color: #666; margin-top: 10px;", "Noteringar för Zappitelli-formler visas inte i denna version.")
    }
  })
  
  observeEvent(input$clear_selection_child, {
    DT::selectRows(proxy = DT::dataTableProxy("resultsTable_child"), selected = NULL)
  })
  
  # --- Iohexol Calculation Section ---
  
  observe({
    req(input$io_sex, input$io_weight, input$io_height, input$io_conc_inj,
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
            style = "background: #f8f9fa; border-radius: 12px; padding: 24px; box-shadow: 0 2px 8px rgba(0,0,0,0.04);",
            tags$div(style = "font-size: 1.8em; font-weight: bold;", "Absolut mGFR: ", tags$span(style="color: #0d6efd;", sprintf("%.1f", result$gfr), " mL/min")),
            tags$div(style = "font-size: 1.8em; font-weight: bold;", "Relativt mGFR: ", tags$span(style="color: #0d6efd;", sprintf("%.1f", result$gfr_adj), " mL/min/1.73m²")),
            tags$div(style = "font-size: 1.2em; color: #888; margin-top: 10px;", sprintf("Optimal provtagningstid: %s", format_hours_minutes(result$t_opt_hours)))
          )
        }
      })
      
      output$io_plot <- renderPlot({
        if (any(is.na(result))) return(NULL)
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
            data = df_opt, aes(time, conc, label = label), direction = "y", nudge_y = log(df_opt$conc) * 0.1,
            segment.color = "#0d6efd", segment.size = 0.5, min.segment.length = 0, label.padding = unit(0.4, "lines"),
            color = "#0d6efd", size = 5, label.r = unit(0.25, "lines")
          ) +
          scale_y_log10(labels = scales::number_format()) +
          scale_x_continuous(limits = c(0, t_end)) +
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
      
      if (!all(sapply(times_str, is_valid_hhmm)) || !all(is_valid_num(concs_num, min = 0, inclusive = FALSE))) {
        show_error("Ogiltiga tidpunkter eller koncentrationer.")
        return()
      }
      
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
          } else {  # 4-point
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
          
          tags$div(
            style = "background: #f8f9fa; border-radius: 12px; padding: 24px; box-shadow: 0 2px 8px rgba(0,0,0,0.04);",
            tags$div(style = "font-size: 1.8em; font-weight: bold;", "Absolut mGFR: ", tags$span(style="color: #0d6efd;", sprintf("%.1f", main_result$gfr), " mL/min")),
            tags$div(style = "font-size: 1.8em; font-weight: bold;", "Relativt mGFR: ", tags$span(style="color: #0d6efd;", sprintf("%.1f", main_result$gfr_adj), " mL/min/1.73m²")),
            hr(style = "margin-top: 20px; margin-bottom: 20px;"),
            sub_calcs_ui
          )
        }
      })
      
      output$io_plot <- renderPlot({
        if (any(is.na(main_result))) return(NULL)
        df_obs <- data.frame(time = times_min / 60, conc = concs_num, point_num = 1:num_points)
        fit <- lm(log(conc) ~ time, data = df_obs)
        
        t_end <- max(df_obs$time) + 0.5
        pred_times <- seq(0, t_end, length.out = 200)
        pred_log <- predict(fit, newdata = data.frame(time = pred_times))
        df_pred <- data.frame(time = pred_times, conc = exp(pred_log))
        
        ggplot() +
          geom_line(data = df_pred, aes(time, conc), color = "black") +
          geom_point(data = df_obs, aes(time, conc), size = 5, color = "#0d6efd") +
          geom_text(data = df_obs, aes(time, conc, label = point_num), color = "white", fontface = "bold", size = 4) +
          scale_y_log10(labels = scales::number_format()) +
          scale_x_continuous(limits = c(0, t_end)) +
          labs(x = "Tid (timmar)", y = "Koncentration (mg/L)") +
          theme_minimal(base_size = 14)
      })
    }
  })
  
  # --- Other Calculations Section ---
  
  observe({
    # Cockcroft-Gault
    cg_age <- input$cg_age
    cg_weight <- input$cg_weight
    cg_scr_umol <- input$cg_creatinine
    cg_sex <- input$cg_sex
    
    output$cg_result_ui <- renderUI({
      if (!is_valid_num(c(cg_age, cg_weight, cg_scr_umol)) || is.null(cg_sex)) {
        return(NULL)
      }
      res <- calc_cockcroft_gault(cg_age, cg_weight, cg_scr_umol, cg_sex)
      if (is_valid_num(res)) {
        tags$div(
          style = "background: #f8f9fa; border-radius: 12px; padding: 24px; box-shadow: 0 2px 8px rgba(0,0,0,0.04);",
          tags$div(style = "font-size: 1.5em; font-weight: bold;", "Kreatininclearance: ", 
                   tags$span(style="color: #0d6efd;", sprintf("%.1f", res), 
                             tags$span(style = "font-size: 0.6em; font-weight: normal; margin-left: 2px;", "mL/min")))
        )
      } else {
        NULL
      }
    })
    
    # Optimal sampling time
    opt_sex <- input$opt_sex
    opt_weight <- input$opt_weight
    opt_height <- input$opt_height
    opt_egfr <- input$opt_egfr
    opt_bsa_formula <- input$opt_bsa_formula
    
    output$opt_result_ui <- renderUI({
      if (!is_valid_num(c(opt_weight, opt_height, opt_egfr)) || is.null(opt_sex) || is.null(opt_bsa_formula)) {
        return(NULL)
      }
      eecv <- jacobsson_eecv(opt_sex, opt_weight)
      bsa <- bsa_select(opt_weight, opt_height, opt_sex, opt_bsa_formula)
      if (!is_valid_num(c(eecv, bsa))) return(NULL)
      abs_gfr <- opt_egfr * bsa / 1.73
      if (!is_valid_num(abs_gfr) || abs_gfr <= 0) return(NULL)
      t_opt_min <- eecv / abs_gfr
      t_opt_hours <- t_opt_min / 60
      res <- format_hours_minutes(t_opt_hours)
      if (!is.na(res)) {
        tags$div(
          style = "background: #f8f9fa; border-radius: 12px; padding: 24px; box-shadow: 0 2px 8px rgba(0,0,0,0.04);",
          tags$div(style = "font-size: 1.5em; font-weight: bold;", "Optimal tid: ", 
                   tags$span(style="color: #0d6efd;", res))
        )
      } else {
        NULL
      }
    })
    
    # GFR Conversion
    direction <- input$gfr_conv_direction
    height <- input$gfr_conv_height
    weight <- input$gfr_conv_weight
    sex <- input$gfr_conv_sex
    bsa_formula <- input$gfr_conv_bsa_formula
    gfr_abs <- input$gfr_abs
    gfr_rel <- input$gfr_rel
    
    output$gfr_conv_result_ui <- renderUI({
      if (is.null(direction) || !is_valid_num(c(height, weight)) || is.null(sex) || is.null(bsa_formula)) {
        return(NULL)
      }
      bsa <- bsa_select(weight, height, sex, bsa_formula)
      if (!is_valid_num(bsa)) return(NULL)
      
      if (direction == "abs_to_rel") {
        if (!is_valid_num(gfr_abs)) return(NULL)
        val <- gfr_abs * (1.73 / bsa)
        label <- "Relativ GFR"
        unit <- "mL/min/1.73m²"
      } else {
        if (!is_valid_num(gfr_rel)) return(NULL)
        val <- gfr_rel * bsa / 1.73
        label <- "Absolut GFR"
        unit <- "mL/min"
      }
      
      if (is_valid_num(val)) {
        tags$div(
          style = "background: #f8f9fa; border-radius: 12px; padding: 24px; box-shadow: 0 2px 8px rgba(0,0,0,0.04);",
          tags$div(style = "font-size: 1.5em; font-weight: bold;", paste0(label, ": "), 
                   tags$span(style="color: #0d6efd;", sprintf("%.1f", val), 
                             tags$span(style = "font-size: 0.6em; font-weight: normal; margin-left: 2px;", unit)))
        )
      } else {
        NULL
      }
    })
  })
  
  # --- Controls / clear buttons ---
  
  observeEvent(input$io_clear, {
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