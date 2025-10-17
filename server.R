source("global.R")
library(shinyjs) # Import shinyjs

server <- function(input, output, session) {
  # Run once after session is flushed to set UI defaults where necessary.
  session$onFlushed(function() {
    updateSelectInput(session, "io_sex", selected = "Man")
    updateSelectInput(session, "io_bsa_formula", selected = "dubois")
    updateSelectInput(session, "io_conc_inj", selected = "300")
    updateSelectInput(session, "sex", selected = "Man")
    
    # --- Initially disable the BIS checkbox ---
    shinyjs::disable("formula_bis")
  }, once = TRUE)
  
  # --- Observer to conditionally enable/disable BIS formula ---
  observeEvent(input$age, {
    age_val <- input$age
    if (is.na(age_val) || age_val < 70) {
      shinyjs::disable("formula_bis")
      # Uncheck the box if it's disabled
      updateCheckboxGroupInput(session, "formulas", selected = setdiff(input$formulas, "BIS"))
    } else {
      shinyjs::enable("formula_bis")
    }
  }, ignoreNULL = FALSE) # ignoreNULL=FALSE ensures it runs on startup
  
  # Validate HH:MM time format (simple wrapper)
  validate_time <- function(time) {
    if (!is.character(time) || time == "") return(FALSE)
    grepl("^([0-1][0-9]|2[0-3]):[0-5][0-9]$", time)
  }
  
  # Consolidated eGFR calculation function
  calc_egfr <- function(scr_umol, scys, sex, age, formulas) {
    rows <- data.frame(Resultat = character(), Value = character(), stringsAsFactors = FALSE)
    scr <- if (is_valid_num(scr_umol)) scr_umol / 88.4 else NA
    
    # EKFC
    if ("EKFC" %in% formulas) {
      if (is_valid_num(scr)) {
        QCr <- ifelse(sex == "Man", 0.90, 0.70)
        rows <- rbind(rows, data.frame(Resultat = "EKFC (kreatinin)", Value = round(ekfc_gfr(scr, QCr, age), 0)))
      }
      if (is_valid_num(scys)) {
        QCys <- 0.83
        rows <- rbind(rows, data.frame(Resultat = "EKFC (cystatin C)", Value = round(ekfc_gfr(scys, QCys, age), 0)))
        Qsexadj <- q_cysc_sexadj(sex, age)
        if (!is.na(Qsexadj)) {
          rows <- rbind(rows, data.frame(Resultat = "EKFC (cystatin C)*", Value = round(ekfc_gfr(scys, Qsexadj, age), 0)))
        }
      }
      if (is_valid_num(scr) && is_valid_num(scys)) {
        QCr <- ifelse(sex == "Man", 0.90, 0.70)
        QCys <- 0.83
        egfr_cr <- ekfc_gfr(scr, QCr, age)
        egfr_cys <- ekfc_gfr(scys, QCys, age)
        rows <- rbind(rows, data.frame(Resultat = "EKFC (kreatinin & cystatin C)", Value = round((egfr_cr + egfr_cys) / 2, 0)))
        Qsexadj <- q_cysc_sexadj(sex, age)
        if (!is.na(Qsexadj)) {
          egfr_cys_sexadj <- ekfc_gfr(scys, Qsexadj, age)
          rows <- rbind(rows, data.frame(Resultat = "EKFC (kreatinin & cystatin C)*", Value = round((egfr_cr + egfr_cys_sexadj) / 2, 0)))
        }
      }
    }
    
    # FAS
    if ("FAS" %in% formulas) {
      if (is_valid_num(scr)) {
        rows <- rbind(rows, data.frame(Resultat = "FAS (kreatinin)", Value = round(fas_gfr_cr(scr, sex, age), 0)))
      }
      if (is_valid_num(scys)) {
        rows <- rbind(rows, data.frame(Resultat = "FAS (cystatin C)", Value = round(fas_gfr_cys(scys, age), 0)))
        rows <- rbind(rows, data.frame(Resultat = "FAS (cystatin C)*", Value = round(fas_gfr_cys_sexadj(scys, sex, age), 0)))
      }
      if (is_valid_num(scr) && is_valid_num(scys)) {
        rows <- rbind(rows, data.frame(Resultat = "FAS (kreatinin & cystatin C)", Value = round((fas_gfr_cr(scr, sex, age) + fas_gfr_cys(scys, age)) / 2, 0)))
        rows <- rbind(rows, data.frame(Resultat = "FAS (kreatinin & cystatin C)*", Value = round((fas_gfr_cr(scr, sex, age) + fas_gfr_cys_sexadj(scys, sex, age)) / 2, 0)))
      }
    }
    
    # r-LMR
    if ("r-LMR" %in% formulas) {
      if (is_valid_num(scr_umol)) {
        rows <- rbind(rows, data.frame(Resultat = "r-LMR (kreatinin)", Value = round(r_lmr_gfr_cr(scr_umol, sex, age), 0)))
      }
      if (is_valid_num(scys)) {
        rows <- rbind(rows, data.frame(Resultat = "r-LMR (cystatin C)", Value = round(r_lmr_gfr_cys(scys, age), 0)))
        rows <- rbind(rows, data.frame(Resultat = "r-LMR (cystatin C)*", Value = round(r_lmr_gfr_cys_sexadj(scys, sex, age), 0)))
      }
      if (is_valid_num(scr_umol) && is_valid_num(scys)) {
        rows <- rbind(rows, data.frame(Resultat = "r-LMR (kreatinin & cystatin C)", Value = round((r_lmr_gfr_cr(scr_umol, sex, age) + r_lmr_gfr_cys(scys, age)) / 2, 0)))
        rows <- rbind(rows, data.frame(Resultat = "r-LMR (kreatinin & cystatin C)*", Value = round((r_lmr_gfr_cr(scr_umol, sex, age) + r_lmr_gfr_cys_sexadj(scys, sex, age)) / 2, 0)))
      }
    }
    
    # CKD-EPI
    if ("CKD-EPI (2021)" %in% formulas) {
      if (is_valid_num(scr_umol)) {
        rows <- rbind(rows, data.frame(Resultat = "CKD-EPI (kreatinin)", Value = round(ckdepi2021_gfr_cr(scr_umol, sex, age), 0)))
      }
      if (is_valid_num(scys)) {
        rows <- rbind(rows, data.frame(Resultat = "CKD-EPI (cystatin C)", Value = round(ckdepi2021_gfr_cys(scys, sex, age), 0)))
      }
      if (is_valid_num(scr_umol) && is_valid_num(scys)) {
        rows <- rbind(rows, data.frame(Resultat = "CKD-EPI (kreatinin & cystatin C)", Value = round(ckdepi2021_gfr_cr_cys(scr_umol, scys, sex, age), 0)))
      }
    }
    
    # LMR18
    if ("LMR18" %in% formulas) {
      if (is_valid_num(scr_umol)) {
        rows <- rbind(rows, data.frame(Resultat = "LMR18 (kreatinin)", Value = round(lmr18_gfr_cr(scr_umol, sex, age), 0)))
      }
    }
    
    # CAPA
    if ("CAPA" %in% formulas) {
      if (is_valid_num(scys)) {
        rows <- rbind(rows, data.frame(Resultat = "CAPA (cystatin C)", Value = round(capa_gfr_cys(scys, age), 0)))
      }
    }
    
    # MDRD
    if ("MDRD" %in% formulas) {
      if (is_valid_num(scr_umol)) {
        rows <- rbind(rows, data.frame(Resultat = "MDRD (kreatinin)", Value = round(mdrd_gfr(scr_umol, sex, age), 0)))
      }
    }
    
    # BIS
    if ("BIS" %in% formulas && is_valid_num(age) && age >= 70) {
      if (is_valid_num(scr_umol)) {
        rows <- rbind(rows, data.frame(Resultat = "BIS (kreatinin)", Value = round(bis_gfr(scr_umol, sex, age), 0)))
      }
    }
    
    return(rows)
  }
  
  # Cache eGFR results so multiple outputs reuse the same calculation
  egfr_results <- reactive({
    req(input$age, input$sex, input$formulas)
    if (!is_valid_num(input$age) || !input$sex %in% c("Man", "Kvinna")) {
      return(data.frame(Resultat = character(), Value = character(), stringsAsFactors = FALSE))
    }
    calc_egfr(input$creatinine, input$cysc, input$sex, input$age, input$formulas)
  })
  
  # Render eGFR results table
  output$resultsTable <- DT::renderDataTable({
    result <- egfr_results()
    if (nrow(result) == 0) return(NULL)
    datatable(
      result,
      options = list(
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE,
        info = FALSE,
        columnDefs = list(list(className = 'dt-center', targets = '_all')),
        language = list(emptyTable = "Inga resultat att visa")
      ),
      rownames = FALSE
    )
  })
  
  # Calculate mean of selected rows
  output$selectedMean <- renderUI({
    req(input$resultsTable_rows_selected)
    selected <- input$resultsTable_rows_selected
    results <- egfr_results()
    if (length(selected) > 0 && nrow(results) > 0) {
      mean_val <- mean(as.numeric(results$Value[selected]), na.rm = TRUE)
      if (is_valid_num(mean_val)) {
        tags$div(
          style = "margin-top: 10px;",
          paste("Medelvärde av valda rader:", round(mean_val, 0), "mL/min/1.73m²")
        )
      }
    }
  })
  
  # Asterisk note
  output$asteriskNote <- renderUI({
    results <- egfr_results()
    if (nrow(results) > 0 && any(grepl("\\*", results$Resultat))) {
      tags$div(
        style = "font-size: 0.8em; color: #666; margin-top: 10px;",
        "* Beräknat med könsspecifik Q-faktor för cystatin C"
      )
    }
  })
  
  # Format decimal hours to hours and minutes
  format_hours_minutes <- function(decimal_hours) {
    if (!is_valid_num(decimal_hours)) return("NA")
    hours <- floor(decimal_hours)
    minutes <- round((decimal_hours - hours) * 60)
    sprintf("%d h %d min", hours, minutes)
  }
  
  # Consolidated Iohexol Calculation
  observe({
    # Common inputs
    req(input$io_age, input$io_sex, input$io_weight, input$io_height, input$io_conc_inj,
        input$io_vol_inj, input$io_inj_time, input$io_bsa_formula, input$num_points)
    
    num_points <- as.numeric(input$num_points)
    
    # Base data
    sex <- input$io_sex
    weight <- as.numeric(input$io_weight)
    height <- as.numeric(input$io_height)
    iodine_mg_ml <- as.numeric(input$io_conc_inj)
    iohexol_mg_ml <- iodine_to_iohexol(iodine_mg_ml)
    dose_mg <- iohexol_mg_ml * as.numeric(input$io_vol_inj)
    bsa_formula <- input$io_bsa_formula
    
    # Generic error display
    show_error <- function(message) {
      output$io_result_ui <- renderUI({ tags$div(style = "color: red;", message) })
      output$io_plot <- renderPlot(NULL)
    }
    
    if (is.na(iohexol_mg_ml)) {
      show_error("Ogiltig iohexolkoncentration.")
      return()
    }
    
    # --- 1-POINT CALCULATION ---
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
        if (is.na(result$gfr) || is.na(result$gfr_adj) || is.na(result$t_opt_hours)) {
          tags$div(style = "color: red;", "Ogiltiga indata eller beräkning misslyckades.")
        } else {
          tags$div(
            style = "background: #f8f9fa; border-radius: 12px; padding: 24px; margin-bottom: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.04);",
            tags$div(style = "font-size: 1.8em; font-weight: bold; margin-bottom: 8px;", "Absolut mGFR: ", tags$span(style="color: #0d6efd;", sprintf("%.1f", result$gfr), tags$span(style = "font-size: 0.6em; font-weight: normal; margin-left: 2px;", "mL/min"))),
            tags$div(style = "font-size: 1.8em; font-weight: bold; margin-bottom: 8px;", "Relativt mGFR: ", tags$span(style="color: #0d6efd;", sprintf("%.1f", result$gfr_adj), tags$span(style = "font-size: 0.6em; font-weight: normal; margin-left: 2px;", "mL/min/1.73m²"))),
            tags$div(style = "font-size: 1.2em; color: #888; margin-top: 10px;", sprintf("Optimal provtagningstid: %s", format_hours_minutes(result$t_opt_hours)))
          )
        }
      })
      
      output$io_plot <- renderPlot({
        if (is.na(result$gfr)) return(NULL)
        eecv <- jacobsson_eecv(sex, weight)
        
        df_sample <- data.frame(time = t_diff_min / 60, conc = plasma_mg_L, point_num = "1")
        df_c0 <- data.frame(time = 0, conc = dose_mg / (eecv / 1000))
        df_line <- rbind(df_c0, df_sample[, c("time", "conc")])
        
        fit <- lm(log10(conc) ~ time, data = df_line)
        slope <- coef(fit)[2]
        intercept <- coef(fit)[1]
        
        t_end <- max(df_sample$time, result$t_opt_hours) + 0.5
        log_conc_end <- intercept + slope * t_end
        conc_end <- 10^log_conc_end
        df_dashed <- data.frame(time = c(df_sample$time, t_end), conc = c(df_sample$conc, conc_end))
        
        t_opt <- result$t_opt_hours
        log_conc_opt <- intercept + slope * t_opt
        conc_opt <- 10^log_conc_opt
        df_opt <- data.frame(time = t_opt, conc = conc_opt, label = sprintf("Optimal provtagningstid:\n%s", format_hours_minutes(result$t_opt_hours)))
        
        ggplot() +
          geom_line(data = df_line, aes(time, conc), color = "black") +
          geom_line(data = df_dashed, aes(time, conc), linetype = "dashed", color = "black") +
          geom_point(data = df_c0, aes(time, conc), size = 5, color = "black") +
          geom_point(data = df_sample, aes(time, conc), size = 5, color = "#0d6efd") +
          geom_text(data = df_sample, aes(time, conc, label = point_num), color = "white", fontface = "bold", size = 4) +
          geom_point(data = df_opt, aes(time, conc), shape = 4, size = 4, color = "#0d6efd") +
          ggrepel::geom_label_repel(
            data = df_opt, aes(time, conc, label = label), direction = "y", nudge_y = 0.05,
            segment.color = "#0d6efd", segment.size = 0.5, min.segment.length = 0, label.padding = unit(0.4, "lines"),
            color = "#0d6efd", size = 5, label.r = unit(0.25, "lines")
          ) +
          scale_y_log10(labels = scales::number_format()) +
          scale_x_continuous(limits = c(0, max(df_sample$time, result$t_opt_hours) + 0.5)) +
          labs(x = "Tid (timmar)", y = "Koncentration (mg/L)") +
          theme_minimal(base_size = 14) +
          theme(axis.title = element_text(face = "bold"), plot.title = element_text(hjust = 0.5, face = "bold"))
      })
      
    } else if (num_points == 2 || num_points == 4) {
      
      # --- NEW HELPER FOR MULTILINE SUB-RESULTS ---
      format_sub_result_multiline <- function(label, result) {
        abs_gfr <- if (is_valid_num(result$gfr)) sprintf("%.1f", result$gfr) else "N/A"
        rel_gfr <- if (is_valid_num(result$gfr_adj)) sprintf("%.1f", result$gfr_adj) else "N/A"
        
        tags$div(
          tags$b(label),
          tags$div(
            style = "display: grid; grid-template-columns: 50px auto; align-items: center; margin-left: 10px; line-height: 1.2;",
            tags$span(style = "text-align: right; padding-right: 5px;", abs_gfr),
            tags$span("mL/min"),
            tags$span(style = "text-align: right; padding-right: 5px;", rel_gfr),
            tags$span("mL/min/1.73m²")
          )
        )
      }
      
      # --- 2-POINT CALCULATION (WITH ENHANCEMENT) ---
      if (num_points == 2) {
        req(input$io_sample_time1, input$io_conc_p1, input$io_sample_time2, input$io_conc_p2)
        
        times_str <- c(input$io_sample_time1, input$io_sample_time2)
        concs_num <- c(as.numeric(input$io_conc_p1), as.numeric(input$io_conc_p2))
        
        req(all(sapply(times_str, validate_time)), all(is_valid_num(concs_num, min = 0, inclusive = FALSE)))
        
        times_min <- sapply(times_str, function(t) calc_time_difference_min(input$io_inj_time, t))
        
        if (any(is.na(times_min)) || any(times_min < 120)) {
          show_error("Ogiltig tidsdifferens, provtagning före 2 timmar.")
          return()
        }
        
        bsa <- bsa_select(weight, height, sex, bsa_formula)
        result_2pt <- slope_intercept_gfr(times_min, concs_num, dose_mg, bsa)
        
        results_1pt <- lapply(1:2, function(i) {
          iohexol_one_point_gfr(sex, weight, height, dose_mg, concs_num[i], times_min[i], bsa_formula)
        })
        
        output$io_result_ui <- renderUI({
          if (is.na(result_2pt$gfr) || is.na(result_2pt$gfr_adj)) {
            tags$div(style = "color: red;", "Ogiltiga indata eller beräkning misslyckades.")
          } else {
            tagList(
              tags$div(
                style = "background: #f8f9fa; border-radius: 12px; padding: 24px; margin-bottom: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.04);",
                tags$div(style = "font-size: 1.8em; font-weight: bold; margin-bottom: 8px;", "Absolut mGFR: ", tags$span(style="color: #0d6efd;", sprintf("%.1f", result_2pt$gfr), tags$span(style = "font-size: 0.6em; font-weight: normal; margin-left: 2px;", "mL/min"))),
                tags$div(style = "font-size: 1.8em; font-weight: bold; margin-bottom: 8px;", "Relativt mGFR: ", tags$span(style="color: #0d6efd;", sprintf("%.1f", result_2pt$gfr_adj), tags$span(style = "font-size: 0.6em; font-weight: normal; margin-left: 2px;", "mL/min/1.73m²"))),
                
                hr(style = "margin-top: 20px; margin-bottom: 20px;"),
                
                h5("Enpunktsberäkningar", style="font-weight: bold; color: #333; margin-bottom: 10px;"),
                fluidRow(
                  column(6, format_sub_result_multiline("Prov 1:", results_1pt[[1]])),
                  column(6, format_sub_result_multiline("Prov 2:", results_1pt[[2]]))
                )
              )
            )
          }
        })
        
        output$io_plot <- renderPlot({
          if (is.na(result_2pt$gfr)) return(NULL)
          df_obs <- data.frame(time_h = times_min / 60, conc = concs_num, point_num = 1:2)
          fit <- lm(log10(conc) ~ time_h, data = df_obs)
          t_end_h <- max(df_obs$time_h) + 0.5
          pred_times_h <- seq(0, t_end_h, length.out = 200)
          pred_log10 <- predict(fit, newdata = data.frame(time_h = pred_times_h))
          df_pred <- data.frame(time_h = pred_times_h, conc = 10^pred_log10)
          
          ggplot() +
            geom_line(data = df_pred, aes(x = time_h, y = conc), color = "black") +
            geom_point(data = df_obs, aes(x = time_h, y = conc), size = 5, color = "#0d6efd") +
            geom_text(data = df_obs, aes(x = time_h, y = conc, label = point_num), color = "white", fontface = "bold", size = 4) +
            scale_y_log10(labels = scales::number_format()) +
            labs(x = "Tid (timmar)", y = "Koncentration (mg/L)") +
            theme_minimal(base_size = 14) +
            theme(axis.title = element_text(face = "bold"), plot.title = element_text(hjust = 0.5, face = "bold"))
        })
        
        # --- 4-POINT CALCULATION (WITH ENHANCED SUB-CALCULATIONS) ---
      } else if (num_points == 4) {
        req(input$io_sample_time1, input$io_conc_p1, input$io_sample_time2, input$io_conc_p2,
            input$io_sample_time3, input$io_conc_p3, input$io_sample_time4, input$io_conc_p4)
        
        times_str <- c(input$io_sample_time1, input$io_sample_time2, input$io_sample_time3, input$io_sample_time4)
        concs_num <- c(as.numeric(input$io_conc_p1), as.numeric(input$io_conc_p2), as.numeric(input$io_conc_p3), as.numeric(input$io_conc_p4))
        
        req(all(sapply(times_str, validate_time)), all(is_valid_num(concs_num, min = 0, inclusive = FALSE)))
        
        times_min <- sapply(times_str, function(t) calc_time_difference_min(input$io_inj_time, t))
        
        if (any(is.na(times_min)) || any(times_min < 120)) {
          show_error("Ogiltig tidsdifferens, provtagning före 2 timmar.")
          return()
        }
        
        bsa <- bsa_select(weight, height, sex, bsa_formula)
        
        # --- Main 4-point calculation ---
        result_4pt <- slope_intercept_gfr(times_min, concs_num, dose_mg, bsa)
        
        # --- All 1-point calculations ---
        results_1pt <- lapply(1:4, function(i) {
          iohexol_one_point_gfr(sex, weight, height, dose_mg, concs_num[i], times_min[i], bsa_formula)
        })
        
        # --- All 2-point calculations ---
        point_pairs <- list(c(1,2), c(1,3), c(1,4), c(2,3), c(2,4), c(3,4))
        results_2pt <- lapply(point_pairs, function(pair) {
          slope_intercept_gfr(times_min[pair], concs_num[pair], dose_mg, bsa)
        })
        
        # --- UI Rendering ---
        output$io_result_ui <- renderUI({
          if (is.na(result_4pt$gfr) || is.na(result_4pt$gfr_adj)) {
            tags$div(style = "color: red;", "Ogiltiga indata eller beräkning misslyckades.")
          } else {
            tagList(
              tags$div(
                style = "background: #f8f9fa; border-radius: 12px; padding: 24px; margin-bottom: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.04);",
                # Main result
                tags$div(style = "font-size: 1.8em; font-weight: bold; margin-bottom: 8px;", "Absolut mGFR: ", tags$span(style="color: #0d6efd;", sprintf("%.1f", result_4pt$gfr), tags$span(style = "font-size: 0.6em; font-weight: normal; margin-left: 2px;", "mL/min"))),
                tags$div(style = "font-size: 1.8em; font-weight: bold; margin-bottom: 8px;", "Relativt mGFR: ", tags$span(style="color: #0d6efd;", sprintf("%.1f", result_4pt$gfr_adj), tags$span(style = "font-size: 0.6em; font-weight: normal; margin-left: 2px;", "mL/min/1.73m²"))),
                
                hr(style = "margin-top: 20px; margin-bottom: 20px;"),
                
                # 2-point results
                h5("Tvåpunktsberäkningar", style="font-weight: bold; color: #333; margin-bottom: 10px;"),
                fluidRow(
                  column(6, format_sub_result_multiline("Prov 1 och 2:", results_2pt[[1]])),
                  column(6, format_sub_result_multiline("Prov 2 och 3:", results_2pt[[4]]))
                ),
                fluidRow(
                  column(6, format_sub_result_multiline("Prov 1 och 3:", results_2pt[[2]])),
                  column(6, format_sub_result_multiline("Prov 2 och 4:", results_2pt[[5]]))
                ),
                fluidRow(
                  column(6, format_sub_result_multiline("Prov 1 och 4:", results_2pt[[3]])),
                  column(6, format_sub_result_multiline("Prov 3 och 4:", results_2pt[[6]]))
                ),
                
                hr(style = "margin-top: 20px; margin-bottom: 20px;"),
                
                # 1-point results
                h5("Enpunktsberäkningar", style="font-weight: bold; color: #333; margin-bottom: 10px;"),
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
        })
        
        output$io_plot <- renderPlot({
          if (is.na(result_4pt$gfr)) return(NULL)
          df_obs <- data.frame(time_h = times_min / 60, conc = concs_num, point_num = 1:4)
          fit <- lm(log10(conc) ~ time_h, data = df_obs)
          t_end_h <- max(df_obs$time_h) + 0.5
          pred_times_h <- seq(0, t_end_h, length.out = 200)
          pred_log10 <- predict(fit, newdata = data.frame(time_h = pred_times_h))
          df_pred <- data.frame(time_h = pred_times_h, conc = 10^pred_log10)
          
          ggplot() +
            geom_line(data = df_pred, aes(x = time_h, y = conc), color = "black") +
            geom_point(data = df_obs, aes(x = time_h, y = conc), size = 5, color = "#0d6efd") +
            geom_text(data = df_obs, aes(x = time_h, y = conc, label = point_num), color = "white", fontface = "bold", size = 4) +
            scale_y_log10(labels = scales::number_format()) +
            labs(x = "Tid (timmar)", y = "Koncentration (mg/L)") +
            theme_minimal(base_size = 14) +
            theme(axis.title = element_text(face = "bold"), plot.title = element_text(hjust = 0.5, face = "bold"))
        })
      }
    }
  })
  
  # --- Controls / clear buttons ---
  
  # eGFR tab clear
  observeEvent(input$clear, {
    updateNumericInput(session, "age", value = NA)
    updateSelectInput(session, "sex", selected = "Man")
    updateNumericInput(session, "creatinine", value = NA)
    updateNumericInput(session, "cysc", value = NA)
    updateCheckboxGroupInput(session, "formulas", selected = c("EKFC"))
    DT::selectRows(proxy = DT::dataTableProxy("resultsTable"), selected = NULL)
  })
  
  # Iohexol tab clear
  observeEvent(input$io_clear, {
    updateNumericInput(session, "io_age", value = NA)
    updateSelectInput(session, "io_sex", selected = "Man")
    updateNumericInput(session, "io_weight", value = NA)
    updateNumericInput(session, "io_height", value = NA)
    updateSelectInput(session, "io_conc_inj", selected = "300")
    updateNumericInput(session, "io_vol_inj", value = 4)
    updateTextInput(session, "io_inj_time", value = "")
    updateSelectInput(session, "io_bsa_formula", selected = "dubois")
    
    # Clear all possible sample inputs
    for (i in 1:4) {
      updateTextInput(session, paste0("io_sample_time", i), value = "")
      updateNumericInput(session, paste0("io_conc_p", i), value = NA)
    }
    
    # Clear outputs
    output$io_result_ui <- renderUI({ NULL })
    output$io_plot <- renderPlot({ NULL })
  })
  
  # Clear eGFR selection
  observeEvent(input$clear_selection, {
    DT::selectRows(proxy = DT::dataTableProxy("resultsTable"), selected = NULL)
  })
}