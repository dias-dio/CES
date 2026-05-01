# server.R


# Helper functions
build_curve_plot <- function(plot_data, is_antiviral) {
  p <- suppressWarnings({
    tt_label <- ifelse(is_antiviral, "\nViability: ", "\nInhib: ")
    leg_label <- ifelse(is_antiviral, "Mock", "Mono")
    
    color_values <- c("Co-culture" = "#d96b8a", "Control" = "#006d2c")
    color_values[[leg_label]] <- if (is_antiviral) "#006d2c" else "#2166ac"
    
    all_x <- plot_data$screen_coculture$df$logconc
    if(!is.null(plot_data$screen_target2)) all_x <- c(all_x, plot_data$screen_target2$df$logconc)
    if(!is.null(plot_data$screen_control)) all_x <- c(all_x, plot_data$screen_control$df$logconc)
    
    unique_x <- sort(unique(all_x))
    custom_labels <- signif(10^unique_x, 3) 
    
    # Dynamic X-axis angle 
    num_doses <- length(unique_x)
    x_angle <- if (num_doses >= 8) 45 else 0
    x_hjust <- if (num_doses >= 8) 1 else 0.5
    
    p_temp <- ggplot() +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 1) +
      coord_cartesian(ylim = c(-100, 100)) +
      scale_x_continuous(breaks = unique_x, labels = custom_labels) +
      labs(title = plot_data$plot_title_text, y = plot_data$y_axis_label, x = paste(plot_data$dr, "(concentration)")) +
      theme_bw() + 
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5, color = "black"),
        legend.position = "bottom", legend.title = element_text(size = 14, face="bold"), 
        legend.text = element_text(size = 13), 
        axis.text.x = element_text(size = 13, colour = "black", angle = x_angle, hjust = x_hjust), 
        axis.text.y = element_text(size = 13, colour = "black"), 
        axis.title = element_text(size = 16, colour = "black"),
        axis.ticks.length = unit(0.1, "cm"),
        panel.grid.minor = element_blank()
      )
    
    if(!is.null(plot_data$screen_target2)) {
      plot_y <- plot_data$screen_target2$y
      plot_pts_x <- plot_data$screen_target2$df$logconc
      plot_pts_y <- plot_data$screen_target2$df$inhibition
      
      if(is_antiviral) { plot_y <- 100 - plot_y; plot_pts_y <- 100 - plot_pts_y }
      
      p_temp <- p_temp + geom_line(data = data.frame(logconc = plot_data$screen_target2$x, inhibition = plot_y), aes(logconc, inhibition, color = leg_label, group = 1, text = paste0("Conc: ", signif(10^logconc, 4), tt_label, round(inhibition, 1), "%")), linewidth = 1.1) +
        geom_point(data = data.frame(logconc = plot_pts_x, inhibition = plot_pts_y), aes(logconc, inhibition, color = leg_label, text = paste0("Conc: ", signif(10^logconc, 4), tt_label, round(inhibition, 1), "%")), size = 3)
    }
    
    if(!is.null(plot_data$screen_control)) {
      p_temp <- p_temp + geom_line(data = data.frame(logconc = plot_data$screen_control$x, inhibition = plot_data$screen_control$y), aes(logconc, inhibition, color = "Control", group = 1, text = paste0("Conc: ", signif(10^logconc, 4), "\nInhib: ", round(inhibition, 1), "%")), linewidth = 1.1) +
        geom_point(data = plot_data$screen_control$df, aes(logconc, inhibition, color = "Control", text = paste0("Conc: ", signif(10^logconc, 4), "\nInhib: ", round(inhibition, 1), "%")), size = 3)
    }
    
    p_temp <- p_temp + geom_line(data = data.frame(logconc = plot_data$screen_coculture$x, inhibition = plot_data$screen_coculture$y), aes(logconc, inhibition, color = "Co-culture", group = 1, text = paste0("Conc: ", signif(10^logconc, 4), "\nInhib: ", round(inhibition, 1), "%")), linewidth = 1.1) +
      geom_point(data = plot_data$screen_coculture$df, aes(logconc, inhibition, color = "Co-culture", text = paste0("Conc: ", signif(10^logconc, 4), "\nInhib: ", round(inhibition, 1), "%")), size = 3)
    
    p_temp <- p_temp + scale_color_manual(name = "Condition", values = color_values)
    
    p_temp
  })
  return(p)
}

build_qc_boxplot <- function(qc_df) {
  q_df <- qc_df %>% filter(CONTENT %in% c("BzCl", "DMSO (Target)", "DMSO (Co-culture)", "DMSO (Control)"))
  if (nrow(q_df) == 0) return(NULL)
  
  q_df$CONTENT <- factor(q_df$CONTENT, levels = c("BzCl", "DMSO (Target)", "DMSO (Co-culture)", "DMSO (Control)"))
  custom_fill <- c("BzCl" = "#C49A00", "DMSO (Target)" = "#4d4d4d",
                   "DMSO (Co-culture)" = "#542788", "DMSO (Control)" = "#2166ac")
  
  ggplot(q_df, aes(x = SCREEN, y = WELL_SIGNAL, fill = CONTENT)) +
    geom_boxplot(color = "black", outlier.shape = 16, alpha = 0.9) +
    labs(x = "Screen", y = "Raw Intensity", fill = NULL) +
    scale_fill_manual(values = custom_fill, drop = TRUE) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.text.x = element_text(size = 14, colour = "black", face = "bold"),
      axis.text.y = element_text(size = 14, colour = "black"),
      axis.title = element_text(size = 16, colour = "black", face = "bold"),
      legend.text = element_text(size = 14, color = "black")
    )
}

build_qc_plate_plot <- function(plate_df, plot_title = NULL) {
  
  if (nrow(plate_df) == 0) return(NULL)
  
  plate_df$CONTENT <- factor(
    plate_df$CONTENT,
    levels = c("Cells/Empty", "DMSO (Target)", "DMSO (Co-culture)", "DMSO (Control)", "BzCl", "Drug")
  )
  custom_color <- c("Cells/Empty" = "#a6cee3", "DMSO (Target)" = "#4d4d4d",
                    "DMSO (Co-culture)" = "#542788", "DMSO (Control)" = "#2166ac",
                    "BzCl" = "#C49A00", "Drug" = "#b2df8a")
  
  df_controls <- plate_df %>%
    filter(CONTENT %in% c("DMSO (Target)", "DMSO (Co-culture)", "DMSO (Control)", "BzCl"))
  df_background <- plate_df %>%
    filter(!CONTENT %in% c("DMSO (Target)", "DMSO (Co-culture)", "DMSO (Control)", "BzCl"))
  
  ggplot(mapping = aes(x = COLUMN, y = WELL_SIGNAL, color = CONTENT,
                       text = paste0("Well: ", WELL, "\nSignal: ", WELL_SIGNAL))) +
    geom_point(data = df_background, size = 2.5, alpha = 0.5) +
    geom_line(data = df_controls,
              aes(x = COLUMN, y = WELL_SIGNAL, group = CONTENT),
              linewidth = 1.2, alpha = 0.8) +
    geom_point(data = df_controls, size = 3.5, alpha = 0.9) +
    scale_color_manual(values = custom_color, drop = TRUE) +
    labs(x = "Plate Column", y = "Raw Intensity", color = NULL, title = plot_title) +
    theme_bw() +
    theme(
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5, color = "black"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.text.x = element_text(size = 14, colour = "black"),
      axis.text.y = element_text(size = 14, colour = "black"),
      axis.title = element_text(size = 16, colour = "black", face = "bold"),
      legend.text = element_text(size = 14, color = "black")
    )
}

calculate_plate_qc <- function(qc_df) {
  if (is.null(qc_df) || nrow(qc_df) == 0) return(NULL)
  
  has_plates <- "PLATE" %in% colnames(qc_df)
  grouping_cols <- if (has_plates) c("PLATE", "SCREEN") else c("SCREEN")
  
  outlier_remove <- function(x) {
    x <- na.omit(x)
    if (length(x) < 4) return(x)
    q1 <- quantile(x, 0.25, na.rm = TRUE)
    q3 <- quantile(x, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    x[x >= (q1 - 1.5 * iqr) & x <= (q3 + 1.5 * iqr)]
  }
  
  stats_df <- qc_df %>%
    filter(CONTENT %in% c("BzCl", "DMSO (Target)", "DMSO (Co-culture)", "DMSO (Control)")) %>%
    group_by(across(all_of(grouping_cols))) %>%
    summarise(
      pos_signal = list(outlier_remove(WELL_SIGNAL[CONTENT == "BzCl"])),
      neg_signal = list(outlier_remove(WELL_SIGNAL[CONTENT %in% c("DMSO (Target)", "DMSO (Co-culture)", "DMSO (Control)")])),
      .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(
      Plate_SSMD = ssmd(unlist(neg_signal), unlist(pos_signal)),
      Signal_Vs_BG = round(mean(unlist(neg_signal), na.rm = TRUE) / mean(unlist(pos_signal), na.rm = TRUE), 1),
      Z_Prime = zfactor(unlist(neg_signal), unlist(pos_signal)),
      Robust_Z_Prime = robustzfactor(unlist(neg_signal), unlist(pos_signal))
    ) %>%
    ungroup() %>%
    dplyr::select(-pos_signal, -neg_signal)
  
  stats_df$Bad <- ifelse(stats_df$Z_Prime < 0.5, 1, 0)
  
  if (has_plates) {
    stats_df <- stats_df %>% rename(Plate = PLATE, Screen = SCREEN) %>%
      dplyr::select(Plate, Plate_SSMD, Signal_Vs_BG, Z_Prime, Robust_Z_Prime, Screen, Bad)
  } else {
    stats_df <- stats_df %>% rename(Screen = SCREEN) %>%
      mutate(Plate = "Default") %>%
      dplyr::select(Plate, Plate_SSMD, Signal_Vs_BG, Z_Prime, Robust_Z_Prime, Screen, Bad)
  }
  
  return(stats_df)
}

sanitize_csv <- function(df) {
  df[] <- lapply(df, function(col) {
    if (is.character(col) || is.factor(col)) {
      col <- as.character(col)
      bad <- !is.na(col) & grepl("^[=+@\t\r-]", col)
      col[bad] <- paste0("'", col[bad])
    }
    col
  })
  df
}

safe_text <- function(x, max_len = 200) {
  if (is.null(x)) return("")
  x <- as.character(x)
  x <- gsub("[<>&\"']", "", x)
  if (nchar(x) > max_len) x <- paste0(substr(x, 1, max_len), "...")
  x
}

notify <- function(msg, type = "error", id = "ces_validation", duration = 6) {
  showNotification(msg, type = type, id = id, duration = duration)
}

ALLOWED_EXTS <- c("csv", "xls", "xlsx", "txt", "tsv")
MAX_ROWS <- 1e6
MAX_COLS <- 200
MAX_DRUG_NAME_LEN <- 200
DEFAULT_TOX_THRESHOLD <- 10

safe_render <- function(expr, fallback_msg = "An unexpected rendering error occurred. Please verify your data.") {
  tryCatch(expr, error = function(e) {
    if (inherits(e, "shiny.silent.error")) {
      stop(e)
    }
    message("Render error: ", conditionMessage(e))
    notify(fallback_msg, type = "error", id = "ces_render", duration = 8)
    NULL
  })
}

peek_has_control <- function(filepath, ext) {
  tryCatch({
    df <- if (ext == "csv") {
      suppressWarnings(suppressMessages(read_csv(filepath, show_col_types = FALSE,
                                                 name_repair = "minimal", n_max = 500)))
    } else if (ext %in% c("xls", "xlsx")) {
      suppressWarnings(suppressMessages(read_excel(filepath, sheet = 1, .name_repair = "minimal")))
    } else {
      suppressWarnings(suppressMessages(read_tsv(filepath, show_col_types = FALSE,
                                                 name_repair = "minimal", n_max = 500)))
    }
    if (is.null(df) || nrow(df) == 0) return(FALSE)
    cn <- toupper(trimws(colnames(df)))
    screen_col <- NULL
    if ("CONDITION" %in% cn) {
      screen_col <- df[[which(cn == "CONDITION")[1]]]
    } else if ("SCREEN" %in% cn) {
      screen_col <- df[[which(cn == "SCREEN")[1]]]
    } else if ("SCREEN_NAME" %in% cn) {
      screen_col <- df[[which(cn == "SCREEN_NAME")[1]]]
    }
    if (is.null(screen_col)) return(FALSE)
    "Control" %in% as.character(screen_col)
  }, error = function(e) FALSE)
}


# Server logic of the CES web application
server <- function(input, output, session) {
  
  session$onFlushed(function() {
    shinyjs::hide("app-loader", anim = TRUE, animType = "fade", time = 0.8)
  }, once = TRUE)
  
  observeEvent(input$go_to_analysis, { updateTabsetPanel(session, "main_navbar", selected = "Analysis") })
  observeEvent(input$link_to_about, { updateTabsetPanel(session, "main_navbar", selected = "Documentation"); runjs("window.scrollTo({top: 0, behavior: 'smooth'});") })
  observeEvent(input$logo_home_link, { updateTabsetPanel(session, "main_navbar", selected = "Home"); runjs("window.scrollTo({top: 0, behavior: 'smooth'});") })
  observeEvent(input$link_to_cite, { 
    updateTabsetPanel(session, "main_navbar", selected = "Cite")
    runjs("window.scrollTo({top: 0, behavior: 'smooth'});") 
  })
  observeEvent(input$link_to_docs_from_analysis, { 
    updateTabsetPanel(session, "main_navbar", selected = "Documentation")
    runjs("window.scrollTo({top: 0, behavior: 'smooth'});") 
  })
  observeEvent(input$link_to_docs_data_req, { 
    updateTabsetPanel(session, "main_navbar", selected = "Documentation")
    runjs("setTimeout(function() { document.getElementById('doc-data-req').scrollIntoView({behavior: 'smooth'}); }, 300);") 
  })
  observeEvent(input$link_to_docs_from_welcome, { 
    updateTabsetPanel(session, "main_navbar", selected = "Documentation")
    runjs("setTimeout(function() { document.getElementById('doc-data-req').scrollIntoView({behavior: 'smooth'}); }, 300);") 
  })
  
  observeEvent(input$notify_download, {
    if (input$notify_download == "pdf") {
      notify("Generating PDF file with all curve fits... This may take a minute for large datasets. Please wait.", 
             type = "message", id = "ces_download", duration = 8)
    } else if (input$notify_download == "xlsx") {
      notify("Preparing XLSX file...", type = "message", id = "ces_download", duration = 3)
    }
  })
  
  example_download <- function(src, fname) {
    downloadHandler(
      filename = function() { fname },
      content = function(file) {
        if (!file.exists(src)) {
          notify(paste("Example file not found on server:", fname), type = "error", id = "ces_example_dl")
          return()
        }
        file.copy(src, file)
      }
    )
  }
  output$download_example_sens_pre  <- example_download("example_data/CES_annotation.xlsx", "CES_annotation.xlsx")
  output$download_example_sens_proc <- example_download("example_data/CES_processed.xlsx", "CES_processed.xlsx")
  output$download_example_anti <- example_download("example_data/CES_antiviral.xlsx", "CES_antiviral.xlsx")
  

  # Core variables
  pipeline_success <- reactiveVal(FALSE)
  final_results <- reactiveVal(NULL)
  is_running <- reactiveVal(FALSE)
  has_control_data <- reactiveVal(FALSE)
  run_settings <- reactiveValues(is_antiviral = FALSE) 
  
  stored_plots <- reactiveValues(curves = list(), dist = NULL, ed_ces = NULL)
  annot_qc_data <- reactiveVal(NULL)
  
  output$analysis_success <- reactive({ pipeline_success() })
  outputOptions(output, "analysis_success", suspendWhenHidden = FALSE)
  
  output$has_control <- reactive({ has_control_data() })
  outputOptions(output, "has_control", suspendWhenHidden = FALSE)
  
  observeEvent(input$file_upload, {
    req(input$file_upload)
    ext <- tolower(tools::file_ext(input$file_upload$name))
    has_ctrl <- if (ext %in% ALLOWED_EXTS) {
      peek_has_control(input$file_upload$datapath, ext)
    } else {
      FALSE
    }
    has_control_data(isTRUE(has_ctrl))
  })
  
  results_with_threshold <- reactive({
    df <- final_results()
    if (is.null(df)) return(NULL)
    if ("Toxic" %in% colnames(df) && !is.null(input$tox_threshold)) {
      thr <- input$tox_threshold
      df$Toxic <- ifelse(!is.na(df$DSS_control) & df$DSS_control >= thr, "Yes", "No")
    }
    df
  })
  
  observeEvent(input$reset_analysis_top, {
    shinyjs::removeClass(id = "main_panel", class = "col-sm-12")
    shinyjs::addClass(id = "main_panel", class = "col-sm-9")
    shinyjs::show("sidebar_panel", anim = TRUE, animType = "fade", time = 0.4)
    
    pipeline_success(FALSE)
    final_results(NULL)
    annot_qc_data(NULL)
    stored_plots$curves <- list()
    stored_plots$dist <- NULL
    stored_plots$ed_ces <- NULL
  })
  

  # Main analysis pipeline logic
  observeEvent(input$run_analysis, ignoreInit = TRUE, {
    
    if (isTRUE(is_running())) return()
    is_running(TRUE)
    
    shinyjs::disable("run_analysis")
    
    on.exit({ 
      shinyjs::enable("run_analysis")
      shinyjs::hide("progress_overlay") 
      is_running(FALSE)
    }, add = TRUE)
    
    if (is.null(input$file_upload)) {
      notify("Please upload a data file before running the analysis.", type = "warning", id = "ces_validation")
      return()
    }
    
    removeNotification("ces_validation")
    removeNotification("ces_warn_names")
    removeNotification("ces_warn_na")
    removeNotification("ces_warn_zero")
    removeNotification("ces_warn_plates")
    removeNotification("ces_run_status")
    removeNotification("ces_render")
    
    pipeline_success(FALSE)
    final_results(NULL)
    annot_qc_data(NULL)
    
    stored_plots$curves <- list(); stored_plots$dist <- NULL; stored_plots$ed_ces <- NULL
    
    ext <- tolower(tools::file_ext(input$file_upload$name))
    if (!(ext %in% ALLOWED_EXTS)) {
      notify("Invalid file type. Please upload a .csv, .xlsx, .xls, .txt, or .tsv file.", type = "error")
      return()
    }
    
    df <- tryCatch({
      if (ext == "csv") { 
        suppressWarnings(suppressMessages(read_csv(input$file_upload$datapath, show_col_types = FALSE, name_repair = "minimal"))) 
      } else if (ext %in% c("xls", "xlsx")) { 
        suppressWarnings(suppressMessages(read_excel(input$file_upload$datapath, sheet = 1, .name_repair = "minimal"))) 
      } else {
        suppressWarnings(suppressMessages(read_tsv(input$file_upload$datapath, show_col_types = FALSE, name_repair = "minimal")))
      }
    }, error = function(e) {
      notify("Failed to read the file. Please ensure it is uncorrupted and matches the documentation guidelines.", type = "error")
      return(NULL)
    })
    
    if (is.null(df)) return()
    
    if (nrow(df) == 0) { 
      notify("The uploaded file is empty.", type = "error")
      return() 
    }
    if (nrow(df) > MAX_ROWS) { 
      notify(paste0("File exceeds the maximum row limit (", format(MAX_ROWS, big.mark = ","), " rows)."), type = "error")
      return() 
    }
    if (ncol(df) > MAX_COLS) {
      notify(paste0("File exceeds the maximum column limit (", MAX_COLS, " columns)."), type = "error")
      return()
    }
    
    df <- as.data.frame(df)
    colnames(df) <- toupper(trimws(colnames(df)))
    
    if (anyDuplicated(colnames(df)) > 0) {
      dup_names <- unique(colnames(df)[duplicated(colnames(df))])
      notify(paste("Duplicate column names detected:", paste(head(dup_names, 5), collapse = ", "),
                   ". Please ensure each column has a unique header."), type = "error")
      return()
    }
    
    if (!("DRUG" %in% colnames(df)) && "DRUG_NAME" %in% colnames(df)) {
      colnames(df)[colnames(df) == "DRUG_NAME"] <- "DRUG"
    }
    if ("CONDITION" %in% colnames(df)) {
      df$SCREEN <- df$CONDITION
    } else if (!("SCREEN" %in% colnames(df)) && "SCREEN_NAME" %in% colnames(df)) {
      colnames(df)[colnames(df) == "SCREEN_NAME"] <- "SCREEN"
    }
    if (!("RESPONSE" %in% colnames(df)) && "PERCENT_INHIBITION" %in% colnames(df)) {
      colnames(df)[colnames(df) == "PERCENT_INHIBITION"] <- "RESPONSE"
    }
    
    is_antiviral  <- input$screening_type == "Antiviral"
    is_annotation <- input$screening_type == "Drug sensitivity" && input$data_format == "Annotation file"
    
    run_settings$is_antiviral <- is_antiviral
    
    if (is_annotation) {
      req_cols_raw <- c("WELL", "PLATE", "DRUG", "CONCENTRATION", "SCREEN", "WELL_SIGNAL")
      missing_cols <- setdiff(req_cols_raw, colnames(df))
      if (length(missing_cols) > 0) { 
        notify(paste("Missing required columns for Annotation file:", paste(missing_cols, collapse = ", ")), type = "error")
        return() 
      }
      
      df$WELL_SIGNAL <- suppressWarnings(as.numeric(df$WELL_SIGNAL))
      if (all(is.na(df$WELL_SIGNAL))) {
        notify("WELL_SIGNAL column must contain numeric data.", type = "error")
        return()
      }
      
      annot_result <- tryCatch({
        df$DRUG_CLEAN <- trimws(toupper(as.character(df$DRUG)))
        
        pos_ctrls <- c("BZCL", "POS")
        mono_neg_ctrls <- c("DMSO", "NEG")
        cc_neg_ctrls <- c("DMSO2", "NEG2")
        
        if (!("Mono" %in% df$SCREEN) || !("Co-culture" %in% df$SCREEN)) {
          notify("Annotation file data requires both Co-culture and Mono screen identifiers.", type = "error")
          return(NULL)
        }
        
        cc_drugs <- df$DRUG_CLEAN[df$SCREEN == "Co-culture"]
        if (!any(pos_ctrls %in% cc_drugs) || !any(cc_neg_ctrls %in% cc_drugs)) {
          notify("Co-culture plates strictly require positive (BZCL/POS) and negative (DMSO2/NEG2) controls in the DRUG column.", type = "error")
          return(NULL)
        }
        
        mono_drugs <- df$DRUG_CLEAN[df$SCREEN == "Mono"]
        if (!any(pos_ctrls %in% mono_drugs) || !any(mono_neg_ctrls %in% mono_drugs)) {
          notify("Mono plates strictly require positive (BZCL/POS) and negative (DMSO/NEG) controls in the DRUG column.", type = "error")
          return(NULL)
        }
        
        if ("Control" %in% df$SCREEN) {
          ctrl_drugs <- df$DRUG_CLEAN[df$SCREEN == "Control"]
          if (!any(pos_ctrls %in% ctrl_drugs) || !any(mono_neg_ctrls %in% ctrl_drugs)) {
            notify("Control plates require positive (BZCL/POS) and negative (DMSO/NEG) controls in the DRUG column.", type = "error")
            return(NULL)
          }
        }
        
        df$COLUMN <- as.numeric(gsub("[A-Za-z]", "", df$WELL))
        df$CONTENT <- "Drug"
        df$CONTENT[df$DRUG_CLEAN %in% c("CELLS", "EMPTY", "", "NA") | is.na(df$DRUG)] <- "Cells/Empty"
        df$CONTENT[df$DRUG_CLEAN %in% pos_ctrls] <- "BzCl"
        df$CONTENT[df$DRUG_CLEAN %in% mono_neg_ctrls & df$SCREEN == "Mono"] <- "DMSO (Target)"
        df$CONTENT[df$DRUG_CLEAN %in% mono_neg_ctrls & df$SCREEN == "Control"] <- "DMSO (Control)"
        df$CONTENT[df$DRUG_CLEAN %in% cc_neg_ctrls & df$SCREEN == "Co-culture"] <- "DMSO (Co-culture)"
        
        annot_qc_data(df)
        
        grouping_cols <- if ("PLATE" %in% colnames(df)) c("PLATE", "SCREEN") else c("SCREEN")
        
        df_processed <- df %>%
          group_by(across(all_of(grouping_cols))) %>%
          mutate(
            avg_low = mean(WELL_SIGNAL[DRUG_CLEAN %in% pos_ctrls], na.rm = TRUE),
            avg_high = ifelse(SCREEN == "Co-culture", 
                              mean(WELL_SIGNAL[DRUG_CLEAN %in% cc_neg_ctrls], na.rm = TRUE), 
                              mean(WELL_SIGNAL[DRUG_CLEAN %in% mono_neg_ctrls], na.rm = TRUE))
          ) %>%
          ungroup()
        
        if (any(is.na(df_processed$avg_high)) || any(is.na(df_processed$avg_low))) {
          notify("Some plates or screens are missing required control wells. Cannot compute inhibition.", type = "error")
          return(NULL)
        }
        
        df_out <- df_processed %>%
          mutate(
            denom = avg_high - avg_low,
            RESPONSE = ifelse(!is.finite(denom) | denom == 0, NA_real_, ((avg_high - WELL_SIGNAL) / denom) * 100)
          ) %>%
          filter(!(DRUG_CLEAN %in% c("BZCL", "POS", "DMSO", "NEG", "DMSO2", "NEG2", "CELLS", "EMPTY")))
        
        n_bad_resp <- sum(is.na(df_out$RESPONSE))
        if (n_bad_resp > 0) {
          df_out <- df_out %>% filter(!is.na(RESPONSE))
          notify(
            paste("Dropped", n_bad_resp, "row(s) from plates with degenerate control values (positive == negative). Other plates were processed normally."),
            type = "warning", id = "ces_warn_plates", duration = 10
          )
        }
        if (nrow(df_out) == 0) {
          notify("No valid plate data remaining after normalization. Please review raw well signals.", type = "error")
          return(NULL)
        }
        df_out
      }, error = function(e) {
        notify("Error during annotation formatting. Please verify your file matches the required format.", type = "error")
        return(NULL)
      })
      
      if (is.null(annot_result)) return()
      df <- annot_result
    }
    
    required_cols <- c("DRUG", "RESPONSE", "CONCENTRATION", "SCREEN")
    missing_req <- setdiff(required_cols, colnames(df))
    if (length(missing_req) > 0) { 
      notify(paste("Missing required columns:", paste(missing_req, collapse = ", ")), type = "error")
      return() 
    }
    
    df$Drug <- as.character(df$DRUG)
    df$Screen <- as.character(df$SCREEN)
    df$Response <- suppressWarnings(as.numeric(df$RESPONSE))
    df$Concentration <- suppressWarnings(as.numeric(df$CONCENTRATION))
    
    long_names <- nchar(df$Drug) > MAX_DRUG_NAME_LEN
    if (any(long_names, na.rm = TRUE)) {
      df$Drug[long_names] <- substr(df$Drug[long_names], 1, MAX_DRUG_NAME_LEN)
      notify(paste(sum(long_names, na.rm = TRUE), "drug name(s) were truncated to", MAX_DRUG_NAME_LEN, "characters."), 
             type = "warning", id = "ces_warn_names")
    }
    
    bad_rows <- is.na(df$Response) | is.na(df$Concentration) | !is.finite(df$Response) | !is.finite(df$Concentration)
    n_bad <- sum(bad_rows)
    if (n_bad > 0) {
      notify(paste("Removed", n_bad, "row(s) with non-numeric, missing, or infinite values."), 
             type = "warning", id = "ces_warn_na")
      df <- df[!bad_rows, ]
    }
    if (nrow(df) == 0) { 
      notify("No valid numeric data remaining after cleaning.", type = "error")
      return() 
    }
    
    n_zero <- sum(df$Concentration <= 0)
    if (n_zero > 0) {
      notify(paste("Removed", n_zero, "row(s) with zero or negative concentration values."), 
             type = "warning", id = "ces_warn_zero")
      df <- df[df$Concentration > 0, ]
    }
    if (nrow(df) == 0) { 
      notify("No valid concentration data remaining.", type = "error")
      return() 
    }
    
    allowed_screens <- if (is_antiviral) c("Co-culture", "Mock") else c("Co-culture", "Mono", "Control")
    invalid_screens <- setdiff(unique(df$Screen), allowed_screens)
    
    if (length(invalid_screens) > 0) { 
      msg <- if (is_antiviral) "For Antiviral screening, only Co-culture and Mock are allowed." 
      else "Only Co-culture, Mono, and Control are allowed."
      notify(paste("Invalid SCREEN value detected:", safe_text(paste(invalid_screens, collapse=", ")), ".", msg), type = "error")
      return() 
    }
    
    screens <- unique(df$Screen)
    if (!"Co-culture" %in% screens) { 
      notify("Dataset must contain Co-culture data.", type = "error")
      return() 
    }
    
    if (is_antiviral) {
      if (!"Mock" %in% screens) { 
        notify("Antiviral datasets must contain Mock (Host cell viability) data.", type = "error")
        return() 
      }
    } else {
      if (!"Mono" %in% screens) { 
        notify("Dataset must contain Mono alongside Co-culture. Control is optional.", type = "error")
        return() 
      }
    }
    
    bad_drugs <- c()
    for (dr in unique(df$Drug)) { 
      if (length(unique(df$Concentration[df$Drug == dr])) <= 2) bad_drugs <- c(bad_drugs, dr) 
    }
    if (length(bad_drugs) > 0) { 
      notify(paste0(length(bad_drugs), " drug(s) require more than two tested concentrations. First few: ", 
                    safe_text(paste(head(bad_drugs, 3), collapse=", "))), type = "error")
      return() 
    }
    
    if (is_antiviral) {
      df$Response[df$Screen == "Mock"] <- 100 - df$Response[df$Screen == "Mock"]
    } else if (!is_annotation) {
      if (!is.null(input$readout_type) && input$readout_type == "Viability (%)") {
        df$Response <- 100 - df$Response
      }
    }
    
    has_mono <- "Mono" %in% screens
    has_mock <- "Mock" %in% screens
    has_control <- "Control" %in% screens
    results_list <- list()
    
    df_split <- split(df, df$Drug)
    unique_drugs <- names(df_split)
    total_drugs <- length(unique_drugs)
    
    failed_drugs <- character(0)
    
    shinyjs::show("progress_overlay")
    
    withProgress(message = 'Running CES pipeline...', detail = 'Please wait...', value = 0, {
      
      for (i in seq_along(unique_drugs)) {
        dr <- unique_drugs[i]
        incProgress(1/total_drugs, detail = paste("Processing:", safe_text(dr, 50)))
        
        per_drug_result <- tryCatch(suppressWarnings(suppressMessages({
          
          df_dr <- df_split[[dr]]
          screen_scr2_dr <- df_dr[df_dr$Screen == "Co-culture", c("Drug", "Concentration", "Screen", "Response")]
          
          screen_coculture <- fit.screen_CC(screen_scr2_dr)
          slope_cc <- screen_coculture$slope; DSS_CC <- screen_coculture$dss; residual_CC <- screen_coculture$sd_model
          y.points_coculture <- screen_coculture$y.points 
          
          if (has_mono || has_mock) {
            target_screen_name <- if (is_antiviral) "Mock" else "Mono"
            screen_scr1_dr <- df_dr[df_dr$Screen == target_screen_name, c("Drug", "Concentration", "Screen", "Response")]
            
            if (is_antiviral) { screen_target <- fit.screen_control(screen_scr1_dr) } 
            else { screen_target <- fit.screen_mono(screen_scr1_dr, screen_coculture$flag_CC) }
            
            screen_target2 <- fit.screen_CC(screen_scr1_dr) 
            y.points_target <- screen_target$y.points; DSS_Target <- screen_target$dss; residual_target <- screen_target$sd_model; slope_mono <- screen_target2$slope
          } else { 
            y.points_target <- 0; DSS_Target <- 0; residual_target <- 0; slope_mono <- 0; screen_target2 <- NULL 
          }
          
          if (has_control) {
            screen_scr3_dr <- df_dr[df_dr$Screen == "Control", c("Drug", "Concentration", "Screen", "Response")]
            fit_ctrl <- fit.screen_control(screen_scr3_dr); fit_ctrl_cc <- fit.screen_CC(screen_scr3_dr)
            y.points_control <- fit_ctrl$y.points; DSS_control <- fit_ctrl$dss; residual_control <- fit_ctrl$sd_model
            
            is_toxic <- ifelse(!is.na(DSS_control) & DSS_control >= DEFAULT_TOX_THRESHOLD, "Yes", "No")
            
          } else { 
            y.points_control <- 0; DSS_control <- NA_real_; residual_control <- NA_real_; fit_ctrl_cc <- NULL
            is_toxic <- "No" 
          }
          
          if (is_antiviral || !has_control) {
            y_integrated <- y.points_coculture - y.points_target
          } else {
            if (!is.null(input$scoring_model) && input$scoring_model == "Therapeutic") {
              y_integrated <- y.points_coculture - (y.points_target + y.points_control - (y.points_target * y.points_control) / 100)
            } else {
              control_tox_fraction <- pmax(0, pmin(y.points_control, 100)) / 100
              delta_kill <- y.points_coculture - y.points_target
              y_integrated <- delta_kill * (1 - control_tox_fraction)
            }
          }
          
          mat_new <- data.frame(x = screen_coculture$doses, y = y_integrated)
          mat_new$y <- pmin(pmax(mat_new$y, -100), 100)
          
          res_score <- Fit_Gaussian_CoCulture(mat_new)
          is_bad_fit <- any(c(residual_target, residual_CC, residual_control) >= 30, na.rm = TRUE)
          exclude_curve <- filter_curves(res_score$CES, slope_cc, slope_mono, DSS_CC, DSS_Target)
          
          CES_val <- ifelse(exclude_curve, NA_real_, res_score$CES)
          
          n_doses <- length(screen_coculture$df$logconc)
          
          conc_vals <- signif(10^screen_coculture$df$logconc, 4)
          conc_list <- setNames(as.list(conc_vals), paste0("Conc_", 1:n_doses))
          
          if (has_mono || has_mock) {
            target_vals <- round(screen_target2$df$inhibition, 2)
            length(target_vals) <- n_doses 
          } else {
            target_vals <- rep(NA_real_, n_doses)
          }
          target_list <- setNames(as.list(target_vals), paste0("D", 1:n_doses, "_target"))

          cc_vals <- round(screen_coculture$df$inhibition, 2)
          length(cc_vals) <- n_doses
          cc_list <- setNames(as.list(cc_vals), paste0("D", 1:n_doses, "_coculture"))
          
          if (has_control) {
            control_vals <- round(fit_ctrl_cc$df$inhibition, 2)
            length(control_vals) <- n_doses
          } else {
            control_vals <- rep(NA_real_, n_doses)
          }
          control_list <- setNames(as.list(control_vals), paste0("D", 1:n_doses, "_control"))
          
          # Build base results 
          res_df <- data.frame(
            Drug = dr, 
            CES = CES_val, 
            nAUC = ifelse(exclude_curve, NA_real_, res_score$normalized_AUC), 
            Peak = ifelse(exclude_curve, NA_real_, res_score$highest_peak), 
            Effective_Dose = ifelse(exclude_curve, NA_real_, res_score$effective_dose), 
            QC_fit = ifelse(is_bad_fit, "Fail", "Pass"), 
            
            IC50_coculture = round(screen_coculture$ic50, 2),
            IC50_target = if(has_mono || has_mock) round(screen_target$ic50, 2) else NA_real_,
            IC50_control = if(has_control) round(fit_ctrl$ic50, 2) else NA_real_,
            
            DSS_coculture = round(DSS_CC, 2),
            DSS_target = round(DSS_Target, 2),
            DSS_control = if(has_control) round(DSS_control, 2) else NA_real_,
            
            Error_coculture = round(residual_CC, 2),
            Error_target = round(residual_target, 2),
            Error_control = if(has_control) round(residual_control, 2) else NA_real_,
            stringsAsFactors = FALSE
          )

          dynamic_dose_cols <- data.frame(c(conc_list, target_list, cc_list, control_list), stringsAsFactors = FALSE)
          res_df <- cbind(res_df, dynamic_dose_cols)
          
          if (!is_antiviral && has_control) {
            res_df$Toxic <- is_toxic
          }
          
          y_axis_label <- ifelse(is_antiviral, "Cell viability (%)", "Cell killing (%)")
          plot_title_text <- if (exclude_curve) {
            paste0("Excluded (QC Filter) | Peak: ", round(res_score$highest_peak, 2), " | nAUC: ", round(res_score$normalized_AUC, 2), " | ED: ", signif(res_score$effective_dose, 3))
          } else {
            paste0("CES: ", round(CES_val, 3), " | Peak: ", round(res_score$highest_peak, 2), " | nAUC: ", round(res_score$normalized_AUC, 2), " | ED: ", signif(res_score$effective_dose, 3))
          }
          
          curve_data <- list(
            dr = dr,
            y_axis_label = y_axis_label,
            plot_title_text = plot_title_text,
            screen_coculture = list(x = screen_coculture$x, y = screen_coculture$y, df = screen_coculture$df),
            screen_target2 = if (!is.null(screen_target2)) list(x = screen_target2$x, y = screen_target2$y, df = screen_target2$df) else NULL,
            screen_control = if (!is.null(fit_ctrl_cc)) list(x = fit_ctrl_cc$x, y = fit_ctrl_cc$y, df = fit_ctrl_cc$df) else NULL
          )
          
          list(res_df = res_df, curve = curve_data)
          
        })), error = function(e) {
          failed_drugs <<- c(failed_drugs, dr)
          NULL
        })
        
        if (!is.null(per_drug_result)) {
          results_list[[dr]] <- per_drug_result$res_df
          stored_plots$curves[[dr]] <- per_drug_result$curve
        }
      }
    })
    
    if (length(failed_drugs) > 0) {
      notify(
        paste0(length(failed_drugs), " of ", total_drugs, 
               " drug(s) failed during curve fitting and were safely skipped. First few: ", 
               safe_text(paste(head(failed_drugs, 3), collapse = ", "))),
        type = "warning", id = "ces_run_status", duration = 10
      )
    }
    
    if (length(results_list) == 0) {
      notify("No valid results were produced. Please verify your data structure matches the documentation.", 
             type = "error", id = "ces_run_status", duration = 10)
      return()
    }
    
    final_df <- tryCatch(
      bind_rows(results_list) %>% arrange(desc(CES)),
      error = function(e) {
        notify("Failed to aggregate results. Please verify your data formatting.", type = "error", id = "ces_run_status")
        NULL
      }
    )
    if (is.null(final_df)) return()
    
    final_results(final_df)
    pipeline_success(TRUE)
    
    shinyjs::hide("sidebar_panel", anim = TRUE, animType = "fade", time = 0.4)
    shinyjs::removeClass(id = "main_panel", class = "col-sm-9")
    shinyjs::addClass(id = "main_panel", class = "col-sm-12")
  })
  

  # Dynamic results UI if the processing is successful
  output$results_tabs_ui <- renderUI({
    df <- final_results()
    req(df, pipeline_success())
    
    df_sorted <- df %>% arrange(is.na(CES), desc(CES))
    dropdown_labels <- paste0(df_sorted$Drug, " (CES: ", ifelse(is.na(df_sorted$CES), "Excluded", round(df_sorted$CES, 2)), ")")
    dropdown_choices <- setNames(df_sorted$Drug, dropdown_labels)
    
    panel_wrap <- "max-width: 1150px; width: 100%; margin: 0 auto; padding: 0 15px;"
    
    tab_overview <- tabPanel(title = tagList(icon("table-list"), " Data overview"), 
                             br(),
                             div(style = "width: 100%;",
                                 div(class = "glass-panel", style = "padding: 25px; margin-bottom: 25px;",
                                     div(style = "display: flex; justify-content: space-between; align-items: flex-start; margin-bottom: 15px;", 
                                         div(
                                           h4("Compound efficacy rankings", style="margin-bottom: 5px; font-weight: bold; color: #2B5C8F;"), 
                                           p("Investigate CES-based rankings and key pharmacological parameters to prioritize active compounds.", style="margin-bottom: 15px; color: #4A4A4A; font-size: 14px;")
                                         ),
                                         div(class = "input-group shadow-sm", style = "width: auto; flex-wrap: nowrap; border-radius: 6px; overflow: hidden;",
                                             span(class = "input-group-text", 
                                                  style = "background-color: #eef4fa; font-weight: 600; color: #2B5C8F; border-color: #ced4da;", 
                                                  "Format"),
                                             tags$select(id = "results_dl_format", 
                                                         class = "form-select", 
                                                         style = "width: 100px; cursor: pointer; border-color: #ced4da;",
                                                         tags$option(value=".xlsx", ".xlsx"),
                                                         tags$option(value=".csv", ".csv"),
                                                         tags$option(value=".rds", ".rds") 
                                             ),
                                             downloadButton("download_results", "Download", 
                                                            class = "btn-success", 
                                                            style = "border-top-left-radius: 0; border-bottom-left-radius: 0; margin: 0; z-index: 0;")
                                         )
                                     ),
                                     DTOutput("data_table")
                                 ),
                                 
                                 div(class = "glass-panel", style = "padding: 25px; margin-bottom: 25px;",
                                     div(style = "display: flex; justify-content: space-between; align-items: flex-start; margin-bottom: 15px;", 
                                         div(
                                           h4("CES distribution", style="margin-bottom: 5px; font-weight: bold; color: #2B5C8F;"), 
                                           p("Visual overview of compound activity, ranked from highest efficacy to lowest.", style="margin-bottom: 15px; color: #4A4A4A; font-size: 14px;")
                                         ),
                                         downloadButton("download_dist_plot", "Download plot (.pdf)", class = "btn-danger btn-sm")
                                     ),
                                     plotlyOutput("ces_dist_plot", width = "100%", height = "420px")
                                 ),
                                 
                                 div(class = "glass-panel", style = "padding: 25px; margin-bottom: 25px;",
                                     div(style = "display: flex; justify-content: space-between; align-items: flex-start; margin-bottom: 15px;", 
                                         div(
                                           h4("Effective dose vs CES", style="margin-bottom: 5px; font-weight: bold; color: #2B5C8F;"), 
                                           p("Examine the relationship between compound potency and overall co-culture efficacy.", style="margin-bottom: 15px; color: #4A4A4A; font-size: 14px;")
                                         ),
                                         downloadButton("download_ed_ces_plot", "Download plot (.pdf)", class = "btn-danger btn-sm")
                                     ),
                                     plotlyOutput("ed_ces_plot", width = "100%", height = "420px")
                                 ),
                                 br()
                             )
    )
    tab_fits <- tabPanel(title = tagList(icon("chart-line"), " Dose-response curves"),
                         br(),
                         div(style = "width: 100%;",
                             div(class = "glass-panel", style = "padding: 25px; margin-bottom: 30px;",
                                 div(style = "margin-bottom: 20px;",
                                     h4("Dose-response modeling", style="margin-bottom: 5px; font-weight: bold; color: #2B5C8F;"), 
                                     p("Review the fitted curves and empirical data points across all experimental conditions.", style="margin-bottom: 15px; color: #4A4A4A; font-size: 14px;")
                                 ),
                                 
                                 div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 20px;",
                                     selectInput("selected_drug", "Select drug to view:", choices = dropdown_choices, width = "400px"),
                                     div(style = "display: flex; gap: 10px;",
                                         downloadButton("download_single_plot", "Download current fit (.pdf)", 
                                                        class = "btn btn-danger btn-sm text-white", 
                                                        style = "color: white !important;"),
                                         
                                         downloadButton("download_plots", "Download all fits (.pdf)", 
                                                        class = "btn btn-danger btn-sm text-white", 
                                                        style = "color: white !important;", 
                                                        onclick = "Shiny.setInputValue(\"notify_download\", \"pdf\", {priority: \"event\"});")
                                     )
                                 ),
                                 div(style = "display: flex; justify-content: center; width: 100%;",
                                     div(style = "width: 70%; margin: 0 auto;",
                                         plotlyOutput("curve_fit_plot", width = "100%", height = "520px")
                                     )
                                 )
                             )
                         )
    )
    
    tab_list <- list(tab_overview, tab_fits)
    
    qc_df <- annot_qc_data()
    
    if (!is.null(qc_df) && nrow(qc_df) > 0) {
      if ("PLATE" %in% colnames(qc_df)) {
        plate_summary <- qc_df %>%
          dplyr::distinct(PLATE, SCREEN) %>%
          dplyr::arrange(PLATE, SCREEN) %>%
          dplyr::group_by(PLATE) %>%
          dplyr::summarise(screens = paste(unique(SCREEN), collapse = ", "), .groups = "drop")
        
        plate_ids    <- as.character(plate_summary$PLATE)
        plate_labels <- paste0("Plate ", plate_summary$PLATE, " (", plate_summary$screens, ")")
        plate_choices <- setNames(plate_ids, plate_labels)
      } else {
        plate_choices <- c("Default" = "Default")
      }
      
      tab_qc <- tabPanel(title = tagList(icon("vial-circle-check"), " Plate-level QC"),
                         br(),
                         div(style = "width: 100%;",
                             div(class = "glass-panel", style = "padding: 25px; margin-bottom: 25px;",
                                 div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                                     h4(icon("check-double"), " Plate QC statistics", style="margin-bottom: 5px; font-weight: bold; color: #2B5C8F;"),
                                     downloadButton("download_qc_stats", "Download stats (.csv)", class = "btn-success btn-sm")
                                 ),
                                 p("Calculated quality control metrics based on control well distributions.", style = "margin-bottom: 15px; color: #4A4A4A; font-size: 14px;"),
                                 DTOutput("qc_stats_table")
                             ),
                             div(class = "glass-panel", style = "padding: 25px; margin-bottom: 25px;",
                                 div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                                     h4(icon("chart-simple"), " Quality control: control distributions", style="margin-bottom: 5px; font-weight: bold; color: #2B5C8F;"),
                                     downloadButton("download_qc_boxplot", "Download plot (.pdf)", class = "btn-danger btn-sm")
                                 ),
                                 p("Distribution of raw well signals for internal positive and negative controls across experimental screens.", style = "margin-bottom: 15px; color: #4A4A4A; font-size: 14px;"),
                                 plotlyOutput("qc_boxplot", width = "100%", height = "480px")
                             ),
                             div(class = "glass-panel", style = "padding: 25px; margin-bottom: 25px;",
                                 div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;", 
                                     h4(icon("table-cells"), " Quality control: plate spatial distribution", style="margin-bottom: 5px; font-weight: bold; color: #2B5C8F;"),
                                     div(style = "display: flex; align-items: center; gap: 10px;",
                                         if ("PLATE" %in% colnames(qc_df)) selectInput("qc_plate_select", label = "Select plate:", choices = plate_choices, width = "260px") else NULL,
                                         downloadButton("download_single_qc_plate_plot", "Download current (.pdf)", class = "btn-danger btn-sm text-white"),
                                         downloadButton("download_all_qc_plate_plots", "Download all (.pdf)", 
                                                        class = "btn-danger btn-sm text-white", 
                                                        onclick = "Shiny.setInputValue('notify_download', 'pdf', {priority: 'event'});")
                                     )
                                 ),
                                 p("Spatial distribution of raw well signals across plate columns to identify edge effects.", style = "margin-bottom: 15px; color: #4A4A4A; font-size: 14px;"),
                                 plotlyOutput("qc_plate_plot", width = "100%", height = "480px")
                             ), br()
                         )
      )
      tab_list <- append(tab_list, list(tab_qc), after = 0)
    }
    
    if ("Toxic" %in% colnames(df)) {
      tab_toxic <- tabPanel(title = tagList(icon("triangle-exclamation"), " Toxicity analysis"),
                            br(),
                            div(style = "width: 100%;",
                                div(class = "glass-panel", style = "padding: 25px; margin-bottom: 30px;",
                                    h4(icon("triangle-exclamation"), " Toxicity analysis summary", style="margin-bottom: 5px; font-weight: bold; color: #2B5C8F;"),
                                    p("Tune the Control toxicity threshold (DSS) below to re-classify toxic compounds interactively. Adjustments do not re-run the analysis.", 
                                      style = "margin-bottom: 15px; color: #4A4A4A; font-size: 14px;"),
                                    div(style = "padding: 15px 20px; background-color: #f4f8fc; border-radius: 8px; border-left: 4px solid #2B5C8F; margin-bottom: 20px;",
                                        sliderInput("tox_threshold", 
                                                    label = tagList(icon("sliders"), " Control toxicity threshold (DSS)"),
                                                    min = 0, max = 30, value = DEFAULT_TOX_THRESHOLD, step = 0.5, 
                                                    width = "100%", ticks = TRUE)
                                    ),
                                    uiOutput("toxic_summary_text"),
                                    DTOutput("toxic_table")
                                )
                            )
      )
      tab_list <- append(tab_list, list(tab_toxic))
    }

    div(style = panel_wrap,
        div(style = "margin-bottom: 25px;",
            div(style = "display: flex; justify-content: space-between; align-items: flex-end; border-bottom: 2px solid #E5E4E2; padding-bottom: 15px;",
                div(
                  h3(icon("microscope"), " Analysis results", style = "margin: 0; color: #2B5C8F; font-weight: 800; font-size: 1.8rem;"),
                  p("Explore your co-culture screening metrics, distributions, and dose-response curve fits.", style = "margin: 5px 0 0 0; color: #6c757d; font-size: 1rem;")
                ),
                actionButton("reset_analysis_top", tagList(icon("arrow-rotate-left"), " Start new analysis"),
                             class = "btn-hero-cta btn-sm", 
                             style = "font-size: 1.05rem; font-weight: bold; padding: 10px 20px; cursor: pointer; border: none;")
            )
        ),
        do.call(tabsetPanel, c(tab_list, list(id = "results_tabset")))
    )
  })
  
  output$toxic_summary_text <- renderUI({
    df <- results_with_threshold()
    req(df, "Toxic" %in% colnames(df))
    thr <- if (is.null(input$tox_threshold)) DEFAULT_TOX_THRESHOLD else input$tox_threshold
    n_toxic <- sum(df$Toxic == "Yes", na.rm = TRUE)
    p(paste0("Compounds flagged as toxic to effector cells at Control DSS \u2265 ", thr, ": ",
             n_toxic, " of ", nrow(df), " compound(s)."), 
      style = "color: #4A4A4A; font-size: 14px; margin-bottom: 15px; font-weight: 500;")
  })
  
  output$data_table <- renderDT({ 
    safe_render({
      df <- results_with_threshold()
      req(df, nrow(df) > 0)

      cols_to_show <- c("Drug", "CES", "nAUC", "Peak", "Effective_Dose", "IC50_coculture")

      if ("IC50_target" %in% colnames(df) && any(!is.na(df$IC50_target))) cols_to_show <- c(cols_to_show, "IC50_target")
      if ("IC50_control" %in% colnames(df) && any(!is.na(df$IC50_control))) cols_to_show <- c(cols_to_show, "IC50_control")
      
      cols_to_show <- c(cols_to_show, "QC_fit")
      if ("Toxic" %in% colnames(df)) cols_to_show <- c(cols_to_show, "Toxic")
      
      df_display <- df[, intersect(cols_to_show, colnames(df))]
      
      cols_to_format <- intersect(c("CES", "nAUC", "Peak", "Effective_Dose", "IC50_coculture", "IC50_target", "IC50_control"), colnames(df_display))
      
      dt <- datatable(
        df_display, 
        class = "cell-border stripe hover compact", 
        options = list(
          pageLength = 10, 
          scrollX = TRUE,
          order = list(list(1, "desc")), 
          dom = '<"top"f>rt<"bottom"lip><"clear">',
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#f8f9fa', 'color': '#212529', 'font-weight': 'bold'});",
            "}"
          )
        ), 
        rownames = FALSE,
        escape = TRUE
      ) %>% 
        formatRound(columns = cols_to_format, digits = 3) %>%
        formatStyle("Drug", fontWeight = "bold") %>%
        formatStyle("QC_fit", color = styleEqual(c("Pass", "Fail"), c("#27AE60", "#DC143C")), fontWeight = "bold") %>%
        formatStyle(
          "CES", 
          color = styleInterval(c(-1e-5, 1e-5), c("#2980B9", "#95A5A6", "#DC143C")), 
          fontWeight = "bold",
          backgroundColor = styleInterval(c(-1e-5, 1e-5), c("rgba(41, 128, 185, 0.08)", "rgba(149, 165, 166, 0.08)", "rgba(220, 20, 60, 0.08)"))
        )
      
      if ("Toxic" %in% colnames(df_display)) {
        dt <- dt %>% formatStyle("Toxic", color = styleEqual(c("Yes", "No"), c("#DC143C", "#27AE60")), fontWeight = "bold")
      }
      dt
    })
  })
  
  # Output for the QC Stats Table
  output$qc_stats_table <- renderDT({
    safe_render({
      df <- annot_qc_data()
      req(df, nrow(df) > 0)
      
      stats_df <- calculate_plate_qc(df)
      req(!is.null(stats_df))
      
      datatable(
        stats_df, 
        class = 'cell-border stripe hover compact',
        options = list(
          pageLength = 10, 
          scrollX = TRUE, 
          dom = '<"top"f>rt<"bottom"lip><"clear">',
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#f8f9fa', 'color': '#212529', 'font-weight': 'bold'});",
            "}"
          )
        ),
        rownames = FALSE,
        escape = TRUE
      ) %>%
        formatStyle('Bad', target = 'row', backgroundColor = styleEqual(c(1), c('rgba(220, 20, 60, 0.1)'))) %>%
        formatStyle('Z_Prime', color = styleInterval(c(0.5), c('#DC143C', '#27AE60')), fontWeight = 'bold')
    })
  })
  
  output$download_qc_stats <- downloadHandler(
    filename = function() { paste0("CES_QC_Statistics_", Sys.Date(), ".csv") },
    content = function(file) {
      df <- annot_qc_data()
      req(df, nrow(df) > 0)
      stats_df <- calculate_plate_qc(df)
      tryCatch(
        write.csv(stats_df, file, row.names = FALSE),
        error = function(e) notify("Failed to generate QC Stats CSV.", type = "error", id = "ces_download")
      )
    }
  )
  
  output$toxic_table <- renderDT({
    safe_render({
      df <- results_with_threshold()
      req(df, "Toxic" %in% colnames(df))
      toxic_df <- df %>% 
        filter(Toxic == "Yes") %>% 
        dplyr::select(Drug, CES, nAUC, Peak, DSS_control, Effective_Dose)
    
      datatable(toxic_df, 
                class = 'cell-border stripe hover compact dt-center',
                options = list(
                  pageLength = 10, 
                  scrollX = TRUE, 
                  dom = 't', 
                  language = list(emptyTable = "No toxic compounds at the current threshold.")
                ),
                rownames = FALSE,
                escape = TRUE) %>%
        formatRound(columns = intersect(c('CES', 'nAUC', 'Peak', 'DSS_control', 'Effective_Dose'), colnames(toxic_df)), digits = 3) %>%
        formatStyle('Drug', fontWeight = 'bold', color = '#2B5C8F')
    })
  })
  
  output$ces_dist_plot <- renderPlotly({ 
    safe_render({
      df <- final_results(); req(df, nrow(df) > 0)
      df <- df %>% arrange(desc(CES))
      df$Drug <- factor(df$Drug, levels = df$Drug)
      df$Color <- ifelse(df$CES > 0, "#DC143C", ifelse(df$CES < 0, "#2980B9", "#95A5A6"))
      
      n_compounds <- nrow(df)
      
      suppressWarnings({
        p_dist <- ggplot(df, aes(x = Drug, y = CES, fill = Color, text = paste0("Drug: ", Drug, "\nCES: ", round(CES, 3)))) + 
          geom_bar(stat = "identity", color = ifelse(n_compounds > 100, NA, "white"), linewidth = 0.5) + 
          scale_fill_identity() + labs(x = "Drug", y = "CES") + theme_bw() + 
          theme(axis.text.y = element_text(size = 14, colour = "black"), 
                axis.title = element_text(size = 16, colour = "black", face = "bold"), 
                axis.ticks.length = unit(0.15, "cm"),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.y = element_blank())
        
        if (n_compounds > 100) {
          p_dist <- p_dist + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_text(margin = margin(t = 10)))
        } else {
          p_dist <- p_dist + theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1, colour = "black"))
        }
        
        stored_plots$dist <- p_dist
      })
      ggplotly(p_dist, tooltip = "text") 
    })
  })
  
  output$ed_ces_plot <- renderPlotly({ 
    safe_render({
      df <- final_results(); req(df, nrow(df) > 0)
      df_plot <- df %>% filter(!is.na(Effective_Dose) & Effective_Dose > 0 & !is.na(CES))
      req(nrow(df_plot) > 0)
      df_plot$Color <- ifelse(df_plot$CES > 0, "#DC143C", ifelse(df_plot$CES < 0, "#2980B9", "#95A5A6"))
      suppressWarnings({
        p_scatter <- ggplot(df_plot, aes(x = log10(Effective_Dose), y = CES, fill = Color, 
                                         text = paste0("Drug: ", Drug, "\nCES: ", round(CES, 3), "\nEff. Dose: ", signif(Effective_Dose, 4)))) + 
          geom_point(shape = 21, size = 4, color = "white", stroke = 0.5) + 
          scale_fill_identity() + labs(x = "Log10 (Effective dose)", y = "CES") + theme_bw() + 
          theme(axis.text.x = element_text(size = 14, colour = "black"), 
                axis.text.y = element_text(size = 14, colour = "black"), 
                axis.title = element_text(size = 16, colour = "black", face = "bold"), 
                axis.ticks.length = unit(0.15, "cm"),
                panel.grid.minor = element_blank())
        stored_plots$ed_ces <- p_scatter
      })
      ggplotly(p_scatter, tooltip = "text") 
    })
  })
  
  output$curve_fit_plot <- renderPlotly({ 
    safe_render({
      req(input$selected_drug, stored_plots$curves[[input$selected_drug]])
      is_antiviral <- run_settings$is_antiviral 
      p <- build_curve_plot(stored_plots$curves[[input$selected_drug]], is_antiviral)
      
      suppressWarnings(
        ggplotly(p, tooltip = "text") %>% 
          layout(
            legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.3),
            margin = list(b = 80) 
          )
      )
    })
  })
  
  output$qc_boxplot <- renderPlotly({
    safe_render({
      df <- annot_qc_data()
      req(df, nrow(df) > 0)
      p <- build_qc_boxplot(df)
      req(!is.null(p))
      suppressWarnings(
        ggplotly(p) %>% layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2))
      )
    })
  })
  
  build_qc_plate_title <- function(plate_df, selected_plate) {
    if (is.null(plate_df) || nrow(plate_df) == 0) return(NULL)
    plate_id <- if (!is.null(selected_plate) && nzchar(selected_plate)) selected_plate else "\u2014"
    screens <- if ("SCREEN" %in% colnames(plate_df)) {
      paste(sort(unique(as.character(plate_df$SCREEN))), collapse = ", ")
    } else ""
    if (nzchar(screens)) paste0("Plate ", plate_id, " \u2014 ", screens) else paste0("Plate ", plate_id)
  }
  
  output$qc_plate_plot <- renderPlotly({
    safe_render({
      df <- annot_qc_data()
      req(df, nrow(df) > 0)
      
      sel <- if (!is.null(input$qc_plate_select)) input$qc_plate_select else NULL
      if ("PLATE" %in% colnames(df) && !is.null(sel)) {
        df <- df %>% filter(PLATE == sel)
      }
      req(nrow(df) > 0)
      
      ttl <- build_qc_plate_title(df, sel)
      p <- build_qc_plate_plot(df, plot_title = ttl)
      req(!is.null(p))
      suppressWarnings(
        ggplotly(p, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2))
      )
    })
  })
  
  output$download_results <- downloadHandler(
    filename = function() { 
      ext <- input$results_dl_format
      if (is.null(ext)) ext <- ".csv"
      paste0("CES_Results_", Sys.Date(), ext) 
    }, 
    content = function(file) { 
      req(results_with_threshold())
      df_out <- results_with_threshold()
      ext <- input$results_dl_format
      
      tryCatch({
        df_export <- sanitize_csv(df_out)
        df_export <- df_export[, !sapply(df_export, function(col) all(is.na(col))), drop = FALSE]
        if ("Toxic" %in% colnames(df_export) && "DSS_control" %in% colnames(df_export)) {
          df_export <- df_export %>% 
            dplyr::relocate(Toxic, .after = DSS_control)
        }
        
        if (ext == ".xlsx") {
          wb <- openxlsx::createWorkbook()
          openxlsx::addWorksheet(wb, "CES_Results")

          df_export <- df_export %>% tibble::add_column(Plot = "", .after = "Drug")

          header_style <- openxlsx::createStyle(
            fontColour = "#FFFFFF", 
            fgFill = "#2B5C8F", 
            halign = "center", 
            valign = "center", 
            textDecoration = "bold",
            border = "TopBottomLeftRight",
            borderColour = "#1a3c5e"
          )
          
          openxlsx::writeData(wb, "CES_Results", df_export, headerStyle = header_style)
          
          openxlsx::addFilter(wb, "CES_Results", row = 1, cols = 1:ncol(df_export))
          
          openxlsx::setColWidths(wb, "CES_Results", cols = 1, widths = 25) 
          openxlsx::setColWidths(wb, "CES_Results", cols = 2, widths = 39) 
          openxlsx::setColWidths(wb, "CES_Results", cols = 3:ncol(df_export), widths = 14) 
          
          openxlsx::setRowHeights(wb, "CES_Results", rows = 1, heights = 25) 
          openxlsx::setRowHeights(wb, "CES_Results", rows = 2:(nrow(df_export) + 1), heights = 137)
          
          center_style <- openxlsx::createStyle(valign = "center", halign = "center")
          openxlsx::addStyle(wb, "CES_Results", style = center_style, rows = 2:(nrow(df_export) + 1), cols = 1:ncol(df_export), gridExpand = TRUE)
          
          fail_rows <- which(df_export$QC_fit == "Fail") + 1 
          if (length(fail_rows) > 0) {
            warning_style <- openxlsx::createStyle(fgFill = "#FFF2CC") 
            openxlsx::addStyle(wb, "CES_Results", style = warning_style, rows = fail_rows, cols = 1:ncol(df_export), gridExpand = TRUE, stack = TRUE)
          }
          
          is_antiviral <- run_settings$is_antiviral
          
          withProgress(message = "Building Excel file...", detail = "Rendering plots (this may take a minute)", value = 0, {
            n_drugs <- nrow(df_export)
            
            for (i in seq_len(n_drugs)) {
              dr <- df_export$Drug[i]
              incProgress(1 / n_drugs, detail = paste("Adding plot for:", safe_text(dr, 30)))
              
              if (!is.null(stored_plots$curves[[dr]])) {
                p <- build_curve_plot(stored_plots$curves[[dr]], is_antiviral)
                
                num_doses <- length(unique(stored_plots$curves[[dr]]$screen_coculture$df$logconc))
                x_angle <- if (num_doses >= 8) 45 else 0
                x_hjust <- if (num_doses >= 8) 1 else 0.5
                
                p_mini <- p + 
                  theme(legend.position = "none", 
                        plot.title = element_blank(),
                        axis.title.x = element_text(size = 11, face = "plain", margin = margin(t = 4)),
                        axis.title.y = element_text(size = 11, face = "plain", margin = margin(r = 4)),
                        axis.text.x = element_text(size = 10, face = "plain", angle = x_angle, hjust = x_hjust), 
                        axis.text.y = element_text(size = 10, face = "plain"),
                        plot.margin = margin(t = 5, r = 10, b = 5, l = 5)) 
                

                tmp <- tempfile(fileext = ".png")
                suppressWarnings(ggsave(tmp, plot = p_mini, width = 2.9, height = 1.9, dpi = 200, bg = "white"))
                
                openxlsx::insertImage(wb, "CES_Results", tmp, startRow = i + 1, startCol = 2, 
                                      width = 2.9, height = 1.9, units = "in")
              }
            }
          })
          
          openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
          
        } else if (ext == ".rds") {
          saveRDS(df_out, file = file) 
        } else {
          write.csv(df_export, file, row.names = FALSE) 
        }
      }, error = function(e) {
        notify("Failed to generate the requested file.", type = "error", id = "ces_download")
      })
    }
  )
  
  output$download_dist_plot <- downloadHandler(
    filename = function() { paste0("CES_Distribution_", Sys.Date(), ".pdf") }, 
    content = function(file) { 
      req(stored_plots$dist)
      tryCatch({
        pdf(file, width = 10, height = 6); print(stored_plots$dist); dev.off()
      }, error = function(e) {
        notify("Failed to generate distribution plot PDF.", type = "error", id = "ces_download")
      })
    }
  )
  
  output$download_ed_ces_plot <- downloadHandler(
    filename = function() { paste0("CES_EffectiveDose_Scatter_", Sys.Date(), ".pdf") }, 
    content = function(file) { 
      req(stored_plots$ed_ces)
      tryCatch({
        pdf(file, width = 8, height = 6); print(stored_plots$ed_ces); dev.off()
      }, error = function(e) {
        notify("Failed to generate scatter plot PDF.", type = "error", id = "ces_download")
      })
    }
  )
  
  output$download_plots <- downloadHandler(
    filename = function() { paste0("CES_Curve_Fits_", Sys.Date(), ".pdf") }, 
    content = function(file) { 
      req(length(stored_plots$curves) > 0)
      tryCatch({
        is_antiviral <- run_settings$is_antiviral
        
        withProgress(message = "Generating PDF...", value = 0, {
          n <- length(stored_plots$curves)
          pdf(file, width = 8, height = 6)
          for (i in seq_along(stored_plots$curves)) { 
            dr <- names(stored_plots$curves)[i]
            incProgress(1/n, detail = paste("Plotting:", safe_text(dr, 50)))
            tryCatch({
              p <- build_curve_plot(stored_plots$curves[[dr]], is_antiviral)
              suppressWarnings(print(p))
            }, error = function(e) {})
          }
          dev.off()
        })
      }, error = function(e) {
        notify("Failed to generate the curve fits PDF.", type = "error", id = "ces_download")
      })
    }
  )
  
  output$download_single_plot <- downloadHandler(
    filename = function() {
      drug_safe <- gsub("[^A-Za-z0-9_-]", "_", input$selected_drug)
      paste0("CES_Curve_", drug_safe, "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      req(input$selected_drug, stored_plots$curves[[input$selected_drug]])
      tryCatch({
        is_antiviral <- run_settings$is_antiviral
        p <- build_curve_plot(stored_plots$curves[[input$selected_drug]], is_antiviral)
        
        pdf(file, width = 8, height = 6)
        suppressWarnings(print(p))
        dev.off()
      }, error = function(e) {
        notify("Failed to generate single curve plot PDF.", type = "error", id = "ces_download")
      })
    }
  )
  
  output$download_qc_boxplot <- downloadHandler(
    filename = function() { paste0("CES_QC_ControlDistributions_", Sys.Date(), ".pdf") },
    content = function(file) {
      df <- annot_qc_data()
      req(df, nrow(df) > 0)
      tryCatch({
        p <- build_qc_boxplot(df)
        req(!is.null(p))
        pdf(file, width = 9, height = 6); print(p); dev.off()
      }, error = function(e) {
        notify("Failed to generate QC control distributions PDF.", type = "error", id = "ces_download")
      })
    }
  )
  
  
  output$download_single_qc_plate_plot <- downloadHandler(
    filename = function() {
      plate_label <- if (!is.null(input$qc_plate_select) && nzchar(input$qc_plate_select)) {
        gsub("[^A-Za-z0-9_-]", "_", as.character(input$qc_plate_select))
      } else {
        "all"
      }
      paste0("CES_QC_PlateSpatial_", plate_label, "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      df <- annot_qc_data()
      req(df, nrow(df) > 0)
      tryCatch({
        sel <- if (!is.null(input$qc_plate_select)) input$qc_plate_select else NULL
        if ("PLATE" %in% colnames(df) && !is.null(sel)) {
          df <- df %>% filter(PLATE == sel)
        }
        req(nrow(df) > 0)
        ttl <- build_qc_plate_title(df, sel)
        p <- build_qc_plate_plot(df, plot_title = ttl)
        req(!is.null(p))
        pdf(file, width = 10, height = 6); print(p); dev.off()
      }, error = function(e) {
        notify("Failed to generate QC plate spatial PDF.", type = "error", id = "ces_download")
      })
    }
  )
  

  output$download_all_qc_plate_plots <- downloadHandler(
    filename = function() { paste0("CES_QC_All_PlateSpatial_", Sys.Date(), ".pdf") },
    content = function(file) {
      df <- annot_qc_data()
      req(df, nrow(df) > 0)
      tryCatch({
        plates <- if ("PLATE" %in% colnames(df)) sort(unique(df$PLATE)) else c("Default")
        
        withProgress(message = "Generating Plate PDFs...", value = 0, {
          pdf(file, width = 10, height = 6)
          for (i in seq_along(plates)) {
            incProgress(1/length(plates), detail = paste("Plotting plate:", plates[i]))
            sub_df <- if ("PLATE" %in% colnames(df)) df %>% filter(PLATE == plates[i]) else df
            ttl <- build_qc_plate_title(sub_df, plates[i])
            p <- build_qc_plate_plot(sub_df, plot_title = ttl)
            if (!is.null(p)) suppressWarnings(print(p))
          }
          dev.off()
        })
      }, error = function(e) {
        notify("Failed to generate all QC plates PDF.", type = "error", id = "ces_download")
      })
    }
  )
  
}