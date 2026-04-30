####################################################################
#
# Selective scoring of drug effects in multicellular co-culture systems
#
# R script to transform raw plate files, Breeze experimental info, and
# annotation data into a standardized format for the CES web application.
#
# Author: Diogo Dias
# Date: 2026-04-30
#
####################################################################

####################################################################
################# Preprocess data for CES webapp ###################
####################################################################

# Load required libraries
# Install first if needed: install.packages(c("openxlsx", "reshape2"))
library(openxlsx)
library(reshape2)


# Helper function to parse concentration values
# Handles: European commas ("0,5" -> 0.5), combination drugs ("0,5/0,05" -> 0.5)
parse_concentration <- function(x) {
  x <- as.character(x)
  x <- sub("/.*", "", x)
  x <- gsub(",", ".", x)
  return(as.numeric(x))
}

# Core function to merge raw plate files + annotation + Breeze info into CES format
convert_to_CES_input <- function(raw_folder, anno_file, breeze_file,output_file = "CES_input.xlsx",
                                 condition_map = c(NK = "Co-culture", R10 = "Mono"),
                                 default_condition = "Mono") {
  # Validate inputs 
  raw_folder <- paste0(gsub("/+$", "", raw_folder), "/")
  if (!dir.exists(raw_folder)) stop("Raw folder not found: ", raw_folder, 
                                    "\n  -> Check that RAW_FOLDER points to your plate files directory.")
  if (!file.exists(anno_file)) stop("Annotation file not found: ", anno_file,
                                    "\n  -> Check that ANNO_FILE is correct and the file is in your working directory.")
  if (!file.exists(breeze_file)) stop("Breeze file not found: ", breeze_file,
                                      "\n  -> Check that BREEZE_FILE is correct and the file is in your working directory.")
  
  plate_files <- list.files(raw_folder, pattern = "\\.xlsx$")
  if (length(plate_files) == 0) stop("No .xlsx files found in: ", raw_folder)
  
  # Annotation
  annoframe <- openxlsx::read.xlsx(anno_file, sheet = 1)

  for (col in c("ProductName", "Plate", "DWell")) {
    if (!col %in% colnames(annoframe))
      stop("Annotation file is missing '", col, "' column.",
           "\n  Available columns: ", paste(colnames(annoframe), collapse = ", "))
  }
  
  if (is.null(annoframe$Content)) {
    pu <- toupper(annoframe$ProductName)
    annoframe$Content <- "sample"
    annoframe$Content[pu == "BZCL"] <- "pos"
    annoframe$Content[pu == "DMSO"] <- "neg"
    annoframe$Content[pu == "DMSO2"] <- "neg2"
    annoframe$Content[pu %in% c("CELLS", "EMPTY", "") | is.na(pu)] <- "cells"
  }
  
  # Find concentration column
  conc_col_name <- NULL
  for (cc in c("conc.(num)", "conc", "Conc", "concentration", "Concentration",
               "CONCENTRATION", "Dose", "dose")) {
    if (cc %in% colnames(annoframe)) { conc_col_name <- cc; break }
  }
  if (is.null(conc_col_name)) {
    warning("No concentration column found in annotation file.",
            "\n  Available columns: ", paste(colnames(annoframe), collapse = ", "),
            "\n  CONCENTRATION will be set to NA for all wells.")
  } else {
    message("Using concentration column: '", conc_col_name, "'")
  }
  
  # Breeze experimental info handler
  expinfo <- openxlsx::read.xlsx(breeze_file, sheet = 1)
  # Breeze format: 3 header rows + 1 column-name row + data rows = minimum 5 rows
  if (nrow(expinfo) < 5) stop("Breeze file has too few rows (need at least 5: ",
                              "3 header + 1 column names + 1 data row).\n  Got ", nrow(expinfo), " rows.",
                              "\n  -> Is this the right file?")
  expinfo <- expinfo[-1:-3, ]
  colnames(expinfo) <- expinfo[1, ]
  expinfo <- expinfo[-1, ]
  expinfo$File_name <- gsub("[.]xlsx", "", expinfo$File_name)
  
  for (col in c("screen_id", "File_name", "Plate")) {
    if (!col %in% colnames(expinfo))
      stop("Breeze file is missing '", col, "' column after parsing.",
           "\n  Found columns: ", paste(colnames(expinfo), collapse = ", "),
           "\n  -> Check that the Breeze file has the standard format.")
  }
  
  # Process plates
  screens <- gsub("[.]xlsx", "", plate_files)
  message("Found ", length(screens), " plate file(s) in ", raw_folder)
  
  wt_map <- c(sample = "Sample", pos = "Positive", neg = "Negative", neg2 = "Negative")
  all_rows <- vector("list", length(screens))
  
  for (i in seq_along(screens)) {
    scr <- screens[i]
    plate_path <- paste0(raw_folder, scr, ".xlsx")
    
    # Read plate (use custom reader if available, otherwise openxlsx)
    if (exists("read_plates", mode = "function")) {
      rawmat <- read_plates(plate_path, sheet = 1)
    } else {
      rawmat <- as.matrix(openxlsx::read.xlsx(plate_path, sheet = 1, colNames = FALSE, rowNames = FALSE))
      row_idx <- which(rawmat[, 1] %in% LETTERS[1:16])
      if (length(row_idx) == 16) {
        rawmat <- rawmat[row_idx, -1, drop = FALSE]
      } else {
        warning("Could not find row labels A-P in plate file: ", scr,
                "\n  Assuming last 16 rows x last 24 columns contain plate data.")
        if (nrow(rawmat) >= 16) rawmat <- rawmat[(nrow(rawmat) - 15):nrow(rawmat), , drop = FALSE]
      }
      if (ncol(rawmat) > 24) rawmat <- rawmat[, (ncol(rawmat) - 23):ncol(rawmat), drop = FALSE]
      if (ncol(rawmat) != 24 || nrow(rawmat) != 16)
        stop("Plate file '", scr, "' does not contain a 16x24 data block.",
             "\n  Got ", nrow(rawmat), " rows x ", ncol(rawmat), " columns.")
      mode(rawmat) <- "numeric"
    }
    dimnames(rawmat) <- list(LETTERS[1:16], 1:24)
    
    data_tbl <- reshape2::melt(rawmat)
    colnames(data_tbl) <- c("Row", "Column", "rawIntensity")
    data_tbl$DWell <- paste0(data_tbl$Row, data_tbl$Column)
    
    # Match Breeze entry
    plateinfo <- expinfo[expinfo$File_name == scr, , drop = FALSE]
    if (nrow(plateinfo) == 0) { warning("No Breeze entry for: ", scr, " -- skipping."); next }
    screen_id <- plateinfo$screen_id[1]
    plate_id <- plateinfo[["Plate"]][1]
    
    # Merge annotation & drop empty wells
    data_tbl <- merge(data_tbl, annoframe[annoframe$Plate == plate_id, ], by = "DWell", all.x = TRUE)
    data_tbl <- data_tbl[!data_tbl$Content %in% "cells" & !is.na(data_tbl$Content), ]
    
    # Condition from screen_id
    condition <- default_condition
    for (pat in names(condition_map)) {
      if (grepl(pat, screen_id, ignore.case = TRUE)) { condition <- condition_map[[pat]]; break }
    }
    
    # Well type (controls will have NA concentration - this is expected)
    well_type <- wt_map[data_tbl$Content]
    well_type[is.na(well_type)] <- "Sample"
    
    # Concentration 
    conc_vals <- if (!is.null(conc_col_name)) parse_concentration(data_tbl[[conc_col_name]]) else NA_real_
    
    all_rows[[i]] <- data.frame(
      WELL = data_tbl$DWell,
      PLATE = plate_id,
      DRUG = data_tbl$ProductName,
      CONCENTRATION = conc_vals,
      SCREEN_NAME = screen_id,
      WELL_SIGNAL = as.numeric(data_tbl$rawIntensity),
      WELL_TYPE = well_type,
      CONDITION = condition,
      stringsAsFactors = FALSE
    )
    message("  ", scr, " -> ", screen_id, " (", condition, ") : ", nrow(all_rows[[i]]), " wells")
  }
  
  # Combine
  result <- do.call(rbind, all_rows)
  rownames(result) <- NULL
  
  # Diagnostics
  bad_signal <- is.na(result$WELL_SIGNAL) | is.nan(result$WELL_SIGNAL) | is.infinite(result$WELL_SIGNAL)
  bad_conc <- is.na(result$CONCENTRATION) | is.nan(result$CONCENTRATION) | is.infinite(result$CONCENTRATION)
  bad_drug <- is.na(result$DRUG) | result$DRUG == ""
  sample_bad_conc <- bad_conc & result$WELL_TYPE == "Sample"
  ctrl_na_conc <- bad_conc & result$WELL_TYPE != "Sample"
  sample_conc <- result$CONCENTRATION[result$WELL_TYPE == "Sample"]
  
  # Flag combination drugs for user awareness
  combo_drugs <- grep("/", result$DRUG, value = TRUE)
  n_combo <- length(combo_drugs)
  
  message("\n-- Summary --")
  message("Total wells: ", nrow(result), "  |  Drugs: ", length(unique(na.omit(result$DRUG))),
          "  |  Screens: ", length(unique(result$SCREEN_NAME)),
          "  |  Conditions: ", paste(unique(result$CONDITION), collapse = ", "))
  print(table(result$WELL_TYPE))
  message("\n-- Issues --")
  message("  Bad/missing WELL_SIGNAL:            ", sum(bad_signal))
  message("  Missing CONCENTRATION (samples):    ", sum(sample_bad_conc),
          if (sum(sample_bad_conc) > 0) "  *** CHECK THIS ***" else "")
  message("  Missing CONCENTRATION (controls):   ", sum(ctrl_na_conc), "  (expected)")
  message("  Missing DRUG name:                  ", sum(bad_drug))
  if (n_combo > 0) {
    message("  Combination drugs (used 1st conc):  ", length(unique(combo_drugs)),
            " drug(s), ", n_combo, " well(s)")
  }
  if (length(sample_conc[!is.na(sample_conc)]) > 0) {
    message("  Sample conc range: ", min(sample_conc, na.rm = TRUE),
            " - ", max(sample_conc, na.rm = TRUE))
  }
  
  unmatched <- unique(result$SCREEN_NAME[result$CONDITION == default_condition])
  if (length(unmatched) > 0) {
    message("\n  Note: These screens used the default condition ('", default_condition, "'):")
    message("    ", paste(unmatched, collapse = ", "))
    message("    -> Add patterns to CONDITION_MAP if this is wrong.")
  }
  
  # Save 
  fmt <- tolower(tools::file_ext(output_file))
  if (fmt == "csv") {
    write.csv(result, output_file, row.names = FALSE)
  } else {
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "CES_Input")
    openxlsx::writeData(wb, "CES_Input", result)
    openxlsx::setColWidths(wb, "CES_Input", cols = 1:ncol(result), widths = "auto")
    
    flagged_idx <- bad_signal | sample_bad_conc | bad_drug
    if (any(flagged_idx)) {
      flagged <- result[flagged_idx, ]
      flagged$FLAG <- ""
      flagged$FLAG[bad_signal[flagged_idx]]       <- paste0(flagged$FLAG[bad_signal[flagged_idx]],       "BAD_SIGNAL; ")
      flagged$FLAG[sample_bad_conc[flagged_idx]]  <- paste0(flagged$FLAG[sample_bad_conc[flagged_idx]],  "BAD_CONC; ")
      flagged$FLAG[bad_drug[flagged_idx]]         <- paste0(flagged$FLAG[bad_drug[flagged_idx]],         "NO_DRUG; ")
      openxlsx::addWorksheet(wb, "Flagged_Rows")
      openxlsx::writeData(wb, "Flagged_Rows", flagged)
      openxlsx::setColWidths(wb, "Flagged_Rows", cols = 1:ncol(flagged), widths = "auto")
      openxlsx::addStyle(wb, "Flagged_Rows",
                         openxlsx::createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE"),
                         rows = 2:(nrow(flagged) + 1), cols = 1:ncol(flagged), gridExpand = TRUE)
    }
    openxlsx::saveWorkbook(wb, output_file, overwrite = TRUE)
  }
  message("\nSaved: ", output_file)
  invisible(result)
}



# USER INPUTS - Edit this section to match your data
# Step 1: Set your working directory (the folder containing your data files)
setwd("")  # <-- Paste your folder path here, e.g. "C:/Users/name/PhD/Data/MyScreen"

# Step 2: Specify your input files
RAW_FOLDER <- "./Raw/" # Folder with raw plate .xlsx files
ANNO_FILE <- "Annotations_FO5A_2_NK.xlsx" # Annotation file
BREEZE_FILE <- "Breeze_all.xlsx" # Breeze experimental info file
OUTPUT_NAME <- "CES_input.xlsx" # Output file (.xlsx or .csv)

# Step 3: Define condition mapping
# How to identify conditions from your Breeze screen_id column.
# Left side = pattern to search for in screen_id (case-insensitive)
# Right side = condition label (must be one of: "Co-culture", "Mono", "Control", "Mock")
#
# The first matching pattern wins, so put specific patterns before general ones.
# Example: screen_id "OCIAML3_NK_24h" contains "NK" -> mapped to "Co-culture"
CONDITION_MAP <- c(
  # Co-culture screen
  "NK" = "Co-culture",
  "CAR-T" = "Co-culture",
  "CART" = "Co-culture",
  "CAR-T" = "Co-culture",
  "CAR" = "Co-culture",
  "T-cell" = "Co-culture",
  "Tcell" = "Co-culture",
  "PBMC" = "Co-culture",
  "Macrophage"= "Co-culture",
  "DC" = "Co-culture",
  # Monoculture / Target-only screen
  "R10" = "Mono",
  "Target" = "Mono",
  "Mono" = "Mono",
  # Controls / Effector screen
  "Control" = "Control",
  "CTRL" = "Control"
  # Add your own patterns here if needed
)

# If no pattern matches, assign this condition:
DEFAULT_CONDITION <- "Mono"

# Run 
final_data <- convert_to_CES_input(
  raw_folder = RAW_FOLDER,
  anno_file = ANNO_FILE,
  breeze_file = BREEZE_FILE,
  output_file = OUTPUT_NAME,
  condition_map = CONDITION_MAP,
  default_condition = DEFAULT_CONDITION
)

message("\nSuccess! Your file is ready for the CES web application (Annotation format).")

