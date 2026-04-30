
####################################################################
# Selective scoring of drug effects in multicellular co-culture systems
#
# Description:
# R script for the CES functions 
#
# Author: Diogo Dias
# Date: 2026-04-30
#
####################################################################

####################################################################
############################# CES functions #############################
####################################################################


# Dose interpolation

#' Compute interpolated dose intervals
#'
#' Between each consecutive pair of doses, inserts 15 evenly spaced
#' intermediate values. Returns the full sorted vector of original +
#' interpolated doses.
#'
#' @param x Numeric vector of dose values (e.g., log10-scale concentrations).
#' @return Sorted numeric vector with interpolated intervals.
compute.intervals <- function(x) {
  list.x <- list()
  index1 <- 1
  index2 <- 2
  for (i in 1:length(x)) {
    if (i == length(x)) break
    new_seq <- seq(x[index1], x[index2], length = 17)
    new_seq <- new_seq[2:16]
    list.x[[index1]] <- new_seq
    index1 <- index1 + 1
    index2 <- index2 + 1
  }
  sort(c(x, unlist(list.x)))
}


# Drug sensitivity score (DSS); see https://doi.org/10.1038/srep05193

#' Compute Drug Sensitivity Score (DSS)
#'
#' Calculates the area-based DSS from fitted dose-response curve
#' parameters, following the method of Yadav et al. (2014).
#'
#' @param ic50 Numeric. IC50 value (in original concentration units).
#' @param slope Numeric. Hill slope of the fitted curve.
#' @param max Numeric. Maximum inhibition (asymptote, 0-100 scale).
#' @param min.conc.tested Numeric. Lowest tested concentration.
#' @param max.conc.tested Numeric. Highest tested concentration.
#' @param y Numeric. Activity threshold (default 10).
#' @param DSS.type Integer. DSS variant: 1 (DSS1), 2 (DSS2, default), or 3 (DSS3).
#' @param concn_scale Numeric. Concentration scaling factor (default 1e-9 for nM).
#' @return Numeric DSS value, rounded to 4 decimal places. Returns NA if
#'   parameters are missing, 0 if IC50 >= max concentration or slope is zero.
dss <- function(ic50, slope, max, min.conc.tested, max.conc.tested,
                y = 10, DSS.type = 2, concn_scale = 1e-9) {
  
  a <- as.numeric(unname(max)); b <- as.numeric(unname(slope)); d <- 0
  ic50 <- as.numeric(unname(ic50))
  min.conc.tested <- as.numeric(unname(min.conc.tested))
  max.conc.tested <- as.numeric(unname(max.conc.tested))
  Min.Conc <- log10(min.conc.tested * concn_scale)
  Max.Conc <- max.conc.tested; x2 <- log10(Max.Conc * concn_scale)
  
  if (is.na(ic50) || is.na(b) || is.na(a) || is.na(Min.Conc) || is.na(Max.Conc)) { dss <- NA } else if (isTRUE(ic50 >= Max.Conc)) { dss <- 0 }
  else if (isTRUE(b == 0)) { dss <- 0 } else {
    if (a > 100) { a <- 100 }
    if (isTRUE(b < 0)) { b <- -b }
    c <- log10(ic50 * concn_scale)
    if (a > y) {
      if (y != 0) {
        x1 <- (c - ((log(a - y) - log(y - d)) / (b * log(10))))
        if (isTRUE(x1 < Min.Conc)) { x1 <- Min.Conc } else if (isTRUE(x1 > x2)) { x1 <- x2 }
      } else { x1 <- Min.Conc }
      int_y <- (((((a - d) * log(1 + 10^(b * (c - x2)))) / (b * log(10))) + a * x2) -
                  ((((a - d) * log(1 + 10^(b * (c - x1)))) / (b * log(10))) + a * x1)) - (y * (x2 - x1))
      total_area <- (x2 - Min.Conc) * (100 - y)
      
      if (DSS.type == 1) { norm_area <- ((int_y / total_area) * 100) }
      if (DSS.type == 2) { norm_area <- ((int_y / total_area) * 100) / log10(a)
      if (isTRUE(norm_area > 50)) { norm_area <- 0 }
      }
      if (DSS.type == 3) { norm_area <- ((int_y / total_area) * 100) * (log10(100) / log10(a)) * ((x2 - x1) / (x2 - Min.Conc)) }
      if (isTRUE(norm_area < 0 | norm_area > 100)) { dss <- 0 } else {
        dss <- round(norm_area, digits = 4) }
    } else { dss <- 0 }
  }
  return(dss)
}


# Dose-response curve fitting (co-culture)

#' Fit dose-response curve for a co-culture condition
#'
#' Fits a four-parameter log-logistic model to co-culture drug screening
#' data. Automatically detects whether the dose-response curve is inverted
#' (inhibitor-like) based on the tail of the median response.
#'
#' @details
#' Procedure: (1) Initial LL.4 fit via drc::drm for starting values.
#' (2) Boundary corrections for IC50, MIN, MAX, SLOPE. (3) Constrained NLS
#' with two starting configurations; best residual SE retained. (4) Post-fit
#' corrections. (5) DSS2 computation. (6) Interpolated predictions via
#' compute.intervals.
#'
#' @param df Data frame with 4 columns: drug name, dose, screen ID,
#'   percent inhibition (-100 to 100 scale).
#' @return A named list with: dss, df, x, y, ic50, IC50_abs, doses,
#'   y.points, sd_model, flag_CC, slope, max.
#' @seealso \code{\link{fit.screen_mono}}, \code{\link{fit.screen_control}}
fit.screen_CC <- function(df) {
  
  mat_tbl <- df
  colnames(mat_tbl) <- c("drug", "dose", "screen", "inhibition")
  mat_tbl$logconc <- log10(mat_tbl$dose)
  readoutCTX <- FALSE; DSS_typ <- 2
  product_id <- drug_name <- unique(mat_tbl$drug)
  flag_reverse <- FALSE
  mat_tbl <- mat_tbl[order(mat_tbl$dose), ]
  n_points <- ceiling(length(mat_tbl$dose) * 0.33)
  
  mat_tbl$inhibition <- replace(mat_tbl$inhibition, mat_tbl$inhibition < -100, -100)
  mat_tbl$inhibition <- replace(mat_tbl$inhibition, mat_tbl$inhibition > 100, 100)
  inhibition <- inhibition2 <- mat_tbl$inhibition
  if (all(inhibition == 0)) inhibition <- rep(0, length(inhibition))
  if (any(duplicated(inhibition))) inhibition <- seq(from = 0, length.out = length(inhibition), by = 0.01) + inhibition
  mat_tbl$inhibition <- inhibition
  
  if (sum(tail(aggregate(. ~ dose, mat_tbl, "median")["inhibition"], n_points) < 0) > 1) {
    flag_reverse <- TRUE; mat_tbl$inhibition <- -1 * mat_tbl$inhibition
  }
  
  estimate_param <- tryCatch(
    {drm(inhibition ~ logconc, data = mat_tbl, fct = LL.4(fixed = c(NA,NA,NA,NA), names = c("SLOPE","MIN","MAX","IC50")), logDose = 10, control = drmc(errorm = FALSE))},
    warning = function(w) {drm(inhibition ~ logconc, data = mat_tbl, fct = L.4(fixed = c(NA,NA,NA,NA), names = c("SLOPE","MIN","MAX","IC50")), logDose = 10)},
    error   = function(e) {drm(inhibition ~ logconc, data = mat_tbl, fct = L.4(fixed = c(NA,NA,NA,NA), names = c("SLOPE","MIN","MAX","IC50")), logDose = 10)})
  coef_estim <- coef(estimate_param); names(coef_estim) <- c("SLOPE","MIN","MAX","IC50")
  coef_estim["SLOPE"] <- coef_estim["SLOPE"] * -1
  
  coef_estim["IC50"] <- ifelse(coef_estim["MAX"] <= coef_estim["MIN"] | coef_estim["IC50"] > max(mat_tbl$dose, na.rm = TRUE), max(mat_tbl$dose, na.rm = TRUE), coef_estim["IC50"])
  coef_estim["IC50"] <- ifelse(coef_estim["IC50"] < 0, min(mat_tbl$dose, na.rm = TRUE), coef_estim["IC50"])
  coef_estim["IC50"] <- ifelse(coef_estim["IC50"] < 0, mean(mat_tbl$dose, na.rm = TRUE), coef_estim["IC50"])
  coef_estim["IC50"] <- log10(coef_estim["IC50"])
  coef_estim["IC50"] <- ifelse(coef_estim["IC50"] < min(mat_tbl$logconc), max(mat_tbl$logconc), coef_estim["IC50"])
  coef_estim["IC50"] <- ifelse(all(mat_tbl$inhibition < 0), max(mat_tbl$logconc, na.rm = TRUE), coef_estim["IC50"])
  coef_estim["MIN"] <- 0; coef_estim["MAX"] <- max(mat_tbl$inhibition, na.rm = TRUE)
  min_lower <- ifelse(min(mat_tbl$inhibition, na.rm = TRUE) > 0, min(mat_tbl$inhibition, na.rm = TRUE), 0)
  min_lower <- ifelse(min_lower >= 100, 99, min_lower)
  coef_estim["MAX"] <- ifelse(coef_estim["MAX"] > 100, 100, coef_estim["MAX"])
  coef_estim["MAX"] <- ifelse(coef_estim["MAX"] < 0, 100, coef_estim["MAX"])
  max_lower <- ifelse(max(mat_tbl$inhibition, na.rm = TRUE) > 100, coef_estim["MAX"], max(mat_tbl$inhibition, na.rm = TRUE))
  max_lower <- ifelse(max_lower < 0, coef_estim["MAX"], max(mat_tbl$inhibition, na.rm = TRUE))
  max_lower <- ifelse(max_lower < 0, 0, max_lower)
  max_lower <- ifelse(max_lower > 100, 100, max_lower)
  run_avg <- caTools::runmean(mat_tbl$inhibition, 10)
  max_upper <- ifelse(any(run_avg[-nrow(mat_tbl)] > run_avg[nrow(mat_tbl)]), max(mat_tbl$inhibition[run_avg > run_avg[nrow(mat_tbl)]]), coef_estim["MAX"])
  max_upper <- ifelse(any(mat_tbl$inhibition > max_upper), mean(mat_tbl$inhibition[mat_tbl$inhibition > max_upper]) + 5, max_upper)
  max_upper <- ifelse(max_upper < 0, coef_estim["MAX"], max_upper)
  max_upper <- ifelse(max_upper > 100, 100, max_upper)
  max_upper <- ifelse(max_lower > max_upper, coef_estim["MAX"], max_upper)
  mean_inh_last <- mean(tail(mat_tbl$inhibition, 2), na.rm = TRUE)
  if (mean_inh_last < 60) {
    if (mean_inh_last > 25) coef_estim["IC50"] <- mean(mat_tbl$logconc, na.rm = TRUE)
    else if (mean_inh_last < 25) coef_estim["IC50"] <- max(mat_tbl$logconc, na.rm = TRUE)
  }
  if (mean(mat_tbl$inhibition[1:3], na.rm = TRUE) < 5) coef_estim["IC50"] <- max(mat_tbl$logconc, na.rm = TRUE)
  if (unname(coef_estim["MIN"]) == unname(coef_estim["MAX"])) coef_estim["MAX"] <- coef_estim["MAX"] + 0.001
  
  nls_result_ic50_old <- function() {
    tryCatch({
      nls(inhibition ~ MIN + (MAX - MIN) / (1 + (10^(SLOPE * (IC50 - logconc)))), data = mat_tbl, algorithm = "port",
          start = list(SLOPE = 1, MIN = coef_estim["MIN"][[1]], MAX = coef_estim["MAX"][[1]], IC50 = coef_estim["IC50"][[1]]),
          lower = list(SLOPE = 0.1, MIN = 0, MAX = max_lower, IC50 = min(mat_tbl$logconc)),
          upper = list(SLOPE = 2.5, MIN = 0, MAX = max_upper, IC50 = max(mat_tbl$logconc)),
          control = list(warnOnly = TRUE, minFactor = 1/2048))
    }, error = function(e) {
      minpack.lm::nlsLM(inhibition ~ MIN + (MAX - MIN) / (1 + (10^(SLOPE * (IC50 - logconc)))), data = mat_tbl,
                        start = list(SLOPE = 1, MIN = coef_estim["MIN"][[1]], MAX = coef_estim["MAX"][[1]], IC50 = coef_estim["IC50"][[1]]),
                        lower = c(SLOPE = 0.1, MIN = 0, MAX = max_lower, IC50 = min(mat_tbl$logconc)),
                        upper = c(SLOPE = 2.5, MIN = 0, MAX = max_upper, IC50 = max(mat_tbl$logconc)))
    })
  }
  
  nls_result_ic50 <- nls_result_ic50_old()
  nls_result_ic50_2 <- tryCatch({
    nls(inhibition ~ MIN + (MAX - MIN) / (1 + (10^(SLOPE * (IC50 - logconc)))), data = mat_tbl, algorithm = "port",
        start = list(SLOPE = 1, MIN = coef_estim["MIN"][[1]], MAX = coef_estim["MAX"][[1]], IC50 = median(mat_tbl$logconc)),
        lower = list(SLOPE = 0.1, MIN = 0, MAX = max_lower, IC50 = min(mat_tbl$logconc)),
        upper = list(SLOPE = 2.5, MIN = 0, MAX = max_upper, IC50 = max(mat_tbl$logconc)),
        control = list(warnOnly = TRUE, minFactor = 1/2048))
  }, warning = function(w) { nls_result_ic50_old() }, error = function(e) { nls_result_ic50_old() })
  
  nls_result_ic50 <- tryCatch({summary(nls_result_ic50); nls_result_ic50}, error = function(e) {nls_result_ic50_2})
  sumIC50 <- list(summary(nls_result_ic50), summary(nls_result_ic50_2))
  ic50std_resid  <- round(sqrt(sum((sumIC50[[1]]$residuals)^2) / (length(sumIC50[[1]]$residuals) - 1)), 1)
  ic50std_resid2 <- round(sqrt(sum((sumIC50[[2]]$residuals)^2) / (length(sumIC50[[2]]$residuals) - 1)), 1)
  switch_ <- which.min(c(ic50std_resid, ic50std_resid2))
  nls_result_ic50 <- list(nls_result_ic50, nls_result_ic50_2)[[switch_]]
  
  sumIC50 <- summary(nls_result_ic50)
  ic50std_resid <- round(sqrt(sum((sumIC50$residuals)^2) / (length(sumIC50$residuals) - 1)), 1)
  max_signal <- max(mat_tbl$dose, na.rm = TRUE); min_signal <- min(mat_tbl$dose, na.rm = TRUE)
  
  coef_ic50 <- coef(nls_result_ic50)[c("IC50","SLOPE","MAX","MIN")]; coef_ic50["IC50"] <- 10^coef_ic50["IC50"]
  coef_ic50["IC50"] <- ifelse(coef_ic50["SLOPE"] < 0, max_signal, coef_ic50["IC50"])
  coef_ic50["IC50"] <- ifelse(coef_ic50["MAX"] < 0, max_signal, coef_ic50["IC50"])
  coef_ic50["IC50"] <- ifelse(coef_ic50["MAX"] < 10, max_signal, coef_ic50["IC50"])
  coef_ic50["MAX"]  <- ifelse(coef_ic50["MAX"] < 0, 0, coef_ic50["MAX"])
  coef_ic50["IC50"] <- ifelse(all(c(max(mat_tbl$inhibition, na.rm = TRUE), min(mat_tbl$inhibition, na.rm = TRUE)) > 50), min_signal, coef_ic50["IC50"])
  
  x <- seq(min(mat_tbl$logconc), max(mat_tbl$logconc), length = 100)
  yic <- predict(nls_result_ic50, data.frame(logconc = x))
  x.new <- compute.intervals(log10(mat_tbl$dose))
  y.points <- predict(nls_result_ic50, data.frame(logconc = x.new))
  
  mat_tblCp <- mat_tbl[, c("inhibition", "dose")]
  X <- data.table::as.data.table(mat_tblCp)
  mat_tblCp <- as.data.frame(X[, list(inhibition = mean(inhibition)), by = "dose"])
  perInh <- t(matrix(mat_tblCp[, "inhibition"], dimnames = list(paste0("D", 1:nrow(mat_tblCp)))))
  
  coef_tec50 <- coef_ic50
  coef_tec50["IC50"] <- ifelse(coef_tec50["MAX"] > 25, coef_tec50["IC50"], max(mat_tbl$dose, na.rm = TRUE))
  names(coef_tec50) <- c("EC50","SLOPE","MAX","MIN")
  coef_tec50["SLOPE"] <- -1 * coef_tec50["SLOPE"]
  tmp <- coef_tec50["MAX"]; coef_tec50["MAX"] <- 100 - coef_tec50["MIN"]; coef_tec50["MIN"] <- 100 - tmp
  
  dss_score <- round(as.numeric(dss(coef_ic50["IC50"], coef_ic50["SLOPE"], coef_ic50["MAX"], min_signal, max_signal, DSS.type = as.integer(DSS_typ))), 1)
  
  xIC50ABS <- seq(min(mat_tbl$logconc), max(mat_tbl$logconc) * 15, length = 5000)
  yicIC50ABS <- predict(nls_result_ic50, data.frame(logconc = xIC50ABS))
  coef_ic50ABS <- if (all(yicIC50ABS < 50)) Inf else 10^xIC50ABS[which.min(abs(yicIC50ABS - 50))]
  
  if (flag_reverse) {
    dss_score <- -1 * dss_score; yic <- -1 * yic; y.points <- -1 * y.points
    coef_ic50["SLOPE"] <- -coef_ic50["SLOPE"]; coef_ic50["MAX"] <- -coef_ic50["MAX"]
  }
  mat_tbl$inhibition <- inhibition2
  
  list(dss = dss_score, df = mat_tbl, x = x, y = yic, ic50 = coef_ic50["IC50"], doses = x.new,
       sd_model = ic50std_resid, IC50_abs = coef_ic50ABS, y.points = y.points,
       flag_CC = flag_reverse, slope = coef_ic50["SLOPE"], max = coef_ic50["MAX"])
}


# Dose-response curve fitting (target monoculture)

#' Fit dose-response curve for a mono-culture (target-only) condition
#'
#' Fits the same four-parameter log-logistic model as fit.screen_CC but
#' applies direction-dependent logic: if the co-culture is an inhibitor
#' (flag_CC = TRUE) but the target-only response increases, the mono
#' curve is flattened to zero with additional noise (no independent target-cell killing).
#' Conversely, if co-culture is an enhancer but target-only decreases,
#' the mono curve is also flattened with additional noise. This is to prevent overbiased
#' co-culture effects from minimal co-culture effect to overbiased target monoculture responses.
#'
#' @param df Data frame with 4 columns: drug name, dose, screen ID,
#'   percent inhibition.
#' @param flag_CC Logical. Whether the paired co-culture curve was
#'   detected as an inhibitor (from fit.screen_CC()$flag_CC).
#' @return Named list of fitted parameters (same structure as
#'   fit.screen_CC), plus case_debug indicating correction branch.
#' @seealso \code{\link{fit.screen_CC}}, \code{\link{fit.screen_control}}
fit.screen_mono <- function(df, flag_CC) {
  
  mat_tbl <- df
  colnames(mat_tbl) <- c("drug", "dose", "screen", "inhibition")
  mat_tbl$logconc <- log10(mat_tbl$dose)
  readoutCTX <- FALSE; DSS_typ <- 2
  product_id <- drug_name <- unique(mat_tbl$drug)
  flag_reverse <- FALSE
  mat_tbl <- mat_tbl[order(mat_tbl$dose), ]
  n_points <- ceiling(length(mat_tbl$dose) * 0.33)
  
  mat_tbl$inhibition <- replace(mat_tbl$inhibition, mat_tbl$inhibition < -100, -100)
  mat_tbl$inhibition <- replace(mat_tbl$inhibition, mat_tbl$inhibition > 100, 100)
  inhibition <- inhibition2 <- mat_tbl$inhibition
  if (all(inhibition == 0)) inhibition <- rep(0, length(inhibition))
  if (any(duplicated(inhibition))) inhibition <- seq(from = 0, length.out = length(inhibition), by = 0.01) + inhibition
  mat_tbl$inhibition <- inhibition
  
  ## Direction-dependent flattening
  if (flag_CC) {
    case_debug <- "Neg"
    if (sum(tail(aggregate(. ~ dose, mat_tbl, "median")["inhibition"], n_points) > 0) > 1) {
      inhibition <- rep(0, length(inhibition))
      if (any(duplicated(inhibition))) inhibition <- seq(from = 0, length.out = length(inhibition), by = 0.01) + inhibition
      mat_tbl$inhibition <- inhibition
      case_debug <- "Neg_fix"
    }
  } else {
    case_debug <- "Pos"
    if (sum(tail(aggregate(. ~ dose, mat_tbl, "median")["inhibition"], n_points) < 0) > 1) {
      inhibition <- rep(0, length(inhibition))
      if (any(duplicated(inhibition))) inhibition <- seq(from = 0, length.out = length(inhibition), by = 0.01) + inhibition
      mat_tbl$inhibition <- inhibition
      case_debug <- "Pos_fix"
    }
  }
  
  if (sum(tail(aggregate(. ~ dose, mat_tbl, "median")["inhibition"], n_points) < 0) > 1) {
    flag_reverse <- TRUE; mat_tbl$inhibition <- -1 * mat_tbl$inhibition
    case_debug <- "Reverse"
  }
  
  estimate_param <- tryCatch(
    {drm(inhibition ~ logconc, data = mat_tbl, fct = LL.4(fixed = c(NA,NA,NA,NA), names = c("SLOPE","MIN","MAX","IC50")), logDose = 10, control = drmc(errorm = FALSE))},
    warning = function(w) {drm(inhibition ~ logconc, data = mat_tbl, fct = L.4(fixed = c(NA,NA,NA,NA), names = c("SLOPE","MIN","MAX","IC50")), logDose = 10)},
    error   = function(e) {drm(inhibition ~ logconc, data = mat_tbl, fct = L.4(fixed = c(NA,NA,NA,NA), names = c("SLOPE","MIN","MAX","IC50")), logDose = 10)})
  coef_estim <- coef(estimate_param); names(coef_estim) <- c("SLOPE","MIN","MAX","IC50")
  coef_estim["SLOPE"] <- coef_estim["SLOPE"] * -1
  
  coef_estim["IC50"] <- ifelse(coef_estim["MAX"] <= coef_estim["MIN"] | coef_estim["IC50"] > max(mat_tbl$dose, na.rm = TRUE), max(mat_tbl$dose, na.rm = TRUE), coef_estim["IC50"])
  coef_estim["IC50"] <- ifelse(coef_estim["IC50"] < 0, min(mat_tbl$dose, na.rm = TRUE), coef_estim["IC50"])
  coef_estim["IC50"] <- ifelse(coef_estim["IC50"] < 0, mean(mat_tbl$dose, na.rm = TRUE), coef_estim["IC50"])
  coef_estim["IC50"] <- log10(coef_estim["IC50"])
  coef_estim["IC50"] <- ifelse(coef_estim["IC50"] < min(mat_tbl$logconc), max(mat_tbl$logconc), coef_estim["IC50"])
  coef_estim["IC50"] <- ifelse(all(mat_tbl$inhibition < 0), max(mat_tbl$logconc, na.rm = TRUE), coef_estim["IC50"])
  coef_estim["MIN"] <- 0; coef_estim["MAX"] <- max(mat_tbl$inhibition, na.rm = TRUE)
  min_lower <- ifelse(min(mat_tbl$inhibition, na.rm = TRUE) > 0, min(mat_tbl$inhibition, na.rm = TRUE), 0)
  min_lower <- ifelse(min_lower >= 100, 99, min_lower)
  coef_estim["MAX"] <- ifelse(coef_estim["MAX"] > 100, 100, coef_estim["MAX"])
  coef_estim["MAX"] <- ifelse(coef_estim["MAX"] < 0, 100, coef_estim["MAX"])
  max_lower <- ifelse(max(mat_tbl$inhibition, na.rm = TRUE) > 100, coef_estim["MAX"], max(mat_tbl$inhibition, na.rm = TRUE))
  max_lower <- ifelse(max_lower < 0, coef_estim["MAX"], max(mat_tbl$inhibition, na.rm = TRUE))
  max_lower <- ifelse(max_lower < 0, 0, max_lower)
  max_lower <- ifelse(max_lower > 100, 100, max_lower)
  run_avg <- caTools::runmean(mat_tbl$inhibition, 10)
  max_upper <- ifelse(any(run_avg[-nrow(mat_tbl)] > run_avg[nrow(mat_tbl)]), max(mat_tbl$inhibition[run_avg > run_avg[nrow(mat_tbl)]]), coef_estim["MAX"])
  max_upper <- ifelse(any(mat_tbl$inhibition > max_upper), mean(mat_tbl$inhibition[mat_tbl$inhibition > max_upper]) + 5, max_upper)
  max_upper <- ifelse(max_upper < 0, coef_estim["MAX"], max_upper)
  max_upper <- ifelse(max_upper > 100, 100, max_upper)
  max_upper <- ifelse(max_lower > max_upper, coef_estim["MAX"], max_upper)
  mean_inh_last <- mean(tail(mat_tbl$inhibition, 2), na.rm = TRUE)
  if (mean_inh_last < 60) {
    if (mean_inh_last > 25) coef_estim["IC50"] <- mean(mat_tbl$logconc, na.rm = TRUE)
    else if (mean_inh_last < 25) coef_estim["IC50"] <- max(mat_tbl$logconc, na.rm = TRUE)
  }
  if (mean(mat_tbl$inhibition[1:3], na.rm = TRUE) < 5) coef_estim["IC50"] <- max(mat_tbl$logconc, na.rm = TRUE)
  if (unname(coef_estim["MIN"]) == unname(coef_estim["MAX"])) coef_estim["MAX"] <- coef_estim["MAX"] + 0.001
  
  nls_result_ic50_old <- function() {
    tryCatch({
      nls(inhibition ~ MIN + (MAX - MIN) / (1 + (10^(SLOPE * (IC50 - logconc)))), data = mat_tbl, algorithm = "port",
          start = list(SLOPE = 1, MIN = coef_estim["MIN"][[1]], MAX = coef_estim["MAX"][[1]], IC50 = coef_estim["IC50"][[1]]),
          lower = list(SLOPE = 0.1, MIN = 0, MAX = max_lower, IC50 = min(mat_tbl$logconc)),
          upper = list(SLOPE = 2.5, MIN = 0, MAX = max_upper, IC50 = max(mat_tbl$logconc)),
          control = list(warnOnly = TRUE, minFactor = 1/2048))
    }, error = function(e) {
      minpack.lm::nlsLM(inhibition ~ MIN + (MAX - MIN) / (1 + (10^(SLOPE * (IC50 - logconc)))), data = mat_tbl,
                        start = list(SLOPE = 1, MIN = coef_estim["MIN"][[1]], MAX = coef_estim["MAX"][[1]], IC50 = coef_estim["IC50"][[1]]),
                        lower = c(SLOPE = 0.1, MIN = 0, MAX = max_lower, IC50 = min(mat_tbl$logconc)),
                        upper = c(SLOPE = 2.5, MIN = 0, MAX = max_upper, IC50 = max(mat_tbl$logconc)))
    })
  }
  
  nls_result_ic50 <- nls_result_ic50_old()
  nls_result_ic50_2 <- tryCatch({
    nls(inhibition ~ MIN + (MAX - MIN) / (1 + (10^(SLOPE * (IC50 - logconc)))), data = mat_tbl, algorithm = "port",
        start = list(SLOPE = 1, MIN = coef_estim["MIN"][[1]], MAX = coef_estim["MAX"][[1]], IC50 = median(mat_tbl$logconc)),
        lower = list(SLOPE = 0.1, MIN = 0, MAX = max_lower, IC50 = min(mat_tbl$logconc)),
        upper = list(SLOPE = 2.5, MIN = 0, MAX = max_upper, IC50 = max(mat_tbl$logconc)),
        control = list(warnOnly = TRUE, minFactor = 1/2048))
  }, warning = function(w) { nls_result_ic50_old() }, error = function(e) { nls_result_ic50_old() })
  
  nls_result_ic50 <- tryCatch({summary(nls_result_ic50); nls_result_ic50}, error = function(e) {nls_result_ic50_2})
  sumIC50 <- list(summary(nls_result_ic50), summary(nls_result_ic50_2))
  ic50std_resid  <- round(sqrt(sum((sumIC50[[1]]$residuals)^2) / (length(sumIC50[[1]]$residuals) - 1)), 1)
  ic50std_resid2 <- round(sqrt(sum((sumIC50[[2]]$residuals)^2) / (length(sumIC50[[2]]$residuals) - 1)), 1)
  switch_ <- which.min(c(ic50std_resid, ic50std_resid2))
  nls_result_ic50 <- list(nls_result_ic50, nls_result_ic50_2)[[switch_]]
  
  sumIC50 <- summary(nls_result_ic50)
  ic50std_resid <- round(sqrt(sum((sumIC50$residuals)^2) / (length(sumIC50$residuals) - 1)), 1)
  max_signal <- max(mat_tbl$dose, na.rm = TRUE); min_signal <- min(mat_tbl$dose, na.rm = TRUE)
  
  coef_ic50 <- coef(nls_result_ic50)[c("IC50","SLOPE","MAX","MIN")]; coef_ic50["IC50"] <- 10^coef_ic50["IC50"]
  coef_ic50["IC50"] <- ifelse(coef_ic50["SLOPE"] < 0, max_signal, coef_ic50["IC50"])
  coef_ic50["IC50"] <- ifelse(coef_ic50["MAX"] < 0, max_signal, coef_ic50["IC50"])
  coef_ic50["IC50"] <- ifelse(coef_ic50["MAX"] < 10, max_signal, coef_ic50["IC50"])
  coef_ic50["MAX"]  <- ifelse(coef_ic50["MAX"] < 0, 0, coef_ic50["MAX"])
  coef_ic50["IC50"] <- ifelse(all(c(max(mat_tbl$inhibition, na.rm = TRUE), min(mat_tbl$inhibition, na.rm = TRUE)) > 50), min_signal, coef_ic50["IC50"])
  
  x <- seq(min(mat_tbl$logconc), max(mat_tbl$logconc), length = 100)
  yic <- predict(nls_result_ic50, data.frame(logconc = x))
  x.new <- compute.intervals(log10(mat_tbl$dose))
  y.points <- predict(nls_result_ic50, data.frame(logconc = x.new))
  
  mat_tblCp <- mat_tbl[, c("inhibition", "dose")]
  X <- data.table::as.data.table(mat_tblCp)
  mat_tblCp <- as.data.frame(X[, list(inhibition = mean(inhibition)), by = "dose"])
  perInh <- t(matrix(mat_tblCp[, "inhibition"], dimnames = list(paste0("D", 1:nrow(mat_tblCp)))))
  
  coef_tec50 <- coef_ic50
  coef_tec50["IC50"] <- ifelse(coef_tec50["MAX"] > 25, coef_tec50["IC50"], max(mat_tbl$dose, na.rm = TRUE))
  names(coef_tec50) <- c("EC50","SLOPE","MAX","MIN")
  coef_tec50["SLOPE"] <- -1 * coef_tec50["SLOPE"]
  tmp <- coef_tec50["MAX"]; coef_tec50["MAX"] <- 100 - coef_tec50["MIN"]; coef_tec50["MIN"] <- 100 - tmp
  
  dss_score <- round(as.numeric(dss(coef_ic50["IC50"], coef_ic50["SLOPE"], coef_ic50["MAX"], min_signal, max_signal, DSS.type = as.integer(DSS_typ))), 1)
  
  xIC50ABS <- seq(min(mat_tbl$logconc), max(mat_tbl$logconc) * 15, length = 5000)
  yicIC50ABS <- predict(nls_result_ic50, data.frame(logconc = xIC50ABS))
  coef_ic50ABS <- if (all(yicIC50ABS < 50)) Inf else 10^xIC50ABS[which.min(abs(yicIC50ABS - 50))]
  
  if (flag_reverse) {
    dss_score <- -1 * dss_score; yic <- -1 * yic; y.points <- -1 * y.points
    coef_ic50["SLOPE"] <- -coef_ic50["SLOPE"]; coef_ic50["MAX"] <- -coef_ic50["MAX"]
  }
  mat_tbl$inhibition <- inhibition2
  
  list(dss = dss_score, df = mat_tbl, x = x, y = yic, ic50 = coef_ic50["IC50"], doses = x.new,
       sd_model = ic50std_resid, IC50_abs = coef_ic50ABS, y.points = y.points,
       case_debug = case_debug, slope = coef_ic50["SLOPE"], max = coef_ic50["MAX"])
}


# Dose-response curve fitting (effector monoculture)

#' Fit dose-response curve for a control (effector-only) condition
#'
#' Fits the same four-parameter model as the other wrappers, with
#' control-specific logic: if all inhibition values are <= 0, the
#' response is treated as flat (no effector-mediated killing).
#'
#' @param df Data frame with 4 columns: drug name, dose, screen ID,
#'   percent inhibition.
#' @return Named list of fitted parameters (same structure as
#'   fit.screen_CC, without flag_CC or case_debug).
#' @seealso \code{\link{fit.screen_CC}}, \code{\link{fit.screen_mono}}
fit.screen_control <- function(df) {
  
  mat_tbl <- df
  colnames(mat_tbl) <- c("drug", "dose", "screen", "inhibition")
  mat_tbl$logconc <- log10(mat_tbl$dose)
  readoutCTX <- FALSE; DSS_typ <- 2
  product_id <- drug_name <- unique(mat_tbl$drug)
  flag_reverse <- FALSE
  mat_tbl <- mat_tbl[order(mat_tbl$dose), ]
  
  mat_tbl$inhibition <- replace(mat_tbl$inhibition, mat_tbl$inhibition < -100, -100)
  mat_tbl$inhibition <- replace(mat_tbl$inhibition, mat_tbl$inhibition > 100, 100)
  inhibition <- inhibition2 <- mat_tbl$inhibition
  
  ## Control-specific: flatten if all values non-positive
  if (all(inhibition <= 0)) inhibition <- rep(0, length(inhibition))
  if (any(duplicated(inhibition))) inhibition <- seq(from = 0, length.out = length(inhibition), by = 0.01) + inhibition
  mat_tbl$inhibition <- inhibition
  
  estimate_param <- tryCatch(
    {drm(inhibition ~ logconc, data = mat_tbl, fct = LL.4(fixed = c(NA,NA,NA,NA), names = c("SLOPE","MIN","MAX","IC50")), logDose = 10, control = drmc(errorm = FALSE))},
    warning = function(w) {drm(inhibition ~ logconc, data = mat_tbl, fct = L.4(fixed = c(NA,NA,NA,NA), names = c("SLOPE","MIN","MAX","IC50")), logDose = 10)},
    error   = function(e) {drm(inhibition ~ logconc, data = mat_tbl, fct = L.4(fixed = c(NA,NA,NA,NA), names = c("SLOPE","MIN","MAX","IC50")), logDose = 10)})
  coef_estim <- coef(estimate_param); names(coef_estim) <- c("SLOPE","MIN","MAX","IC50")
  coef_estim["SLOPE"] <- coef_estim["SLOPE"] * -1
  
  coef_estim["IC50"] <- ifelse(coef_estim["MAX"] <= coef_estim["MIN"] | coef_estim["IC50"] > max(mat_tbl$dose, na.rm = TRUE), max(mat_tbl$dose, na.rm = TRUE), coef_estim["IC50"])
  coef_estim["IC50"] <- ifelse(coef_estim["IC50"] < 0, min(mat_tbl$dose, na.rm = TRUE), coef_estim["IC50"])
  coef_estim["IC50"] <- ifelse(coef_estim["IC50"] < 0, mean(mat_tbl$dose, na.rm = TRUE), coef_estim["IC50"])
  coef_estim["IC50"] <- log10(coef_estim["IC50"])
  coef_estim["IC50"] <- ifelse(coef_estim["IC50"] < min(mat_tbl$logconc), max(mat_tbl$logconc), coef_estim["IC50"])
  coef_estim["IC50"] <- ifelse(all(mat_tbl$inhibition < 0), max(mat_tbl$logconc, na.rm = TRUE), coef_estim["IC50"])
  coef_estim["MIN"] <- 0; coef_estim["MAX"] <- max(mat_tbl$inhibition, na.rm = TRUE)
  min_lower <- ifelse(min(mat_tbl$inhibition, na.rm = TRUE) > 0, min(mat_tbl$inhibition, na.rm = TRUE), 0)
  min_lower <- ifelse(min_lower >= 100, 99, min_lower)
  coef_estim["MAX"] <- ifelse(coef_estim["MAX"] > 100, 100, coef_estim["MAX"])
  coef_estim["MAX"] <- ifelse(coef_estim["MAX"] < 0, 100, coef_estim["MAX"])
  max_lower <- ifelse(max(mat_tbl$inhibition, na.rm = TRUE) > 100, coef_estim["MAX"], max(mat_tbl$inhibition, na.rm = TRUE))
  max_lower <- ifelse(max_lower < 0, coef_estim["MAX"], max(mat_tbl$inhibition, na.rm = TRUE))
  max_lower <- ifelse(max_lower < 0, 0, max_lower)
  max_lower <- ifelse(max_lower > 100, 100, max_lower)
  run_avg <- caTools::runmean(mat_tbl$inhibition, 10)
  max_upper <- ifelse(any(run_avg[-nrow(mat_tbl)] > run_avg[nrow(mat_tbl)]), max(mat_tbl$inhibition[run_avg > run_avg[nrow(mat_tbl)]]), coef_estim["MAX"])
  max_upper <- ifelse(any(mat_tbl$inhibition > max_upper), mean(mat_tbl$inhibition[mat_tbl$inhibition > max_upper]) + 5, max_upper)
  max_upper <- ifelse(max_upper < 0, coef_estim["MAX"], max_upper)
  max_upper <- ifelse(max_upper > 100, 100, max_upper)
  max_upper <- ifelse(max_lower > max_upper, coef_estim["MAX"], max_upper)
  mean_inh_last <- mean(tail(mat_tbl$inhibition, 2), na.rm = TRUE)
  if (mean_inh_last < 60) {
    if (mean_inh_last > 25) coef_estim["IC50"] <- mean(mat_tbl$logconc, na.rm = TRUE)
    else if (mean_inh_last < 25) coef_estim["IC50"] <- max(mat_tbl$logconc, na.rm = TRUE)
  }
  if (mean(mat_tbl$inhibition[1:3], na.rm = TRUE) < 5) coef_estim["IC50"] <- max(mat_tbl$logconc, na.rm = TRUE)
  if (unname(coef_estim["MIN"]) == unname(coef_estim["MAX"])) coef_estim["MAX"] <- coef_estim["MAX"] + 0.001
  
  nls_result_ic50_old <- function() {
    tryCatch({
      nls(inhibition ~ MIN + (MAX - MIN) / (1 + (10^(SLOPE * (IC50 - logconc)))), data = mat_tbl, algorithm = "port",
          start = list(SLOPE = 1, MIN = coef_estim["MIN"][[1]], MAX = coef_estim["MAX"][[1]], IC50 = coef_estim["IC50"][[1]]),
          lower = list(SLOPE = 0.1, MIN = 0, MAX = max_lower, IC50 = min(mat_tbl$logconc)),
          upper = list(SLOPE = 2.5, MIN = 0, MAX = max_upper, IC50 = max(mat_tbl$logconc)),
          control = list(warnOnly = TRUE, minFactor = 1/2048))
    }, error = function(e) {
      minpack.lm::nlsLM(inhibition ~ MIN + (MAX - MIN) / (1 + (10^(SLOPE * (IC50 - logconc)))), data = mat_tbl,
                        start = list(SLOPE = 1, MIN = coef_estim["MIN"][[1]], MAX = coef_estim["MAX"][[1]], IC50 = coef_estim["IC50"][[1]]),
                        lower = c(SLOPE = 0.1, MIN = 0, MAX = max_lower, IC50 = min(mat_tbl$logconc)),
                        upper = c(SLOPE = 2.5, MIN = 0, MAX = max_upper, IC50 = max(mat_tbl$logconc)))
    })
  }
  
  nls_result_ic50 <- nls_result_ic50_old()
  nls_result_ic50_2 <- tryCatch({
    nls(inhibition ~ MIN + (MAX - MIN) / (1 + (10^(SLOPE * (IC50 - logconc)))), data = mat_tbl, algorithm = "port",
        start = list(SLOPE = 1, MIN = coef_estim["MIN"][[1]], MAX = coef_estim["MAX"][[1]], IC50 = median(mat_tbl$logconc)),
        lower = list(SLOPE = 0.1, MIN = 0, MAX = max_lower, IC50 = min(mat_tbl$logconc)),
        upper = list(SLOPE = 2.5, MIN = 0, MAX = max_upper, IC50 = max(mat_tbl$logconc)),
        control = list(warnOnly = TRUE, minFactor = 1/2048))
  }, warning = function(w) { nls_result_ic50_old() }, error = function(e) { nls_result_ic50_old() })
  
  nls_result_ic50 <- tryCatch({summary(nls_result_ic50); nls_result_ic50}, error = function(e) {nls_result_ic50_2})
  sumIC50 <- list(summary(nls_result_ic50), summary(nls_result_ic50_2))
  ic50std_resid  <- round(sqrt(sum((sumIC50[[1]]$residuals)^2) / (length(sumIC50[[1]]$residuals) - 1)), 1)
  ic50std_resid2 <- round(sqrt(sum((sumIC50[[2]]$residuals)^2) / (length(sumIC50[[2]]$residuals) - 1)), 1)
  switch_ <- which.min(c(ic50std_resid, ic50std_resid2))
  nls_result_ic50 <- list(nls_result_ic50, nls_result_ic50_2)[[switch_]]
  
  sumIC50 <- summary(nls_result_ic50)
  ic50std_resid <- round(sqrt(sum((sumIC50$residuals)^2) / (length(sumIC50$residuals) - 1)), 1)
  max_signal <- max(mat_tbl$dose, na.rm = TRUE); min_signal <- min(mat_tbl$dose, na.rm = TRUE)
  
  coef_ic50 <- coef(nls_result_ic50)[c("IC50","SLOPE","MAX","MIN")]; coef_ic50["IC50"] <- 10^coef_ic50["IC50"]
  coef_ic50["IC50"] <- ifelse(coef_ic50["SLOPE"] < 0, max_signal, coef_ic50["IC50"])
  coef_ic50["IC50"] <- ifelse(coef_ic50["MAX"] < 0, max_signal, coef_ic50["IC50"])
  coef_ic50["IC50"] <- ifelse(coef_ic50["MAX"] < 10, max_signal, coef_ic50["IC50"])
  coef_ic50["MAX"]  <- ifelse(coef_ic50["MAX"] < 0, 0, coef_ic50["MAX"])
  coef_ic50["IC50"] <- ifelse(all(c(max(mat_tbl$inhibition, na.rm = TRUE), min(mat_tbl$inhibition, na.rm = TRUE)) > 50), min_signal, coef_ic50["IC50"])
  
  x <- seq(min(mat_tbl$logconc), max(mat_tbl$logconc), length = 100)
  yic <- predict(nls_result_ic50, data.frame(logconc = x))
  x.new <- compute.intervals(log10(mat_tbl$dose))
  y.points <- predict(nls_result_ic50, data.frame(logconc = x.new))
  
  mat_tblCp <- mat_tbl[, c("inhibition", "dose")]
  X <- data.table::as.data.table(mat_tblCp)
  mat_tblCp <- as.data.frame(X[, list(inhibition = mean(inhibition)), by = "dose"])
  perInh <- t(matrix(mat_tblCp[, "inhibition"], dimnames = list(paste0("D", 1:nrow(mat_tblCp)))))
  
  coef_tec50 <- coef_ic50
  coef_tec50["IC50"] <- ifelse(coef_tec50["MAX"] > 25, coef_tec50["IC50"], max(mat_tbl$dose, na.rm = TRUE))
  names(coef_tec50) <- c("EC50","SLOPE","MAX","MIN")
  coef_tec50["SLOPE"] <- -1 * coef_tec50["SLOPE"]
  tmp <- coef_tec50["MAX"]; coef_tec50["MAX"] <- 100 - coef_tec50["MIN"]; coef_tec50["MIN"] <- 100 - tmp
  
  dss_score <- round(as.numeric(dss(coef_ic50["IC50"], coef_ic50["SLOPE"], coef_ic50["MAX"], min_signal, max_signal, DSS.type = as.integer(DSS_typ))), 1)
  
  xIC50ABS <- seq(min(mat_tbl$logconc), max(mat_tbl$logconc) * 15, length = 5000)
  yicIC50ABS <- predict(nls_result_ic50, data.frame(logconc = xIC50ABS))
  coef_ic50ABS <- if (all(yicIC50ABS < 50)) Inf else 10^xIC50ABS[which.min(abs(yicIC50ABS - 50))]
  
  mat_tbl$inhibition <- inhibition2
  
  list(dss = dss_score, df = mat_tbl, x = x, y = yic, ic50 = coef_ic50["IC50"], doses = x.new,
       sd_model = ic50std_resid, IC50_abs = coef_ic50ABS, y.points = y.points,
       slope = coef_ic50["SLOPE"], max = coef_ic50["MAX"])
}


# Gaussian mixture model and parametric AUC (i.e., nAUC) 

#' Four-component symmetric Gaussian mixture
#'
#' @param x Numeric vector of doses (log10 scale).
#' @param A1,mu1,sigma1 Amplitude, mean, SD for component 1.
#' @param A2,mu2,sigma2 Amplitude, mean, SD for component 2.
#' @param A3,mu3,sigma3 Amplitude, mean, SD for component 3.
#' @param A4,mu4,sigma4 Amplitude, mean, SD for component 4.
#' @return Numeric vector of mixture values at each x.
#' @keywords internal
mixture_sym_gaussian <- function(x, A1, mu1, sigma1, A2, mu2, sigma2,
                                 A3, mu3, sigma3, A4, mu4, sigma4) {
  A1 * exp(-((x - mu1)^2) / (2 * sigma1^2)) +
    A2 * exp(-((x - mu2)^2) / (2 * sigma2^2)) +
    A3 * exp(-((x - mu3)^2) / (2 * sigma3^2)) +
    A4 * exp(-((x - mu4)^2) / (2 * sigma4^2))
}

#' Analytical AUC of a single Gaussian component
#'
#' @param A Numeric. Amplitude.
#' @param mu Numeric. Mean.
#' @param sigma Numeric. Standard deviation.
#' @param lower Numeric. Lower integration bound.
#' @param upper Numeric. Upper integration bound.
#' @return Numeric AUC.
#' @keywords internal
auc_gaussian <- function(A, mu, sigma, lower, upper) {
  lower_cdf <- pnorm(lower, mean = mu, sd = abs(sigma))
  upper_cdf <- pnorm(upper, mean = mu, sd = abs(sigma))
  A * sqrt(2 * pi * sigma^2) * (upper_cdf - lower_cdf)
}



# CES computation

#' Fit Gaussian mixture to co-culture delta curve and compute CES
#'
#' Fits a four-component symmetric Gaussian mixture to the difference
#' curve (co-culture minus mono-culture fitted values) using multi-start
#' gradient-based optimization (nlminb). Falls back to DEoptim if all
#' starts fail.
#' CES additionally requires a minimum of 10% activity to compute 
#' CES-derived curves and results (i.e., assumption of 10% biological noise).
#'
#' CES = (AUC * log10(Peak)) / dose_range
#'
#' @param df_CC A list with elements:
#'   \describe{
#'     \item{x}{Numeric vector. Log10-dose grid.}
#'     \item{y}{Numeric vector. Delta curve values.}
#'   }
#' @return A named list: CES, highest_peak, effective_dose, normalized_AUC,
#'   case, dose_shift, dose_seq, fitted_values.
Fit_Gaussian_CoCulture <- function(df_CC) {
  
  doses <- as.numeric(df_CC$x)
  predicted.points <- as.numeric(df_CC$y)
  dose_shift <- max(0, -min(doses))
  df.drug <- data.frame(x = doses + dose_shift, y = predicted.points)
  x_min <- min(df.drug$x); x_max <- max(df.drug$x); x_range <- x_max - x_min
  
  flag_shift <- FALSE; flag_negative <- FALSE; flag_activity <- FALSE
  
  if (max(abs(df.drug$y)) < 10) {
    return(list(CES = 0, highest_peak = 0, effective_dose = NA_integer_,
                normalized_AUC = 0, case = "Insufficient activity", dose_shift = dose_shift,
                dose_seq = seq(x_min, x_max, length.out = 100) - dose_shift,
                fitted_values = rep(0, 100)))
  }
  
  if (min(df.drug$y) < -10) {
    if (all(df.drug$y < 0) || max(df.drug$y) < 10) {
      flag_negative <- TRUE; df.drug$y <- -1 * df.drug$y; units_shift <- 0; case <- "Negative"
    } else {
      flag_shift <- TRUE; units_shift <- abs(min(df.drug$y)); df.drug$y <- df.drug$y + units_shift; case <- "Shift"
    }
  } else { units_shift <- 0; case <- "Positive" }
  
  x_data <- df.drug$x; y_data <- df.drug$y; y_max <- max(y_data); n_points <- length(x_data)
  
  cost_function <- function(params) {
    y_pred <- mixture_sym_gaussian(x_data, params[1], params[2], params[3], params[4], params[5], params[6],
                                   params[7], params[8], params[9], params[10], params[11], params[12])
    sum((y_data - y_pred)^2)
  }
  
  grad_function <- function(params) {
    G <- matrix(0, nrow = n_points, ncol = 4); y_pred <- numeric(n_points)
    for (i in 1:4) {
      idx <- (i - 1) * 3; A <- params[idx + 1]; mu <- params[idx + 2]; sigma <- params[idx + 3]
      G[, i] <- exp(-((x_data - mu)^2) / (2 * sigma^2)); y_pred <- y_pred + A * G[, i]
    }
    r <- y_data - y_pred; grad <- numeric(12)
    for (i in 1:4) {
      idx <- (i - 1) * 3; A <- params[idx + 1]; mu <- params[idx + 2]; sigma <- params[idx + 3]
      dx <- x_data - mu; g_i <- G[, i]
      grad[idx + 1] <- -2 * sum(r * g_i)
      grad[idx + 2] <- -2 * sum(r * A * g_i * dx / (sigma^2))
      grad[idx + 3] <- -2 * sum(r * A * g_i * (dx^2) / (sigma^3))
    }
    grad
  }
  
  min_sigma <- max(0.05, x_range / 50); sig_narrow <- max(min_sigma * 2, x_range / 10)
  sig_wide <- x_range / 4; sig_full <- x_range / 2
  lower_bounds <- rep(c(0, x_min, min_sigma), 4)
  upper_bounds <- rep(c(y_max, x_max, x_range), 4)
  idx_max <- which.max(y_data); x_peak <- x_data[idx_max]
  
  starts <- list(
    c(y_max, x_peak, sig_narrow, y_max/2, x_min + x_range*0.3, sig_wide,
      y_max/2, x_min + x_range*0.7, sig_wide, y_max/4, mean(x_data), sig_full),
    c(y_max/2, x_min + x_range*0.2, sig_wide, y_max/2, x_min + x_range*0.4, sig_wide,
      y_max/2, x_min + x_range*0.6, sig_wide, y_max/2, x_min + x_range*0.8, sig_wide),
    c(y_max, x_peak, sig_full, y_max*0.1, x_peak, sig_narrow, 0, x_min, min_sigma, 0, x_min, min_sigma)
  )
  starts <- lapply(starts, function(s) pmin(pmax(s, lower_bounds), upper_bounds))
  
  y_energy <- sum(y_data^2); early_stop_tol <- 1e-6 * max(y_energy, 1)
  results <- vector("list", length(starts)); best_obj <- Inf; best_idx <- NA_integer_
  
  for (k in seq_along(starts)) {
    results[[k]] <- tryCatch(
      nlminb(starts[[k]], cost_function, gradient = grad_function,
             lower = lower_bounds, upper = upper_bounds,
             control = list(iter.max = 500, eval.max = 1000)),
      error = function(e) list(objective = Inf))
    obj_k <- results[[k]]$objective
    if (!is.null(obj_k) && is.finite(obj_k) && obj_k < best_obj) { best_obj <- obj_k; best_idx <- k }
    if (best_obj < early_stop_tol) break
  }
  
  if (is.infinite(best_obj)) {
    fit <- DEoptim::DEoptim(cost_function, lower = lower_bounds, upper = upper_bounds,
                            control = DEoptim::DEoptim.control(trace = FALSE, itermax = 500))
    best_params <- fit$optim$bestmem
  } else { best_params <- results[[best_idx]]$par }
  
  dose_seq <- seq(x_min, x_max, length.out = 100)
  fitted_values <- do.call(mixture_sym_gaussian, c(list(dose_seq), as.list(best_params)))
  AUCs <- sapply(1:4, function(i) {
    idx <- (i - 1) * 3 + 1
    auc_gaussian(best_params[idx], best_params[idx + 1], best_params[idx + 2], x_min, x_max)
  })
  normalized_AUC <- sum(AUCs) / x_range
  
  x_sim <- seq(x_min, x_max, length.out = 512)
  max_xy <- do.call(mixture_sym_gaussian, c(list(x_sim), as.list(best_params)))
  i_max <- which.max(max_xy); effective_dose <- 10^(x_sim[i_max] - dose_shift); highest_peak <- max(max_xy)
  
  dose_seq <- dose_seq - dose_shift; fitted_values <- fitted_values - units_shift
  highest_peak <- highest_peak - units_shift; normalized_AUC <- as.numeric(unname(normalized_AUC))
  if (flag_shift) normalized_AUC <- normalized_AUC - units_shift
  
  if (!flag_activity) {
    if (highest_peak < 10) highest_peak <- 10
    CES <- normalized_AUC * log10(highest_peak) / x_range
    CES <- round(CES, digits = 4)
  }
  
  if (flag_negative) {
    normalized_AUC <- -1 * normalized_AUC; highest_peak <- -highest_peak
    fitted_values <- -1 * fitted_values; CES <- -CES
  }
  
  if (case == "Insufficient activity") dose_seq <- seq(x_min, x_max, length.out = 100) - dose_shift
  
  highest_peak <- pmin(pmax(highest_peak, -100), 100)
  highest_peak <- round(highest_peak, digits = 3)
  normalized_AUC <- round(normalized_AUC, digits = 3)
  effective_dose <- round(effective_dose, digits = 3)
  
  list(CES = CES, highest_peak = highest_peak, effective_dose = effective_dose,
       normalized_AUC = normalized_AUC, case = case, dose_shift = dose_shift,
       dose_seq = dose_seq, fitted_values = fitted_values)
}



# Post fitting QC filter for artifacts 


#' Flag drugs for exclusion based on curve-fitting artefacts
#'
#' Two exclusion rules:
#' 1. Both curves decrease (negative slopes) yet CES is positive.
#' 2. Both DSS scores are strongly negative (DSS <= -10 in both arms).
#'
#' @param CES Numeric vector. Co-culture Efficacy Scores.
#' @param slope_cc Numeric vector. Hill slopes from co-culture fits.
#' @param slope_mono Numeric vector. Hill slopes from mono-culture fits.
#' @param DSS_CC Numeric vector. DSS scores from co-culture fits.
#' @param DSS_mono Numeric vector. DSS scores from mono-culture fits.
#' @return Logical vector. TRUE = drug should be excluded.
filter_curves <- function(CES, slope_cc, slope_mono, DSS_CC, DSS_mono) {
  both_down_CES_pos <- (slope_cc < 0 & slope_mono < 0) & (CES > 0)
  both_highly_down <- (DSS_CC <= -10 & DSS_mono <= -10)
  out <- both_down_CES_pos | both_highly_down
  out[is.na(out)] <- FALSE
  out
}




# Run the full CES pipeline for a single compound

#'
#' Takes per-drug data frames for each experimental condition, fits
#' dose-response curves, computes the delta interaction profile, fits
#' the Gaussian mixture model, and returns the CES results along with
#' diagnostic plots.
#'
#' Two modesl are supported:
#' \itemize{
#'   \item \strong{2-condition}: Co-culture and target monoculture only
#'   or Co-culture and Mock in the case of antiviral datasets
#'     (\code{df_ctrl = NULL}). The delta curve is computed as
#'     \code{y_cc - y_mono}.
#'   \item \strong{3-condition}: Co-culture, target monoculture, and
#'     effector monoculture. The \code{scoring_model} argument selects
#'     either the therapeutic delta (Bliss independence) or the
#'     mechanistic delta (survival-fraction filter).
#' }
#'
#' @param df_cc Data frame. Co-culture condition for one drug.
#'   Four columns: drug name, dose, screen ID, percent inhibition.
#' @param df_mono Data frame. Target monoculture condition (same format).
#' @param df_ctrl Data frame or NULL. Effector monoculture condition
#'   (same format). If NULL, 2-condition scoring is used.
#' @param scoring_model Character. Ignored when \code{df_ctrl = NULL}.
#'   One of \code{"therapeutic"} (default) or \code{"mechanistic"}.
#' @param plot Logical. Whether to generate diagnostic plots (default TRUE).
#'
#' @return A named list with three elements:
#'   \describe{
#'     \item{results}{A single-row data frame with columns: Drug, Model,
#'       CES, Peak, AUC, Effective_dose, DSS_CC, DSS_mono, DSS_ctrl,
#'       dDSS, Toxic, flag_fit.}
#'     \item{plot_curves}{A ggplot object showing the fitted dose-response
#'       curves for all conditions (NULL if \code{plot = FALSE}).}
#'     \item{plot_score}{A ggplot object showing the Gaussian mixture fit
#'       to the delta curve (NULL if \code{plot = FALSE}).}
#'   }
#'
#' @examples
#' # 3-condition (therapeutic)
#' out <- run_CES(df_cc, df_mono, df_ctrl, scoring_model = "therapeutic")
#' print(out$results)
#'
#' # 2-condition
#' out <- run_CES(df_cc, df_mono)
#' print(out$results)
#'
#' @seealso \code{\link{fit.screen_CC}}, \code{\link{fit.screen_mono}},
#'   \code{\link{fit.screen_control}}, \code{\link{Fit_Gaussian_CoCulture}}
run_CES <- function(df_cc, df_mono, df_ctrl = NULL, scoring_model = "therapeutic", plot = TRUE) {
  
  three_cond <- !is.null(df_ctrl)
  drug_name  <- as.character(df_cc[1, 1])
  
  if (three_cond) {
    model_label <- scoring_model
  } else {
    model_label <- "2-condition"
  }

  # Curve fitting 
  fit_cc <- fit.screen_CC(df_cc)
  fit_mono <- fit.screen_mono(df_mono, fit_cc$flag_CC)
  
  # Full (unconstrained) fits for slope QC and plotting
  fit_mono_full <- fit.screen_CC(df_mono)
  
  if (three_cond) {
    fit_ctrl <- fit.screen_control(df_ctrl)
    fit_ctrl_full <- fit.screen_CC(df_ctrl)
  }

  # DSS, dDSS, toxicity
  DSS_CC <- fit_cc$dss
  DSS_mono <- fit_mono$dss
  
  if (three_cond) {
    DSS_ctrl <- fit_ctrl$dss
    dDSS <- DSS_CC - DSS_mono - DSS_ctrl
    Toxic <- ifelse(DSS_ctrl >= 10, "Yes", "No")
  } else {
    DSS_ctrl <- NA_real_
    dDSS <- DSS_CC - DSS_mono
    Toxic <- NA_character_
  }
  
  # Delta curve 
  y_cc <- fit_cc$y.points
  y_mono <- fit_mono$y.points
  
  if (three_cond && scoring_model == "therapeutic") {
    y_ctrl <- fit_ctrl$y.points
    delta <- y_cc - (y_mono + y_ctrl - (y_mono * y_ctrl) / 100)
  } else if (three_cond && scoring_model == "mechanistic") {
    y_ctrl <- fit_ctrl$y.points
    NK_fraction <- pmax(0, pmin(y_ctrl, 100)) / 100
    delta <- (y_cc - y_mono) * (1 - NK_fraction)
  } else {
    delta <- y_cc - y_mono
  }
  
  mat_delta <- data.frame(x = fit_cc$doses, y = pmin(pmax(delta, -100), 100))
  
  # Gaussian mixture fit and CES computation
  res <- Fit_Gaussian_CoCulture(mat_delta)
  CES <- res$CES
  Peak <- res$highest_peak
  Effective_dose <- res$effective_dose
  AUC <- res$normalized_AUC
  
  # QC filters
  slope_cc <- fit_cc$slope
  slope_mono <- fit_mono_full$slope
  
  if (filter_curves(CES, slope_cc, slope_mono, DSS_CC, DSS_mono)) {
    CES <- NA; Peak <- NA; Effective_dose <- NA; AUC <- NA
  }
  
  flag_fit <- if (three_cond) {
    any(c(fit_mono$sd_model, fit_cc$sd_model, fit_ctrl$sd_model) >= 30)
  } else {
    any(c(fit_mono$sd_model, fit_cc$sd_model) >= 30)
  }
  
  # Results
  results <- data.frame(
    Drug = drug_name,
    Model = model_label,
    CES = CES,
    Peak = Peak,
    AUC = AUC,
    Effective_dose = Effective_dose,
    DSS_CC  = DSS_CC,
    DSS_mono = DSS_mono,
    DSS_ctrl = DSS_ctrl,
    dDSS = dDSS,
    Toxic = Toxic,
    flag_fit = flag_fit
  )
  
  # Plots
  plot_curves <- NULL
  plot_score  <- NULL
  
  if (plot && !is.na(CES)) {
    
    # Dose-response curves
    plot_curves <- ggplot(fit_mono_full$df, aes(logconc, inhibition)) +
      geom_point(color = "#2166ac", size = 3) +
      geom_line(data = data.frame(x = fit_mono_full$x, y = fit_mono_full$y),
                aes(x, y), color = "#2166ac", linewidth = 1) +
      geom_point(data = fit_cc$df, aes(logconc, inhibition),
                 color = "#d96b8a", size = 3) +
      geom_line(data = data.frame(x = fit_cc$x, y = fit_cc$y),
                aes(x, y), color = "#d96b8a", linewidth = 1) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
      ylim(-100, 100) +
      labs(x = paste0(drug_name, " (nM)"), y = "Cell killing (%)") +
      scale_x_continuous(breaks = fit_mono_full$df$logconc,
                         labels = fit_mono_full$df$dose) +
      theme_classic()
    
    if (three_cond) {
      plot_curves <- plot_curves +
        geom_point(data = fit_ctrl_full$df, aes(logconc, inhibition),
                   color = "#006d2c", size = 3) +
        geom_line(data = data.frame(x = fit_ctrl_full$x, y = fit_ctrl_full$y),
                  aes(x, y), color = "#006d2c", linewidth = 1)
    }
    
    # CES Gaussian fit
    fitted_curve <- data.frame(
      x = res$dose_seq,
      y = pmin(pmax(res$fitted_values, -100), 100)
    )
    
    plot_score <- ggplot(mat_delta, aes(x, y)) +
      geom_area(data = fitted_curve, aes(x, y),
                fill = "#E8DAEF", alpha = 0.5) +
      geom_point(color = "#7a0177", size = 2) +
      geom_line(data = fitted_curve, aes(x, y),
                color = "black", linewidth = 1) +
      geom_segment(x = log10(Effective_dose), xend = log10(Effective_dose),
                   y = 0, yend = Peak,
                   arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
                   color = "black", linewidth = 0.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
      ylim(-100, 100) +
      labs(x = paste0(drug_name, " (nM)"), y = "Efficacy (%)") +
      scale_x_continuous(breaks = fit_mono$df$logconc,
                         labels = fit_mono$df$dose) +
      theme_classic()
  }
  
  list(results = results, plot_curves = plot_curves, plot_score = plot_score)
}





