####################################################################
# Selective scoring of drug effects in multicellular co-culture systems - Webapp auxiliar CES functions
#
# Description:
# R script for the auxiliar CES functions for the Webapp
#
# Author: Diogo Dias
# Date: 2026-04-29
#
####################################################################

####################################################################
############################# CES functions for the Webapp #############################
####################################################################

##### Begin script #####

# Plate-level QC statistical functions
stderr <- compiler::cmpfun(function(x){sqrt(var(x,na.rm=T)/length(na.omit(x)))})
lowsd <- compiler::cmpfun(function(x){return(mean(x, na.rm=T)-stderr(x))})
highsd <- compiler::cmpfun(function(x){return(mean(x, na.rm=T)+stderr(x))})
pop.sd <- compiler::cmpfun(function(x){sqrt(var(x, na.rm=T)*(length(na.omit(x))-1)/length(na.omit(x)))})
ssmd <- compiler::cmpfun(function(x,y){round((mean(x, na.rm=T)-mean(y, na.rm=T))/sqrt(var(x, na.rm=T)+var(y, na.rm=T)), 2)})
zfactor <- compiler::cmpfun(function(x,y){round((1-(3 * (pop.sd(x)+pop.sd(y)))/(abs(mean(x, na.rm=T)-mean(y, na.rm=T)))),2)})
robustzfactor <- compiler::cmpfun(function(x,y){round((1-(3 * (mad(x, na.rm=T)+mad(y, na.rm=T)))/(abs(median(x, na.rm=T)-median(y, na.rm=T)))),2)})


outlier_remove <- compiler::cmpfun(function(x){
  qq <- unname(quantile(x, probs=c(.25, .75), na.rm = T))
  outlier_detector <- 1.5 * IQR(x, na.rm = T)
  x[x < (qq[1] - outlier_detector) | x > (qq[2] + outlier_detector)] <- NA
  x
})  


compute.intervals <- function(x){
  
  list.x <- list()
  
  index1 <- 1
  index2 <- 2
  
  for (i in 1:length(x)){
    
    if(i == length(x)){break}
    
    new_seq <- seq(x[index1], x[index2], length=17)
    new_seq <- new_seq[2:16]
    list.x[[index1]] <- new_seq
    
    index1 <- index1+1
    index2 <- index2+1
  }
  
  final.x <- sort(c(x, unlist(list.x)))
  return(final.x)
  
}


dss <- function(ic50,slope,max,min.conc.tested,max.conc.tested,y=10,DSS.type=2,concn_scale=1e-9){
  
  a=as.numeric(unname(max)); b=as.numeric(unname(slope)); d=0; ic50 = as.numeric(unname(ic50));
  min.conc.tested = as.numeric(unname(min.conc.tested)); max.conc.tested = as.numeric(unname(max.conc.tested));
  Min.Conc<- log10(min.conc.tested*concn_scale); Max.Conc<- max.conc.tested; x2<-log10(Max.Conc*concn_scale)  
  
  if(is.na(ic50)||is.na(b)||is.na(a)||is.na(Min.Conc)||is.na(Max.Conc)){ dss<-NA } else if(isTRUE(ic50>=Max.Conc)){ dss<-0 }
  else if(isTRUE(b==0)){ dss<-0 } else{
    if(a>100){ a<-100  }
    if(isTRUE(b<0)){ b<--b  }
    c<-log10(ic50*concn_scale)
    if(a>y){
      if(y!=0){
        x1<-(c - ((log(a-y)-log(y-d))/(b*log(10))))
        if(isTRUE(x1 < Min.Conc)){x1<-Min.Conc} else if(isTRUE(x1 > x2)){x1<-x2}
      }
      else {x1<-Min.Conc}
      int_y=(((((a-d)*log(1+10^(b*(c-x2))))/(b*log(10)))+a*x2)-((((a-d)*log(1+10^(b*(c-x1))))/(b*log(10)))+a*x1)) - (y*(x2-x1))
      total_area<-(x2-Min.Conc)*(100-y)
      
      if(DSS.type==1){norm_area<-((int_y/total_area)*100)} #DSS1
      if(DSS.type==2){norm_area<-((int_y/total_area)*100)/log10(a)#DSS2 #AUC1
      if(isTRUE(norm_area > 50)){ norm_area <- 0}
      }
      if(DSS.type==3){norm_area<-((int_y/total_area)*100)*(log10(100)/log10(a))*((x2-x1)/(x2-Min.Conc))}#DSS3 #AUC5
      if(isTRUE(norm_area < 0|norm_area > 100)){ dss<-0 } else {
        dss<-round(norm_area,digits=4)}
    } else {dss<-0} 
  } 
  return (dss)
}

fit.screen_CC <- function(df){
  
  mat_tbl <- df
  
  colnames(mat_tbl) <- c("drug", "dose", "screen", "inhibition")
  mat_tbl$logconc <- log10(mat_tbl$dose)
  
  readoutCTX = F
  DSS_typ = 2
  product_id <- drug_name <- unique(mat_tbl$drug)
  
  flag_reverse = !1
  
  mat_tbl <- mat_tbl[order(mat_tbl$dose), ]
  
  ## Compute the no. points that correspond to the 33% of the tail
  n_points <- ceiling(length(mat_tbl$dose)*0.33)
  
  mat_tbl$inhibition <- replace(mat_tbl$inhibition, mat_tbl$inhibition < -100, -100)
  mat_tbl$inhibition <- replace(mat_tbl$inhibition, mat_tbl$inhibition > 100, 100)
  
  inhibition = inhibition2 <- mat_tbl$inhibition
  
  ## If all inhibition values are 0, replace with small noise
  if(all(inhibition == 0)) inhibition <- rep(0, length(inhibition)) 
  if(any(duplicated(inhibition))) inhibition <- seq(from = 0, length.out = length(inhibition), by = 0.01) + inhibition;
  
  mat_tbl$inhibition <- inhibition
  
  ## Ifmedian of the tail from all the raw dose-response data points are negative, swap the values for curve fitting
  ## E.g., c(1, 10, -3, -10, -12) -> c(-1, -10, 3, 10, 12)
  if(sum(tail(aggregate(. ~ dose, mat_tbl, "median")["inhibition"],n_points) < 0) > 1 ){
    flag_reverse = !0;
    mat_tbl$inhibition = -1*mat_tbl$inhibition;
  }
  
  ## LL4 breeeze fit
  estimate_param <- tryCatch({drm(inhibition ~ logconc, data = mat_tbl, fct = LL.4(fixed = c(NA, NA, NA,NA),names = c("SLOPE","MIN","MAX","IC50")),logDose=10,control = drmc(errorm = F))}, 
                             warning=function(w){drm(inhibition ~ logconc, data = mat_tbl, fct = L.4(fixed = c(NA, NA, NA,NA), names = c("SLOPE","MIN","MAX","IC50")),logDose=10)},
                             error=function(e){drm(inhibition ~ logconc, data = mat_tbl, fct = L.4(fixed = c(NA, NA, NA,NA), names = c("SLOPE","MIN","MAX","IC50")),logDose=10)})
  # (extract and name coefficients)
  coef_estim <- coef(estimate_param); names(coef_estim) <- c("SLOPE","MIN","MAX","IC50")
  # see http://www.ncbi.nlm.nih.gov/pmc/articles/PMC4696819/
  coef_estim["SLOPE"] <- coef_estim["SLOPE"]*-1 
  
  # if curve decreases or IC50 is higher than max (i.e. IC50 is "outlier"), set IC50 to max conc.
  coef_estim["IC50"] <- ifelse(coef_estim["MAX"]<=coef_estim["MIN"] | coef_estim["IC50"]>max(mat_tbl$dose,na.rm=T), max(mat_tbl$dose,na.rm=T),coef_estim["IC50"])
  # if IC50 is less than 0 set it to min. conc. and if even min. conc. < 0, then set IC50 to mean of all conc.
  coef_estim["IC50"] <- ifelse(coef_estim["IC50"]<0,min(mat_tbl$dose,na.rm=T),coef_estim["IC50"])
  coef_estim["IC50"] <- ifelse(coef_estim["IC50"]<0,mean(mat_tbl$dose,na.rm=T),coef_estim["IC50"])
  # similar to previous step but now compare log10(IC50) with log(min. conc.).
  coef_estim["IC50"] <- log10(coef_estim["IC50"])
  coef_estim["IC50"] <- ifelse(coef_estim["IC50"]<min(mat_tbl$logconc),max(mat_tbl$logconc),coef_estim["IC50"])
  # if all inhib. < 0 set IC50 to max. log. conc !!!!! not obvious why!
  coef_estim["IC50"] <- ifelse(all(mat_tbl$inhibition<0),max(mat_tbl$logconc,na.rm=T),coef_estim["IC50"])
  #(Trying to fix curves that need outlier kickout)
  coef_estim["MIN"] <- 0; coef_estim["MAX"] <- max(mat_tbl$inhibition,na.rm=T)
  #(Fix off minimums) Find lowest inhibition value. If it is not in (0:100), fix it whether to 0 or 99.  
  min_lower <- ifelse(min(mat_tbl$inhibition,na.rm=T) > 0,min(mat_tbl$inhibition,na.rm=T),0)
  min_lower <- ifelse(min_lower >= 100,99,min_lower)
  #similar to previous step but for MAX
  coef_estim["MAX"] <- ifelse(coef_estim["MAX"]>100,100,coef_estim["MAX"])
  coef_estim["MAX"] <- ifelse(coef_estim["MAX"]<0,100,coef_estim["MAX"])
  #max_lower and max_upper - lower and upper bounds for 'nl2sol' algorithm in nonlinear least-squares
  max_lower <- ifelse(max(mat_tbl$inhibition,na.rm=T)>100,coef_estim["MAX"],max(mat_tbl$inhibition,na.rm=T))
  max_lower <- ifelse(max_lower < 0,coef_estim["MAX"],max(mat_tbl$inhibition,na.rm=T))
  max_lower <- ifelse(max_lower < 0,0,max_lower)
  max_lower <- ifelse(max_lower > 100,100,max_lower)
  #(Fix upper maximum for negative slopes)
  run_avg <- caTools::runmean(mat_tbl$inhibition, 10)
  max_upper <- ifelse(any(run_avg[-nrow(mat_tbl)]>run_avg[nrow(mat_tbl)]),max(mat_tbl$inhibition[run_avg>run_avg[nrow(mat_tbl)]]),coef_estim["MAX"])
  max_upper <- ifelse(any(mat_tbl$inhibition > max_upper),mean(mat_tbl$inhibition[mat_tbl$inhibition > max_upper])+5,max_upper)
  max_upper <- ifelse(max_upper < 0,coef_estim["MAX"],max_upper)
  max_upper <- ifelse(max_upper > 100,100,max_upper) #coef_estim["MAX"]
  max_upper <- ifelse(max_lower > max_upper,coef_estim["MAX"],max_upper)
  # left it as it was, just rewritten a bit (ALEKS). not clear how values 25, 60 and 5 are chosen. 
  mean_inh_last = mean(tail(mat_tbl$inhibition,2),na.rm=T)
  if(mean_inh_last < 60) {
    if(mean_inh_last > 25) coef_estim["IC50"] <- mean(mat_tbl$logconc,na.rm=T)
    else if(mean_inh_last < 25) coef_estim["IC50"] <- max(mat_tbl$logconc,na.rm=T)}
  if(mean(mat_tbl$inhibition[1:3],na.rm=T)<5) coef_estim["IC50"] <- max(mat_tbl$logconc,na.rm=T)
  #add a bit of positive noise to MAX if it is the same as MIN. 
  if(unname(coef_estim["MIN"]) == unname(coef_estim["MAX"])) coef_estim["MAX"] <- coef_estim["MAX"] + 0.001
  
  
  #adaptive nonlinear Least-Squares algorithm NL2SOL to estimate parameters.
  nls_result_ic50_old <- function(){
    tryCatch({
      nls(inhibition ~ MIN + (MAX - MIN)/ (1 + (10^(SLOPE * (IC50 - logconc)))), data=mat_tbl, algorithm="port", 
          start=list(SLOPE=1,MIN=coef_estim["MIN"][[1]],MAX=coef_estim["MAX"][[1]],IC50=coef_estim["IC50"][[1]]),
          lower=list(SLOPE=0.1,MIN=0,MAX=max_lower, IC50=min(mat_tbl$logconc)),
          upper=list(SLOPE=2.5,MIN=0,MAX=max_upper, IC50=max(mat_tbl$logconc)),
          control=list(warnOnly=T,minFactor = 1/2048))
    }, error = function(e) {
      
      # allows higher residual sum-of-squares
      minpack.lm::nlsLM(inhibition ~ MIN + (MAX - MIN)/ (1 + (10^(SLOPE * (IC50 - logconc)))), data=mat_tbl,
                        start=list(SLOPE=1, MIN=coef_estim["MIN"][[1]],MAX=coef_estim["MAX"][[1]],IC50=coef_estim["IC50"][[1]]),
                        lower=c(SLOPE=0.1, MIN=0,MAX=max_lower, IC50=min(mat_tbl$logconc)),
                        upper=c(SLOPE=2.5, MIN=0,MAX=max_upper, IC50=max(mat_tbl$logconc)))
    })
  } 
  
  # IC50 first
  nls_result_ic50 <- nls_result_ic50_old();
  
  # IC50 second
  nls_result_ic50_2 <- tryCatch({
    # allows higher residual sum-of-squares
    nls(inhibition ~ MIN + (MAX - MIN)/ (1 + (10^(SLOPE * (IC50 - logconc)))), data=mat_tbl, algorithm="port", 
        start=list(SLOPE=1,MIN=coef_estim["MIN"][[1]],MAX=coef_estim["MAX"][[1]], IC50=median(mat_tbl$logconc)),
        lower=list(SLOPE=0.1,MIN=0,MAX=max_lower, IC50=min(mat_tbl$logconc)),
        upper=list(SLOPE=2.5,MIN=0,MAX=max_upper, IC50=max(mat_tbl$logconc)),
        control=list(warnOnly=T,minFactor = 1/2048))
  },warning = function(w) {
    nls_result_ic50_old()
  },error = function(e) {
    nls_result_ic50_old()
  })
  
  #element (4, 4) is zero, so the inverse cannot be computed
  nls_result_ic50 = tryCatch({summary(nls_result_ic50); nls_result_ic50},error=function(e){nls_result_ic50_2})
  
  #Calculate the standard error scores
  sumIC50 = list(summary(nls_result_ic50), summary(nls_result_ic50_2))
  
  ic50std_resid <- round(sqrt(sum((sumIC50[[1]]$residuals)^2)/(length(sumIC50[[1]]$residuals)-1)),1);
  ic50std_resid2 <- round(sqrt(sum((sumIC50[[2]]$residuals)^2)/(length(sumIC50[[2]]$residuals)-1)),1);
  
  # continue with the best
  switch_ = which.min(c(ic50std_resid, ic50std_resid2))
  nls_result_ic50 = list(nls_result_ic50, nls_result_ic50_2)[[switch_]]
  
  
  #if SLOPE <= 0.2, decrease IC50, change lower bound for SLOPE to 0.1 and repeat.
  # if(coef(nls_result_ic50)["SLOPE"] <= 0.2)
  #{
  #if(mean_inh_last > 60)
  # coef_estim["IC50"] <- min(mat_tbl$logconc,na.rm=T)
  # nls_result_ic50 <- nls(inhibition ~ MIN + (MAX - MIN)/ (1 + (10^(SLOPE * (IC50 - logconc)))), data=mat_tbl, algorithm="port",
  #  start=list(SLOPE=1, MIN=unname(coef_estim["MIN"]),MAX=unname(coef_estim["MAX"]),IC50=unname(coef_estim["IC50"])),
  #lower=list(SLOPE=0.1,MIN=min_lower,MAX=max_lower,IC50=min(mat_tbl$logconc)),
  #upper=list(SLOPE=2.5, MIN=0,MAX=max_upper, IC50=max(mat_tbl$logconc)),
  #control=list(warnOnly=T,minFactor = 1/2048))
  #}
  
  #Calculate the standard error scores
  sumIC50 = summary(nls_result_ic50); 
  ic50std_Error <- sumIC50$coefficients["IC50","Std. Error"]; #tec50std_Error <- sumTEC50$coefficients["TEC50","Std. Error"]
  ic50std_resid <- round(sqrt(sum((sumIC50$residuals)^2)/(length(sumIC50$residuals)-1)),1);
  max_signal <- max(mat_tbl$dose,na.rm=T); min_signal <- min(mat_tbl$dose,na.rm=T)
  
  #############################  
  #############   Final modification & STD error
  
  #prepare final data and convert IC50 back from log scale (inverse)
  coef_ic50 <- coef(nls_result_ic50)[c("IC50", "SLOPE","MAX","MIN")]; coef_ic50["IC50"] <- 10^coef_ic50["IC50"]
  #(Fix ic50 for curves in wrong direction)
  coef_ic50["IC50"] <- ifelse(coef_ic50["SLOPE"]<0,max_signal,coef_ic50["IC50"])
  #(Fix based on MAX)
  coef_ic50["IC50"] <- ifelse(coef_ic50["MAX"]<0,max_signal,coef_ic50["IC50"])
  coef_ic50["IC50"] <- ifelse(coef_ic50["MAX"]<10,max_signal,coef_ic50["IC50"])
  coef_ic50["MAX"] <- ifelse(coef_ic50["MAX"]<0,0,coef_ic50["MAX"])
  #(Fix over sensitive drugs)
  coef_ic50["IC50"] <- ifelse(all(c(max(mat_tbl$inhibition,na.rm=T),min(mat_tbl$inhibition,na.rm=T))>50),min_signal,coef_ic50["IC50"])
  
  
  # for ploting
  x <- seq(min(mat_tbl$logconc),max(mat_tbl$logconc), length=100)
  yic <- predict(nls_result_ic50, data.frame(logconc=x))
  #Fitted data points at additional doses 
  #y.points <- predict(nls_result_ic50, data.frame(logconc=mat_tbl$logconc))
  
  ## Compute the dose-response values from the curve fitting from the doses between each initial concentration
  
  x.new <- compute.intervals(log10(mat_tbl$dose))
  y.points <- predict(nls_result_ic50, data.frame(logconc=x.new))
  
  ## Average replicates, if applicable
  mat_tblCp <- mat_tbl[, c("inhibition", "dose")]
  cols_ <- colnames(mat_tblCp)[!grepl("inhibition", colnames(mat_tblCp))] # columns which should be equal to average PI
  X <- as.data.table(mat_tblCp)
  mat_tblCp <- as.data.frame(X[,list(inhibition = mean(inhibition)),cols_], stringAsFactors = !1)
  
  
  perInh <- t(matrix(mat_tblCp[,"inhibition"],dimnames=
                       list(paste0(rep("D", length(mat_tblCp[,"inhibition"])), 1:length(mat_tblCp[,"inhibition"])))))
  
  coef_tec50 = coef_ic50; 
  coef_tec50["IC50"] <- ifelse(coef_tec50["MAX"] > 25, coef_tec50["IC50"], max(mat_tbl$dose,na.rm=T))
  if(readoutCTX){
    names(coef_tec50) <- c("TC50","SLOPE","MAX","MIN"); ytec <- yic; perViaTox <- perInh;
  } else{
    names(coef_tec50) <- c("EC50","SLOPE","MAX","MIN");
    coef_tec50["SLOPE"] = -1 * coef_tec50["SLOPE"]; # min - 0, max - 77 in ec50 it is max - 100, min - 23
    tmp = coef_tec50["MAX"]; coef_tec50["MAX"] = 100 - coef_tec50["MIN"]; coef_tec50["MIN"] = 100 - tmp; ytec <- 100 - yic;
    perViaTox <- 100 - perInh;
  }
  
  ## Compute DSS
  dss_score <- round(as.numeric(dss(coef_ic50["IC50"],coef_ic50["SLOPE"],coef_ic50["MAX"],min_signal, max_signal, DSS.type=as.integer(DSS_typ))),1);
  
  ####
  # Absolute IC50
  xIC50ABS <- seq(min(mat_tbl$logconc),max(mat_tbl$logconc)*15, length=5000)
  yicIC50ABS <- predict(nls_result_ic50, data.frame(logconc=xIC50ABS))
  if(all(yicIC50ABS < 50)) coef_ic50ABS= Inf else coef_ic50ABS = 10**xIC50ABS[which.min(abs(yicIC50ABS - 50))]
  ####
  
  ## If we reversed the initial raw dose-response points -> reverse the fitted curve to match the initial data points as well as the DSS score
  if(flag_reverse){
    dss_score = -1*dss_score
    yic = -1*yic
    y.points = -1*y.points
    coef_ic50["SLOPE"] <- -coef_ic50["SLOPE"]
    coef_ic50["MAX"] <- -coef_ic50["MAX"]
    
  }
  
  ## For plotting since inhibition2 has the original values
  mat_tbl$inhibition <- inhibition2
  
  list_dss <- list(dss = dss_score, df = mat_tbl, x = x, y = yic, ic50 = coef_ic50["IC50"], doses = x.new, sd_model = ic50std_resid, 
                   IC50_abs = coef_ic50ABS, y.points = y.points, flag_CC = flag_reverse, slope = coef_ic50["SLOPE"], max = coef_ic50["MAX"])
  
  return(list_dss)
}

fit.screen_mono <- function(df, flag_CC){
  
  mat_tbl <- df
  
  colnames(mat_tbl) <- c("drug", "dose", "screen", "inhibition")
  mat_tbl$logconc <- log10(mat_tbl$dose)
  
  readoutCTX = F
  DSS_typ = 2
  product_id <- drug_name <- unique(mat_tbl$drug)
  
  flag_reverse = !1
  
  mat_tbl <- mat_tbl[order(mat_tbl$dose), ] 
  
  n_points <- ceiling(length(mat_tbl$dose)*0.33)
  
  mat_tbl$inhibition <- replace(mat_tbl$inhibition, mat_tbl$inhibition < -100, -100)
  mat_tbl$inhibition <- replace(mat_tbl$inhibition, mat_tbl$inhibition > 100, 100)
  
  inhibition = inhibition2 <- mat_tbl$inhibition
  
  ## If all inhibition values are 0, replace with small noise
  if(all(inhibition == 0)) inhibition <- rep(0, length(inhibition)) 
  if(any(duplicated(inhibition))) inhibition <- seq(from = 0, length.out = length(inhibition), by = 0.01) + inhibition;
  
  mat_tbl$inhibition <- inhibition
  
  
  if(flag_CC){ #If co-culture curve is negative (i.e., Inhibitors)
    
    case_debug <- "Neg"
    
    if(sum(tail(aggregate(. ~ dose, mat_tbl, "median")["inhibition"],n_points) > 0) > 1 ){ # If co-culture < 0 and inhibition of target only is increasing: fix curve at 0
      
      inhibition <- rep(0, length(inhibition)) 
      if(any(duplicated(inhibition))) inhibition <- seq(from = 0, length.out = length(inhibition), by = 0.01) + inhibition
      mat_tbl$inhibition <- inhibition
      case_debug <- "Neg_fix"
      
    } 
  } 
  
  if(!flag_CC){ #If co-culture curve is positive (i.e., Enhancers)
    
    case_debug <- "Pos"
    
    if(sum(tail(aggregate(. ~ dose, mat_tbl, "median")["inhibition"],n_points) < 0) > 1 ){ # If co-culture > 0 and inhibition of target only is increasing: fix curve at 0
      
      inhibition <- rep(0, length(inhibition)) 
      if(any(duplicated(inhibition))) inhibition <- seq(from = 0, length.out = length(inhibition), by = 0.01) + inhibition
      mat_tbl$inhibition <- inhibition
      case_debug <- "Pos_fix"
    } 
    
  } 
  
  
  if(sum(tail(aggregate(. ~ dose, mat_tbl, "median")["inhibition"],n_points) < 0) > 1 ){
    flag_reverse = !0
    mat_tbl$inhibition = -1*mat_tbl$inhibition
    
    case_debug <- "Reverse"
    
  }
  
  
  estimate_param <- tryCatch({drm(inhibition ~ logconc, data = mat_tbl, fct = LL.4(fixed = c(NA, NA, NA,NA),names = c("SLOPE","MIN","MAX","IC50")),logDose=10,control = drmc(errorm = F))}, 
                             warning=function(w){drm(inhibition ~ logconc, data = mat_tbl, fct = L.4(fixed = c(NA, NA, NA,NA), names = c("SLOPE","MIN","MAX","IC50")),logDose=10)},
                             error=function(e){drm(inhibition ~ logconc, data = mat_tbl, fct = L.4(fixed = c(NA, NA, NA,NA), names = c("SLOPE","MIN","MAX","IC50")),logDose=10)})
  # (extract and name coefficients)
  coef_estim <- coef(estimate_param); names(coef_estim) <- c("SLOPE","MIN","MAX","IC50")
  # see http://www.ncbi.nlm.nih.gov/pmc/articles/PMC4696819/
  coef_estim["SLOPE"] <- coef_estim["SLOPE"]*-1 
  
  # if curve decreases or IC50 is higher than max (i.e. IC50 is "outlier"), set IC50 to max conc.
  coef_estim["IC50"] <- ifelse(coef_estim["MAX"]<=coef_estim["MIN"] | coef_estim["IC50"]>max(mat_tbl$dose,na.rm=T), max(mat_tbl$dose,na.rm=T),coef_estim["IC50"])
  # if IC50 is less than 0 set it to min. conc. and if even min. conc. < 0, then set IC50 to mean of all conc.
  coef_estim["IC50"] <- ifelse(coef_estim["IC50"]<0,min(mat_tbl$dose,na.rm=T),coef_estim["IC50"])
  coef_estim["IC50"] <- ifelse(coef_estim["IC50"]<0,mean(mat_tbl$dose,na.rm=T),coef_estim["IC50"])
  # similar to previous step but now compare log10(IC50) with log(min. conc.).
  coef_estim["IC50"] <- log10(coef_estim["IC50"])
  coef_estim["IC50"] <- ifelse(coef_estim["IC50"]<min(mat_tbl$logconc),max(mat_tbl$logconc),coef_estim["IC50"])
  # if all inhib. < 0 set IC50 to max. log. conc !!!!! not obvious why!
  coef_estim["IC50"] <- ifelse(all(mat_tbl$inhibition<0),max(mat_tbl$logconc,na.rm=T),coef_estim["IC50"])
  #(Trying to fix curves that need outlier kickout)
  coef_estim["MIN"] <- 0; coef_estim["MAX"] <- max(mat_tbl$inhibition,na.rm=T)
  #(Fix off minimums) Find lowest inhibition value. If it is not in (0:100), fix it whether to 0 or 99.  
  min_lower <- ifelse(min(mat_tbl$inhibition,na.rm=T) > 0,min(mat_tbl$inhibition,na.rm=T),0)
  min_lower <- ifelse(min_lower >= 100,99,min_lower)
  #similar to previous step but for MAX
  coef_estim["MAX"] <- ifelse(coef_estim["MAX"]>100,100,coef_estim["MAX"])
  coef_estim["MAX"] <- ifelse(coef_estim["MAX"]<0,100,coef_estim["MAX"])
  #max_lower and max_upper - lower and upper bounds for 'nl2sol' algorithm in nonlinear least-squares
  max_lower <- ifelse(max(mat_tbl$inhibition,na.rm=T)>100,coef_estim["MAX"],max(mat_tbl$inhibition,na.rm=T))
  max_lower <- ifelse(max_lower < 0,coef_estim["MAX"],max(mat_tbl$inhibition,na.rm=T))
  max_lower <- ifelse(max_lower < 0,0,max_lower)
  max_lower <- ifelse(max_lower > 100,100,max_lower)
  #(Fix upper maximum for negative slopes)
  run_avg <- caTools::runmean(mat_tbl$inhibition, 10)
  max_upper <- ifelse(any(run_avg[-nrow(mat_tbl)]>run_avg[nrow(mat_tbl)]),max(mat_tbl$inhibition[run_avg>run_avg[nrow(mat_tbl)]]),coef_estim["MAX"])
  max_upper <- ifelse(any(mat_tbl$inhibition > max_upper),mean(mat_tbl$inhibition[mat_tbl$inhibition > max_upper])+5,max_upper)
  max_upper <- ifelse(max_upper < 0,coef_estim["MAX"],max_upper)
  max_upper <- ifelse(max_upper > 100,100,max_upper) #coef_estim["MAX"]
  max_upper <- ifelse(max_lower > max_upper,coef_estim["MAX"],max_upper)
  # left it as it was, just rewritten a bit (ALEKS). not clear how values 25, 60 and 5 are chosen. 
  mean_inh_last = mean(tail(mat_tbl$inhibition,2),na.rm=T)
  if(mean_inh_last < 60) {
    if(mean_inh_last > 25) coef_estim["IC50"] <- mean(mat_tbl$logconc,na.rm=T)
    else if(mean_inh_last < 25) coef_estim["IC50"] <- max(mat_tbl$logconc,na.rm=T)}
  if(mean(mat_tbl$inhibition[1:3],na.rm=T)<5) coef_estim["IC50"] <- max(mat_tbl$logconc,na.rm=T)
  #add a bit of positive noise to MAX if it is the same as MIN. 
  if(unname(coef_estim["MIN"]) == unname(coef_estim["MAX"])) coef_estim["MAX"] <- coef_estim["MAX"] + 0.001
  
  
  #adaptive nonlinear Least-Squares algorithm NL2SOL to estimate parameters.
  nls_result_ic50_old <- function(){
    tryCatch({
      nls(inhibition ~ MIN + (MAX - MIN)/ (1 + (10^(SLOPE * (IC50 - logconc)))), data=mat_tbl, algorithm="port", 
          start=list(SLOPE=1,MIN=coef_estim["MIN"][[1]],MAX=coef_estim["MAX"][[1]],IC50=coef_estim["IC50"][[1]]),
          lower=list(SLOPE=0.1,MIN=0,MAX=max_lower, IC50=min(mat_tbl$logconc)),
          upper=list(SLOPE=2.5,MIN=0,MAX=max_upper, IC50=max(mat_tbl$logconc)),
          control=list(warnOnly=T,minFactor = 1/2048))
    }, error = function(e) {
      
      # allows higher residual sum-of-squares
      minpack.lm::nlsLM(inhibition ~ MIN + (MAX - MIN)/ (1 + (10^(SLOPE * (IC50 - logconc)))), data=mat_tbl,
                        start=list(SLOPE=1, MIN=coef_estim["MIN"][[1]],MAX=coef_estim["MAX"][[1]],IC50=coef_estim["IC50"][[1]]),
                        lower=c(SLOPE=0.1, MIN=0,MAX=max_lower, IC50=min(mat_tbl$logconc)),
                        upper=c(SLOPE=2.5, MIN=0,MAX=max_upper, IC50=max(mat_tbl$logconc)))
    })
  } 
  
  # IC50 first
  nls_result_ic50 <- nls_result_ic50_old();
  
  # IC50 second
  nls_result_ic50_2 <- tryCatch({
    # allows higher residual sum-of-squares
    nls(inhibition ~ MIN + (MAX - MIN)/ (1 + (10^(SLOPE * (IC50 - logconc)))), data=mat_tbl, algorithm="port", 
        start=list(SLOPE=1,MIN=coef_estim["MIN"][[1]],MAX=coef_estim["MAX"][[1]], IC50=median(mat_tbl$logconc)),
        lower=list(SLOPE=0.1,MIN=0,MAX=max_lower, IC50=min(mat_tbl$logconc)),
        upper=list(SLOPE=2.5,MIN=0,MAX=max_upper, IC50=max(mat_tbl$logconc)),
        control=list(warnOnly=T,minFactor = 1/2048))
  },warning = function(w) {
    nls_result_ic50_old()
  },error = function(e) {
    nls_result_ic50_old()
  })
  
  #element (4, 4) is zero, so the inverse cannot be computed
  nls_result_ic50 = tryCatch({summary(nls_result_ic50); nls_result_ic50},error=function(e){nls_result_ic50_2})
  
  #Calculate the standard error scores
  sumIC50 = list(summary(nls_result_ic50), summary(nls_result_ic50_2))
  
  ic50std_resid <- round(sqrt(sum((sumIC50[[1]]$residuals)^2)/(length(sumIC50[[1]]$residuals)-1)),1);
  ic50std_resid2 <- round(sqrt(sum((sumIC50[[2]]$residuals)^2)/(length(sumIC50[[2]]$residuals)-1)),1);
  
  # continue with the best
  switch_ = which.min(c(ic50std_resid, ic50std_resid2))
  nls_result_ic50 = list(nls_result_ic50, nls_result_ic50_2)[[switch_]]
  
  
  #if SLOPE <= 0.2, decrease IC50, change lower bound for SLOPE to 0.1 and repeat.
  #if(coef(nls_result_ic50)["SLOPE"] <= 0.2)
  #{
  # if(mean_inh_last > 60)
  # coef_estim["IC50"] <- min(mat_tbl$logconc,na.rm=T)
  #nls_result_ic50 <- nls(inhibition ~ MIN + (MAX - MIN)/ (1 + (10^(SLOPE * (IC50 - logconc)))), data=mat_tbl, algorithm="port",
  #  start=list(SLOPE=1, MIN=unname(coef_estim["MIN"]),MAX=unname(coef_estim["MAX"]),IC50=unname(coef_estim["IC50"])),
  #lower=list(SLOPE=0.1,MIN=min_lower,MAX=max_lower,IC50=min(mat_tbl$logconc)),
  # upper=list(SLOPE=2.5, MIN=0,MAX=max_upper, IC50=max(mat_tbl$logconc)),
  #control=list(warnOnly=T,minFactor = 1/2048))
  #}
  
  #Calculate the standard error scores
  sumIC50 = summary(nls_result_ic50); 
  ic50std_Error <- sumIC50$coefficients["IC50","Std. Error"]; #tec50std_Error <- sumTEC50$coefficients["TEC50","Std. Error"]
  ic50std_resid <- round(sqrt(sum((sumIC50$residuals)^2)/(length(sumIC50$residuals)-1)),1);
  max_signal <- max(mat_tbl$dose,na.rm=T); min_signal <- min(mat_tbl$dose,na.rm=T)
  
  #############################  
  #############   Final modification & STD error
  
  #prepare final data and convert IC50 back from log scale (inverse)
  coef_ic50 <- coef(nls_result_ic50)[c("IC50", "SLOPE","MAX","MIN")]; coef_ic50["IC50"] <- 10^coef_ic50["IC50"]
  #(Fix ic50 for curves in wrong direction)
  coef_ic50["IC50"] <- ifelse(coef_ic50["SLOPE"]<0,max_signal,coef_ic50["IC50"])
  #(Fix based on MAX)
  coef_ic50["IC50"] <- ifelse(coef_ic50["MAX"]<0,max_signal,coef_ic50["IC50"])
  coef_ic50["IC50"] <- ifelse(coef_ic50["MAX"]<10,max_signal,coef_ic50["IC50"])
  coef_ic50["MAX"] <- ifelse(coef_ic50["MAX"]<0,0,coef_ic50["MAX"])
  #(Fix over sensitive drugs)
  coef_ic50["IC50"] <- ifelse(all(c(max(mat_tbl$inhibition,na.rm=T),min(mat_tbl$inhibition,na.rm=T))>50),min_signal,coef_ic50["IC50"])
  
  
  # for ploting
  x <- seq(min(mat_tbl$logconc),max(mat_tbl$logconc), length=100)
  yic <- predict(nls_result_ic50, data.frame(logconc=x))
  
  #Fitted data points at additional doses 
  x.new <- compute.intervals(log10(mat_tbl$dose))
  y.points <- predict(nls_result_ic50, data.frame(logconc=x.new))
  
  ##average replicates
  mat_tblCp <- mat_tbl[, c("inhibition", "dose")]
  cols_ <- colnames(mat_tblCp)[!grepl("inhibition", colnames(mat_tblCp))] # columns which should be equal to average PI
  X <- as.data.table(mat_tblCp)
  mat_tblCp <- as.data.frame(X[,list(inhibition = mean(inhibition)),cols_], stringAsFactors = !1)
  
  
  perInh <- t(matrix(mat_tblCp[,"inhibition"],dimnames=
                       list(paste0(rep("D", length(mat_tblCp[,"inhibition"])), 1:length(mat_tblCp[,"inhibition"])))))
  
  coef_tec50 = coef_ic50; 
  coef_tec50["IC50"] <- ifelse(coef_tec50["MAX"] > 25, coef_tec50["IC50"], max(mat_tbl$dose,na.rm=T))
  if(readoutCTX){
    names(coef_tec50) <- c("TC50","SLOPE","MAX","MIN"); ytec <- yic; perViaTox <- perInh;
  } else{
    names(coef_tec50) <- c("EC50","SLOPE","MAX","MIN");
    coef_tec50["SLOPE"] = -1 * coef_tec50["SLOPE"]; # min - 0, max - 77 in ec50 it is max - 100, min - 23
    tmp = coef_tec50["MAX"]; coef_tec50["MAX"] = 100 - coef_tec50["MIN"]; coef_tec50["MIN"] = 100 - tmp; ytec <- 100 - yic;
    perViaTox <- 100 - perInh;
  }
  
  dss_score <- round(as.numeric(dss(coef_ic50["IC50"],coef_ic50["SLOPE"],coef_ic50["MAX"],min_signal, max_signal, DSS.type=as.integer(DSS_typ))),1);
  
  ####
  # Absolute IC50
  xIC50ABS <- seq(min(mat_tbl$logconc),max(mat_tbl$logconc)*15, length=5000)
  yicIC50ABS <- predict(nls_result_ic50, data.frame(logconc=xIC50ABS))
  if(all(yicIC50ABS < 50)) coef_ic50ABS= Inf else coef_ic50ABS = 10**xIC50ABS[which.min(abs(yicIC50ABS - 50))]
  ####
  
  
  if(flag_reverse){
    dss_score = -1*dss_score
    yic = -1*yic
    y.points = -1*y.points
    coef_ic50["SLOPE"] <- -coef_ic50["SLOPE"]
    coef_ic50["MAX"] <- -coef_ic50["MAX"]
    
  }
  
  mat_tbl$inhibition <- inhibition2
  
  list_dss <- list(dss = dss_score, df = mat_tbl, x = x, y = yic, ic50 = coef_ic50["IC50"], doses = x.new, sd_model = ic50std_resid, 
                   IC50_abs = coef_ic50ABS, y.points = y.points, case_debug = case_debug, slope = coef_ic50["SLOPE"], max = coef_ic50["MAX"])
  
  return(list_dss)
}

fit.screen_control <- function(df){
  
  mat_tbl <- df
  
  colnames(mat_tbl) <- c("drug", "dose", "screen", "inhibition")
  mat_tbl$logconc <- log10(mat_tbl$dose)
  
  readoutCTX = F
  DSS_typ = 2
  product_id <- drug_name <- unique(mat_tbl$drug)
  
  flag_reverse = !1
  
  mat_tbl <- mat_tbl[order(mat_tbl$dose), ]
  
  mat_tbl$inhibition <- replace(mat_tbl$inhibition, mat_tbl$inhibition < -100, -100)
  mat_tbl$inhibition <- replace(mat_tbl$inhibition, mat_tbl$inhibition > 100, 100)
  
  inhibition = inhibition2 <- mat_tbl$inhibition
  
  ## If all inhibition values are 0, replace with small noise
  if(all(inhibition <= 0)) inhibition <- rep(0, length(inhibition)) 
  if(any(duplicated(inhibition))) inhibition <- seq(from = 0, length.out = length(inhibition), by = 0.01) + inhibition;
  
  mat_tbl$inhibition <- inhibition
  
  estimate_param <- tryCatch({drm(inhibition ~ logconc, data = mat_tbl, fct = LL.4(fixed = c(NA, NA, NA,NA),names = c("SLOPE","MIN","MAX","IC50")),logDose=10,control = drmc(errorm = F))}, 
                             warning=function(w){drm(inhibition ~ logconc, data = mat_tbl, fct = L.4(fixed = c(NA, NA, NA,NA), names = c("SLOPE","MIN","MAX","IC50")),logDose=10)},
                             error=function(e){drm(inhibition ~ logconc, data = mat_tbl, fct = L.4(fixed = c(NA, NA, NA,NA), names = c("SLOPE","MIN","MAX","IC50")),logDose=10)})
  # (extract and name coefficients)
  coef_estim <- coef(estimate_param); names(coef_estim) <- c("SLOPE","MIN","MAX","IC50")
  # see http://www.ncbi.nlm.nih.gov/pmc/articles/PMC4696819/
  coef_estim["SLOPE"] <- coef_estim["SLOPE"]*-1 
  
  # if curve decreases or IC50 is higher than max (i.e. IC50 is "outlier"), set IC50 to max conc.
  coef_estim["IC50"] <- ifelse(coef_estim["MAX"]<=coef_estim["MIN"] | coef_estim["IC50"]>max(mat_tbl$dose,na.rm=T), max(mat_tbl$dose,na.rm=T),coef_estim["IC50"])
  # if IC50 is less than 0 set it to min. conc. and if even min. conc. < 0, then set IC50 to mean of all conc.
  coef_estim["IC50"] <- ifelse(coef_estim["IC50"]<0,min(mat_tbl$dose,na.rm=T),coef_estim["IC50"])
  coef_estim["IC50"] <- ifelse(coef_estim["IC50"]<0,mean(mat_tbl$dose,na.rm=T),coef_estim["IC50"])
  # similar to previous step but now compare log10(IC50) with log(min. conc.).
  coef_estim["IC50"] <- log10(coef_estim["IC50"])
  coef_estim["IC50"] <- ifelse(coef_estim["IC50"]<min(mat_tbl$logconc),max(mat_tbl$logconc),coef_estim["IC50"])
  # if all inhib. < 0 set IC50 to max. log. conc !!!!! not obvious why!
  coef_estim["IC50"] <- ifelse(all(mat_tbl$inhibition<0),max(mat_tbl$logconc,na.rm=T),coef_estim["IC50"])
  #(Trying to fix curves that need outlier kickout)
  coef_estim["MIN"] <- 0; coef_estim["MAX"] <- max(mat_tbl$inhibition,na.rm=T)
  #(Fix off minimums) Find lowest inhibition value. If it is not in (0:100), fix it whether to 0 or 99.  
  min_lower <- ifelse(min(mat_tbl$inhibition,na.rm=T) > 0,min(mat_tbl$inhibition,na.rm=T),0)
  min_lower <- ifelse(min_lower >= 100,99,min_lower)
  #similar to previous step but for MAX
  coef_estim["MAX"] <- ifelse(coef_estim["MAX"]>100,100,coef_estim["MAX"])
  coef_estim["MAX"] <- ifelse(coef_estim["MAX"]<0,100,coef_estim["MAX"])
  #max_lower and max_upper - lower and upper bounds for 'nl2sol' algorithm in nonlinear least-squares
  max_lower <- ifelse(max(mat_tbl$inhibition,na.rm=T)>100,coef_estim["MAX"],max(mat_tbl$inhibition,na.rm=T))
  max_lower <- ifelse(max_lower < 0,coef_estim["MAX"],max(mat_tbl$inhibition,na.rm=T))
  max_lower <- ifelse(max_lower < 0,0,max_lower)
  max_lower <- ifelse(max_lower > 100,100,max_lower)
  #(Fix upper maximum for negative slopes)
  run_avg <- caTools::runmean(mat_tbl$inhibition, 10)
  max_upper <- ifelse(any(run_avg[-nrow(mat_tbl)]>run_avg[nrow(mat_tbl)]),max(mat_tbl$inhibition[run_avg>run_avg[nrow(mat_tbl)]]),coef_estim["MAX"])
  max_upper <- ifelse(any(mat_tbl$inhibition > max_upper),mean(mat_tbl$inhibition[mat_tbl$inhibition > max_upper])+5,max_upper)
  max_upper <- ifelse(max_upper < 0,coef_estim["MAX"],max_upper)
  max_upper <- ifelse(max_upper > 100,100,max_upper) #coef_estim["MAX"]
  max_upper <- ifelse(max_lower > max_upper,coef_estim["MAX"],max_upper)
  # left it as it was, just rewritten a bit (ALEKS). not clear how values 25, 60 and 5 are chosen. 
  mean_inh_last = mean(tail(mat_tbl$inhibition,2),na.rm=T)
  if(mean_inh_last < 60) {
    if(mean_inh_last > 25) coef_estim["IC50"] <- mean(mat_tbl$logconc,na.rm=T)
    else if(mean_inh_last < 25) coef_estim["IC50"] <- max(mat_tbl$logconc,na.rm=T)}
  if(mean(mat_tbl$inhibition[1:3],na.rm=T)<5) coef_estim["IC50"] <- max(mat_tbl$logconc,na.rm=T)
  #add a bit of positive noise to MAX if it is the same as MIN. 
  if(unname(coef_estim["MIN"]) == unname(coef_estim["MAX"])) coef_estim["MAX"] <- coef_estim["MAX"] + 0.001
  
  
  #adaptive nonlinear Least-Squares algorithm NL2SOL to estimate parameters.
  nls_result_ic50_old <- function(){
    tryCatch({
      nls(inhibition ~ MIN + (MAX - MIN)/ (1 + (10^(SLOPE * (IC50 - logconc)))), data=mat_tbl, algorithm="port", 
          start=list(SLOPE=1,MIN=coef_estim["MIN"][[1]],MAX=coef_estim["MAX"][[1]],IC50=coef_estim["IC50"][[1]]),
          lower=list(SLOPE=0.1,MIN=0,MAX=max_lower, IC50=min(mat_tbl$logconc)),
          upper=list(SLOPE=2.5,MIN=0,MAX=max_upper, IC50=max(mat_tbl$logconc)),
          control=list(warnOnly=T,minFactor = 1/2048))
    }, error = function(e) {
      
      # allows higher residual sum-of-squares
      minpack.lm::nlsLM(inhibition ~ MIN + (MAX - MIN)/ (1 + (10^(SLOPE * (IC50 - logconc)))), data=mat_tbl,
                        start=list(SLOPE=1, MIN=coef_estim["MIN"][[1]],MAX=coef_estim["MAX"][[1]],IC50=coef_estim["IC50"][[1]]),
                        lower=c(SLOPE=0.1, MIN=0,MAX=max_lower, IC50=min(mat_tbl$logconc)),
                        upper=c(SLOPE=2.5, MIN=0,MAX=max_upper, IC50=max(mat_tbl$logconc)))
    })
  } 
  
  # IC50 first
  nls_result_ic50 <- nls_result_ic50_old();
  
  # IC50 second
  nls_result_ic50_2 <- tryCatch({
    # allows higher residual sum-of-squares
    nls(inhibition ~ MIN + (MAX - MIN)/ (1 + (10^(SLOPE * (IC50 - logconc)))), data=mat_tbl, algorithm="port", 
        start=list(SLOPE=1,MIN=coef_estim["MIN"][[1]],MAX=coef_estim["MAX"][[1]], IC50=median(mat_tbl$logconc)),
        lower=list(SLOPE=0.1,MIN=0,MAX=max_lower, IC50=min(mat_tbl$logconc)),
        upper=list(SLOPE=2.5,MIN=0,MAX=max_upper, IC50=max(mat_tbl$logconc)),
        control=list(warnOnly=T,minFactor = 1/2048))
  },warning = function(w) {
    nls_result_ic50_old()
  },error = function(e) {
    nls_result_ic50_old()
  })
  
  #element (4, 4) is zero, so the inverse cannot be computed
  nls_result_ic50 = tryCatch({summary(nls_result_ic50); nls_result_ic50},error=function(e){nls_result_ic50_2})
  
  #Calculate the standard error scores
  sumIC50 = list(summary(nls_result_ic50), summary(nls_result_ic50_2))
  
  ic50std_resid <- round(sqrt(sum((sumIC50[[1]]$residuals)^2)/(length(sumIC50[[1]]$residuals)-1)),1);
  ic50std_resid2 <- round(sqrt(sum((sumIC50[[2]]$residuals)^2)/(length(sumIC50[[2]]$residuals)-1)),1);
  
  # continue with the best
  switch_ = which.min(c(ic50std_resid, ic50std_resid2))
  nls_result_ic50 = list(nls_result_ic50, nls_result_ic50_2)[[switch_]]
  
  #Calculate the standard error scores
  sumIC50 = summary(nls_result_ic50); 
  ic50std_Error <- sumIC50$coefficients["IC50","Std. Error"]; #tec50std_Error <- sumTEC50$coefficients["TEC50","Std. Error"]
  ic50std_resid <- round(sqrt(sum((sumIC50$residuals)^2)/(length(sumIC50$residuals)-1)),1);
  max_signal <- max(mat_tbl$dose,na.rm=T); min_signal <- min(mat_tbl$dose,na.rm=T)
  
  #############################  
  #############   Final modification & STD error
  
  #prepare final data and convert IC50 back from log scale (inverse)
  coef_ic50 <- coef(nls_result_ic50)[c("IC50", "SLOPE","MAX","MIN")]; coef_ic50["IC50"] <- 10^coef_ic50["IC50"]
  #(Fix ic50 for curves in wrong direction)
  coef_ic50["IC50"] <- ifelse(coef_ic50["SLOPE"]<0,max_signal,coef_ic50["IC50"])
  #(Fix based on MAX)
  coef_ic50["IC50"] <- ifelse(coef_ic50["MAX"]<0,max_signal,coef_ic50["IC50"])
  coef_ic50["IC50"] <- ifelse(coef_ic50["MAX"]<10,max_signal,coef_ic50["IC50"])
  coef_ic50["MAX"] <- ifelse(coef_ic50["MAX"]<0,0,coef_ic50["MAX"])
  #(Fix over sensitive drugs)
  coef_ic50["IC50"] <- ifelse(all(c(max(mat_tbl$inhibition,na.rm=T),min(mat_tbl$inhibition,na.rm=T))>50),min_signal,coef_ic50["IC50"])
  
  
  # for ploting
  x <- seq(min(mat_tbl$logconc),max(mat_tbl$logconc), length=100)
  yic <- predict(nls_result_ic50, data.frame(logconc=x))
  
  #Fitted data points at additional doses 
  x.new <- compute.intervals(log10(mat_tbl$dose))
  y.points <- predict(nls_result_ic50, data.frame(logconc=x.new))
  
  ##average replicates
  mat_tblCp <- mat_tbl[, c("inhibition", "dose")]
  cols_ <- colnames(mat_tblCp)[!grepl("inhibition", colnames(mat_tblCp))] # columns which should be equal to average PI
  X <- as.data.table(mat_tblCp)
  mat_tblCp <- as.data.frame(X[,list(inhibition = mean(inhibition)),cols_], stringAsFactors = !1)
  
  
  perInh <- t(matrix(mat_tblCp[,"inhibition"],dimnames=
                       list(paste0(rep("D", length(mat_tblCp[,"inhibition"])), 1:length(mat_tblCp[,"inhibition"])))))
  
  coef_tec50 = coef_ic50; 
  coef_tec50["IC50"] <- ifelse(coef_tec50["MAX"] > 25, coef_tec50["IC50"], max(mat_tbl$dose,na.rm=T))
  if(readoutCTX){
    names(coef_tec50) <- c("TC50","SLOPE","MAX","MIN"); ytec <- yic; perViaTox <- perInh;
  } else{
    names(coef_tec50) <- c("EC50","SLOPE","MAX","MIN");
    coef_tec50["SLOPE"] = -1 * coef_tec50["SLOPE"]; # min - 0, max - 77 in ec50 it is max - 100, min - 23
    tmp = coef_tec50["MAX"]; coef_tec50["MAX"] = 100 - coef_tec50["MIN"]; coef_tec50["MIN"] = 100 - tmp; ytec <- 100 - yic;
    perViaTox <- 100 - perInh;
  }
  
  dss_score <- round(as.numeric(dss(coef_ic50["IC50"],coef_ic50["SLOPE"],coef_ic50["MAX"],min_signal, max_signal, DSS.type=as.integer(DSS_typ))),1);
  
  ####
  # Absolute IC50
  xIC50ABS <- seq(min(mat_tbl$logconc),max(mat_tbl$logconc)*15, length=5000)
  yicIC50ABS <- predict(nls_result_ic50, data.frame(logconc=xIC50ABS))
  if(all(yicIC50ABS < 50)) coef_ic50ABS= Inf else coef_ic50ABS = 10**xIC50ABS[which.min(abs(yicIC50ABS - 50))]
  ####
  
  mat_tbl$inhibition <- inhibition2
  
  list_dss <- list(dss = dss_score, df = mat_tbl, x = x, y = yic, ic50 = coef_ic50["IC50"], doses = x.new, sd_model = ic50std_resid, 
                   IC50_abs = coef_ic50ABS, y.points = y.points, slope = coef_ic50["SLOPE"], max = coef_ic50["MAX"])
  
  return(list_dss)
}


mixture_sym_gaussian <- function(x, A1, mu1, sigma1, A2, mu2, sigma2, 
                                 A3, mu3, sigma3, A4, mu4, sigma4) {
  A1 * exp(-((x - mu1)^2) / (2 * sigma1^2)) +
    A2 * exp(-((x - mu2)^2) / (2 * sigma2^2)) +
    A3 * exp(-((x - mu3)^2) / (2 * sigma3^2)) +
    A4 * exp(-((x - mu4)^2) / (2 * sigma4^2))
}

auc_gaussian <- function(A, mu, sigma, lower, upper) {
  lower_cdf <- pnorm(lower, mean = mu, sd = abs(sigma))
  upper_cdf <- pnorm(upper, mean = mu, sd = abs(sigma))
  A * sqrt(2 * pi * sigma^2) * (upper_cdf - lower_cdf)
}

Fit_Gaussian_CoCulture <- function(df_CC) {
  
  doses <- as.numeric(df_CC$x)
  predicted.points <- as.numeric(df_CC$y)
  
  dose_shift <- max(0, -min(doses))
  df.drug <- data.frame(x = doses + dose_shift, y = predicted.points)
  
  x_min <- min(df.drug$x)
  x_max <- max(df.drug$x)
  x_range <- x_max - x_min
  
  flag_shift <- FALSE
  flag_negative <- FALSE
  flag_activity <- FALSE
  
  if (max(abs(df.drug$y)) < 10) {
    
    highest_peak <- 0
    effective_dose <- NA_integer_
    normalized_AUC <- 0
    units_shift <- 0
    dose_seq <- fitted_values <- rep(0, 100)
    case <- "Insufficient activity"
    CES <- 0
    flag_activity <- TRUE
    
  } else {
    
    if (min(df.drug$y) < -10) {
      if (all(df.drug$y < 0) || max(df.drug$y) < 10) {
        flag_negative <- TRUE
        df.drug$y <- -1 * df.drug$y
        units_shift <- 0
        case <- "Negative"
      } else {
        flag_shift <- TRUE
        units_shift <- abs(min(df.drug$y))
        df.drug$y <- df.drug$y + units_shift
        case <- "Shift"
      }
    } else {
      units_shift <- 0
      case <- "Positive"
    }
    
    # ------------------------------------------------------------------
    # Precompute
    # ------------------------------------------------------------------
    x_data <- df.drug$x
    y_data <- df.drug$y
    y_max <- max(y_data)
    n_points <- length(x_data)
    
    # Cost: SSE between data and 4-Gaussian mixture
    cost_function <- function(params) {
      y_pred <- mixture_sym_gaussian(
        x_data, params[1], params[2], params[3], params[4], params[5], params[6],
        params[7], params[8], params[9], params[10], params[11], params[12]
      )
      sum((y_data - y_pred)^2)
    }
    
    # Analytical gradient of SSE w.r.t. (A_i, mu_i, sigma_i) for i = 1..4
    # For peak i with g_i = exp(-(x-mu_i)^2 / (2*sigma_i^2)) and residual r = y - y_pred:
    #   dSSE/dA_i     = -2 * sum(r * g_i)
    #   dSSE/dmu_i    = -2 * sum(r * A_i * g_i * (x - mu_i) / sigma_i^2)
    #   dSSE/dsigma_i = -2 * sum(r * A_i * g_i * (x - mu_i)^2 / sigma_i^3)
    grad_function <- function(params) {
      G <- matrix(0, nrow = n_points, ncol = 4)
      y_pred <- numeric(n_points)
      
      for (i in 1:4) {
        idx <- (i - 1) * 3
        A     <- params[idx + 1]
        mu    <- params[idx + 2]
        sigma <- params[idx + 3]
        G[, i] <- exp(-((x_data - mu)^2) / (2 * sigma^2))
        y_pred <- y_pred + A * G[, i]
      }
      
      r <- y_data - y_pred
      grad <- numeric(12)
      
      for (i in 1:4) {
        idx <- (i - 1) * 3
        A     <- params[idx + 1]
        mu    <- params[idx + 2]
        sigma <- params[idx + 3]
        dx <- x_data - mu
        g_i <- G[, i]
        
        grad[idx + 1] <- -2 * sum(r * g_i)
        grad[idx + 2] <- -2 * sum(r * A * g_i * dx / (sigma^2))
        grad[idx + 3] <- -2 * sum(r * A * g_i * (dx^2) / (sigma^3))
      }
      
      grad
    }
    
    # Smoothness bounds — prevents needle-peak artifacts
    min_sigma  <- max(0.05, x_range / 50)
    sig_narrow <- max(min_sigma * 2, x_range / 10)
    sig_wide   <- x_range / 4
    sig_full   <- x_range / 2
    
    lower_bounds <- rep(c(0,     x_min, min_sigma), 4)
    upper_bounds <- rep(c(y_max, x_max, x_range),   4)
    
    idx_max <- which.max(y_data)
    x_peak  <- x_data[idx_max]
    
    starts <- list(
      # S1: Dominant peak at observed max + broad support
      c(y_max,     x_peak,                 sig_narrow,
        y_max / 2, x_min + x_range * 0.3,  sig_wide,
        y_max / 2, x_min + x_range * 0.7,  sig_wide,
        y_max / 4, mean(x_data),           sig_full),
      
      # S2: Even spread across dose range
      c(y_max / 2, x_min + x_range * 0.2,  sig_wide,
        y_max / 2, x_min + x_range * 0.4,  sig_wide,
        y_max / 2, x_min + x_range * 0.6,  sig_wide,
        y_max / 2, x_min + x_range * 0.8,  sig_wide),
      
      # S3: Single broad peak (unimodal — catches simple sigmoid-like curves)
      c(y_max,       x_peak, sig_full,
        y_max * 0.1, x_peak, sig_narrow,
        0,           x_min,  min_sigma,
        0,           x_min,  min_sigma)
    )
    
    starts <- lapply(starts, function(s) pmin(pmax(s, lower_bounds), upper_bounds))
    
    # Early-stop threshold: relative SSE below ~machine precision of the signal
    y_energy       <- sum(y_data^2)
    early_stop_tol <- 1e-6 * max(y_energy, 1)  # guard against y_energy == 0
    
    results  <- vector("list", length(starts))
    best_obj <- Inf
    best_idx <- NA_integer_
    
    for (k in seq_along(starts)) {
      results[[k]] <- tryCatch(
        nlminb(
          starts[[k]], cost_function, gradient = grad_function,
          lower = lower_bounds, upper = upper_bounds,
          control = list(iter.max = 500, eval.max = 1000)
        ),
        error = function(e) list(objective = Inf)
      )
      
      obj_k <- results[[k]]$objective
      if (!is.null(obj_k) && is.finite(obj_k) && obj_k < best_obj) {
        best_obj <- obj_k
        best_idx <- k
      }
      
      # Near-perfect fit — no need to run further starts
      if (best_obj < early_stop_tol) break
    }
    
    # DEoptim fallback only if all nlminb starts failed outright
    if (is.infinite(best_obj)) {
      fit <- DEoptim::DEoptim(
        cost_function,
        lower = lower_bounds, upper = upper_bounds,
        control = DEoptim::DEoptim.control(trace = FALSE, itermax = 500)
      )
      best_params <- fit$optim$bestmem
    } else {
      best_params <- results[[best_idx]]$par
    }
    
    # ------------------------------------------------------------------
    # METRICS
    # ------------------------------------------------------------------
    dose_seq      <- seq(x_min, x_max, length.out = 100)
    fitted_values <- do.call(mixture_sym_gaussian, c(list(dose_seq), as.list(best_params)))
    
    AUCs <- sapply(1:4, function(i) {
      idx <- (i - 1) * 3 + 1
      auc_gaussian(best_params[idx], best_params[idx + 1], best_params[idx + 2], x_min, x_max)
    })
    
    normalized_AUC <- sum(AUCs) / x_range
    
    x_sim  <- seq(x_min, x_max, length.out = 2^9)  # 512 points is ample for smooth mixtures
    max_xy <- do.call(mixture_sym_gaussian, c(list(x_sim), as.list(best_params)))
    
    i_max          <- which.max(max_xy)
    effective_dose <- 10^(x_sim[i_max] - dose_shift)
    highest_peak   <- max(max_xy)
  }
  
  # ------------------------------------------------------------------
  # POST-PROCESSING
  # ------------------------------------------------------------------
  dose_seq       <- dose_seq - dose_shift
  fitted_values  <- fitted_values - units_shift
  highest_peak   <- highest_peak - units_shift
  normalized_AUC <- as.numeric(unname(normalized_AUC))
  
  if (flag_shift) {
    normalized_AUC <- normalized_AUC - units_shift
  }
  
  if (!flag_activity) {
    if (highest_peak < 10) highest_peak <- 10
    CES <- normalized_AUC * log10(highest_peak) / x_range
    CES <- round(CES, digits = 4)
  }
  
  if (flag_negative) {
    normalized_AUC <- -1 * normalized_AUC
    highest_peak   <- -highest_peak
    fitted_values  <- -1 * fitted_values
    CES            <- -CES
  }
  
  if (case == "Insufficient activity") {
    dose_seq <- seq(x_min, x_max, length.out = 100) - dose_shift
  }
  
  highest_peak   <- pmin(pmax(highest_peak, -100), 100)
  highest_peak   <- round(highest_peak, digits = 3)
  normalized_AUC <- round(normalized_AUC, digits = 3)
  effective_dose <- round(effective_dose, digits = 3)
  
  list(
    CES = CES, highest_peak = highest_peak, effective_dose = effective_dose,
    normalized_AUC = normalized_AUC, case = case, dose_shift = dose_shift,
    dose_seq = dose_seq, fitted_values = fitted_values
  )
}

filter_curves <- function(CES, slope_cc, slope_mono, DSS_CC, DSS_mono) {
  
  # 1) Both curves go down but CES is positive -> likely artifact
  both_down_CES_pos <- (slope_cc < 0 & slope_mono < 0) & (CES > 0)
  
  # 2) Strongly negative behaviour in both CC and mono across doses
  #    (extreme, globally "bad" drug-response, independent of toxicity labeling)
  both_highly_down <- (DSS_CC <= -10 & DSS_mono <= -10) 
  
  # TRUE = to be excluded
  out <- both_down_CES_pos | both_highly_down
  
  # Missing info -> do not exclude just because of NA
  out[is.na(out)] <- FALSE
  
  return(out)
}



