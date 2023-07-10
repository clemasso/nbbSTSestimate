#'
#' Estimation of time series by State Space models (incl. low frequency input)
#'
#' The package performs in-sample imputation of missing values, as well as
#' for-now- and back-casting of univariate time series by integrating (or not)
#' low-frequency input. Common structural models can be used for this purpose.
#' Treatment of outliers and calendar effects are integrated. The tool includes
#' automatic procedures for model selection, outlier detection and insertion of
#' calendar effects.
#' 
#' @param x (Set of) high frequency series to impute. 
#'          Should be either a (m)ts object or a data frame. In the second case, 
#'          the first column should be in a format that can be translated into 
#'          a Date object.
#' @param x.lf (Set of) low frequency series.
#'             Should be either a (m)ts object or a data frame. In the second 
#'             case, the first column should be in a format that can be 
#'             translated into a Date object. It can also be defined as NULL
#'             (default value) when no low frequency series is available.
#'             As for x, it can also include missing values
#' @param stsmodel Structural model to consider for the high frequency series.
#'                 When 'auto' is selected (default value), an automatic
#'                 selection of model is performed. When 'mixed' is selected,
#'                 it allows the user to attribute a different model for each
#'                 series individually via the parameter 'stsmodel.df'
#' @param stsmodel.df Only used when stsmodel = 'mixed'. Ignored otherwise.
#'                    When relevant, it should consist of a data.frame 
#'                    containing two columns: the first one called 'series_name' 
#'                    and the second one called 'stsmodel'. The stsmodel for 
#'                    each series must be a valid model choosen in the list of 
#'                    models authorized in 'stsmodel' (including 'auto').
#' @param outliers Treatment of outliers. When outliers = 'auto', an automatic
#'                 outlier detection is performed for each series. When 
#'                 outliers = 'none', no outlier is considered. Please use this
#'                 option with care as structural models, like any linear model, 
#'                 are sensitive to undetected outliers which could result to 
#'                 poor imputation. Finally, outliers = 'userdefined' allows
#'                 you to define the outliers type and period manually or use 
#'                 a different treatment for each series via the parameter
#'                 'outliers.df'.
#' @param outliers.df Only used when outliers = 'userdefined'. Ignored otherwise.
#'                    When relevant, it should consist of a data.frame 
#'                    containing two columns: the first one called 'series_name' 
#'                    and the second one called 'outliers'. The outliers 
#'                    specified for each series should mentionned the type of 
#'                    outlier and the period in the following format 
#'                    'LS_2023Q1'for quarterly series of 'AO_2023M01' for
#'                    monthly series. To  define several outliers for a series,
#'                    just write the outliers one below each other while 
#'                    repeating the series name each time in the first column.
#'                    It is also possible to use the options 'auto' and 'none'
#'                    for a series as in 'outliers'.                                                            
#' @param cal.effect Treatment of calendar effects. When cal.effect = 'auto', 
#'                   an automatic detection of calendar effects is performed 
#'                   for each series. It can also be ignored or forced. When 
#'                   calendar effects are considered, the specifications defined
#'                   in 'cal.effect.td' and 'cal.effect.easter' are used. 
#'                   Finally, when 'mixed' is selected, it allows the user to 
#'                   use different specifications for each series via the
#'                   parameter 'cal.effect.df'. Note that calendar effects are
#'                   mostly important for monthly series. In quarterly series,
#'                   they are roughly detectable and therefore less relevant.  
#' @param cal.effect.td Calendar to consider for trading days. The 'Default'
#'                      only consider the number of Monday, Tuesday, ..., Sunday
#'                      at each period. 'WesternEU' and 'BE' also add national
#'                      holidays (see vignette for more details) and consider
#'                      them as Sundays.                   
#' @param cal.effect.easter A boolean parameter whether Easter holiday should 
#'                          or not be considered amongst the calendar effects.                  
#' @param cal.effect.df Only used when cal.effect = 'mixed'. Ignored otherwise.
#'                      When relevant, it should consist of a data.frame 
#'                      containing four columns called 'series_name', 
#'                      'cal.effect', 'cal.effect.td' and 'cal.effect.easter'.
#'                      The option selected for each of the three last variables 
#'                      should  be a valid option in the list of options 
#'                      authorized in the corresponding parameter.  
#' @param conversion type of consistency between the high frequency series and 
#'                   the low frequency series (if any)
#' @param path.xlsx a string containing the path of the XLSX file to copy
#'                  the imputed series. If the path is NULL, no file is created.
#' @import data.table readxl openxlsx
#' @import rjd3toolkit rjd3sts rjd3tramoseats
#' @importFrom zoo as.Date.yearmon
#' @return an object of class "nbb.estimateSTS.output"
#' @export
#' @examples
#' retail_extract<-cbind(GroceryStores=rjd3toolkit::retail$GroceryStores, 
#'                       BookStores=rjd3toolkit::retail$BookStores, 
#'                       ShoeStores=rjd3toolkit::retail$ShoeStores)
#' 
#' x<-window(retail_extract, c(1990,1), c(2012,6), extend = TRUE)
#' x.lf<-ts(rbind(matrix(c(325000,330000,7800,8000,17500,18000), nrow = 2, byrow = FALSE),
#'          aggregate.ts(retail_extract, nfreq = 1),
#'          matrix(c(525000,15000,27000), nrow = 1)), start = c(1990,1), frequency = 1)
#' res1 <- estimateSTS(x, x.lf) # auto procedures for model selection, outlier detection and calendar effect by default
#' #plot(res1,"BookStores")
#'   
#' outliers_input<-data.frame(series_name=c("GroceryStores", "GroceryStores", "BookStores", "ShoeStores"),
#'                             outliers=c("LS_2008M12", "AO_2002M04", "auto", "auto"))
#' res2 <- estimateSTS(x, x.lf, stsmodel = "bsm", 
#'                  outliers = "userdefined",
#'                  outliers.df = outliers_input,
#'                  cal.effect = "auto",
#'                  cal.effect.td = "BE",
#'                  cal.effect.easter = TRUE)
#'                    
estimateSTS <- function(x, 
                        x.lf = NULL, 
                        stsmodel = c("auto", "bsm", "llt", "ll", "noise", "mixed"),
                        stsmodel.df = NULL,
                        outliers = c("auto", "none", "userdefined"),
                        outliers.df = NULL,
                        cal.effect = c("auto", "none", "forced", "mixed"),
                        cal.effect.td = c("Default", "WesternEU", "BE", "none"),
                        cal.effect.easter = TRUE,
                        cal.effect.df = NULL,
                        conversion = c("Sum", "Average"), 
                        path.xlsx = NULL){

  cl<-match.call()
  
  # I. Pre-processing
  
  # I.a. Input validation

  x<-convert_to_ts(x)
  if(!is.null(x.lf)){
    x.lf<-convert_to_ts(x.lf)
  }
  stsmodel<-match.arg(stsmodel)
  if(stsmodel == "mixed"){
    stsmodel_df<-handle_stsmodel_df(x, stsmodel.df)
  }else{
    stsmodel<-check_stsmodel(x, stsmodel)
  }
  outliers<-match.arg(outliers)
  if(outliers == "userdefined"){
    iv_list<-handle_outliers_data(x, outliers.df)
  }
  cal.effect<-match.arg(cal.effect)
  if(cal.effect == "mixed"){
    cal_effect_df<-handle_cal_effect_df(x, cal.effect.df)
  }else{
    cal.effect<-check_cal.effect(x, cal.effect)
  }
  cal.effect.td<-match.arg(cal.effect.td)
  stopifnot(is.logical(cal.effect.easter))
  conversion<-match.arg(conversion)
  if(conversion == "Average"){
    x<-x/frequency(x)
  }
  if(!is.null(path.xlsx)){
    if(!file.exists(dirname(path.xlsx))) stop("Path xlsx not found!")
  }
  
  # I.b. Output initialization
  
  out <- list()
  class(out) <- "nbb.estimateSTS.output"
  
  if(is.null(ncol(x))){
    nr<-length(x)
    nc<-1
  }else{
    nr<-nrow(x)
    nc<-ncol(x)
  }
  x_imputed<-as.data.frame(matrix(nrow=nr,ncol=nc))
  colnames(x_imputed)<-colnames(x)
  
  
  # II. Processing
  
  freq<-frequency(x)
  n<-ifelse(is.null(ncol(x)),1,ncol(x))
  
  for (i in 1:n){
    
    # II.a. Select series
    
    if(is.null(ncol(x))){
      namei<-fifelse(is.null(names(x)), "series", "names(x)")
      xi<-x
      xi_lf<-x.lf
    }else{
      xi<-x[,i]
      namei<-colnames(x)[i]
      if(is.null(x.lf)){
        xi_lf<-NULL
      }else{
        if(colnames(x)[i] %in% colnames(x.lf)){
          xi_lf<-x.lf[,colnames(x)[i]]
        }else{
          xi_lf<-NULL
        }
      }
    }
 
    # II.b. Take the cumulative sum per LF period when xi.lf is provided 
    
    if(!is.null(xi_lf)){
      
      xi_dt<-data.table(xi)
      xi_lf_dt<-data.table(xi_lf)
      freq_lf<-frequency(xi_lf)
      
      ## Define time group variable for cumulative sum 
      gvar<-create_time_gvar(xi, freq_lf)
      gvar_lf<-create_time_gvar(xi_lf, freq_lf)
      xi_dt[,group:=gvar]
      xi_lf_dt[,group:=gvar_lf]
      
      ## Calculate cumulative sum
      xi_dt[, xic := cumsum(xi), by = group]
      
      ## Integrate lf input
      pos_xi_lastby<-xi_dt[,.(group)]
      pos_xi_lastby[,pos := .I]      
      pos_xi_lastby<-pos_xi_lastby[,.SD[.N],by=group]
      
      xi_lf_dt<-merge(xi_lf_dt, pos_xi_lastby, by="group")
      pos_lf<-xi_lf_dt$pos
      value_lf<-xi_lf_dt$x
      
      xi_lf_hf<-rep(NA,nrow(xi_dt))
      xi_lf_hf[pos_lf]<-value_lf 
      xi_dt<-cbind(xi_dt, xi_lf_hf)
      
      xi_dt[, xicc := fifelse(is.na(xi_lf_hf), xic, xi_lf_hf)]
      
      ## Check coherence between LF and HF data
      xi_dt[, diff_lf_hf := xi_lf_hf - xic]
      xi_dt[, diff_lf_hf_rel := abs(fifelse(xic != 0, diff_lf_hf/xic, diff_lf_hf))]
      if (any(!is.na(xi_dt[,diff_lf_hf]) & xi_dt[,diff_lf_hf_rel] > 0.001)){
        warning(paste0(namei,": The series could not be processed. This is because some LF data do not match the sum of the corresponding HF data. Please check your input and use a benchmarking tool for this series if necessary."), call. = FALSE)
        out[[i]] <- list(NULL)
        names(out)[i] <- namei
        x_imputed[,i] <- ts(NA, start=start(x), frequency=freq)
        next
      }
      
      ## Deal with partially missing data on LF period(s)
      pmd_lf_dt<-xi_dt[!is.na(xi) & is.na(xic)]
      
      if(nrow(pmd_lf_dt) > 0){
        concerned_groups<-unique(pmd_lf_dt$group)
        processable<-TRUE
        
        for(g in concerned_groups){
          xi_dt[group == g, xi_lead := as.numeric(shift(xi,-1))]
          for(k in 1:(freq-1)){
            xi_dt[group == g, xicc_lead := shift(xicc,-1)]
            xi_dt[group == g & is.na(xicc) & !is.na(xi_lead) & !is.na(xicc_lead) , xicc := xicc_lead - xi_lead]
          }
          if(nrow(xi_dt[group == g & (!is.na(xi_lead) & is.na(xicc))]) > 0) processable<-FALSE
        }
        
        if(!processable){
          warning(paste0(namei,": The series could not processed. This is because missing/non-missing data within some LF period are not regrouped and, therefore, cumulated data could not be computed. These cases cannot yet be handled."), call. = FALSE)
          out[[i]] <- list(NULL)
          names(out)[i] <- namei
          x_imputed[,i] <- ts(NA, start=start(xi), frequency=freq)
          next
        }
      }
      
      ## Model input
      xi_run<-ts(xi_dt$xicc, start = start(xi), frequency = freq)
      cumulator<-TRUE
      ratio_hflf<-freq/freq_lf
      
    }else{
      xi_run<- xi
      cumulator<-FALSE
      ratio_hflf<-NA
    }
    
    # II.c. Model selection
    
    if(!is.null(stsmodel.df)){
      stsmodel<-stsmodel_df[stsmodel_df$series_name == namei,]$stsmodel
    }
    
    auto_model<-fifelse(stsmodel == "auto", TRUE, FALSE)
    
    if(auto_model){
      if(is_seasonal(xi)){
        stsmodel_selected<-"bsm"
      }else{
        xi_fill<-try(ts_interpolate(xi, method="airline"), silent = TRUE) # quick filling of missing values
        if(!"try-error" %in% class(xi_fill)) {
          d<-as.numeric(differencing_fast(xi_fill, freq, mad = TRUE)$differences["order",1])
          if(d == 0){
            stsmodel_selected<-"noise"
          }else if(d == 1){
            stsmodel_selected<-"ll"
          }else if(d > 1){
            stsmodel_selected<-"llt"
          }
        }else{
          stsmodel_selected<-"ll"
        }
      }
    }else{
      stsmodel_selected<-stsmodel
    }  
    
    # II.d. Outliers
    
    if(outliers == "userdefined"){
      ifelse(iv_list[[namei]] == "auto", outliers<-"auto", regiv<-iv_list[[namei]])
    }
    
    if(outliers == 'auto'){
      res_outliers<-try(tramo_outliers(xi)$model, silent = TRUE) # checking for outliers with missing values first
      if(!"try-error" %in% class(res_outliers)) {
        iv<-res_outliers$X
        if(ncol(iv) == 0) iv<-NULL
      }else{
        if(!exists("xi_fill")) xi_fill<-try(ts_interpolate(xi, method="airline"), silent = TRUE) # quick filling of missing values if too many of them
        
        if(!"try-error" %in% class(xi_fill)) {
          res_outliers<-try(tramo_outliers(xi_fill)$model, silent = TRUE)
          if(!"try-error" %in% class(res_outliers)) {
            iv<-res_outliers$X
            if(ncol(iv) == 0) iv<-NULL
          }else{
            iv<-NULL
            warning(paste0(namei, ": Outlier detection could not be performed due to the characteristics of the series. No outlier defined for this series."), call. = FALSE)
          }
        }else{
          iv<-NULL
        }
      }
      
      if(!is.null(iv)){
        outlier_variables<-res_outliers$variables[substr(res_outliers$variables,1,2) %in% c("AO","LS","TC","SO")]
        colnames(iv)<-outlier_variables
        iv<-ts(iv, start = start(xi), frequency = freq)
      }
      regiv<-iv
    }else if(outliers == "none"){
      regiv<-NULL 
    } 
    
    # II.e. Calendar effects
    
    if(cal.effect == "mixed"){
      cal_effect_df_i<-cal_effect_df[cal_effect_df$series_name == namei,]
      cal.effect<-cal_effect_df_i$cal.effect
      cal.effect.td<-cal_effect_df_i$cal.effect.td
      cal.effect.easter<-as.logical(cal_effect_df_i$cal.effect.easter)
    }
    
    if(cal.effect == 'auto'){
      if (freq > 4){
        xi_fill<-ts_interpolate(xi, method="airline") # quick filling of missing values
        
        ## TD
        f_pval<-td_f(xi_fill)$pvalue
        if(f_pval < .05){
          regTD<-create_htdreg(xi, type = cal.effect.td)
        }else{
          regTD<-NULL
        }
        ## Easter
        easterreg<-create_easterreg(xi)
        t_pval<-try(easter_f(xi_fill, easterreg, regressors=cbind(regiv,regTD))$pvalue, silent = TRUE)
        if(!"try-error" %in% class(t_pval)) {
          if(t_pval < .05){
            regeaster<-easterreg
          }else{
            regeaster<-NULL
          }
        }else{
          regeaster<-NULL
        }
        ## All
        regcal<-cbind(regTD, regeaster)
      } else{
        regcal<-NULL
      }
    }else if(cal.effect == "forced"){
      if(!cal.effect.td == "none" & cal.effect.easter){
        regTD<-create_htdreg(xi, cal.effect.td)
        regeaster<-create_easterreg(xi)
        regcal<-cbind(regTD, regeaster)
        colnames(regcal)<-c(paste0("CAL_",colnames(regTD)), "CAL_easter")
      }else if(!cal.effect.td == "none"){
        regcal<-regTD<-create_htdreg(xi, cal.effect.td)
        colnames(regcal)<-paste0("CAL_",colnames(regTD))
      }else if(cal.effect.easter){
        regcal<-regeaster<-create_easterreg(xi)
        colnames(regcal)<-"CAL_easter"
      }else{
        regcal<-NULL
      }
    }else if(cal.effect == "none"){
      regcal<-NULL
    }

    # II.f. Regroup regressors
    
    regall<-cbind(regiv,regcal)
    
    if(!is.null(regall)){
      colnames(regall)<-c(colnames(regiv), colnames(regcal))
    }

    # II.g. Model series by state space
    ## NOTE: Implicit assumption that hf and lf data come from the same source (same quality)

    ## Run model
    res<-try(sts.run(xi_run, stsmodel = stsmodel_selected, cumulator = cumulator, cumulator.ratio = ratio_hflf, regressors = regall), silent = TRUE)
    
    if("try-error" %in% class(res)) {
      warning(paste0(namei, ": The series could not be processed. It could be an initialization issue. Please check your input or ask for help if necessary."), call. = FALSE)
      out[[i]] <- list(NULL)
      names(out)[i] <- namei
      x_imputed[,i] <- ts(NA, start=start(x), frequency=frequency(x))
      next
    }
    
    # II.h. Tests of specification (+re-run automatic procedure with adjusted specification based on the results of the tests)
    
    ## On instrumental variables
    if(!is.null(regiv)){
      if(outliers == "auto"){
        if(is_nonsignificant_iv(res)){
          regall_cleaned<-clean_iv(xi_run, stsmodel_selected, cumulator=cumulator, cumulator.ratio = ratio_hflf, regressors = regall, t_reg = res$regressors$tstat)
          res<-sts.run(xi_run, stsmodel_selected, cumulator = cumulator, cumulator.ratio = ratio_hflf, regressors = regall_cleaned)
        }else{
          regall_cleaned<-regall
        }
      }else{
        if(is_nonsignificant_iv(res)){
          regall_cleaned<-regall
          warning(paste0(namei, ": Some specified outliers are not significant."), call. = FALSE) # just a warning if not automatic procedure
        }
      }
    }else{
      regall_cleaned<-regall
    }
    
    ## On model
    if(stsmodel_selected == "bsm"){
      if(!auto_model & !is_seasonal(xi)){
        include_slope<-is_slope(xi_run, cumulator=cumulator, cumulator.ratio = ratio_hflf, regressors = regall_cleaned)
        suggested_model<-fifelse(include_slope, "Local Linear Trend", "Local Level")
        warning(paste0(namei, ": A BSM model was selected but statistical evidence shows that a ", suggested_model, " model might be more suited for this series."), call. = FALSE)
      }
    }else if(stsmodel_selected == "llt"){
      if(auto_model){
        include_slope<-is_slope(xi_run, cumulator=cumulator, cumulator.ratio = ratio_hflf, regressors = regall_cleaned)
        if(!include_slope){
          stsmodel_selected<-"ll"
          res<-sts.run(xi_run, stsmodel_selected, cumulator = cumulator, cumulator.ratio = ratio_hflf, regressors = regall_cleaned)
        }
      }else{
        if(is_seasonal(xi)){
          warning(paste0(namei, ": A Local Linear Trend model was selected but statistical evidence shows that a BSM model might be more suited for this series."), call. = FALSE)
        }else if(!is_slope(xi_run, cumulator=cumulator, cumulator.ratio = ratio_hflf, regressors = regall_cleaned)){
          warning(paste0(namei, ": A Local Linear Trend model was selected but statistical evidence shows that a Local level model might be more suited for this series."), call. = FALSE)
        }
      }
    }else if(stsmodel_selected == "ll"){
      if(auto_model){
        include_slope<-is_slope(xi_run, cumulator=cumulator, cumulator.ratio = ratio_hflf, regressors = regall_cleaned)
        if(include_slope){
          stsmodel_selected<-"llt"
          res<-sts.run(xi_run, stsmodel_selected, cumulator = cumulator, cumulator.ratio = ratio_hflf, regressors = regall_cleaned)
        }
      }else{
        if(is_seasonal(xi)){
          warning(paste0(namei, ": A Local Level model was selected but statistical evidence shows that a BSM model might be more suited for this series."), call. = FALSE)
        }else if(is_slope(xi_run, cumulator=cumulator, cumulator.ratio = ratio_hflf, regressors = regall_cleaned)){
          warning(paste0(namei, ": A Local Level model was selected but statistical evidence shows that a Local Linear Trend model might be more suited for this series."), call. = FALSE)
        }
      }
    }else if(stsmodel_selected == "noise"){
      if(auto_model){
        include_trend<-is_trend(xi_run, cumulator=cumulator, cumulator.ratio = ratio_hflf, regressors = regall_cleaned)
        if(include_trend){
          include_slope<-is_slope(xi_run, cumulator=cumulator, cumulator.ratio = ratio_hflf, regressors = regall_cleaned)
          stsmodel_selected<-fifelse(include_slope, "llt","ll")
          res<-sts.run(xi_run, stsmodel_selected, cumulator = cumulator, cumulator.ratio = ratio_hflf, regressors = regall_cleaned)
        }
      }else{
        if(is_seasonal(xi)){
          warning(paste0(namei, ": A Noise model was selected but statistical evidence shows that a BSM model might be more suited for this series."), call. = FALSE)
        }else if(is_trend(xi_run, cumulator=cumulator, cumulator.ratio = ratio_hflf, regressors = regall_cleaned)){
          include_slope<-is_slope(xi_run, cumulator=cumulator, cumulator.ratio = ratio_hflf, regressors = regall_cleaned)
          suggested_model<-fifelse(include_slope, "Local Linear Trend", "Local Level")
          warning(paste0(namei, ": A Noise model was selected but statistical evidence shows that a ", suggested_model, " model might be more suited for this series."), call. = FALSE)
        }
      }
    }
    
    # II.i. Rescale output when conversion = 'Average'
    
    if(conversion == "Average"){
      res$table<-res$table*freq
      if(!is.null(res$regressors)){
        res$regressors$param<-res$regressors$param*freq
        res$regressors$param<-res$regressors$vparam*freq^2
      }
      res$hparam<-res$hparam*freq^2
      res$likelihood$ser<-res$likelihood$ser*freq # also, differences in likelihood not adapted here
      res$vslope<-res$vslope*freq^2
    }
    
    # II.j. Fill output
    
    out[[i]] <- res
    names(out)[i] <- namei
    x_imputed[,i] <- res$table[,"Series"]
  }

  
  # III. Output
  
  # III.a. Format the set of imputed series and rescale initial input when conversion = 'Average'
  
  x_imputed<-ts(x_imputed, start = start(x), end = end(x), frequency = freq)
  if(conversion == "Average") x <- x*freq
  
  # III.b. Extract imputed series in an .xlsx file if requested
  
  if(!is.null(path.xlsx)){
    wb <- createWorkbook()
    addWorksheet(wb, "imputed series")
    writeData(wb, "imputed series", data.frame(date=as.Date.yearmon(time(x_imputed)), x_imputed), startRow = 1, startCol = 1)
    saveWorkbook(wb, file = path.xlsx, overwrite = TRUE)
  }
  
  # III.c. Add output
  
  out$call <- cl
  out$x <- x
  out$x.lf <- x.lf
  out$x.imputed <- x_imputed
  
  return(out)
}

#'
#' Estimation of time series by State Space models (incl. low frequency input)
#' with input coming from a structured XLSX file
#' 
#' See ?estimateSTS for more description. See vignette for explanation about the
#' required structure of the XLSX file.
#' 
#' @param path.data a string containing the path of the input XLSX file.
#' @param is.lf a boolean parameter whether low frequency series should be 
#'              considered.  
#' @param conversion type of consistency between the high frequency series and 
#'                   the low frequency series (if any)
#' @param path.output a string containing the path of the output (which is also
#'                    an XLSX file) containing the imputed series.
#'                    If the path is NULL, no file is created.
#'                  
#' @import readxl
#' @return an object of class "nbb.estimateSTS.output"
#' @export
#' @examples
#' \dontrun{
#' file.name<-"myinput.xlsx"
#' res<-estimateSTS_fromXLSX(path.data = "input_to_R.xlsx",
#'                           is.lf = TRUE,
#'                           conversion = "Sum",
#'                           path.output = "output_R.xlsx")
#' }
#' 
estimateSTS_fromXLSX <- function(path.data,
                                 is.lf = TRUE,
                                 conversion = c("Sum", "Average"), 
                                 path.output = NULL){
  
  x<-as.data.frame(read_excel(path.data, sheet = "x"))
  if(is.lf){
    x.lf<-as.data.frame(read_excel(path.data, sheet = "x.lf"))
  }else{
    x.lf<-NULL
  }
  stsmodel.df<-as.data.frame(read_excel(path.data, sheet = "stsmodel"))
  outliers.df<-as.data.frame(read_excel(path.data, sheet = "outliers"))
  cal.effect.df<-as.data.frame(read_excel(path.data, sheet = "cal.effect"))
  
  out <- estimateSTS(x, 
                     x.lf, 
                     stsmodel = "mixed",
                     stsmodel.df,
                     outliers = "userdefined",
                     outliers.df,
                     cal.effect = "mixed",
                     cal.effect.td = "Default",
                     cal.effect.easter = TRUE,
                     cal.effect.df,
                     conversion, 
                     path.xlsx = path.output)
  
  return(out)
}
