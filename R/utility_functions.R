# Convert data frame to ts object
convert_to_ts<-function(x){
  if (is.ts(x)){
    x_ts <- x
  } else if (is.data.frame(x)){
    if (!ncol(x) > 1) stop("Missing input!")
    first_date <- x[[1,1]]
    if (is.convertible.to.date(first_date)){
      first_yr <- year(first_date)
      colnames(x)<-c("DATE",colnames(x)[-1])
      all_dates_yr <- year(x$DATE)
      freq <- length(all_dates_yr[all_dates_yr == first_yr+1])
      first_infra <- freq - length(all_dates_yr[all_dates_yr == first_yr]) + 1
      x_ts <- ts(x[,-1], start = c(first_yr, first_infra), frequency = freq)
    } else stop("First column of the input data frame cannot be converted to a date object.")
  } else stop("Input series should be either a data frame or a ts object")
  return(x_ts)
}

# Check if character argument can be converted to a 'Date' object
is.convertible.to.date <- function(x) !is.na(as.Date(as.character(x), tz = 'UTC', format = '%Y-%m-%d'))

# Check if stsmodel provided by the user is valid
check_stsmodel <- function(x, stsmodel){
  if (frequency(x) == 1){
    if (stsmodel == "bsm"){
      stsmodel<-"auto"
      warning("You cannot defined stsmodel = 'bsm' with yearly data. stsmodel = 'auto' was considered instead. Please check your input if you want it otherwise.", call. = FALSE)
    }
  }
  return(stsmodel)
}

# Check that 'stsmodel.df' is filled in properly when stsmodel = "mixed"
handle_stsmodel_df <- function(x, stsmodel.df){
  if (!is.data.frame(stsmodel.df)){
    stop("Since you mentionned stsmodel = 'mixed', you must provide the stsmodel of the series as a data.frame in 'stsmodel.df'.
         The stsmodel.df should contain two columns: the first one called 'series_name' and the second one called 'stsmodel'.
         The stsmodel for each series must be a valid model choosen in the list of models authorized in 'stsmodel' (including 'auto').")
  } else {
    if (all(colnames(stsmodel.df) %in% c("series_name", "stsmodel"))){
      ## convert columns to character
      stsmodel.df<-data.frame(lapply(stsmodel.df, as.character), stringsAsFactors=FALSE)

      ## check if the stsmodel is valid
      for(i in seq_len(nrow(stsmodel.df))){
        if (!stsmodel.df[i,2] %in% c("auto", "bsm", "llt", "ll", "noise")){
          stsmodel.df[i,2]<-"auto"
          warning(paste0(stsmodel.df[i,1], ": the stsmodel submitted for this series is not valid. stsmodel = 'auto' was considered instead. Please check your input if you want it otherwise."), call. = FALSE)
        }

        if (frequency(x) == 1){
          if (stsmodel.df[i,2] == "bsm"){
            stsmodel.df[i,2]<-"auto"
            warning(paste0(stsmodel.df[i,1], ": you cannot defined stsmodel = 'bsm' with yearly data. stsmodel = 'auto' was considered instead. Please check your input if you want it otherwise."), call. = FALSE)
          }
        }
      }

      ## check duplicates in series name
      series_names<-stsmodel.df$series_name
      if (anyDuplicated(series_names) == 0L){
        warning("The stsmodel.df contains duplicates. Only the first occurence will be considered.", call. = FALSE)
        stsmodel.df<-stsmodel.df[!duplicated(stsmodel.df$series_name),]
        series_names<-stsmodel.df$series_name
      }
      ## series name in stsmodel.df but not in the data
      for(s in series_names){
        if (!s %in% colnames(x)){
          stop(paste0("The series name ", s, " is mentionned in stsmodel.df but it is not found in the data. Please check your input."))
        }
      }
      ## series name in the data but not in stsmodel.df
      x_series_names<-colnames(x)[-1]
      for(s in x_series_names){
        if (!s %in% series_names){
          stsmodel.df<-rbind(stsmodel.df, c(s, "auto"))
          warning(paste0("The series name ", s, " is in the data but it is not mentionned in stsmodel.df. An 'auto' model was defined by default for this series."), call. = FALSE)
        }
      }
    } else {
      stop("For stsmodel.df, if stsmodel='mixed', you must provide a data.frame with two columns called 'series_name' and 'stsmodel'.")
    }
  }
  return(stsmodel.df)
}

# Handle outliers data to create a list of IV variables (or 'auto')
handle_outliers_data <- function(x, outliers.data){

  if (is.vector(outliers.data)){
    if (is.null(ncol(x))){
      n<-1
    }
  } else {
    if (!is.data.frame(outliers.data)){
      stop("Since you mentionned outliers = 'userdefined', you must provide the outliers as a data.frame in 'outliers.data'.
         The outliers should contain two columns: the first one called 'series_name' and the second one called 'outliers'.
         The outliers should be mentionned in a specific format (e.g. AO_2020Q2, AO_2020M4) or they can be defined as 'auto'.")
    } else {
      if (all(colnames(outliers.data) %in% c("series_name", "outliers"))){
        outliers.data<-data.frame(lapply(outliers.data, as.character), stringsAsFactors=FALSE)
        series_names<-unique(outliers.data$series_name)

        if (is.mts(x)){
          ## series name in outliers.data but not in the data
          for(s in series_names){
            if (!s %in% colnames(x)){
              stop(paste0("The series name ", s, " is mentionned in outliers.data but it is not found in the data. Please check your input."))
            }
          }
          ## series name in the data but not in outliers.data
          x_series_names<-colnames(x)[-1]
          for(s in x_series_names){
            if (!s %in% series_names){
              outliers.data<-rbind(outliers.data, c(s, "auto"))
              warning(paste0("The series name ", s, " is in the data but it is not mentionned in outliers.data. An outliers='auto' was defined by default for this series."), call. = FALSE)
            }
          }
        }
      } else {
        stop("For outliers.data, if outliers='userdefined', you must provide a data.frame with two columns called 'series_name' and 'outliers'.")
      }
    }
    n<-length(series_names)
  }

  iv_list<-list() # initialization output

  for(j in 1:n){

    if (is.data.frame(outliers.data)){
      series_name<-series_names[j]
      if (is.mts(x)) xj<-x[,series_name] else xj<-x
      outliers<-as.character(outliers.data[outliers.data$series_name==series_name,]$outliers)
    } else {
      series_name<-fifelse(is.null(names(x)), "iv", "names(x)")
      xj<-x
      outliers<-outliers.data
    }

    if (any(outliers == "auto")){
      iv_list[[j]]<-"auto"
      names(iv_list)[j] <- fifelse(n > 1, series_name, "series")
    } else {
      type<-substr(outliers,1,2)

      for(t in type){
        if (!t %in% c('AO','LS','TC','SO')){
          stop(paste0(t, " is not a proper type of outliers. Only AO, LS, TC and SO are handled."))
        }
      }

      yr<-substr(outliers,4,7)
      per<-substr(outliers,9,10)

      for(i in seq_along(yr)){
        yri<-as.numeric(yr[i])
        peri<-as.numeric(per[i])

        if (is.na(yri)){stop(paste0(yri, " is not a proper year in outliers definition."))}
        if (is.na(peri)){stop(paste0(peri, " is not a proper quarter or month in outliers definition."))}

        if (yri<start(x)[1] || yri>end(x)[1] || (yri==start(x)[1] && peri<start(x)[2]) || (yri==end(x)[1] && peri>end(x)[2])) {
          stop(paste0("The outlier ", outliers[i], " is outside the range of the series."))
        }
      }

      freq_outliers<-substr(outliers,8,8)
      if (frequency(x)==4){
        if (!all(freq_outliers=="Q")){stop("Frequency declared in outliers description does not match the actual frequency of the series.")}
      } else if (frequency(x)==12){
        if (!all(freq_outliers=="M")){stop("Frequency declared in outliers description does not match the actual frequency of the series.")}
      }

      for(i in seq_along(outliers)){

        outlier_decimal<-decimal_period(as.numeric(yr[i]),as.numeric(per[i]),frequency(x))
        pos<-which(time(x) == outlier_decimal)

        if (type[i]=='AO'){
          iv<-ao_variable(s=xj, pos=pos)
        } else if (type[i]=='LS'){
          iv<-ls_variable(s=xj, pos=pos)
        } else if (type[i]=='TC'){
          iv<-tc_variable(s=xj, pos=pos)
        } else if (type[i]=='SO'){
          iv<-so_variable(s=xj, pos=pos)
        }

        if (i==1){
          ivs<-matrix(iv,ncol=1)
        } else {
          ivs<-cbind(ivs,iv)
        }
      }
      ivs<-ts(ivs, start = start(x), frequency = frequency(x))
      colnames(ivs)<-outliers
      iv_list[[j]]<-ivs
      names(iv_list)[j] <- fifelse(n > 1, series_name, "series")
    }
  }
  return(iv_list)
}

# Check that 'cal.effect.df' is filled in properly when cal.effect = "mixed"
handle_cal_effect_df <- function(x, cal.effect.df){
  if (!is.data.frame(cal.effect.df)){
    stop("Since you mentionned cal.effect = 'mixed', you must provide the way to handle calendar effect for each series as a data.frame in 'cal.effect.df'.
         The cal.effect.df should contain four columns called 'series_name', 'cal.effect', 'cal.effect.td' and 'cal.effect.easter'.
         The three last columns must constitute valid options for each series, taken in the list of authorized options in 'cal.effect','cal.effect.td' and 'cal.effect.easter'.")
  } else {
    if (all(colnames(cal.effect.df) %in% c("series_name", "cal.effect", "cal.effect.td", "cal.effect.easter"))){
      ## convert columns to character
      cal.effect.df<-data.frame(lapply(cal.effect.df, as.character), stringsAsFactors=FALSE)

      ## check column content
      if (!all(cal.effect.df$cal.effect.td %in% c("Default", "WesternEU", "BE", "none"))) stop("Invalid calendar(s) in 'cal.effect.df'. Please check your input.")
      if (anyNA((as.logical(cal.effect.df$cal.effect.easter)))) stop("Invalid argument in 'cal.effect.df' for the easter effect. Must be TRUE or FALSE for each series. Please check your input.")

      ## check duplicates in series name
      series_names<-cal.effect.df$series_name
      if (anyDuplicated(series_names) == 0L) {
        warning("The cal.effect.df contains duplicates. Only the first occurence will be considered.", call. = FALSE)
        cal.effect.df<-cal.effect.df[!duplicated(cal.effect.df$series_name),]
        series_names<-cal.effect.df$series_name
      }
      ## series name in cal.effect.df but not in the data
      for(s in series_names){
        if (!s %in% colnames(x)){
          stop(paste0("The series name ", s, " is mentionned in cal.effect.df but it is not found in the data. Please check your input."))
        }
      }
      ## series name in the data but not in cal.effect.df
      x_series_names<-colnames(x)[-1]
      for(s in x_series_names){
        if (!s %in% series_names){
          cal.effect.df<-rbind(cal.effect.df, c(s, "auto", "Default", TRUE))
          warning(paste0("The series name ", s, " is in the data but it is not mentionned in cal.effect.df. An automatic calendar detection was processed by default for this series."), call. = FALSE)
        }
      }
    } else {
      stop("For cal.effect.df, if cal.effect='mixed', you must provide a data.frame with four columns called 'series_name', 'cal.effect', cal.effect.td' and 'cal.effect.easter'")
    }
  }
  return(cal.effect.df)
}

# Return period in decimal
decimal_period <- function(year, quarter_month, freq){
  as.numeric(time(ts(start = c(year, quarter_month), frequency = freq)))
}

# Check calendar effect are valid
check_cal.effect <- function(x, cal.effect){
  if (frequency(x) == 1){
    if (cal.effect == "forced"){
      cal.effect<-"none"
      warning("You cannot defined cal.effect = 'forced' with yearly data. cal.effect='none' was considered instead.", call. = FALSE)
    }
  }
  return(cal.effect)
}

# Create time group variable for cumulative sum
create_time_gvar <- function(x, freq){
  yr<-floor(time(x))

  if (freq==1){
    gvar<-yr
  } else if (freq==4){
    date_decimal<-time(x) %% 1
    q_char<-fcase(
      date_decimal<0.25,"Q1",
      date_decimal<0.5,"Q2",
      date_decimal<0.75,"Q3",
      date_decimal<=1,"Q4"
    )
    gvar<-paste0(yr,q_char)
  } else {
    stop("Frequency of low frequency data > 4 not yet handled. Contact developer if needed.")
  }
  return(gvar)
}

# Moving average of time series (source: package forecast)
ma <- function(x, order, centre = TRUE){
  if (abs(order - round(order)) > 1e-08) {
    stop("order must be an integer")
  }
  if (order%%2 == 0 && centre) {
    w <- c(0.5, rep(1, order - 1), 0.5)/order
  }
  else {
    w <- rep(1, order)/order
  }
  return(filter(x, w))
}

# Determine whether there is seasonality in a series
is_seasonal <- function(x){

  if (frequency(x)>1){

    x_diff<-diff(x)
    qs_pval<-try(seasonality_qs(x_diff,frequency(x))$pvalue, silent=TRUE) # Ljung-Box
    f_pval<-try(seasonality_f(x_diff, frequency(x))$pvalue, silent=TRUE) # F-test on seasonal dummies
    friedman_pval<-try(seasonality_friedman(x_diff,frequency(x))$pvalue, silent=TRUE) # Friedman non-parametric test

    test_succeeded<-c(!inherits(qs_pval, "try-error"), !inherits(f_pval, "try-error"), !inherits(friedman_pval, "try-error"))
    if (all(test_succeeded)){
      pvals<-c(qs_pval, f_pval, friedman_pval)
      include_seasonality<-fifelse(length(pvals[which(pvals < .05)])>=2, TRUE, FALSE)
    } else if (any(test_succeeded)){
      if (test_succeeded[1]){
        include_seasonality<-fifelse(qs_pval<.01,TRUE,FALSE)
      } else if (test_succeeded[2]){
        include_seasonality<-fifelse(f_pval<.01,TRUE,FALSE)
      } else if (test_succeeded[3]){
        include_seasonality<-fifelse(friedman_pval<.01,TRUE,FALSE)
      }
    } else {
      include_seasonality<-FALSE
    }
  } else {
    include_seasonality<-FALSE
  }
  return(include_seasonality)
}

# Calendar variables
create_htdreg <- function(x, type = c("Default", "WesternEU", "BE")){

  if (type == "Default"){
    x_calendar<-national_calendar(list())
  } else if (type == "BE"){
    x_calendar<-national_calendar(list(
      special_day("NEWYEAR"),
      special_day("EASTERMONDAY"),
      special_day("MAYDAY"),
      special_day("ASCENSION"),
      special_day("WHITMONDAY"),
      fixed_day(7, 21),
      special_day("ASSUMPTION"),
      special_day("ALLSAINTSDAY"),
      special_day("ARMISTICE"),
      special_day("CHRISTMAS")))
  } else if (type == "WesternEU"){
    x_calendar<-national_calendar(list(
      special_day("NEWYEAR"),
      special_day("EASTERMONDAY"),
      special_day("MAYDAY"),
      special_day("ASCENSION"),
      special_day("WHITMONDAY"),
      special_day("ASSUMPTION"),
      special_day("ALLSAINTSDAY"),
      special_day("CHRISTMAS")))
  }

  htdreg<-calendar_td(x_calendar,s=x)
  colnames(htdreg)<-c("Mon","Tue","Wed","Thu","Fri","Sat")

  return(htdreg)
}

# Easter variable
create_easterreg <- function(x){
  easter_mat<-matrix(easter_variable(s=x),ncol=1)
  colnames(easter_mat)<-"easter"
  return(ts(easter_mat,start=start(x), frequency=frequency(x)))
}

# Test easter effect
easter_f<-function(x, easter.reg, regressors){

  regall<-cbind(easter.reg,regressors)

  model<-model()
  add(model, locallineartrend("trend"))
  add(model, seasonal("seas", frequency(x)))
  add(model, noise("noise"))
  add(model, reg("reg", regall))
  eq<-equation("eq")
  add_equation(eq, "trend")
  add_equation(eq, "seas")
  add_equation(eq, "noise")
  add_equation(eq, "reg")
  add(model, eq)
  res<-estimate(model, x)

  if (frequency(x) == 4){
    pos_easter<-7
  } else if (frequency(x) == 12){
    pos_easter<-15
  }

  easter_est<-smoothed_states(res)[1,pos_easter]
  easter_est_sd<-smoothed_states_stdev(res)[1,pos_easter]

  t<-easter_est/easter_est_sd

  n<-length(x)
  k<-length(result(res,"parameters"))
  pval<-pt(abs(t), df = n-k+1,lower.tail = FALSE)*2 # two-tailed test

  return(list(value = t, pvalue = pval))
}

# Run structural model by State Space
sts.run <- function(x, stsmodel = c("bsm", "llt", "ll", "noise"), cumulator = FALSE, cumulator.ratio = NA, regressors = NULL){

  # 1. Create the model and the equation
  model<-model()
  eq<-equation("eq")

  # 2. Create the components
  noise<-noise("noise")
  if (stsmodel == 'bsm'){
    trend<-locallineartrend("trend")
    seas<-seasonal("seas", frequency(x))
  } else if (stsmodel == 'llt'){
    trend<-locallineartrend("trend")
  } else if (stsmodel == 'll'){
    trend<-locallevel("ll")
  }
  if (!is.null(regressors)){
    reg<-reg("iv", regressors)
  }

  # 3. Add the components to the model and create the equation
  if (cumulator){
    if (stsmodel == 'bsm'){
      if (!is.null(regressors)){
        all<-aggregation("m", list(trend, seas, noise, reg))
      } else {
        all<-aggregation("m", list(trend, seas, noise))
      }
    } else if (stsmodel == 'llt'){
      if (!is.null(regressors)){
        all<-aggregation("m", list(trend, noise, reg))
      } else {
        all<-aggregation("m", list(trend, noise))
      }
    } else if (stsmodel == 'll'){
      if (!is.null(regressors)){
        all<-aggregation("m", list(trend, noise, reg))
      } else {
        all<-aggregation("m", list(trend, noise))
      }
    } else if (stsmodel == 'noise'){
      if (!is.null(regressors)){
        all<-aggregation("m", list(noise, reg))
      } else {
        all<-noise
      }
    }
    c<-cumul("c", all, period = cumulator.ratio)
    add(model, c)
    add_equation(eq, "c")

  } else {
    if (stsmodel == 'bsm'){
      add(model, trend)
      add(model, seas)
      add(model, noise)
      add_equation(eq, "trend")
      add_equation(eq, "seas")
      add_equation(eq, "noise")
      if (!is.null(regressors)){
        add(model, reg)
        add_equation(eq, "iv")
      }
    } else if (stsmodel == 'llt'){
      add(model, trend)
      add(model, noise)
      add_equation(eq, "trend")
      add_equation(eq, "noise")
      if (!is.null(regressors)){
        add(model, reg)
        add_equation(eq, "iv")
      }
    } else if (stsmodel == 'll'){
      add(model, trend)
      add(model, noise)
      add_equation(eq, "ll")
      add_equation(eq, "noise")
      if (!is.null(regressors)){
        add(model, reg)
        add_equation(eq, "iv")
      }
    } else if (stsmodel == 'noise'){
      add(model, noise)
      add_equation(eq, "noise")
      if (!is.null(regressors)){
        add(model, reg)
        add_equation(eq, "iv")
      }
    }
  }

  # 4. Estimate the model
  add(model, eq)
  res<-estimate(model, x, marginal = TRUE)

  # 5. Output

  ## 5.1. Main results
  table_est<-get_table_est(res, stsmodel, cumulator, regressors, x)
  regressors_est<-get_regressors_est(res, stsmodel, cumulator, regressors)

  ## 5.2 Parameters
  sf<-result(res, "scalingfactor")
  param<-result(res, "parameters") * sf
  names(param)<-result(res, "parametersnames")

  ## 5.3. Residuals & diagnostics
  vtilt<-result(res,"likelihood.residuals")
  vtilt_statistics <- perform_diagnostic_tests(vtilt, frequency = frequency(x))
  residuals_out<-list(residuals = vtilt, statistics = vtilt_statistics)

  ## 5.4. Likelihood
  likelihood<-list(ll=result(res, "likelihood.ll"), ser=result(res,"likelihood.ser"))

  ## 5.5. Variance of the slope component (used for test of specification)
  vslope<-getvslope(res, stsmodel)

  output<-list(table=table_est, regressors=regressors_est, hparam=param, residuals=residuals_out, likelihood=likelihood, stsmodel=stsmodel, vslope=vslope)

  return(output)
}

# Get output related to the different components of the series
get_table_est <- function(results, stsmodel, cumulator, regressors, x){

  ## Estimates of smoothed states
  smoothed_states<-data.frame(result(results,"ssf.smoothing.states"))

  ## Number of regressors and their names
  if (!is.null(regressors)){
    reg_names<-colnames(regressors)
    nreg<-length(reg_names)
  }

  ## Extract components

  ### Component's estimates without regressors

  ### NOTE: in smoothed_states with cumulator (without cumulator, there is no cumulated results and all the other columns are shifted left):
  ###       BSM: V1=cumulated resuls, V2=trend, V3=slope, V4-V6=seasonal (Q) V4-V14 (M), V7=irregular V15 (M), V8+=regressors V16+ (M)
  ###       Local Linear Trend: V1=cumulated resuls, V2=trend, V3=slope, V4=irregular, V5+=regressors
  ###       Local Level: V1=cumulated resuls, V2=trend, V3=irregular, V4+=regressors
  ###       Noise: V1=cumulated resuls, V2=irregular, V3+=regressor

  if (stsmodel == "bsm"){
    pos_trend<-fifelse(cumulator,2,1)
    pos_slope<-pos_trend+1
    pos_seasonal<-pos_trend+2

    trend<-smoothed_states[,pos_trend]
    slope<-smoothed_states[,pos_slope]
    seasonal<-smoothed_states[,pos_seasonal]

    if (frequency(x) == 4){
      pos_irregular<-pos_trend+5
    } else if (frequency(x) == 12){
      pos_irregular<-pos_trend+13
    }
    irregular<-smoothed_states[,pos_irregular]
  } else if (stsmodel == "llt"){
    pos_trend<-fifelse(cumulator,2,1)
    pos_slope<-pos_trend+1

    trend<-smoothed_states[,pos_trend]
    slope<-smoothed_states[,pos_slope]
    seasonal<-numeric(nrow(smoothed_states))

    pos_irregular<-pos_trend+2
    irregular<-smoothed_states[,pos_irregular]
  } else if (stsmodel == "ll"){
    pos_trend<-fifelse(cumulator,2,1)

    trend<-smoothed_states[,pos_trend]
    slope<-numeric(nrow(smoothed_states))
    seasonal<-numeric(nrow(smoothed_states))

    pos_irregular<-pos_trend+1
    irregular<-smoothed_states[,pos_irregular]
  } else if (stsmodel == "noise"){
    trend<-numeric(nrow(smoothed_states))
    slope<-numeric(nrow(smoothed_states))
    seasonal<-numeric(nrow(smoothed_states))

    pos_irregular<-fifelse(cumulator,2,1)
    irregular<-smoothed_states[,pos_irregular]
  }

  ### Add regressors effect if any
  if (!is.null(regressors)){
    smoothed_states_ext<-cbind(smoothed_states,as.data.frame(regressors))

    for (i in 1:nreg){
      ### regression type
      if (toupper(substr(reg_names[i],1,2)) %in% c("AO","LS","TC","SO")){
        reg_type<-toupper(substr(reg_names[i],1,2))
      } else if (substr(reg_names[i],1,3) == "CAL"){
        reg_type<-"CAL"
      } else {
        reg_type<-"undefined"
      }

      # regression effect
      regi<-smoothed_states_ext[,pos_irregular+i]*smoothed_states_ext[,pos_irregular+i+nreg]

      # add regression effect to the right component
      if (reg_type %in% c("LS","TC")){
        trend <- trend + regi
      } else if (reg_type %in% c("CAL","SO")){
        seasonal <- seasonal + regi
      } else if (reg_type %in% c("AO","undefined")){
        irregular <- irregular + regi
      }
    }
  }

  ## Final table
  series <- trend + seasonal + irregular
  table_mat <- cbind(series, trend, slope, seasonal, irregular)
  table_ts <- ts(table_mat, start = start(x), frequency = frequency(x))
  colnames(table_ts)<-c("Series", "Trend", "Slope", "Seasonal", "Irregular")

  return(table_ts)
}

# Get output related to regressors
get_regressors_est <- function(results, stsmodel, cumulator, regressors){

  if (!is.null(regressors)){
    ## smoothed states estimates and their variance
    smoothing_states_est<-result(results,"ssf.smoothing.states")
    smoothing_vstates_est<-result(results,"ssf.smoothing.vstates")

    ## positions of regressors in the states
    if (stsmodel == "bsm"){
      if (frequency(regressors) == 4){
        fc_cumulator<- 8
      } else if (frequency(regressors) == 12){
        fc_cumulator<- 16
      }
    }

    switch(stsmodel,
           "bsm" = {fc <- fifelse(cumulator,fc_cumulator,fc_cumulator-1)},
           "llt" = {fc <- fifelse(cumulator,5,4)},
           "ll" = {fc <- fifelse(cumulator,4,3)},
           "noise" = {fc <- fifelse(cumulator,3,2)})

    lc<-ncol(smoothing_states_est)

    ## output
    b_reg<-smoothing_states_est[1,fc:lc]
    b_vreg<-smoothing_vstates_est[1,fc:lc]
    t<-b_reg/sqrt(b_vreg)
    names(b_reg)<-names(b_vreg)<-names(t)<-colnames(regressors)

    regressors_est <- list(regressors = regressors, param = b_reg, vparam = b_vreg, tstat = t)
  } else {
    regressors_est <- NULL
  }

  return(regressors_est)
}

# Perform diagnostic tests
perform_diagnostic_tests <- function(x, frequency){

  # Normality
  dh<-doornikhansen(x)
  pval_norm<-get_pvalue_test(list(doornik_hansen=dh))

  # Autocorrelation
  lj<-ljungbox(x, k=frequency*2, mean=FALSE)
  pval_ac<-get_pvalue_test(list(lj))
  rownames(pval_ac)<-paste0("ljungbox_residuals_", frequency*2)

  # Randomness
  runs<-testofruns(x)
  udr<-testofupdownruns(x)
  pval_rand<-get_pvalue_test(list(runs_around_mean=runs, up_and_down_runs=udr))

  # Linearity
  ljsq<-ljungbox(x^2, k=frequency*2)
  pval_lin<-get_pvalue_test(list(ljsq))
  rownames(pval_lin)<-paste0("ljungbox_squaredresiduals_", frequency*2)

  return(list(normality = pval_norm, autocorrelations = pval_ac, randomness = pval_rand, linearity = pval_lin))
}

# Get p-value and test name from diagnostic test in rjd3toolkit
get_pvalue_test <- function(tests_results){
  matrix(sapply(tests_results, function(x) x$pvalue),
         nrow = length(tests_results),
         ncol = 1,
         dimnames = list(names(tests_results), "P-Value"))
}


# Get output related to the variance of the slope
getvslope <- function(results, stsmodel){
  if (stsmodel %in% c("bsm", "llt")){
    smoothing_vstates_est<-result(results,"ssf.smoothing.vstates")
    lr<-nrow(smoothing_vstates_est)
    vslope<-as.numeric(smoothing_vstates_est[lr,3])
  } else {
    vslope<-NULL
  }
  return(vslope)
}

# Check if there are non-significant IV variable(s)
is_nonsignificant_iv <- function(results){
  t<-results$regressors$tstat
  t_outliers<-t[substr(names(t),1,2) %in% c("AO","LS","TC","SO")]
  t_outliers_ns<-t_outliers[abs(t_outliers)<1.96]

  include_nonsignificant_iv<-fifelse(length(t_outliers_ns) > 0, TRUE, FALSE)

  return(include_nonsignificant_iv)
}

# Remove non-significant instrumental variables recursively
clean_iv <- function(x, stsmodel, cumulator, cumulator.ratio, regressors, t_reg){
  repeat{
    t_iv<-t_reg[substr(names(t_reg),1,2) %in% c("AO","LS","TC","SO")]
    t_iv_ns<-t_iv[abs(t_iv)<1.96]
    if (length(t_iv_ns) == 0){
      regressors_cleaned<-regressors
      break
    } else {
      regressors_cleaned<-regressors[,!colnames(regressors) %in% names(t_iv_ns), drop = FALSE]
      if (length(regressors_cleaned) == 0){
        regressors_cleaned<-NULL
        break
      } else {
        res<-try(sts.run(x, stsmodel, cumulator, cumulator.ratio = cumulator.ratio, regressors = regressors_cleaned), silent=TRUE)
        if (!inherits(res, "try-error")) {
          regressors<-res$regressors$regressors
          t_reg<-res$regressors$tstat
        } else {
          regressors_cleaned<-regressors
          break
        }
      }
    }
  }
  return(regressors_cleaned)
}

# Determine whether there should be a slope component in the trend of a seasonality adjusted series
is_slope <- function(x, cumulator, cumulator.ratio, regressors){

  res_llt<-try(sts.run(x, stsmodel = "llt", cumulator = cumulator, cumulator.ratio = cumulator.ratio, regressors = regressors), silent=TRUE)

  if (!inherits(res_llt, "try-error")) {
    constant_slope<-fifelse(var(res_llt$table[,"Slope"]) == 0,TRUE,FALSE)
    if (constant_slope){
      slope_est<-as.numeric(res_llt$table[1,"Slope"])
      t<-slope_est/sqrt(res_llt$vslope)
      include_slope<-fifelse(abs(t)>1.96,TRUE,FALSE)
    } else {
      res_ll<-try(sts.run(x, stsmodel = "ll", cumulator = cumulator, cumulator.ratio = cumulator.ratio, regressors = regressors), silent=TRUE)
      if (!inherits(res_ll, "try-error")) {
        ser_llt<-res_llt$likelihood$ser
        ser_ll<-res_ll$likelihood$ser
        include_slope<-fifelse(ser_llt<ser_ll,TRUE,FALSE)
      } else {
        include_slope<-TRUE
      }
    }
  } else {
    include_slope<-FALSE
  }

  return(include_slope)
}

# Determine whether there should be a trend (and a slope component) in a seasonality adjusted series
is_trend <- function(x, cumulator, cumulator.ratio, regressors){

    res_llt<-try(sts.run(x, stsmodel = "llt", cumulator = cumulator, cumulator.ratio = cumulator.ratio, regressors = regressors), silent=TRUE)
    res_ll<-try(sts.run(x, stsmodel = "ll", cumulator = cumulator, cumulator.ratio = cumulator.ratio, regressors = regressors), silent=TRUE)
    res_noise<-try(sts.run(x, stsmodel = "noise", cumulator = cumulator, cumulator.ratio = cumulator.ratio, regressors = regressors), silent=TRUE)

    test_succeeded<-c(!inherits(res_llt, "try-error"), !inherits(res_ll, "try-error"), !inherits(res_noise, "try-error"))
    if (all(test_succeeded)){
        ser_llt<-res_llt$likelihood$ser
        ser_ll<-res_ll$likelihood$ser
        ser_noise<-res_noise$likelihood$ser
        include_trend<-fifelse(ser_noise > ser_llt | ser_noise > ser_ll, TRUE, FALSE)
    } else if (any(test_succeeded)){
        if (test_succeeded[1] && !test_succeeded[2] && test_succeeded[3]){
            ser_llt<-res_llt$likelihood$ser
            ser_noise<-res_noise$likelihood$ser
            include_trend<-fifelse(ser_noise > ser_llt, TRUE, FALSE)
        } else if (test_succeeded[1] && !test_succeeded[2] && test_succeeded[3]){
            ser_ll<-res_ll$likelihood$ser
            ser_noise<-res_noise$likelihood$ser
            include_trend<-fifelse(ser_noise > ser_ll, TRUE, FALSE)
        } else if (!test_succeeded[1] && !test_succeeded[2] && test_succeeded[3]){
            include_trend<-FALSE
        } else {
            include_trend<-TRUE
        }
    } else {
        include_trend<-TRUE
    }
    return(include_trend)
}
