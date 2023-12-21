
# Main class

#' Print function for objects of class 'nbb.estimateSTS.output'
#'
#' @param x an object of class 'nbb.estimateSTS.output'
#' @param \dots further arguments passed to or from other methods.
#' @export
#'
print.nbb.estimateSTS.output <- function(x, ...){

  series_attr<-names(x)[!names(x) %in% c("call", "x", "x.lf", "x.imputed")]

  if(length(series_attr) == 1){
    print.default(x[[1]][[3]], ...)
  }

  cat("Call:\n")
  print.default(x$call, ...)

  cat("\nComposition of the first-level output:\n")
  print.default(names(x), ...)

  cat("\nUse summary() for more details.")
}

#' Summary function for objects of class 'nbb.estimateSTS.output'
#'
#' @param x an object of class 'nbb.estimateSTS.output'
#' @param \dots further arguments passed to or from other methods.
#' @export
#'
summary.nbb.estimateSTS.output<- function(x, ...){

  cat("Call:\n")
  print.default(x$call, ...)

  nx<-fifelse(is.null(ncol(x$x)), 1, ncol(x$x))
  if(!is.null(x$x.lf)){
    nxl<-fifelse(is.null(ncol(x$x.lf)), 1, ncol(x$x.lf))
  }else{
    nxl<-0
  }
  cat(paste0("\nNumber of series: ", nx))
  cat(paste0("\nNumber of series incl. low frequency input: ", nxl, "\n"))
  cat(paste0("\nPeriod covered: from ", paste0(start(x$x)[1],"-",start(x$x)[2]), " to ", paste0(end(x$x)[1],"-",end(x$x)[2]), "\n"))

  # models
  cat("\nModel:\n")
  if(nxl > 0){
    cat("Imputation using State Space models with cumulator (when low frequency data were submitted) \n")
  } else{
    cat("Imputation using State Space models \n")
  }

  cat("\nComposition of the first-level output:\n")
  print.default(names(x), ...)
}


#' Plot function for objects of class 'nbb.estimateSTS.output'
#'
#' @param x an object of class 'nbb.estimateSTS.output'
#' @param series_name a character object with the name of the series
#' @param \dots further arguments passed to or from other methods.
#' @export
#'
plot.nbb.estimateSTS.output <- function(x, series_name = "", ...){

  if(series_name == ""){
    series_name <- names(x)[1]

    if(length(names(x)[!names(x) %in% c("call", "x", "x.lf", "x.imputed")]) > 1){
      cat("The first series is plot by default. Change 'series_name' argument to plot results for another series.")
    }
  }else if (!series_name %in% colnames(x$x)){
    stop("Series name not found in the input data!")
  }

  if(!is.null(colnames(x$x))){
    x_init<-x$x[,series_name]
  }else{
    x_init<-x$x
  }

  ts.plot(x[[series_name]]$table[,"Series"], gpars=list(main = series_name, xlab="", ylab="", pch=19, cex=0.8, las=2, col = "red"))
  lines(x_init, col="black")
  legend("topleft", legend=c("Initial", "Imputed"),col=c("black", "red"), lty=1, cex=0.8)
}
