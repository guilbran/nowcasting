#' @title Pseudo Real time data base
#' @description Create a pseudo real time data base, based on delays estipulates by the users, and data inputed by the
#' @param mts A matrix time series - \code{mts} - with the series to be transformed
#' @param delay A numeric vector with the delay in days the information is available after the reference month. Each element correspond to the series in the respective column in \code{mts}. 
#' @param today The day where the data is supposed to be collected.
#' @return A \code{mts} with the series transformed.
#' @import zoo lubridate
#' @export

PRTDB<-function(mts,delay,today = Sys.Date()){
  mts_new <- mts
  
  # define the last day of the month
  month_end<-as.Date(mts)+months(1)-days(1)
  
  # create list with release date
  release<-lapply(1:length(delay),function(x) month_end+days(delay)[x])
  
  # Eliminate information not available until the day
      for (i in 1:length(delay)){
      mts_new[release[[i]]>today,i] <- NA
      }
  
  return(mts_new)
}