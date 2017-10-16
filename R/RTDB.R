#' @title Re-create Real Time Data Base
#' @description Create a time series matrix \code{mts} replicating the information available in a given date.
#' @param series_code Vector with the series encoding follow the Bacen (Banco Central do Brasil) standards.
#' @param vintage The vintage encoded by the day of the extraction
#' @import DBI RMySQL
#' @examples
#' # Extracting GDP serie at real-time from Central Bank of Brasil data base
#' gdp<-base_extraction(22099)
#' @references Central Bank of Brazil
#' @export

RTDB<-function(series_code,vintage){

  # library(DBI)
  # library(RMySQL)

  # series_code<-c(1,12)
  # vintage<-Sys.Date()
  # v_ind<-as.character(vintage)

  # SQL<-paste("SELECT * FROM dbvintage WHERE vintage_cod =",paste0("\'",v_ind,"\'"))
  
  SQL<-paste("SELECT", paste0('X,',paste0("serie",series_code,collapse = ',')) ,"FROM dbvintage WHERE vintage_cod =",paste0("\'",v_ind,"\'"))
  conn = dbConnect(MySQL(),db="pibnow",user="pibnow_user",password="123456",host="200.20.164.178",port=3306)
  dados = DBI::dbGetQuery(conn = conn,statement = SQL)
  DBI::dbDisconnect(conn)

  dados<-ts(dados[,-1],start=as.numeric(c(substr(dados[1,1],1,4),substr(dados[1,1],6,7))),frequency=12)
  
  return(dados)
}



