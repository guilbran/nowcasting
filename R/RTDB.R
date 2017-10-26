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


RTDB<-function(series_code = NULL,vintage = NULL){

  # library(DBI)
  # library(RMySQL)
  # series_code<-666
  # vintage<-as.Date('2017-10-26')
  
  v_ind<-as.character(vintage)

  if(is.null(series_code) & is.null(vintage)){
  SQL<-"SELECT `COLUMN_NAME` FROM `INFORMATION_SCHEMA`.`COLUMNS` WHERE `TABLE_SCHEMA`='pibnow' AND `TABLE_NAME`='dbvintage'"
  conn = dbConnect(MySQL(),db="pibnow",user="pibnow_user",password="123456",host="200.20.164.178",port=3306)
  dados0 <- DBI::dbGetQuery(conn = conn,statement = SQL)
  DBI::dbDisconnect(conn)
  dados1<-data.frame(series_code=substr(dados0[3:(dim(dados0)[1]-1),],6,50))
  return(dados1)
  }else{
    return_try<-tryCatch({
                SQL<-paste("SELECT", paste0('X,',paste0("serie",series_code,collapse = ',')) ,"FROM dbvintage WHERE vintage_cod =",paste0("\'",v_ind,"\'"))
                conn = dbConnect(MySQL(),db="pibnow",user="pibnow_user",password="123456",host="200.20.164.178",port=3306)
                dados0 <- DBI::dbGetQuery(conn = conn,statement = SQL)
                DBI::dbDisconnect(conn)},
                error = function(err) {
                    return(FALSE)
      })
    
    if(return_try==FALSE){
    message('Sorry, this serie(s) is(are) not available.')
    }else{
      if(nrow(dados0)==0){
        SQL<-paste("SELECT vintage_cod FROM dbvintage")
        conn = dbConnect(MySQL(),db="pibnow",user="pibnow_user",password="123456",host="200.20.164.178",port=3306)
        dados0 <- DBI::dbGetQuery(conn = conn,statement = SQL)
        DBI::dbDisconnect(conn)
        
        vintage_cod<-as.Date(unique(dados0)[,1])
        if(as.Date(vintage)<vintage_cod[1]){
          v_ind<-vintage_cod[1]
          SQL<-paste("SELECT", paste0('X,',paste0("serie",series_code,collapse = ',')) ,"FROM dbvintage WHERE vintage_cod =",paste0("\'",v_ind,"\'"))
          conn = dbConnect(MySQL(),db="pibnow",user="pibnow_user",password="123456",host="200.20.164.178",port=3306)
          dados0 <- DBI::dbGetQuery(conn = conn,statement = SQL)
          DBI::dbDisconnect(conn)
          dados1<-ts(dados0[,-1],start=as.numeric(c(substr(dados0[1,1],1,4),substr(dados0[1,1],6,7))),frequency=12)
          message('Sorry, this vintage is not available for this(ese) serie(s) yet :(',
                         paste('\nBut, I return (invisible) the first vintage values:',vintage_cod[1],':)'))
          invisible(dados1)
          
        }else{
        ind_previous<-max(which(vintage_cod<as.Date(vintage)))
        v_ind<-vintage_cod[ind_previous]
        SQL<-paste("SELECT", paste0('X,',paste0("serie",series_code,collapse = ',')) ,"FROM dbvintage WHERE vintage_cod =",paste0("\'",v_ind,"\'"))
        conn = dbConnect(MySQL(),db="pibnow",user="pibnow_user",password="123456",host="200.20.164.178",port=3306)
        dados0 <- DBI::dbGetQuery(conn = conn,statement = SQL)
        DBI::dbDisconnect(conn)
        dados1<-ts(dados0[,-1],start=as.numeric(c(substr(dados0[1,1],1,4),substr(dados0[1,1],6,7))),frequency=12)
        message('Sorry, this vintage is not available for this(ese) serie(s) yet :(',
                       paste('\nBut, I return (invisible) the last vintage available:',vintage_cod[ind_previous],':)'))
        invisible(dados1)
        }
      }else{
          dados1<-ts(dados0[,-1],start=as.numeric(c(substr(dados0[1,1],1,4),substr(dados0[1,1],6,7))),frequency=12)
          return(dados1)
        }
    }
  }
}



