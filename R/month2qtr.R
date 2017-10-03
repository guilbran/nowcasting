#' @title Monthly to quarterly agregation
#' @description It transforsms a monthly time series in a quarterly, selecting the last month to represent the quarter. So its necessary to agregate the original serie before use this function.
#' @param x Variable in month frequency
#' @return The correpondent quarterly transformation or agregation.
#' @examples 
#' # Selecting only last month of matrix time series vintage:
#' mestri_vintage<-month2qtr(vintage)
#' 
#' # Selecting only last month of first two columns of matrix time series vintage:
#' mestri_vintage<-month2qtr(vintage[,1:2])
#' 
#' @import zoo
#' @export

# função que pega uma série mensal e transforma em trimestral selecionando o 
# último mês de cada trimestre como o valor do trimestre

month2qtr <- function(x){
  data <- zoo::as.Date(x)
  ano_inicial <- as.numeric(substr(data[1],1,4))
  meses <- substr(data,6,7)
  ultimo_tri <- meses %in% c("03","06","09","12")
  tri <- which(c("03","06","09","12") == meses[which(ultimo_tri)[1]])
  # x.tri <- ts(x[ultimo_tri,], start = c(ano_inicial,tri), freq = 4)
  x.tri <- ts(data.frame(x)[ultimo_tri,], start = c(ano_inicial,tri), frequency = 4)
  x.tri
}