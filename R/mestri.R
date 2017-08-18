#' @title Monthly to quarterly agregation
#' @description It transforsms a monthly time series in a quarterly.
#' @param x Variable in month frequency
#' @return The correpondent quarterly transformation or agregation.
#' @examples 
#' # GDP index at market prices at quarterly frequency
#' pib<-mestri(lag(base_extraction(22099),-2))
#' 
#' # Industrial production selecting only last month
#' prod_ind<-mestri(base_extraction(21859))
#' 
#' # Selecting only last month of matrix time series vintage:
#' mestri_vintage<-mestri(vintage)
#' 
#' # Selecting only last month of first two columns of matrix time series vintage:
#' mestri_vintage<-mestri(vintage[,1:2])
#' 
#' @import zoo
#' @export

# função que pega uma série mensal e transforma em trimestral selecionando o 
# último mês de cada trimestre como o valor do trimestre

mestri <- function(x){
  data <- zoo::as.Date(x)
  ano_inicial <- as.numeric(substr(data[1],1,4))
  meses <- substr(data,6,7)
  ultimo_tri <- meses %in% c("03","06","09","12")
  tri <- which(c("03","06","09","12") == meses[which(ultimo_tri)[1]])
  # x.tri <- ts(x[ultimo_tri,], start = c(ano_inicial,tri), freq = 4)
  x.tri <- ts(data.frame(x)[ultimo_tri,], start = c(ano_inicial,tri), frequency = 4)
  x.tri
}