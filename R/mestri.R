#' @title Monthly to quarterly agregation
#' @description It transforsms a monthly time series in a quarterly.
#' @param x Variable in month frequency
#' @return The correpondent quarterly transformation or agregation.


# função que pega uma série mensal e transforma em trimestral selecionando o 
# último mês de cada trimestre como o valor do trimestre

mestri <- function(x){
  data <- as.Date(x)
  ano_inicial <- as.numeric(substr(data[1],1,4))
  meses <- substr(data,6,7)
  ultimo_tri <- meses %in% c("03","06","09","12")
  tri <- which(c("03","06","09","12") == meses[which(ultimo_tri)[1]])
  # x.tri <- ts(x[ultimo_tri,], start = c(ano_inicial,tri), freq = 4)
  x.tri <- ts(data.frame(x)[ultimo_tri,], start = c(ano_inicial,tri), frequency = 4)
  x.tri
}