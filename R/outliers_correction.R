#' @title Outliers Correction
#' @description Essa função opera em dois estágios. Inicialmente substitui os missings e outliers pela mediana da série.
#' Os outliers são definidos como as observações que distão da mediana mais de 4 vezes o IQR. No segundo estágio é feita uma média móvel de ordem \code{k_ma}, os valores iniciais de missings e outliers são substituídos pelo resultado dessa média móvel.
#' @param x. Um objeto \code{ts} que se deseja aplicar a transformação de substituição de outliers e missings.
#' @param k_ma. Ordem da média móvel utilizada para substituição das variáveis
#' @import matlab

outliers_correction <- function(x, k_ma = 3){
  # x é um série temporal
  
  # encontrar missings
  missing <- is.na(x)
  
  # outlier são as obs que ultrapassam 4* distância interquartilica
  outlier <- abs(x - median(x, na.rm = T)) > 4*abs(quantile(x, probs = 0.25, na.rm = T) -  quantile(x, probs = 0.75, na.rm = T)) & !missing
  
  Z <- x

  # substituir outliers e missings pela mediana
  Z[outlier] <- median(x, na.rm = T)
  Z[missing] <- median(x, na.rm = T)
  
  # Média móvel de ordem K
  xpad <- c(Z[1]*ones(k_ma,1), Z, Z[length(Z)]*ones(k_ma,1))
  x_ma <- xpad*NA
  for(j in (k_ma + 1):(length(xpad) - k_ma)){
    x_ma[j - k_ma] = mean(xpad[(j - k_ma):(j + k_ma)])
  }
  x_ma <- x_ma[1:length(x)]
  
  Z[outlier] <- x_ma[outlier]
  Z[missing] <- x_ma[missing]
  
  Z
}