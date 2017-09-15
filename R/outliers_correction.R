#' @import matlab

outliers_correction <- function(x, k_ma = 3){
  # x é um série temporal
  
  # encontrar missings
  missing <- is.na(x)
  
  # Função criada no R para superar os problemas da função do matlab
  # outlier são as obs que ultrapassam 4* distância interquartilica
  outlier <- abs(x - median(x, na.rm = T)) > 4*abs(quantile(x, probs = 0.25, na.rm = T) -  quantile(x, probs = 0.75, na.rm = T)) & !missing

  ### Problema 2: Usa NA para calcular o tamanho do vetor
  # TT <- length(x)   # problem 1
  # TT <- sum(!is.na(x))   # Solução: utilizar apenas as observações completas
  
  ### Problema 3: utilizar a função round do matlab gera viés
  # round2 = function(x, n) {
  #   posneg = sign(x)
  #   z = abs(x)*10^n
  #   z = z + 0.5
  #   z = trunc(z)
  #   z = z/10^n
  #   z*posneg
  # }
  # outlier <- abs(x - sort(x)[round2(TT*1/2,0)]) > 4*abs(sort(x)[round2(TT*1/4,0)] -  sort(x)[round2(TT*3/4,0)]) & !missing
  # possível solução: utilizar a função round do R que controla pelo viés.
  
  Z <- x

  # substituir outliers e missings pela mediana
  Z[outlier] <- median(x, na.rm = T)
  Z[missing] <- median(x, na.rm = T)
  # Z[outlier] <- sort(x)[round2(TT*1/2,0)]
  # Z[missing] <- sort(x)[round2(TT*1/2,0)]

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