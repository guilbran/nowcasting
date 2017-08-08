#' @param series

transformar <- function(series){#, transf){ #, remove.outlier = T, medias_moveis = F, reducao = F){
  
  # ajuste <- function(x){ 
  #   x <- na.omit(ts(x, start = start(series), freq = 12))
  #   tryCatch(seasonal::seas(x, transform.function = "none", outlier = NULL), error = function(e) NULL)
  # }
  # 
  # estat_qs <- function(x){ 
  #   tryCatch(qs(x)[c("qsori","qsorievadj"),"p-val"], error = function(e) c(NA,NA))
  # }
  # 
  # # verificar sazonalidade
  # teste_sazon_lista <- apply(series, MARGIN = 2, FUN = ajuste)
  # sazon_qs <- data.frame(matrix(NA, ncol = length(teste_sazon_lista), nrow = 2))
  # colnames(sazon_qs) <- names(teste_sazon_lista)
  # 
  # series_ajustadas <- series
  # 
  # for(i in 1:length(teste_sazon_lista)){
  #   sazon_qs[,i] <- tryCatch(qs(teste_sazon_lista[[i]])[c("qsori","qsorievadj"),"p-val"], error = function(e) c(NA,NA))
  # }
  # 
  # sazon_qs <- sazon_qs[,-1]
  # 
  # # séries com sazonalidade
  # 
  # soma <- data.frame(nome = colnames(sazon_qs), soma = colSums(sazon_qs < 0.05))
  # nomes_sazon <- as.character(soma[soma$soma > 0 & !is.na(soma$soma),"nome"])
  # 
  # # substituir as séries com ajuste sazonal
  # for(i in nomes_sazon){
  #   window(series_ajustadas[,i], start = start(teste_sazon_lista[[i]]$series$s11),
  #          end = end(teste_sazon_lista[[i]]$series$s11), freq = 12) <- teste_sazon_lista[[i]]$series$s11
  # }

  # teste de raiz unitária
  teste_raiz_lista <- apply(series, MARGIN = 2, FUN = function(x){BETS.ur_test(na.omit(x))$results})
  teste_raiz <- do.call(rbind.data.frame, teste_raiz_lista)
  teste_raiz[paste0("serie", c(4506,4507,4509,4511,4512,7412,7415,7416,7417,7418,7419,13667,21868)),"rej.H0"] <- "no"
  nomes_unit <- rownames(teste_raiz[teste_raiz$rej.H0 == "no",])
  
  series_estacionarias <- series
  
  for(i in nomes_unit){
    window(series_estacionarias[,i], start = start(diff(series[,i])),
           end = end(diff(series[,i])), freq = 12) <- diff(series[,i])
  }
  
  return(series_estacionarias)

  # nomes_series <- colnames(series)
  # nomes_transf <- colnames(transf)
  # transf <- transf[,colnames(transf) %in% colnames(series)]
  # 
  # # aplicar transformações
  # series0 <- tryCatch(series[,transf == 0], error = function(e) NULL)
  # series1 <- tryCatch((series[,transf == 1]/lag(series[,transf == 1], k = -1) - 1)*100, error = function(e) NULL)
  # #series2 <- diff(series[,transf == 2])
  # series3 <- tryCatch(diff((series[,transf == 3]/lag(series[,transf == 3], k = -12) - 1)*100), error = function(e) NULL)
  # 
  # series_transf <- cbind(series0,series1,series3)
  # colnames(series_transf) <- c(colnames(series[,transf == 0]),
  #                              colnames(series[,transf == 1]),
  #                              #colnames(series[,transf == 2]),
  #                              colnames(series[,transf == 3]))
  # 
  # # arrumar ordem para a ordem inicial e início da janela
  # series_transf <- series_transf[,colnames(series)]#, start = c(1983,2), freq = 12)
  # 
  # # if(medias_moveis){
  # # 
  # #   # filtro 
  # series_filter <- stats::filter(series_transf, c(1,2,3,2,1))
  # #series_filter[,transf == 0] <- series_transf[, transf == 0]
  # colnames(series_filter) <- colnames(series_transf)
  # series_transf <- series_filter
  # # 
  # # }
  # # 
  # # if(reducao){
  # #   # usar somente as séries que tem menos de 1/3 NA
  # #   series_transf <- series_transf[,colSums(is.na(series_transf)) < nrow(series_transf)/3]
  # # }
  # # 
  # # 
  # # if(remove.outlier){
  # #   # correção de outliers/missing (substitui pela mediana)
  # #   base_semoutlier <- base*NA
  # #   for(i in 1:ncol(series_transf)){
  # #     base_semoutlier[,i] <- outliers_correction(base[,i])
  # #   }
  # #   # BASE FINAL
  # #   series_transf[1:(nrow(series_transf) - 12),] <- base_semoutlier[1:(nrow(base_semoutlier) - 12),]
  # # 
  # # }
  # 
  # 
  # return(series_transf)
  # 
}