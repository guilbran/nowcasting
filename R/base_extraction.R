#' @title Extract time series and create a base

#' @description Crio uma matriz de series temporais \code{mts} utilizadas para calcular o fator comum.

#' @param series_code. Vector with the series encoding follow the Bacen (Banco Central do Brasil) standards.
#' @import xts
#' @importFrom  BETS BETS.get
#' @import zoo
#' @return A \code{mts} in the same specification required in argument \code{base} of function \code{nowcasting}
#' @seealso \code{\link[BETS]{BETS.get}}
#' @export


base_extraction<-function(series_code){

# Seleção da série de dados
# Abaixo a lista com os códigos das variáveis tanto no Bacen quanto no BETS
codigos<-series_code

datas<-seq(as.Date("1994-07-01"),Sys.Date(),by="days") # vetor de datas desde 1994-07-01 até a data atual
base<-data.frame(datas)
start.time<-Sys.time()
for (i in 1:length(codigos)){
serie <-{}
serie_aux<-{}
serie<-BETS.get(codigos[i],data.frame = TRUE)     # trago a série em formato de data.frame
  for (jdatas in 1:length(datas)){                # séries diárias, mensais e trimestrais no mesmo data.frame
    ind <- which(serie[,1]==datas[jdatas])
    if (length(ind) == 0){
      serie_aux[jdatas] <- NA
    } else {
      serie_aux[jdatas]<- serie[ind,2]
    }
  }
  base<-cbind(base,serie_aux)
  names(base)[i+1]<-paste0('serie',codigos[i])
  print(i)
}

# Transformação da base para mensal ----

# Diária para mensal
# A média mensal representa a variável mensal
basexts <- xts(base[,-1],as.Date(base[,1]))
basemonth<-data.frame(apply.monthly(basexts,mean,na.rm=T))
basemonth[is.na(basemonth)]<-NA

# Trimestral para mensal
# interpolação linear de trimestral para mensal
# serie1344interp<-na.approx(basemonth$serie1344)   
# basemonth$serie1344<-c(serie1344interp,rep(NA,length(basemonth$serie1344)-length(serie1344interp)))
# serie7341interp<-na.approx(basemonth$serie7341)   
# basemonth$serie7341<-c(rep(NA,which(!is.na(basemonth[,"serie7341"]))[1]-1),serie7341interp,rep(NA,nrow(basemonth) - (tail(which(!is.na(basemonth[,"serie7341"])),1))))

# Acrescentando datas adicionais ----
# nas<-data.frame(matrix(NA,nrow=10,ncol=dim(basemonth)[2]))
# names(nas)<-names(basemonth)
# basemonth1<-rbind(basemonth,nas)
# mes_extra<-as.Date(row.names(basemonth1))[min(which(is.na(as.Date(row.names(basemonth1)))))-1]-days(3)+months(1:10)
# row.names(basemonth1)<-c(row.names(basemonth[1:(length(basemonth)-11)]),as.character(mes_extra))
# basemonth<-basemonth1

# write.csv2(basemonth,paste0('./base_dados/base_mes_',Sys.Date(),'.csv'),na = '')
year<-as.numeric(substr(row.names(basemonth)[1],1,4))
month<-as.numeric(substr(row.names(basemonth)[1],6,7))
mybase<-ts(basemonth,start=c(year,month),freq=12)
return(mybase)

}



