#' @title Nowcasting of a quarterly time serie using a dynamic factor.

#' @description Estimate nowcasting and foreacasting for a quarterly time serie. The method is based on 
#' \emph{Giannone et al. 2008}
#' @param y Stationary quarterly time-series 
#' @param regressors A time series matrix (\code{mts}) representing the regressors of interest. The series must be stationary.
#' @param q Dynamic rank. Number of error terms. If not specified q = 2.
#' @param r Static rank (r>=q), i.e. number of factors. If not specified r = 2.
#' @param p AR order of factors. If not specified p = 1.
#' @param method giannoneetal2008 or banrun2011
#' @return A \code{list} containing two elements:
#' 
#' A \code{data.frame} named \code{main} contains the original serie, the estimation in the sample, the estimation out of the sample;
#' 
#' A \code{list} named \code{factors} contains the estimated factors and coeffients..
#' @references Giannone, D., Reichlin, L., & Small, D. (2008). Nowcasting: The real-time informational content of macroeconomic data. Journal of Monetary Economics, 55(4), 665-676.<doi:10.1016/j.jmoneco.2008.05.010>
#' @examples
#' \dontrun{
#' pib<-base_extraction(22099)
#' now<-nowcast(lag(pib,-2),Bpanel(vintage,rep(3,dim(vintage)[2])))
#' now$main
#' now$factors
#' # Giannone et al. 2008
#' pib<-base_extraction(22099)
#' retpib2<-diff(diff(lag(pib,-2),3),12)
#' base1<-Bpanel(vintage,rep(3,dim(vintage)[2]),aggregate = T)
#' now1<-nowcast(retpib2,base1)
#' now1$main
#' summary(now1$reg)
#' 
#' # Banbura e Runs 2007
#' base2<-Bpanel(vintage,rep(3,dim(vintage)[2]),aggregate = F)
#' now2<-nowcast(retpib2,base1,2,2,1,'banrun2007')
#' now2$main
#' summary(now2$reg)
#' }
#' @seealso \code{\link[nowcasting]{base_extraction}}
#' @export

nowcast <- function(y, regressors, q = 2, r = 2, p = 1,method='giannoneetal2008'){

  if(method=='giannoneetal2008'){
    factors <- FactorExtraction(regressors, q = q, r = r, p = p)
    fatores <- factors$fator_final
    prev <- bridge(y,fatores)
    # return(list(main = prev$main, reg = prev$reg, factors = factors))
    
  }else if(method=='banrun2011'){
    factors <- FactorExtraction(regressors, q = q, r = r, p = p)
    fatores <- stats::filter(factors$fator_final, c(1,2,3,2,1), sides = 1)
    prev <- bridge(y,fatores)
    
    aux_month<-prev$reg$coefficients*cbind(rep(1,dim(factors$fator_final)[1]),factors$fator_final)
    monthgdp<-ts(rowSums(aux_month),start=start(factors$fator_final),freq=12)
    
    # return(list(monthgdp=monthgdp,main = prev$main, reg = prev$reg, factors = factors))
  }

  # voltar da padronização
  fit<-factors$fator_final%*%t(factors$eigen$vectors[,1:r])
  colnames(fit)<-colnames(regressors)
  x <- regressors
  z <- x
  s <- apply(z, MARGIN = 2, FUN = sd,na.rm=T)
  M <- apply(z, MARGIN = 2, FUN = mean,na.rm=T)
  for(i in 1:dim(regressors)[2]){
    z[,i] <- (x[,i] - M[i])/s[i]
  }
  x1<-fit
  fore_regressors<-regressors[,colnames(regressors) %in% colnames(fit)]
  for(i in colnames(fit)){
    x1[,i]<-s[i]*fit[,i]+M[i]
    fore_regressors[is.na(fore_regressors[,i]),i] <- x1[is.na(fore_regressors[,i]),i]
  }
  
  if(!(exists('monthgdp'))){
    monthgdp<-NULL
  }
  
  return(list(monthgdp=monthgdp,main = prev$main, reg = prev$reg, factors = factors,fore_regressors = fore_regressors))
  
}
