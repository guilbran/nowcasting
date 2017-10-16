#' @title Nowcasting of a quarterly time serie using a dynamic factor.
#' @description Estimate nowcasting and foreacasting for a quarterly time serie.
#' @param y Stationary quarterly time-series 
#' @param x A time series matrix (\code{mts}) representing the regressors of interest. The series must be stationary.
#' @param q Dynamic rank. Number of error terms. If not specified q = 2.
#' @param r Static rank (r>=q), i.e. number of factors. If not specified r = 2.
#' @param p AR order of factors. If not specified p = 1.
#' @param method 2sq: Two stages quarterly for Giannone et al. 2008; 2sm: Two stages monthly for Banbura and Runstler 2011; EM: Expected Maximization Giannone et al 2011
#' @return A \code{list} containing two elements:
#' 
#' A \code{data.frame} named \code{main} contains the original serie, the estimation in the sample, the estimation out of the sample;
#' 
#' A \code{list} named \code{factors} contains the estimated factors and coeffients..
#' @references Giannone, D., Reichlin, L., & Small, D. (2008). Nowcasting: The real-time informational content of macroeconomic data. Journal of Monetary Economics, 55(4), 665-676.<doi:10.1016/j.jmoneco.2008.05.010>
#' @examples
#' \dontrun{
#' pib<-base_extraction(22099)
#' retpib2<-month2qtr(diff(diff(lag(pib,-2),3),12))
#' base1<-Bpanel(BRGDP,rep(4,dim(BRGDP)[2]),aggregate = T)
#' now1<-nowcast(retpib2,base1,1,1,1,method = '2sq')
#'
#' base2<-Bpanel(BRGDP,rep(4,dim(BRGDP)[2]),aggregate = F)
#' now2<-nowcast(retpib2,base2,2,2,1,'2sm')
#' 
#' now3<-nowcast(retpib2,base2,1,1,1,'EM')
#' 
#' }
#' @seealso \code{\link[nowcasting]{base_extraction}}
#' @export

nowcast <- function(y, x, q = 2, r = 2, p = 1,method='2sq'){

  if(method=='2sq'){
    factors <- FactorExtraction(x, q = q, r = r, p = p)
    fatores <- factors$dynamic_factors
    prev <- bridge(y,fatores)
    # return(list(main = prev$main, reg = prev$reg, factors = factors))
    
  }else if(method=='2sm'){
    factors <- FactorExtraction(x, q = q, r = r, p = p)
    fatores <- stats::filter(factors$dynamic_factors, c(1,2,3,2,1), sides = 1)
    prev <- bridge(y,fatores)
    
    aux_month<-prev$reg$coefficients*cbind(rep(1,dim(factors$dynamic_factors)[1]),factors$dynamic_factors)
    monthgdp<-ts(rowSums(aux_month),start=start(factors$dynamic_factors),freq=12)
    
    # return(list(monthgdp=monthgdp,main = prev$main, reg = prev$reg, factors = factors))
  }else if(method=='EM'){
    X<-cbind(x,qtr2month(y))
    Par<-list(r=rep(r,3),p=p,max_iter=3,i_idio=c(rep(T,dim(x)[2]),F),
              Rconstr = matrix(c(
                c(2,3,2,1),
                c(-1,0,0,0),
                c(0,-1,0,0),
                c(0,0,-1,0),
                c(0,0,0,-1))
                ,4,5),
              q = matrix(rep(0,4),4,1),nQ = 1,
              blocks = matrix(rep(1,dim(x)[2]*3),dim(x)[2],3))
    Res<-EM_DFM_SS_block_idioQARMA_restrMQ(X,Par)
    factors<-list(dynamic_factors = Res$FF,A = Res$A, C = Res$C, Q = Res$Q, R = Res$R, initx = Res$Z_0,
            initV = Res$V_0)
    monthgdp<-ts(Res$X_sm[,dim(Res$X_sm)[2]],start=start(X),frequency = 12)
    
    Y<-cbind(y,month2qtr(monthgdp),month2qtr(monthgdp))
     Y[is.na(Y[,1]),2]<-NA
    Y[!is.na(Y[,1]),3]<-NA
    colnames(Y)<-c('y','in','out')
    
    return(list(main = Y,factors = factors,fore_x = ts(Res$X_sm[,-dim(Res$X_sm)[2]],start=start(X),frequency = 12),
                monthgdp = monthgdp))
  }

  # voltar da padronização
  fit<-factors$dynamic_factors%*%t(factors$eigen$vectors[,1:r])
  colnames(fit)<-colnames(x)
  x <- x
  z <- x
  s <- apply(z, MARGIN = 2, FUN = sd,na.rm=T)
  M <- apply(z, MARGIN = 2, FUN = mean,na.rm=T)
  for(i in 1:dim(x)[2]){
    z[,i] <- (x[,i] - M[i])/s[i]
  }
  x1<-fit
  fore_x<-x[,colnames(x) %in% colnames(fit)]
  for(i in colnames(fit)){
    x1[,i]<-s[i]*fit[,i]+M[i]
    fore_x[is.na(fore_x[,i]),i] <- x1[is.na(fore_x[,i]),i]
  }
  
  if(!(exists('monthgdp'))){
    monthgdp<-NULL
  }
  
  return(list(main = prev$main, reg = prev$reg, factors = factors,fore_x = fore_x,monthgdp = monthgdp))
  
}
