#' @name  giannoneetal2008
#' @title Example of replication files in Giannone et al. 2008
#' @docType data
#' @format
#' @description A \code{list} with 2 elements: 
#' 
#' \itemize{
#' \item \code{Base} is a \code{mts} with 193 series and 312 observations;
#' \item \code{Legenda} is a \code{data.frame} with specifications of the series in Base.
#' }
#' 
#' @usage giannoneetal2008
#' @source This dataset is avaiable as \emph{replication files} of the seminal work
#'  Giannone, Domenico, Lucrezia Reichlin, and David Small. "Nowcasting: The real-time informational content of macroeconomic data." \emph{Journal of Monetary Economics} 55.4 (2008): 665-676.
#'  One can find these \emph{replication files} in the following url: \url{https://www.newyorkfed.org/research/economists/giannone/pub}
#' @examples 
#' #### Giannone et al (2008) - Example
#' trans<-giannoneetal2008$Legenda$Transformation[-length(giannoneetal2008$Legenda$Transformation)]
#' base<-giannoneetal2008$Base[,-dim(giannoneetal2008$Base)[2]]
#' gdp<-giannoneetal2008$Base[,dim(giannoneetal2008$Base)[2]]
#' 
#' # Balanced Panel
#' base<-Bpanel(base = base,trans = trans)
#' 
#' # Estimation of nowcast
#' now<-nowcast(y = gdp,regressors = base)
#' 
#' # Main results:
#' now$main
#' ts.plot(now$main,col=1:3,main='Main results')
#' 
#' # the results are not the same as in the reference paper because we make some changes
#' # in the outlier correction function.
#' 
#' # Factors:
#' now$factors$fator_final
#' ts.plot(now$factors$fator_final,col=1:2,main='Factors')
NULL
