#' @title Plot for nowcast output function
#' @description Make plot to visualize the output of nowcast function
#' @param out Output of function nowcast
#' @param type 'fcst', 'factors', 'eigenvalues' or 'eigenvectors'
#' @examples
#' \dontrun{
#' trans <- USGDP$Legenda$Transformation[-length(USGDP$Legenda$Transformation)]
#' base <- USGDP$Base[,-dim(USGDP$Base)[2]]
#' gdp <- month2qtr(USGDP$Base[,dim(USGDP$Base)[2]])
#' x <- Bpanel(base = base, trans = trans)
#' now <- nowcast(y = gdp, x = x, q = 2, r = 3,method = 'GRS2008')
#' 
#' nowcast.plot(now, type = "fcst")
#' nowcast.plot(now, type = "factors")
#' nowcast.plot(now, type = "eigenvalues")
#' nowcast.plot(now, type = "eigenvectors")
#' 
#' x2 <- Bpanel(base = base, trans = trans,aggregate = F)
#' now2 <- nowcast(y = gdp, x = x2, q = 2, r = 3,method = 'BR2011')
#' 
#' nowcast.plot(now2, type = "fcst")
#' nowcast.plot(now2, type = "factors")
#' nowcast.plot(now2, type = "eigenvalues")
#' nowcast.plot(now2, type = "eigenvectors")
#' }
#' @export

nowcast.plot <- function(out, type = "fcst"){
  
  if(type == "fcst"){
    
    data <- data.frame(date = as.Date(out$main), out$main)
    data[max(which(is.na(data$out))),"out"] <- data[max(which(is.na(data$out))),"in."] 
    
    par(mar=c(5.1, 4.1, 4.1, 6), xpd = F)
    plot(data[,"y"], xaxt = "n", main = "",  bty = "l",
         col = "#FFFFFF", ylab = "", xlab = "Time")
    grid(col = "#D9D9D9")
    axis(1, at = seq(1,nrow(data),4), labels = substr(data[seq(1,nrow(data),4),"date"],1,7), las=1, cex = 0.7)
    lines(data[,"y"], type = "l", lty = 3, col = "#707070")
    lines(data[,"in."], type = "l", lty = 1, lwd = 1, col = "dodgerblue")
    lines(data[,"out"], type = "l", lty = 2, lwd = 1, col = "orangered")
    par(xpd = T)
    legend("topright", inset=c(-0.11,0), legend=c("y","yhat","fcst"), bty = "n",
           lty = c(3,1,2), lwd = c(1,1,1), col = c("darkgrey","dodgerblue","orangered"))
    title(main = list("Forecasting", font = 1, cex = 0.9))

  }else if(type == "eigenvalues"){
  
    par(mar = c(5.1,4.1,4.1,2.1), xpd = F)
    eig <- out$factors$eigen$values/sum(out$factors$eigen$values)*100
    n <- min(20,length(eig))
    barplot(eig[1:n], col = "#ADD8E6", border = "steelblue", names.arg = 1:n, ylim = c(0, seq(0,100,5)[min(which(!(max(eig[1:n]) > seq(0,100,5))))]),
            xlab = "eigenvalues", ylab = "%")
    grid(col = "#D9D9D9")
    title(main = list("eigenvalues: percentual variance", font = 1, cex = 0.9))

  }else if(type == "eigenvectors"){
    
    par(mar = c(5.1,4.1,4.1,6), xpd = F)
    vec <- out$factors$eigen$vectors[,1]
    pvec <- (out$factors$eigen$vectors[,1]^2)*100
    color <- ifelse(vec >= 0, "dodgerblue", "orangered")
    plot(pvec, main = "",  bty = "l", xaxt = "n", type = "h", ylab = "weight (%)", xlab = "variable", col = color)
    axis(1, at = seq(1,length(vec),1), labels = seq(1,length(vec),1), las=1, cex = 0.7)
    title(main = list("Variable Percentual Weight in Factor 1", font = 1, cex = 0.9))
    par(xpd = T)
    text(y = max(pvec)*1.08, x = length(pvec)*1.1, labels = "signal weights:", col = 1, cex = 0.8)
    text(y = max(pvec), x = length(pvec)*1.1, labels = "positive", col = "dodgerblue", cex = 0.8)
    text(y = max(pvec)*0.92, x = length(pvec)*1.1, labels = "negative", col = "orangered", cex = 0.8)

    
  }else if(type == "factors"){
    
    par(mar=c(5.1, 4.1, 4.1, 6), xpd = F)
    n <- ncol(data.frame(out$factors$dynamic_factors))
    ts.plot(out$factors$dynamic_factors, col = c(1,"orangered","blue"), lty = c(1,2,3), gpars = list(bty = "l"))
    anos <- unique(as.numeric(substr(as.Date(out$factors$dynamic_factors),1,4)))
    grid()
    par(xpd = T)
    title(main = list("Estimated Factors", font = 1, cex = 1))
    legend("topright", inset = -0.11, legend = paste("Factor", 1:n), bty = "n",
           col = c(1,"orangered","blue"), lty = c(1,2,3), cex = 0.9)

  }
}

