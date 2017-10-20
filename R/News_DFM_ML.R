# %%%  Replication files for:
# %%%  ""Nowcasting", 2010, (by Marta Banbura, Domenico Giannone and Lucrezia Reichlin), 
# %%% in Michael P. Clements and David F. Hendry, editors, Oxford Handbook on Economic Forecasting.
# %%%
# %%% The software can be freely used in applications. 
# %%% Users are kindly requested to add acknowledgements to published work and 
# %%% to cite the above reference in any resulting publications
#' @import R.matlab matlab zoo
#' @export

library(R.matlab)
library(matlab)
library(zoo)
library(Matrix)
R_new <- readMat("C:/Users/guilherme.branco/Desktop/EM-transcription/arquivos pra fç EMstep/R_new.mat")
R_new <- R_new$R.new[,,1]
names(R_new)[1]<-'X_sm'
names(R_new)[9]<-'Z_0'
names(R_new)[10]<-'V_0'
X_old <- data.frame(read.csv("C:/Users/guilherme.branco/Desktop/EM-transcription/arquivos pra fç EMstep/X_old.csv", header = F))
X_new <- data.frame(read.csv("C:/Users/guilherme.branco/Desktop/EM-transcription/arquivos pra fç EMstep/X_new.csv", header = F))
# 
Q = R_new
t_fcst = 187   # Qual a data do nowcast?
# v_news = NULL
v_news<-"V25" # Qual a série de interesse? Pode ser um vetor?


News_DFM_ML <- function(X_old = NULL, X_new = NULL, Q = NULL, t_fcst = NULL, v_news = NULL){
  
  nomes<-names(X_new)
  r <- ncol(Q$C)
  TT <- nrow(X_new)
  N <- ncol(X_new)
  gList <- unique(unlist(Q$Groups))
  groupnews <- zeros(1,length(gList))
  singlenews <- data.frame(zeros(1,N))
  names(singlenews)<-names(X_new)
  gain <- NULL
  gainSer <- NULL
  actual <- NULL
  fore <- NULL
  filt <- NULL
  
  #if(!is.null(v_news)){
  # if(!is.nan(X_new[t_fcst,v_news])){
  # if(!is.na(X_new[t_fcst,v_news])){
  if(sum(as.numeric(!is.na(X_new[t_fcst,v_news])))>0){    # Caso permita um vetor como série de interesse
    Res_old <- para_const(X_old, Q, 0)
    temp <- X_new[t_fcst,v_news] - Res_old$X_sm[t_fcst,v_news]
    singlenews[v_news] <- temp
    # groupnews[, gList %in% Q$Groups[v_news]] <- temp
    y_old <- Res_old$X_sm[t_fcst,v_news]
    y_new <- X_new[t_fcst,v_news]
  }else{
    
    Mx = Q$Mx
    Wx = Q$Wx
    
    miss_old <- is.na(X_old)
    miss_new <- is.na(X_new)
    temp <- miss_old - miss_new
    v_miss <- as.vector(which(colSums(temp == 1)==1))
    # x <- t_fcst*v_miss
    t_miss <- as.vector(unlist(apply(temp, MARGIN = 2, FUN = function(x) find(x == 1))))
    lag <- t_fcst - t_miss
    k <- max(c(abs(lag), max(lag)-min(lag)))
    
    C <- Q$C
    R <- t(Q$R)
    
    n_news <- length(lag)
    
    Res_old <- para_const(X_old, Q, k)
    Res_new <- para_const(X_new, Q, 0)
    
    y_old <- Res_old$X_sm[t_fcst,v_news]
    y_new <- Res_new$X_sm[t_fcst,v_news]
    
    # if(!is.empty(t_miss)){
    if(!is.null(t_miss)){
      P <- Res_old$P[,,2:dim(Res_old$P)[3]]
      P1 <- NULL
      for(i in 1:length(lag)){
        h <- abs(t_fcst-t_miss[i])
        m <- max(t_miss[i], t_fcst)
        if(t_miss[i] > t_fcst){
          Pp <- t(P[1:r,(h*r+1):(h*r+r),m])
        }else{
          Pp <- P[1:r,(h*r+1):(h*r+r),m]
        }
        P1 <- cbind(P1, Pp %*% C[v_miss[i],1:r])
      }
      
      innov <- NULL
      for(i in 1:length(t_miss)){
        X_new_norm <- (X_new[t_miss[i],v_miss[i]] - Mx[,v_miss[i]])/Wx[,v_miss[i]]
        X_sm_norm <- (Res_old$X_sm[t_miss[i],v_miss[i]] - Mx[,v_miss[i]])/Wx[,v_miss[i]]
        innov[i] <- X_new_norm-X_sm_norm
      }
      
      ins <- ncol(innov)
      P2 <- NULL
      p2 <- NULL
      WW <- matrix(NA,dim(R)[1],dim(R)[2])
      for(i in 1:length(lag)){
        for(j in 1:length(lag)){
          h <- abs(lag[i]-lag[j])
          m <- max(t_miss[i],t_miss[j])
          if(t_miss[j] > t_miss[i]){
            Pp <- t(P[1:r,(h*r+1):((h+1)*r),m])
          }else{
            Pp <- P[1:r,(h*r+1):((h+1)*r),m]
          }
          if(v_miss[i] == v_miss[j] & t_miss[i] != t_miss[j]){
            WW[v_miss[i],v_miss[j]] <- 0
          }else{
            WW[v_miss[i],v_miss[j]] <- R[v_miss[i],v_miss[j]]
          }
          p2 <- cbind(p2,C[v_miss[i],1:r] %*% Pp %*% C[v_miss[j],1:r] + WW[v_miss[i],v_miss[j]])
        }
        # colnames(p2) <- colnames(P2)
        P2 <- rbind(P2,p2)
        p2 <- NULL
      }
      
      totnews <- Wx[nomes==v_news] %*% C[nomes==v_news,1:r] %*% P1 %*% solve(P2) %*% innov
      temp <- Wx[nomes==v_news] %*% C[nomes==v_news,1:r] %*% P1 %*% solve(P2) * innov
      gain <- Wx[nomes==v_news] %*% C[nomes==v_news,1:r] %*% P1 %*% solve(P2)
      
      # Ainda preciso introduzir esse parâmetro na lista Q
      # gainSer <- Q$Series[v_miss]
      
      singlenews <- zeros(max(t_miss)-min(t_miss)+1,N)
      actual <- zeros(N,1)
      fore <- zeros(N,1)
      filt <- zeros(N,1)
      
      for(i in 1:length(innov)){
        singlenews[t_miss[i]-min(t_miss)+1,v_miss[i]] <- temp[i]
        actual[v_miss[i],1] <- X_new[t_miss[i],v_miss[i]];
        fore[v_miss[i],1] <- Res_old$X_sm[t_miss[i],v_miss[i]]
        filt[v_miss[i],] <- gain[i]/Wx[v_miss[i]]
      }
      
      singlenews <- sum(singlenews)

      # Falta implementar por grupo de notícias      
      # for(i in 1:length(gList)){
      #   groupnews[i] <- gain[, Q$Groups[v_miss] %in% gList[i]] %*% t(innov[Q$Groups[v_miss] %in% gList[i]])
      # }
      # 
      v_miss0 = unique(v_miss)
      idx <- which(v_miss0 %in% v_miss)
      j <- idx
      v_miss <- v_miss0
      gain <- gain[idx]
      # gainSer <- gainSer[idx]
      
    }
  }
  
  # output
  return(list(OldFcst = y_old,
              NewFcst = y_new,
              GroupNews = groupnews,
              SerNews = singlenews,
              gainT = gain,
              serGainT = gainSer,
              Actual = actual,
              Fcst = fore,
              Filt = filt))
  
}


t_fcst = 187   # Qual a data do nowcast?
v_news<-c("V1","V2") # Qual a série de interesse? Pode ser um vetor?

news<-News_DFM_ML(X_old,X_new,Q,t_fcst,v_news)

news1<-News_DFM_ML(X_old,X_new,Q,t_fcst,"V1")
news2<-News_DFM_ML(X_old,X_new,Q,t_fcst,"V2")

news$OldFcst
news$NewFcst
news$SerNews

news1$NewFcst
news2$NewFcst

news$SerNews
news1$SerNews

news$gainT
news1$gainT
news2$gainT

news$Actual
news1$Actual
news2$Actual


cbind(news$Fcst,
news1$Fcst,
news2$Fcst)

cbind(news$Filt,
news1$Filt,
news2$Filt)
