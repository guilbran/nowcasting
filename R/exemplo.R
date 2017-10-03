
pib<-base_extraction(22099)
now<-nowcast(lag(pib,-2),Bpanel(vintage,rep(3,dim(vintage)[2])))
now$prev
now$factors


# Giannone et al. 2008
pib<-base_extraction(22099)
retpib2<-monqua(diff(diff(lag(pib,-2),3),12))
base1<-Bpanel(vintage,rep(4,dim(vintage)[2]),aggregate = T)
now1<-nowcast(retpib2,base1)
now1$main
summary(now1$reg)

ts.plot(now1$main,col=1:3)


# Banbura e Runs 2007

base2<-Bpanel(vintage,rep(3,dim(vintage)[2]),aggregate = F)
now2<-nowcast(retpib2,base2,2,2,1,'banrun2011')
now2$main
summary(now2$reg)

ts.plot(now2$main,col=1:3)


ts.plot(now2$monthgdp)
ts.plot(now2$main,col=1:3)
ts.plot(filter(now2$factors$fator_final,c(1,2,3,2,1),sides = 1))

now2$reg$coefficients

now2$factors$fator_final


ts.plot(now1$factors$fator_final,col=1:2)
ts.plot(now2$factors$fator_final,col=1:2)

end(na.omit(retpib2))

