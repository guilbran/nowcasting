

# Giannone et al (2008) - Example
trans<-giannoneetal2008$Legenda$Transformation[-length(giannoneetal2008$Legenda$Transformation)]
base<-giannoneetal2008$Base[,-dim(giannoneetal2008$Base)[2]]
gdp<-giannoneetal2008$Base[,dim(giannoneetal2008$Base)[2]]

now<-nowcast(gdp,base,trans)

# Main results:
now$main
ts.plot(now$main,col=1:3,main='Main results')

# the results are not the same as in the reference paper cause we make some changes
# in the outlier correction function.

# Factors:
now$factors$fator_final
ts.plot(now$factors$fator_final,col=1:2)
ts.plot(cbind(now$main[,1],monqua(now$factors$fator_final)),col=c(1,2,4))


