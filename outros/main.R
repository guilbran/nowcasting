
setwd('C:/Users/guilherme.branco/Desktop/RTDB')

library(devtools)
install_github('guilbran/nowcasting',force=T)

library(lubridate)
library(nowcasting)
library(zoo)
library(xts)

vintage
# Extrair a base
base<-base_extraction(c(1455,21859))
base<-window(base,start=c(2001,12),frequency=12)

# Criar o painel balanceado (substitui outliers,excluir séries, subst outliers, estacionariza, transforma em trimestral)
base0<-PRTDB(base,c(15,0),as.Date('2017-07-05'))
base01<-arrumarVintage(base0,c(3,3))
base1<-arrumarVintage(base,c(3,3))

# Encontrar os fatores dinâmicos
f<-FactorExtraction(x = Bpanel(vintage,rep(3,dim(vintage)[2])),q = 2,r = 2,p = 1)

now <- nowcast(y = mestri(lag(pib,-2)),regressors = vintage,legend = rep(3,dim(vintage)[2]),q = 2,r = 3,p = 1)

now2<- nowcast(y = diff(diff(mestri(lag(pib,-2)),4))
               ,regressors = vintage,legend = rep(3,dim(vintage)[2])
               ,q = 2,r = 2,p = 1)

summary(now2$reg)
ts.plot(now2$main,col=1:3)
now2$main


now$fatores$eigen$values/sum(now$fatores$eigen$values)

plot(now$fatores$eigen$values/sum(now$fatores$eigen$values))

arrumarVintage(vintage,rep(3,dim(vintage)[2]))
arrumarVintage(vintage[,1:2],c(3,3))
arrumarVintage(vintage[,1],c(3))

arrumarVintage(base,c(3,3,3))

colSums(is.na(base))/dim(base)[1]

# ler a base com as legendas
legendas<-readxl::read_excel('atraso_na_divulgação.xlsx',sheet = 2,col_names = F)
# legendas<-legendas[7:11,]

# extrair informação do Bacen
base<-base_extraction(legendas$X__1)
pib<-base_extraction(22099)

a<-mestri(lag(pib,-2))

frequency(a)



arrumarVintage(base = base,legenda = c(3,3))
arrumarVintage(base = pib,legenda = 3)

nowcast(diff(pib),base,c(3,3))


# Sequência com sextas feiras
datas <- seq(as.Date('2011-01-07'),as.Date('2017-03-10'),by='week')
# datas <- seq(as.Date('2013-12-06'),as.Date('2017-03-10'),by='week')

# Cuidar para a transformação 3 dar certo
transf<-rep(3,dim(base1)[2])
transf[sapply(1:dim(base)[2],function(x) sum(base[,x]==0,na.rm = T))!=0]<-2

now<-{}
a<-Sys.time()
for (i in 1:length(datas)){
base1<-PRTDB(base,legendas$X__2,datas[i])
pib1<-mestri(PRTDB(lag(pib,-2),60,datas[i]))
now[[i]]<-nowcast(pib1,base1,transf,1,1,1)
print(i)
}
b<-Sys.time()
Tot.time<-b-a
Tot.time

names(now)<-datas


# Aqui condenso as previsões em um único mts
sup<-{}
for (i in 1:length(now)){
sup<-cbind(sup,now[[i]]$prev[,3])
}
colnames(sup)<-as.character(datas)

# Análise individual de cada trimestre de interesse
s<-as.Date((na.omit(sup[,1])))[1]
e<-as.Date((na.omit(sup[,dim(sup)[2]])))[5]

trimestres<-seq(s,e,by='quarter')


int_quarter<-trimestres[10] # ou qualquer outro trimestre

seq<-sup[as.Date(sup)==int_quarter,]
seq1<-xts(seq,order.by = datas)

if(is.na(pib[as.Date(pib)==int_quarter])){
  YLIM<-NULL
}else{
  YLIM<-c(pib[as.Date(pib)==int_quarter]-10,pib[as.Date(pib)==int_quarter]+10)
}

plot(seq1,main = as.yearqtr(int_quarter),ylim = YLIM)
abline(h=pib[as.Date(pib)==int_quarter],col=2)




# Nowcasting

sup_now<-sup
pib1<-mestri(lag(pib,-2))

sup1<-{}
pibnow<-{}
pibref<-{}
for(i in 1:length(trimestres)){

tri_ref<-as.yearqtr(trimestres)[i]

tri_now<-as.yearqtr(as.Date(colnames(sup)))
sup_now[as.yearqtr(as.Date(sup)) %in% tri_ref, !(tri_now %in% tri_ref)] <- NA
sup1<-sup_now[as.yearqtr(as.Date(sup)) %in% tri_ref, (tri_now %in% tri_ref)]
pibnow<-c(pibnow,sup1)

pib_ref<-pib1[as.yearqtr(as.Date(pib1)) %in% tri_ref]
pibref<-c(pibref,rep(pib_ref,length(sup1)))

}

pibnow<-xts(pibnow,as.Date(colnames(sup)))
pibref<-xts(pibref,as.Date(colnames(sup)))

plot(pibnow,main='PIBnow')
lines(pibref,col=2,lwd=2)


for (i in 1:171){
  Sys.sleep(0.2)
  ts.plot(now[[i]]$prev,col=1:3,main=i)
}



