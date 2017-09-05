# read files
base<-read.csv2('./outros/factor122807.2.csv')
legends<-read.csv2('./outros/Legend.csv')

# Transformation vector
trans <- legends$Transformation[-length(legends$Transformation)]

# Regressors matrix time serie
dates<-as.Date(as.character(base[-1,1]),'%d/%m/%Y')

start_year<-substr(dates[1],1,4)
start_month <- substr(dates[1],6,7)
basets<-ts(base[-1,-1],start = c(as.numeric(start_year),as.numeric(start_month)),frequency = 12)

regressors<-basets[,-dim(basets)[2]]
gdp<-basets[,dim(basets)[2]]

x<-Bpanel(regressors,trans)
x<-Bpanel(basets,c(trans,0))

now<-nowcast(gdp,basets,c(trans,0))

now<-nowcast(gdp,regressors,trans)

now$main

plot(now$factors$eigen$values/(sum(now$factors$eigen$values)))

now$factors$fator_final

now$factors$C

now$main


ts.plot(now$main,col=1:3)
summary(now$reg)

now$factors$eigen$values/sum(now$factors$eigen$values)
plot(now$factors$eigen$values/sum(now$factors$eigen$values))

colnames(regressors)

nowcast

mestri(gdp)

ts.plot(now$main,col=1:3)

vintageTRANSF<-Bpanel(regressors,trans)
factor<-FactorExtraction(vintageTRANSF,2,2,1)
now<-aux_nowcast2(mestri(gdp),factor$fator_final)

factor$fator_final

plot(factor$eigen$values/sum(factor$eigen$values))


nowcast


pib<-base_extraction(22099)
nowcast(mestri(lag(pib,-2)),vintage,rep(3,dim(vintage)[2]))


covR<-read.csv2('covR.csv')[,-1]
covmatlab<-read.csv2('covmatlab.csv',header = F)
covRnovo<-cov(x)*((dim(x)[1]-1)/dim(x)[1])

covR-covmatlab

eiR<-eigen(covR)
eimatlab<-eigen(covmatlab)
eiRnovo<-eigen(covRnovo)

eiR$values[1:3]
eiRnovo$values[1:3]
eimatlab$values[1:3]

ts.plot(base[,'IPFPS'])

cov(base[,'IPFPS'],base[,'IPMINE'])

cov(cbind(z[,'IPFPS'],z[,'IPMINE']))

covR$IPMINE[2]
covmatlab[2,which(colnames(z)=='IPMINE')]


