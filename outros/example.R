# ler a base com as legendas
legendas<-read.csv2('C:/Users/guilherme.branco/Desktop/RTDB/atraso_na_divulgação.csv',header = F)

# extrair informação do Bacen
base<-base_extraction(legendas$V1)
pib<-base_extraction(22099)

# Sequência com sextas feiras
datas <- seq(as.Date('2011-01-07'),as.Date('2017-03-10'),by='week')
datas <- seq(as.Date('2013-12-06'),as.Date('2017-03-10'),by='week')



now<-{}
a<-Sys.time()
for (i in 1:length(datas)){
  base1<-PRTDB(base,legendas$X__2,datas[i])
  pib1<-mestri(PRTDB(lag(pib,-2),60,datas[i]))
  now[[i]]<-nowcast(pib1,base1,rep(3,dim(base1)[2]),1,1,1)
  # tryCatch({
  # 
  #   now[[i]]<-nowcast(pib1,base1,rep(3,dim(base1)[2]),2,2,1)},
  # 
  #   error = function(e){
  # 
  #     return(warning(paste('ERRO NESSA BOSS ',i)))
  # 
  # })
  print(i)
}
b<-Sys.time()
Tot.time<-b-a
Tot.time

now[[i]]


i <- 50
i <- 152
base1<-PRTDB(base,legendas$V2,datas[i])
pib1<-mestri(PRTDB(lag(pib,-2),60,datas[i]))
try(now[[i]]<-nowcast(pib1,base1,rep(3,dim(base1)[2]),2,2,1))





