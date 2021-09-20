setwd("C:/Users/Mare/Desktop/CHICO/FGV/REGIÕES")
library(mgcv)
memory.limit(999999)
head(SE)
##### Abrir DATASET
CENTRO <- readRDS("CENTRO.rds")
CENTRO <- subset(CENTRO, CENTRO[2]>="2003")

##Modelo -> Inserir apenas as variáveis climáticas.
results<-data.frame()
{     
  results.temp <- data.frame()
  fit.prec <- gam(precipitacao_mmdia ~ Year+as.factor(Month), data=CENTRO)
  fit.temp <- gam(temperatura_c ~ Year+as.factor(Month), data=CENTRO)
  fit.ur <- gam(umidade_relativa_percentual ~ Year+as.factor(Month), data=CENTRO)
  fit.vv <- gam(vento_velocidade_ms ~ Year+as.factor(Month), data=CENTRO)
  fit.dv <- gam(vento_direcao_grau ~ Year+as.factor(Month), data=CENTRO)#### Altere o Poluente analisado
  results.temp[1,1]<-fit.prec$coefficients[2]
  results.temp[1,2]<-fit.temp$coefficients[2]
  results.temp[1,3]<-fit.ur$coefficients[2]
  results.temp[1,4]<-fit.vv$coefficients[2]
  results.temp[1,5]<-fit.dv$coefficients[2]
  colnames(results.temp)<-c("Precipitação (mm)", "Temperatura (c)","Umidade (%)"," Velocidade do Vento (m/s)", "Direção do Vento (graus)")
  results<-rbind(results, results.temp)
write.csv(results, file="CENTRO_Clima.csv", row.names=FALSE)
}
