setwd("C:/Users/Micro/Documents/FGV/REGIÕES")
library(mgcv)
memory.limit(999999)
head(NO)
##### Abrir DATASET

NE <- readRDS("NE.rds")
NE <- subset(NE, NE[2]>="2003")
BC<-read.csv("BC.csv",sep=";",dec=",")


quantile = as.numeric(quantile(NO$co_ppb, c(.95)))   # Mudar o poluente aqui na variável
NO$case <- ifelse(NO$co_ppb >= quantile, 1 , 0)  # Mudar o poluente aqui na variável
NO_sub_b <- subset(NO, NO$case=="0")

##Modelo Sem Ajustes -> Inserir apenas o tempo como variavel.
results<-data.frame()
for (k in 1:12)  {      #### Por mês
  results.temp <- data.frame()
  fit.pollutant <- gam(co ~ ano, subset=BC$mês==k, data=BC)     #### Altere o Poluente analisado
  results.temp[1,1]<-k
  results.temp[1,2]<-fit.pollutant$coefficients[2]
  
  ###### Change here the name
  colnames(results.temp)<-c("Mês", "co_Trend")
  results<-rbind(results, results.temp)}
  write.csv(results, file="co_UNADJUSTED_SE.csv", row.names=FALSE)
}

quantile = as.numeric(quantile(SE$no2_ppb, c(.95)))   # Mudar o poluente aqui na variável
SE$caseb <- ifelse(SE$no2_ppb >= quantile, 1 , 0)  # Mudar o poluente aqui na variável
SE_sub_c <- subset(SE, SE$caseb=="0")
results<-data.frame()
for (k in 1:12)  {      #### Por mês
  results.temp <- data.frame()
  fit.pollutant <- gam(no2_ppb ~ Year + as.factor(Day) + as.factor(wday), subset=SE_sub_c$Month==k, data=SE_sub_c)     #### Altere o Poluente analisado
  results.temp[1,1]<-k
  results.temp[1,2]<-fit.pollutant$coefficients[2]
  
  ###### Change here the name
  colnames(results.temp)<-c("Mês", "no2_Trend")
  results<-rbind(results, results.temp)
  write.csv(results, file="no2_UNADJUSTED_SE.csv", row.names=FALSE)
}

quantile = as.numeric(quantile(SE$o3_ppb, c(.95)))   # Mudar o poluente aqui na variável
SE$casec <- ifelse(SE$o3_ppb >= quantile, 1 , 0)  # Mudar o poluente aqui na variável
SE_sub_d <- subset(SE, SE$casec=="0")
results<-data.frame()
for (k in 1:12)  {      #### Por mês
  results.temp <- data.frame()
  fit.pollutant <- gam(o3_ppb ~ Year + as.factor(Day) + as.factor(wday), subset=SE_sub_d$Month==k, data=SE_sub_d)     #### Altere o Poluente analisado
  results.temp[1,1]<-k
  results.temp[1,2]<-fit.pollutant$coefficients[2]
  
  ###### Change here the name
  colnames(results.temp)<-c("Mês", "o3_Trend")
  results<-rbind(results, results.temp)
  write.csv(results, file="o3_UNADJUSTED_SE.csv", row.names=FALSE)
}


quantile = as.numeric(quantile(SE$so2_ugm3,na.rm=T, c(.95)))   # Mudar o poluente aqui na variável
SE$cased <- ifelse(SE$so2_ugm3 >= quantile, 1 , 0)  # Mudar o poluente aqui na variável
SE_sub_e <- subset(SE, SE$cased=="0")
results<-data.frame()
for (k in 1:12)  {      #### Por mês
  results.temp <- data.frame()
  fit.pollutant <- gam(so2_ugm3 ~ Year + as.factor(Day) + as.factor(wday), subset=SE_sub_b$Month==k, data=SE_sub_b)     #### Altere o Poluente analisado
  results.temp[1,1]<-k
  results.temp[1,2]<-fit.pollutant$coefficients[2]
  
  ###### Change here the name
  colnames(results.temp)<-c("Mês", "so2_Trend")
  results<-rbind(results, results.temp)
  write.csv(results, file="so2_UNADJUSTED_SE_2.csv", row.names=FALSE)
}

quantile = as.numeric(quantile(SE$pm25_ugm3,na.rm=T, c(.95)))   # Mudar o poluente aqui na variável
SE$casee <- ifelse(SE$pm25_ugm3 >= quantile, 1 , 0)  # Mudar o poluente aqui na variável
SE_sub_f <- subset(SE, SE$casee=="0")
results<-data.frame()
for (k in 1:12)  {      #### Por mês
  results.temp <- data.frame()
  fit.pollutant <- gam(pm25_ugm3 ~ Year + as.factor(Day) + as.factor(wday), subset=SE_sub_f$Month==k, data=SE_sub_f)     #### Altere o Poluente analisado
  results.temp[1,1]<-k
  results.temp[1,2]<-fit.pollutant$coefficients[2]
  
  ###### Change here the name
  colnames(results.temp)<-c("Mês", "pm25_Trend")
  results<-rbind(results, results.temp)
  write.csv(results, file="pm25_UNADJUSTED_SE.csv", row.names=FALSE)
}

##MODELO AJUSTADO
quantile = as.numeric(quantile(NE$co_ppb, c(.95)))   # Mudar o poluente aqui na variável
NE$case <- ifelse(NE$co_ppb >= quantile, 1 , 0)  # Mudar o poluente aqui na variável
NE_sub_b <- subset(NE, NE$case=="0")
results<-data.frame()
for (k in 1:12)  {      #### Por mês
  results.temp <- data.frame()
  fit.pollutant <- gam(co_ppb ~ Year + as.factor(Day) + as.factor(wday) + s(temperatura_c) + s(precipitacao_mmdia) + s(umidade_relativa_percentual) + s(vento_velocidade_ms) + s(vento_direcao_grau), subset=NE_sub_b$Month==k, data=NE_sub_b)     #### Altere o Poluente analisado
  results.temp[1,1]<-k
  results.temp[1,2]<-fit.pollutant$coefficients[2]
    ###### Change here the name
  colnames(results.temp)<-c("Mês", "co_Trend")
  results<-rbind(results, results.temp)
  write.csv(results, file="co_ADJUSTED_NE_2.csv", row.names=FALSE)
}

quantile = as.numeric(quantile(NE$no2_ppb, c(.95)))   # Mudar o poluente aqui na variável
NE$case <- ifelse(NE$no2_ppb >= quantile, 1 , 0)  # Mudar o poluente aqui na variável
NE_sub_b <- subset(NE, NE$case=="0")
results<-data.frame()
for (k in 1:12)  {      #### Por mês
  results.temp <- data.frame()
  fit.pollutant <- gam(no2_ppb ~ Year + as.factor(Day) + as.factor(wday) + s(temperatura_c) + s(precipitacao_mmdia) + s(umidade_relativa_percentual) + s(vento_velocidade_ms) + s(vento_direcao_grau), subset=NE_sub_b$Month==k, data=NE_sub_b)     #### Altere o Poluente analisado
  results.temp[1,1]<-k
  results.temp[1,2]<-fit.pollutant$coefficients[2]
  ###### Change here the name
  colnames(results.temp)<-c("Mês", "No2_Trend")
  results<-rbind(results, results.temp)
  write.csv(results, file="no2_ADJUSTED_NE_2.csv", row.names=FALSE)
}

quantile = as.numeric(quantile(NE$o3_ppb, c(.95)))   # Mudar o poluente aqui na variável
NE$case <- ifelse(NE$o3_ppb >= quantile, 1 , 0)  # Mudar o poluente aqui na variável
NE_sub_b <- subset(NE, NE$case=="0")
results<-data.frame()
for (k in 1:12)  {      #### Por mês
  results.temp <- data.frame()
  fit.pollutant <- gam(o3_ppb ~ Year + as.factor(Day) + as.factor(wday) + s(temperatura_c) + s(precipitacao_mmdia) + s(umidade_relativa_percentual) + s(vento_velocidade_ms) + s(vento_direcao_grau), subset=NE_sub_b$Month==k, data=NE_sub_b)     #### Altere o Poluente analisado
  results.temp[1,1]<-k
  results.temp[1,2]<-fit.pollutant$coefficients[2]
  ###### Change here the name
  colnames(results.temp)<-c("Mês", "o3_Trend")
  results<-rbind(results, results.temp)
  write.csv(results, file="o3_ADJUSTED_NE_2.csv", row.names=FALSE)
}

quantile = as.numeric(quantile(NE$so2_ugm3, na.rm=T,c(.95)))   # Mudar o poluente aqui na variável
NE$case <- ifelse(NE$so2_ugm3 >= quantile, 1 , 0)  # Mudar o poluente aqui na variável
NE_sub_b <- subset(NE, NE$case=="0")
results<-data.frame()
for (k in 1:12)  {      #### Por mês
  results.temp <- data.frame()
  fit.pollutant <- gam(so2_ugm3 ~ Year + as.factor(Day) + as.factor(wday) + s(temperatura_c) + s(precipitacao_mmdia) + s(umidade_relativa_percentual) + s(vento_velocidade_ms) + s(vento_direcao_grau), subset=NE_sub_b$Month==k, data=NE_sub_b)     #### Altere o Poluente analisado
  results.temp[1,1]<-k
  results.temp[1,2]<-fit.pollutant$coefficients[2]
  ###### Change here the name
  colnames(results.temp)<-c("Mês", "so2_Trend")
  results<-rbind(results, results.temp)
  write.csv(results, file="so2_ADJUSTED_NE_2.csv", row.names=FALSE)
}

quantile = as.numeric(quantile(NE$pm25_ugm3,na.rm=T, c(.95)))   # Mudar o poluente aqui na variável
NE$case <- ifelse(NE$pm25_ugm3 >= quantile, 1 , 0)  # Mudar o poluente aqui na variável
NE_sub_b <- subset(NE, NE$case=="0")
results<-data.frame()
for (k in 1:12)  {      #### Por mês
  results.temp <- data.frame()
  fit.pollutant <- gam(pm25_ugm3 ~ Year + as.factor(Day) + as.factor(wday) + s(temperatura_c) + s(precipitacao_mmdia) + s(umidade_relativa_percentual) + s(vento_velocidade_ms) + s(vento_direcao_grau), subset=NE_sub_b$Month==k, data=NE_sub_b)     #### Altere o Poluente analisado
  results.temp[1,1]<-k
  results.temp[1,2]<-fit.pollutant$coefficients[2]
  ###### Change here the name
  colnames(results.temp)<-c("Mês", "pm25_Trend")
  results<-rbind(results, results.temp)
  write.csv(results, file="pm25_ADJUSTED_NE2.csv", row.names=FALSE)
}



##### standard error calculation (Bootstrap analysis) - ADJUSTED AND UNADJUSTED

for (i in 1:length(regional_names)){
  conc.reg <- subset(daily.sodium, AREA==regional_names[i])  ###### change here the name of the input dataset
  results <-data.frame()
  reg.abbr <- regional_abbr[i]  
  
  #for (k in 1:12){                             ##### This is by month
  #conc <- subset(conc.reg, MONTH==k)         ##### This is by month
  for (k in 0:1){                             ##### This is by season (0 = cold season;  1 = warm season)
    conc <- subset(conc.reg, warm==k)         ##### This is by season
    
    conc$index <- seq(1, dim(conc)[1], 1)
    unique.id <- unique(conc$SITE_NUM)
    n.site <- length(unique.id)
    
    time.begin <- date()   
    
    for (j in 1:100){
      big.bootc <- data.frame(matrix(NA, n.site, dim(conc)[2]))
      results.temp <-data.frame()
      
      for (s in 1:n.site)
      {
        site <- unique.id[s]
        conc.site <- conc[conc$SITE_NUM==site,]
        
        nd     <- dim(conc.site)[1]
        bs    <- 20   ########################## This is for 20 days
        nblocks <- ceiling(nd/bs)  
        
        if (j%%20 == 0) print(j)
        inds   <- round(runif(nblocks, 1, nd)) ###### nd-bs+1
        
        sample <- matrix(NA, nblocks, bs)
        for (i in 1:nblocks){
          if (inds[i] <= (dim(conc.site)[1]-bs+1)){
            sample[i,] <- inds[i]:(inds[i]+bs-1)
          } else {
            sample[i,] <- c(inds[i]:dim(conc.site)[1], 1:(20-(dim(conc.site)[1]-inds[i])-1))
          }
        }
        
        sample.vec <- as.vector(t(sample))[1:dim(conc.site)[1]]
        
        bootc.dat <- conc.site[sample.vec,]
        if (s==1){
          big.bootc <- bootc.dat}
        if (s>1){
          big.bootc <- rbind(big.bootc,bootc.dat)}
      }
      
      fit.pollutant.unadjusted <- gam(POLLUTION ~ YEAR + as.factor(MONTH) + as.factor(WEEKDAYS), data=big.bootc)
      fit.pollutant.adjusted <- gam(POLLUTION ~ YEAR + as.factor(MONTH)+as.factor(WEEKDAYS)+s(TempC)+s(WDSP)+s(RH), data=big.bootc) 
      
      results.temp[1,1]<-regional_names[i]
      results.temp[1,2]<-k
      results.temp[1,3]<-j
      results.temp[1,4]<-fit.pollutant.unadjusted$coefficients[2]
      results.temp[1,5]<-fit.pollutant.adjusted$coefficients[2]
      
      colnames(results.temp)<-c("regname", "warm", "bstrap","sodium_trend_unadjusted", "sodium_trend_adjusted")   ##### Change here the name
      results <- rbind(results, results.temp)
      
    }
    time.end <- date()
    
    print(time.begin)
    print(time.end)
    
    rm(conc)
  }
  name <- paste("unad_adjusted_daily_sodium_trends_bstrap_by_season_",reg.abbr,".csv", sep="")   ###### Change here the name of the output
  write.csv(results, file=name, row.names=FALSE)
}


