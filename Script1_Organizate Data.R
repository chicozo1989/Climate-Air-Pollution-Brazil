library(plyr)
setwd("C:/Users/Mare/Desktop/CHICO/FGV/SUL/RS")
##Loop over the wildfire and covariate files
fileNames = list.files(recursive = TRUE)
for (fileName in fileNames) {
  
  # Open the data
  data_wildfire_covariates <- read.csv(fileName, header = TRUE)
  
  # Extract Date, Year, Month, Day, and Hour into different columns
  data_wildfire_covariates$Date <- as.Date(data_wildfire_covariates$datahora)
  data_wildfire_covariates$Year <- lubridate::year(data_wildfire_covariates$datahora)
  data_wildfire_covariates$Month <- lubridate::month(data_wildfire_covariates$datahora)
  data_wildfire_covariates$Day <- lubridate::day(data_wildfire_covariates$datahora)
  data_wildfire_covariates$Hour <- lubridate::hour(data_wildfire_covariates$datahora)
  
  # Aggregate by Date (daily average)
  data_wildfire_covariates_daily <- ddply(data_wildfire_covariates, c("Date", "mun_geocod", "mun_nome", "mun_lat", "mun_lon", "mun_uf_nome", "Year", "Month", "Day"), summarise,
                                          co_ppb = mean(co_ppb, na.rm=TRUE),
                                          no2_ppb = mean(no2_ppb, na.rm=TRUE),
                                          o3_ppb = mean(o3_ppb, na.rm=TRUE),
                                          pm25_ugm3 = mean(pm25_ugm3, na.rm=TRUE),
                                          so2_ugm3 = mean(so2_ugm3, na.rm=TRUE),
                                          precipitacao_mmdia = mean(precipitacao_mmdia, na.rm=TRUE),
                                          temperatura_c = mean(temperatura_c, na.rm=TRUE),
                                          umidade_relativa_percentual = mean(umidade_relativa_percentual, na.rm=TRUE),
                                          vento_direcao_grau = mean(vento_direcao_grau, na.rm=TRUE),
                                          vento_velocidade_ms = mean(vento_velocidade_ms, na.rm=TRUE),
                                          focos_queimada = mean(focos_queimada, na.rm=TRUE))
  
  # Save the new files
  name <- paste0(fileName, "_edited", ".csv")
  write.csv(data_wildfire_covariates_daily, file=name)
  
  rm(data_wildfire_covariates)
  rm(data_wildfire_covariates_daily)
  
  print(paste(Sys.time(),fileName))
  
}

  library(data.table)


  filenames <- list.files(pattern = "_edited")
 NE<- rbindlist(lapply(fileNames,fread))
  saveRDS(NE, file="NE.rds")
  write.table(CENTRO, "CENTRO.csv", row.names = F)
