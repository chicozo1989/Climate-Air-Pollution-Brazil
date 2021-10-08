

### Study: Weather impact on air pollution in Brazil ####
#########################################################

### Script for the bootstrap analysis ####
##########################################


### Step 1 ###
##############

### Open the dataset
setwd("C:/Users/Micro/Documents/FGV/REGIÕES")
data <- readRDS("SE.rds")

region = "Southeast"  # Change the region name here according to the dataset opened above
head(data)

### Select only observations for the period between 2003-2018 
data_2 <- subset(data, data[8]>="2003")

### Define the variables for the loop
pollution = c("temperatura_c", "precipitacao_mmdia", "umidade_relativa_percentual", "vento_velocidade_ms")
months = c(1:12)

### Packages
library(mgcv)
library(boot)

### Save memory during the analyses
memory.limit(999999)



### Step 2 ###
##############

### Start the loop over pollution and months
results<-data.frame()

for(i in pollution){
  
  # Remove observations with concentration higher than the percentile 0.95
  data_3 <- subset(data_2, data_2[i] < quantile(data_2[i], 0.95, na.rm=T))
  
  for (k in months) {
    
    data_4 <- subset(data_3, data_3$Month==k)
    
    
    ### UNADJUSTED MODEL 
    # Define the function for the bootstrap
    set.seed(626)
    rsq <- function(formula, data, indices) {
      d <- data[indices,] # allows boot to select sample
      fit <- gam(formula, data=d)
      return(coef(fit))
    }
    
    # Run Bootstrapping with 1000 replications 
    results_unadjusted <- boot(data = data_4, statistic = rsq,
                               R=1000, formula = data_4[[i]] ~ Year)
    
  
   
    # Extract results
    original_unadjusted <- results_unadjusted$t0[2]
    std_error_unadjusted <- apply(results_unadjusted$t, 2, sd)[2]
    
    
    # Save results
    results.temp <- data.frame()
    results.temp[1,1]<- region
    results.temp[1,2]<- i
    results.temp[1,3]<- k
    results.temp[1,4]<- original_unadjusted
    results.temp[1,5]<- std_error_unadjusted

    
    colnames(results.temp)<-c("Region", "Pollutant", "Month", "Original_unadjusted", "std_error_unadjusted")
    
    results <- rbind(results, results.temp)
    
    print(paste0(Sys.time(), " --- Completed month ", k, " for pollutant ", i))
    
    rm(data_4)
    rm(results_unadjusted)
    rm(original_unadjusted)
    rm(std_error_unadjusted)
 
  }
  
  print(paste0(Sys.time(), " --- Pollutant ", i, " is done!!!"))
  rm(data_3)
  
}

# Take a look at the final outcome
results

# Save the outcome as CSV
setwd("C:/Users/Micro/Documents/FGV/REGIÕES")
name <- paste("Bootstrap_climate_",region,".csv", sep="") 
write.csv(results, file=name, row.names=FALSE)

# Clean environment
rm(list=ls())
gc()









