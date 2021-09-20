


library(RCurl)
library(bitops)
library(metafor)
library(Formula)



setwd("C:/Users/Micro/Documents/FGV/META-ANALISE")
data <- read.csv("mas_ws.csv",sep = ";",dec = ",")
head(data)


# Subset the dataset (by season)
data_1 <- data[which(data$Month=="1"), ]
data_2 <- data[which(data$Month=="2"), ]
data_3 <- data[which(data$Month=="3"), ]
data_4 <- data[which(data$Month=="4"), ]
data_5 <- data[which(data$Month=="5"), ]
data_6 <- data[which(data$Month=="6"), ]
data_7 <- data[which(data$Month=="7"), ]
data_8 <- data[which(data$Month=="8"), ]
data_9 <- data[which(data$Month=="9"), ]
data_10 <- data[which(data$Month=="10"), ]
data_11 <- data[which(data$Month=="11"), ]
data_12 <- data[which(data$Month=="12"), ]



### Apply escalc() function to calculate effect sizes and their variance.
data_1 <- escalc(yi = ws, sei = SD_und, data = data_1, measure = "SMD", append = TRUE)
data_2 <- escalc(yi = ws, sei = SD_und, data = data_2, measure = "SMD", append = TRUE)
data_3 <- escalc(yi = ws, sei = SD_und, data = data_3, measure = "SMD", append = TRUE)
data_4 <- escalc(yi = ws, sei = SD_und, data = data_4, measure = "SMD", append = TRUE)
data_5 <- escalc(yi = ws, sei = SD_und, data = data_5, measure = "SMD", append = TRUE)
data_6 <- escalc(yi = ws, sei = SD_und, data = data_6, measure = "SMD", append = TRUE)
data_7 <- escalc(yi = ws, sei = SD_und, data = data_7, measure = "SMD", append = TRUE)
data_8 <- escalc(yi = ws, sei = SD_und, data = data_8, measure = "SMD", append = TRUE)
data_9 <- escalc(yi = ws, sei = SD_und, data = data_9, measure = "SMD", append = TRUE)
data_10 <- escalc(yi = ws, sei = SD_und, data = data_10, measure = "SMD", append = TRUE)
data_11 <- escalc(yi = ws, sei = SD_und, data = data_11, measure = "SMD", append = TRUE)
data_12 <- escalc(yi = ws, sei = SD_und, data = data_12, measure = "SMD", append = TRUE)

### Using the effect sizes and variance we can calculate the random-effects meta-analysis. 
ma_model_1 <- rma(yi, vi, data = data_1)
ma_model_2 <- rma(yi, vi, data = data_2)
ma_model_3 <- rma(yi, vi, data = data_3)
ma_model_4 <- rma(yi, vi, data = data_4)
ma_model_5 <- rma(yi, vi, data = data_5)
ma_model_6 <- rma(yi, vi, data = data_6)
ma_model_7 <- rma(yi, vi, data = data_7)
ma_model_8 <- rma(yi, vi, data = data_8)
ma_model_9 <- rma(yi, vi, data = data_9)
ma_model_10 <- rma(yi, vi, data = data_10)
ma_model_11 <- rma(yi, vi, data = data_11)
ma_model_12 <- rma(yi, vi, data = data_12)


summary(ma_model_1)
summary(ma_model_2)
summary(ma_model_3)
summary(ma_model_4)
summary(ma_model_5)
summary(ma_model_6)
summary(ma_model_7)
summary(ma_model_8)
summary(ma_model_9)
summary(ma_model_10)
summary(ma_model_11)
summary(ma_model_12)



### Q-Q normal plots:
#   The plot shows the theoretical quantiles of a normal distribution on the horizontal axis 
#   against the observed quantiles of the (externally) standardized residuals on the vertical axis.

qqnorm(ma_model_1)
qqnorm(ma_model_COld)




### Tests for bias. It is a test from the funnel plot (created above).
#   If Egger's regressions test or the Rank COrrelation test do not present 
#   significance (p>0.05), so tehre is no evidence of publication bias.      
forest(ma_model_oct,slab = paste(data_apr$Regname))

regtest(ma_model_jan)

regtest(ma_model_COld)

