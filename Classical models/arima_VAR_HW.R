#Program to estimate the out-of-sample results for the ARIMA model
rm(list=ls())

library("readxl")
require("writexl")
library(lmtest)
library(forecast)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#estimation of parameters
# indica0<-read_xlsx("data_Monthly_Homicides_1221.xlsx",sheet=1,col_names = TRUE) 
# fecha<-as.matrix(indica0[1:(nrow(indica0)),1])
# indica<-as.matrix(indica0[1:(nrow(indica0)),2:5])
# 
# gg1<-subset(indica[,2], indica[,2]!=99999)
# 
# for (i in 1:5){
# fitARIMA <- arima(gg1*10^(-5),order=c(3,0,3),include.mean = TRUE,method="ML")   #este tiene AIC de -6271 para los datos 1221, vs -6260 de arima(2,0,2)  vs -6253 de arima(2,0,0) o -6187 de arima(0,1,0)
# coef(fitARIMA)
# }


library(forecast)
w <- c(1110,1011,1112,1113,1114,1115,1116,1117,1118,1119,1120,1121)
for(momento in w){
#we pass the ARIMA(3,0,3) in sample parameters

indica0<-read_xlsx(paste0("data_Monthly_Homicides_",momento,".xlsx"),sheet="Sheet1",col_names = TRUE) 
fecha<-as.matrix(indica0[14:(nrow(indica0)),1])
indica<-as.matrix(indica0[14:(nrow(indica0)),2:5])
gg1<-subset(indica[,2], indica[,2]!=99999)


fitARIMA <- arima(gg1*10^(-5),order=c(3,0,3),include.mean = TRUE,method="ML",fixed=c(-8.9383e-02,1.2456e-01,9.4871e-01,6.7975e-01,6.3719e-01,-3.5170e-01,3.4826e-05)) 
futurVal <- forecast(fitARIMA,h=24, level=c(99.5))
#plot(futurVal)
futurVal

write_xlsx(futurVal,paste0("Results_MonthHom_guns_ar2_pequeño_",momento,".xlsx"))

}




#VAR


rm(list=ls())

library(vars)
library(tsDyn)
library(panelvar)
library(plm)
library(Spillover)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
data0 <- openxlsx::read.xlsx("data_Monthly_Homicides_1221.xlsx",sheet=1)

data0 <- data0[,c("X1","Quarterly","Monthly.HomFi_PopEst_sa","Homicide_Guns_Archive_sa", "MO_Homic&(G_or_F)" ,"Ciudades", "GT_Homicides_sa_det","BCs")]
colnames(data0) <- c("date","X1","Hom","GA", "MOH","H3", "GTH","BCs")
#data$X1 <- as.Date(data$X1)
data <- data0[min(which(data0$H3 != 9.999900e+04)):max(which(data0$Hom != 9.999900e+04)),]

df <- data.frame(data[,2:8])

colnames(df) <- c("X1","Hom","GA", "MOH","H3", "GTH","BCs")
#df <-df[,-3]
df <-df[,-c(1,3,6)]

VARselect(df, lag.max = 5,type='const')

nhor <- 24

var.model <- VAR(df, p = 3) #not necessary

#and now we use it in sample for the others

vartsDyn<- lineVar(df, lag = 3)

df$date <- data$date


#the exercise is here
#we simply select here the end of the series, and we predict from that point 24 observations
fore_v2 <- predict(vartsDyn,df[(which(df$date=="12.16")-2):which(df$date=="12.16"),1:5],n.ahead = 24) 

library(xlsx)  #change the name of the output each time
write.xlsx(fore_v2[,1],"results_1217.xlsx")



#Holt Winters

rm(list=ls())

library("readxl")
require("writexl")
library(forecast)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
list.files()

indica0<-openxlsx::read.xlsx("data_Monthly_Homicides_1221.xlsx",sheet=1)
indica<-as.matrix(indica0[1:(nrow(indica0)),3])

gg1<-subset(indica[,1], indica[,1]!=99999)


hom <- ts(gg1/10000,frequency=12,start(1999,1))

HW1 <- HoltWinters(hom)   #alpha=0.3, beta=0.1, gamma=0.1 with the whole sample


gg1 <- gg1[1:(length(gg1)-1*12)]  #change the "1" until 12, and the name each time of output file
HW2 <- HoltWinters(hom, alpha=0.3, beta=0.1, gamma=0.1) #we use the in-sample parameters

HW2.pred <- predict(HW2, 24)

write.xlsx(t(t(HW2.pred)), "results_12_20.xlsx")