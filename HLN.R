#maximum length DM is  n^1/3 + 1 with the number of forecast
library(xlsx)
library(multDM)

#gw.test(x = df$RFnow6, y = df$DFMnow6, p = df$Realnow6, T = length(df$Realnow6),    #another option is Giacomini-White
#        tau = 1, method = "HAC", alternative = "less")

#Estimations#############################
####Backcast
#this can be adapted to do it more efficently in a simple loop
df0 <- read.xlsx("C:/Users/salvador/Documents/Doctorado/ProyectoFulbright/Pistolas/DFM_v4/GW/HLN_contri/Forecasts_OUT_back_tri.xlsx",sheetName = "h1")

M=ceiling(length(df0$DFMback1)^(1/3))
T <- length(df0$DFMback1)
#we use the DM test and apply the Harvey correction. We specify the alternative hypothesis as first forecast more accurate than second
DM.test(df0$DFMback1,df0$ARIMAback1,df0$Realback1,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)
#in each case, we apply the correction manually. Another option is setting in the DM.test function c=TRUE.  
2*pnorm(-abs(DM.test(df0$DFMback1,df0$ARIMAback1,df0$Realback1,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)))

DM.test(df0$DFMback1,df0$RFback1,df0$Realback1,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)
2*pnorm(-abs(DM.test(df0$DFMback1,df0$RFback1,df0$Realback1,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)))

DM.test(df0$DFMback1,df0$Gboostback1,df0$Realback1,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)
2*pnorm(-abs(DM.test(df0$DFMback1,df0$Gboostback1,df0$Realback1,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)))

DM.test(df0$DFMback1,df0$LSTMback1,df0$Realback1,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)
2*pnorm(-abs(DM.test(df0$DFMback1,df0$LSTMback1,df0$Realback1,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)))


df0 <- read.xlsx("C:/Users/salvador/Documents/Doctorado/ProyectoFulbright/Pistolas/DFM_v4/GW/HLN_contri/Forecasts_OUT_back_tri.xlsx",sheetName = "h2")

M=ceiling(length(df0$DFMback2)^(1/3))
T <- length(df0$DFMback2)
DM.test(df0$DFMback2,df0$ARIMAback2,df0$Realback2,loss.type="AE",2,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)
2*pnorm(-abs(DM.test(df0$DFMback2,df0$ARIMAback2,df0$Realback2,loss.type="AE",2,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)))

DM.test(df0$DFMback2,df0$RFback2,df0$Realback2,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)
2*pnorm(-abs(DM.test(df0$DFMback2,df0$RFback2,df0$Realback2,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)))

DM.test(df0$DFMback2,df0$Gboostback2,df0$Realback2,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)
2*pnorm(-abs(DM.test(df0$DFMback2,df0$Gboostback2,df0$Realback2,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)))

DM.test(df0$DFMback2,df0$LSTMback2,df0$Realback2,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)
2*pnorm(-abs(DM.test(df0$DFMback2,df0$LSTMback2,df0$Realback2,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)))


df0 <- read.xlsx("C:/Users/salvador/Documents/Doctorado/ProyectoFulbright/Pistolas/DFM_v4/GW/HLN_contri/Forecasts_OUT_back_tri.xlsx",sheetName = "h3")

M=ceiling(length(df0$DFMback3)^(1/3))
T <- length(df0$DFMback3)
DM.test(df0$DFMback3,df0$ARIMAback3,df0$Realback3,loss.type="AE",3,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)
2*pnorm(-abs(DM.test(df0$DFMback3,df0$ARIMAback3,df0$Realback3,loss.type="AE",3,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)))

DM.test(df0$DFMback3,df0$RFback3,df0$Realback3,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)
2*pnorm(-abs(DM.test(df0$DFMback3,df0$RFback3,df0$Realback3,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)))

DM.test(df0$DFMback3,df0$Gboostback3,df0$Realback3,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)
2*pnorm(-abs(DM.test(df0$DFMback3,df0$Gboostback3,df0$Realback3,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)))

DM.test(df0$DFMback3,df0$LSTMback3,df0$Realback3,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)
2*pnorm(-abs(DM.test(df0$DFMback3,df0$LSTMback3,df0$Realback3,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)))

########
#Nowcast
df0 <- read.xlsx("C:/Users/salvador/Documents/Doctorado/ProyectoFulbright/Pistolas/DFM_v4/GW/HLN_contri/Forecasts_OUT_now_tri.xlsx",1)

M=ceiling(length(df0$DFMnow1)^(1/3))
T <- length(df0$DFMnow1)
DM.test(df0$DFMnow1,df0$ARIMAnow1,df0$Realnow1,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)
2*pnorm(-abs(DM.test(df0$DFMnow1,df0$ARIMAnow1,df0$Realnow1,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)))

DM.test(df0$DFMnow1,df0$RFnow1,df0$Realnow1,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)
2*pnorm(-abs(DM.test(df0$DFMnow1,df0$RFnow1,df0$Realnow1,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)))

DM.test(df0$DFMnow1,df0$Gboostnow1,df0$Realnow1,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)
2*pnorm(-abs(DM.test(df0$DFMnow1,df0$Gboostnow1,df0$Realnow1,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)))

DM.test(df0$DFMnow1,df0$LSTMnow1,df0$Realnow1,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)
2*pnorm(-abs(DM.test(df0$DFMnow1,df0$LSTMnow1,df0$Realnow1,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)))



df0 <- read.xlsx("C:/Users/salvador/Documents/Doctorado/ProyectoFulbright/Pistolas/DFM_v4/GW/HLN_contri/Forecasts_OUT_now_tri.xlsx",2)

M=ceiling(length(df0$DFMnow2)^(1/3))
T <- length(df0$DFMnow2)
DM.test(df0$DFMnow2,df0$ARIMAnow2,df0$Realnow2,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)
2*pnorm(-abs(DM.test(df0$DFMnow2,df0$ARIMAnow2,df0$Realnow2,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)))

DM.test(df0$DFMnow2,df0$RFnow2,df0$Realnow2,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)
2*pnorm(-abs(DM.test(df0$DFMnow2,df0$RFnow2,df0$Realnow2,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)))

DM.test(df0$DFMnow2,df0$Gboostnow2,df0$Realnow2,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)
2*pnorm(-abs(DM.test(df0$DFMnow2,df0$Gboostnow2,df0$Realnow2,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)))

DM.test(df0$DFMnow2,df0$LSTMnow2,df0$Realnow2,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)
2*pnorm(-abs(DM.test(df0$DFMnow2,df0$LSTMnow2,df0$Realnow2,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)))



df0 <- read.xlsx("C:/Users/salvador/Documents/Doctorado/ProyectoFulbright/Pistolas/DFM_v4/GW/HLN_contri/Forecasts_OUT_now_tri.xlsx",3)

M=ceiling(length(df0$DFMnow3)^(1/3))
T <- length(df0$DFMnow3)
DM.test(df0$DFMnow3,df0$ARIMAnow3,df0$Realnow3,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)
2*pnorm(-abs(DM.test(df0$DFMnow3,df0$ARIMAnow3,df0$Realnow3,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)))

DM.test(df0$DFMnow3,df0$RFnow3,df0$Realnow3,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)
2*pnorm(-abs(DM.test(df0$DFMnow3,df0$RFnow3,df0$Realnow3,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)))

DM.test(df0$DFMnow3,df0$Gboostnow3,df0$Realnow3,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)
2*pnorm(-abs(DM.test(df0$DFMnow3,df0$Gboostnow3,df0$Realnow3,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)))

DM.test(df0$DFMnow3,df0$LSTMnow3,df0$Realnow3,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)
2*pnorm(-abs(DM.test(df0$DFMnow3,df0$LSTMnow3,df0$Realnow3,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)))



df0 <- read.xlsx("C:/Users/salvador/Documents/Doctorado/ProyectoFulbright/Pistolas/DFM_v4/GW/HLN_contri/Forecasts_OUT_now_tri.xlsx",4)

M=ceiling(length(df0$DFMnow4)^(1/3))
T <- length(df0$DFMnow4)
DM.test(df0$DFMnow4,df0$ARIMAnow4,df0$Realnow4,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)
2*pnorm(-abs(DM.test(df0$DFMnow4,df0$ARIMAnow4,df0$Realnow4,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)))

DM.test(df0$DFMnow4,df0$RFnow4,df0$Realnow4,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)
2*pnorm(-abs(DM.test(df0$DFMnow4,df0$RFnow4,df0$Realnow4,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)))

DM.test(df0$DFMnow4,df0$Gboostnow4,df0$Realnow4,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)
2*pnorm(-abs(DM.test(df0$DFMnow4,df0$Gboostnow4,df0$Realnow4,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)))

DM.test(df0$DFMnow4,df0$LSTMnow4,df0$Realnow4,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)
2*pnorm(-abs(DM.test(df0$DFMnow4,df0$LSTMnow4,df0$Realnow4,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)))



df0 <- read.xlsx("C:/Users/salvador/Documents/Doctorado/ProyectoFulbright/Pistolas/DFM_v4/GW/HLN_contri/Forecasts_OUT_now_tri.xlsx",5)

M=ceiling(length(df0$DFMnow5)^(1/3))
T <- length(df0$DFMnow5)
DM.test(df0$DFMnow5,df0$ARIMAnow5,df0$Realnow5,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)
2*pnorm(-abs(DM.test(df0$DFMnow5,df0$ARIMAnow5,df0$Realnow5,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)))

DM.test(df0$DFMnow5,df0$RFnow5,df0$Realnow5,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)
2*pnorm(-abs(DM.test(df0$DFMnow5,df0$RFnow5,df0$Realnow5,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)))

DM.test(df0$DFMnow5,df0$Gboostnow5,df0$Realnow5,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)
2*pnorm(-abs(DM.test(df0$DFMnow5,df0$Gboostnow5,df0$Realnow5,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)))

DM.test(df0$DFMnow5,df0$LSTMnow5,df0$Realnow5,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)
2*pnorm(-abs(DM.test(df0$DFMnow5,df0$LSTMnow5,df0$Realnow5,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)))



df0 <- read.xlsx("C:/Users/salvador/Documents/Doctorado/ProyectoFulbright/Pistolas/DFM_v4/GW/HLN_contri/Forecasts_OUT_now_tri.xlsx",6)

M=ceiling(length(df0$DFMnow6)^(1/3))
T <- length(df0$DFMnow6)
DM.test(df0$DFMnow6,df0$ARIMAnow6,df0$Realnow6,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)
2*pnorm(-abs(DM.test(df0$DFMnow6,df0$ARIMAnow6,df0$Realnow6,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)))

DM.test(df0$DFMnow6,df0$RFnow6,df0$Realnow6,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)
2*pnorm(-abs(DM.test(df0$DFMnow6,df0$RFnow6,df0$Realnow6,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)))

DM.test(df0$DFMnow6,df0$Gboostnow6,df0$Realnow6,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)
2*pnorm(-abs(DM.test(df0$DFMnow6,df0$Gboostnow6,df0$Realnow6,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)))

DM.test(df0$DFMnow6,df0$LSTMnow6,df0$Realnow6,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)
2*pnorm(-abs(DM.test(df0$DFMnow6,df0$LSTMnow6,df0$Realnow6,loss.type="AE",1,c=FALSE,H1="more")[["statistic"]]*sqrt((T+1-2*M+M*(M-1))/T)))



#######################################################################
#Pesaran-Timmerman test

#We create the function for the test
pttest<-function(y,yhat){ 
  len <- length(y)
  pyz <- sum(sign(y)==sign(yhat))/len
  py <- sum(y>0)/len
  qy <- py*(1-py)/len
  pz <- sum(yhat > 0)/len
  qz <- pz*(1 - pz)/len
  p <- py*pz + (1 - py)*(1 - pz)
  v <- p*(1 - p)/len
  w <- ((2*py - 1)^2) * qz + ((2*pz - 1)^2) * qy + 4*qy*qz
  pt <- (pyz - p) / (sqrt(v - w))
  pval <- 1 - pnorm(pt, 0, 1)
  return(list(pyz,pt,pval))
}

#Backcast example
df0 <- read.xlsx("DataForPTTest_tri.xlsx","Backh1")
serie1 <- df0$RealDif1
seriehat <- df0$RFDif1
#Output
print(paste0("direction accuracy",pttest(serie1,seriehat)[[1]],"PT ",pttest(serie1,seriehat)[[2]],"pvalor ",pttest(serie1,seriehat)[[3]]))


#Nowcast example
df0 <- read.xlsx("DataForPTTest_tri.xlsx","Nowh6")

serie1 <- df0$RealDif6
seriehat <- df0$LSTMDif6

print(paste0("direction accuracy",pttest(serie1,seriehat)[[1]],"PT ",pttest(serie1,seriehat)[[2]],"pvalor ",pttest(serie1,seriehat)[[3]]))
