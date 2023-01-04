setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(xlsx)
#data <- openxlsx::read.xlsx("Datos_2010_imputate.xlsx",sheet=1)
data <- openxlsx::read.xlsx("Tw_Hom.xlsx",1)

#We can take a look on some tests for stationarity

#library(tseries)
#adf.test(data$meanneg) #for unit roots
#kpss.test(data$meanneg)
#library(Kendall) #to see trends
#MannKendall(data$meanneg)


library(pracma)

#We detrend a linear trend from the series (if not needed, is just a change in level):
x <- detrend(as.numeric(data$Twitter.number.Homicide),tt="linear")

#We can take a look on the spectral density if we want to specify some frequencies.
#In our case, we use always daily, monthly and yearly:

#t <- seq(1,length(x),1)
#par(mfrow=c(2,1))
#plot(t,x,'l')
#spectrum(x)
#del<-1 # sampling interval
#smoothing it
#x.spec <- spectrum(x,log="no",spans=c(2,2),plot=FALSE)
#spx <- x.spec$freq/del
#spy <- 2*x.spec$spec
#plot(spy~spx,xlab="frequency",ylab="spectral density",type="l")
#lst <- sort(spy, index.return=TRUE, decreasing=TRUE)
#outliers <- lapply(lst, `[`, lst$x %in% head(unique(lst$x),100))
#1/spx[outliers$ix]   #And then you could choose the frequencies 

#Now we adjust STL with multiple seasons, with the package forecast
library(forecast)
#we specify daily, monthly and yearly 
x_2 <- msts(x, seasonal.periods=c(12))
#x_2 <- msts(x, seasonal.periods=c(12))
data_seas <- mstl(x_2)    

# With the trend that the decomposition still capture(non linear)+ remainder, we get the detrended and SA series 
#df <- data.frame(data$MO.riots,data_seas[,c(1,2,6)],data_seas[,2]+data_seas[,6])


#If we also want to output the SA series (no detrended), don't take the detrended one
x_3 <- msts(data$Twitter.number.Homicide,seasonal.periods=c(12))

data_seas_3 <- mstl(x_3)  
df_3 <- data.frame(data_seas_3)
#so we include the dates, the series, the linealy detrended from data_seas,
#the detrended and SA from data_seas as mentioned before, and the same from the non detrended one, so the SA series
df <- data.frame(data$Date,data$Twitter.number.Homicide,data_seas_3[,3]+data_seas_3[,4],data_seas_3[,4],data_seas_3[,2]+data_seas_3[,4])

#And everything as output
colnames(df) <- c("dates","data","detrended","detrended_sa","sa")

writexl::write_xlsx(df,"Tw_Hom_sa_det.xlsx")






################################################################################################

#WEIGHTING THE CRIMES


data <- read.csv("NYPD_Complaint_Data_Historic.csv",1)

#filter just felonies like violent crimes
data_fel <- subset(data,LAW_CAT_CD=="FELONY")

#to have an idea of the numbers
count_crimes <- sum(duplicated(data$OFNS_DESC))
count_crimes2 <- table(data$OFNS_DESC)

tail(sort(count_crimes2),5)
main_crimes <- c(tail(sort(count_crimes2),5),count_crimes2[44],count_crimes2[65])


#creation of a weighted series with BJS weights
#full series
count_time2 <- table(data_fel$CMPLNT_FR_DT)
df_count_dates <- data.frame(sort(count_time2))
df_count_dates <- df_count_dates[order(as.Date(df_count_dates$Var1, format="%m/%d/%Y")),]



#Murder
data_fel_mur <- subset(data_fel,OFNS_DESC=="MURDER & NON-NEGL. MANSLAUGHTER")
data_fel_mur <- table(data_fel_mur$CMPLNT_FR_DT)
df_count_dates_fel_mur <- data.frame(sort(data_fel_mur))[order(as.Date(data.frame(sort(data_fel_mur))$Var1, format="%m/%d/%Y")),]

#Larceny
data_fel_gra <- subset(data_fel,OFNS_DESC=="GRAND LARCENY")
data_fel_gra <- table(data_fel_gra$CMPLNT_FR_DT)
df_count_dates_fel_gra <- data.frame(sort(data_fel_gra))[order(as.Date(data.frame(sort(data_fel_gra))$Var1, format="%m/%d/%Y")),]


#Felony assault
data_fel_ass <- subset(data_fel,OFNS_DESC=="FELONY ASSAULT")
data_fel_ass <- table(data_fel_ass$CMPLNT_FR_DT)
df_count_dates_fel_ass <- data.frame(sort(data_fel_ass))[order(as.Date(data.frame(sort(data_fel_ass))$Var1, format="%m/%d/%Y")),]

#Robbery
data_fel_rob <- subset(data_fel,OFNS_DESC=="ROBBERY")
data_fel_rob <- table(data_fel_rob$CMPLNT_FR_DT)
df_count_dates_fel_rob <- data.frame(sort(data_fel_rob))[order(as.Date(data.frame(sort(data_fel_rob))$Var1, format="%m/%d/%Y")),]



#Burglary
data_fel_bur <- subset(data_fel,OFNS_DESC=="BURGLARY")
data_fel_bur <- table(data_fel_bur$CMPLNT_FR_DT)
df_count_dates_fel_bur <- data.frame(sort(data_fel_bur))[order(as.Date(data.frame(sort(data_fel_bur))$Var1, format="%m/%d/%Y")),]


#Miscellaneous Penal Law
data_fel_mis <- subset(data_fel,OFNS_DESC=="MISCELLANEOUS PENAL LAW")
data_fel_mis <- table(data_fel_mis$CMPLNT_FR_DT)
df_count_dates_fel_mis <- data.frame(sort(data_fel_mis))[order(as.Date(data.frame(sort(data_fel_mis))$Var1, format="%m/%d/%Y")),]



#Grand Larceny of Motor Vehicle
data_fel_graMV <- subset(data_fel,OFNS_DESC=="GRAND LARCENY OF MOTOR VEHICLE")
data_fel_graMV <- table(data_fel_graMV$CMPLNT_FR_DT)
df_count_dates_fel_graMV <- data.frame(sort(data_fel_graMV))[order(as.Date(data.frame(sort(data_fel_graMV))$Var1, format="%m/%d/%Y")),]




#Criminal Mischief & Related of
data_fel_cri <- subset(data_fel,OFNS_DESC=="CRIMINAL MISCHIEF & RELATED OF")
data_fel_cri <- table(data_fel_cri$CMPLNT_FR_DT)
df_count_dates_fel_cri <- data.frame(sort(data_fel_cri))[order(as.Date(data.frame(sort(data_fel_cri))$Var1, format="%m/%d/%Y")),]

#Dangerous Drugs
data_fel_dru <- subset(data_fel,OFNS_DESC=="DANGEROUS DRUGS")
data_fel_dru <- table(data_fel_dru$CMPLNT_FR_DT)
df_count_dates_fel_dru <- data.frame(sort(data_fel_dru))[order(as.Date(data.frame(sort(data_fel_dru))$Var1, format="%m/%d/%Y")),]


#Theft-Fraud
data_fel_the <- subset(data_fel,OFNS_DESC=="THEFT-FRAUD")
data_fel_the <- table(data_fel_the$CMPLNT_FR_DT)
df_count_dates_fel_the <- data.frame(sort(data_fel_the))[order(as.Date(data.frame(sort(data_fel_the))$Var1, format="%m/%d/%Y")),]


#Dangerous weapons
data_fel_wep <- subset(data_fel,OFNS_DESC=="DANGEROUS WEAPONS")
data_fel_wep <- table(data_fel_wep$CMPLNT_FR_DT)
df_count_dates_fel_wep <- data.frame(sort(data_fel_wep))[order(as.Date(data.frame(sort(data_fel_wep))$Var1, format="%m/%d/%Y")),]


#Forgery
data_fel_forg <- subset(data_fel,OFNS_DESC=="FORGERY")
data_fel_forg <- table(data_fel_forg$CMPLNT_FR_DT)
df_count_dates_fel_forg <- data.frame(sort(data_fel_forg))[order(as.Date(data.frame(sort(data_fel_forg))$Var1, format="%m/%d/%Y")),]

#Rape
data_fel_rap <- subset(data_fel,OFNS_DESC=="FORGERY")
data_fel_rap <- table(data_fel_rap$CMPLNT_FR_DT)
df_count_dates_fel_rap <- data.frame(sort(data_fel_rap))[order(as.Date(data.frame(sort(data_fel_rap))$Var1, format="%m/%d/%Y")),]

#Sex Crimes
data_fel_sex <- subset(data_fel,OFNS_DESC=="SEX CRIMES")
data_fel_sex <- table(data_fel_sex$CMPLNT_FR_DT)
df_count_dates_fel_sex <- data.frame(sort(data_fel_sex))[order(as.Date(data.frame(sort(data_fel_sex))$Var1, format="%m/%d/%Y")),]

#Arson
data_fel_ar <- subset(data_fel,OFNS_DESC=="ARSON")
data_fel_ar <- table(data_fel_ar$CMPLNT_FR_DT)
df_count_dates_fel_ar <- data.frame(sort(data_fel_ar))[order(as.Date(data.frame(sort(data_fel_ar))$Var1, format="%m/%d/%Y")),]

#Possession of stolen property
data_fel_pos <- subset(data_fel,OFNS_DESC=="POSSESSION OF STOLEN PROPERTY")
data_fel_pos <- table(data_fel_pos$CMPLNT_FR_DT)
df_count_dates_fel_pos <- data.frame(sort(data_fel_pos))[order(as.Date(data.frame(sort(data_fel_pos))$Var1, format="%m/%d/%Y")),]

#Kidnapping & Related offenses
data_fel_kidoff <- subset(data_fel,OFNS_DESC=="KIDNAPPING & RELATED OFFENSES")
data_fel_kidoff <- table(data_fel_kidoff$CMPLNT_FR_DT)
df_count_dates_fel_kidoff <- data.frame(sort(data_fel_kidoff))[order(as.Date(data.frame(sort(data_fel_kidoff))$Var1, format="%m/%d/%Y")),]

#Abortion
data_fel_abo <- subset(data_fel,OFNS_DESC=="ABORTION")
data_fel_abo <- table(data_fel_abo$CMPLNT_FR_DT)
df_count_dates_fel_abo <- data.frame(sort(data_fel_abo))[order(as.Date(data.frame(sort(data_fel_abo))$Var1, format="%m/%d/%Y")),]


#Prostitution & Related offenses
data_fel_pro <- subset(data_fel,OFNS_DESC=="PROSTITUTION & RELATED OFFENSES")
data_fel_pro <- table(data_fel_pro$CMPLNT_FR_DT)
df_count_dates_fel_pro <- data.frame(sort(data_fel_pro))[order(as.Date(data.frame(sort(data_fel_pro))$Var1, format="%m/%d/%Y")),]

#Homicide-Negligent
data_fel_hom <- subset(data_fel,OFNS_DESC=="HOMICIDE-NEGLIGENT")
data_fel_hom <- table(data_fel_hom$CMPLNT_FR_DT)
df_count_dates_fel_hom <- data.frame(sort(data_fel_hom))[order(as.Date(data.frame(sort(data_fel_hom))$Var1, format="%m/%d/%Y")),]

#Gambling
data_fel_gam <- subset(data_fel,OFNS_DESC=="GAMBLING")
data_fel_gam <- table(data_fel_gam$CMPLNT_FR_DT)
df_count_dates_fel_gam <- data.frame(sort(data_fel_gam))[order(as.Date(data.frame(sort(data_fel_gam))$Var1, format="%m/%d/%Y")),]


#Kidnapping
data_fel_kid <- subset(data_fel,OFNS_DESC=="KIDNAPPING")
data_fel_kid <- table(data_fel_kid$CMPLNT_FR_DT)
df_count_dates_fel_kid <- data.frame(sort(data_fel_kid))[order(as.Date(data.frame(sort(data_fel_kid))$Var1, format="%m/%d/%Y")),]


#Now we join
require(dplyr)
df_count_dates <- left_join(x = df_count_dates, y = df_count_dates_fel_abo, by = "Var1", all.x=TRUE) 
df_count_dates <- left_join(x = df_count_dates, y = df_count_dates_fel_ar, by = "Var1", all.x=TRUE) 
df_count_dates <- left_join(x = df_count_dates, y = df_count_dates_fel_ass, by = "Var1", all.x=TRUE) 
df_count_dates <- left_join(x = df_count_dates, y = df_count_dates_fel_bur, by = "Var1", all.x=TRUE) 
df_count_dates <- left_join(x = df_count_dates, y = df_count_dates_fel_cri, by = "Var1", all.x=TRUE) 
df_count_dates <- left_join(x = df_count_dates, y = df_count_dates_fel_dru, by = "Var1", all.x=TRUE) 
df_count_dates <- left_join(x = df_count_dates, y = df_count_dates_fel_forg, by = "Var1", all.x=TRUE) 
df_count_dates <- left_join(x = df_count_dates, y = df_count_dates_fel_gam, by = "Var1", all.x=TRUE) 
df_count_dates <- left_join(x = df_count_dates, y = df_count_dates_fel_gra, by = "Var1", all.x=TRUE) 
df_count_dates <- left_join(x = df_count_dates, y = df_count_dates_fel_graMV, by = "Var1", all.x=TRUE) 
df_count_dates <- left_join(x = df_count_dates, y = df_count_dates_fel_kid, by = "Var1", all.x=TRUE) 
df_count_dates <- left_join(x = df_count_dates, y = df_count_dates_fel_kidoff, by = "Var1", all.x=TRUE) 
df_count_dates <- left_join(x = df_count_dates, y = df_count_dates_fel_mis, by = "Var1", all.x=TRUE) 
df_count_dates <- left_join(x = df_count_dates, y = df_count_dates_fel_mur, by = "Var1", all.x=TRUE) 
df_count_dates <- left_join(x = df_count_dates, y = df_count_dates_fel_pos, by = "Var1", all.x=TRUE) 
df_count_dates <- left_join(x = df_count_dates, y = df_count_dates_fel_pro, by = "Var1", all.x=TRUE) 
df_count_dates <- left_join(x = df_count_dates, y = df_count_dates_fel_rap, by = "Var1", all.x=TRUE) 
df_count_dates <- left_join(x = df_count_dates, y = df_count_dates_fel_rob, by = "Var1", all.x=TRUE) 
df_count_dates <- left_join(x = df_count_dates, y = df_count_dates_fel_sex, by = "Var1", all.x=TRUE) 
df_count_dates <- left_join(x = df_count_dates, y = df_count_dates_fel_the, by = "Var1", all.x=TRUE) 
df_count_dates <- left_join(x = df_count_dates, y = df_count_dates_fel_wep, by = "Var1", all.x=TRUE) 


df_count_dates[is.na(df_count_dates)] <- 0

colnames(df_count_dates) <- c('Var1','total','abo','ar','ass','bur','cri','dru','forg','gam','gra',
                              'graMV','kid','kidoff','mis','Mur','pos','pro','rap',
                              'rob','sex','the','wep')


#filter the outliers dates
df_count_dates$Date <- as.Date( as.character(df_count_dates$Var1), "%m/%d/%Y")

df_count_dates <- subset(df_count_dates,as.Date(df_count_dates$Date) > "2006-01-08")

#manually include the weights from the BJS survey
df_count_dates$composite <- 8.76*df_count_dates$gra + 10.9*df_count_dates$ass + 15.66*df_count_dates$rob + 9.77*df_count_dates$bur +
  1.9*df_count_dates$mis + 6.6*df_count_dates$graMV + 12.7*df_count_dates$cri + 11.9*df_count_dates$dru + 5.3*df_count_dates$the + 
  3.03*df_count_dates$wep + 3.6*df_count_dates$forg + 25.3*df_count_dates$rap + 5.1*df_count_dates$sex + 19.97*df_count_dates$ar + 
  10.3*df_count_dates$pos + 43.73*df_count_dates$Mur + 21.2*df_count_dates$kidoff + 2.1*df_count_dates$pro +
  1.7*df_count_dates$gam + 22.85*df_count_dates$kid

#If we want to plot it
plot(df_count_dates$Date,df_count_dates$composite,type="l")
df_count_dates[which(df_count_dates$composite==max(df_count_dates$composite)),]

write_xlsx(df_count_dates,"Crimes_2006_NYC_fel_composite.xlsx")











