setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(xlsx)
data <- openxlsx::read.xlsx("times_series_original_&_sa_det.xlsx",1)

#We can take a look on some tests for stationarity

#library(tseries)
#adf.test(data$meanneg) #for unit roots
#kpss.test(data$meanneg)
#library(Kendall) #to see trends
#MannKendall(data$meanneg)
library(tidyr)
data <- subset(data,select=c("Date","Twitter.number.Homicide"))   #choose the variable needed. 
#we can do all within a loop also
data <-drop_na(data)
library(pracma)

#We detrend a linear trend from the series (if not needed, is just a change in level):
x <- detrend(as.numeric(data$Twitter.number.Homicide),tt="linear")

##############################################################################
#We can take a look on the spectral density if we want to specify some frequencies.
#In our case, we use always monthly, but we could specify several:

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

#########################################################################33
#Now we adjust STL with multiple seasons, with the package forecast
library(forecast)
#we specify  monthly 
x_2 <- msts(x, seasonal.periods=c(12))
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

#writexl::write_xlsx(df,"Tw_Hom_sa_det.xlsx")










