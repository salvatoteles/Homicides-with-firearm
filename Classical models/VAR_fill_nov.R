#https://www.r-bloggers.com/2021/11/vector-autoregressive-model-var-using-r/

#predecimos una unidad a futuro, y a la siguiente le pasamos el valor real de las observaciones
rm(list=ls())

library(vars)
library(tsDyn)
library(panelvar)
library(plm)
library(Spillover)
library("readxl")

#w <- c(0119,0110,0111,0112,0113,0114,0115,0116)
w <- c(1109,1110,1111,1112,1113,1114,1115,1116,1117,1118,1119,1120,1121)
w1 <- c(1118,1119,1120,1121)


for (momento in w){

data0 <- read_xlsx(paste0("data_Monthly_Homicides_",momento,".xlsx"),sheet=1,col_names = TRUE) 

data0 <- data0[,c("X1","Quarterly","Monthly.HomFi_PopEst_sa","Homicide_Guns_Archive_sa", "MO_Homic&(G_or_F)" ,"Ciudades", "GT_Homicides_sa_det","BCs")]
colnames(data0) <- c("date","X1","Hom","GA", "MOH","H3", "GTH","BCs")
data <- data0[min(which(data0$H3 != 9.999900e+04)):max(which(data0$Hom != 9.999900e+04)),]

df <- data.frame(data[,2:8])

colnames(df) <- c("X1","Hom","GA", "MOH","H3", "GTH","BCs")
#df <-df[,-3]
df <-df[,-c(1,3)]

VARselect(df, lag.max = 3,type='const')

vartsDyn<- lineVar(df, lag = 3)

nhor <- 23   #changes
for (i in 1:nhor){

df_pred <- tail(df,3)

fore_v2 <- predict(vartsDyn,df_pred,n.ahead = 1)
df_new <- data0[(max(which(data0$Hom != 9.999900e+04))+i),]  #addition changes
df_new$Hom <- fore_v2[1]
df_new <- data.frame(df_new[,2:8])
df_new <-df_new[,-c(1,3)]



df <- rbind(df,df_new)

}


var.pred <- predict(vartsDyn,tail(df,3),n.ahead = 24-nhor)

fore_v2 <- c(tail(df$Hom,nhor),var.pred[,1])

results_date <- c(paste0("1.",(momento-1) - round(momento-1, -2)),paste0("2.",(momento-1) - round(momento-1, -2)),
                  paste0("3.",(momento-1) - round(momento-1, -2)),paste0("4.",(momento-1) - round(momento-1, -2)),
                  paste0("5.",(momento-1) - round(momento-1, -2)),paste0("6.",(momento-1) - round(momento-1, -2)),
                  paste0("7.",(momento-1) - round(momento-1, -2)),paste0("8.",(momento-1) - round(momento-1, -2)),
                  paste0("9.",(momento-1) - round(momento-1, -2)),paste0("10.",(momento-1) - round(momento-1, -2)),
                  paste0("11.",(momento-1) - round(momento-1, -2)),paste0("12.",(momento-1) - round(momento-1, -2)),
                  paste0("1.",(momento) - round(momento, -2)),paste0("2.",(momento) - round(momento, -2)),
                  paste0("3.",(momento) - round(momento, -2)),paste0("4.",(momento) - round(momento, -2)),
                  paste0("5.",(momento) - round(momento, -2)),paste0("6.",(momento) - round(momento, -2)),
                  paste0("7.",(momento) - round(momento, -2)),paste0("8.",(momento) - round(momento, -2)),
                  paste0("9.",(momento) - round(momento, -2)),paste0("10.",(momento) - round(momento, -2)),
                  paste0("11.",(momento) - round(momento, -2)),paste0("12.",(momento) - round(momento, -2)))

library(xlsx)
write.xlsx(data.frame(results_date,fore_v2),paste0("results_",momento,".xlsx"))

}






for (momento1 in w1){
        
        data0 <- read_xlsx(paste0("data_Monthly_Homicides_",momento,".xlsx"),sheet=1,col_names = TRUE) 
        
        data0 <- data0[,c("X1","Quarterly","Monthly.HomFi_PopEst_sa","Homicide_Guns_Archive_sa", "MO_Homic&(G_or_F)" ,"Ciudades", "GT_Homicides_sa_det","BCs")]
        colnames(data0) <- c("date","X1","Hom","GA", "MOH","H3", "GTH","BCs")
        data <- data0[min(which(data0$GA != 9.999900e+04)):max(which(data0$Hom != 9.999900e+04)),]
        
        df <- data.frame(data[,2:8])
        
        colnames(df) <- c("X1","Hom","GA", "MOH","H3", "GTH","BCs")
        #df <-df[,-3]
        df <-df[,-c(1)]
        
        VARselect(df, lag.max = 3,type='const')
        
        vartsDyn<- lineVar(df, lag = 3)
        
        nhor <- 23  #nhor changes 
        for (j in 1:nhor){
                
                df_pred <- tail(df,3)
                
                fore_v2 <- predict(vartsDyn,df_pred,n.ahead = 1)
                df_new <- data0[(max(which(data0$Hom != 9.999900e+04))+j),] #the add changes
                df_new$Hom <- fore_v2[1]
                df_new <- data.frame(df_new[,2:8])
                df_new <-df_new[,-c(1)]
                
                
                
                df <- rbind(df,df_new)
                
        }
        
        
        var.pred <- predict(vartsDyn,tail(df,3),n.ahead = 24-nhor)
        
        fore_v2 <- c(tail(df$Hom,nhor),var.pred[,1])
        
        results_date <- c(paste0("1.",(momento1-1) - round(momento1-1, -2)),paste0("2.",(momento1-1) - round(momento1-1, -2)),
                          paste0("3.",(momento1-1) - round(momento1-1, -2)),paste0("4.",(momento1-1) - round(momento1-1, -2)),
                          paste0("5.",(momento1-1) - round(momento1-1, -2)),paste0("6.",(momento1-1) - round(momento1-1, -2)),
                          paste0("7.",(momento1-1) - round(momento1-1, -2)),paste0("8.",(momento1-1) - round(momento1-1, -2)),
                          paste0("9.",(momento1-1) - round(momento1-1, -2)),paste0("10.",(momento1-1) - round(momento1-1, -2)),
                          paste0("11.",(momento1-1) - round(momento1-1, -2)),paste0("12.",(momento1-1) - round(momento1-1, -2)),
                          paste0("1.",(momento1) - round(momento1, -2)),paste0("2.",(momento1) - round(momento1, -2)),
                          paste0("3.",(momento1) - round(momento1, -2)),paste0("4.",(momento1) - round(momento1, -2)),
                          paste0("5.",(momento1) - round(momento1, -2)),paste0("6.",(momento1) - round(momento1, -2)),
                          paste0("7.",(momento1) - round(momento1, -2)),paste0("8.",(momento1) - round(momento1, -2)),
                          paste0("9.",(momento1) - round(momento1, -2)),paste0("10.",(momento1) - round(momento1, -2)),
                          paste0("11.",(momento1) - round(momento1, -2)),paste0("12.",(momento1) - round(momento1, -2)))
        
        library(xlsx)
        write.xlsx(data.frame(results_date,fore_v2),paste0("results_",momento1,"conGA",".xlsx"))
        
}






