#Program for the estimations with different configurations with RF

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(caret)
library(gbm)
library(dplyr)
library(xgboost)
library("readxl")
require("writexl")
library(randomForest)



###################################(you can jump this part)
#Let's first estimate optimization (not really needed, differences are very small)
#One can optimize the parameters with the whole sample and then fixed them. 
#The optimization at every period was tested that didn't improve results
#since the sample is small, we train with the whole sample
w <- c(1221)

for(momento in w){

  data0<-read_xlsx(paste0("datos_Monthly_Homicides_",momento,".xlsx"),sheet="Sheet1",col_names = TRUE) 
  colnames(data0) <- c("date","X1","Hom","Hom1","Hom2","Hom3","Hom4",
                       "GA","GA1","GA2","GA3","GA4",
                       "MOH","MOH1","MOH2","MOH3","MOH4",
                       "H3","H31","H32","H33","H34", 
                       "GTH","GTH1","GTH2","GTH3","GTH4",
                       "BCs","BCs1","BCs2","BCs3","BCs4","BCs5","BCs6","BCs7",
                       "GTG","GTG1","GTG2","GTG3","GTG4",
                       "MOR","MOR1","MOR2","MOR3","MOR4",
                       "EPU","EPU1","EPU2","EPU3","EPU4",
                       "MOU","MOU1","MOU2","MOU3","MOU4",
                       "TW","TW1","TW2","TW3","TW4")
  #data$X1 <- as.Date(data$X1)
  data <- data0[min(which(data0$GA4 != 9.999900e+04)):max(which(data0$Hom != 9.999900e+04)),]
  df <- data.frame(data[,3:ncol(data)])
  #seed <- 7
  control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
  # set.seed(seed)
  metric <- "RMSE"
  modellist <- list()
  for (nodesize in c(3, 5,9,15)) {
    for (ntree in c(100,200,500,1000)) {
      rf_random <- train(Hom~., data=df, method="rf", metric=metric, tuneLength=20, trControl=control,ntree=ntree,nodesize=nodesize)
      print(ntree)
      print(nodesize)
      print(subset(data.frame(rf_random[["results"]]),mtry==rf_random$bestTune$mtry))
    }
  }
  #the different configurations for more than mmtry=35 barely change RMSE, we used 48,200 and 5 from the output. 
  
}

#####################################################
#Example estimations for the november months configurations
w <- c(1109,1110,1111,1112) #due to cities var
#w <- c(1113,1114,1115,1116)  #due to twitter var
#w <- c(1117,1118,1119) #due to GA var
#w <- c(1120,1121)

for(momento in w){
  
  data0<-read_xlsx(paste0("datos_Monthly_Homicides_",momento,".xlsx"),sheet="Sheet1",col_names = TRUE) 
  
  colnames(data0) <- c("date","X1","Hom","Hom1","Hom2","Hom3","Hom4",
                       "GA","GA1","GA2","GA3","GA4",
                       "MOH","MOH1","MOH2","MOH3","MOH4",
                       "H3","H31","H32","H33","H34", 
                       "GTH","GTH1","GTH2","GTH3","GTH4",
                       "BCs","BCs1","BCs2","BCs3","BCs4","BCs5","BCs6","BCs7",
                       "GTG","GTG1","GTG2","GTG3","GTG4",
                       "MOR","MOR1","MOR2","MOR3","MOR4",
                       "EPU","EPU1","EPU2","EPU3","EPU4",
                       "MOU","MOU1","MOU2","MOU3","MOU4",
                       "TW","TW1","TW2","TW3","TW4")
  
  data <- data0[min(which(data0$H34 != 9.999900e+04)):max(which(data0$Hom != 9.999900e+04)),]
  data <- subset(data,select = -c(GA,GA1,GA2,GA3,GA4,TW,TW1,TW2,TW3,TW4))
  
  df <- data.frame(data[,3:ncol(data)])


  #Random Search. We could again perform it, but we tested through paralell optimization at every period,
  #that the results of the out-of-sample exercise were not improved, so we fixed the parameters with those from the whole sample
  # seed <- 7
  # control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
  # set.seed(seed)
  # mtry <- sqrt(ncol(df))
  # metric <- "RMSE"
  # modellist <- list()
  # for (nodesize in c(3, 5,9,15)) {
  #   for (ntree in c(100,200,500,1000)) {
  #     rf_random <- train(Hom~., data=df, method="rf", metric=metric, tuneLength=20, trControl=control,ntree=ntree,nodesize=nodesize)
  #     print(ntree)
  #     print(nodesize)
  #     print(subset(data.frame(rf_random[["results"]]),mtry==rf_random$bestTune$mtry))
  #   }
  # }
  
  #In case mtry of higher than dimension, it resets to a valid range
  #model backcast t+1
  RF.tree=randomForest(Hom~., data = df,mtry=48, ntree=200, nodesize = 5)
    
  #We need the different configurations
  df_v1 <- subset(df, select = -c(Hom1))
  
  df_v2 <- subset(df, select = -c(Hom1,Hom2))
  
  df_v3 <- subset(df, select = -c(Hom1,Hom2,Hom3))
  
  df_v4 <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  
  df_v5 <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4,MOH,H3,GTH,BCs,GTG,MOR,EPU,MOU))
  
  
  RF.tree_v1=randomForest(Hom~., data = df_v1,mtry=48, ntree=200, nodesize = 5)
  RF.tree_v2=randomForest(Hom~., data = df_v2,mtry=48, ntree=200, nodesize = 5)
  RF.tree_v3=randomForest(Hom~., data = df_v3,mtry=48, ntree=200, nodesize = 5)
  RF.tree_v4=randomForest(Hom~., data = df_v4,mtry=48, ntree=200, nodesize = 5)
  #in this case we had to retrain, since there were less available series than previous cases, so we fixed 39,1000 and 3
  #again the differences throughout configurations is very small
  RF.tree_v5=randomForest(Hom~., data = df_v5,mtry=39, ntree=1000, nodesize = 3)
  
  
  
  data2 <- data0[,-c(2,3,8,9,10,11,12,56,57,58,59,60)]
  #df <-df[,-3]
  #df <- subset(data2,date==1.15)
  df <- subset(data2,date==as.numeric(paste0("1.",substr((momento-1),3,4))))
  df[,(1)]<- NULL
  Test_hom_1 <- predict( RF.tree, newdata=df )
  
  df <- subset(data2,date==as.numeric(paste0("2.",substr((momento-1),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1))
  Test_hom_2 <- predict( RF.tree_v1, newdata=df)
  
  df <- subset(data2,date==as.numeric(paste0("3.",substr((momento-1),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2))
  Test_hom_3 <- predict( RF.tree_v2, newdata=df )
  
  df <- subset(data2,date==as.numeric(paste0("4.",substr((momento-1),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3))
  Test_hom_4 <- predict( RF.tree_v3, newdata=df )
  
  df <- subset(data2,date==as.numeric(paste0("5.",substr((momento-1),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_5 <- predict( RF.tree_v4, newdata=df)
  
  df <- subset(data2,date==as.numeric(paste0("6.",substr((momento-1),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_6 <- predict( RF.tree_v4, newdata=df )
  
  df <- subset(data2,date==as.numeric(paste0("7.",substr((momento-1),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_7 <- predict( RF.tree_v4, newdata=df )
  
  df <- subset(data2,date==as.numeric(paste0("8.",substr((momento-1),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_8 <- predict( RF.tree_v4, newdata=df )
  
  df <- subset(data2,date==as.numeric(paste0("9.",substr((momento-1),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_9 <- predict( RF.tree_v4, newdata=df )
  
  df <- subset(data2,date==as.numeric(paste0("10.",substr((momento-1),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_10 <- predict( RF.tree_v4, newdata=df)
  
  df <- subset(data2,date==as.numeric(paste0("11.",substr((momento-1),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_11 <- predict( RF.tree_v4, newdata=df)
  
  df <- subset(data2,date==as.numeric(paste0("12.",substr((momento-1),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_12 <- predict( RF.tree_v4, newdata=df)
  
  df <- subset(data2,date==as.numeric(paste0("1.",substr((momento),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_13 <- predict( RF.tree_v4, newdata=df)
  
  df <- subset(data2,date==as.numeric(paste0("2.",substr((momento),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_14 <- predict( RF.tree_v4, newdata=df)
  
  df <- subset(data2,date==as.numeric(paste0("3.",substr((momento),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_15 <- predict( RF.tree_v4, newdata=df)
  
  df <- subset(data2,date==as.numeric(paste0("4.",substr((momento),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_16 <- predict( RF.tree_v4, newdata=df)
  
  df <- subset(data2,date==as.numeric(paste0("5.",substr((momento),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_17 <- predict( RF.tree_v4, newdata=df)
  
  df <- subset(data2,date==as.numeric(paste0("6.",substr((momento),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_18 <- predict( RF.tree_v4, newdata=df)
  
  df <- subset(data2,date==as.numeric(paste0("7.",substr((momento),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_19 <- predict( RF.tree_v4, newdata=df)
  
  df <- subset(data2,date==as.numeric(paste0("8.",substr((momento),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_20 <- predict( RF.tree_v4, newdata=df)
  
  df <- subset(data2,date==as.numeric(paste0("9.",substr((momento),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_21 <- predict( RF.tree_v4, newdata=df)
  
  df <- subset(data2,date==as.numeric(paste0("10.",substr((momento),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_22 <- predict( RF.tree_v4, newdata=df)
  
  df <- subset(data2,date==as.numeric(paste0("11.",substr((momento),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_23 <- predict( RF.tree_v4, newdata=df)
  
  df <- subset(data2,date==as.numeric(paste0("12.",substr((momento),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4,MOH,H3,GTH,BCs,GTG,MOR,EPU,MOU))
  Test_hom_24 <- predict( RF.tree_v5, newdata=df)
  
  
  
  results_date <- c(paste0("1.",(momento) - round(momento-1, -2)),paste0("2.",(momento) - round(momento-1, -2)),
                    paste0("3.",(momento) - round(momento-1, -2)),paste0("4.",(momento) - round(momento-1, -2)),
                    paste0("5.",(momento) - round(momento-1, -2)),paste0("6.",(momento) - round(momento-1, -2)),
                    paste0("7.",(momento) - round(momento-1, -2)),paste0("8.",(momento) - round(momento-1, -2)),
                    paste0("9.",(momento) - round(momento-1, -2)),paste0("10.",(momento) - round(momento-1, -2)),
                    paste0("11.",(momento) - round(momento-1, -2)),paste0("12.",(momento) - round(momento-1, -2)),
                    paste0("1.",(momento+1) - round(momento, -2)),paste0("2.",(momento+1) - round(momento, -2)),
                    paste0("3.",(momento+1) - round(momento, -2)),paste0("4.",(momento+1) - round(momento, -2)),
                    paste0("5.",(momento+1) - round(momento, -2)),paste0("6.",(momento+1) - round(momento, -2)),
                    paste0("7.",(momento+1) - round(momento, -2)),paste0("8.",(momento+1) - round(momento, -2)),
                    paste0("9.",(momento+1) - round(momento, -2)),paste0("10.",(momento+1) - round(momento, -2)),
                    paste0("11.",(momento+1) - round(momento, -2)),paste0("12.",(momento+1) - round(momento, -2)))
  results_fore <- c(Test_hom_1,Test_hom_2,Test_hom_3,Test_hom_4,Test_hom_5,Test_hom_6,Test_hom_7,Test_hom_8,Test_hom_9,Test_hom_10,Test_hom_11,Test_hom_12
                    ,Test_hom_13,Test_hom_14,Test_hom_15,Test_hom_16,Test_hom_17,Test_hom_18,Test_hom_19,Test_hom_20,Test_hom_21,Test_hom_22,Test_hom_23,Test_hom_24)
  
  write_xlsx(data.frame(results_date,results_fore),paste0("./results_",momento,".xlsx"))
  
  
}
  

##########now with the period after the variable cities appeared

w <- c(1113,1114,1115,1116)  #due to the twitter var


for(momento in w){
  
  data0<-read_xlsx(paste0("datos_Monthly_Homicides_",momento,".xlsx"),sheet="Sheet1",col_names = TRUE) 
  
  colnames(data0) <- c("date","X1","Hom","Hom1","Hom2","Hom3","Hom4",
                       "GA","GA1","GA2","GA3","GA4",
                       "MOH","MOH1","MOH2","MOH3","MOH4",
                       "H3","H31","H32","H33","H34", 
                       "GTH","GTH1","GTH2","GTH3","GTH4",
                       "BCs","BCs1","BCs2","BCs3","BCs4","BCs5","BCs6","BCs7",
                       "GTG","GTG1","GTG2","GTG3","GTG4",
                       "MOR","MOR1","MOR2","MOR3","MOR4",
                       "EPU","EPU1","EPU2","EPU3","EPU4",
                       "MOU","MOU1","MOU2","MOU3","MOU4",
                       "TW","TW1","TW2","TW3","TW4")

data <- data0[min(which(data0$TW4 != 9.999900e+04)):max(which(data0$Hom != 9.999900e+04)),]
data <- subset(data,select = -c(GA,GA1,GA2,GA3,GA4))

df <- data.frame(data[,3:ncol(data)])

#model backcast t+1
RF.tree=randomForest(Hom~., data = df,mtry=48, ntree=200, nodesize = 5)

#We need the different configurations
df_v1 <- subset(df, select = -c(Hom1))

df_v2 <- subset(df, select = -c(Hom1,Hom2))

df_v3 <- subset(df, select = -c(Hom1,Hom2,Hom3))

df_v4 <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))

df_v5 <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4,MOH,H3,GTH,BCs,GTG,MOR,EPU,MOU,TW))


RF.tree_v1=randomForest(Hom~., data = df_v1,mtry=48, ntree=200, nodesize = 5)
RF.tree_v2=randomForest(Hom~., data = df_v2,mtry=48, ntree=200, nodesize = 5)
RF.tree_v3=randomForest(Hom~., data = df_v3,mtry=48, ntree=200, nodesize = 5)
RF.tree_v4=randomForest(Hom~., data = df_v4,mtry=48, ntree=200, nodesize = 5)
#in this case we had to retrain, since there were less available series than previous cases
RF.tree_v5=randomForest(Hom~., data = df_v5,mtry=39, ntree=1000, nodesize = 3)


data2 <- data0[,-c(2,3,8,9,10,11,12)]
#df <-df[,-3]
#df <- subset(data2,date==1.15)
df <- subset(data2,date==as.numeric(paste0("1.",substr((momento-1),3,4))))
df[,(1)]<- NULL
Test_hom_1 <- predict( RF.tree, newdata=df )

df <- subset(data2,date==as.numeric(paste0("2.",substr((momento-1),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1))
Test_hom_2 <- predict( RF.tree_v1, newdata=df)

df <- subset(data2,date==as.numeric(paste0("3.",substr((momento-1),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2))
Test_hom_3 <- predict( RF.tree_v2, newdata=df )

df <- subset(data2,date==as.numeric(paste0("4.",substr((momento-1),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3))
Test_hom_4 <- predict( RF.tree_v3, newdata=df )

df <- subset(data2,date==as.numeric(paste0("5.",substr((momento-1),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
Test_hom_5 <- predict( RF.tree_v4, newdata=df)

df <- subset(data2,date==as.numeric(paste0("6.",substr((momento-1),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
Test_hom_6 <- predict( RF.tree_v4, newdata=df )

df <- subset(data2,date==as.numeric(paste0("7.",substr((momento-1),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
Test_hom_7 <- predict( RF.tree_v4, newdata=df )

df <- subset(data2,date==as.numeric(paste0("8.",substr((momento-1),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
Test_hom_8 <- predict( RF.tree_v4, newdata=df )

df <- subset(data2,date==as.numeric(paste0("9.",substr((momento-1),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
Test_hom_9 <- predict( RF.tree_v4, newdata=df )

df <- subset(data2,date==as.numeric(paste0("10.",substr((momento-1),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
Test_hom_10 <- predict( RF.tree_v4, newdata=df)

df <- subset(data2,date==as.numeric(paste0("11.",substr((momento-1),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
Test_hom_11 <- predict( RF.tree_v4, newdata=df)

df <- subset(data2,date==as.numeric(paste0("12.",substr((momento-1),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
Test_hom_12 <- predict( RF.tree_v4, newdata=df)

df <- subset(data2,date==as.numeric(paste0("1.",substr((momento),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
Test_hom_13 <- predict( RF.tree_v4, newdata=df)

df <- subset(data2,date==as.numeric(paste0("2.",substr((momento),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
Test_hom_14 <- predict( RF.tree_v4, newdata=df)

df <- subset(data2,date==as.numeric(paste0("3.",substr((momento),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
Test_hom_15 <- predict( RF.tree_v4, newdata=df)

df <- subset(data2,date==as.numeric(paste0("4.",substr((momento),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
Test_hom_16 <- predict( RF.tree_v4, newdata=df)

df <- subset(data2,date==as.numeric(paste0("5.",substr((momento),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
Test_hom_17 <- predict( RF.tree_v4, newdata=df)

df <- subset(data2,date==as.numeric(paste0("6.",substr((momento),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
Test_hom_18 <- predict( RF.tree_v4, newdata=df)

df <- subset(data2,date==as.numeric(paste0("7.",substr((momento),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
Test_hom_19 <- predict( RF.tree_v4, newdata=df)

df <- subset(data2,date==as.numeric(paste0("8.",substr((momento),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
Test_hom_20 <- predict( RF.tree_v4, newdata=df)

df <- subset(data2,date==as.numeric(paste0("9.",substr((momento),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
Test_hom_21 <- predict( RF.tree_v4, newdata=df)

df <- subset(data2,date==as.numeric(paste0("10.",substr((momento),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
Test_hom_22 <- predict( RF.tree_v4, newdata=df)

df <- subset(data2,date==as.numeric(paste0("11.",substr((momento),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
Test_hom_23 <- predict( RF.tree_v4, newdata=df)

df <- subset(data2,date==as.numeric(paste0("12.",substr((momento),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4,MOH,H3,GTH,BCs,GTG,MOR,EPU,MOU,TW))
Test_hom_24 <- predict( RF.tree_v5, newdata=df)



results_date <- c(paste0("1.",(momento) - round(momento-1, -2)),paste0("2.",(momento) - round(momento-1, -2)),
                  paste0("3.",(momento) - round(momento-1, -2)),paste0("4.",(momento) - round(momento-1, -2)),
                  paste0("5.",(momento) - round(momento-1, -2)),paste0("6.",(momento) - round(momento-1, -2)),
                  paste0("7.",(momento) - round(momento-1, -2)),paste0("8.",(momento) - round(momento-1, -2)),
                  paste0("9.",(momento) - round(momento-1, -2)),paste0("10.",(momento) - round(momento-1, -2)),
                  paste0("11.",(momento) - round(momento-1, -2)),paste0("12.",(momento) - round(momento-1, -2)),
                  paste0("1.",(momento+1) - round(momento, -2)),paste0("2.",(momento+1) - round(momento, -2)),
                  paste0("3.",(momento+1) - round(momento, -2)),paste0("4.",(momento+1) - round(momento, -2)),
                  paste0("5.",(momento+1) - round(momento, -2)),paste0("6.",(momento+1) - round(momento, -2)),
                  paste0("7.",(momento+1) - round(momento, -2)),paste0("8.",(momento+1) - round(momento, -2)),
                  paste0("9.",(momento+1) - round(momento, -2)),paste0("10.",(momento+1) - round(momento, -2)),
                  paste0("11.",(momento+1) - round(momento, -2)),paste0("12.",(momento+1) - round(momento, -2)))
results_fore <- c(Test_hom_1,Test_hom_2,Test_hom_3,Test_hom_4,Test_hom_5,Test_hom_6,Test_hom_7,Test_hom_8,Test_hom_9,Test_hom_10,Test_hom_11,Test_hom_12
                  ,Test_hom_13,Test_hom_14,Test_hom_15,Test_hom_16,Test_hom_17,Test_hom_18,Test_hom_19,Test_hom_20,Test_hom_21,Test_hom_22,Test_hom_23,Test_hom_24)

write_xlsx(data.frame(results_date,results_fore),paste0("./results_",momento,".xlsx"))

}



######next after Guns Archive appear

w <- c(1117,1118,1119) #due to GA


for(momento in w){
  
  data0<-read_xlsx(paste0("datos_Monthly_Homicides_",momento,".xlsx"),sheet="Sheet1",col_names = TRUE) 
  
  colnames(data0) <- c("date","X1","Hom","Hom1","Hom2","Hom3","Hom4",
                       "GA","GA1","GA2","GA3","GA4",
                       "MOH","MOH1","MOH2","MOH3","MOH4",
                       "H3","H31","H32","H33","H34", 
                       "GTH","GTH1","GTH2","GTH3","GTH4",
                       "BCs","BCs1","BCs2","BCs3","BCs4","BCs5","BCs6","BCs7",
                       "GTG","GTG1","GTG2","GTG3","GTG4",
                       "MOR","MOR1","MOR2","MOR3","MOR4",
                       "EPU","EPU1","EPU2","EPU3","EPU4",
                       "MOU","MOU1","MOU2","MOU3","MOU4",
                       "TW","TW1","TW2","TW3","TW4")

  
  data <- data0[min(which(data0$GA4 != 9.999900e+04)):max(which(data0$Hom != 9.999900e+04)),]
  
  df <- data.frame(data[,3:ncol(data)])
  
  
  
  RF.tree=randomForest(Hom~., data = df,mtry=48, ntree=200, nodesize = 5)
  
  #We need the different configurations
  df_v1 <- subset(df, select = -c(Hom1))
  
  df_v2 <- subset(df, select = -c(Hom1,Hom2))
  
  df_v3 <- subset(df, select = -c(Hom1,Hom2,Hom3))
  
  df_v4 <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  
  df_v5 <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4,GA,MOH,H3,GTH,BCs,GTG,MOR,EPU,MOU,TW))
  
  
  RF.tree_v1=randomForest(Hom~., data = df_v1,mtry=48, ntree=200, nodesize = 5)
  RF.tree_v2=randomForest(Hom~., data = df_v2,mtry=48, ntree=200, nodesize = 5)
  RF.tree_v3=randomForest(Hom~., data = df_v3,mtry=48, ntree=200, nodesize = 5)
  RF.tree_v4=randomForest(Hom~., data = df_v4,mtry=48, ntree=200, nodesize = 5)
  #in this case we had to retrain, since there were less available series than previous cases
  RF.tree_v5=randomForest(Hom~., data = df_v5,mtry=39, ntree=1000, nodesize = 3)
  
  
  
  data2 <- data0[,-c(2,3)]
  #df <-df[,-3]
  #df <- subset(data2,date==1.15)
  df <- subset(data2,date==as.numeric(paste0("1.",substr((momento-1),3,4))))
  df[,(1)]<- NULL
  Test_hom_1 <- predict( RF.tree, newdata=df )
  
  df <- subset(data2,date==as.numeric(paste0("2.",substr((momento-1),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1))
  Test_hom_2 <- predict( RF.tree_v1, newdata=df)
  
  df <- subset(data2,date==as.numeric(paste0("3.",substr((momento-1),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2))
  Test_hom_3 <- predict( RF.tree_v2, newdata=df )
  
  df <- subset(data2,date==as.numeric(paste0("4.",substr((momento-1),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3))
  Test_hom_4 <- predict( RF.tree_v3, newdata=df )
  
  df <- subset(data2,date==as.numeric(paste0("5.",substr((momento-1),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_5 <- predict( RF.tree_v4, newdata=df)
  
  df <- subset(data2,date==as.numeric(paste0("6.",substr((momento-1),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_6 <- predict( RF.tree_v4, newdata=df )
  
  df <- subset(data2,date==as.numeric(paste0("7.",substr((momento-1),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_7 <- predict( RF.tree_v4, newdata=df )
  
  df <- subset(data2,date==as.numeric(paste0("8.",substr((momento-1),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_8 <- predict( RF.tree_v4, newdata=df )
  
  df <- subset(data2,date==as.numeric(paste0("9.",substr((momento-1),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_9 <- predict( RF.tree_v4, newdata=df )
  
  df <- subset(data2,date==as.numeric(paste0("10.",substr((momento-1),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_10 <- predict( RF.tree_v4, newdata=df)
  
  df <- subset(data2,date==as.numeric(paste0("11.",substr((momento-1),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_11 <- predict( RF.tree_v4, newdata=df)
  
  df <- subset(data2,date==as.numeric(paste0("12.",substr((momento-1),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_12 <- predict( RF.tree_v4, newdata=df)
  
  df <- subset(data2,date==as.numeric(paste0("1.",substr((momento),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_13 <- predict( RF.tree_v4, newdata=df)
  
  df <- subset(data2,date==as.numeric(paste0("2.",substr((momento),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_14 <- predict( RF.tree_v4, newdata=df)
  
  df <- subset(data2,date==as.numeric(paste0("3.",substr((momento),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_15 <- predict( RF.tree_v4, newdata=df)
  
  df <- subset(data2,date==as.numeric(paste0("4.",substr((momento),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_16 <- predict( RF.tree_v4, newdata=df)
  
  df <- subset(data2,date==as.numeric(paste0("5.",substr((momento),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_17 <- predict( RF.tree_v4, newdata=df)
  
  df <- subset(data2,date==as.numeric(paste0("6.",substr((momento),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_18 <- predict( RF.tree_v4, newdata=df)
  
  df <- subset(data2,date==as.numeric(paste0("7.",substr((momento),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_19 <- predict( RF.tree_v4, newdata=df)
  
  df <- subset(data2,date==as.numeric(paste0("8.",substr((momento),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_20 <- predict( RF.tree_v4, newdata=df)
  
  df <- subset(data2,date==as.numeric(paste0("9.",substr((momento),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_21 <- predict( RF.tree_v4, newdata=df)
  
  df <- subset(data2,date==as.numeric(paste0("10.",substr((momento),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_22 <- predict( RF.tree_v4, newdata=df)
  
  df <- subset(data2,date==as.numeric(paste0("11.",substr((momento),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  Test_hom_23 <- predict( RF.tree_v4, newdata=df)
  
  df <- subset(data2,date==as.numeric(paste0("12.",substr((momento),3,4))))
  df[,(1)]<- NULL
  df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4,GA,MOH,H3,GTH,BCs,GTG,MOR,EPU,MOU,TW))
  Test_hom_24 <- predict( RF.tree_v5, newdata=df)
  
  
  #colnames(df)<- c("GA","MO_G/F","Ciudades","GT_Hom","Bcs")
  
  
  
  
  results_date <- c(paste0("1.",(momento) - round(momento-1, -2)),paste0("2.",(momento) - round(momento-1, -2)),
                    paste0("3.",(momento) - round(momento-1, -2)),paste0("4.",(momento) - round(momento-1, -2)),
                    paste0("5.",(momento) - round(momento-1, -2)),paste0("6.",(momento) - round(momento-1, -2)),
                    paste0("7.",(momento) - round(momento-1, -2)),paste0("8.",(momento) - round(momento-1, -2)),
                    paste0("9.",(momento) - round(momento-1, -2)),paste0("10.",(momento) - round(momento-1, -2)),
                    paste0("11.",(momento) - round(momento-1, -2)),paste0("12.",(momento) - round(momento-1, -2)),
                    paste0("1.",(momento+1) - round(momento, -2)),paste0("2.",(momento+1) - round(momento, -2)),
                    paste0("3.",(momento+1) - round(momento, -2)),paste0("4.",(momento+1) - round(momento, -2)),
                    paste0("5.",(momento+1) - round(momento, -2)),paste0("6.",(momento+1) - round(momento, -2)),
                    paste0("7.",(momento+1) - round(momento, -2)),paste0("8.",(momento+1) - round(momento, -2)),
                    paste0("9.",(momento+1) - round(momento, -2)),paste0("10.",(momento+1) - round(momento, -2)),
                    paste0("11.",(momento+1) - round(momento, -2)),paste0("12.",(momento+1) - round(momento, -2)))
  results_fore <- c(Test_hom_1,Test_hom_2,Test_hom_3,Test_hom_4,Test_hom_5,Test_hom_6,Test_hom_7,Test_hom_8,Test_hom_9,Test_hom_10,Test_hom_11,Test_hom_12
                    ,Test_hom_13,Test_hom_14,Test_hom_15,Test_hom_16,Test_hom_17,Test_hom_18,Test_hom_19,Test_hom_20,Test_hom_21,Test_hom_22,Test_hom_23,Test_hom_24)
  
  
  write_xlsx(data.frame(results_date,results_fore),paste0("./results_",momento,".xlsx"))
  
}



#Final, due to imputed quarterly series

w <- c(1120,1121)   


for(momento in w){
  
  data0<-read_xlsx(paste0("datos_Monthly_Homicides_",momento,".xlsx"),sheet="Sheet1",col_names = TRUE) 
  
#colnames(data0) <- c("date","X1","Hom","GA", "MOH","H3", "GTH","BCs")
colnames(data0) <- c("date","X1","Hom","Hom1","Hom2","Hom3","Hom4",
                     "GA","GA1","GA2","GA3","GA4",
                     "MOH","MOH1","MOH2","MOH3","MOH4",
                     "H3","H31","H32","H33","H34", 
                     "GTH","GTH1","GTH2","GTH3","GTH4",
                     "BCs","BCs1","BCs2","BCs3","BCs4","BCs5","BCs6","BCs7",
                     "GTG","GTG1","GTG2","GTG3","GTG4",
                     "MOR","MOR1","MOR2","MOR3","MOR4",
                     "EPU","EPU1","EPU2","EPU3","EPU4",
                     "MOU","MOU1","MOU2","MOU3","MOU4",
                     "TW","TW1","TW2","TW3","TW4")

data <- data0[min(which(data0$X1 != 9.999900e+04)):max(which(data0$Hom != 9.999900e+04)),]
#data <- data0[min(which(data0$X1 != 9.999900e+04)):max(which(data0$Hom != 9.999900e+04)),]

df <- data.frame(data[,2:ncol(data)])
colnames(df) <- colnames(data0)[-c(1)]

RF.tree=randomForest(Hom~., data = df,mtry=48, ntree=200, nodesize = 5)

#We need the different configurations
df_v1 <- subset(df, select = -c(Hom1))

df_v2 <- subset(df, select = -c(Hom1,Hom2))

df_v3 <- subset(df, select = -c(Hom1,Hom2,Hom3))

df_v4 <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
df_v4notri <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4,X1))  #configuration without quarterly imputed variable

df_v5 <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4,X1,GA,MOH,H3,GTH,BCs,GTG,MOR,EPU,MOU,TW))


RF.tree_v1=randomForest(Hom~., data = df_v1,mtry=48, ntree=200, nodesize = 5)
RF.tree_v2=randomForest(Hom~., data = df_v2,mtry=48, ntree=200, nodesize = 5)
RF.tree_v3=randomForest(Hom~., data = df_v3,mtry=48, ntree=200, nodesize = 5)
RF.tree_v4=randomForest(Hom~., data = df_v4,mtry=48, ntree=200, nodesize = 5)
RF.tree_v4notri=randomForest(Hom~., data = df_v4notri,mtry=48, ntree=200, nodesize = 5)
#in this case we had to retrain, since there were less available series than previous cases
RF.tree_v5=randomForest(Hom~., data = df_v5,mtry=39, ntree=1000, nodesize = 3)



data2 <- data0[,-c(3)]
#df <-df[,-3]
#df <- subset(data2,date==1.15)
df <- subset(data2,date==as.numeric(paste0("1.",substr((momento-1),3,4))))
df[,(1)]<- NULL
Test_hom_1 <- predict( RF.tree, newdata=df )

df <- subset(data2,date==as.numeric(paste0("2.",substr((momento-1),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1))
Test_hom_2 <- predict( RF.tree_v1, newdata=df)

df <- subset(data2,date==as.numeric(paste0("3.",substr((momento-1),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2))
Test_hom_3 <- predict( RF.tree_v2, newdata=df )

df <- subset(data2,date==as.numeric(paste0("4.",substr((momento-1),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3))
Test_hom_4 <- predict( RF.tree_v3, newdata=df )

df <- subset(data2,date==as.numeric(paste0("5.",substr((momento-1),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
Test_hom_5 <- predict( RF.tree_v4, newdata=df)

df <- subset(data2,date==as.numeric(paste0("6.",substr((momento-1),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
Test_hom_6 <- predict( RF.tree_v4, newdata=df )

df <- subset(data2,date==as.numeric(paste0("7.",substr((momento-1),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
Test_hom_7 <- predict( RF.tree_v4, newdata=df )

df <- subset(data2,date==as.numeric(paste0("8.",substr((momento-1),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
Test_hom_8 <- predict( RF.tree_v4, newdata=df )

df <- subset(data2,date==as.numeric(paste0("9.",substr((momento-1),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
Test_hom_9 <- predict( RF.tree_v4, newdata=df )

df <- subset(data2,date==as.numeric(paste0("10.",substr((momento-1),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
Test_hom_10 <- predict( RF.tree_v4, newdata=df)

df <- subset(data2,date==as.numeric(paste0("11.",substr((momento-1),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
Test_hom_11 <- predict( RF.tree_v4, newdata=df)

df <- subset(data2,date==as.numeric(paste0("12.",substr((momento-1),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
Test_hom_12 <- predict( RF.tree_v4, newdata=df)

df <- subset(data2,date==as.numeric(paste0("1.",substr((momento),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4,X1))
Test_hom_13 <- predict( RF.tree_v4notri, newdata=df)

df <- subset(data2,date==as.numeric(paste0("2.",substr((momento),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4,X1))
Test_hom_14 <- predict( RF.tree_v4notri, newdata=df)

df <- subset(data2,date==as.numeric(paste0("3.",substr((momento),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4,X1))
Test_hom_15 <- predict( RF.tree_v4notri, newdata=df)

df <- subset(data2,date==as.numeric(paste0("4.",substr((momento),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4,X1))
Test_hom_16 <- predict( RF.tree_v4notri, newdata=df)

df <- subset(data2,date==as.numeric(paste0("5.",substr((momento),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4,X1))
Test_hom_17 <- predict( RF.tree_v4notri, newdata=df)

df <- subset(data2,date==as.numeric(paste0("6.",substr((momento),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4,X1))
Test_hom_18 <- predict( RF.tree_v4notri, newdata=df)

df <- subset(data2,date==as.numeric(paste0("7.",substr((momento),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4,X1))
Test_hom_19 <- predict( RF.tree_v4notri, newdata=df)

df <- subset(data2,date==as.numeric(paste0("8.",substr((momento),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4,X1))
Test_hom_20 <- predict( RF.tree_v4notri, newdata=df)

df <- subset(data2,date==as.numeric(paste0("9.",substr((momento),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4,X1))
Test_hom_21 <- predict( RF.tree_v4notri, newdata=df)

df <- subset(data2,date==as.numeric(paste0("10.",substr((momento),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4,X1))
Test_hom_22 <- predict( RF.tree_v4notri, newdata=df)

df <- subset(data2,date==as.numeric(paste0("11.",substr((momento),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4,X1))
Test_hom_23 <- predict( RF.tree_v4notri, newdata=df)

df <- subset(data2,date==as.numeric(paste0("12.",substr((momento),3,4))))
df[,(1)]<- NULL
df <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4,X1,GA,MOH,H3,GTH,BCs,GTG,MOR,EPU,MOU,TW))
Test_hom_24 <- predict( RF.tree_v5, newdata=df)


#colnames(df)<- c("GA","MO_G/F","Ciudades","GT_Hom","Bcs")




results_date <- c(paste0("1.",(momento) - round(momento-1, -2)),paste0("2.",(momento) - round(momento-1, -2)),
                  paste0("3.",(momento) - round(momento-1, -2)),paste0("4.",(momento) - round(momento-1, -2)),
                  paste0("5.",(momento) - round(momento-1, -2)),paste0("6.",(momento) - round(momento-1, -2)),
                  paste0("7.",(momento) - round(momento-1, -2)),paste0("8.",(momento) - round(momento-1, -2)),
                  paste0("9.",(momento) - round(momento-1, -2)),paste0("10.",(momento) - round(momento-1, -2)),
                  paste0("11.",(momento) - round(momento-1, -2)),paste0("12.",(momento) - round(momento-1, -2)),
                  paste0("1.",(momento+1) - round(momento, -2)),paste0("2.",(momento+1) - round(momento, -2)),
                  paste0("3.",(momento+1) - round(momento, -2)),paste0("4.",(momento+1) - round(momento, -2)),
                  paste0("5.",(momento+1) - round(momento, -2)),paste0("6.",(momento+1) - round(momento, -2)),
                  paste0("7.",(momento+1) - round(momento, -2)),paste0("8.",(momento+1) - round(momento, -2)),
                  paste0("9.",(momento+1) - round(momento, -2)),paste0("10.",(momento+1) - round(momento, -2)),
                  paste0("11.",(momento+1) - round(momento, -2)),paste0("12.",(momento+1) - round(momento, -2)))
results_fore <- c(Test_hom_1,Test_hom_2,Test_hom_3,Test_hom_4,Test_hom_5,Test_hom_6,Test_hom_7,Test_hom_8,Test_hom_9,Test_hom_10,Test_hom_11,Test_hom_12
                  ,Test_hom_13,Test_hom_14,Test_hom_15,Test_hom_16,Test_hom_17,Test_hom_18,Test_hom_19,Test_hom_20,Test_hom_21,Test_hom_22,Test_hom_23,Test_hom_24)

write_xlsx(data.frame(results_date,results_fore),paste0("./results_",momento,".xlsx"))


}



