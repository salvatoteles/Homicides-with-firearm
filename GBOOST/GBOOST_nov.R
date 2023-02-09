#code for GBOOST estimation for every november, taking into account their configurations
#and letting fixed the parameters as with the whole sample optimization
#other approaches as multivariate boost and optimization at every period yielded similar results

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(caret)
library(gbm)
library(dplyr)
library(xgboost)

###################################(you can jump this part)
#As in RF, let's first estimate optimization (not really needed, again the differences are very small)
#One can also here optimize the parameters with the whole sample and then fixed them. 
#The optimization at every period was tested that didn't improve results
#since the sample is small, we train with the whole sample
w <- c(1221)

for(momento in w){
  
  data0<-read_xlsx(paste0("data_Monthly_Homicides_",momento,".xlsx"),sheet="Sheet1",col_names = TRUE) 
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
  seed <- 7
  set.seed(seed)
  
  validation <- trainControl(## 10-fold CV
    method = "cv",
   number = 10)

  tuning_grid <-  expand.grid(interaction.depth = c(3, 5,8),
                              n.trees = c(20, 40, 60, 150,200,250,350,500,700,1000),
                              shrinkage = c(0.5, 0.1, 0.01),
                              n.minobsinnode = c(2,3,4))

  best_model <- train(Hom~., data = df, 
                        method = "gbm",
                        trControl = validation,
                        verbose = FALSE,
                        tuneGrid = tuning_grid)

  best_model$bestTune
  #the different configurations later barely change RMSE, we used 20,3,0.5 and 3 from the output. 
  
}

#####################################################
#Example estimations for the november months configurations
w <- c(1109,1110,1111,1112) #due to cities var
#w <- c(1113,1114,1115,1116)  #due to twitter var
#w <- c(1117,1118,1119) #due to GA var
#w <- c(1120,1121)

for(momento in w){
  
  data0<-read_xlsx(paste0("data_Monthly_Homicides_",momento,".xlsx"),sheet="Sheet1",col_names = TRUE) 
  
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
  
  
  GB.tree=gbm(Hom~., df,
              distribution="gaussian",            # Determines the loss function: "adaboost" for binary responses that use exponential function of AdaBoost algorithm
              n.trees=20,                       # Number of iterations (= number of trees) in boosting algorithm
              interaction.depth=3,                # Number of nodes in the trees
              shrinkage=0.5,                     # Learning rate that controls the number of steps in the algorithm
              #bag.fraction=0.80,                  # Proportion of obs in random training set. If it is 1 uses Gradient Boosting while if it is < 1 uses Stochastic Gradient Boosting
              #train.fraction=1.0,                 # The first train.fraction*nrows(data) observations are used to fit the gbm and the remainder are used for computing out-of-sample estimates of the loss function.We keep it 1.0 and then we divide train test the sample
              #cv.folds=5,                         # Number of cross validation to obtain the error as a function of the number of trees. Estimate the generalization error returned in cv.error
              n.minobsinnode=2,                 # Minimum number of obs in a node to allow splitting
              n.cores=1 )                         # Number of CPU cores to use
  
  
  #We need the different configurations
  df_v1 <- subset(df, select = -c(Hom1))
  
  df_v2 <- subset(df, select = -c(Hom1,Hom2))
  
  df_v3 <- subset(df, select = -c(Hom1,Hom2,Hom3))
  
  df_v4 <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  
  df_v5 <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4,MOH,H3,GTH,BCs,GTG,MOR,EPU,MOU))
  
  
  GB.tree_v1=gbm(Hom~., df_v1, distribution="gaussian",n.trees=20,interaction.depth=3,shrinkage=0.5,n.minobsinnode=3,n.cores=1 )
  
  
  GB.tree_v2=gbm(Hom~., df_v2, distribution="gaussian",n.trees=20,interaction.depth=3,shrinkage=0.5,n.minobsinnode=3,n.cores=1 )
  
  GB.tree_v3=gbm(Hom~., df_v3, distribution="gaussian",n.trees=20,interaction.depth=3,shrinkage=0.5,n.minobsinnode=3,n.cores=1 )
  
  
  GB.tree_v4=gbm(Hom~., df_v4, distribution="gaussian",n.trees=20,interaction.depth=3,shrinkage=0.5,n.minobsinnode=3,n.cores=1 )
  
  #in this case we had to retrain, since there were less available series than previous cases, so we fixed 39,1000 and 3
  #again the differences throughout configurations is very small
  GB.tree_v5=gbm(Hom~., df_v5, distribution="gaussian",n.trees=40,interaction.depth=5,shrinkage=0.1,n.minobsinnode=3,n.cores=1 )
  
  
  
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





w <- c(1113,1114,1115,1116)  #due to the twitter var


for(momento in w){
  
  data0<-read_xlsx(paste0("data_Monthly_Homicides_",momento,".xlsx"),sheet="Sheet1",col_names = TRUE) 
  
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
  GB.tree=gbm(Hom~., df,
              distribution="gaussian",            # Determines the loss function: "adaboost" for binary responses that use exponential function of AdaBoost algorithm
              n.trees=20,                       # Number of iterations (= number of trees) in boosting algorithm
              interaction.depth=3,                # Number of nodes in the trees
              shrinkage=0.5,                     # Learning rate that controls the number of steps in the algorithm
              #bag.fraction=0.80,                  # Proportion of obs in random training set. If it is 1 uses Gradient Boosting while if it is < 1 uses Stochastic Gradient Boosting
              #train.fraction=1.0,                 # The first train.fraction*nrows(data) observations are used to fit the gbm and the remainder are used for computing out-of-sample estimates of the loss function.We keep it 1.0 and then we divide train test the sample
              #cv.folds=5,                         # Number of cross validation to obtain the error as a function of the number of trees. Estimate the generalization error returned in cv.error
              n.minobsinnode=2,                 # Minimum number of obs in a node to allow splitting
              n.cores=1 )                         # Number of CPU cores to use
  
  #We need the different configurations
  df_v1 <- subset(df, select = -c(Hom1))
  
  df_v2 <- subset(df, select = -c(Hom1,Hom2))
  
  df_v3 <- subset(df, select = -c(Hom1,Hom2,Hom3))
  
  df_v4 <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  
  df_v5 <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4,MOH,H3,GTH,BCs,GTG,MOR,EPU,MOU,TW))
  
  
  
  GB.tree_v1=gbm(Hom~., df_v1, distribution="gaussian",n.trees=20,interaction.depth=3,shrinkage=0.5,n.minobsinnode=3,n.cores=1 )
  
  
  GB.tree_v2=gbm(Hom~., df_v2, distribution="gaussian",n.trees=20,interaction.depth=3,shrinkage=0.5,n.minobsinnode=3,n.cores=1 )
  
  GB.tree_v3=gbm(Hom~., df_v3, distribution="gaussian",n.trees=20,interaction.depth=3,shrinkage=0.5,n.minobsinnode=3,n.cores=1 )
  
  
  GB.tree_v4=gbm(Hom~., df_v4, distribution="gaussian",n.trees=20,interaction.depth=3,shrinkage=0.5,n.minobsinnode=3,n.cores=1 )
  
  
  #in this case we had to retrain, since there were less available series than previous cases
  GB.tree_v5=gbm(Hom~., df_v5, distribution="gaussian",n.trees=40,interaction.depth=5,shrinkage=0.1,n.minobsinnode=3,n.cores=1 )
  
  
  
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
  
  data0<-read_xlsx(paste0("data_Monthly_Homicides_",momento,".xlsx"),sheet="Sheet1",col_names = TRUE) 
  
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
  
  
  
  GB.tree=gbm(Hom~., df,
              distribution="gaussian",            # Determines the loss function: "adaboost" for binary responses that use exponential function of AdaBoost algorithm
              n.trees=20,                       # Number of iterations (= number of trees) in boosting algorithm
              interaction.depth=3,                # Number of nodes in the trees
              shrinkage=0.5,                     # Learning rate that controls the number of steps in the algorithm
              #bag.fraction=0.80,                  # Proportion of obs in random training set. If it is 1 uses Gradient Boosting while if it is < 1 uses Stochastic Gradient Boosting
              #train.fraction=1.0,                 # The first train.fraction*nrows(data) observations are used to fit the gbm and the remainder are used for computing out-of-sample estimates of the loss function.We keep it 1.0 and then we divide train test the sample
              #cv.folds=5,                         # Number of cross validation to obtain the error as a function of the number of trees. Estimate the generalization error returned in cv.error
              n.minobsinnode=2,                 # Minimum number of obs in a node to allow splitting
              n.cores=1 )                         # Number of CPU cores to use
  
  #We need the different configurations
  df_v1 <- subset(df, select = -c(Hom1))
  
  df_v2 <- subset(df, select = -c(Hom1,Hom2))
  
  df_v3 <- subset(df, select = -c(Hom1,Hom2,Hom3))
  
  df_v4 <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  
  df_v5 <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4,GA,MOH,H3,GTH,BCs,GTG,MOR,EPU,MOU,TW))
  
  
  
  
  GB.tree_v1=gbm(Hom~., df_v1, distribution="gaussian",n.trees=20,interaction.depth=3,shrinkage=0.5,n.minobsinnode=3,n.cores=1 )
  
  
  GB.tree_v2=gbm(Hom~., df_v2, distribution="gaussian",n.trees=20,interaction.depth=3,shrinkage=0.5,n.minobsinnode=3,n.cores=1 )
  
  GB.tree_v3=gbm(Hom~., df_v3, distribution="gaussian",n.trees=20,interaction.depth=3,shrinkage=0.5,n.minobsinnode=3,n.cores=1 )
  
  
  GB.tree_v4=gbm(Hom~., df_v4, distribution="gaussian",n.trees=20,interaction.depth=3,shrinkage=0.5,n.minobsinnode=3,n.cores=1 )
  
  #in this case we check configuration
  GB.tree_v5=gbm(Hom~., df_v5, distribution="gaussian",n.trees=40,interaction.depth=5,shrinkage=0.1,n.minobsinnode=3,n.cores=1 )
  
  
  
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
  
  data0<-read_xlsx(paste0("data_Monthly_Homicides_",momento,".xlsx"),sheet="Sheet1",col_names = TRUE) 
  
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
  
  GB.tree=gbm(Hom~., df,
              distribution="gaussian",            # Determines the loss function: "adaboost" for binary responses that use exponential function of AdaBoost algorithm
              n.trees=20,                       # Number of iterations (= number of trees) in boosting algorithm
              interaction.depth=3,                # Number of nodes in the trees
              shrinkage=0.5,                     # Learning rate that controls the number of steps in the algorithm
              #bag.fraction=0.80,                  # Proportion of obs in random training set. If it is 1 uses Gradient Boosting while if it is < 1 uses Stochastic Gradient Boosting
              #train.fraction=1.0,                 # The first train.fraction*nrows(data) observations are used to fit the gbm and the remainder are used for computing out-of-sample estimates of the loss function.We keep it 1.0 and then we divide train test the sample
              #cv.folds=5,                         # Number of cross validation to obtain the error as a function of the number of trees. Estimate the generalization error returned in cv.error
              n.minobsinnode=1,                 # Minimum number of obs in a node to allow splitting
              n.cores=1 )                         # Number of CPU cores to use
  
  
  #We need the different configurations
  df_v1 <- subset(df, select = -c(Hom1))
  
  df_v2 <- subset(df, select = -c(Hom1,Hom2))
  
  df_v3 <- subset(df, select = -c(Hom1,Hom2,Hom3))
  
  df_v4 <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4))
  df_v4notri <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4,X1))  #configuration without quarterly imputed variable
  
  df_v5 <- subset(df, select = -c(Hom1,Hom2,Hom3,Hom4,X1,GA,MOH,H3,GTH,BCs,GTG,MOR,EPU,MOU,TW))
  
  
  GB.tree_v1=gbm(Hom~., df_v1, distribution="gaussian",n.trees=20,interaction.depth=3,shrinkage=0.5,n.minobsinnode=3,n.cores=1 )
  
  
  GB.tree_v2=gbm(Hom~., df_v2, distribution="gaussian",n.trees=20,interaction.depth=3,shrinkage=0.5,n.minobsinnode=3,n.cores=1 )
  
  GB.tree_v3=gbm(Hom~., df_v3, distribution="gaussian",n.trees=20,interaction.depth=3,shrinkage=0.5,n.minobsinnode=3,n.cores=1 )
  
  
  GB.tree_v4=gbm(Hom~., df_v4, distribution="gaussian",n.trees=20,interaction.depth=3,shrinkage=0.5,n.minobsinnode=3,n.cores=1 )
  
  GB.tree_v4notri=gbm(Hom~., df_v4notri, distribution="gaussian",n.trees=20,interaction.depth=3,shrinkage=0.5,n.minobsinnode=3,n.cores=1 )
  #in this case we checked the configuration
  GB.tree_v5=gbm(Hom~., df_v5, distribution="gaussian",n.trees=40,interaction.depth=5,shrinkage=0.1,n.minobsinnode=3,n.cores=1 )
  
  
  
  
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




