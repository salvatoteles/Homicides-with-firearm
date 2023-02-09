#Estimation of nomvember 2013,14,15 and 16 for the outofsample exercise for LSTM.
#LSTM by filling one by one (gives multiple outputs for multiple variables), updating predictions later with observed data if available
#First, prediction at t+1 con all variables and keep prediction
#Second, model with prediction and substitute prediction for all the available variables at t+1
#3) THen similar, and all variables but homicides with lag 1
#3) THen similar, and all variables but homicides with lag 1 and lag 2
#5 Then the same until lag 4. For nowcast, the same model 
#7) We remove the variables until t-4
#8) when no more variables available, we go ahead with the predictions at t+1
#again, the training depends on the moment a new variable appears

remove(list=ls())   # Remove global environment
cat("\f")           # Clear the screen
graphics.off()      # Close the current graphical device
set.seed(1)
library("readxl")
require("writexl")
library(ks)
library(matrixStats)
library(keras)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#w <- c(1109,1110,1111,1112) #due to the availability of "cities" as the newest variable
w <- c(1113,1114,1115,1116)  #due to the availability of "twitter" as the newest variable
#w <- c(1117,1118,1119) #due to the availability of  "Guns Archive" as the newest variable
#w <- c(1120,1121)  #due to the availability of  "Quarterly Homicides Prov. Estimate" as the newest variable
#momento <- c(1221)

for(momento in w){
indica0<-read_xlsx(paste0("data_Monthly_Homicides_",momento,".xlsx"),sheet="Sheet1",col_names = TRUE) 


fecha<-as.matrix(indica0[14:(nrow(indica0)),1])

colnames(indica0) <- c("date","X1","Hom","Hom1","Hom2","Hom3","Hom4",
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


#############################The different networks to be trained for the configurations
#############################First network

indica <- indica0[min(which(indica0$TW4 != 9.999900e+04)):max(which(indica0$Hom != 9.999900e+04)),]
indica <- data.frame(indica[,2:ncol(indica)])
indica <- subset(indica,select=-c(X1,GA,GA1,GA2,GA3,GA4))

#Prediction 3 lags for the binning of the series
prediction <- 3
lag <- prediction


#data scaled
indica_scaled <- indica
for (i in 1:ncol(indica)){
  indica_scaled[,i] <- (indica[,i] - mean(indica[,i]))/sd(indica[,i])
}
indica_scaled <- as.matrix(indica_scaled)

x_train <- indica_scaled
x_train_data <- list()

#lag the data 
for (i in 1:ncol(x_train)) {
  x_train_data[[i]] <- t(sapply(
    1:(length(x_train[, i]) - lag - prediction + 1),
    function(x) x_train[x:(x + lag - 1), i]
  ))
}
#create a 3D tensor
x_train_arr <- array(
  data = as.numeric(unlist(x_train_data)),
  dim = c(
    nrow(x_train_data[[1]]),
    lag,
    ncol(indica)
  )
)
y_train_data <- t(sapply(
  (1 + lag):(dim(indica_scaled)[1] - prediction + 1),
  function(x) indica_scaled[x:(x + prediction - 1)]
))

y_train_arr <- array(
  data = as.numeric(unlist(y_train_data)),
  dim = c(
    nrow(y_train_data),
    prediction,
    1
  )
)


lstm_model <- keras_model_sequential()
lstm_model %>%
  layer_lstm(units = 50, # size of the layer
             batch_input_shape = c(1, 3, dim(x_train)[2]), # batch size, timesteps, features
             return_sequences = TRUE,
             stateful = TRUE) %>%
  # fraction of the units to drop for the linear transformation of the inputs
  layer_dropout(rate = 0.5) %>%
  layer_lstm(units = 20,
             return_sequences = TRUE,
             stateful = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  time_distributed(keras::layer_dense(units = 1))

lstm_model %>%
  compile(loss = "mae", optimizer = "adam", metrics = "accuracy")

lstm_model %>% fit(
  x = x_train_arr,
  y = y_train_arr,
  batch_size = 1,
  epochs = 10,
  verbose = 0,
  shuffle = FALSE
)

#################################Second network

indica <- indica0[min(which(indica0$TW4 != 9.999900e+04)):max(which(indica0$Hom != 9.999900e+04)),]
indica <- data.frame(indica[,2:ncol(indica)])
indica <- subset(indica, select = -c(Hom1,X1,GA,GA1,GA2,GA3,GA4))

indica_scaled <- indica
for (i in 1:ncol(indica)){
  indica_scaled[,i] <- (indica[,i] - mean(indica[,i]))/sd(indica[,i])
}
indica_scaled <- as.matrix(indica_scaled)

x_train <- indica_scaled
x_train_data <- list()

#lag the data 
for (i in 1:ncol(x_train)) {
  x_train_data[[i]] <- t(sapply(
    1:(length(x_train[, i]) - lag - prediction + 1),
    function(x) x_train[x:(x + lag - 1), i]
  ))
}
#create a 3D tensor
x_train_arr <- array(
  data = as.numeric(unlist(x_train_data)),
  dim = c(
    nrow(x_train_data[[1]]),
    lag,
    ncol(indica)
  )
)
y_train_data <- t(sapply(
  (1 + lag):(dim(indica_scaled)[1] - prediction + 1),
  function(x) indica_scaled[x:(x + prediction - 1)]
))

y_train_arr <- array(
  data = as.numeric(unlist(y_train_data)),
  dim = c(
    nrow(y_train_data),
    prediction,
    1
  )
)

lstm_model_v1 <- keras_model_sequential()
lstm_model_v1 %>%
  layer_lstm(units = 50, # size of the layer
             batch_input_shape = c(1, 3, dim(x_train)[2]), # batch size, timesteps, features
             return_sequences = TRUE,
             stateful = TRUE) %>%
  # fraction of the units to drop for the linear transformation of the inputs
  layer_dropout(rate = 0.5) %>%
  layer_lstm(units = 20,
             return_sequences = TRUE,
             stateful = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  time_distributed(keras::layer_dense(units = 1))

lstm_model_v1 %>%
  compile(loss = "mae", optimizer = "adam", metrics = "accuracy")

lstm_model_v1 %>% fit(
  x = x_train_arr,
  y = y_train_arr,
  batch_size = 1,
  epochs = 10,
  verbose = 0,
  shuffle = FALSE
)


############################################Thrid network

indica <- indica0[min(which(indica0$TW4 != 9.999900e+04)):max(which(indica0$Hom != 9.999900e+04)),]
indica <- data.frame(indica[,2:ncol(indica)])
indica <- subset(indica, select = -c(Hom1,Hom2,X1,GA,GA1,GA2,GA3,GA4))

indica_scaled <- indica
for (i in 1:ncol(indica)){indica_scaled[,i] <- (indica[,i] - mean(indica[,i]))/sd(indica[,i])}
indica_scaled <- as.matrix(indica_scaled)

x_train <- indica_scaled
x_train_data <- list()

#lag the data 
for (i in 1:ncol(x_train)) {
  x_train_data[[i]] <- t(sapply(1:(length(x_train[, i]) - lag - prediction + 1),function(x) x_train[x:(x + lag - 1), i]))}
#create a 3D tensor
x_train_arr <- array(data = as.numeric(unlist(x_train_data)),dim = c(nrow(x_train_data[[1]]),lag,ncol(indica)))
y_train_data <- t(sapply((1 + lag):(dim(indica_scaled)[1] - prediction + 1),function(x) indica_scaled[x:(x + prediction - 1)]))

y_train_arr <- array(data = as.numeric(unlist(y_train_data)),dim = c(nrow(y_train_data),prediction,1))

lstm_model_v2 <- keras_model_sequential()
lstm_model_v2 %>% layer_lstm(units = 50, # size of the layer
             batch_input_shape = c(1, 3, dim(x_train)[2]), # batch size, timesteps, features
             return_sequences = TRUE,
             stateful = TRUE) %>%
  # fraction of the units to drop for the linear transformation of the inputs
  layer_dropout(rate = 0.5) %>%
  layer_lstm(units = 20,return_sequences = TRUE,stateful = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  time_distributed(keras::layer_dense(units = 1))

lstm_model_v2 %>% compile(loss = "mae", optimizer = "adam", metrics = "accuracy")

lstm_model_v2 %>% fit(x = x_train_arr,y = y_train_arr,batch_size = 1,epochs = 10,verbose = 0,shuffle = FALSE)

###########################################Fourth network

indica <- indica0[min(which(indica0$TW4 != 9.999900e+04)):max(which(indica0$Hom != 9.999900e+04)),]
indica <- data.frame(indica[,2:ncol(indica)])
indica <- subset(indica, select = -c(Hom1,Hom2,Hom3,X1,GA,GA1,GA2,GA3,GA4))


indica_scaled <- indica
for (i in 1:ncol(indica)){indica_scaled[,i] <- (indica[,i] - mean(indica[,i]))/sd(indica[,i])}
indica_scaled <- as.matrix(indica_scaled)

x_train <- indica_scaled
x_train_data <- list()

#lag the data 
for (i in 1:ncol(x_train)) {
  x_train_data[[i]] <- t(sapply(1:(length(x_train[, i]) - lag - prediction + 1),function(x) x_train[x:(x + lag - 1), i]))}
#create a 3D tensor
x_train_arr <- array(data = as.numeric(unlist(x_train_data)),dim = c(nrow(x_train_data[[1]]),lag,ncol(indica)))
y_train_data <- t(sapply((1 + lag):(dim(indica_scaled)[1] - prediction + 1),function(x) indica_scaled[x:(x + prediction - 1)]))

y_train_arr <- array(data = as.numeric(unlist(y_train_data)),dim = c(nrow(y_train_data),prediction,1))

lstm_model_v3 <- keras_model_sequential()
lstm_model_v3 %>% layer_lstm(units = 50, # size of the layer
                             batch_input_shape = c(1, 3, dim(x_train)[2]), # batch size, timesteps, features
                             return_sequences = TRUE,
                             stateful = TRUE) %>%
  # fraction of the units to drop for the linear transformation of the inputs
  layer_dropout(rate = 0.5) %>%
  layer_lstm(units = 20,return_sequences = TRUE,stateful = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  time_distributed(keras::layer_dense(units = 1))

lstm_model_v3 %>% compile(loss = "mae", optimizer = "adam", metrics = "accuracy")

lstm_model_v3 %>% fit(x = x_train_arr,y = y_train_arr,batch_size = 1,epochs = 10,verbose = 0,shuffle = FALSE)

###########################################Fifth(quarterly)
indica <- indica0[min(which(indica0$TW4 != 9.999900e+04)):max(which(indica0$Hom != 9.999900e+04)),]
indica <- data.frame(indica[,2:ncol(indica)])
indica <- subset(indica, select = -c(Hom1,Hom2,Hom3,Hom4,X1,GA,GA1,GA2,GA3,GA4))


indica_scaled <- indica
for (i in 1:ncol(indica)){indica_scaled[,i] <- (indica[,i] - mean(indica[,i]))/sd(indica[,i])}
indica_scaled <- as.matrix(indica_scaled)

x_train <- indica_scaled
x_train_data <- list()

#lag the data 
for (i in 1:ncol(x_train)) {
  x_train_data[[i]] <- t(sapply(1:(length(x_train[, i]) - lag - prediction + 1),function(x) x_train[x:(x + lag - 1), i]))}
#create a 3D tensor
x_train_arr <- array(data = as.numeric(unlist(x_train_data)),dim = c(nrow(x_train_data[[1]]),lag,ncol(indica)))
y_train_data <- t(sapply((1 + lag):(dim(indica_scaled)[1] - prediction + 1),function(x) indica_scaled[x:(x + prediction - 1)]))

y_train_arr <- array(data = as.numeric(unlist(y_train_data)),dim = c(nrow(y_train_data),prediction,1))

lstm_model_v4t <- keras_model_sequential()
lstm_model_v4t %>% layer_lstm(units = 50, # size of the layer
                             batch_input_shape = c(1, 3, dim(x_train)[2]), # batch size, timesteps, features
                             return_sequences = TRUE,
                             stateful = TRUE) %>%
  # fraction of the units to drop for the linear transformation of the inputs
  layer_dropout(rate = 0.5) %>%
  layer_lstm(units = 20,return_sequences = TRUE,stateful = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  time_distributed(keras::layer_dense(units = 1))

lstm_model_v4t %>% compile(loss = "mae", optimizer = "adam", metrics = "accuracy")

lstm_model_v4t %>% fit(x = x_train_arr,y = y_train_arr,batch_size = 1,epochs = 10,verbose = 0,shuffle = FALSE)

############################################Fifth (no quarterly)
indica <- indica0[min(which(indica0$TW4 != 9.999900e+04)):max(which(indica0$Hom != 9.999900e+04)),]
indica <- data.frame(indica[,2:ncol(indica)])
indica <- subset(indica, select = -c(X1,Hom1,Hom2,Hom3,Hom4,GA,GA1,GA2,GA3,GA4))


indica_scaled <- indica
for (i in 1:ncol(indica)){indica_scaled[,i] <- (indica[,i] - mean(indica[,i]))/sd(indica[,i])}
indica_scaled <- as.matrix(indica_scaled)

x_train <- indica_scaled
x_train_data <- list()

#lag the data 
for (i in 1:ncol(x_train)) {
  x_train_data[[i]] <- t(sapply(1:(length(x_train[, i]) - lag - prediction + 1),function(x) x_train[x:(x + lag - 1), i]))}
#create a 3D tensor
x_train_arr <- array(data = as.numeric(unlist(x_train_data)),dim = c(nrow(x_train_data[[1]]),lag,ncol(indica)))
y_train_data <- t(sapply((1 + lag):(dim(indica_scaled)[1] - prediction + 1),function(x) indica_scaled[x:(x + prediction - 1)]))

y_train_arr <- array(data = as.numeric(unlist(y_train_data)),dim = c(nrow(y_train_data),prediction,1))

lstm_model_v4 <- keras_model_sequential()
lstm_model_v4 %>% layer_lstm(units = 50, # size of the layer
                             batch_input_shape = c(1, 3, dim(x_train)[2]), # batch size, timesteps, features
                             return_sequences = TRUE,
                             stateful = TRUE) %>%
  # fraction of the units to drop for the linear transformation of the inputs
  layer_dropout(rate = 0.5) %>%
  layer_lstm(units = 20,return_sequences = TRUE,stateful = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  time_distributed(keras::layer_dense(units = 1))

lstm_model_v4 %>% compile(loss = "mae", optimizer = "adam", metrics = "accuracy")

lstm_model_v4 %>% fit(x = x_train_arr,y = y_train_arr,batch_size = 1,epochs = 10,verbose = 0,shuffle = FALSE)
###########################################Sixth network

indica <- indica0[min(which(indica0$TW4 != 9.999900e+04)):max(which(indica0$Hom != 9.999900e+04)),]
indica <- data.frame(indica[,2:ncol(indica)])
indica <- subset(indica, select = -c(X1,Hom1,Hom2,Hom3,Hom4,GA,MOH,H3,GTH,BCs,GTG,MOR,EPU,MOU,TW,GA1,GA2,GA3,GA4))


indica_scaled <- indica
for (i in 1:ncol(indica)){indica_scaled[,i] <- (indica[,i] - mean(indica[,i]))/sd(indica[,i])}
indica_scaled <- as.matrix(indica_scaled)

x_train <- indica_scaled
x_train_data <- list()

#lag the data 
for (i in 1:ncol(x_train)) {
  x_train_data[[i]] <- t(sapply(1:(length(x_train[, i]) - lag - prediction + 1),function(x) x_train[x:(x + lag - 1), i]))}
#create a 3D tensor
x_train_arr <- array(data = as.numeric(unlist(x_train_data)),dim = c(nrow(x_train_data[[1]]),lag,ncol(indica)))
y_train_data <- t(sapply((1 + lag):(dim(indica_scaled)[1] - prediction + 1),function(x) indica_scaled[x:(x + prediction - 1)]))

y_train_arr <- array(data = as.numeric(unlist(y_train_data)),dim = c(nrow(y_train_data),prediction,1))

lstm_model_v5 <- keras_model_sequential()
lstm_model_v5 %>% layer_lstm(units = 50, # size of the layer
                             batch_input_shape = c(1, 3, dim(x_train)[2]), # batch size, timesteps, features
                             return_sequences = TRUE,
                             stateful = TRUE) %>%
  # fraction of the units to drop for the linear transformation of the inputs
  layer_dropout(rate = 0.5) %>%
  layer_lstm(units = 20,return_sequences = TRUE,stateful = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  time_distributed(keras::layer_dense(units = 1))

lstm_model_v5 %>% compile(loss = "mae", optimizer = "adam", metrics = "accuracy")

lstm_model_v5 %>% fit(x = x_train_arr,y = y_train_arr,batch_size = 1,epochs = 10,verbose = 0,shuffle = FALSE)

############################################Seventh network


indica <- indica0[min(which(indica0$TW4 != 9.999900e+04)):max(which(indica0$Hom != 9.999900e+04)),]
indica <- data.frame(indica[,2:ncol(indica)])
indica <- subset(indica, select = -c(X1,Hom1,Hom2,Hom3,Hom4,X1,GA,MOH,H3,GTH,BCs,GTG,MOR,EPU,MOU,TW,
                                     GA1,MOH1,H31,GTH1,BCs1,GTG1,MOR1,EPU1,MOU1,TW1,GA2,GA3,GA4))


indica_scaled <- indica
for (i in 1:ncol(indica)){indica_scaled[,i] <- (indica[,i] - mean(indica[,i]))/sd(indica[,i])}
indica_scaled <- as.matrix(indica_scaled)

x_train <- indica_scaled
x_train_data <- list()

#lag the data 
for (i in 1:ncol(x_train)) {
  x_train_data[[i]] <- t(sapply(1:(length(x_train[, i]) - lag - prediction + 1),function(x) x_train[x:(x + lag - 1), i]))}
#create a 3D tensor
x_train_arr <- array(data = as.numeric(unlist(x_train_data)),dim = c(nrow(x_train_data[[1]]),lag,ncol(indica)))
y_train_data <- t(sapply((1 + lag):(dim(indica_scaled)[1] - prediction + 1),function(x) indica_scaled[x:(x + prediction - 1)]))

y_train_arr <- array(data = as.numeric(unlist(y_train_data)),dim = c(nrow(y_train_data),prediction,1))

lstm_model_v6 <- keras_model_sequential()
lstm_model_v6 %>% layer_lstm(units = 50, # size of the layer
                             batch_input_shape = c(1, 3, dim(x_train)[2]), # batch size, timesteps, features
                             return_sequences = TRUE,
                             stateful = TRUE) %>%
  # fraction of the units to drop for the linear transformation of the inputs
  layer_dropout(rate = 0.5) %>%
  layer_lstm(units = 20,return_sequences = TRUE,stateful = TRUE) %>%
  layer_dropout(rate = 0.5) %>%
  time_distributed(keras::layer_dense(units = 1))

lstm_model_v6 %>% compile(loss = "mae", optimizer = "adam", metrics = "accuracy")

lstm_model_v6 %>% fit(x = x_train_arr,y = y_train_arr,batch_size = 1,epochs = 10,verbose = 0,shuffle = FALSE)


###########################################################################################################
###########################################First forecast

indica <- indica0[min(which(indica0$TW4 != 9.999900e+04)):max(which(indica0$Hom != 9.999900e+04)),]
indica <- data.frame(indica[,2:ncol(indica)])
indica <- subset(indica,select=-c(X1,GA,GA1,GA2,GA3,GA4))

indica_scaled <- indica
for (i in 1:ncol(indica)){
  indica_scaled[,i] <- (indica[,i] - mean(indica[,i]))/sd(indica[,i])
}
indica_scaled <- as.matrix(indica_scaled)

x_test_data <- indica_scaled[(nrow(indica_scaled) - prediction + 1):nrow(indica_scaled),]

x_pred_arr <- array(
  data = unlist(x_test_data),
  dim = c(
    1,
    lag,
    ncol(indica)
  )
)

lstm_forecast <- lstm_model %>%
  predict(x_pred_arr, batch_size = 1) %>%
  .[, , 1]

lstm_forecast <- lstm_forecast * sd(indica[,"Hom"]) + mean(indica[,"Hom"])

########################Second forecast. I include the first estimation of homicides and go forward one period

indica <- indica0[min(which(indica0$TW4 != 9.999900e+04)):(max(which(indica0$Hom != 9.999900e+04))+1),]
indica$Hom[max(which(indica$Hom != 9.999900e+04))+1] <- lstm_forecast[1]
indica <- data.frame(indica[,2:ncol(indica)])
indica <- subset(indica,select=-c(X1,GA,GA1,GA2,GA3,GA4))

indica_scaled <- indica
for (i in 1:ncol(indica)){
  indica_scaled[,i] <- (indica[,i] - mean(indica[,i]))/sd(indica[,i])
}
indica_scaled <- as.matrix(indica_scaled)

x_test_data <- indica_scaled[(nrow(indica_scaled) - prediction + 1):nrow(indica_scaled),]

x_pred_arr <- array(
  data = unlist(x_test_data),
  dim = c(1,lag,ncol(indica)))

lstm_forecast_v0 <- lstm_model %>%
  predict(x_pred_arr, batch_size = 1) %>%
  .[, , 1]

lstm_forecast_v0 <- lstm_forecast * sd(indica[,"Hom"]) + mean(indica[,"Hom"])


#######################Third prediction, second network. Include previous forecast
indica <- indica0[min(which(indica0$TW4 != 9.999900e+04)):(max(which(indica0$Hom != 9.999900e+04))+2),]
indica$Hom[max(which(indica$Hom != 9.999900e+04))+1] <- lstm_forecast[1]
indica$Hom[max(which(indica$Hom != 9.999900e+04))+1] <- lstm_forecast_v0[1]
indica <- data.frame(indica[,2:ncol(indica)])
indica <- subset(indica,select=-c(Hom1,X1,GA,GA1,GA2,GA3,GA4))

indica_scaled <- indica
for (i in 1:ncol(indica)){indica_scaled[,i] <- (indica[,i] - mean(indica[,i]))/sd(indica[,i])}
indica_scaled <- as.matrix(indica_scaled)

x_test_data <- indica_scaled[(nrow(indica_scaled) - prediction + 1):nrow(indica_scaled),]
x_pred_arr <- array(data = unlist(x_test_data),dim = c(1,lag,ncol(indica)))

lstm_forecast_v1 <- lstm_model_v1 %>%
  predict(x_pred_arr, batch_size = 1) %>%
  .[, , 1]

lstm_forecast_v1 <- lstm_forecast_v1 * sd(indica[,"Hom"]) + mean(indica[,"Hom"])


#######################Fourth prediction, third network. Include previous forecast
indica <- indica0[min(which(indica0$TW4 != 9.999900e+04)):(max(which(indica0$Hom != 9.999900e+04))+3),]
indica$Hom[max(which(indica$Hom != 9.999900e+04))+1] <- lstm_forecast[1]
indica$Hom[max(which(indica$Hom != 9.999900e+04))+1] <- lstm_forecast_v0[1]
indica$Hom[max(which(indica$Hom != 9.999900e+04))+1] <- lstm_forecast_v1[1]
indica <- data.frame(indica[,2:ncol(indica)])
indica <- subset(indica,select=-c(Hom1,Hom2,X1,GA,GA1,GA2,GA3,GA4))


indica_scaled <- indica
for (i in 1:ncol(indica)){
  indica_scaled[,i] <- (indica[,i] - mean(indica[,i]))/sd(indica[,i])
}
indica_scaled <- as.matrix(indica_scaled)

x_test_data <- indica_scaled[(nrow(indica_scaled) - prediction + 1):nrow(indica_scaled),]
x_pred_arr <- array(data = unlist(x_test_data),dim = c(1,lag,ncol(indica)))

lstm_forecast_v2 <- lstm_model_v2 %>% predict(x_pred_arr, batch_size = 1) %>% .[, , 1]

lstm_forecast_v2 <- lstm_forecast_v2 * sd(indica[,"Hom"]) + mean(indica[,"Hom"])


#######################Fifth prediction, fourth network. Include previous forecast
indica <- indica0[min(which(indica0$TW4 != 9.999900e+04)):(max(which(indica0$Hom != 9.999900e+04))+4),]
indica$Hom[max(which(indica$Hom != 9.999900e+04))+1] <- lstm_forecast[1]
indica$Hom[max(which(indica$Hom != 9.999900e+04))+1] <- lstm_forecast_v0[1]
indica$Hom[max(which(indica$Hom != 9.999900e+04))+1] <- lstm_forecast_v1[1]
indica$Hom[max(which(indica$Hom != 9.999900e+04))+1] <- lstm_forecast_v2[1]
indica <- data.frame(indica[,2:ncol(indica)])
indica <- subset(indica,select=-c(Hom1,Hom2,Hom3,X1,GA,GA1,GA2,GA3,GA4))


indica_scaled <- indica
for (i in 1:ncol(indica)){
  indica_scaled[,i] <- (indica[,i] - mean(indica[,i]))/sd(indica[,i])
}
indica_scaled <- as.matrix(indica_scaled)

x_test_data <- indica_scaled[(nrow(indica_scaled) - prediction + 1):nrow(indica_scaled),]
x_pred_arr <- array(data = unlist(x_test_data),dim = c(1,lag,ncol(indica)))

lstm_forecast_v3 <- lstm_model_v3 %>% predict(x_pred_arr, batch_size = 1) %>% .[, , 1]

lstm_forecast_v3 <- lstm_forecast_v3 * sd(indica[,"Hom"]) + mean(indica[,"Hom"])


#######################From sixth to 24th forecast with the fifth network non quarterly  
for (j in 5:23){
indica <- indica0[min(which(indica0$TW4 != 9.999900e+04)):(max(which(indica0$Hom != 9.999900e+04))+j),]
indica$Hom[max(which(indica$Hom != 9.999900e+04))+1] <- lstm_forecast[1]
indica$Hom[max(which(indica$Hom != 9.999900e+04))+1] <- lstm_forecast_v0[1]
for (k in 1:(j-2)){
indica$Hom[max(which(indica$Hom != 9.999900e+04))+1] <- eval(parse(text=paste0("lstm_forecast_v",k,"[1]")))
}
indica <- data.frame(indica[,2:ncol(indica)])
indica <- subset(indica,select=-c(X1,Hom1,Hom2,Hom3,Hom4,GA,GA1,GA2,GA3,GA4))


indica_scaled <- indica
for (i in 1:ncol(indica)){
  indica_scaled[,i] <- (indica[,i] - mean(indica[,i]))/sd(indica[,i])
}
indica_scaled <- as.matrix(indica_scaled)

x_test_data <- indica_scaled[(nrow(indica_scaled) - prediction + 1):nrow(indica_scaled),]
x_pred_arr <- array(data = unlist(x_test_data),dim = c(1,lag,ncol(indica)))

#lstm_forecast_v3 <- lstm_model_v3 %>% predict(x_pred_arr, batch_size = 1) %>% .[, , 1]
valor <- lstm_model_v4 %>% predict(x_pred_arr, batch_size = 1) %>% .[, , 1]
nam <- paste0("lstm_forecast_v",j-1)
assign(nam, valor[1] * sd(indica[,"Hom"]) + mean(indica[,"Hom"]))
#lstm_forecast_v3 <- lstm_forecast_v3 * sd(indica[,"Hom"]) + mean(indica[,"Hom"])

}


#########################################################################################


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
results_fore <- c(lstm_forecast[1],lstm_forecast_v0[1],lstm_forecast_v1[1],lstm_forecast_v2[1],lstm_forecast_v3[1],lstm_forecast_v4[1],
                  lstm_forecast_v5[1],lstm_forecast_v6[1],lstm_forecast_v7[1],lstm_forecast_v8[1],lstm_forecast_v9[1],lstm_forecast_v10[1],
                  lstm_forecast_v11[1],lstm_forecast_v12[1],lstm_forecast_v13[1],lstm_forecast_v14[1],lstm_forecast_v15[1],lstm_forecast_v16[1],
                  lstm_forecast_v17[1],lstm_forecast_v18[1],lstm_forecast_v19[1],lstm_forecast_v20[1],lstm_forecast_v21[1],lstm_forecast_v22[1])



library(writexl)
write_xlsx(data.frame(results_date,results_fore),paste0("./results_",momento,".xlsx"))
}