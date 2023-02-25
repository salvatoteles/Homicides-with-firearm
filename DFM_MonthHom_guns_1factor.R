# Code to estimate DFM as in the paper
#"On the use of dynamic factor modeling to predict homicides
#with firearm in the United States"
# November, 2022

#############################################################
remove(list=ls())   # Remove global environment
cat("\f")           # Clear the screen
graphics.off()      # Close the current graphical device
set.seed(1)
library("readxl")
require("writexl")
library(ks)
library(matrixStats)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))   #work in the same location as the data

#############################################################
#  control specification 
#############################################################
va<-1    # va is the variance of rnd number to fill unobseved data
vf<-1    # var of factor error 
n<-6    # total number of indicators 
qf<-2    # lag length of factor and idios of homicides. Take into account later the quarterly indicator, that makes sometimes the need
#to add +1 to build the matrices
q1<-2    # lag length of all idios. of monthly indicators 
pphi<-max(c(qf,q1)); # pphi is the max lag length 
nk<-pphi+1      # nk is the first observation for which the likelihood will be evaluated 
pnk<-(qf+1)+(q1+1)*n    # pnk is the dimension of the state space (B), 18 due to idios. terms and three for factor, due to the quart. variable                            
je <- 1  #set to zero if you want to skip the estimations of standard deviations with the hessian
#############################################################
#                      LOAD DATA                            #
#############################################################

w <- c(1221) 
#w <- c(1121,1221)   #Two datasets for example to work in loop for the outofsample exercise
 
conv <- c()   #if we want to save convergence of the process in a vector when multiple datasets

for (momento in w){
#load the indicators depending on the dataset
indica0<-read_xlsx(paste0("data_Monthly_Homicides_",momento,".xlsx"),sheet=1,col_names = TRUE) 

date<-as.matrix(indica0[2:(nrow(indica0)),1])
indica<-as.matrix(indica0[2:(nrow(indica0)),2:10])  #remove other series
indica <- indica[,-c(6,7,8)]   #Not using Google Trends and BC non lagged
gg1<-subset(indica[,2], indica[,2]!=99999)
me1<-mean2(gg1)   #needed to reconstruct the factor
std1<-sd(gg1)
vname1<-c("Quarterly","Homicide_HomFi_PopEst_sa","Guns_Archive","MOHom","Ciudades","Bcs_lag7") #name series

#############################################################
#                   FUNCTIONS                             #
#############################################################
standard<-function(datos){    #to standarize data
  datos2<-datos
  for(j in 1:ncol(datos2)){
    dataj<-subset(datos2[,j], datos2[,j]!=99999)
    datajm<-mean2(dataj)
    datajst<-sd(dataj)
    for (i in 1:nrow(datos)){
      if (datos2[i,j] != 99999){
        datos2[i,j]=(datos[i,j]-datajm)/datajst}
    }
  }
  return(datos2)
}

relleno<-function(datos,va){  #to fill where missing values
  for(j in 1:ncol(datos)){
    dataj<-datos[,j]
    nm<-sum(dataj==99999)
    index<-which(dataj==99999)
    dataj[index]<-1
    datos[,j]<-dataj
  }
  return(datos)
}

trans<-function(vari2){    #for the variance parameters
  cy2<-n+qf+n*q1        
  rv<-length(vari2)
  partrans<-c(vari2[1:cy2],(vari2[(cy2+1):rv]^2))
  return(partrans)
}

ofn<-function(th2,Yv){    #optimization procedure based on Kalman
  #Filter<-matrix(0,captst,pnk)
  th<-trans(th2)    #for the variance parameters   
  rv<-length(th)  #in our case, 6 parameters loading, 12 idiosync, 6 of variances and 2 of factor
  RQHF<-matrices(th)   #from the matrices function, we built the needed matrices
  R<-RQHF[[1]]
  Q<-RQHF[[2]]
  H<-RQHF[[3]]
  F<-RQHF[[4]]
  
  beta00<-matrix(0,pnk,1)   #we need the initial space vector
  P00<-diag(pnk)     #and space matrix
  
  like<-matrix(0,captst,1)
  fun<-0
  for(it in 1:captst){           # kalman filter iterations
    indexit<-index[it,]
    Hit<-matrix(rep(indexit,each=ncol(H)),ncol=ncol(H),byrow=TRUE)*H  # Element-wise multiplication with out built matrix
    Rit<-diag(n)      #the diagonal R vector
    Rit<-diag(c(matrix(1,n,1)-indexit),n)
    
    beta10<-F%*%beta00                 # Prediction equations
    P10<-F%*%P00%*%t(F)+Q
    n10<-Yv[it,]-(Hit%*%beta10)        # forecast error 
    F10<-Hit%*%P10%*%t(Hit)+Rit
    
    dF<-det(F10)
    # if (dF<1e-5){
    #   print(dF)
    #   fun<-1000000
    #   break
    # }
    
    like[it]<--0.5*(log(2*pi*det(F10))+t(n10)%*%solve(F10,tol=1e-27)%*%n10)   #estimation of likelihood
    K<-P10%*%t(Hit)%*%solve(F10)       # Kalman gain      
    beta11<-beta10+K%*%n10             # Updating equations 
    #Filter[it,]<<-t(beta11)
    Filter[it,]<-t(beta11)    #we feed to Filter at each period the result of the kalman filter. 
    Filter_0[it,] <- t(beta10)   #to the the filter matrix at t|t+1
    #the optimization procedure will make that the final Filter will be with the optimized parameters
    P11<-P10-K%*%Hit%*%P10
    
    Filter2 <<-Filter   #we need to assign to be able to work with it outside the function
    Filter2_0 <<- Filter_0
    
    beta00<-beta11    #and we update parameters in the periodic loop
    P00<-P11
    fun<-fun-like[it]
  }
  
  #Filter <<- Filter
  K2 <<- K    #if we later want to study forecast weights, we assign also these
  Hit2 <<- Hit
  F2 <<- F
  beta112 <<-beta11
  #fun<--colSums(like)
  #print(fun)
  cat("\f")
  print(th)
  print(fun)
  return(fun)
}

matrices<-function(z){    #to create the matrices. We need to adapt the parameters for the quarterly series
  Rs<-(va^2)*matrix(1,n,1)
  Qs<-matrix(0,pnk,pnk)
  Qs[1,1]<-vf  #the next five lines fill create the Q matrix
  cy2<-n+qf+n*q1
  cy3<-qf+1
  for(j in 1:n){
    Qs[(cy3+(j-1)*(q1+1)+1),(cy3+(j-1)*(q1+1)+1)]<-z[(cy2+j)]
  }
  nece<-cbind(1,matrix(0,1,q1))   #we fill the H matrix 
  Hs<-cbind(z[1:n],matrix(0,n,(qf)),kronecker(diag(n), nece))  #first row and triplets for the idiosyn.
  Hs[1,qf] <- z[1]; Hs[1,qf+1]<-z[1];  #need to add the first parameter in the second and third row for the quarterly
  #now we create the F matrix
  cy2<-n;
  F1<-cbind(t(z[(n+1):(n+qf)]),matrix(0,1,(pnk-qf)))  #first row of the F matrix
  F2<-cbind(diag(qf-1),matrix(0,qf-1,pnk-qf+1))  #second row
  F2b<-cbind(0,diag(qf-1),matrix(0,qf-1,pnk-qf)) #third row
  cy2<-cy2+qf
  nece<-matrix(0,(q1+1)*n,(q1+1)*n)
  ma<-matrix(0,q1+1,q1+1)  #the next three row create a 3x3 matrix with the zeros and one that will create the submatrix
  #of parameters idiosyn.
  ma[(2:q1),(1:q1-1)]<-diag(q1-1)
  ma[(3:(q1+1)),(2:q1)]<-diag(q1-1)
  for(j in 1:n){    #we include the idyosinc terms, and append to create the 18x18 submatrix with the idiosync
    ma[1,(1:q1)]<-z[(cy2+(j-1)*q1+1):(cy2+j*q1)]
    nece[((j-1)*(q1+1)+1):(j*(q1+1)),((j-1)*(q1+1)+1):(j*(q1+1))]<-ma
  }
  F3=cbind(matrix(0,(q1+1)*n,qf+1),nece)  #we append the first three columns but the first three rows
  Fs=rbind(F1,F2,F2b,F3)   #we append the first three rows
  return(list(Rs,Qs,Hs,Fs))
}


#############################################################
#             Fill unobserved and standardize               #            
#############################################################
indica<-standard(indica)
indica2<-relleno(indica,va)
y<-indica2
capt<-nrow(y)
captst<-capt-pphi            # captst is the effective sample size @
Yv<-y[nk:capt,]
mat1<-indica[nk:capt,]
mat<-(mat1!=99999)
index<-replace(mat,mat<0,0)

#############################################################
#                 Initial parameters' values                #
#############################################################
#Parameters to initialize if no idea about them
#B<-0.3*c(rep(1,n))
#phif<-0.2*c(rep(1,qf))
#phiy<-0.3*c(rep(1,n*q1))
#v<-1*c(rep(1,n))
#startval<-c(B,phif,phiy,v)

#We use the already worked for 6 variables_1221
startval <- c(0.0745,  0.511,  0.2198,  0.1774,  0.2223,  0.1086,  0.5672,
             0.411,  1.609, -0.865, 0.4373,  0.558,  0.215, -0.059 ,
             0.262,  0.056,  0.185,  0.2498,  0.59879224,  0.24639772,  0.15331963,
             0.20608082,  0.14663668,  0.72284051,  0.65885320,  0.50094384)

nth<-length(startval)

#############################################################
#         The following matrices  are needed              #
#############################################################
Filter<-matrix(0,captst,pnk)  # Filtered inferences from Kalman
pq<-qf+q1
pnk2<-(pq^2+pq)/2;
Pmat<-matrix(0,captst,pnk2)
  
#############################################################
#               Numerical optimization                     #
#############################################################
options=list(maxit = 300 #depending on the method of optimization, different parameters are fixed
            #,reltol=1e-5
            #abstol=1e-8
            #,trace=TRUE
            #,ndeps=1e-3
            ,factr=1e8 #to get tolerance in L-BFGS-B, this number is multiplied in R by 1e-15
            #,pgtol=1e-10
            #trace=10
            )
#, control=options

res <- optim(par=startval,fn=ofn,Yv,gr=NULL,method="L-BFGS-B",hessian = FALSE,control=options)
conv[which(w==momento)] <- res$convergence

#Parameters convergence 
#Model 6 variables 2021M12

#converged <- c(0.069,  0.345,  0.205,  0.164,  0.212,  0.103,  0.592,  0.387,
# 1.571, -0.836,  0.594,  0.404,  0.072, -0.022,  0.264,  0.064,
# 0.177,  0.247,  0.600,  0.248,  0.023,  0.015,  0.020,  0.524,
# 0.433,  0.251)

#For weights in forecast:
# res <- ofn(converged,Yv)   #if we now the parameters, we can directly run this and no optimized
# res$par <- converged
#This is the needed lists from the ofn process if weights in forecasts
#are computed according to Banbura(08).
#K2
#Hit2
#F2
#beta112

# Mt <- solve(diag(n*q1+qf)-(diag(n*q1+qf)-K2%*%Hit2)%*%F2)%*%K2
# pesos <- res$par[2]*Mt[1,]+Mt[5,]
# pesos_per <- abs(pesos)/(sum(abs(pesos)))
# pesos_per

##########################
#Reconstruction of series
##########################
#The variable is reconstructed from the filtered output from Kalman
#case variable homicides. The second parameter, times the first column of the filter matrix(i.e. the factor)
#plus the seventh column, with is the u2,t term (i.e. the idio. of homicides)
homm <- res$par[2]*(Filter2[,1])+(Filter2[,((qf+1) + 4)])   
stdhom <- std1
mehom <- me1
homm <- homm*stdhom + mehom   #we need to reestandarize
common1 <- (res$par[2]*(Filter2[,1]))*stdhom   #we see it separately for plotting
idio <- (Filter2[,((qf+1) + 4)])*stdhom   #also idiosyncratic component

media <- mehom
plot(homm,ylab="value",type="l",ylim=c(-0.10,0.6))  #we plot(careful with the limits to visualize, one can also 
#estimate or not in terms of homicides per 100,000 inhabitans) the reconstructed series
lines(common1,type="l",col=2)  #the factor
#lines(common2,type="l",col=3)
lines(idio,type="l",col=3)  #the idiosyn.
lines(mehom,type="l",col=4)   #the mean in we want
legend("topleft", legend=c("value", "common1","idiosyncratic"),
       col=c("black", "red","green"), lty=1:2, cex=0.8)


#approximation to % of explained variance
fit2 <- lm(common1 ~ homm)  #we regress the factor with the series
ajuste <- summary(fit2)
ajuste$r.squared


####################################################################
#Proofs of normality
ef <- Filter2[1:(dim(Filter2)[1]-12),1] - res$par[7]*Filter2[1:(dim(Filter2)[1]-12),2] - res$par[8]*Filter2[1:(dim(Filter2)[1]-12),3]
#each factor has a different sample period. We need Filter0 for the quarterly variable, due to the missing values
#e_u1 <- Filter2[219:(dim(Filter2)[1]-12),((qf+1) + 1)]-res$par[9]*Filter2[219:(dim(Filter2)[1]-12),((qf+1) + 2)]-res$par[10]*Filter2[219:(dim(Filter2)[1]-12),((qf+1) + 3)]
e_u1_0_v2 <- Filter2_0[219:(dim(Filter2_0)[1]-12),((qf+1) + 1)]-res$par[9]*dplyr::lag(Filter2_0[219:(dim(Filter2_0)[1]-12),((qf+1) + 1)],1)-res$par[8]*dplyr::lag(Filter2_0[219:(dim(Filter2_0)[1]-12),((qf+1) + 1)],2)
e_u1_0_v2 <- e_u1_0_v2[3:length(e_u1_0_v2)]
e_u2 <- Filter2[1:(dim(Filter2)[1]-12),((qf+1) + 4)]-res$par[11]*Filter2[1:(dim(Filter2)[1]-12),((qf+1) + 5)]-res$par[12]*Filter2[1:(dim(Filter2)[1]-12),((qf+1) + 6)]
e_u3 <- Filter2[180:(dim(Filter2)[1]-12),((qf+1) + 7)]-res$par[13]*Filter2[180:(dim(Filter2)[1]-12),((qf+1) + 8)]-res$par[14]*Filter2[180:(dim(Filter2)[1]-12),((qf+1) + 9)]
e_u4 <- Filter2[1:(dim(Filter2)[1]-12-12),((qf+1) + 10)]-res$par[15]*Filter2[1:(dim(Filter2)[1]-12-12),((qf+1) + 11)]-res$par[16]*Filter2[1:(dim(Filter2)[1]-12-12),((qf+1) + 12)]
e_u5 <- Filter2[85:(dim(Filter2)[1]-12),((qf+1) + 13)]-res$par[17]*Filter2[85:(dim(Filter2)[1]-12),((qf+1) + 14)]-res$par[18]*Filter2[85:(dim(Filter2)[1]-12),((qf+1) + 15)]
e_u6 <- Filter2[8:(dim(Filter2)[1]-12),((qf+1) + 16)]-res$par[19]*Filter2[8:(dim(Filter2)[1]-12),((qf+1) + 17)]-res$par[20]*Filter2[8:(dim(Filter2)[1]-12),((qf+1) + 18)]

ks.test(e_u1_0_v2,"pnorm",mean=mean(e_u1_0_v2),sd=sd(e_u1_0_v2)) #p<0.05 rejects normality. Change here the 
library(tseries)
library(nortest) 
lillie.test(e_u1_0_v2)
library(vsgoftest)
vs.test(e_u1_0_v2,densfun='dnorm')
library(EnvStats)
dat.censored <- ef; censored <- dat.censored < -0.1|dat.censored > 0.1; dat.censored[censored] <- 0
gof.list <- gofTestCensored(dat.censored, censored, test = "sf", distribution = "norm"); gof.list
dat.censored <- e_u2; censored <- dat.censored < -0.1|dat.censored > 0.1; dat.censored[censored] <- 0
gof.list <- gofTestCensored(dat.censored, censored, test = "sf", distribution = "norm"); gof.list
dat.censored <- e_u3; censored <- dat.censored < -0.25|dat.censored > 0.25; dat.censored[censored] <- 0
gof.list <- gofTestCensored(dat.censored, censored, test = "sf", distribution = "norm"); gof.list
dat.censored <- e_u4; censored <- dat.censored < -1|dat.censored > 1; dat.censored[censored] <- 0
gof.list <- gofTestCensored(dat.censored, censored, test = "sf", distribution = "norm"); gof.list
dat.censored <- e_u5; censored <- dat.censored < -1.5|dat.censored > 1.5; dat.censored[censored] <- 0
gof.list <- gofTestCensored(dat.censored, censored, test = "sf", distribution = "norm"); gof.list
dat.censored <- e_u6; censored <- dat.censored < -0.4|dat.censored > 0.4; dat.censored[censored] <- 0
gof.list <- gofTestCensored(dat.censored, censored, test = "sf", distribution = "norm"); gof.list

#Ljung Box and Box Pierce for autocorrelation
Box.test(e_u6_v2, lag = 1, type = "Box-Pierce")
Box.test(e_u6_v2, lag = 1, type = "Ljung-Box")



#############################################################
#            To get standard deviations                     #
#############################################################
 library(rootSolve)
 x <- res$par
 if (je!=0){   #then we estimate 
 options=list(fnscale=1
             ,parscale=rep.int(1, nth)
             ,ndeps=rep.int(1e-3, nth)
 )
 h<-optimHess(x,ofn,Yv,gr=NULL,control = options)   #we need to optimize hessian
 hi<-solve(h);  #obtain the inverse
 stdor<-diag(hi)^.5;  
 gr<-gradient(trans,x,centered = FALSE, pert = 1e-8)
 Hfin<-gr%*%hi%*%t(gr)
 std=abs(diag(Hfin))^.5
 }

#we approximate significance with the ratio of parameter and its std (1.96 level in normal)
 pval <- x/std
 paste0("values are"," ",(1-pnorm(pval[1:n]))*2)   #if we want de standard deviations just for the loading factors

#Let's get data for the interested output
results <- data.frame(date[3:capt],homm,common1,idio,rep(media,length(date[3:capt])))

library(xlsx)
write.xlsx(results, file=paste0("Results_MonthHom_guns_1factor_",momento,".xlsx"), sheetName="Series")
write.xlsx(x, file=paste0("Results_MonthHom_guns_1factor_",momento,".xlsx"), sheetName="Parameters", append=TRUE)
write.xlsx((1-pnorm(pval[1:n]))*2, file=paste0("Results_MonthHom_guns_1factor_",momento,".xlsx"), sheetName="Signif", append=TRUE)
write.xlsx(ajuste$r.squared, file=paste0("Results_MonthHom_guns_1factor_",momento,".xlsx"), sheetName="R2", append=TRUE)
#write.xlsx(pesos, file="Results_MonthHom_guns_1factor_.xlsx", sheetName="Pesos", append=TRUE)

}

