rm(list=ls())
setwd("C:/Users/Nutzer/Desktop/Masterarbeit/code kompakt")

# 0.2  Load Required Packages and Functions ####################################

# Load the script which includes all the required packages and functions
source("Initialize.R")

# 0.3 Read and Prepare Data ####################################################

# Load the Stata 12 format data
SS <- read.dta("salty_snack_main_l2_voleq.dta")

SS$p_id <- as.factor(SS$upc)
SS$iri_key <- as.factor(SS$iri_key)
SS$week <- as.factor(SS$week)
SS$producttype <- factor(SS$producttype)
SS$package <- factor(SS$package)
SS$flavorscent <- factor(SS$flavorscent)
SS$fatcontent <- factor(SS$fatcontent)
SS$cookingmethod <- factor(SS$cookingmethod)
SS$saltsodiumcontent <- factor(SS$saltsodiumcontent)
SS$typeofcut <- factor(SS$typeofcut)
SS$pr <- factor(SS$pr)
SS$f <- factor(SS$f)
SS$d <- factor(SS$d)
SS$brand <- factor(SS$l5)

#prepare data
#drop first year so it is more likely to have stor-item combinations with observations for full 5 years
ts_data=subset(SS,as.numeric(SS$week)>52)
ts_data=ts_data[order(ts_data$date),]
ts_data$count=paste0(ts_data$iri_key,ts_data$colupc)
count_table=table(ts_data$count)
count_table=subset(count_table,count_table==max(count_table))

#so zu mindest 119 store-item kombinationen

count_obs=ts_data %>% count(ts_data$iri_key,ts_data$colupc)
count_obs=subset(count_obs,count_obs$n==max(count_table))
colnames(count_obs)<-c("iri_key","colupc","n")
count_obs=paste0(count_obs$iri_key,count_obs$colupc)

#subset with all variables
SS_53_313=subset(ts_data,ts_data$count%in%count_obs)

#subset with 4 variables
SS_ts=subset(ts_data,ts_data$count%in%count_obs)
SS_ts=SS_ts[c("iri_key","colupc","date","logunits")]

#subset with 8 variables for arimax
SS_ARIMAX=SS_53_313[c("iri_key","colupc","date","logunits","logprice","pr","f","d")]

#necessary for split
SS_ARIMAX=data.table(SS_ARIMAX)
SS_ts=data.table(SS_ts)
SS_53_313=data.table(SS_53_313)

#Nun nochmal mit einer Woche mehr in die Vergangeheit um auch lags für Woche 53 zu haben

ts_data2=subset(SS,as.numeric(SS$week)>51)
ts_data2=ts_data2[order(ts_data2$date),]
ts_data2$count=paste0(ts_data2$iri_key,ts_data2$colupc)
count_table=table(ts_data2$count)
count_table=subset(count_table,count_table==max(count_table))

count_obs=ts_data2 %>% count(ts_data2$iri_key,ts_data2$colupc)
count_obs=subset(count_obs,count_obs$n==max(count_table))
colnames(count_obs)<-c("iri_key","colupc","n")
count_obs=paste0(count_obs$iri_key,count_obs$colupc)

SS_52_313=subset(ts_data2,ts_data2$count%in%count_obs)

SS_52_313=data.table(SS_52_313)

ML_split_lag=split(SS_52_313, by=c("iri_key", "colupc"),drop=TRUE, keep.by=TRUE)

for (i in 1:length(ML_split_lag)) {logunits=zoo(ML_split_lag[[i]]$logunits)
lags=as.data.frame(lag(logunits))
ML_split_lag[[i]]=data.frame(cbind(ML_split_lag[[i]],lags))
}

                         
#prepare names for prophet
colnames(SS_ts)=c("iri_key","colupc","ds","y")

train_splitting= split(SS_ts, by=c("iri_key", "colupc"),drop=TRUE, keep.by=FALSE)
arimax_splitting= split(SS_ARIMAX, by=c("iri_key", "colupc"),drop=TRUE, keep.by=FALSE)
ML_split= split(SS_53_313, by=c("iri_key", "colupc"),drop=TRUE, keep.by=FALSE)

rm(SS)


#1.Zeitreihenmodelle ####################################################################

##########################################################################################

#now comparison with arima models
m=12

ts_mat=matrix(0,nrow=dim(train_splitting[[1]])[1],ncol=length(train_splitting))
for (i in 1:length(train_splitting)) {ts_mat[,i]=train_splitting[[i]]$y}
colnames(ts_mat)=names(train_splitting)
ts_train=ts_mat[1:(dim(ts_mat)[1]-m),]
ts_test=ts_mat[(dim(ts_mat)[1]-m+1):dim(train_splitting[[1]])[1],]
mult_ts=ts(ts_train,start = c(2001,52),end = c(2006,40),frequency = 52)
ts_pred=matrix(0, nrow = m, ncol = length(train_splitting))

#arima with loop

rmse12=seq(1,length(train_splitting))
rmse1=seq(1,length(train_splitting))
smape12=seq(1,length(train_splitting))
smape1=seq(1,length(train_splitting))
rmsefit=seq(1,length(train_splitting))


arimaresult=matrix(0, nrow = 100, ncol = 6)
colnames(arimaresult)=c("Methode","RMSE1","SMAPE1","RMSE12","SMAPE12","RMSE")

counter=0
for (q in 0:3){
  for (d in 1:2) {
    for(p in 0:3){if ((q==3&d==1&p==2)|(q==3&d==1&p==3)|(q==3&d==2&p==1)|(q==3&d==2&p==2)) {
      next
    }
for (i in 1:length(train_splitting)) {  fit=Arima(mult_ts[,i],order=c(p,d,q))
ts_pred[,i]=predict(fit,12)$pred[1:12]
rmse12[i]=rmse(ts_test[,i],ts_pred[,i])
rmse1[i]=rmse(ts_test[,i][1],ts_pred[,i][1])
smape12[i]=sMAPE(exp(ts_test[,i]),exp(ts_pred[,i]))
smape1[i]=sMAPE(exp(ts_test[,i][1]),exp(ts_pred[,i][1]))
rmsefit[i]=sqrt(mean((fit$residuals)^2))
}  
counter=counter+1
arimaresult[counter,1]=paste("ARIMA(",p,",",d,",",q,")",sep="")
arimaresult[counter,2]=as.numeric(mean(rmse1))
arimaresult[counter,3]=as.numeric(mean(smape1))
arimaresult[counter,4]=as.numeric(mean(rmse12))
arimaresult[counter,5]=as.numeric(mean(smape12)) 
arimaresult[counter,6]=as.numeric(mean(rmsefit))
      }}}

arimaresult=arimaresult[arimaresult[,2]!=0,]
arimaresult_good=arimaresult[arimaresult[,6]==min(arimaresult[,6]),]
arimaresult_good[2:6]=round(as.numeric(arimaresult_good[2:6]),3)
#write.table(arimaresult_good,"arimaresult_good.csv")


armaresult=matrix(0, nrow = 100, ncol = 6)
colnames(armaresult)=c("Methode","RMSE1","SMAPE1","RMSE12","SMAPE12","RMSE")

counter=0
for (q in 0:3){
  for (d in 0:0) {
    for(p in 0:3){if ((q==2&d==0&p==2)|(q==2&d==0&p==3)|(q==3&d==0&p==3)|(q==3&d==0&p==2)|(q==3&d==1&p==2)|(q==3&d==1&p==3)|(q==3&d==2&p==1)|(q==3&d==2&p==2)) {
      next
    }
      for (i in 1:length(train_splitting)) {  fit=Arima(mult_ts[,i],order=c(p,d,q))
      ts_pred[,i]=predict(fit,12)$pred[1:12]
      rmse12[i]=rmse(ts_test[,i],ts_pred[,i])
      rmse1[i]=rmse(ts_test[,i][1],ts_pred[,i][1])
      smape12[i]=sMAPE(exp(ts_test[,i]),exp(ts_pred[,i]))
      smape1[i]=sMAPE(exp(ts_test[,i][1]),exp(ts_pred[,i][1]))
      rmsefit[i]=sqrt(mean((fit$residuals)^2))
      }  
      counter=counter+1
      armaresult[counter,1]=paste("ARMA(",p,",",q,")",sep="")
      armaresult[counter,2]=as.numeric(mean(rmse1))
      armaresult[counter,3]=as.numeric(mean(smape1))
      armaresult[counter,4]=as.numeric(mean(rmse12))
      armaresult[counter,5]=as.numeric(mean(smape12)) 
      armaresult[counter,6]=as.numeric(mean(rmsefit))
    }}}

armaresult=armaresult[armaresult[,2]!=0,]
armaresult_good=armaresult[armaresult[,6]==min(armaresult[,6]),]
armaresult_good[2:6]=round(as.numeric(armaresult_good[2:6]),3)
#write.table(armaresult_good,"armaresult_good.csv")



##########################################################################################
#ARMAX

ts_arimax=matrix(0,nrow=dim(arimax_splitting[[1]])[1],ncol=length(arimax_splitting))

for (i in 1:length(arimax_splitting)) {ts_arimax[,i]=arimax_splitting[[i]]$logunits}

ts_train=ts_arimax[1:(dim(ts_arimax)[1]-m),]
ts_test=ts_arimax[(dim(ts_arimax)[1]-m+1):dim(arimax_splitting[[1]])[1],]
arimax_pred=matrix(0, nrow = m, ncol = dim(ts_train)[2])

xreglist=list()

x_formula <- formula(~(logprice+pr+f+d))

for (i in 1:length(arimax_splitting)) {
  xreglist[[i]] <- model.matrix(x_formula,data=arimax_splitting[[i]])
  xreglist[[i]]=xreglist[[i]][,apply(xreglist[[i]], 2, function(col) { length(unique(col[1:249])) > 1 })]
}

w=1:length(arimax_splitting)
w=w[!w%in% c(34,105)]

#für arimax muss ein dummy entfernt werden das ist aber nur bei drei fällen relevant

for (i in w) {
  xreglist[[i]]=xreglist[[i]][, colSums(xreglist[[i]])>2]
}

armaxresult=matrix(0, nrow = 100, ncol = 6)
colnames(armaxresult)=c("Methode","RMSE1","SMAPE1","RMSE12","SMAPE12","RMSE")

counter=0
for (q in 0:3){
  for (d in 0:0) {
    for(p in 0:3){

for (i in 1:length(arimax_splitting)) {
  if(i==34|i==105){fit <- Arima(ts_train[,i], xreg=xreglist[[i]][1:(dim(ts_arimax)[1]-m)], order=c(p,d,q),method="CSS")
  ts_pred[,i]=predict(fit,12,newxreg=xreglist[[i]][(dim(ts_arimax)[1]-m+1):dim(ts_arimax)[1]])$pred[1:12]
}
  else if(i==33|i==74|i==103){fit <- Arima(ts_train[,i], xreg=xreglist[[i]][1:(dim(ts_arimax)[1]-m),-3], order=c(p,d,q),method="CSS")
  ts_pred[,i]=predict(fit,12,newxreg=xreglist[[i]][(dim(ts_arimax)[1]-m+1):dim(ts_arimax)[1],-3])$pred[1:12]}
  else{fit <- Arima(ts_train[,i], xreg=xreglist[[i]][1:(dim(ts_arimax)[1]-m),], order=c(p,d,q),method="CSS")
  ts_pred[,i]=predict(fit,12,newxreg=xreglist[[i]][(dim(ts_arimax)[1]-m+1):dim(ts_arimax)[1],])$pred[1:12]}
  rmse12[i]=rmse(ts_test[,i],ts_pred[,i])
  rmse1[i]=rmse(ts_test[,i][1],ts_pred[,i][1])
  smape12[i]=sMAPE(exp(ts_test[,i]),exp(ts_pred[,i]))
  smape1[i]=sMAPE(exp(ts_test[,i][1]),exp(ts_pred[,i][1]))
  rmsefit[i]=sqrt(mean((fit$residuals)^2))
}
      counter=counter+1
      armaxresult[counter,1]=paste("ARMAX(",p,",",q,")",sep="")
      armaxresult[counter,2]=as.numeric(mean(rmse1))
      armaxresult[counter,3]=as.numeric(mean(smape1))
      armaxresult[counter,4]=as.numeric(mean(rmse12))
      armaxresult[counter,5]=as.numeric(mean(smape12)) 
      armaxresult[counter,6]=as.numeric(mean(rmsefit))}}}

armaxresult=armaxresult[armaxresult[,2]!=0,]
armaxresult_good=armaxresult[armaxresult[,6]==min(armaxresult[,6])|armaxresult[,1]=="ARMAX(3,0)",]
armaxresult_good[,2:6]=round(as.numeric(armaxresult_good[,2:6]),3)

arimaxresult=matrix(0, nrow = 100, ncol = 6)
colnames(arimaxresult)=c("Methode","RMSE1","SMAPE1","RMSE12","SMAPE12","RMSE")

options(nwarnings = 200) 

counter=0
for (q in 0:3){
  for (d in 1:2) {
    for(p in 0:3){
      
      for (i in 1:length(arimax_splitting)) {
        if(i==34|i==105){fit <- Arima(ts_train[,i], xreg=xreglist[[i]][1:(dim(ts_arimax)[1]-m)], order=c(p,d,q),method="CSS")
        ts_pred[,i]=predict(fit,12,newxreg=xreglist[[i]][(dim(ts_arimax)[1]-m+1):dim(ts_arimax)[1]])$pred[1:12]
        }
        else if(i==33|i==74|i==103){fit <- Arima(ts_train[,i], xreg=xreglist[[i]][1:(dim(ts_arimax)[1]-m),-3], order=c(p,d,q),method="CSS")
        ts_pred[,i]=predict(fit,12,newxreg=xreglist[[i]][(dim(ts_arimax)[1]-m+1):dim(ts_arimax)[1],-3])$pred[1:12]}
        else{fit <- Arima(ts_train[,i], xreg=xreglist[[i]][1:(dim(ts_arimax)[1]-m),], order=c(p,d,q),method="CSS")
        ts_pred[,i]=predict(fit,12,newxreg=xreglist[[i]][(dim(ts_arimax)[1]-m+1):dim(ts_arimax)[1],])$pred[1:12]}
        rmse12[i]=rmse(ts_test[,i],ts_pred[,i])
        rmse1[i]=rmse(ts_test[,i][1],ts_pred[,i][1])
        smape12[i]=sMAPE(exp(ts_test[,i]),exp(ts_pred[,i]))
        smape1[i]=sMAPE(exp(ts_test[,i][1]),exp(ts_pred[,i][1]))
        rmsefit[i]=sqrt(mean((fit$residuals)^2))
      }
      counter=counter+1
      arimaxresult[counter,1]=paste("ARIMAX(",p,",",d,",",q,")",sep="")
      arimaxresult[counter,2]=as.numeric(mean(rmse1))
      arimaxresult[counter,3]=as.numeric(mean(smape1))
      arimaxresult[counter,4]=as.numeric(mean(rmse12))
      arimaxresult[counter,5]=as.numeric(mean(smape12)) 
      arimaxresult[counter,6]=as.numeric(mean(rmsefit))}}}

#das Modell mit dem besten in sample fit und das modell für den keine zeitreihe ma part not invertible
arimaxresult=arimaxresult[arimaxresult[,2]!=0,]
arimaxresult_good=arimaxresult[arimaxresult[,6]==min(arimaxresult[,6])|arimaxresult[,1]=="ARIMAX(0,1,3)",]
arimaxresult_good[,2:6]=round(as.numeric(arimaxresult_good[,2:6]),3)


#################################################################################################
#auto arima

rmse_auto_12=seq(1,length(train_splitting))
rmse_auto_1=seq(1,length(train_splitting))
SMAPE_auto_12=seq(1,length(train_splitting))
SMAPE_auto_1=seq(1,length(train_splitting))
RMSE_in=seq(1,length(train_splitting))


for (i in 1:length(train_splitting)) {fit=auto.arima(mult_ts[,i]);
ts_pred[,i]=forecast(fit,m)$mean
rmse_auto_12[i]=rmse(ts_test[,i],ts_pred[,i])
rmse_auto_1[i]=rmse(ts_test[,i][1],ts_pred[,i][1])
SMAPE_auto_12[i]=sMAPE(exp(ts_test[,i]),exp(ts_pred[,i]))
SMAPE_auto_1[i]=sMAPE(exp(ts_test[,i][1]),exp(ts_pred[,i][1]))
RMSE_in[i]=sqrt(mean((fit$residuals)^2))
}


fit=auto.arima(mult_ts[,2])
fit

auto_arima_result=data.frame("Auto ARIMA",round(mean(rmse_auto_1,),3), round(mean(SMAPE_auto_1),3),round(mean(rmse_auto_12),3),round(mean(SMAPE_auto_12,),3),round(mean(RMSE_in,),3))
colnames(auto_arima_result)=c("Methode","RMSE1","SMAPE1","RMSE12","SMAPE12","RMSE")
auto_arima_result=as.matrix(auto_arima_result)

TS_Results=rbind(armaresult_good,arimaresult_good,auto_arima_result,armaxresult_good,arimaxresult_good)
write.table(TS_Results,"TS_Results.csv")

print(xtable(TS_Results,caption = "Vorhersagefehler der Zeitreihenmodelle",label="Zeitreihenergebnisse"),include.rownames=FALSE)


#################################################################################################
#2. ML Methodem

#dont need week fixed for out of smple prediction
x_formula <- formula(~(logprice+pr+f+d+brand+vol_eq+producttype+package+flavorscent+fatcontent+
                         cookingmethod+saltsodiumcontent+typeofcut+iri_key)) 

y_formula <- formula(logunits~(logprice+pr+f+d+brand+vol_eq+producttype+package+flavorscent+fatcontent+
                                 cookingmethod+saltsodiumcontent+typeofcut+iri_key))

x_mat2 <- model.matrix(x_formula,data=SS_53_313)
x<- x_mat2[as.numeric(SS_53_313$week)<302,]
xtest<-x_mat2[as.numeric(SS_53_313$week)>301,]

y<-SS_53_313$logunits[as.numeric(SS_53_313$week)<302]
ytest<-SS_53_313$logunits[as.numeric(SS_53_313$week)>301]

# x and y for t+1
x1test<-x_mat2[as.numeric(SS_53_313$week)==302,]
y1test<-SS_53_313$logunits[as.numeric(SS_53_313$week)==302]


#linear Regression
f1 <- lm(y~x-1)
f1.coef <- f1$coefficients
f1.coef[is.na(f1.coef)] <- 0 

p1_12 <- f1.coef %*% t(xtest)
p1_1 <- f1.coef %*% t(x1test)


#forward stepwise

f2 <- lars(x=x,y=y,type="stepwise",eps=0.0001)
best_beta=which.min(f2$Cp)[1]

p2_12<-predict(f2,s=best_beta,xtest,type="fit",mode="step")$fit
p2_1<-predict(f2,s=best_beta,x1test,type="fit",mode="step")$fit


f2x<-predict(f2,s=best_beta,xtest,type="coefficients",mode="step")$coefficients

f2.coef_zero=f2x[f2x==0]
f2.coef_nonzero=f2x[f2x!=0]

#forward stagewise

f3=lars(type="forward.stagewise",x=x,y=y,eps=0.0001)

best_beta=which.min(f3$Cp)[1]


# Predict in Validation Set and Test Set
f3x <- predict(f3,s=best_beta,type="coef")$coef

f3.coef_zero=f3x[f3x==0]
f3.coef_nonzero=f3x[f3x!=0]

p3_12<-predict(f3,s=best_beta,xtest,type="fit",mode="step")$fit
p3_1<-predict(f3,s=best_beta,x1test,type="fit",mode="step")$fit

#Bagging
SS_train=SS_53_313[as.numeric(SS_53_313$week)<302,]
SS_test=SS_53_313[as.numeric(SS_53_313$week)>301,]
SS_test1=SS_53_313[as.numeric(SS_53_313$week)==302,]

set.seed(1)
f7 <- bagging(y_formula,SS_train)


# Predict in Validation Set and Test Set
p7_12 <- predict(f7,SS_test)
p7_1 <- predict(f7,SS_test1)


#LASSO
set.seed(1)
f11 <- cv.glmnet(x, y, type.measure="mse", 
                 alpha=1, family="gaussian")

p11_12 <-   predict(f11, s=f11$lambda.1se, newx=xtest)
p11_1 <-   predict(f11, s=f11$lambda.1se, newx=x1test)


#Ridge
set.seed(1)
f12 <- cv.glmnet(x, y, type.measure="mse", 
                 alpha=0, family="gaussian")

p12_12 <-   predict(f12, s=f12$lambda.1se, newx=xtest)
p12_1 <-   predict(f12, s=f12$lambda.1se, newx=x1test)

#Random Forest


h2o.init(nthreads=-1,max_mem_size = "12G")    
h2o.removeAll() 

train <- as.h2o(cbind(y,x))
test<- as.h2o(cbind(ytest,xtest))
test1<- as.h2o(cbind(y1test,x1test))

f14 <- h2o.randomForest(      
  training_frame = train,      
  x=3:154,                       
  y=1,                         
  ntrees = 500,                  
  max_depth = 400, 
  min_rows=5,
  score_each_iteration = T,      
  seed = 1000000, ignore_const_cols = FALSE )                


p14_12<-as.numeric(as.character(unlist(as.data.frame(h2o.predict(object = f14, newdata = test))[[1]])))
p14_1<-as.numeric(as.character(unlist(as.data.frame(h2o.predict(object = f14, newdata = test1))[[1]])))

h2o.shutdown(prompt=FALSE)


set.seed.parallelSWM(1)
f15 <- parallelSVM(x=x[,2:153],y=y,samplingSize = 0.8)

p15_12=as.numeric(as.character(predict(f15,xtest[,-1])))
p15_1=as.numeric(as.character(predict(f15,x1test[,-1])))


#########################################################################################
#Logit Modelle

#actual marketsize and shares

marketsize <- aggregate(units~iri_key+week,data=SS_53_313,sum,na.rm=T)
names(marketsize)[3] <- "marketsize"

SS_53_313$sortid=seq(1,dim(SS_53_313)[1])
SS_53_313 <- merge(SS_53_313,marketsize,by=c("iri_key","week"))
SS_53_313=SS_53_313[order(SS_53_313$sortid),]

SS_53_313$shareorg=SS_53_313$units/SS_53_313$marketsize

#prediction for marketsize

marketsize=data.table(marketsize)
marketsize_split=split(marketsize, by=c("iri_key"),drop=TRUE, keep.by=FALSE) #das brauche ich für ml methoden pred 12 weeks
marketsize_mat=matrix(0,nrow=dim(marketsize_split[[1]])[1],ncol=length(marketsize_split))

for (i in 1:length(marketsize_split)) {marketsize_mat[,i]=marketsize_split[[i]]$marketsize}

colnames(marketsize_mat)=names(marketsize_split)
p=12

ts_train=marketsize_mat[1:(dim(ts_mat)[1]-p),]
ts_test=marketsize_mat[(dim(ts_mat)[1]-p+1):dim(ts_mat)[1],]

mult_ts=ts(ts_train,start = c(2001,52),end = c(2006,40),frequency = 52)

ts_pred=matrix(0, nrow = p, ncol = length(marketsize_split))

rmse_arima_12=seq(1,length(marketsize_split))
rmse_arima_1=seq(1,length(marketsize_split))
smape_arima_12=seq(1,length(marketsize_split))
smape_arima_1=seq(1,length(marketsize_split))
RMSE_in=seq(1,length(marketsize_split))

arimaresult=matrix(0, nrow = 80, ncol = 6)
colnames(arimaresult)=c("Methode","RMSE_1_week","SMAPE_1_week","RMSE_12_weeks","SMAPE_12_weeks","RMSE")

counter=0
for (q in 0:3){
  for (d in 0:3) {
    for(p in 0:3){if ((q==2&d==3&p==1)|(q==3&d==0&p==3)|(q==3&d==3&p==1)|(q==3&d==3&p==3)) {
      next
    }
      for (i in 1:length(marketsize_split)) {fit=Arima(mult_ts[,i],order=c(p,d,q))
      ts_pred[,i]=predict(fit,12)$pred[1:12]
      rmse_arima_12[i]=rmse(ts_test[,i],ts_pred[,i])
      rmse_arima_1[i]=rmse(ts_test[,i][1],ts_pred[,i][1])
      smape_arima_12[i]=sMAPE(ts_test[,i],ts_pred[,i])
      smape_arima_1[i]=sMAPE(ts_test[,i][1],ts_pred[,i][1])
      RMSE_in[i]=sqrt(mean((fit$residuals)^2))
      }  
      counter=counter+1
      arimaresult[counter,1]=paste("arima(",p,",",d,",",q,")",sep="")
      arimaresult[counter,2]=mean(rmse_arima_1)
      arimaresult[counter,3]=mean(smape_arima_1)
      arimaresult[counter,4]=mean(rmse_arima_12)
      arimaresult[counter,5]=mean(smape_arima_12) 
      arimaresult[counter,6]=mean(RMSE_in)
    }}}


arimaresult_good=arimaresult[arimaresult[,2]!=0,]
arimaresult_good=arimaresult[as.numeric(arimaresult_good[,6])==min(as.numeric(arimaresult_good[,6])),]

for (i in 1:length(marketsize_split)) {fit=arima(mult_ts[,i],order=c(3,1,3))
ts_pred[,i]=predict(fit,12)$pred[1:12]}

#ts pred enthält die prediction der marketsize
#now prediction for market share


#shares with logit

x_formula <- formula(~(logprice+pr+f+d+brand+vol_eq+producttype+package+flavorscent+
                         fatcontent+cookingmethod+saltsodiumcontent+typeofcut))

x_mat <- model.matrix(x_formula,data=SS_53_313[as.numeric(SS_53_313$week)<302,])
y <- log(SS_53_313$shareorg[as.numeric(SS_53_313$week)<302])

x_mat2 <- model.matrix(x_formula,data=SS_53_313[as.numeric(SS_53_313$week)>301,])


f1 <- lm(y~x_mat-1)

f1.coef <- as.numeric(f1$coefficients)
f1.coef[is.na(f1.coef)] <- 0 

p <- exp(as.numeric(f1.coef %*% t(x_mat2)))

p=as.data.frame(cbind(p,as.vector(SS_53_313$week[as.numeric(SS_53_313$week)>301]),as.vector(SS_53_313$iri_key[as.numeric(SS_53_313$week)>301])))
colnames(p)[1:3]=c("p","week","iri_key")

nenner=aggregate(as.numeric(as.character(p))~iri_key+week,data=p,sum)
colnames(nenner)[3]="nenner"
p$sortid=seq(1,dim(p)[1])
p=merge(p,nenner,by=c("iri_key","week"))
p$share=as.numeric(as.character(p$p))/p$nenner


rownames(ts_pred)=seq(302,313)

ts_pred=cbind(ts_pred,rownames(ts_pred))

list=list()

for (i in 1:24) {list[[i]]=cbind(as.numeric(ts_pred[,i]),as.numeric(ts_pred[,25]),as.numeric(rep(names(marketsize_split)[i])))}

marketsize2=do.call(rbind, list)

colnames(marketsize2)=c("market","week","iri_key")
marketsize2=as.data.frame(marketsize2)

p$week=as.numeric(as.character(p$week))
p$iri_key=as.numeric(as.character(p$iri_key))

p=merge(p,marketsize2,by=c("iri_key","week"))

p$pred=p$share*p$market

p=p[order(p$sortid),]

p4_12=log(p$pred)
p4_1=log(p$pred[p$week==302])


Methoden=c("Lineare Regression","Forward Stepwise","Forward Stagewise","LASSO","Ridge","SVM","Random Forest","Bagging","Logit")


rmse_all_12=mapply(rmse,list(p1_12,p2_12,p3_12,p11_12,p12_12,p15_12,p14_12,p7_12,p4_12),MoreArgs=list(ytest) )
rmse_all_1=mapply(rmse,list(p1_1,p2_1,p3_1,p11_1,p12_1,p15_1,p14_1,p7_1,p4_1),MoreArgs=list(y1test) )
smape_all_12=mapply(sMAPE,list(exp(p1_12),exp(p2_12),exp(p3_12),exp(p11_12),exp(p12_12),exp(p15_12),exp(p14_12),exp(p7_12),exp(p4_12)),MoreArgs=list(exp(ytest)) )
smape_all_1=mapply(sMAPE,list(exp(p1_1),exp(p2_1),exp(p3_1),exp(p11_1),exp(p12_1),exp(p15_1),exp(p14_1),exp(p7_1),exp(p4_1)),MoreArgs=list(exp(y1test)) )


Resultate_ML=cbind(Methoden,rmse_1=round(rmse_all_1,3),smape_1=round(smape_all_1,3),rmse_12=round(rmse_all_12,3),smape_12=round(smape_all_12,3))

print(xtable(Resultate_ML,caption="Out of Sample Period Vorhersagefehler der maschinellen Lernmethoden",label="OutofSampleML1"),include.rownames=FALSE )


write.table(Resultate_ML,"Resultate_ML.csv")




#################################################################################################
#2.1 ML Methodem mit trend


#2.3 ML Methoden mit trend und lags 
rm(SS, ts_data,ts_data2)

SS_52=do.call(rbind,ML_split_lag)

SS_52=subset(SS_52,SS_52$week!=52)
SS_52$t=(as.numeric(SS_52$week)-53)/261
SS_52$t2=((as.numeric(SS_52$week)-53)/261)^2
SS_52$t3=((as.numeric(SS_52$week)-53)/261)^3


#dont need week fixed for out of smple prediction
x_formula <- formula(~(logprice+pr+f+d+brand+vol_eq+producttype+package+flavorscent+fatcontent+
                         cookingmethod+saltsodiumcontent+typeofcut+iri_key+t+t2+t3+lag.logunits.)) 

y_formula <- formula(logunits~(logprice+pr+f+d+brand+vol_eq+producttype+package+flavorscent+fatcontent+
                                 cookingmethod+saltsodiumcontent+typeofcut+iri_key+t+t2+t3+lag.logunits.))

x_mat2 <- model.matrix(x_formula,data=SS_52)
x<- x_mat2[as.numeric(SS_52$week)<302,]
xtest<-x_mat2[as.numeric(SS_52$week)>301,]

y<-SS_52$logunits[as.numeric(SS_52$week)<302]
ytest<-SS_52$logunits[as.numeric(SS_52$week)>301]

# x and y for t=302,313
test_weeks=seq(302,313)
liste=list()
liste2=list()

for (i in 1:12) {nam=paste("x",i,"test",sep="")
liste[[i]]=assign(nam,x_mat2[as.numeric(SS_52$week)==test_weeks[i],])
nam2=paste("y",i,"test",sep="")
liste2[[i]]=assign(nam2,as.data.frame(SS_52$logunits[as.numeric(SS_52$week)==test_weeks[i]]))
}

#linear Regression
f1 <- lm(y~x-1)
f1.coef <- f1$coefficients
f1.coef[is.na(f1.coef)] <- 0 

p1_1 <- f1.coef %*% t(x1test)

for (i in 2:12) {nam=paste("p1_",i,sep="")
j=i-1
liste[[i]][,157]=get(paste("p1_", j, sep=""))
assign(nam,f1.coef%*% t(liste[[i]]))
}

p1_12w=do.call(cbind,list(p1_1,p1_2,p1_3,p1_4,p1_5,p1_6,p1_7,p1_8,p1_9,p1_10,p1_11,p1_12))

y_12w=do.call(rbind,list(y1test,y2test,y3test,y4test,y5test,y6test,y7test,y8test,y9test,y10test,y11test,y12test))

lm_trend_lags=cbind(Methode="Lineare Regression",rmse_12=round(rmse(p1_12w,y_12w[,1]),3),smape_12=round(sMAPE(exp(p1_12w),exp(y_12w[,1])),3),rmse_1=round(rmse(p1_1,y1test[,1]),3),smape_1=round(sMAPE(exp(p1_1),exp(y1test[,1])),3))

#forward stepwise

f2 <- lars(x=x,y=y,type="stepwise",eps=0.0001)
best_beta=which.min(f2$Cp)[1]

p2_1<-predict(f2,s=best_beta,x1test,type="fit",mode="step")$fit

for (i in 2:12) {nam=paste("p2_",i,sep="")
j=i-1
liste[[i]][,157]=get(paste("p2_", j, sep=""))
assign(nam,as.vector(predict(f2,s=best_beta,liste[[i]],type="fit",mode="step")$fit))
}

p2_12w=c(p2_1,p2_2,p2_3,p2_4,p2_5,p2_6,p2_7,p2_8,p2_9,p2_10,p2_11,p2_12)

stepwise_trend_lags=cbind(Methode="Forward Stepwise",rmse_12=round(rmse(y_12w[,1],p2_12w),3),smape_12=round(sMAPE(exp(y_12w[,1]),exp(p2_12w)),3),rmse_1=round(rmse(p2_1,y1test[,1]),3),smape_1=round(sMAPE(exp(p2_1),exp(y1test[,1])),3))


#forward stagewise

f3 <- lars(x=x,y=y,type="forward.stagewise",eps=0.0001)
best_beta=which.min(f3$Cp)[1]

p3_1<-predict(f3,s=best_beta,x1test,type="fit",mode="step")$fit

for (i in 2:12) {nam=paste("p3_",i,sep="")
j=i-1
liste[[i]][,157]=get(paste("p3_", j, sep=""))
assign(nam,as.vector(predict(f3,s=best_beta,liste[[i]],type="fit",mode="step")$fit))
}

p3_12w=c(p3_1,p3_2,p3_3,p3_4,p3_5,p3_6,p3_7,p3_8,p3_9,p3_10,p3_11,p3_12)

stagewise_trend_lags=cbind(Methode="Forward Stagewise",rmse_12=round(rmse(y_12w[,1],p3_12w),3),smape_12=round(sMAPE(exp(y_12w[,1]),exp(p3_12w)),3),rmse_1=round(rmse(p3_1,y1test[,1]),3),smape_1=round(sMAPE(exp(p3_1),exp(y1test[,1])),3))


#Bagging

SS_train=SS_52[as.numeric(SS_52$week)<302,]

test_weeks=seq(302,313)
listeSS=list()

for (i in 1:12) {nam=paste("SS",i,"test",sep="")
listeSS[[i]]=assign(nam,SS_52[as.numeric(SS_52$week)==test_weeks[i],])
}

set.seed(1)
f7 <- bagging(y_formula,SS_train)

p7_1 <- predict(f7,SS1test)

for (i in 2:12) {nam=paste("p7_",i,sep="")
j=i-1
listeSS[[i]][,280]=get(paste("p7_", j, sep=""))
assign(nam,as.vector(predict(f7,listeSS[[i]])))
}


p7_12w=c(p7_1,p7_2,p7_3,p7_4,p7_5,p7_6,p7_7,p7_8,p7_9,p7_10,p7_11,p7_12)

SStest_logunits=c(SS1test$logunits,SS2test$logunits,SS3test$logunits,SS4test$logunits,SS5test$logunits,SS6test$logunits,SS7test$logunits,SS8test$logunits,SS9test$logunits,SS10test$logunits,SS11test$logunits,SS12test$logunits)


bagging_trend_lags=cbind(Methode="Bagging",rmse_12=round(rmse(SStest_logunits,p7_12w),3),smape_12=round(sMAPE(exp(SStest_logunits),exp(p7_12w)),3),rmse_1=round(rmse(p7_1,SS1test$logunits),3),smape_1=round(sMAPE(exp(p7_1),exp(SS1test$logunits)),3))


#LASSO
set.seed(1)
f11 <- cv.glmnet(x, y, type.measure="mse", 
                 alpha=1, family="gaussian")

p11_1 <-   predict(f11, s=f11$lambda.1se, newx=x1test)

for (i in 2:12) {nam=paste("p11_",i,sep="")
j=i-1
liste[[i]][,157]=get(paste("p11_", j, sep=""))
assign(nam,as.vector(predict(f11,s=f11$lambda.1se,newx=liste[[i]])))
}

p11_12w=c(p11_1,p11_2,p11_3,p11_4,p11_5,p11_6,p11_7,p11_8,p11_9,p11_10,p11_11,p11_12)

LASSO_trend_lags=cbind(Methode="LASSO",rmse_12=round(rmse(y_12w[,1],p11_12w),3),smape_12=round(sMAPE(exp(y_12w[,1]),exp(p11_12w)),3),rmse_1=round(rmse(p11_1,y1test[,1]),3),smape_1=round(sMAPE(exp(p11_1),exp(y1test[,1])),3))

#Ridge
set.seed(1)
f12 <- cv.glmnet(x, y, type.measure="mse", 
                 alpha=0, family="gaussian")

p12_1 <-   predict(f12, s=f12$lambda.1se, newx=x1test)

for (i in 2:12) {nam=paste("p12_",i,sep="")
j=i-1
liste[[i]][,157]=get(paste("p12_", j, sep=""))
assign(nam,as.vector(predict(f12,s=f11$lambda.1se,newx=liste[[i]])))
}

p12_12w=c(p12_1,p12_2,p12_3,p12_4,p12_5,p12_6,p12_7,p12_8,p12_9,p12_10,p12_11,p12_12)

Ridge_trend_lags=cbind(Methode="Ridge",rmse_12=round(rmse(y_12w[,1],p12_12w),3),smape_12=round(sMAPE(exp(y_12w[,1]),exp(p12_12w)),3),rmse_1=round(rmse(p12_1,y1test[,1]),3),smape_1=round(sMAPE(exp(p12_1),exp(y1test[,1])),3))


#Random Forest

xy=cbind(y,x)

h2o.init(nthreads=-1,max_mem_size = "12G")    
h2o.removeAll() 

train <- as.h2o(xy)

f14 <- h2o.randomForest(      
  training_frame = train,     
  x=3:157,                       
  y=1,                         
  ntrees = 500,                 
  max_depth = 400,
  min_rows=5,
  score_each_iteration = T,      
  seed = 1000000, ignore_const_cols = FALSE )                

p14_1<-as.numeric(as.character(unlist(as.data.frame(h2o.predict(object = f14, newdata = as.h2o(x1test)))[[1]])))

for (i in 2:12) {nam=paste("p14_",i,sep="")
j=i-1
liste[[i]][,157]=get(paste("p14_", j, sep=""))
assign(nam,as.numeric(as.character(unlist(as.data.frame(h2o.predict(object = f14, newdata = as.h2o(liste[[i]])))[[1]]))))
}

p14_12w=c(p14_1,p14_2,p14_3,p14_4,p14_5,p14_6,p14_7,p14_8,p14_9,p14_10,p14_11,p14_12)
RF_trend_lags=cbind(Methode="Random Forest",rmse_12=round(rmse(y_12w[,1],p14_12w),3),smape_12=round(sMAPE(exp(y_12w[,1]),exp(p14_12w)),3),rmse_1=round(rmse(p14_1,y1test[,1]),3),smape_1=round(sMAPE(exp(p14_1),exp(y1test[,1])),3))

h2o.shutdown(prompt=FALSE)

#SVM
set.seed.parallelSWM(1)
f15 <- parallelSVM(x=x[,2:157],y=y,samplingSize = 0.8)

p15_1=as.numeric(as.character(predict(f15,x1test[,-1])))

for (i in 2:12) {nam=paste("p15_",i,sep="")
j=i-1
liste[[i]][,157]=get(paste("p15_", j, sep=""))
assign(nam,as.vector(as.numeric(as.character(predict(f15,liste[[i]][,-1])))))
}

p15_12w=c(p15_1,p15_2,p15_3,p15_4,p15_5,p15_6,p15_7,p15_8,p15_9,p15_10,p15_11,p15_12)

SVM_trend_lags=cbind(Methode="SVM",rmse_12=round(rmse(y_12w[,1],p15_12w),3),smape_12=round(sMAPE(exp(y_12w[,1]),exp(p15_12w)),3),rmse_1=round(rmse(p15_1,y1test[,1]),3),smape_1=round(sMAPE(exp(p15_1),exp(y1test[,1])),3))

Result_trend_lags2=rbind(lm_trend_lags,stepwise_trend_lags,stagewise_trend_lags,bagging_trend_lags,LASSO_trend_lags,Ridge_trend_lags,RF_trend_lags,SVM_trend_lags)
Result_trend_lags3=cbind(Result_trend_lags2[,1],Result_trend_lags2[,4:5],Result_trend_lags2[,2:3])
Result_trend_lags=rbind(Result_trend_lags3[1:3,],Result_trend_lags3[5:6,],Result_trend_lags3[8,],Result_trend_lags3[7,],Result_trend_lags3[4,])
colnames(Result_trend_lags)[1]="Methode"
Result_trend_lags[4,1]="Lasso"

write.table(Result_trend_lags,"Result_trend_lags.csv")
print(xtable(Result_trend_lags,caption = "Resultate der Machine Learning Methoden mit Trend und Lags der verkauften Einheiten.",label="MLTrendLAg"),include.rownames=FALSE)


#3.1 ML Methoden Loop
#for looping over stor-item combinations #############################################################

x_formula3 <- formula(~(logprice+pr+f+d)) 
y_formula2 <- formula(logunits~(logprice+pr+f+d))


rmse_lm=seq(1,length(train_splitting))
SMAPE_lm=seq(1,length(train_splitting))

#12 weeks ahead
for (i in 1:length(train_splitting)) {x_mat3 <- model.matrix(x_formula3,ML_split[[i]])
x<- x_mat3[as.numeric(ML_split[[i]]$week)<302,]
xtest<-x_mat3[as.numeric(ML_split[[i]]$week)>301,]

y<-ML_split[[i]]$logunits[as.numeric(ML_split[[i]]$week)<302]
ytest<-ML_split[[i]]$logunits[as.numeric(ML_split[[i]]$week)>301]
f1 <- lm(y~x-1)
f1.coef <- f1$coefficients
f1.coef[is.na(f1.coef)] <- 0 
p1 <- f1.coef %*% t(xtest)
rmse_lm[i]=rmse(p1,ytest)
SMAPE_lm[i]=sMAPE(exp(p1),exp(ytest))
}

rmse_12_lm=mean(rmse_lm) #0.4855198
smape_12_lm=mean(SMAPE_lm) #0.1678928



#1 week ahead
for (i in 1:length(train_splitting)) {x_mat3 <- model.matrix(x_formula3,ML_split[[i]])
x<- x_mat3[as.numeric(ML_split[[i]]$week)<302,]
xtest<-x_mat3[as.numeric(ML_split[[i]]$week)==302,]

y<-ML_split[[i]]$logunits[as.numeric(ML_split[[i]]$week)<302]
ytest<-ML_split[[i]]$logunits[as.numeric(ML_split[[i]]$week)==302]
f1 <- lm(y~x-1)
f1.coef <- f1$coefficients
f1.coef[is.na(f1.coef)] <- 0 
p1 <- f1.coef %*% xtest
rmse_lm[i]=rmse(p1,ytest)
SMAPE_lm[i]=sMAPE(exp(p1),exp(ytest))
}

rmse_1_lm=mean(rmse_lm) #0.3604654
smape_1_lm=mean(SMAPE_lm) #0.1382299


LM_result=cbind(Methode="linear Regression",rmse_12=round(rmse_12_lm,3),smape_12=round(smape_12_lm,3),rmse_1=round(rmse_1_lm,3),smape_1=round(smape_1_lm,3))

#forward stepwise loop

rmse_1=seq(1,length(train_splitting))
SMAPE_1=seq(1,length(train_splitting))

rmse_12=seq(1,length(train_splitting))
SMAPE_12=seq(1,length(train_splitting))

for (i in 1:length(train_splitting)) {x_mat3 <- model.matrix(x_formula3,ML_split[[i]])
x<- x_mat3[as.numeric(ML_split[[i]]$week)<302,]
xtest<-x_mat3[as.numeric(ML_split[[i]]$week)>301,]

y<-ML_split[[i]]$logunits[as.numeric(ML_split[[i]]$week)<302]
ytest<-ML_split[[i]]$logunits[as.numeric(ML_split[[i]]$week)>301]

f2=lars(x=x,y=y,type="stepwise",eps=0.0001)

best_beta=which.min(f2$Cp)[1]

p2<-predict(f2,s=best_beta,xtest,type="fit",mode="step")$fit

rmse_12[i]=rmse(p2,ytest)
SMAPE_12[i]=sMAPE(exp(p2),exp(ytest))
rmse_1[i]=rmse(p2[1],ytest[1])
SMAPE_1[i]=sMAPE(exp(p2[1]),exp(ytest[1]))
}
Stepwise_result=cbind(Methode="Forward Stepwise",rmse_12=round(mean(rmse_12),3),smape_12=round(mean(SMAPE_12),3),rmse_1=round(mean(rmse_1),3),smape_1=round(mean(SMAPE_1),3))

#forward stagewise loop ################################################################

for (i in 1:length(train_splitting)) {x_mat3 <- model.matrix(x_formula3,ML_split[[i]])
x<- x_mat3[as.numeric(ML_split[[i]]$week)<302,]
xtest<-x_mat3[as.numeric(ML_split[[i]]$week)>301,]

y<-ML_split[[i]]$logunits[as.numeric(ML_split[[i]]$week)<302]
ytest<-ML_split[[i]]$logunits[as.numeric(ML_split[[i]]$week)>301]

f2=lars(x=x,y=y,type="forward.stagewise",eps=0.0001)

best_beta=which.min(f2$Cp)[1]

p2<-predict(f2,s=best_beta,xtest,type="fit",mode="step")$fit

rmse_12[i]=rmse(p2,ytest)
SMAPE_12[i]=sMAPE(exp(p2),exp(ytest))
rmse_1[i]=rmse(p2[1],ytest[1])
SMAPE_1[i]=sMAPE(exp(p2[1]),exp(ytest[1]))
}
Stagewise_result=cbind(Methode="Forward Stagewise",rmse_12=round(mean(rmse_12),3),smape_12=round(mean(SMAPE_12),3),rmse_1=round(mean(rmse_1),3),smape_1=round(mean(SMAPE_1),3))


#Lasso loop ################################################################
set.seed(1)
for (i in 1:length(train_splitting)) {x_mat3 <- model.matrix(x_formula3,ML_split[[i]])
x<- x_mat3[as.numeric(ML_split[[i]]$week)<302,]
xtest<-x_mat3[as.numeric(ML_split[[i]]$week)>301,]

y<-ML_split[[i]]$logunits[as.numeric(ML_split[[i]]$week)<302]
ytest<-ML_split[[i]]$logunits[as.numeric(ML_split[[i]]$week)>301]

f11 <- cv.glmnet(x, y, type.measure="mse",alpha=1, family="gaussian")

p11 <-   predict(f11, s=f11$lambda.1se, newx=xtest)

rmse_12[i]=rmse(p11,ytest)
SMAPE_12[i]=sMAPE(exp(p11),exp(ytest))
rmse_1[i]=rmse(p11[1],ytest[1])
SMAPE_1[i]=sMAPE(exp(p11[1]),exp(ytest[1]))
}
LASSO_result=cbind(Methode="LASSO",rmse_12=round(mean(rmse_12),3),smape_12=round(mean(SMAPE_12),3),rmse_1=round(mean(rmse_1),3),smape_1=round(mean(SMAPE_1),3))

#Ridge loop ################################################################
set.seed(1)
for (i in 1:length(train_splitting)) {x_mat3 <- model.matrix(x_formula3,ML_split[[i]])
x<- x_mat3[as.numeric(ML_split[[i]]$week)<302,]
xtest<-x_mat3[as.numeric(ML_split[[i]]$week)>301,]

y<-ML_split[[i]]$logunits[as.numeric(ML_split[[i]]$week)<302]
ytest<-ML_split[[i]]$logunits[as.numeric(ML_split[[i]]$week)>301]

f11 <- cv.glmnet(x, y, type.measure="mse",alpha=0, family="gaussian")

p11 <-   predict(f11, s=f11$lambda.1se, newx=xtest)

rmse_12[i]=rmse(p11,ytest)
SMAPE_12[i]=sMAPE(exp(p11),exp(ytest))
rmse_1[i]=rmse(p11[1],ytest[1])
SMAPE_1[i]=sMAPE(exp(p11[1]),exp(ytest[1]))
}
Ridge_result=cbind(Methode="Ridge",rmse_12=round(mean(rmse_12),3),smape_12=round(mean(SMAPE_12),3),rmse_1=round(mean(rmse_1),3),smape_1=round(mean(SMAPE_1),3))




#Bagging loop ###########################################################################

rmse_bg_1=seq(1,length(train_splitting))
SMAPE_bg_1=seq(1,length(train_splitting))

rmse_bg_12=seq(1,length(train_splitting))
SMAPE_bg_12=seq(1,length(train_splitting))

set.seed(1)
for (i in 1:length(train_splitting)){f7 <- bagging(y_formula2,ML_split[[i]][as.numeric(ML_split[[i]]$week)<302,])
p7=predict(f7,ML_split[[i]][as.numeric(ML_split[[i]]$week)>301,])
p7_1=predict(f7,ML_split[[i]][as.numeric(ML_split[[i]]$week)==302,])

rmse_bg_12[i]=rmse(p7,ML_split[[i]]$logunits[as.numeric(ML_split[[i]]$week)>301])
SMAPE_bg_12[i]=sMAPE(exp(p7),exp(ML_split[[i]]$logunits[as.numeric(ML_split[[i]]$week)>301]))
rmse_bg_1[i]=rmse(p7_1,ML_split[[i]]$logunits[as.numeric(ML_split[[i]]$week)==302])
SMAPE_bg_1[i]=sMAPE(exp(p7_1),exp(ML_split[[i]]$logunits[as.numeric(ML_split[[i]]$week)==302]))

}


Bagging_result=cbind(Methode="Bagging",rmse_12=round(mean(rmse_bg_12),3),smape_12=round(mean(SMAPE_bg_12),3),rmse_1=round(mean(rmse_bg_1),3),smape_1=round(mean(SMAPE_bg_1),3))

#RF loop
rmse_rf_1=seq(1,length(train_splitting))
SMAPE_rf_1=seq(1,length(train_splitting))

h2o.init(nthreads=-1,max_mem_size = "12G")    
h2o.removeAll() 


for (i in 1:length(train_splitting)) {x_mat3 <- model.matrix(x_formula3,ML_split[[i]])
x<- x_mat3[as.numeric(ML_split[[i]]$week)<302,]
xtest<-x_mat3[as.numeric(ML_split[[i]]$week)>301,]

y<-ML_split[[i]]$logunits[as.numeric(ML_split[[i]]$week)<302]
ytest<-ML_split[[i]]$logunits[as.numeric(ML_split[[i]]$week)>301]

xy=cbind(y,x)
testxy=cbind(ytest,xtest)


train <- as.h2o(xy)
test<- as.h2o(testxy)

f14 <- h2o.randomForest(      
  training_frame = train,      
  x=3:dim(train)[2],                       
  y=1,                         
  ntrees = 500,                 
  max_depth = 400,
  nbins=5,
  score_each_iteration = T,      
  seed = 1000000, ignore_const_cols = FALSE )                


p14_12<-h2o.predict(object = f14, newdata = test)
p14_12=as.data.frame(p14_12)
p14_12=as.numeric(as.character(unlist(p14_12[[1]])))

p14_1<-h2o.predict(object = f14, newdata = test[1,])
p14_1=as.data.frame(p14_1)
p14_1=as.numeric(as.character(unlist(p14_1[[1]])))

rmse_lm[i]=rmse(p14_12,ytest)
SMAPE_lm[i]=sMAPE(exp(p14_12),exp(ytest))

rmse_rf_1[i]=rmse(p14_1,ytest[1])
SMAPE_rf_1[i]=sMAPE(exp(p14_1),exp(ytest[1]))

}

rmse_1_rf=mean(rmse_rf_1) #0.3604654
smape_1_rf=mean(SMAPE_rf_1)

rmse_12_rf=mean(rmse_lm) 
smape_12_rf=mean(SMAPE_lm)


RF_result=cbind(Methode="RF",rmse_12=round(rmse_12_rf,3),smape_12=round(smape_12_rf,3),rmse_1=round(rmse_1_rf,3),smape_1=round(smape_1_rf,3))

#parallelSVM loop #############################################################################
rm(arimax_splitting, ts_data,SS_53_313,SS_ts,SS_ARIMAX)
x_formula3 <- formula(~(logprice+pr+f+d)) 
y_formula2 <- formula(logunits~(logprice+pr+f+d))


rmse_svm_1=seq(1,length(ML_split))
SMAPE_svm_1=seq(1,length(ML_split))

rmse_svm_12=seq(1,length(ML_split))
SMAPE_svm_12=seq(1,length(ML_split))


set.seed.parallelSWM(1)
for (i in 1:length(ML_split)){
x_mat3 <- model.matrix(x_formula3,ML_split[[i]])
x<- x_mat3[as.numeric(ML_split[[i]]$week)<302,]
xtest<-x_mat3[as.numeric(ML_split[[i]]$week)>301,]

y<-ML_split[[i]]$logunits[as.numeric(ML_split[[i]]$week)<302]
ytest<-ML_split[[i]]$logunits[as.numeric(ML_split[[i]]$week)>301]

f7 <- parallelSVM(x=x,y=y,samplingSize = 0.8)
p15=predict(f7,xtest)

rmse_svm_12[i]=rmse(as.numeric(as.character(p15)),ytest)
SMAPE_svm_12[i]=sMAPE(exp(as.numeric(as.character(p15))),exp(ytest))
rmse_svm_1[i]=rmse(as.numeric(as.character(p15))[1],ytest[1])
SMAPE_svm_1[i]=sMAPE(exp(as.numeric(as.character(p15))[1]),exp(ytest[1]))
print(i)
}

SVM_result=cbind(Methode="SVM",rmse_12=round(mean(rmse_svm_12),3),smape_12=round(mean(SMAPE_svm_12),3),rmse_1=round(mean(rmse_svm_1),3),smape_1=round(mean(SMAPE_svm_1),3))

for (i in 1:length(ML_split)){f7 <- svm(y_formula2,ML_split[[i]][as.numeric(ML_split[[i]]$week)<302,],kernel="radial",scale=FALSE)
p15=predict(f7,ML_split[[i]][as.numeric(ML_split[[i]]$week)>301,])
p15_1=predict(f7,ML_split[[i]][as.numeric(ML_split[[i]]$week)==302,])

rmse_svm_12[i]=rmse(as.numeric(as.character(p15)),ML_split[[i]]$logunits[as.numeric(ML_split[[i]]$week)>301])
SMAPE_svm_12[i]=sMAPE(exp(p15),exp(ML_split[[i]]$logunits[as.numeric(ML_split[[i]]$week)>301]))
rmse_svm_1[i]=rmse(as.numeric(as.character(p15_1)),ML_split[[i]]$logunits[as.numeric(ML_split[[i]]$week)==302])
SMAPE_svm_1[i]=sMAPE(exp(p15_1),exp(ML_split[[i]]$logunits[as.numeric(ML_split[[i]]$week)==302]))
print(i)
}


SVM_result2=cbind(Methode="SVM(e1071)",rmse_12=round(mean(rmse_svm_12),3),smape_12=round(mean(SMAPE_svm_12),3),rmse_1=round(mean(rmse_svm_1),3),smape_1=round(mean(SMAPE_svm_1),3))

Resultate_loop=rbind(LM_result,Stepwise_result,Stagewise_result,LASSO_result,Ridge_result,SVM_result,SVM_result2,RF_result,Bagging_result)
Resultate_loop2=cbind(Resultate_loop[,1],Resultate_loop[,4:5],Resultate_loop[,2:3])

write.table(Resultate_loop2,"Resultate_loop2.csv")
print(xtable(Resultate_loop2,caption = "Resultate der Machine Learning Methoden angewendet auf die individuellen Geschäfts-/Produktkombinationen.",label="indvML"),include.rownames=FALSE)



#3.2 ML Loop with trend und lags
#for looping over stor-item combinations #############################################################

rm(ts_data,ts_data2,SS,SS_ARIMAX,lags,count_obs,count_table,SS_ts,SS_52_313,SS_53_313,logunits)


#add continous time variables
for (i in 1:length(ML_split_lag)) {ML_split_lag[[i]]=subset(ML_split_lag[[i]],ML_split_lag[[i]]$week!=52)
ML_split_lag[[i]]$t=(as.numeric(ML_split_lag[[i]]$week)-53)/261
ML_split_lag[[i]]$t2=((as.numeric(ML_split_lag[[i]]$week)-53)/261)^2
ML_split_lag[[i]]$t3=((as.numeric(ML_split_lag[[i]]$week)-53)/261)^3
}

x_formula3 <- formula(~(logprice+pr+f+d+t+t2+t3+lag.logunits.)) 
y_formula2 <- formula(logunits~(logprice+pr+f+d+t+t2+t3+lag.logunits.))

rmse12=seq(1,length(ML_split_lag))
SMAPE12=seq(1,length(ML_split_lag))
rmse1=seq(1,length(ML_split_lag))
SMAPE1=seq(1,length(ML_split_lag))

#prepare x and y

xtrainlist=list() #traininsdaten x werte
ytrain=matrix(0,nrow=249,ncol=119) #trainigsdaten y Werte
xtestlist=list() #testdaten x werte
ytest=matrix(0,nrow = 12,ncol = 119) #y werte test set
ypred=matrix(0,nrow = 12,ncol = 119) #y werte predicted


test_weeks=seq(302,313)


for (i in 1:length(ML_split_lag)) {
x_mat3 <- model.matrix(x_formula3,ML_split_lag[[i]])
xtrainlist[[i]]=x_mat3[as.numeric(ML_split_lag[[i]]$week)<302,]
ytrain[,i]=ML_split_lag[[i]]$logunits[as.numeric(ML_split_lag[[i]]$week)<302]
xtestlist[[i]]=x_mat3[as.numeric(ML_split_lag[[i]]$week)>301,]
ytest[,i]=ML_split_lag[[i]]$logunits[as.numeric(ML_split_lag[[i]]$week)>301]
}

#lineare Regression

for (i in 1:length(ML_split_lag)) {f1=lm(ytrain[,i]~xtrainlist[[i]]-1)
f1.coef <- f1$coefficients
f1.coef[is.na(f1.coef)] <- 0 
p1_1 <- f1.coef %*% xtestlist[[i]][1,]
ypred[1,i]=p1_1
for (j in 2:12) {nam=paste("p1_",j,sep="")
k=j-1
xtestlist[[i]][j,13]=get(paste("p1_", k, sep=""))
ypred[j,i]=assign(nam,f1.coef%*% xtestlist[[i]][j,])
}}

for (i in 1:119) {
 rmse12[i]= rmse(ypred[,i],ytest[,i])
 SMAPE12[i]= sMAPE(exp(ypred[,i]),exp(ytest[,i]))
 rmse1[i]= rmse(ypred[1,i],ytest[1,i])
 SMAPE1[i]= sMAPE(exp(ypred[1,i]),exp(ytest[1,i]))
}

LM_result=cbind(Methode="linear Regression",rmse_12=round(mean(rmse12),3),smape_12=round(mean(SMAPE12),3),rmse_1=round(mean(rmse1),3),smape_1=round(mean(SMAPE1),3))

#forward stepwise loop

for (i in 1:length(ML_split_lag)) {f2=lars(x=xtrainlist[[i]],y=ytrain[,i],type="stepwise",eps=0.0001)
best_beta=which.min(f2$Cp)[1]
p2_1 <- predict(f2,s=best_beta,xtestlist[[i]],type="fit",mode="step")$fit[1]
ypred[1,i]=p2_1
for (j in 2:12) {nam=paste("p2_",j,sep="")
k=j-1
xtestlist[[i]][j,13]=get(paste("p2_", k, sep=""))
ypred[j,i]=assign(nam,predict(f2,s=best_beta,xtestlist[[i]],type="fit",mode="step")$fit[j])
}}

for (i in 1:119) {
  rmse12[i]= rmse(ypred[,i],ytest[,i])
  SMAPE12[i]= sMAPE(exp(ypred[,i]),exp(ytest[,i]))
  rmse1[i]= rmse(ypred[1,i],ytest[1,i])
  SMAPE1[i]= sMAPE(exp(ypred[1,i]),exp(ytest[1,i]))
}


Stepwise_result=cbind(Methode="Forward Stepwise",rmse_12=round(mean(rmse12),3),smape_12=round(mean(SMAPE12),3),rmse_1=round(mean(rmse1),3),smape_1=round(mean(SMAPE1),3))

#forward stagewise loop ################################################################

for (i in 1:length(ML_split_lag)) {f3=lars(x=xtrainlist[[i]],y=ytrain[,i],type="forward.stagewise",eps=0.0001)
best_beta=which.min(f3$Cp)[1]
p3_1 <- predict(f3,s=best_beta,xtestlist[[i]],type="fit",mode="step")$fit[1]
ypred[1,i]=p3_1
for (j in 2:12) {nam=paste("p3_",j,sep="")
k=j-1
xtestlist[[i]][j,13]=get(paste("p3_", k, sep=""))
ypred[j,i]=assign(nam,predict(f3,s=best_beta,xtestlist[[i]],type="fit",mode="step")$fit[j])
}}

for (i in 1:119) {
  rmse12[i]= rmse(ypred[,i],ytest[,i])
  SMAPE12[i]= sMAPE(exp(ypred[,i]),exp(ytest[,i]))
  rmse1[i]= rmse(ypred[1,i],ytest[1,i])
  SMAPE1[i]= sMAPE(exp(ypred[1,i]),exp(ytest[1,i]))
}

Stagewise_result=cbind(Methode="Forward Stagewise",rmse_12=round(mean(rmse12),3),smape_12=round(mean(SMAPE12),3),rmse_1=round(mean(rmse1),3),smape_1=round(mean(SMAPE1),3))


#Lasso loop ################################################################
set.seed(1)
for (i in 1:length(ML_split_lag)) {f11 <- cv.glmnet(xtrainlist[[i]], ytrain[,i], type.measure="mse",alpha=1, family="gaussian")
p11_1 <- predict(f11, s=f11$lambda.1se, newx=xtestlist[[i]])[1]
ypred[1,i]=p11_1
for (j in 2:12) {nam=paste("p11_",j,sep="")
k=j-1
xtestlist[[i]][j,13]=get(paste("p11_", k, sep=""))
ypred[j,i]=assign(nam,predict(f11,s=f11$lambda.1se,xtestlist[[i]])[j])
}}

for (i in 1:119) {
  rmse12[i]= rmse(ypred[,i],ytest[,i])
  SMAPE12[i]= sMAPE(exp(ypred[,i]),exp(ytest[,i]))
  rmse1[i]= rmse(ypred[1,i],ytest[1,i])
  SMAPE1[i]= sMAPE(exp(ypred[1,i]),exp(ytest[1,i]))
}

LASSO_result=cbind(Methode="LASSO",rmse_12=round(mean(rmse12),3),smape_12=round(mean(SMAPE12),3),rmse_1=round(mean(rmse1),3),smape_1=round(mean(SMAPE1),3))

#Ridge loop ################################################################

set.seed(1)
for (i in 1:length(ML_split_lag)) {f12 <- cv.glmnet(xtrainlist[[i]], ytrain[,i], type.measure="mse",alpha=0, family="gaussian")
p12_1 <- predict(f12, s=f12$lambda.1se, newx=xtestlist[[i]])[1]
ypred[1,i]=p12_1
for (j in 2:12) {nam=paste("p12_",j,sep="")
k=j-1
xtestlist[[i]][j,13]=get(paste("p12_", k, sep=""))
ypred[j,i]=assign(nam,predict(f12,s=f12$lambda.1se,xtestlist[[i]])[j])
}}

for (i in 1:119) {
  rmse12[i]= rmse(ypred[,i],ytest[,i])
  SMAPE12[i]= sMAPE(exp(ypred[,i]),exp(ytest[,i]))
  rmse1[i]= rmse(ypred[1,i],ytest[1,i])
  SMAPE1[i]= sMAPE(exp(ypred[1,i]),exp(ytest[1,i]))
}

Ridge_result=cbind(Methode="Ridge",rmse_12=round(mean(rmse12),3),smape_12=round(mean(SMAPE12),3),rmse_1=round(mean(rmse1),3),smape_1=round(mean(SMAPE1),3))



#Bagging loop ###########################################################################

set.seed(1)
for (i in 1:length(ML_split_lag)){f7 <- bagging(y_formula2,ML_split_lag[[i]][as.numeric(ML_split_lag[[i]]$week)<302,])
p7_1=predict(f7,ML_split_lag[[i]][as.numeric(ML_split_lag[[i]]$week)==302,])
ypred[1,i]=p7_1

for (j in 2:12) {nam=paste("p7_",j,sep="")
k=j-1
ML_split_lag[[i]]$lag.logunits.[as.numeric(ML_split_lag[[i]]$week)==test_weeks[j]]=get(paste("p7_", k, sep=""))
ypred[j,i]=assign(nam,predict(f7,ML_split_lag[[i]][as.numeric(ML_split_lag[[i]]$week)==test_weeks[j],]))
}}

for (i in 1:119) {
  rmse12[i]= rmse(ypred[,i],ytest[,i])
  SMAPE12[i]= sMAPE(exp(ypred[,i]),exp(ytest[,i]))
  rmse1[i]= rmse(ypred[1,i],ytest[1,i])
  SMAPE1[i]= sMAPE(exp(ypred[1,i]),exp(ytest[1,i]))
}

Bagging_result=cbind(Methode="Bagging",rmse_12=round(mean(rmse12),3),smape_12=round(mean(SMAPE12),3),rmse_1=round(mean(rmse1),3),smape_1=round(mean(SMAPE1),3))

#RF loop


h2o.init(nthreads=-1,max_mem_size = "12G")    
h2o.removeAll() 


for (i in 1:length(ML_split_lag)) {
xy=cbind(ytrain[,i],xtrainlist[[i]])

train <- as.h2o(xy)

f14 <- h2o.randomForest(      
  training_frame = train,      
  x=3:dim(train)[2],                       
  y=1,                         
  ntrees = 500,                 
  max_depth = 400,
  nbins=5,
  score_each_iteration = T,      
  seed = 1000000, ignore_const_cols = FALSE )
p14_1<-as.numeric(as.character(unlist(as.data.frame(h2o.predict(object = f14, newdata = as.h2o(xtestlist[[i]])))[1])))[1]
ypred[1,i]=p14_1
for (j in 2:12) {nam=paste("p14_",j,sep="")
k=j-1
xtestlist[[i]][j,13]=get(paste("p14_", k, sep=""))
ypred[j,i]=assign(nam,as.numeric(as.character(unlist(as.data.frame(h2o.predict(object = f14, newdata = as.h2o(xtestlist[[i]])))[1])))[j])
}
}
for (i in 1:119) {
  rmse12[i]= rmse(ypred[,i],ytest[,i])
  SMAPE12[i]= sMAPE(exp(ypred[,i]),exp(ytest[,i]))
  rmse1[i]= rmse(ypred[1,i],ytest[1,i])
  SMAPE1[i]= sMAPE(exp(ypred[1,i]),exp(ytest[1,i]))
}

RF_result=cbind(Methode="RF",rmse_12=round(mean(rmse12),3),smape_12=round(mean(SMAPE12),3),rmse_1=round(mean(rmse1),3),smape_1=round(mean(SMAPE1),3))




#parallelSVM loop #############################################################################
rm(arimax_splitting, ts_data,SS_53_313,SS_ts,SS_ARIMAX)

set.seed.parallelSWM(1)
for (i in 1:length(ML_split_lag)){f7 <- parallelSVM(x=xtrainlist[[i]][,-1],y=ytrain[,i],probability=TRUE,samplesize=0.8)
p15_1=as.numeric(as.character(predict(f7,newdata=t(as.matrix(xtestlist[[i]][1,-1])),probability=TRUE)))
print(i)
ypred[1,i]=p15_1
for (j in 2:12) {nam=paste("p15_",j,sep="")
k=j-1
xtestlist[[i]][j,13]=get(paste("p15_", k, sep=""))
ypred[j,i]=assign(nam,as.numeric(as.character(predict(f7,newdata=t(as.matrix(xtestlist[[i]][j,-1]))))))
}
}

for (i in 1:119) {
  rmse12[i]= rmse(ypred[,i],ytest[,i])
  SMAPE12[i]= sMAPE(exp(ypred[,i]),exp(ytest[,i]))
  rmse1[i]= rmse(ypred[1,i],ytest[1,i])
  SMAPE1[i]= sMAPE(exp(ypred[1,i]),exp(ytest[1,i]))
}

SVM_result=cbind(Methode="SVM",rmse_12=round(mean(rmse12),3),smape_12=round(mean(SMAPE12),3),rmse_1=round(mean(rmse1),3),smape_1=round(mean(SMAPE1),3))


for (i in 1:length(ML_split_lag)){f7 <- svm(xtrainlist[[i]][,-1],ytrain[,i],type="eps",kernel="radial",scale=FALSE)
p15_1=predict(f7,newdata=t(as.matrix(xtestlist[[i]][1,-1])))
print(i)
ypred[1,i]=p15_1
for (j in 2:12) {nam=paste("p15_",j,sep="")
k=j-1
xtestlist[[i]][j,13]=get(paste("p15_", k, sep=""))
ypred[j,i]=assign(nam,as.numeric(as.character(predict(f7,newdata=t(as.matrix(xtestlist[[i]][j,-1]))))))
}
}

for (i in 1:119) {
  rmse12[i]= rmse(ypred[,i],ytest[,i])
  SMAPE12[i]= sMAPE(exp(ypred[,i]),exp(ytest[,i]))
  rmse1[i]= rmse(ypred[1,i],ytest[1,i])
  SMAPE1[i]= sMAPE(exp(ypred[1,i]),exp(ytest[1,i]))
}


SVM_result2=cbind(Methode="SVM (e1071)",rmse_12=round(mean(rmse12),3),smape_12=round(mean(SMAPE12),3),rmse_1=round(mean(rmse1),3),smape_1=round(mean(SMAPE1),3))

Result_loop_trend_lags=rbind(LM_result,Stepwise_result,Stagewise_result,LASSO_result,Ridge_result,SVM_result,SVM_result2,RF_result,Bagging_result)
Result_loop_trend_lags2=cbind(Result_loop_trend_lags[,1],Result_loop_trend_lags[,4:5],Result_loop_trend_lags[,2:3])
write.table(Result_loop_trend_lags,"Result_loop_trend_lags.csv")


print(xtable(Result_loop_trend_lags2,caption="Resultate der Machine Learning Methoden angewendet auf die individuellen Geschäfts-/Produktkombinationen mit Trend und Lags.",label="indMLTRendLag",digits=c(0,0,3,3,3,3)),include.rownames=FALSE)

