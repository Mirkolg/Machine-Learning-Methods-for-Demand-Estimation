rm(list=ls())
setwd("C:/Users/Nutzer/Desktop/Masterarbeit/code kompakt")


# 0.2  Load Required Packages and Functions ####################################

# Load the script which includes all the required packages and functions
source("Initialize.R")

# Set Run Parameters
# If test is TRUE, this will subset the data by 10% and run the code
test <- FALSE

# 0.3 Read and Prepare Data ####################################################

                              
# Load the Stata 12 format data
SS <- read.dta("salty_snack_main_l2_voleq.dta")
#SS <- read.dta("salty_snack_main_l2_voleq_0.05.dta")
#SS <- read.dta("salty_snack_main_l2_voleq_0.05_0.05.dta")

SS$sort_id=seq(1,dim(SS)[1])


if (test == TRUE) {
  #subset the data frame to test the code
  set.seed(1)  
  SS$sub <- runif(dim(SS)[1],0,1)
  SS <- subset(SS,sub<=0.2)
  SS <- subset(SS,select=-sub)
}


# Define Categorical Variables
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

# Spilt the data set into training(j=1), test(j=0) set and validation (j=2) set
set.seed(1)
SS$j <- runif(dim(SS)[1],0,1)
SS$j[SS$j>=0.4] <- 1
SS$j[SS$j<0.4 & SS$j >=0.25] <- 2
SS$j[SS$j<0.25] <- 0 

SS1 <- SS[SS$j==1,]
SS2 <- SS[SS$j==2,]
SS0 <- SS[SS$j==0,]


# 0.4 Create Formulae ##########################################################
 
# Define X Matrix for without competitor prices

  # Version 2: without competitor price
  
  x_formula <- formula(~(logprice+pr+f+d+brand+vol_eq+producttype+package+flavorscent+fatcontent+
                           cookingmethod+saltsodiumcontent+typeofcut+iri_key+week))
  x_mat <- model.matrix(x_formula,data=SS)
  
  
  x_formula2 <- formula(~(brand+vol_eq+producttype+package+flavorscent+fatcontent+
                           cookingmethod+saltsodiumcontent+typeofcut+iri_key+week-1))
  x_mat2 <- model.matrix(x_formula2,data=SS)
  
  y_formula <- formula(logunits~(logprice+pr+f+d+brand+vol_eq+producttype+package+flavorscent+fatcontent+
                           cookingmethod+saltsodiumcontent+typeofcut+iri_key+week))
  

  # Define Share forluma
  share_formula <- formula(share~logprice+pr+f+d+brand+vol_eq+producttype+package+flavorscent+
                           fatcontent+cookingmethod+saltsodiumcontent+typeofcut-1)
  share_formula2 <- formula(share2~logprice+pr+f+d+brand+vol_eq+producttype+package+flavorscent+
                            fatcontent+cookingmethod+saltsodiumcontent+typeofcut-1)

  
  share_formula4 <- formula(share4~logprice+pr+f+d+brand+vol_eq+producttype+package+flavorscent+
                              fatcontent+cookingmethod+saltsodiumcontent+typeofcut-1)

  
  share_formula6 <- formula(share4~logprice+pr+f+d+brand+vol_eq+producttype+package+flavorscent+
                              fatcontent+cookingmethod+saltsodiumcontent+typeofcut+withinnestshare-1)
  

# Define Covariates Matrix x and Response Variable y by Train(train*), Validation(val*) and Test(test*)
# Rename trainx and trainy to x and y for simpilicity
testx <- x_mat[SS$j==0,]
x <- x_mat[SS$j==1,]
valx <- x_mat[SS$j==2,]

testy <- SS$logunits[SS$j==0]
y <- SS$logunits[SS$j==1]
valy <- SS$logunits[SS$j==2]



# 1.0 Main Models ##############################################################
#                                              
#                                              
#                 9 Main Models and the Combined Model             
#                                              
#                                              
################################################################################


# Run 9 Models on Train Set
# 1.1 Model 1: Linear Regression ###############################################
f1 <- lm(y~x-1)

# Predict in Validation Set
f1.coef <- f1$coefficients
f1.coef[is.na(f1.coef)] <- 0 
p1 <- as.numeric(f1.coef %*% t(valx))

# Predict in Test Set
p1t <- as.numeric(f1.coef %*% t(testx))

# 1.2 Model 2: Forward Stepwise ################################################

f2 <- lars(x=x,y=y,type="stepwise",eps=0.0001,intercept = TRUE)

#colnames indicates step number
best_beta=which.min(f2$Cp)[1]

# Predict in Validation Set and Test Set

p2<-predict(f2,s=best_beta,valx,type="fit",mode="step")$fit
p2t <-predict(f2,s=best_beta,testx,type="fit",mode="step")$fit


# 1.3 Model 3: Forward Stagewise ###############################################
f3=lars(type="forward.stagewise",x=x,y=y,eps=0.0001)

best_beta=which.min(f3$Cp)[1]

# Predict in Validation Set and Test Set
p3<-predict(f3,s=best_beta,valx,type="fit",mode="step")$fit
p3t <-predict(f3,s=best_beta,testx,type="fit",mode="step")$fit


# 1.7 Model 7: Bagging #####################################################

set.seed(1)
f7 <- bagging(y_formula,SS1)

# Predict in Validation Set and Test Set
p7 <- predict(f7,SS2)
p7t <- predict(f7,SS0)


# 1.8 Model 8: Logistic Model ##################################################
# Predict Market Size Using a Gradient Boosting Model
set.seed(1)
f8x <- gbm.fixed(data=as.data.frame(cbind(x_mat2,SS$logunits)),1:dim(x_mat2)[2],dim(x_mat2)[2]+1,n.trees=500,family="gaussian",keep.data=TRUE)

SS$units_hat <- round(exp(f8x$fit))

# Aggregate Weekly Store Total Predicted Units and Merge Total Units (Market Size) into Data
total <- aggregate(units_hat~iri_key+week,data=SS,sum,na.rm=T)
names(total)[3] <- "total_units"
SS <- merge(SS,total,by=c("iri_key","week"))

SS=SS[order(SS$sort_id),]

# Calculate Predicted Market Share (A Market is a store during a week.)
SS$share <- SS$units_hat/SS$total_units


f8 <- glm(share_formula,data=SS[SS$j==1,],family=binomial)

# Predict in Validation Set
p8share <- predict(f8,newdata=SS2,type="response")
p8 <- log(p8share*SS$total_units[SS$j==2])

# Predict in Test Set
p8share.t <- predict(f8,newdata=SS0,type="response")
p8t <- log(p8share.t*SS$total_units[SS$j==0])


# 1.9 Model 9: Logistic Model (Traditional) ####################################


# Predict Market Size Using a Linear Model
f9x <- lm(logunits~brand+vol_eq+producttype+package+flavorscent+fatcontent+cookingmethod+saltsodiumcontent+typeofcut+iri_key+week,data=SS)

SS$units_hat2 <- round(exp(f9x$fit))

# Aggregate Weekly Store Total Predicted Units and Merge Total Units (Market Size) into Data
total2 <- aggregate(units_hat2~iri_key+week,data=SS,sum,na.rm=T)
names(total2)[3] <- "total_units2"
SS <- merge(SS,total2,by=c("iri_key","week"))
SS=SS[order(SS$sort_id),]

# Calculate Predicted Market Share (A Market is a store during a week.)
SS$share2 <- SS$units_hat2/SS$total_units2

f9 <- glm(share_formula2,data=SS[SS$j==1,],family=binomial)

# Predict in Validation Set

p9share <- predict(f9,newdata=SS2,type="response")
p9 <- log(p9share*SS$total_units2[SS$j==2])

# Predict in Test Set

p9share.t <- predict(f9,newdata=SS0,type="response")
p9t <- log(p9share.t*SS$total_units2[SS$j==0])


#1.9_4 average shares ##############################################################

#calculate average sales market/week for trainings set we need them to calculate marketsize including observations for val. and test
average_sales=aggregate(units~iri_key+week,mean,data=SS1)
names(average_sales)[3]="units_hat_4"

#merge in the avg sales
SS0=merge(SS0,average_sales,by=c("iri_key","week"))
SS0=SS0[order(SS0$sort_id),]

SS2=merge(SS2,average_sales,by=c("iri_key","week"))
SS2=SS2[order(SS2$sort_id),]

#here we can use observed sales
SS1$units_hat_4=SS1$units

S=rbind(SS0,SS2,SS1)

marketsize_pred=aggregate(units_hat_4~iri_key+week,sum,data=S)
names(marketsize_pred)[3]="total_units4"

S=merge(S,marketsize_pred,by=c("iri_key","week"))
S$share4=S$units_hat_4/S$total_units4
S=S[order(S$sort_id),]

#1.11 LASSO #################################################################

set.seed(1)
f11 <- cv.glmnet(x, y, type.measure="mse",alpha=1, family="gaussian")

p11 <-   predict(f11, s=f11$lambda.1se, newx=valx)
p11t <-   predict(f11, s=f11$lambda.1se, newx=testx)

#1.12 Ridge ###################################################################

set.seed(1)
f12 <- cv.glmnet(x, y, type.measure="mse",alpha=0, family="gaussian")

p12 <-   predict(f12, s=f12$lambda.1se, newx=valx)
p12t <-   predict(f12, s=f12$lambda.1se, newx=testx)

#1.14 RandomForest ################################################################
rm(SS,SS1,SS2,SS0,x_mat)
gc()

h2o.init(nthreads=-1,max_mem_size = "16G")    
h2o.removeAll() 

train <- as.h2o(cbind(y,x))
test<- as.h2o(cbind(testy,testx))
valid <- as.h2o(cbind(valy,valx))

rm(x,testx,valx)
gc()

f14 <- h2o.randomForest(        
  training_frame = train,       
  x=3:466,                   
  y=1,                        
  ntrees = 500,                  
  max_depth = 400, 
  min_rows=5,
  #stopping_rounds = 2,        
  score_each_iteration = T,      
  seed = 1000000, ignore_const_cols = FALSE)              

p14<-as.numeric(as.character(unlist(as.data.frame(h2o.predict(object = f14,newdata = valid))[[1]])))
p14t<-as.numeric(as.character(unlist(as.data.frame(h2o.predict(object = f14,newdata = test))[[1]])))

h2o.shutdown(prompt=FALSE)

#1.15 SVM ####################################################################

set.seed.parallelSWM(1)
f15 <- parallelSVM(x=x[,-1],y=y,samplingSize = 0.1,numberCores = 5)

p15=as.numeric(as.character(predict(object = f15,newdata=valx[,-1])))
p15t=as.numeric(as.character(predict(object = f15,newdata=testx[,-1])))


###########################################################################
#Ergebnisse exportieren

write.table(p1,"p1.csv")
write.table(p2,"p2.csv")
write.table(p3,"p3.csv")
write.table(p7,"p7.csv")
write.table(p8,"p8.csv")
write.table(p9,"p9.csv")
write.table(p11,"p11.csv")
write.table(p12,"p12.csv")
write.table(p14,"p14.csv")
write.table(p15,"p15.csv")

write.table(p1t,"p1t.csv")
write.table(p2t,"p2t.csv")
write.table(p3t,"p3t.csv")
write.table(p7t,"p7t.csv")
write.table(p8t,"p8t.csv")
write.table(p9t,"p9t.csv")
write.table(p11t,"p11t.csv")
write.table(p12t,"p12t.csv")
write.table(p14t,"p14t.csv")
write.table(p15t,"p15t.csv")

p1=read.table("p1.csv")[,1]
p2=read.table("p2.csv")[,1]
p3=read.table("p3.csv")[,1]
p7=read.table("p7.csv")[,1]
p8=read.table("p8.csv")[,1]
p9=read.table("p9.csv")[,1]
p9_5=read.table("p9_5.csv")[,1]
p10=read.table("p10.csv")[,1]
p11=read.table("p11.csv")[,1]
p12=read.table("p12.csv")[,1]
p14=read.table("p14.csv")[,1]
p15=read.table("p15.csv")[,1]
p16=read.table("p16.csv")[,1]

p1t=read.table("p1t.csv")[,1]
p2t=read.table("p2t.csv")[,1]
p3t=read.table("p3t.csv")[,1]
p7t=read.table("p7t.csv")[,1]
p8t=read.table("p8t.csv")[,1]
p9t=read.table("p9t.csv")[,1]
p9t_5=read.table("p9t_5.csv")[,1]
p10t=read.table("p10t.csv")[,1]
p11t=read.table("p11t.csv")[,1]
p12t=read.table("p12t.csv")[,1]
p14t=read.table("p14t.csv")[,1]
p15t=read.table("p15t.csv")[,1]
p16t=read.table("p16t.csv")[,1]

Library=as.data.frame(cbind(SS$week,as.numeric(as.character(SS$iri_key)),SS$colupc,SS$sort_id))
colnames(Library)=c("week","iri_key","colupc","sort_id")
colnames(p16)=c("pred","week","iri_key","colupc")
colnames(p16t)=c("pred","week","iri_key","colupc")


p16_sort=merge(p16,Library,by=c("iri_key","colupc","week"))
p16_sort=p16_sort[order(p16_sort$sort_id),]

p16t_sort=merge(p16t,Library,by=c("iri_key","colupc","week"))
p16t_sort=p16t_sort[order(p16t_sort$sort_id),]

p16=p16_sort$pred
p16t=p16t_sort$pred

write.table(p16,"p16.csv")
write.table(p16t,"p16t.csv")

#  Combine Models modified##########################################################
# Regress Actual Values on Eight Predict Series
# Constraints: Coefficients are positive, sum to 1 and no intercept
X <- t(as.matrix(rbind(p1,p2,p3,p7,p8,p9,p9_5,p10,p11,p12,p14,p15,p16)))
Rinv <- solve(chol(t(X) %*% X))
A <- cbind(rep(1,dim(X)[2]),diag(dim(X)[2]))
B <- c(1,rep(0,dim(X)[2]))
D <- t(valy) %*% X
# Calculate the coefficients, aka weights of models
sol <- solve.QP(Dmat=Rinv,dvec=D,Amat=A,bvec=B,meq=1,factorized=TRUE)
f_combine <- sol$solution
p_combine <- f_combine %*% t(X)

# Apply the Weights in Test Set and Create a New Prediction
X.t <- t(as.matrix(rbind(p1t,p2t,p3t,p7t,p8t,p9t,p9t_5,p10t,p11t,p12t,p14t,p15t,p16t)))
p_combine_t <- f_combine %*% t(X.t)

expvaly=exp(valy)
exptesty=exp(testy)

rmse_all <-  mapply(rmse,list(p1,p2,p3,p7,p8,p9,p9_5,p10,p11,p12,p14,p15,p16,p_combine),MoreArgs=list(valy)) 
smape_all=mapply(sMAPE,list(exp(p1),exp(p2),exp(p3),exp(p7),exp(p8),exp(p9),exp(p9_5),exp(p10),exp(p11),exp(p12),exp(p14),exp(p15),exp(p16),exp(p_combine)),MoreArgs=list(expvaly)) 

rmse_all_test <-  mapply(rmse,list(p1t,p2t,p3t,p7t,p8t,p9t,p9t_5,p10t,p11t,p12t,p14t,p15t,p16t,p_combine_t),MoreArgs=list(testy)) 
smape_all_test=mapply(sMAPE,list(exp(p1t),exp(p2t),exp(p3t),exp(p7t),exp(p8t),exp(p9t),exp(p9t_5),exp(p10t),exp(p11t),exp(p12t),exp(p14t),exp(p15t),exp(p16t),exp(p_combine_t)),MoreArgs=list(exptesty)) 


#für alle Daten
Methode=c("Lineare Regression","Forward Stepwise","Forward Stagewise","Bagging","Logit GB","Logit","Logit OLS","Nested-Logit","Lasso","Ridge","Random Forest","SVM","Mixed-Logit","Komb. Mod.")

result=cbind(Methode,round(rmse_all,3),round(smape_all,3),round(rmse_all_test,3), round(smape_all_test,3),paste(round(append(f_combine,1)*100, 2), "%", sep=""))

colnames(result)[2]=c("RMSE_val")
colnames(result)[3]=c("SMAPE_val")
colnames(result)[4]=c("RMSE_test")
colnames(result)[5]=c("SMAPE_test")
colnames(result)[6]=c("Gewichte")


result_sortiert=rbind(result[1:3,],result[9:10,],result[12,],result[11,],result[4,],result[5:7,],result[8,],result[13,],result[14,])

print(xtable(result_sortiert,caption="Vergleich der Modelle anhand des Vorhersagefehlers (die ersten beiden Logit Modelle sind die in vorgeschlagenen, Logit OLS ist das Modell das ich zusätzlcih verwendet habe)",label="tab:kombiniertesModell"),include.rownames=FALSE)

write.table(result_sortiert,"result_full.csv")


########################################################################################
#detalierte Ergebnisse zu den einzelnen Methoden

#1. lin. Regression

f1_sum=summary(f1)$coef
f1_sum=summary(f1)$coef[drop=F]

#adjust p values

f1_sum_sig=subset(f1_sum,f1_sum[,4]<=0.05)


#delete week and irikey coefficents before export
week_colnames=paste("x",colnames(x)[83:465],sep="")

f1_sum_sig=f1_sum_sig[!rownames(f1_sum_sig) %in% week_colnames, ]
xtable(f1_sum_sig,caption = "Signifikante Koeffizienten der linearen Regression (p-Werte gerundet auf 3 Nachkommastellen)",label = "tab:lineareRegression",digits=c(0,3,3,3,3))


#random forest

f14_var_ip=h2o.varimp(f14)
f14_var_ip=f14_var_ip[1:10,]
f14_var_ip[,2]=round(f14_var_ip[,2],0)
f14_var_ip[,3]=round(f14_var_ip[,3],2)
f14_var_ip[,4]=round(f14_var_ip[,4],2)

print(xtable(f14_var_ip,caption = "Bedeutung der unterschiedlichen Variablen für die Reinheit in den Knoten.",digits=c(0,0,0,2,2)),include.rownames=FALSE)

#nested logit

f10_sum=summary(fm)$coef
f10_sum_sig=subset(f10_sum,f10_sum[,4]<=0.05)
xtable(f10_sum_sig,caption = "Signifikante Koeffizienten der Mixed Logit (p-Werte gerundet auf 3 Nachkommastellen)",label = "tab:MixedLogitOLS",digits=c(0,3,3,3,3))


