rm(list=ls())
setwd("C:/Users/Nutzer/Desktop/Masterarbeit/code kompakt")

# 0.2  Load Required Packages and Functions ####################################

# Load the script which includes all the required packages and functions
source("Initialize.R")

#der erste Part erstellt die IVs für die Mixed Logit ansosten S_instruments laden in 1.1
##############################################################################################################################################################
#BLP instruments 1.0

SSS=SS[c("iri_key","colupc","l4","week","units","j","logprice","pr","f","d","vol_eq","package","flavorscent","fatcontent","typeofcut")]

#code into dummys ignore salt and cooking method

SSS$pr=as.numeric(SSS$pr)
SSS$f=ifelse(SSS$f =="NONE", 1, 0)
SSS$d=ifelse(SSS$d ==0, 1, 0)
SSS$package=ifelse(SSS$package =="BAG", 1, 0)
SSS$flavorscent=ifelse(SSS$flavorscent =="ORIGINAL", 1, 0)
SSS$fatcontent=ifelse(SSS$fatcontent =="REGULAR", 1, 0)
SSS$typeofcut=ifelse(SSS$typeofcut =="FLAT", 1, 0)

#non price characteristics

characteristics=c("pr","f","d","vol_eq","package","flavorscent","fatcontent","typeofcut")

#3 instruments per observed characteristic

m=length(characteristics)
n=m*3

IV=paste0("IV",1:n)
IV_mat=as.data.frame(matrix(0, nrow = dim(SSS)[1], ncol = n))
colnames(IV_mat)=IV

SSS=cbind(SSS,IV_mat)

#sum over all characteristics of same firm + rival firms

SSS$market=paste0(SSS$iri_key,SSS$week)

#IV charcteristic itself
for (j in 1:m) {characteristic=characteristics[j]  
SSS[,paste0("IV",1+(j-1)*3)]=SSS[,characteristic]}

for (j in 1:m) {characteristic=as.name(characteristics[j]) 
formel=as.formula(paste(characteristic,"~market+l4"))
formel2=as.formula(paste(characteristic,"~market"))

own=aggregate(formel,sum,data=SSS)
names(own)[3]=paste0(characteristic,"_own")
SSS=merge(SSS,own,by=c("market","l4"))

rival=aggregate(formel2,sum,data=SSS)
names(rival)[2]=paste0(characteristic,"_rival")
SSS=merge(SSS,rival,by=c("market"))

SSS[,paste0("IV",2+(j-1)*3)]=SSS[,paste0(characteristic,"_own")]-SSS[,paste0("IV",1+(j-1)*3)]
SSS[,paste0("IV",3+(j-1)*3)]=SSS[,paste0(characteristic,"_rival")]-SSS[,paste0(characteristic,"_own")]}


SSS0=subset(SSS,SSS$j==0)
SSS1=subset(SSS,SSS$j==1)
SSS2=subset(SSS,SSS$j==2)

#calculate average sales market/week for trainings set we need them to calculate marketsize including observations for val. and test
average_sales=aggregate(units~iri_key+week,mean,data=SSS1)
names(average_sales)[3]="units_hat"

#merge in the avg sales
SSS0=merge(SSS0,average_sales,by=c("iri_key","week"))
SSS2=merge(SSS2,average_sales,by=c("iri_key","week"))

#here we can use observed sales
SSS1$units_hat=SSS1$units

S=rbind(SSS0,SSS2,SSS1)

marketsize_pred=aggregate(units_hat~iri_key+week,sum,data=S)
names(marketsize_pred)[3]="marketsize_pred"

S=merge(S,marketsize_pred,by=c("iri_key","week"))

S$share=S$units_hat/S$marketsize_pred

S$productid=paste0("product",as.numeric(as.factor(S$colupc)))

write.table(S, file = "S_instruments")

#hier starten wenn S_instruments benutzt werden soll 1.1
S=read.table("S_instruments")

m=unique(S$market)

p=0.25 #% der Märkte die benutzt werden sollen

set.seed(1)
n <- runif(length(m),0,1)
mm=cbind(m,n)
mm=subset(mm,mm[,2]<p)

S=subset(S,S$market%in%mm[,1])

S1=subset(S,S$j==1)
S0=subset(S,S$j==0)
S2=subset(S,S$j==2)

#rm(SS,SS_BLP_2,SS0,SS1,SS2, SSS0,SSS1,SSS2,SSS,S)


nevos_model <- as.formula("share ~ logprice + pr + f + d + vol_eq + package + flavorscent + fatcontent + typeofcut |
    0|
    logprice + pr +vol_eq|
    0+ IV1 + IV2 + IV3 + IV4 + IV5 + IV6 + IV7 + IV8 + IV9 + IV10 + 
    IV11 + IV12 + IV13 + IV14 + IV15 + IV16 + IV17 + IV18 + IV19 + IV20+ IV21+ IV22+ IV23+ IV24")



nevo_data <- BLP_data(model = nevos_model,
                      market_identifier="market",
                      product_identifier = "productid",
                      productData = S1,
                      integration_method = "MLHS", 
                      integration_accuracy = 20, integration_seed = 212)


trainproduts=cbind(nevo_data$parameters$market_id_char_in,nevo_data$parameters$product_id)
colnames(trainproduts)=c("market","productid")
train_numb=aggregate(productid~market,data=trainproduts,length)
colnames(train_numb)[2]=c("train_numb")

#werden mehr als 10% der Märkte verwendet muss estimate BLP mit trace abgewandelt werden.
#omega <- diag( diag( xi_out %*% t(xi_out) ) )
#b<- t(Z) %*% omega %*% Z
#in Zeile 96/97 ersetzen

trace(estimateBLP,edit = TRUE)
untrace(estimateBLP)
start.time <- Sys.time()
blp_est2 <- estimateBLP( blp_data=nevo_data, printLevel =1 )
end.time <- Sys.time()
t2 <- end.time - start.time 
t2


theta=as.matrix(blp_est2$theta_rc)
colnames(theta)="unobs_sd"
rownames(theta) <- c("(Intercept)","logprice","pr","vol_eq")

delta_data <- data.frame( "productid" = nevo_data$parameters$product_id,
                          "market" = nevo_data$parameters$market_id_char_in,
                          "delta" = blp_est2$delta )

xi_data<- data.frame( "productid" = nevo_data$parameters$product_id,
                      "market" = nevo_data$parameters$market_id_char_in,
                      "xi" = blp_est2$xi )


nevo_data <- update_BLP_data(data_update = delta_data, 
                             blp_data = nevo_data)


#in sample
shareObj <- getShareInfo(blp_data=nevo_data, par_theta2 = theta, printLevel = 4)

p15=shareObj$shares*S1$marketsize_pred

#elastitzitäten
theta1_price <- blp_est2$theta_lin["logprice",]
elasttitzitat=get_elasticities(blp_data=nevo_data, 
                 share_info = shareObj, 
                 theta_lin = theta1_price,
                 variable = "logprice",
                 products = c("product7","product216","product30"),
                 market = "200171103")

colnames(elasttitzitat)=c("Jays Krunchers Original","Jays Original","Lays Classic")
rownames(elasttitzitat)=c("Jays Krunchers Original","Jays Original","Lays Classic")

print(xtable(elasttitzitat,caption = "Preiselastizitäten für 3 zufällig ausgewählte Produkte"))

#library for markets
lib=cbind(S$market,S$iri_key)
lib2=unique(lib)
colnames(lib2)=c("market","iri_key")
xi_data=merge(xi_data,lib2,by=c("market"))


#out of sample

#test data set
nevo_data_test <- BLP_data(model = nevos_model,
                           market_identifier="market",
                           product_identifier = "productid",
                           productData = S0,
                           integration_method = "MLHS", 
                           integration_accuracy = 20, integration_seed = 212)


testproducts=cbind(nevo_data_test$parameters$market_id_char_in,nevo_data_test$parameters$product_id)
colnames(testproducts)=c("market","productid")
test_numb=aggregate(productid~market,data=testproducts,length)
colnames(test_numb)[2]=c("test_numb")


delta_data2 <- data.frame( "productid" = nevo_data_test$parameters$product_id,
                           "market" = nevo_data_test$parameters$market_id_char_in,
                           "delta" = nevo_data_test$data$delta )
delta_data2=merge(delta_data2,lib2,by=c("market"))

delta_data2$lin=nevo_data_test$data$X_lin%*%blp_est2$theta_lin





total <- aggregate(xi~productid+iri_key,data=xi_data,mean,na.rm=T)
names(total)[3]="xi2"

delta_data2=merge(delta_data2,total,by=c("productid","iri_key"))
delta_data2$delta=delta_data2$lin+delta_data2$xi2
delta_data2=delta_data2[,c(-2,-5,-6)]


nevo_data_test <- update_BLP_data(data_update = delta_data2, 
                                  blp_data = nevo_data_test)

shareObj <- getShareInfo(blp_data=nevo_data_test,par_theta2 = theta, printLevel = 4)

test_shares=cbind(shareObj$shares,nevo_data_test$parameters$market_id_char_in)
colnames(test_shares)=c("share","market")
test_shares=merge(test_shares,train_numb,by=c("market"))
test_shares=merge(test_shares,test_numb,by=c("market"))
test_shares$newshares=as.numeric(as.character(test_shares$share))*(as.numeric(test_shares$test_numb)/as.numeric(test_shares$train_numb))

p15t=test_shares$newshares*S0$marketsize_pred

p15t=shareObj$shares*S0$marketsize_pred*(dim(S0)[1]/dim(S1)[1])

p15t=shareObj$shares*S0$marketsize_pred

#validation data set
nevo_data_validation <- BLP_data(model = nevos_model,
                                 market_identifier="market",
                                 product_identifier = "productid",
                                 productData = S2,
                                 integration_method = "MLHS", 
                                 integration_accuracy = 20, integration_seed = 212)

valproducts=cbind(nevo_data_validation$parameters$market_id_char_in,nevo_data_validation$parameters$product_id)
colnames(valproducts)=c("market","productid")
val_numb=aggregate(productid~market,data=valproducts,length)
colnames(val_numb)[2]=c("val_numb")





delta_data3 <- data.frame( "productid" = nevo_data_validation$parameters$product_id,
                           "market" = nevo_data_validation$parameters$market_id_char_in,
                           "delta" = nevo_data_validation$data$delta )

delta_data3=merge(delta_data3,lib2,by=c("market"))
delta_data3$lin=nevo_data_validation$data$X_lin%*%blp_est2$theta_lin


delta_data3=merge(delta_data3,total,by=c("productid","iri_key"))
delta_data3$delta=delta_data3$lin+delta_data3$xi2
delta_data3=delta_data3[,c(-2,-5,-6)]


nevo_data_validation <- update_BLP_data(data_update = delta_data3, 
                                        blp_data = nevo_data_validation)

shareObj <- getShareInfo(blp_data=nevo_data_validation,par_theta2 = theta, printLevel = 4)

val_shares=cbind(shareObj$shares,nevo_data_validation$parameters$market_id_char_in)
colnames(val_shares)=c("share","market")
val_shares=merge(val_shares,train_numb,by=c("market"))
val_shares=merge(val_shares,val_numb,by=c("market"))
val_shares$newshares=as.numeric(as.character(val_shares$share))*(as.numeric(val_shares$val_numb)/as.numeric(val_shares$train_numb))

p15v=val_shares$newshares*S2$marketsize_pred

p15v=shareObj$shares*S2$marketsize_pred*(dim(S2)[1]/dim(S1)[1])

p15v=shareObj$shares*S2$marketsize_pred

rmse(log(p15),log(S1$units))
rmse(log(p15t),log(S0$units)) 
rmse(log(p15v),log(S2$units))

Resultate_Mixed_0.25_nuber=cbind(Metohde="Mixed-Logit Anzahl",rmse_test=round(rmse(log(p15t),log(S0$units)),3)
                           ,smape_test=round(sMAPE(log(p15t),log(S0$units)),3),rmse_val=round(rmse(log(p15v),log(S2$units)),3)
                           ,smape_val=round(sMAPE(log(p15v),log(S2$units)),3))


Resultate_Mixed_0.25_dim=cbind(Metohde="Mixed-Logit dim",rmse_test=round(rmse(log(p15t),log(S0$units)),3)
                                 ,smape_test=round(sMAPE(log(p15t),log(S0$units)),3),rmse_val=round(rmse(log(p15v),log(S2$units)),3)
                                 ,smape_val=round(sMAPE(log(p15v),log(S2$units)),3))

Resultate_Mixed_0.25=cbind(Metohde="Mixed-Logit",rmse_test=round(rmse(log(p15t),log(S0$units)),3)
                               ,smape_test=round(sMAPE(log(p15t),log(S0$units)),3),rmse_val=round(rmse(log(p15v),log(S2$units)),3)
                               ,smape_val=round(sMAPE(log(p15v),log(S2$units)),3))

#comparison with (logit in Sample)
share_formula <- formula(share ~ logprice + pr + f + d + vol_eq + package + flavorscent + fatcontent + typeofcut-1)

f9 <- glm(share_formula,data=S1,family=binomial)

p9share <- predict(f9,newdata=S1,type="response")
p9 <- log(p9share*S1$marketsize_pred)

p9tshare <- predict(f9,newdata=S2,type="response")
p9t <- log(p9tshare*S2$marketsize_pred)

p9vshare <- predict(f9,newdata=S0,type="response")
p9v <- log(p9vshare*S0$marketsize_pred)

Resultate_Logit_0.25=cbind(Metohde="Logit",rmse_in=round(rmse(p9,log(S1$units)),3),smape_in=round(sMAPE(p9,log(S1$units)),3),rmse_test=round(rmse(p9t,log(S2$units)),3)
                           ,smape_test=round(sMAPE(p9t,log(S2$units)),3),rmse_val=round(rmse(p9v,log(S0$units)),3)
                           ,smape_val=round(sMAPE(p9v,log(S0$units)),3))


#now prediction for complete training and test

S=read.table("S_instruments")

S0=subset(S,S$j==0)
S2=subset(S,S$j==2)
S1=subset(S,S$j==1)


lib=cbind(S$market,S$iri_key)
lib2=unique(lib)
colnames(lib2)=c("market","iri_key")

nevo_data_test <- BLP_data(model = nevos_model,
                           market_identifier="market",
                           product_identifier = "productid",
                           productData = S0,
                           integration_method = "MLHS", 
                           integration_accuracy = 20, integration_seed = 212)



delta_data2 <- data.frame( "productid" = nevo_data_test$parameters$product_id,
                           "market" = nevo_data_test$parameters$market_id_char_in,
                           "delta" = nevo_data_test$data$delta )

delta_data2=merge(delta_data2,lib2,by=c("market"))
delta_data2$lin=nevo_data_test$data$X_lin%*%blp_est2$theta_lin
delta_data2=merge(delta_data2,total,by=c("productid","iri_key"))
delta_data2$delta=delta_data2$lin+delta_data2$xi2
delta_data2=delta_data2[,c(-2,-5,-6)]

nevo_data_test <- update_BLP_data(data_update = delta_data2, 
                                  blp_data = nevo_data_test)

shareObj <- getShareInfo(blp_data=nevo_data_test,par_theta2 = theta, printLevel = 4)
p15t=shareObj$shares*S0$marketsize_pred*(dim(S0)[1]/dim(S1)[1])

#p15t=shareObj$shares*S0$marketsize_pred

nevo_data_validation <- BLP_data(model = nevos_model,
                                 market_identifier="market",
                                 product_identifier = "productid",
                                 productData = S2,
                                 integration_method = "MLHS", 
                                 integration_accuracy = 20, integration_seed = 212)


delta_data3 <- data.frame( "productid" = nevo_data_validation$parameters$product_id,
                           "market" = nevo_data_validation$parameters$market_id_char_in,
                           "delta" = nevo_data_validation$data$delta )


delta_data3=merge(delta_data3,lib2,by=c("market"))
delta_data3$lin=nevo_data_validation$data$X_lin%*%blp_est2$theta_lin
delta_data3=merge(delta_data3,total,by=c("productid","iri_key"))
delta_data3$delta=delta_data3$lin+delta_data3$xi2
delta_data3=delta_data3[,c(-2,-5,-6)]


nevo_data_validation <- update_BLP_data(data_update = delta_data3, 
                                        blp_data = nevo_data_validation)


shareObj <- getShareInfo(blp_data=nevo_data_validation,par_theta2 = theta, printLevel = 4)
p15v=shareObj$shares*S2$marketsize_pred*(dim(S2)[1]/dim(S1)[1])
#p15v=shareObj$shares*S2$marketsize_pred

p16t=read.table("p16t.csv")
p16=read.table("p16.csv")

p16t=cbind(p16t,S0$week,S0$iri_key,S0$colupc)
p16=cbind(p16,S2$week,S2$iri_key,S2$colupc)

write.table(p16t,"p16t.csv")
write.table(p16,"p16.csv")



Resultate_Mixed_1_dim=cbind(Metohde="Mixed-Logit 1 dim",rmse_test=round(rmse(log(p15t),log(S0$units)),3)
                                        ,smape_test=round(sMAPE(log(p15t),log(S0$units)),3),rmse_val=round(rmse(log(p15v),log(S2$units)),3)
                                        ,smape_val=round(sMAPE(log(p15v),log(S2$units)),3))

Resultate_Mixed_1=cbind(Metohde="Mixed-Logit 1",rmse_test=round(rmse(log(p15t),log(S0$units)),3)
                            ,smape_test=round(sMAPE(log(p15t),log(S0$units)),3),rmse_val=round(rmse(log(p15v),log(S2$units)),3)
                            ,smape_val=round(sMAPE(log(p15v),log(S2$units)),3))





#export stuff
#25% results

Results_0_25=rbind(Resultate_Mixed_0.25,Resultate_Mixed_0.25_dim,Resultate_Mixed_0.25_nuber)
write.table(Results_0_25,"Results_0_25.csv")
Results_0_25=read.table("Results_0_25.csv")

#100% results

Results_1=rbind(Resultate_Mixed_1_dim,Resultate_Mixed_1)
write.table(Results_1,"Results_1.csv")

#combined
Results_both=rbind(Results_0_25,Results_1)

print(xtable(Results_both,caption = "Resultat des Mixed-Logit Modells" ),include.rownames=FALSE)

#save(blp_est2,file="blp2.RData")
load("blp2.RData")

blp2_summary <- summary(blp_est2)
capture.output(blp2_summary, file = "myfile.txt")

lin_coef=blp2_summary$LinCoefficients
rc_coef=blp2_summary$RcCoefficients

Coefficents=rbind(lin_coef,c(0,0,0,0),rc_coef)
print(xtable(Coefficents,caption = "Koeffizienten des Mixed-Logit Modells",digits=c(0,2,2,2,3) ))


write.table(lin_coef,"lin_coef.csv")
write.table(rc_coef,"rc_coef.csv")



