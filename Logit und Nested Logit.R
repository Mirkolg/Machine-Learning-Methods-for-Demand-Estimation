
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

rm(SS,SS1,SS2,x_mat,x,valx,testx,SS0)
gc()

x_formula <- formula(~(logprice+pr+f+d+brand+vol_eq+producttype+package+flavorscent+
                         fatcontent+cookingmethod+saltsodiumcontent+typeofcut))

x_mat <- model.matrix(x_formula,data=S)


testx <- x_mat[S$j==0,]
x <- x_mat[S$j==1,]
valx <- x_mat[S$j==2,]

testy <- log(S$share4[S$j==0])
y <- log(S$share4[S$j==1])
valy <- log(S$share4[S$j==2])


f1 <- lm(y~x-1)

f1.coef <- as.numeric(f1$coefficients)
f1.coef[is.na(f1.coef)] <- 0 

p <- as.numeric(f1.coef %*% t(x))
p1 <- as.numeric(f1.coef %*% t(valx))
p2 <- as.numeric(f1.coef %*% t(testx))

p=as.data.frame(cbind(p,S$iri_key[S$j==1],S$week[S$j==1]))
p1=as.data.frame(cbind(p1,S$iri_key[S$j==2],S$week[S$j==2]))
p2=as.data.frame(cbind(p2,S$iri_key[S$j==0],S$week[S$j==0]))

p$sort_id=seq(1,dim(p)[1])
p1$sort_id=seq(1,dim(p1)[1])
p2$sort_id=seq(1,dim(p2)[1])

colnames(p)[1:3]=c("p","iri_key","week")
colnames(p1)[1:3]=c("p","iri_key","week")
colnames(p2)[1:3]=c("p","iri_key","week")

P=rbind(p,p1,p2)
P=as.data.frame(P)
colnames(P)=c("delta","iri_key","week")

P$delta=exp(P$delta)
delta=aggregate(delta~iri_key+week,P,sum)

p1=merge(p1,delta,by=c("iri_key","week"))
p2=merge(p2,delta,by=c("iri_key","week"))

p1=p1[order(p1$sort_id),]
p2=p2[order(p2$sort_id),]

p1$sj=exp(p1$p)/p1$delta
p2$sj=exp(p2$p)/p2$delta

p1$totalunits=S$total_units4[S$j==2]
p2$totalunits=S$total_units4[S$j==0]

p1$pred=p1$sj*p1$totalunits
p2$pred=p2$sj*p2$totalunits

p9_5=log(p1$pred)
p9t_5=log(p2$pred)

#Nested Logit ################################################################################################################

#Nests are taste original , ONION BBQ and everthing else

S$taste=ifelse(S$flavorscent =="ORIGINAL", 1, ifelse(S$flavorscent =="ONION", 2, ifelse(S$flavorscent =="BBQ", 3, 0)))

nestshare=aggregate(share4~iri_key+week+taste,sum,data=S)
colnames(nestshare)[4]="nestshare"
S=merge(S,nestshare,by=c("iri_key","week","taste"))
S=S[order(S$sort_id),]
S$withinnestshare=log(S$share4/S$nestshare)

#instrumental variable

nestsize=aggregate(logprice~iri_key+week+taste,length,data=S)
colnames(nestsize)[4]="nestsize"
S=merge(S,nestsize,by=c("iri_key","week","taste"))
S=S[order(S$sort_id),]

fm <- ivreg(log(share4)~logprice+pr+f+d+brand+vol_eq+producttype+package+flavorscent+fatcontent+cookingmethod+saltsodiumcontent+typeofcut+withinnestshare-1| 
              logprice+pr+f+d+brand+vol_eq+producttype+package+flavorscent+fatcontent+cookingmethod+saltsodiumcontent+typeofcut+nestsize ,data=S[S$j==1,])



x_formula <- formula(~(logprice+pr+f+d+brand+vol_eq+producttype+package+flavorscent+ fatcontent+cookingmethod+saltsodiumcontent+typeofcut+withinnestshare-1))

x_mat <- model.matrix(x_formula,data=S)
dim(x_mat)
colnames(x_mat)

testx <- x_mat[S$j==0,]
x <- x_mat[S$j==1,]
valx <- x_mat[S$j==2,]

fm.coef <- as.numeric(fm$coefficients)
fm.coef[is.na(fm.coef)] <- 0 

p <- as.numeric(fm.coef %*% t(x))
p1 <- as.numeric(fm.coef %*% t(valx))
p2 <- as.numeric(fm.coef %*% t(testx))


p=as.data.frame(cbind(p,S$iri_key[S$j==1],S$week[S$j==1]))
p1=as.data.frame(cbind(p1,S$iri_key[S$j==2],S$week[S$j==2]))
p2=as.data.frame(cbind(p2,S$iri_key[S$j==0],S$week[S$j==0]))

p$sort_id=seq(1,dim(p)[1])
p1$sort_id=seq(1,dim(p1)[1])
p2$sort_id=seq(1,dim(p2)[1])

colnames(p)[1:3]=c("p","iri_key","week")
colnames(p1)[1:3]=c("p","iri_key","week")
colnames(p2)[1:3]=c("p","iri_key","week")

P=rbind(p,p1,p2)
P=as.data.frame(P)
colnames(P)=c("delta","iri_key","week")

P$delta=exp(P$delta)
delta=aggregate(delta~iri_key+week,P,sum)

p1=merge(p1,delta,by=c("iri_key","week"))
p2=merge(p2,delta,by=c("iri_key","week"))

p1=p1[order(p1$sort_id),]
p2=p2[order(p2$sort_id),]

p1$sj=exp(p1$p)/p1$delta
p2$sj=exp(p2$p)/p2$delta

p1$totalunits=S$total_units4[S$j==2]
p2$totalunits=S$total_units4[S$j==0]

p1$pred=p1$sj*p1$totalunits
p2$pred=p2$sj*p2$totalunits

rmse(log(p1$pred),S$logunits[S$j==2])

p10=log(p1$pred)
p10t=log(p2$pred)


#export
write.table(p9_5,"p9_5.csv")
write.table(p9t_5,"p9t_5.csv")

write.table(p10,"p10.csv")
write.table(p10t,"p10t.csv")


f1_sum=summary(f1)$coef[drop=F]
f1_sum_sig=subset(f1_sum,f1_sum[,4]<=0.05)

xtable(f1_sum_sig,caption = "Signifikante Koeffizienten der Logit mit OLS (p-Werte gerundet auf 3 Nachkommastellen)",label = "tab:LogitOLS",digits=c(0,3,3,3,3))





