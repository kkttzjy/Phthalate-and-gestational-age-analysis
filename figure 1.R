rawdata <-readRDS("rawdata.rds")
## transform response varibale and exposure variable
# calculate mean
sg_mehp<-log(rawdata[,64:66])
sg_mehhp<-log(rawdata[,68:70])
sg_meohp<-log(rawdata[,72:74])
sg_mecpp<-log(rawdata[,76:78])
sg_dehpsum<-log(rawdata[,80:82])
sg_mbzp<-log(rawdata[,84:86])
sg_mbp<-log(rawdata[,88:90])
sg_mibp<-log(rawdata[,92:94])
sg_mep<-log(rawdata[,96:98])
sg_mcpp<-log(rawdata[,100:102])
sg_mean_mehp<-rowMeans(sg_mehp,na.rm=TRUE)
sg_mean_mehhp<-rowMeans(sg_mehhp,na.rm=TRUE)
sg_mean_meohp<-rowMeans(sg_meohp,na.rm=TRUE)
sg_mean_mecpp<-rowMeans(sg_mecpp,na.rm=TRUE)
sg_mean_dehpsum<-rowMeans(sg_dehpsum,na.rm=TRUE)
sg_mean_mbzp<-rowMeans(sg_mbzp,na.rm=TRUE)
sg_mean_mbp<-rowMeans(sg_mbp,na.rm=TRUE)
sg_mean_mibp<-rowMeans(sg_mibp,na.rm=TRUE)
sg_mean_mep<-rowMeans(sg_mep,na.rm=TRUE)
sg_mean_mcpp<-rowMeans(sg_mcpp,na.rm=TRUE)
rawdata$sg_mean_mehp<-sg_mean_mehp
rawdata$sg_mean_mehhp<-sg_mean_mehhp
rawdata$sg_mean_meohp<-sg_mean_meohp
rawdata$sg_mean_mecpp<-sg_mean_mecpp
rawdata$sg_mean_dehpsum<-sg_mean_dehpsum
rawdata$sg_mean_mbzp<-sg_mean_mbzp
rawdata$sg_mean_mbp<-sg_mean_mbp
rawdata$sg_mean_mibp<-sg_mean_mibp
rawdata$sg_mean_mep<-sg_mean_mep
rawdata$sg_mean_mcpp<-sg_mean_mcpp
rawdata[,7:46]<-log(rawdata[,7:46])
mehp<-rawdata[,7:9]
mehhp<-rawdata[,11:13]
meohp<-rawdata[,15:17]
mecpp<-rawdata[,19:21]
dehpsum<-rawdata[,23:25]
mbzp<-rawdata[,27:29]
mbp<-rawdata[,31:33]
mibp<-rawdata[,35:37]
mep<-rawdata[,39:41]
mcpp<-rawdata[,43:45]
mean_mehp<-rowMeans(mehp,na.rm=TRUE)
mean_mehhp<-rowMeans(mehhp,na.rm=TRUE)
mean_meohp<-rowMeans(meohp,na.rm=TRUE)
mean_mecpp<-rowMeans(mecpp,na.rm=TRUE)
mean_dehpsum<-rowMeans(dehpsum,na.rm=TRUE)
mean_mbzp<-rowMeans(mbzp,na.rm=TRUE)
mean_mbp<-rowMeans(mbp,na.rm=TRUE)
mean_mibp<-rowMeans(mibp,na.rm=TRUE)
mean_mep<-rowMeans(mep,na.rm=TRUE)
mean_mcpp<-rowMeans(mcpp,na.rm=TRUE)
rawdata$mean_mehp<-mean_mehp
rawdata$mean_mehhp<-mean_mehhp
rawdata$mean_meohp<-mean_meohp
rawdata$mean_mecpp<-mean_mecpp
rawdata$mean_dehpsum<-mean_dehpsum
rawdata$mean_mbzp<-mean_mbzp
rawdata$mean_mbp<-mean_mbp
rawdata$mean_mibp<-mean_mibp
rawdata$mean_mep<-mean_mep
rawdata$mean_mcpp<-mean_mcpp
sg<-rawdata[,47:49]
rawdata$averagesg<-rowMeans(sg,na.rm=TRUE)
rawdata$race_cat_new <- as.factor(rawdata$race_cat_new)
rawdata$edu_cat_new <- as.factor(rawdata$edu_cat_new)
## transform response varibale and exposure variable
continousdata<-rawdata
continousdata$final_ga<-(rawdata$final_ga)*7
continousdata$log_final_ga<-log(continousdata$final_ga)
continousdata$birth<-1

case<-subset(continousdata, preterm==1)
control<-subset(continousdata, preterm==0)
# data split
set.seed(1234)
library(caret)
# case
## 40% of the sample size
smp_size <- floor((0.4) * nrow(case))
## set the seed to make your partition reproductible
set.seed(123)
test_ind <- sample(seq_len(nrow(case)), size = smp_size)
testcase <- case[test_ind, ]
validationcase <- case[-test_ind, ]
# control
## 40% of the sample size
smp_size <- floor((0.4) * nrow(control))
## set the seed to make your partition reproductible
set.seed(1234)
test_ind <- sample(seq_len(nrow(control)), size = smp_size)
testcontrol <- control[test_ind, ]
validationcontrol <- control[-test_ind, ]

test <- rbind(testcase,testcontrol)
validation <- rbind(validationcase,validationcontrol)

### logsitic regression
## WQS-Corr
library(gWQS)
test$group = 0
validation$group = 1
datawqs<-as.data.frame(rbind(test, validation))
phthalateexposure = c( "sg_mean_mecpp", "sg_mean_mbzp", "sg_mean_mbp",
                       "sg_mean_mibp", "sg_mean_mep", "sg_mean_mcpp")
wqsdata<-datawqs[, c(108,110:114,51,52,53,54,3,128)]

wqsdatacomplete<-na.omit(wqsdata)

results1 = gwqs(preterm ~ age+race_cat_new+edu_cat_new+insur_new, mix_name = phthalateexposure, data = wqsdatacomplete,
                q = 4, validation = 0, valid_var = "group",
                b = 1000, b1_pos = T, family = "binomial", seed = 2016, wqs2 = FALSE, plots = TRUE,
                tables = TRUE)

complete<-na.omit(validation[,c(108,110:114,51,52,53,54,3)])
phthalate<-complete[, 1:6]
phthalate = quantile_f(phthalate, phthalateexposure, 4)
q_name = paste(phthalateexposure, "q", sep = "_")
meanweight<-results1$final_weights
mweight<-meanweight$mean_weight
qphthalate<-as.matrix(phthalate[,7:12])
wqs_final1<-qphthalate%*%mweight
quantile(wqs_final1)

covariates<-validation[,c(51,52,53,54,3)]
covariates<-na.omit(covariates)
newdata <- cbind(wqs_final1, covariates)
newdata <- na.omit(newdata)
for (i in 1 : 278 ) {
  if (newdata$wqs_final1[i] < 0.9554456){
    newdata$wqs[i] = 0
  }
  else if (newdata$wqs_final1[i] > 0.9554456 & newdata$wqs_final1[i] < 1.4728226 ){
    newdata$wqs[i] = 1
  }
  else if (newdata$wqs_final1[i] > 1.4728226 & newdata$wqs_final1[i] < 2.1599789 ){
    newdata$wqs[i] = 2
  }
  else {newdata$wqs[i] = 3}
}
newdata$wqs <- as.factor(newdata$wqs)
newdata$wqs <- relevel(newdata$wqs, ref = "0")
require(nnet)
mylogit <- multinom(preterm ~ wqs+age+race_cat_new+edu_cat_new+insur_new, data = newdata,ref = "0")
or <- exp(coef(mylogit))
CI <- exp(confint(mylogit))
CIlower <-CI[,1]
CIupper <- CI[,2]
ORplot <- cbind(or,CIlower, CIupper)
ORplot <- ORplot[c(2,3,4),]
cat <- c(1,2,3)
ORplot <- cbind(ORplot, cat)
or00 <- t(c(1,1,1,0))
ORplot <- rbind(or00, ORplot)
new1 <- as.data.frame(ORplot)
new1$cat <- as.factor(new1$cat)


## WQS-Stepwise
phthalateexposure = c( "sg_mean_mehp", "sg_mean_mehhp", "sg_mean_meohp",
                       "sg_mean_mecpp", "sg_mean_mbp", "sg_mean_mibp")
wqsdata<-datawqs[, c(105,106,107,108,111,112,51,52,53,54,3,128)]

wqsdatacomplete<-na.omit(wqsdata)

results2 = gwqs(preterm ~ age+race_cat_new+edu_cat_new+insur_new, mix_name = phthalateexposure, data = wqsdatacomplete,
                q = 4, validation = 0, valid_var = "group",
                b = 1000, b1_pos = T, family = "binomial", seed = 2016, wqs2 = FALSE, plots = TRUE,
                tables = TRUE)
complete<-na.omit(validation[,c(105,106,107,108,111,112,51,52,53,54,3)])
phthalate<-complete[, 1:6]
phthalate = quantile_f(phthalate, phthalateexposure, 4)
q_name = paste(phthalateexposure, "q", sep = "_")
meanweight<-results2$final_weights
mweight<-meanweight$mean_weight
qphthalate<-as.matrix(phthalate[,7:12])
wqs_final2<-qphthalate%*%mweight
quantile(wqs_final2)

# OR
covariates<-validation[,c(51,52,53,54,3)]
covariates<-na.omit(covariates)
newdata <- cbind(wqs_final2, covariates)
newdata <- na.omit(newdata)
for (i in 1 : 278 ) {
  if (newdata$wqs_final2[i] < 0.617758){
    newdata$wqs[i] = 0
  }
  else if (newdata$wqs_final2[i] > 0.617758 & newdata$wqs_final2[i] < 1.494012 ){
    newdata$wqs[i] = 1
  }
  else if (newdata$wqs_final2[i] > 1.494012 & newdata$wqs_final2[i] < 2.365786 ){
    newdata$wqs[i] = 2
  }
  else {newdata$wqs[i] = 3}
}
newdata$wqs <- as.factor(newdata$wqs)
newdata$wqs <- relevel(newdata$wqs, ref = "0")
require(nnet)
mylogit <- multinom(preterm ~ wqs+age+race_cat_new+edu_cat_new+insur_new, data = newdata,ref = "0")
or <- exp(coef(mylogit))
CI <- exp(confint(mylogit))
CIlower <-CI[,1]
CIupper <- CI[,2]
ORplot <- cbind(or,CIlower, CIupper)
ORplot <- ORplot[c(2,3,4),]
cat <- c(1,2,3)
ORplot <- cbind(ORplot, cat)
or00 <- t(c(1,1,1,0))
ORplot <- rbind(or00, ORplot)
new2 <- as.data.frame(ORplot)
new2$cat <- as.factor(new2$cat)


# ERS-Corr
betam<-c(3.58E-01,
         1.81E-02,
         2.63E-01,
         -2.68E-01,
         7.22E-02,
         -6.69E-02)
exposurem<-rawdata[, c(118,120,121,122,123,124)]
exposurem<-data.matrix(exposurem)
ERS2m<-as.data.frame(exposurem%*%betam)
ERS2m$preterm<-continousdata$preterm
ERS_control<-ERS2m[ERS2m$preterm==0,]
ERScomplete<-na.omit(ERS_control$V1)
mean(ERScomplete)
range(ERScomplete)
sd(ERScomplete)
quantile(ERScomplete)


library(outliers)
chisq.out.test(ERScomplete,variance = var(ERScomplete),opposite = TRUE)
chisq.out.test(ERScomplete,variance = var(ERScomplete),opposite = FALSE)

remove<-c(4.1504775197978)
ERScomplete<-ERScomplete[! ERScomplete %in% remove]
quantile(ERScomplete)

# OR
newdata <- cbind2(ERS2m, rawdata)
newdata <- newdata[,c(1,2,53,54,55,56,127)]
newdata <- newdata[order(newdata$V1),]
newdata <- newdata[ which(newdata$V1!=4.1504775197978),]
newdata <- na.omit(newdata)
for (i in 1 : 462 ) {
  if (newdata$V1[i] < 1.5212268){
    newdata$ERS[i] = 0
  }
  else if (newdata$V1[i] > 1.5212268 & newdata$V1[i] < 1.8049002 ){
    newdata$ERS[i] = 1
  }
  else if (newdata$V1[i] > 1.8049002 & newdata$V1[i] < 2.1273780 ){
    newdata$ERS[i] = 2
  }
  else {newdata$ERS[i] = 3}
}
newdata$ERS <- as.factor(newdata$ERS)
newdata$ERS <- relevel(newdata$ERS, ref = "0")
require(nnet)
mylogit <- multinom(preterm ~ ERS+averagesg+age+race_cat_new+edu_cat_new+insur_new, data = newdata,ref = "0")
or <- exp(coef(mylogit))
CI <- exp(confint(mylogit))
CIlower <-CI[,1]
CIupper <- CI[,2]
ORplot <- cbind(or,CIlower, CIupper)
ORplot <- ORplot[c(2,3,4),]
cat <- c(1,2,3)
ORplot <- cbind(ORplot, cat)
or00 <- t(c(1,1,1,0))
ORplot <- rbind(or00, ORplot)
new3 <- as.data.frame(ORplot)
new3$cat <- as.factor(new3$cat)



## ERS-Stepwise
betam<-c(0.424,
         0.761,
         -2.289,
         1.357,
         0.317,
         -0.305)
exposurem<-rawdata[, c(115,116,117,118,121,122)]
exposurem<-data.matrix(exposurem)
ERS2m<-as.data.frame(exposurem%*%betam)
ERS2m$preterm<-continousdata$preterm
ERS_control<-ERS2m[ERS2m$preterm==0,]
ERScomplete<-na.omit(ERS_control$V1)
mean(ERScomplete)
range(ERScomplete)
sd(ERScomplete)
quantile(ERScomplete)

library(outliers)
chisq.out.test(ERScomplete,variance = var(ERScomplete),opposite = TRUE)
chisq.out.test(ERScomplete,variance = var(ERScomplete),opposite = FALSE)
remove<-c(4.1504775197978)
ERScomplete<-ERScomplete[! ERScomplete %in% remove]
quantile(ERScomplete)

# OR
newdata <- cbind2(ERS2m, rawdata)
newdata <- newdata[,c(1,2,53,54,55,56,127)]
newdata <- newdata[order(newdata$V1),]
newdata <- newdata[ which(newdata$V1!= -0.122082151373762),]
newdata <- na.omit(newdata)
for (i in 1 : 462 ) {
  if (newdata$V1[i] < 2.0635147){
    newdata$ERS[i] = 0
  }
  else if (newdata$V1[i] > 2.0635147 & newdata$V1[i] < 2.3825464 ){
    newdata$ERS[i] = 1
  }
  else if (newdata$V1[i] > 2.3825464 & newdata$V1[i] < 2.7491998 ){
    newdata$ERS[i] = 2
  }
  else {newdata$ERS[i] = 3}
}
newdata$ERS <- as.factor(newdata$ERS)
newdata$ERS <- relevel(newdata$ERS, ref = "0")
require(nnet)
mylogit <- multinom(preterm ~ ERS+averagesg+age+race_cat_new+edu_cat_new+insur_new, data = newdata,ref = "0")
or <- exp(coef(mylogit))
CI <- exp(confint(mylogit))
CIlower <-CI[,1]
CIupper <- CI[,2]
ORplot <- cbind(or,CIlower, CIupper)
ORplot <- ORplot[c(2,3,4),]
cat <- c(1,2,3)
ORplot <- cbind(ORplot, cat)
or00 <- t(c(1,1,1,0))
ORplot <- rbind(or00, ORplot)
new4 <- as.data.frame(ORplot)
new4$cat <- as.factor(new4$cat)


# MECPP
quantile(rawdata$mean_mecpp)
# OR
newdata <- rawdata[,c(1,3,51,52,53,54,118,125)]
for (i in 1 : 482 ) {
  if (newdata$mean_mecpp[i] < 2.9191){
    newdata$mecpp[i] = 0
  }
  else if (newdata$mean_mecpp[i] > 2.9191 & newdata$mean_mecpp[i] < 3.6010 ){
    newdata$mecpp[i] = 1
  }
  else if (newdata$mean_mecpp[i] > 3.6010 & newdata$mean_mecpp[i] < 4.4152 ){
    newdata$mecpp[i] = 2
  }
  else {newdata$mecpp[i] = 3}
}
newdata$mecpp <- as.factor(newdata$mecpp)
newdata$mecpp <- relevel(newdata$mecpp, ref = "0")
require(nnet)
mylogit <- multinom(preterm ~ mecpp+averagesg+age+race_cat_new+edu_cat_new, data = newdata,ref = "0")
or <- exp(coef(mylogit))
CI <- exp(confint(mylogit))
CIlower <-CI[,1]
CIupper <- CI[,2]
ORplot <- cbind(or,CIlower, CIupper)
ORplot <- ORplot[c(2,3,4),]
cat <- c(1,2,3)
ORplot <- cbind(ORplot, cat)
or00 <- t(c(1,1,1,0))
ORplot <- rbind(or00, ORplot)
new5 <- as.data.frame(ORplot)
new5$cat <- as.factor(new5$cat)

# MBP
quantile(rawdata$mean_mbp)
newdata <- rawdata[,c(1,3,51,52,53,54,121,125)]
for (i in 1 : 482 ) {
  if (newdata$mean_mbp[i] < 2.0945){
    newdata$mbp[i] = 0
  }
  else if (newdata$mean_mbp[i] > 2.0945 & newdata$mean_mbp[i] < 2.7380 ){
    newdata$mbp[i] = 1
  }
  else if (newdata$mean_mbp[i] > 2.7380 & newdata$mean_mbp[i] < 3.3446 ){
    newdata$mbp[i] = 2
  }
  else {newdata$mbp[i] = 3}
}
newdata$mbp <- as.factor(newdata$mbp)
newdata$mbp <- relevel(newdata$mbp, ref = "0")
require(nnet)
mylogit <- multinom(preterm ~ mbp+averagesg+age+race_cat_new+edu_cat_new+insur_new, data = newdata,ref = "0")
or <- exp(coef(mylogit))
CI <- exp(confint(mylogit))
CIlower <-CI[,1]
CIupper <- CI[,2]
ORplot <- cbind(or,CIlower, CIupper)
ORplot <- ORplot[c(2,3,4),]
cat <- c(1,2,3)
ORplot <- cbind(ORplot, cat)
or00 <- t(c(1,1,1,0))
ORplot <- rbind(or00, ORplot)
new6 <- as.data.frame(ORplot)
new6$cat <- as.factor(new6$cat)


### AFT model
## WQS-Corr
covariates<-validation[,c(51,52,53,54)]
log_final_ga<-log(validation$final_ga)
weights<-validation$weight
AFTdata<-cbind(log_final_ga, covariates, weights)
AFTdata<-na.omit(AFTdata)
AFTdata<-as.data.frame(cbind(AFTdata, wqs_final1))
quantile(wqs_final1)

newdata <- AFTdata
for (i in 1 : 278 ) {
  if (newdata$wqs_final1[i] < 0.9554456){
    newdata$wqs[i] = 0
  }
  else if (newdata$wqs_final1[i] > 0.9554456 & newdata$wqs_final1[i] < 1.4728226 ){
    newdata$wqs[i] = 1
  }
  else if (newdata$wqs_final1[i] > 1.4728226 & newdata$wqs_final1[i] < 2.2224104 ){
    newdata$wqs[i] = 2
  }
  else {newdata$wqs[i] = 3}
}
newdata$wqs <- as.factor(newdata$wqs)
newdata$wqs <- relevel(newdata$wqs, ref = "0")
myloggt <- lm(log_final_ga ~ wqs+age+race_cat_new+edu_cat_new+insur_new, data = newdata, weights=weights)
or <- exp(coef(myloggt))-1
CI <- exp(confint(myloggt))-1
CIlower <-CI[,1]
CIupper <- CI[,2]
ORplot <- cbind(or,CIlower, CIupper)
ORplot <- ORplot[c(2,3,4),]
cat <- c(1,2,3)
ORplot <- cbind(ORplot, cat)
or00 <- t(c(0,0,0,0))
ORplot <- rbind(or00, ORplot)
newA1 <- as.data.frame(ORplot)
newA1$cat <- as.factor(newA1$cat)

## WQS-Stepwise
covariates<-validation[,c(51,52,53,54)]
log_final_ga<-log(validation$final_ga)
weights<-validation$weight
AFTdata<-cbind(log_final_ga, covariates, weights)
AFTdata<-na.omit(AFTdata)
AFTdata<-as.data.frame(cbind(AFTdata, wqs_final2))
quantile(wqs_final2)

newdata <- AFTdata
for (i in 1 : 278 ) {
  if (newdata$wqs_final2[i] < 0.617758){
    newdata$wqs[i] = 0
  }
  else if (newdata$wqs_final2[i] > 0.617758 & newdata$wqs_final2[i] < 1.494012 ){
    newdata$wqs[i] = 1
  }
  else if (newdata$wqs_final2[i] > 1.494012 & newdata$wqs_final2[i] < 2.365786 ){
    newdata$wqs[i] = 2
  }
  else {newdata$wqs[i] = 3}
}
newdata$wqs <- as.factor(newdata$wqs)
newdata$wqs <- relevel(newdata$wqs, ref = "0")
myloggt <- lm(log_final_ga ~ wqs+age+race_cat_new+edu_cat_new+insur_new, data = newdata, weights=weights)
or <- exp(coef(myloggt))-1
CI <- exp(confint(myloggt))-1
CIlower <-CI[,1]
CIupper <- CI[,2]
ORplot <- cbind(or,CIlower, CIupper)
ORplot <- ORplot[c(2,3,4),]
cat <- c(1,2,3)
ORplot <- cbind(ORplot, cat)
or00 <- t(c(0,0,0,0))
ORplot <- rbind(or00, ORplot)
newA2 <- as.data.frame(ORplot)
newA2$cat <- as.factor(newA2$cat)


##ERS-Corr
betam<-c(3.58E-01,
         1.81E-02,
         2.63E-01,
         -2.68E-01,
         7.22E-02,
         -6.69E-02)
exposurem<-rawdata[, c(118,120,121,122,123,124)]
exposurem<-data.matrix(exposurem)
ERS2m<-as.data.frame(exposurem%*%betam)
ERS2m$preterm<-continousdata$preterm
ERS_control<-ERS2m[ERS2m$preterm==0,]
ERScomplete<-na.omit(ERS_control$V1)
mean(ERScomplete)
range(ERScomplete)
sd(ERScomplete)
quantile(ERScomplete)

library(outliers)
ERS_all<-ERS2m$V1
chisq.out.test(ERS_all,variance = var(ERS_all),opposite = FALSE)
quantile(ERScomplete)

newdata <- cbind2(ERS2m, continousdata)
newdata <- newdata[,c(1,53,54,55,56,106,127,128)]
newdata <- newdata[order(newdata$V1),]
newdata <- newdata[ which(newdata$V1!=4.1504775197978),]

for (i in 1 : 463) {
  if (newdata$V1[i] < 1.5173250){
    newdata$ERS[i] = 0
  }
  else if (newdata$V1[i] > 1.5173250 & newdata$V1[i] < 1.8043991 ){
    newdata$ERS[i] = 1
  }
  else if (newdata$V1[i] > 1.8043991 & newdata$V1[i] < 2.1231388){
    newdata$ERS[i] = 2
  }
  else {newdata$ERS[i] = 3}
}
newdata$ERS <- as.factor(newdata$ERS)
newdata$ERS <- relevel(newdata$ERS, ref = "0")
myloggt <- lm(log_final_ga ~ ERS+averagesg+age+race_cat_new+edu_cat_new+insur_new, data = newdata, weights=weight)
or <- exp(coef(myloggt))-1
CI <- exp(confint(myloggt))-1
CIlower <-CI[,1]
CIupper <- CI[,2]
ORplot <- cbind(or,CIlower, CIupper)
ORplot <- ORplot[c(2,3,4),]
cat <- c(1,2,3)
ORplot <- cbind(ORplot, cat)
or00 <- t(c(0,0,0,0))
ORplot <- rbind(or00, ORplot)
newA3 <- as.data.frame(ORplot)
newA3$cat <- as.factor(newA3$cat)

## ERS-Stepwise
betam<-c(0.424,
         0.761,
         -2.289,
         1.357,
         0.317,
         -0.305)
exposurem<-rawdata[, c(115,116,117,118,121,122)]
exposurem<-data.matrix(exposurem)
ERS2m<-as.data.frame(exposurem%*%betam)
ERS2m$preterm<-continousdata$preterm
ERS_control<-ERS2m[ERS2m$preterm==0,]
ERScomplete<-na.omit(ERS_control$V1)
mean(ERScomplete)
range(ERScomplete)
sd(ERScomplete)
quantile(ERScomplete)

library(outliers)
ERS_all<-ERS2m$V1
chisq.out.test(ERS_all,variance = var(ERS_all),opposite = FALSE)
quantile(ERScomplete)

newdata <- cbind2(ERS2m, continousdata)
newdata <- newdata[,c(1,53,54,55,56,106,127,128)]
newdata <- newdata[order(newdata$V1),]
newdata <- newdata[ which(newdata$V1!= -0.122082151373762),]

for (i in 1 : 463) {
  if (newdata$V1[i] < 2.0635147){
    newdata$ERS[i] = 0
  }
  else if (newdata$V1[i] > 2.0635147 & newdata$V1[i] < 2.3825464 ){
    newdata$ERS[i] = 1
  }
  else if (newdata$V1[i] > 2.3825464 & newdata$V1[i] < 2.7491998){
    newdata$ERS[i] = 2
  }
  else {newdata$ERS[i] = 3}
}
newdata$ERS <- as.factor(newdata$ERS)
newdata$ERS <- relevel(newdata$ERS, ref = "0")
myloggt <- lm(log_final_ga ~ ERS+averagesg+age+race_cat_new+edu_cat_new+insur_new, data = newdata, weights=weight)
or <- exp(coef(myloggt))-1
CI <- exp(confint(myloggt))-1
CIlower <-CI[,1]
CIupper <- CI[,2]
ORplot <- cbind(or,CIlower, CIupper)
ORplot <- ORplot[c(2,3,4),]
cat <- c(1,2,3)
ORplot <- cbind(ORplot, cat)
or00 <- t(c(0,0,0,0))
ORplot <- rbind(or00, ORplot)
newA4 <- as.data.frame(ORplot)
newA4$cat <- as.factor(newA4$cat)


## MECPP
quantile(rawdata$mean_mecpp)
newdata <- continousdata[,c(1,2,51,52,53,54,104,118,125,126)]
for (i in 1 : 482 ) {
  if (newdata$mean_mecpp[i] < 2.9191){
    newdata$mecpp[i] = 0
  }
  else if (newdata$mean_mecpp[i] > 2.9191 & newdata$mean_mecpp[i] < 3.6010 ){
    newdata$mecpp[i] = 1
  }
  else if (newdata$mean_mecpp[i] > 3.6010 & newdata$mean_mecpp[i] < 4.4152 ){
    newdata$mecpp[i] = 2
  }
  else {newdata$mecpp[i] = 3}
}
newdata$mecpp <- as.factor(newdata$mecpp)
newdata$mecpp <- relevel(newdata$mecpp, ref = "0")
myloggt <- lm(log_final_ga ~ mecpp+averagesg+age+race_cat_new+edu_cat_new, data = newdata, weights=weight)
or <- exp(coef(myloggt))-1
CI <- exp(confint(myloggt))-1
CIlower <-CI[,1]
CIupper <- CI[,2]
ORplot <- cbind(or,CIlower, CIupper)
ORplot <- ORplot[c(2,3,4),]
cat <- c(1,2,3)
ORplot <- cbind(ORplot, cat)
or00 <- t(c(0,0,0,0))
ORplot <- rbind(or00, ORplot)
newA5 <- as.data.frame(ORplot)
newA5$cat <- as.factor(newA5$cat)

## MBP
quantile(rawdata$mean_mbp)
newdata <- continousdata[,c(1,2,51,52,53,54,104,121,125,126)]
for (i in 1 : 482 ) {
  if (newdata$mean_mbp[i] < 2.0945){
    newdata$mbp[i] = 0
  }
  else if (newdata$mean_mbp[i] > 2.0945 & newdata$mean_mbp[i] < 2.7380 ){
    newdata$mbp[i] = 1
  }
  else if (newdata$mean_mbp[i] > 2.7380 & newdata$mean_mbp[i] < 3.3446 ){
    newdata$mbp[i] = 2
  }
  else {newdata$mbp[i] = 3}
}
newdata$mbp <- as.factor(newdata$mbp)
newdata$mbp <- relevel(newdata$mbp, ref = "0")
myloggt <- lm(log_final_ga ~ mbp+averagesg+age+race_cat_new+edu_cat_new+insur_new, data = newdata, weights=weight)
or <- exp(coef(myloggt))-1
CI <- exp(confint(myloggt))-1
CIlower <-CI[,1]
CIupper <- CI[,2]
ORplot <- cbind(or,CIlower, CIupper)
ORplot <- ORplot[c(2,3,4),]
cat <- c(1,2,3)
ORplot <- cbind(ORplot, cat)
or00 <- t(c(0,0,0,0))
ORplot <- rbind(or00, ORplot)
newA6 <- as.data.frame(ORplot)
newA6$cat <- as.factor(newA6$cat)


### Cox model
## WQS-Corr
covariates<-validation[,c(51,52,53,54)]
birth<-validation$birth
final_ga<-validation$final_ga
Coxdata<-cbind(final_ga, covariates, weights, birth)
Coxdata<-na.omit(Coxdata)
newdata <- cbind(wqs_final1, Coxdata)
quantile(wqs_final1)
for (i in 1 : 278 ) {
  if (newdata$wqs_final1[i] < 0.9554456){
    newdata$wqs[i] = 0
  }
  else if (newdata$wqs_final1[i] > 0.9554456 & newdata$wqs_final1[i] < 1.4728226 ){
    newdata$wqs[i] = 1
  }
  else if (newdata$wqs_final1[i] > 1.4728226 & newdata$wqs_final1[i] < 2.1599789 ){
    newdata$wqs[i] = 2
  }
  else {newdata$wqs[i] = 3}
}
newdata$wqs <- as.factor(newdata$wqs)
newdata$wqs <- relevel(newdata$wqs, ref = "0")
SURV<-newdata
cut.points <- unique(SURV$final_ga[SURV$birth == 1])
library("survival")
model.1 <- coxph(Surv(final_ga, birth) ~ wqs+age+race_cat_new+edu_cat_new+insur_new, data = SURV, weights=weights)
or <- exp(coef(model.1))
CI <- exp(confint(model.1))
CIlower <-CI[,1]
CIupper <- CI[,2]
ORplot <- cbind(or,CIlower, CIupper)
ORplot <- ORplot[c(1,2,3),]
cat <- c(1,2,3)
ORplot <- cbind(ORplot, cat)
or00 <- t(c(1,1,1,0))
ORplot <- rbind(or00, ORplot)
newC1 <- as.data.frame(ORplot)
newC1$cat <- as.factor(newC1$cat)

## WQS-Stepwise
covariates<-validation[,c(51,52,53,54)]
birth<-validation$birth
final_ga<-validation$final_ga
Coxdata<-cbind(final_ga, covariates, weights, birth)
Coxdata<-na.omit(Coxdata)
newdata <- cbind(wqs_final2, Coxdata)
quantile(wqs_final2)
for (i in 1 : 278 ) {
  if (newdata$wqs_final2[i] < 0.617758){
    newdata$wqs[i] = 0
  }
  else if (newdata$wqs_final2[i] > 0.617758 & newdata$wqs_final2[i] < 1.494012 ){
    newdata$wqs[i] = 1
  }
  else if (newdata$wqs_final2[i] > 1.494012 & newdata$wqs_final2[i] < 2.365786 ){
    newdata$wqs[i] = 2
  }
  else {newdata$wqs[i] = 3}
}
newdata$wqs <- as.factor(newdata$wqs)
newdata$wqs <- relevel(newdata$wqs, ref = "0")
SURV<-newdata
cut.points <- unique(SURV$final_ga[SURV$birth == 1])
library("survival")
model.1 <- coxph(Surv(final_ga, birth) ~ wqs+age+race_cat_new+edu_cat_new+insur_new, data = SURV, weights=weights)
or <- exp(coef(model.1))
CI <- exp(confint(model.1))
CIlower <-CI[,1]
CIupper <- CI[,2]
ORplot <- cbind(or,CIlower, CIupper)
ORplot <- ORplot[c(1,2,3),]
cat <- c(1,2,3)
ORplot <- cbind(ORplot, cat)
or00 <- t(c(1,1,1,0))
ORplot <- rbind(or00, ORplot)
newC2 <- as.data.frame(ORplot)
newC2$cat <- as.factor(newC2$cat)



## ERS-Corr
betam<-c(3.58E-01,
         1.81E-02,
         2.63E-01,
         -2.68E-01,
         7.22E-02,
         -6.69E-02)
exposurem<-rawdata[, c(118,120,121,122,123,124)]
exposurem<-data.matrix(exposurem)
ERS2m<-as.data.frame(exposurem%*%betam)
ERS2m$preterm<-continousdata$preterm
ERS_control<-ERS2m[ERS2m$preterm==0,]
ERScomplete<-na.omit(ERS_control$V1)
mean(ERScomplete)
range(ERScomplete)
sd(ERScomplete)
quantile(ERScomplete)

library(outliers)
ERS_all<-ERS2m$V1
chisq.out.test(ERS_all,variance = var(ERS_all),opposite = FALSE)
quantile(ERScomplete)

newdata <- cbind2(ERS2m, continousdata)
newdata <- newdata[,c(1,8,53,54,55,56,106,127,129)]
newdata <- na.omit(newdata)
newdata <- newdata[order(newdata$V1),]
newdata <- newdata[ which(newdata$V1!=4.1504775197978),]

for (i in 1 : 462 ) {
  if (newdata$V1[i] < 1.5212268){
    newdata$ERS[i] = 0
  }
  else if (newdata$V1[i] > 1.5212268 & newdata$V1[i] < 1.8049002 ){
    newdata$ERS[i] = 1
  }
  else if (newdata$V1[i] > 1.8049002 & newdata$V1[i] < 2.1273780 ){
    newdata$ERS[i] = 2
  }
  else {newdata$ERS[i] = 3}
}
newdata$ERS <- as.factor(newdata$ERS)
newdata$ERS <- relevel(newdata$ERS, ref = "0")
SURV<-newdata
cut.points <- unique(SURV$final_ga[SURV$birth == 1])
library("survival")
model.1 <- coxph(Surv(final_ga, birth) ~ ERS+age+race_cat_new+edu_cat_new+insur_new+averagesg, data = SURV, weights=weight)
or <- exp(coef(model.1))
CI <- exp(confint(model.1))
CIlower <-CI[,1]
CIupper <- CI[,2]
ORplot <- cbind(or,CIlower, CIupper)
ORplot <- ORplot[c(1,2,3),]
cat <- c(1,2,3)
ORplot <- cbind(ORplot, cat)
or00 <- t(c(1,1,1,0))
ORplot <- rbind(or00, ORplot)
newC3 <- as.data.frame(ORplot)
newC3$cat <- as.factor(newC3$cat)


## ERS-Stepwise
betam<-c(0.424,
         0.761,
         -2.289,
         1.357,
         0.317,
         -0.305)
exposurem<-rawdata[, c(115,116,117,118,121,122)]
exposurem<-data.matrix(exposurem)
ERS2m<-as.data.frame(exposurem%*%betam)
ERS2m$preterm<-continousdata$preterm
ERS_control<-ERS2m[ERS2m$preterm==0,]
ERScomplete<-na.omit(ERS_control$V1)
mean(ERScomplete)
range(ERScomplete)
sd(ERScomplete)
quantile(ERScomplete)

library(outliers)
ERS_all<-ERS2m$V1
chisq.out.test(ERS_all,variance = var(ERS_all),opposite = FALSE)
quantile(ERScomplete)


# OR
newdata <- cbind2(ERS2m, continousdata)
newdata <- newdata[,c(1,8,53,54,55,56,106,127,129)]
newdata <- na.omit(newdata)
newdata <- newdata[order(newdata$V1),]
newdata <- newdata[ which(newdata$V1!=-0.122082151373762),]

for (i in 1 : 462 ) {
  if (newdata$V1[i] < 2.0635147){
    newdata$ERS[i] = 0
  }
  else if (newdata$V1[i] > 2.0635147 & newdata$V1[i] < 2.3825464 ){
    newdata$ERS[i] = 1
  }
  else if (newdata$V1[i] > 2.3825464 & newdata$V1[i] < 2.7491998 ){
    newdata$ERS[i] = 2
  }
  else {newdata$ERS[i] = 3}
}
newdata$ERS <- as.factor(newdata$ERS)
newdata$ERS <- relevel(newdata$ERS, ref = "0")
SURV<-newdata
cut.points <- unique(SURV$final_ga[SURV$birth == 1])
library("survival")
model.1 <- coxph(Surv(final_ga, birth) ~ ERS+age+race_cat_new+edu_cat_new+insur_new+averagesg, data = SURV, weights=weight)
or <- exp(coef(model.1))
CI <- exp(confint(model.1))
CIlower <-CI[,1]
CIupper <- CI[,2]
ORplot <- cbind(or,CIlower, CIupper)
ORplot <- ORplot[c(1,2,3),]
cat <- c(1,2,3)
ORplot <- cbind(ORplot, cat)
or00 <- t(c(1,1,1,0))
ORplot <- rbind(or00, ORplot)
newC4 <- as.data.frame(ORplot)
newC4$cat <- as.factor(newC4$cat)



## MECPP
quantile(rawdata$mean_mecpp)
newdata <- continousdata[,c(1,6,51,52,53,54,104,118,125,127)]
for (i in 1 : 482 ) {
  if (newdata$mean_mecpp[i] < 2.9191){
    newdata$mecpp[i] = 0
  }
  else if (newdata$mean_mecpp[i] > 2.9191 & newdata$mean_mecpp[i] < 3.6010 ){
    newdata$mecpp[i] = 1
  }
  else if (newdata$mean_mecpp[i] > 3.6010 & newdata$mean_mecpp[i] < 4.4152 ){
    newdata$mecpp[i] = 2
  }
  else {newdata$mecpp[i] = 3}
}
newdata$mecpp <- as.factor(newdata$mecpp)
newdata$mecpp <- relevel(newdata$mecpp, ref = "0")
SURV<-newdata
cut.points <- unique(SURV$final_ga[SURV$birth == 1])
library("survival")
model.1 <- coxph(Surv(final_ga, birth) ~ mecpp+age+race_cat_new+edu_cat_new+averagesg, data = SURV, weights=weight)
or <- exp(coef(model.1))
CI <- exp(confint(model.1))
CIlower <-CI[,1]
CIupper <- CI[,2]
ORplot <- cbind(or,CIlower, CIupper)
ORplot <- ORplot[c(1,2,3),]
cat <- c(1,2,3)
ORplot <- cbind(ORplot, cat)
or00 <- t(c(1,1,1,0))
ORplot <- rbind(or00, ORplot)
newC5 <- as.data.frame(ORplot)
newC5$cat <- as.factor(newC5$cat)

## MBP
quantile(rawdata$mean_mbp)
newdata <- continousdata[,c(1,6,51,52,53,54,104,121,125,127)]
for (i in 1 : 482 ) {
  if (newdata$mean_mbp[i] < 2.0945){
    newdata$mbp[i] = 0
  }
  else if (newdata$mean_mbp[i] > 2.0945 & newdata$mean_mbp[i] < 2.7380 ){
    newdata$mbp[i] = 1
  }
  else if (newdata$mean_mbp[i] > 2.7380 & newdata$mean_mbp[i] < 3.3446 ){
    newdata$mbp[i] = 2
  }
  else {newdata$mbp[i] = 3}
}
newdata$mbp <- as.factor(newdata$mbp)
newdata$mbp <- relevel(newdata$mbp, ref = "0")
SURV<-newdata
cut.points <- unique(SURV$final_ga[SURV$birth == 1])
library("survival")
model.1 <- coxph(Surv(final_ga, birth) ~ mbp+age+race_cat_new+edu_cat_new+averagesg+insur_new, data = SURV, weights=weight)
or <- exp(coef(model.1))
CI <- exp(confint(model.1))
CIlower <-CI[,1]
CIupper <- CI[,2]
ORplot <- cbind(or,CIlower, CIupper)
ORplot <- ORplot[c(1,2,3),]
cat <- c(1,2,3)
ORplot <- cbind(ORplot, cat)
or00 <- t(c(1,1,1,0))
ORplot <- rbind(or00, ORplot)
newC6 <- as.data.frame(ORplot)
newC6$cat <- as.factor(newC6$cat)



## Whole plot for Figure 1
new_l<-rbind(new1,new2,new3,new4,new5,new6)
new_A<-rbind(newA1,newA2,newA3,newA4,newA5,newA6)
new_C<-rbind(newC1,newC2,newC3,newC4,newC5,newC6)


pdf("Figure 1.pdf",width=6.7)
par(mfrow=c(3,1))
x = c(1,8,15,22,2,9,16,23,3,10,17,24,4,11,18,25,5,12,19,26,6,13,20,27)
color =c(rep("orange",4), rep("blue",4), rep("red",4), rep("green",4),
         rep("purple",4),rep("black",4))

plot(new_l$or~x, col=color, pch=20, ylim=c(0, 10), xlim=c(0,35), xlab="", xaxt="n", 
     ylab="Odds Ratios", main="Logistic Regression")
abline(h=1, col="grey", lty=2)

arrows(x, new_l$CIlower, x, new_l$CIupper, col=color,angle=90,length=0.05,code=3,lwd=2)
legend("topright", legend=c("WQS-Corr","WQS-Stepwise","ERS-Corr","ERS-Stepwise", "MECPP", "MBP"),
       col=c( "orange", "blue", "red", "green","purple","black"), lwd=3, inset=c(0.02,0.01), bty="n", cex=0.9)
axis(1, c(4,11,18,25), expression(paste("Quartile 1"), paste("Quartile 2"), paste("Quartile 3"), paste("Quartile 4")),  tck=0)

plot(new_C$or~x, col=color, pch=20, ylim=c(0.5, 2.4), xlim=c(0,35), xlab="", xaxt="n", 
     ylab="Hazard Ratios", main="Cox Model")
abline(h=1, col="grey", lty=2)

arrows(x, new_C$CIlower, x, new_C$CIupper, col=color,angle=90,length=0.05,code=3,lwd=2)
legend("topright", legend=c("WQS-Corr","WQS-Stepwise","ERS-Corr","ERS-Stepwise", "MECPP", "MBP"),
       col=c( "orange", "blue", "red", "green","purple","black"), lwd=3, inset=c(0.02,0.01), bty="n", cex=0.9)
axis(1, c(4,11,18,25), expression(paste("Quartile 1"), paste("Quartile 2"), paste("Quartile 3"), paste("Quartile 4")),  tck=0)

plot(new_A$or~x, col=color, pch=20, ylim=c(-0.06, 0.03), xlim=c(0,35), xlab="", xaxt="n", 
     ylab="Percent Change", main="AFT Model")
abline(h=0, col="grey", lty=2)

arrows(x, new_A$CIlower, x, new_A$CIupper, col=color,angle=90,length=0.05,code=3,lwd=2)
legend("topright", legend=c("WQS-Corr","WQS-Stepwise","ERS-Corr","ERS-Stepwise", "MECPP", "MBP"),
       col=c( "orange", "blue", "red", "green","purple","black"), lwd=3, inset=c(0.02,0.01), bty="n", cex=0.9)
axis(1, c(4,11,18,25), expression(paste("Quartile 1"), paste("Quartile 2"), paste("Quartile 3"), paste("Quartile 4")),  tck=0)

dev.off()

## plot values
library(xlsx)
write.xlsx(new_l,"Figure 1 data.xlsx", sheetName = "logistic")
write.xlsx(new_A,"Figure 1 data.xlsx", sheetName = "AFT", append = T)
write.xlsx(new_C,"Figure 1 data.xlsx", sheetName = "Cox", append = T)

