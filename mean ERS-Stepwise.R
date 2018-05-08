rawdata <-readRDS("rawdata.rds")
## transform response varibale and exposure variable
rawdata[,7:46]<-log(rawdata[,7:46])
# calculate mean
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

## logistic regression
betam<-c(0.424,
         0.761,
         -2.289,
         1.357,
         0.317,
         -0.305)
exposurem<-rawdata[, c(105,106,107,108,111,112)]
exposurem<-data.matrix(exposurem)
ERS2m<-exposurem%*%betam
newdata <- cbind2(ERS2m, rawdata)
newdata <- newdata[,c(1,4,52,53,54,55,116)]
newdata <- na.omit(newdata)
ERS_logistic<-glm(preterm~x+averagesg+age+race_cat_new+edu_cat_new+insur_new,data=newdata,family=binomial(link="logit"))
summary(ERS_logistic)
exp(coef(ERS_logistic)*(quantile(newdata$x,0.75)-quantile(newdata$x,0.25)))
exp(confint(ERS_logistic)*(quantile(newdata$x,0.75)-quantile(newdata$x,0.25)))


## log gestational age(AFT)
continousdata<-rawdata
continousdata$final_ga<-(rawdata$final_ga)*7
continousdata$log_final_ga<-log(continousdata$final_ga)
newdata <- cbind2(ERS2m, continousdata)
newdata <- newdata[,c(1,52,53,54,55,105,116,117)]
ERS_AFT<-lm(log_final_ga~x+averagesg+age+race_cat_new+edu_cat_new+insur_new, data=newdata)
summary(ERS_AFT)
exp(coef(ERS_AFT)*(quantile(newdata$x,0.75)-quantile(newdata$x,0.25)))-1
exp(confint(ERS_AFT)*(quantile(newdata$x,0.75)-quantile(newdata$x,0.25)))-1


## Cox model
continousdata$birth<-1
newdata <- cbind2(ERS2m, continousdata)
newdata <- newdata[,c(1,7,52,53,54,55,105,116,118)]
SURV<-newdata
cut.points <- unique(SURV$final_ga[SURV$birth == 1])
library("survival")
model.1 <- coxph(Surv(final_ga, birth) ~ x+averagesg+age+race_cat_new+edu_cat_new+insur_new, data = SURV)
summary(model.1)
exp(coef(model.1)*(quantile(newdata$x,0.75)-quantile(newdata$x,0.25)))
exp(confint(model.1)*(quantile(newdata$x,0.75)-quantile(newdata$x,0.25)))

