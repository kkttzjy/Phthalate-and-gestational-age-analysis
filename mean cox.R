rawdata <-readRDS("rawdata.rds")
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
continousdata<-rawdata
continousdata$final_ga<-(rawdata$final_ga)*7
continousdata$race_cat_new <- as.factor(continousdata$race_cat_new)
continousdata$edu_cat_new <- as.factor(continousdata$edu_cat_new)


hr<-rep(0,10)
confint<-matrix(0,nrow=10,ncol=2)
## Cox model with single average exposure
continousdata$birth<-1
# mehp
temp.dat <- cbind(continousdata[, c(1,2,6,51,52,53,54,104,105,115,116)])
SURV<-temp.dat
cut.points <- unique(SURV$final_ga[SURV$birth == 1])
library("survival")
model.1 <- coxph(Surv(final_ga, birth) ~ mean_mehp+age+race_cat_new+edu_cat_new+averagesg, data = SURV, weights=weight)
summary(model.1)
hr[1]<-exp(coef(model.1)*(quantile(mean_mehp,0.75)-quantile(mean_mehp,0.25)))[1]
confint[1,]<-exp(confint(model.1)*(quantile(mean_mehp,0.75)-quantile(mean_mehp,0.25)))[1,]

# mehhp
temp.dat <- cbind(continousdata[, c(1,2,6,51,52,53,54,104,106,115,116)])
SURV<-temp.dat
cut.points <- unique(SURV$final_ga[SURV$birth == 1])
library("survival")
model.1 <- coxph(Surv(final_ga, birth) ~ mean_mehhp+age+race_cat_new+edu_cat_new+averagesg, data = SURV, weights=weight)
summary(model.1)
hr[2]<-exp(coef(model.1)*(quantile(mean_mehhp,0.75)-quantile(mean_mehhp,0.25)))[1]
confint[2,]<-exp(confint(model.1)*(quantile(mean_mehhp,0.75)-quantile(mean_mehhp,0.25)))[1,]


# meohp
temp.dat <- cbind(continousdata[, c(1,2,6,51,52,53,54,104,107,115,116)])
SURV<-temp.dat
cut.points <- unique(SURV$final_ga[SURV$birth == 1])
library("survival")
model.1 <- coxph(Surv(final_ga, birth) ~ mean_meohp+age+race_cat_new+edu_cat_new+averagesg, data = SURV, weights=weight)
summary(model.1)
hr[3]<-exp(coef(model.1)*(quantile(mean_meohp,0.75)-quantile(mean_meohp,0.25)))[1]
confint[3,]<-exp(confint(model.1)*(quantile(mean_meohp,0.75)-quantile(mean_meohp,0.25)))[1,]


# mecpp
temp.dat <- cbind(continousdata[, c(1,2,6,51,52,53,54,104,108,115,116)])
SURV<-temp.dat
cut.points <- unique(SURV$final_ga[SURV$birth == 1])
library("survival")
model.1 <- coxph(Surv(final_ga, birth) ~ mean_mecpp+age+race_cat_new+edu_cat_new+averagesg, data = SURV, weights=weight)
summary(model.1)
hr[4]<-exp(coef(model.1)*(quantile(mean_mecpp,0.75)-quantile(mean_mecpp,0.25)))[1]
confint[4,]<-exp(confint(model.1)*(quantile(mean_mecpp,0.75)-quantile(mean_mecpp,0.25)))[1,]

# dehpsum
temp.dat <- cbind(continousdata[, c(1,2,6,51,52,53,54,104,109,115,116)])
SURV<-temp.dat
cut.points <- unique(SURV$final_ga[SURV$birth == 1])
library("survival")
model.1 <- coxph(Surv(final_ga, birth) ~ mean_dehpsum+age+race_cat_new+edu_cat_new+averagesg, data = SURV, weights=weight)
summary(model.1)
hr[5]<-exp(coef(model.1)*(quantile(mean_dehpsum,0.75)-quantile(mean_dehpsum,0.25)))[1]
confint[5,]<-exp(confint(model.1)*(quantile(mean_dehpsum,0.75)-quantile(mean_dehpsum,0.25)))[1,]


# mbzp
temp.dat <- cbind(continousdata[, c(1,2,6,51,52,53,54,104,110,115,116)])
SURV<-temp.dat
cut.points <- unique(SURV$final_ga[SURV$birth == 1])
library("survival")
model.1 <- coxph(Surv(final_ga, birth) ~ mean_mbzp+age+race_cat_new+edu_cat_new+averagesg+insur_new, data = SURV, weights=weight)
summary(model.1)
hr[6]<-exp(coef(model.1)*(quantile(mean_mbzp,0.75)-quantile(mean_mbzp,0.25)))[1]
confint[6,]<-exp(confint(model.1)*(quantile(mean_mbzp,0.75)-quantile(mean_mbzp,0.25)))[1,]


# mbp
temp.dat <- cbind(continousdata[, c(1,2,6,51,52,53,54,104,111,115,116)])
SURV<-temp.dat
cut.points <- unique(SURV$final_ga[SURV$birth == 1])
library("survival")
model.1 <- coxph(Surv(final_ga, birth) ~ mean_mbp+age+race_cat_new+edu_cat_new+averagesg+insur_new, data = SURV, weights=weight)
summary(model.1)
hr[7]<-exp(coef(model.1)*(quantile(mean_mbp,0.75)-quantile(mean_mbp,0.25)))[1]
confint[7,]<-exp(confint(model.1)*(quantile(mean_mbp,0.75)-quantile(mean_mbp,0.25)))[1,]

# mibp
temp.dat <- cbind(continousdata[, c(1,2,6,51,52,53,54,104,112,115,116)])
SURV<-temp.dat
cut.points <- unique(SURV$final_ga[SURV$birth == 1])
library("survival")
model.1 <- coxph(Surv(final_ga, birth) ~ mean_mibp+age+race_cat_new+edu_cat_new+averagesg+insur_new, data = SURV, weights=weight)
summary(model.1)
hr[8]<-exp(coef(model.1)*(quantile(mean_mibp,0.75)-quantile(mean_mibp,0.25)))[1]
confint[8,]<-exp(confint(model.1)*(quantile(mean_mibp,0.75)-quantile(mean_mibp,0.25)))[1,]

# mep
temp.dat <- cbind(continousdata[, c(1,2,6,51,52,53,54,104,113,115,116)])
SURV<-temp.dat
cut.points <- unique(SURV$final_ga[SURV$birth == 1])
library("survival")
model.1 <- coxph(Surv(final_ga, birth) ~ mean_mep+age+race_cat_new+edu_cat_new+averagesg+insur_new, data = SURV, weights=weight)
summary(model.1)
hr[9]<-exp(coef(model.1)*(quantile(mean_mep,0.75)-quantile(mean_mep,0.25)))[1]
confint[9,]<-exp(confint(model.1)*(quantile(mean_mep,0.75)-quantile(mean_mep,0.25)))[1,]

# mcpp
temp.dat <- cbind(continousdata[, c(1,2,6,51,52,53,54,104,114,115,116)])
SURV<-temp.dat
cut.points <- unique(SURV$final_ga[SURV$birth == 1])
library("survival")
model.1 <- coxph(Surv(final_ga, birth) ~ mean_mcpp+age+race_cat_new+edu_cat_new+averagesg+insur_new, data = SURV, weights=weight)
summary(model.1)
hr[10]<-exp(coef(model.1)*(quantile(mean_mcpp,0.75)-quantile(mean_mcpp,0.25)))[1]
confint[10,]<-exp(confint(model.1)*(quantile(mean_mcpp,0.75)-quantile(mean_mcpp,0.25)))[1,]
