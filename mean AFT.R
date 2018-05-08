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
## transform response varibale and exposure variable
continousdata<-rawdata
continousdata$final_ga<-(rawdata$final_ga)*7
continousdata$log_final_ga<-log(continousdata$final_ga)
continousdata$race_cat_new <- as.factor(continousdata$race_cat_new)
continousdata$edu_cat_new <- as.factor(continousdata$edu_cat_new)

changerate<-rep(0,10)
confint<-matrix(0,nrow=10,ncol=2)
## single mean exposure
MEHP_mean<-lm(log_final_ga~mean_mehp+averagesg+age+race_cat_new+edu_cat_new, data=continousdata, weights=weight)
summary(MEHP_mean)
changerate[1]<-(exp(coef(MEHP_mean)*(quantile(mean_mehp,0.75)-quantile(mean_mehp,0.25)))-1)[2]
confint[1,]<-(exp(confint(MEHP_mean)*(quantile(mean_mehp,0.75)-quantile(mean_mehp,0.25)))-1)[2,]

MEHHP_mean<-lm(log_final_ga~mean_mehhp+averagesg+age+race_cat_new+edu_cat_new, data=continousdata, weights=weight)
summary(MEHHP_mean)
changerate[2]<-(exp(coef(MEHHP_mean)*(quantile(mean_mehhp,0.75)-quantile(mean_mehhp,0.25)))-1)[2]
confint[2,]<-(exp(confint(MEHHP_mean)*(quantile(mean_mehhp,0.75)-quantile(mean_mehhp,0.25)))-1)[2,]


MEOHP_mean<-lm(log_final_ga~mean_meohp+averagesg+age+race_cat_new+edu_cat_new, data=continousdata, weights=weight)
summary(MEOHP_mean)
changerate[3]<-(exp(coef(MEOHP_mean)*(quantile(mean_meohp,0.75)-quantile(mean_meohp,0.25)))-1)[2]
confint[3,]<-(exp(confint(MEOHP_mean)*(quantile(mean_meohp,0.75)-quantile(mean_meohp,0.25)))-1)[2,]

MECPP_mean<-lm(log_final_ga~mean_mecpp+averagesg+age+race_cat_new+edu_cat_new, data=continousdata, weights=weight)
summary(MECPP_mean)
changerate[4]<-(exp(coef(MECPP_mean)*(quantile(mean_mecpp,0.75)-quantile(mean_mecpp,0.25)))-1)[2]
confint[4,]<-(exp(confint(MECPP_mean)*(quantile(mean_mecpp,0.75)-quantile(mean_mecpp,0.25)))-1)[2,]

DEHPSUM_mean<-lm(log_final_ga~mean_dehpsum+averagesg+age+race_cat_new+edu_cat_new, data=continousdata, weights=weight)
summary(DEHPSUM_mean)
changerate[5]<-(exp(coef(DEHPSUM_mean)*(quantile(mean_dehpsum,0.75)-quantile(mean_dehpsum,0.25)))-1)[2]
confint[5,]<-(exp(confint(DEHPSUM_mean)*(quantile(mean_dehpsum,0.75)-quantile(mean_dehpsum,0.25)))-1)[2,]

MBZP_mean<-lm(log_final_ga~mean_mbzp+averagesg+age+race_cat_new+edu_cat_new+insur_new, data=continousdata, weights=weight)
summary(MBZP_mean)
changerate[6]<-(exp(coef(MBZP_mean)*(quantile(mean_mbzp,0.75)-quantile(mean_mbzp,0.25)))-1)[2]
confint[6,]<-(exp(confint(MBZP_mean)*(quantile(mean_mbzp,0.75)-quantile(mean_mbzp,0.25)))-1)[2,]

MBP_mean<-lm(log_final_ga~mean_mbp+averagesg+age+race_cat_new+edu_cat_new+insur_new, data=continousdata, weights=weight)
summary(MBP_mean)
changerate[7]<-(exp(coef(MBP_mean)*(quantile(mean_mbp,0.75)-quantile(mean_mbp,0.25)))-1)[2]
confint[7,]<-(exp(confint(MBP_mean)*(quantile(mean_mbp,0.75)-quantile(mean_mbp,0.25)))-1)[2,]

MIBP_mean<-lm(log_final_ga~mean_mibp+averagesg+age+race_cat_new+edu_cat_new+insur_new, data=continousdata, weights=weight)
summary(MIBP_mean)
changerate[8]<-(exp(coef(MIBP_mean)*(quantile(mean_mibp,0.75)-quantile(mean_mibp,0.25)))-1)[2]
confint[8,]<-(exp(confint(MIBP_mean)*(quantile(mean_mibp,0.75)-quantile(mean_mibp,0.25)))-1)[2,]

MEP_mean<-lm(log_final_ga~mean_mep+averagesg+age+race_cat_new+edu_cat_new+insur_new, data=continousdata, weights=weight)
summary(MEP_mean)
changerate[9]<-(exp(coef(MEP_mean)*(quantile(mean_mep,0.75)-quantile(mean_mep,0.25)))-1)[2]
confint[9,]<-(exp(confint(MEP_mean)*(quantile(mean_mep,0.75)-quantile(mean_mep,0.25)))-1)[2,]

MCPP_mean<-lm(log_final_ga~mean_mcpp+averagesg+age+race_cat_new+edu_cat_new+insur_new, data=continousdata, weights=weight)
summary(MCPP_mean)
changerate[10]<-(exp(coef(MCPP_mean)*(quantile(mean_mcpp,0.75)-quantile(mean_mcpp,0.25)))-1)[2]
confint[10,]<-(exp(confint(MCPP_mean)*(quantile(mean_mcpp,0.75)-quantile(mean_mcpp,0.25)))-1)[2,]

