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
continousdata$birth<-1
continousdata$log_final_ga<-log(continousdata$final_ga)
library("My.stepwise")

## mean analysis weights
## ERS-Corr
## Cox model
SURV<-continousdata
cut.points <- unique(SURV$final_ga[SURV$birth == 1])
library("survival")
model.1 <- coxph(Surv(final_ga, birth) ~ mean_mecpp+mean_mbzp+mean_mbp+mean_mibp+mean_mep+mean_mcpp+averagesg+age+race_cat_new+edu_cat_new+insur_new, data = SURV, weights=weight)
round(coef(model.1), digits = 4)

## AFT
AFT<-lm(log_final_ga ~ mean_mecpp+mean_mbzp+mean_mbp+mean_mibp+mean_mep+mean_mcpp+averagesg+age+race_cat_new+edu_cat_new+insur_new, data=continousdata, weights=weight)
round(coef(AFT), digits = 4)

## ERS-Stepwise
## Cox model
model.1 <- coxph(Surv(final_ga, birth) ~ mean_mehp+mean_mehhp+mean_meohp+mean_mecpp+mean_mbp+mean_mibp+age+race_cat_new+edu_cat_new+insur_new, data = SURV, weights=weight)
round(coef(model.1), digits = 4)
## AFT
AFT<-lm(log_final_ga ~ mean_meohp+mean_mecpp+mean_mbp+mean_mibp+age+race_cat_new+edu_cat_new+insur_new, data=continousdata, weights=weight)
round(coef(AFT), digits = 4)

## ERS-All
## Cox model
model.1 <- coxph(Surv(final_ga, birth) ~ 
                   mean_mehp+mean_mehhp+mean_meohp+mean_mecpp+mean_dehpsum+
                   mean_mbzp+mean_mbp+mean_mibp+mean_mep+mean_mcpp+
                   age+race_cat_new+edu_cat_new+insur_new, data = SURV, weights=weight)
round(coef(model.1), digits = 4)
## AFT
AFT<-lm(log_final_ga ~ 
          mean_mehp+mean_mehhp+mean_meohp+mean_mecpp+mean_dehpsum+
          mean_mbzp+mean_mbp+mean_mibp+mean_mep+mean_mcpp+
          age+race_cat_new+edu_cat_new+insur_new, data=continousdata, weights=weight)
round(coef(AFT), digits = 4)
