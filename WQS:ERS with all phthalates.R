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
rawdata$race_cat_new <- as.factor(rawdata$race_cat_new)
rawdata$edu_cat_new <- as.factor(rawdata$edu_cat_new)

## mean analysis weights
## ERS with all phthalates
Multimean<-glm(preterm~mean_mehp+mean_mehhp+mean_meohp+mean_mecpp+mean_dehpsum+
                 mean_mbzp+mean_mbp+mean_mibp+mean_mep+mean_mcpp+averagesg+age+
                 race_cat_new+edu_cat_new+insur_new,data=rawdata,family=binomial(link="logit"),na.action(na.omit))
summary(Multimean)
weight_all<-round(Multimean$coefficients[2:11],digits=2)
## logistic regression
betam<-weight_all
exposurem<-rawdata[, c(105:114)]
exposurem<-data.matrix(exposurem)
ERS2m<-exposurem%*%betam
ERScomplete<-na.omit(ERS2m)
quantile(ERS2m,0.75)-quantile(ERS2m,0.25)
newdata <- cbind2(ERS2m, rawdata)
newdata <- newdata[,c(1,4,52,53,54,55,116)]
newdata <- na.omit(newdata)
ERS_logistic<-glm(preterm~x+averagesg+age+race_cat_new+edu_cat_new+insur_new,data=newdata,family=binomial())
summary(ERS_logistic)
exp(coef(ERS_logistic)*(quantile(newdata$x,0.75)-quantile(newdata$x,0.25)))
exp(confint(ERS_logistic)*(quantile(newdata$x,0.75)-quantile(newdata$x,0.25)))
## log gestational age(AFT)
continousdata<-rawdata
continousdata$final_ga<-(rawdata$final_ga)*7
continousdata$log_final_ga<-log(continousdata$final_ga)
ERScomplete<-na.omit(ERS2m)
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
newdata <- na.omit(newdata)
SURV<-newdata
cut.points <- unique(SURV$final_ga[SURV$birth == 1])
library("survival")
model.1 <- coxph(Surv(final_ga, birth) ~ x+averagesg+age+race_cat_new+edu_cat_new+insur_new, data = SURV)
summary(model.1)
exp(coef(model.1)*(quantile(newdata$x,0.75)-quantile(newdata$x,0.25)))
exp(confint(model.1)*(quantile(newdata$x,0.75)-quantile(newdata$x,0.25)))

### WQS with all phthalates
rawdata <- read.csv("~/Documents/Summer research/data/rawdata.csv")
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


# WQS-Correlation
## Logistic regression
library(gWQS)
test$group = 0
validation$group = 1
datawqs<-as.data.frame(rbind(test, validation))
phthalateexposure = colnames(datawqs)[105:114]
wqsdata<-datawqs[, c(105:114,51,52,53,54,3,128)]


results1 = gwqs(preterm ~ age+race_cat_new+edu_cat_new+insur_new, mix_name = phthalateexposure, data = wqsdata,
                q = 4, validation = 0, valid_var = "group",
                b = 1000, b1_pos = T, family = "binomial", seed = 2016, wqs2 = FALSE, plots = TRUE,
                tables = TRUE)

complete<-na.omit(validation[,c(105:114,51,52,53,54,3)])
phthalate<-complete[, 1:10]
phthalate = quantile_f(phthalate, phthalateexposure, 4)
q_name = paste(phthalateexposure, "q", sep = "_")
meanweight<-results1$final_weights
mweight<-meanweight$mean_weight
qphthalate<-as.matrix(phthalate[,11:20])
wqs_final1<-qphthalate%*%mweight
quantile(wqs_final1)
## Logistic regression
exp(coef(results1$fit)*(quantile(wqs_final1,0.75)-quantile(wqs_final1,0.25)))
exp(confint(results1$fit)*(quantile(wqs_final1,0.75)-quantile(wqs_final1,0.25)))
quantile(wqs_final1,0.75)-quantile(wqs_final1,0.25)
## AFT 
covariates<-na.omit(validation[,c(126,104,51,52,53,54)])
AFTdata<-cbind(wqs_final1, covariates)
AFT<-lm(log_final_ga~wqs_final1+age+race_cat_new+edu_cat_new+insur_new, data=AFTdata, weights=weight)
exp(coef(AFT)*(quantile(wqs_final1,0.75)-quantile(wqs_final1,0.25)))-1
exp(confint(AFT)*(quantile(wqs_final1,0.75)-quantile(wqs_final1,0.25)))-1
## Cox
covariates<-na.omit(validation[,c(6,127,104,51,52,53,54)])
Coxdata<-cbind(wqs_final1, covariates)
SURV<-Coxdata
cut.points <- unique(SURV$final_ga[SURV$birth == 1])
library("survival")
cox <- coxph(Surv(final_ga, birth) ~ wqs_final1+age+race_cat_new+edu_cat_new+insur_new, data = SURV, weights=weight)
exp(coef(cox)*(quantile(wqs_final1,0.75)-quantile(wqs_final1,0.25)))
exp(confint(cox)*(quantile(wqs_final1,0.75)-quantile(wqs_final1,0.25)))


