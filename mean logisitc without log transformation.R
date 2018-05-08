rawdata <-readRDS("rawdata.rds")
rawdata[,7:46]<-rawdata[,7:46]
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

or<-rep(0,10)
confint<-matrix(0,nrow=10,ncol=2)
# Single average exposure
Mmehp<-glm(preterm~mean_mehp+averagesg+age+race_cat_new+edu_cat_new,data=rawdata,family=binomial(link="logit"),na.action(na.omit))
summary(Mmehp)
or[1]<-exp(coef(Mmehp)*(quantile(mean_mehp,0.75)-quantile(mean_mehp,0.25)))[2]
confint[1,]<-exp(confint(Mmehp)*(quantile(mean_mehp,0.75)-quantile(mean_mehp,0.25)))[2,]
AIC(Mmehp)
BIC(Mmehp)

Mmehhp<-glm(preterm~mean_mehhp+averagesg+age+race_cat_new+edu_cat_new,data=rawdata,family=binomial(link="logit"),na.action(na.omit))
summary(Mmehhp)
or[2]<-exp(coef(Mmehhp)*(quantile(mean_mehhp,0.75)-quantile(mean_mehhp,0.25)))[2]
confint[2,]<-exp(confint(Mmehhp)*(quantile(mean_mehhp,0.75)-quantile(mean_mehhp,0.25)))[2,]
AIC(Mmehhp)
BIC(Mmehhp)

Mmeohp<-glm(preterm~mean_meohp+averagesg+age+race_cat_new+edu_cat_new,data=rawdata,family=binomial(link="logit"),na.action(na.omit))
summary(Mmeohp)
or[3]<-exp(coef(Mmeohp)*(quantile(mean_meohp,0.75)-quantile(mean_meohp,0.25)))[2]
confint[3,]<-exp(confint(Mmeohp)*(quantile(mean_meohp,0.75)-quantile(mean_meohp,0.25)))[2,]
AIC(Mmeohp)
BIC(Mmeohp)

Mmecpp<-glm(preterm~mean_mecpp+averagesg+age+race_cat_new+edu_cat_new,data=rawdata,family=binomial(link="logit"),na.action(na.omit))
summary(Mmecpp)
or[4]<-exp(coef(Mmecpp)*(quantile(mean_mecpp,0.75)-quantile(mean_mecpp,0.25)))[2]
confint[4,]<-exp(confint(Mmecpp)*(quantile(mean_mecpp,0.75)-quantile(mean_mecpp,0.25)))[2,]
AIC(Mmecpp)
BIC(Mmecpp)

Mdehpsum<-glm(preterm~mean_dehpsum+averagesg+age+race_cat_new+edu_cat_new,data=rawdata,family=binomial(link="logit"),na.action(na.omit))
summary(Mdehpsum)
or[5]<-exp(coef(Mdehpsum)*(quantile(mean_dehpsum,0.75)-quantile(mean_dehpsum,0.25)))[2]
confint[5,]<-exp(confint(Mdehpsum)*(quantile(mean_dehpsum,0.75)-quantile(mean_dehpsum,0.25)))[2,]
AIC(Mdehpsum)
BIC(Mdehpsum)

Mmbzp<-glm(preterm~mean_mbzp+averagesg+age+race_cat_new+edu_cat_new+insur_new,data=rawdata,family=binomial(link="logit"),na.action(na.omit))
summary(Mmbzp)
or[6]<-exp(coef(Mmbzp)*(quantile(mean_mbzp,0.75)-quantile(mean_mbzp,0.25)))[2]
confint[6,]<-exp(confint(Mmbzp)*(quantile(mean_mbzp,0.75)-quantile(mean_mbzp,0.25)))[2,]
AIC(Mmbzp)
BIC(Mmbzp)

Mmbp<-glm(preterm~mean_mbp+averagesg+age+race_cat_new+edu_cat_new+insur_new,data=rawdata,family=binomial(link="logit"),na.action(na.omit))
summary(Mmbp)
or[7]<-exp(coef(Mmbp)*(quantile(mean_mbp,0.75)-quantile(mean_mbp,0.25)))[2]
confint[7,]<-exp(confint(Mmbp)*(quantile(mean_mbp,0.75)-quantile(mean_mbp,0.25)))[2,]
AIC(Mmbp)
BIC(Mmbp)

Mmibp<-glm(preterm~mean_mibp+averagesg+age+race_cat_new+edu_cat_new+insur_new,data=rawdata,family=binomial(link="logit"),na.action(na.omit))
summary(Mmibp)
or[8]<-exp(coef(Mmibp)*(quantile(mean_mibp,0.75)-quantile(mean_mibp,0.25)))[2]
confint[8,]<-exp(confint(Mmibp)*(quantile(mean_mibp,0.75)-quantile(mean_mibp,0.25)))[2,]
AIC(Mmibp)
BIC(Mmibp)

Mmep<-glm(preterm~mean_mep+averagesg+age+race_cat_new+edu_cat_new+insur_new,data=rawdata,family=binomial(link="logit"),na.action(na.omit))
summary(Mmep)
or[9]<-exp(coef(Mmep)*(quantile(mean_mep,0.75)-quantile(mean_mep,0.25)))[2]
confint[9,]<-exp(confint(Mmep)*(quantile(mean_mep,0.75)-quantile(mean_mep,0.25)))[2,]
AIC(Mmep)
BIC(Mmep)

Mmcpp<-glm(preterm~mean_mcpp+averagesg+age+race_cat_new+edu_cat_new+insur_new,data=rawdata,family=binomial(link="logit"),na.action(na.omit))
summary(Mmcpp)
or[10]<-exp(coef(Mmcpp)*(quantile(mean_mcpp,0.75)-quantile(mean_mcpp,0.25)))[2]
confint[10,]<-exp(confint(Mmcpp)*(quantile(mean_mcpp,0.75)-quantile(mean_mcpp,0.25)))[2,]
AIC(Mmcpp)
BIC(Mmcpp)