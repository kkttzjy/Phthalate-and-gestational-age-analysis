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
## ERS-Corr
Multimean<-glm(preterm~mean_mecpp+mean_mbzp+mean_mbp+mean_mibp+mean_mep+mean_mcpp+averagesg+age+race_cat_new+edu_cat_new+insur_new,data=rawdata,family=binomial(link="logit"),na.action(na.omit))
summary(Multimean)


## ERS-Stepwise
Multimean<-glm(preterm~mean_mehp+mean_mehhp+mean_meohp+mean_mecpp+mean_mbp+mean_mibp+age+race_cat_new+edu_cat_new+insur_new,data=rawdata,family=binomial(link="logit"),na.action(na.omit))
summary(Multimean)


## time-varying analysis weights
## ERS-Corr
# V1
M_V1<-glm(preterm~mecpp_1+mbzp_1+mbp_1+mibp_1+mep_1+mcpp_1+sg_1+age+race_cat_new+edu_cat_new+insur_new+urineam_1+gat1, data=rawdata,family=binomial())
summary(M_V1)
# V2
M_V2<-glm(preterm~mecpp_2+mbzp_2+mbp_2+mibp_2+mep_2+mcpp_2+sg_2+age+race_cat_new+edu_cat_new+insur_new+urineam_2+gat2, data=rawdata,family=binomial())
summary(M_V2)
# V3
M_V3<-glm(preterm~mecpp_3+mbzp_3+mbp_3+mibp_3+mep_3+mcpp_3+sg_3+age+race_cat_new+edu_cat_new+insur_new+urineam_3+gat3, data=rawdata,family=binomial())
summary(M_V3)
# V4
M_V4<-glm(preterm~mecpp_4+mbzp_4+mbp_4+mibp_4+mep_4+mcpp_4+sg_4+age+race_cat_new+edu_cat_new+insur_new+urineam_4+gat4, data=rawdata,family=binomial())
summary(M_V4)

## ERS-Stepwise
# V1
M_V1<-glm(preterm~mehp_1+mehhp_1+meohp_1+mecpp_1+mbp_1+mibp_1+sg_1+age+race_cat_new+edu_cat_new+insur_new+urineam_1+gat1, data=rawdata,family=binomial())
summary(M_V1)
# V2
M_V2<-glm(preterm~mehp_2+mehhp_2+meohp_2+mecpp_2+mbp_2+mibp_2+sg_2+age+race_cat_new+edu_cat_new+insur_new+urineam_2+gat2, data=rawdata,family=binomial())
summary(M_V2)
# V3
M_V3<-glm(preterm~mehp_3+mehhp_3+meohp_3+mecpp_3+mbp_3+mibp_3+sg_3+age+race_cat_new+edu_cat_new+insur_new+urineam_3+gat3, data=rawdata,family=binomial())
summary(M_V3)
# V4
M_V4<-glm(preterm~mehp_4+mehhp_4+meohp_4+mecpp_4+mbp_4+mibp_4+sg_4+age+race_cat_new+edu_cat_new+insur_new+urineam_4+gat4, data=rawdata,family=binomial())
summary(M_V4)