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
### check the similarity between first three visits and the fourth visit
mehp_diff1<-abs(rawdata$mehp_4-rawdata$mehp_1)/rawdata$mehp_1
mehp_diff2<-abs(rawdata$mehp_4-rawdata$mehp_2)/rawdata$mehp_2
mehp_diff3<-abs(rawdata$mehp_4-rawdata$mehp_3)/rawdata$mehp_3
mehp_diff<-abs(rawdata$mehp_4-mean_mehp)/mean_mehp
summary(mehp_diff1)
summary(mehp_diff2)
summary(mehp_diff3)
summary(mehp_diff)

