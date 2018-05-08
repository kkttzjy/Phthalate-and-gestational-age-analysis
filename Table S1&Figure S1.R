rawdata <-readRDS("rawdata.rds")
# sg-corrected
rawdata[,64:103]<-log(rawdata[,64:103])
sgmehp<-rawdata[,64:66]
sgmehhp<-rawdata[,68:70]
sgmeohp<-rawdata[,72:74]
sgmecpp<-rawdata[,76:78]
sgdehpsum<-rawdata[,80:82]
sgmbzp<-rawdata[,84:86]
sgmbp<-rawdata[,88:90]
sgmibp<-rawdata[,92:94]
sgmep<-rawdata[,96:98]
sgmcpp<-rawdata[,100:102]
mean_sgmehp<-rowMeans(sgmehp,na.rm=TRUE)
mean_sgmehhp<-rowMeans(sgmehhp,na.rm=TRUE)
mean_sgmeohp<-rowMeans(sgmeohp,na.rm=TRUE)
mean_sgmecpp<-rowMeans(sgmecpp,na.rm=TRUE)
mean_sgdehpsum<-rowMeans(sgdehpsum,na.rm=TRUE)
mean_sgmbzp<-rowMeans(sgmbzp,na.rm=TRUE)
mean_sgmbp<-rowMeans(sgmbp,na.rm=TRUE)
mean_sgmibp<-rowMeans(sgmibp,na.rm=TRUE)
mean_sgmep<-rowMeans(sgmep,na.rm=TRUE)
mean_sgmcpp<-rowMeans(sgmcpp,na.rm=TRUE)
rawdata$mean_sgmehp<-mean_sgmehp
rawdata$mean_sgmehhp<-mean_sgmehhp
rawdata$mean_sgmeohp<-mean_sgmeohp
rawdata$mean_sgmecpp<-mean_sgmecpp
rawdata$mean_sgdehpsum<-mean_sgdehpsum
rawdata$mean_sgmbzp<-mean_sgmbzp
rawdata$mean_sgmbp<-mean_sgmbp
rawdata$mean_sgmibp<-mean_sgmibp
rawdata$mean_sgmep<-mean_sgmep
rawdata$mean_sgmcpp<-mean_sgmcpp

## Correlation matrix
# sg-corrected mean
# sgmean 
corr1<-cor(rawdata[,115:124],use="complete.obs", method="pearson" )
write.csv(corr1,'sgmeancorr.csv')

### kernal density of final gestational age
png("densityplot2.png", width = 17, height = 12, units = 'cm', res = 300)
hist(rawdata$final_ga,xlab="Final gestational age (Weeks)",
     ylim=c(0,250),
     breaks = seq(20,45,2.5),
     main=NULL)
dev.off()
