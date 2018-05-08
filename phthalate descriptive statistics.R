rawdata <-readRDS("rawdata.rds")
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
### descriptive statistics
## convert wide to long format
newdata <- rawdata[,64:103]
longdata <- reshape(newdata, 
                    varying = c(1:40), 
                    idvar="id", sep="_", timevar="order",
                    direction = "long")
longdata<-longdata[order(longdata$id),]
names(longdata)[names(longdata)=="order"] <- "time"

round(exp(mean(log(longdata$mehpsg),na.rm = T)),digits = 2)
round(exp(sd(log(longdata$mehpsg),na.rm = T)),digits = 2)
round(quantile(longdata$mehpsg, probs = c(0.25,0.5,0.75,0.9,0.95,1), na.rm = T),digits = 2)

round(exp(mean(log(longdata$mehhpsg),na.rm = T)),digits = 2)
round(exp(sd(log(longdata$mehhpsg),na.rm = T)),digits = 2)
round(quantile(longdata$mehhpsg, probs = c(0.25,0.5,0.75,0.9,0.95,1), na.rm = T),digits = 2)

round(exp(mean(log(longdata$meohpsg),na.rm = T)),digits = 2)
round(exp(sd(log(longdata$meohpsg),na.rm = T)),digits = 2)
round(quantile(longdata$meohpsg, probs = c(0.25,0.5,0.75,0.9,0.95,1), na.rm = T),digits = 2)

round(exp(mean(log(longdata$mecppsg),na.rm = T)),digits = 2)
round(exp(sd(log(longdata$mecppsg),na.rm = T)),digits = 2)
round(quantile(longdata$mecppsg, probs = c(0.25,0.5,0.75,0.9,0.95,1), na.rm = T),digits = 2)

round(exp(mean(log(longdata$dehpsumsg),na.rm = T)),digits = 2)
round(exp(sd(log(longdata$dehpsumsg),na.rm = T)),digits = 2)
round(quantile(longdata$dehpsumsg, probs = c(0.25,0.5,0.75,0.9,0.95,1), na.rm = T),digits = 2)

round(exp(mean(log(longdata$mbzpsg),na.rm = T)),digits = 2)
round(exp(sd(log(longdata$mbzpsg),na.rm = T)),digits = 2)
round(quantile(longdata$mbzpsg, probs = c(0.25,0.5,0.75,0.9,0.95,1), na.rm = T),digits = 2)

round(exp(mean(log(longdata$mbpsg),na.rm = T)),digits = 2)
round(exp(sd(log(longdata$mbpsg),na.rm = T)),digits = 2)
round(quantile(longdata$mbpsg, probs = c(0.25,0.5,0.75,0.9,0.95,1), na.rm = T),digits = 2)

round(exp(mean(log(longdata$mibpsg),na.rm = T)),digits = 2)
round(exp(sd(log(longdata$mibpsg),na.rm = T)),digits = 2)
round(quantile(longdata$mibpsg, probs = c(0.25,0.5,0.75,0.9,0.95,1), na.rm = T),digits = 2)

round(exp(mean(log(longdata$mepsg),na.rm = T)),digits = 2)
round(exp(sd(log(longdata$mepsg),na.rm = T)),digits = 2)
round(quantile(longdata$mepsg, probs = c(0.25,0.5,0.75,0.9,0.95,1), na.rm = T),digits = 2)

round(exp(mean(log(longdata$mcppsg),na.rm = T)),digits = 2)
round(exp(sd(log(longdata$mcppsg),na.rm = T)),digits = 2)
round(quantile(longdata$mcppsg, probs = c(0.25,0.5,0.75,0.9,0.95,1), na.rm = T),digits = 2)

