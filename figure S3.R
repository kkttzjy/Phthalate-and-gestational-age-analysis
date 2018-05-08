rawdata <-readRDS("rawdata.rds")
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

library(gWQS)
test$group = 0
validation$group = 1
datawqs<-as.data.frame(rbind(test, validation))

### logistic regression
## WQS-Corr
phthalateexposure = c( "sg_mean_mecpp", "sg_mean_mbzp", "sg_mean_mbp",
                       "sg_mean_mibp", "sg_mean_mep", "sg_mean_mcpp")
wqsdata<-datawqs[, c(108,110:114,51,52,53,54,3,128)]

wqsdatacomplete<-na.omit(wqsdata)

results1 = gwqs(preterm ~ age+race_cat_new+edu_cat_new+insur_new, mix_name = phthalateexposure, data = wqsdatacomplete,
                q = 4, validation = 0, valid_var = "group",
                b = 1000, b1_pos = T, family = "binomial", seed = 2016, wqs2 = FALSE, plots = TRUE,
                tables = TRUE)

complete<-na.omit(validation[,c(108,110:114,51,52,53,54,3)])
phthalate<-complete[, 1:6]
phthalate = quantile_f(phthalate, phthalateexposure, 4)
q_name = paste(phthalateexposure, "q", sep = "_")
meanweight<-results1$final_weights
mweight<-meanweight$mean_weight
qphthalate<-as.matrix(phthalate[,7:12])
wqs_cor<-qphthalate%*%mweight

## WQS-Stepwise
phthalateexposure = c( "sg_mean_mehp", "sg_mean_mehhp", "sg_mean_meohp",
                       "sg_mean_mecpp", "sg_mean_mbp", "sg_mean_mibp")
wqsdata<-datawqs[, c(105,106,107,108,111,112,51,52,53,54,3,128)]

wqsdatacomplete<-na.omit(wqsdata)

results2 = gwqs(preterm ~ age+race_cat_new+edu_cat_new+insur_new, mix_name = phthalateexposure, data = wqsdatacomplete,
                q = 4, validation = 0, valid_var = "group",
                b = 1000, b1_pos = T, family = "binomial", seed = 2016, wqs2 = FALSE, plots = TRUE,
                tables = TRUE)
complete<-na.omit(validation[,c(105,106,107,108,111,112,51,52,53,54,3)])
phthalate<-complete[, 1:6]
phthalate = quantile_f(phthalate, phthalateexposure, 4)
q_name = paste(phthalateexposure, "q", sep = "_")
meanweight<-results2$final_weights
mweight<-meanweight$mean_weight
qphthalate<-as.matrix(phthalate[,7:12])
wqs_step<-qphthalate%*%mweight

## ERS-Corr
betam<-c(3.58E-01,
         1.81E-02,
         2.63E-01,
         -2.68E-01,
         7.22E-02,
         -6.69E-02)
exposurem<-na.omit(validation[, c(118,120,121,122,123,124,51,52,53,54,3)])
exposurem<-data.matrix(exposurem)[,1:6]
ERS2m<-as.data.frame(exposurem%*%betam)
ERS_corr<-na.omit(ERS2m)

## ERS-Stepwise
betam<-c(0.424,
         0.761,
         -2.289,
         1.357,
         0.317,
         -0.305)
exposurem<-na.omit(validation[, c(115,116,117,118,121,122,51,52,53,54,3)])
exposurem<-data.matrix(exposurem)[,1:6]
ERS2m<-as.data.frame(exposurem%*%betam)
ERS_step<-na.omit(ERS2m)


compare<-as.data.frame(cbind(wqs_cor,wqs_step,ERS_corr,ERS_step))
colnames(compare)<-c('WQS-Corr','WQS-Stepwise','ERS-Corr','ERS-Stepwise')

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

png("scatterplot matrix new2.png", width = 17, height = 17, units = 'cm', res = 300)

pairs(compare,
      lower.panel=panel.smooth, upper.panel=panel.cor, diag.panel=panel.hist, 
      pch=20)
dev.off()