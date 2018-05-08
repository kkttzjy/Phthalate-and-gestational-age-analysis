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
ERS_all<-exposurem%*%betam

### ERS-Corr
betam<-c(3.58E-01,
         1.81E-02,
         2.63E-01,
         -2.68E-01,
         7.22E-02,
         -6.69E-02)
exposurem<-rawdata[, c(108,110,111,112,113,114)]
exposurem<-data.matrix(exposurem)
ERS_corr<-exposurem%*%betam

### ERS-Stepwise
betam<-c(0.424,
         0.761,
         -2.289,
         1.357,
         0.317,
         -0.305)
exposurem<-rawdata[, c(105,106,107,108,111,112)]
exposurem<-data.matrix(exposurem)
ERS_stepwise<-exposurem%*%betam

### scatter correlation plot
compare<-as.data.frame(cbind(ERS_all, ERS_corr, ERS_stepwise))
colnames(compare)<-c('ERS-All','ERS-Corr','ERS-Stepwise')

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

png("scatterplot matrix ERS-All ERS-Corr ERS-Stepwise.png", width = 17, height = 17, units = 'cm', res = 300)

pairs(compare,
      lower.panel=panel.smooth, upper.panel=panel.cor, diag.panel=panel.hist, 
      pch=20)
dev.off()
