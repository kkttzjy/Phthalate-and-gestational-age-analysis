rawdata <-readRDS("rawdata.rds")
## transform response varibale
rawdata[,7:46]<-log(rawdata[7:46])
continousdata<-rawdata
continousdata$final_ga<-(rawdata$final_ga)*7
continousdata$log_final_ga<-log(continousdata$final_ga)
continousdata$gat1<-log((rawdata$gat1)*7)
continousdata$gat2<-log((rawdata$gat2)*7)
continousdata$gat3<-log((rawdata$gat3)*7)
continousdata$gat4<-log((rawdata$gat4)*7)
names(continousdata)[names(continousdata)=="gat1"] <- "gat_1"
names(continousdata)[names(continousdata)=="gat2"] <- "gat_2"
names(continousdata)[names(continousdata)=="gat3"] <- "gat_3"
names(continousdata)[names(continousdata)=="gat4"] <- "gat_4"

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

## convert wide to long format
longdata <- reshape(continousdata, 
                    varying = c(7:50,55:58,60:63), 
                    idvar="id", sep="_", timevar="order",
                    direction = "long")
longdata<-longdata[order(longdata$id),]
names(longdata)[names(longdata)=="order"] <- "time"


##Two stage mixed effects model
dat.mean <- aggregate(longdata[, c(3,7:10,65,66)], list(id = longdata$id), mean)
dat.mean$race_cat_new <- as.factor(dat.mean$race_cat_new)
dat.mean$edu_cat_new <- as.factor(dat.mean$edu_cat_new)


# mehp
require(lme4)
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata[,c(2,3,55,65,67)]
temp.dat <- na.omit(temp.dat)
temp.mod <- lmer(mehp ~ gat + sg + ( 1| id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
cor(mean_mehp, pred.int[, 1], method = "pearson")


# mehhp
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(mehhp ~gat + sg + (1 | id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
cor(mean_mehhp, pred.int[, 1], method = "pearson")


# meohp
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(meohp ~gat + sg + (1 | id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
cor(mean_meohp, pred.int[, 1], method = "pearson")



# mecpp
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(mecpp ~gat + sg + (1 | id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
cor(mean_mecpp, pred.int[, 1], method = "pearson")



#dehpsum
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(dehpsum ~gat + sg + (1 | id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
cor(mean_dehpsum, pred.int[, 1], method = "pearson")


#mbzp
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(mbzp ~gat + sg + (1 | id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
cor(mean_mbzp, pred.int[, 1], method = "pearson")


#mbp
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(mbp ~gat + sg + (1 | id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
cor(mean_mbp, pred.int[, 1], method = "pearson")


#mibp
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(mibp ~gat + sg + (1 | id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
cor(mean_mibp, pred.int[, 1], method = "pearson")


#mep
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(mep ~gat + sg + (1| id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
cor(mean_mep, pred.int[, 1], method = "pearson")


#mcpp
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(mcpp ~gat + sg + (1 | id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
cor(mean_mcpp, pred.int[, 1], method = "pearson")

