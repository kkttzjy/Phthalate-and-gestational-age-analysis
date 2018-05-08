rawdata <-readRDS("rawdata.rds")
rawdata[,7:46]<-log(rawdata[,7:46])
continousdata<-rawdata
continousdata$final_ga<-(rawdata$final_ga)*7
continousdata$log_final_ga<-log(continousdata$final_ga)
continousdata$birth<-1
continousdata$gat1<-log((rawdata$gat1)*7)
continousdata$gat2<-log((rawdata$gat2)*7)
continousdata$gat3<-log((rawdata$gat3)*7)
continousdata$gat4<-log((rawdata$gat4)*7)
names(continousdata)[names(continousdata)=="gat1"] <- "gat_1"
names(continousdata)[names(continousdata)=="gat2"] <- "gat_2"
names(continousdata)[names(continousdata)=="gat3"] <- "gat_3"
names(continousdata)[names(continousdata)=="gat4"] <- "gat_4"


beta1<-c(0.101,
         0.609,
         -1.723,
         1.208,
         0.319,
         -0.076)
exposure1<-rawdata[, c(7,11,15,19,31,35)]
exposure1<-data.matrix(exposure1)
ERS_1<-exposure1%*%beta1
beta2<-c(0.535,
         0.205,
         -1.454,
         0.732,
         0.225,
         -0.019)
exposure2<-rawdata[, c(8,12,16,20,32,36)]
exposure2<-data.matrix(exposure2)
ERS_2<-exposure2%*%beta2
beta3<-c(0.118,
         0.418,
         -1.365,
         0.987,
         0.300,
         -0.130)
exposure3<-rawdata[, c(9,13,17,21,33,37)]
exposure3<-data.matrix(exposure3)
ERS_3<-exposure3%*%beta3
beta4<-c(0.487,
         0.879,
         -3.033,
         1.685,
         0.545,
         -0.295)
exposure4<-rawdata[, c(10,14,18,22,34,38)]
exposure4<-data.matrix(exposure4)
ERS_4<-exposure4%*%beta4
ERS<-cbind(ERS_1,ERS_2,ERS_3,ERS_4)

newdata <- cbind(ERS_1,ERS_2,ERS_3,ERS_4,continousdata)
## convert wide to long format
longdata <- reshape(newdata, 
                    varying = c(1:4,51:54,59:62,64:67), 
                    idvar="id", sep="_", timevar="order",
                    direction = "long")
longdata<-longdata[order(longdata$id),]
names(longdata)[names(longdata)=="order"] <- "time"

### logistic regression
dat.mean <- aggregate(longdata[, c(3,47:50,97,98)], list(id = longdata$id), mean)
dat.mean$race_cat_new <- as.factor(dat.mean$race_cat_new)
dat.mean$edu_cat_new <- as.factor(dat.mean$edu_cat_new)

require(lme4)
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(ERS ~gat + sg + (1 | id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
mod.twostage1 <- vector("list", 2)
temp.dat <- cbind(dat.mean, fitted.int = pred.int[, 1])
temp.mod <- glm(preterm ~ fitted.int+age+race_cat_new+edu_cat_new+insur_new, data = temp.dat, family=binomial(link="logit"))
mod.twostage1[[1]] <- temp.mod
mod.twostage1
summary(temp.mod)
exp(coef(temp.mod)*(quantile(temp.dat$fitted.int,0.75)-quantile(temp.dat$fitted.int,0.25)))
exp(confint(temp.mod)*(quantile(temp.dat$fitted.int,0.75)-quantile(temp.dat$fitted.int,0.25)))
quantile(temp.dat$fitted.int,0.75)-quantile(temp.dat$fitted.int,0.25)



### AFT
newdata <- cbind(ERS_1,ERS_2,ERS_3,ERS_4,continousdata)
## convert wide to long format
longdata <- reshape(newdata, 
                    varying = c(1:4,51:54,59:62,64:67), 
                    idvar="id", sep="_", timevar="order",
                    direction = "long")
longdata<-longdata[order(longdata$id),]
names(longdata)[names(longdata)=="order"] <- "time"

dat.mean <- aggregate(longdata[, c(93,47:50,97,98)], list(id = longdata$id), mean)
dat.mean$race_cat_new <- as.factor(dat.mean$race_cat_new)
dat.mean$edu_cat_new <- as.factor(dat.mean$edu_cat_new)

require(lme4)
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(ERS ~gat + sg + (1 | id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
mod.twostage1 <- vector("list", 2)
temp.dat <- cbind(dat.mean, fitted.int = pred.int[, 1], weight=continousdata$weight)
temp.mod <- lm(log_final_ga ~ fitted.int+age+race_cat_new+edu_cat_new+insur_new, data = temp.dat,weights = weight)
mod.twostage1[[1]] <- temp.mod
mod.twostage1
summary(temp.mod)
exp(coef(temp.mod)*(quantile(temp.dat$fitted.int,0.75)-quantile(temp.dat$fitted.int,0.25)))-1
exp(confint(temp.mod)*(quantile(temp.dat$fitted.int,0.75)-quantile(temp.dat$fitted.int,0.25)))-1


## Cox
newdata <- cbind(ERS_1,ERS_2,ERS_3,ERS_4,continousdata)
## convert wide to long format
longdata <- reshape(newdata, 
                    varying = c(1:4,51:54,59:62,64:67), 
                    idvar="id", sep="_", timevar="order",
                    direction = "long")
longdata<-longdata[order(longdata$id),]
names(longdata)[names(longdata)=="order"] <- "time"

dat.mean <- aggregate(longdata[, c(6,47:50,97,98)], list(id = longdata$id), mean)
dat.mean$race_cat_new <- as.factor(dat.mean$race_cat_new)
dat.mean$edu_cat_new <- as.factor(dat.mean$edu_cat_new)

require(lme4)
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(ERS ~gat + sg + (1 | id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
mod.twostage1 <- vector("list", 2)
temp.dat <- cbind(dat.mean, fitted.int = pred.int[, 1], birth=continousdata$birth, weight=continousdata$weight)
SURV<-temp.dat
cut.points <- unique(SURV$final_ga[SURV$birth == 1])
library("survival")
model.1 <- coxph(Surv(final_ga, birth) ~ fitted.int+age+race_cat_new+edu_cat_new+insur_new, data = SURV, weights = weight)
summary(model.1)
exp(coef(model.1)*(quantile(SURV$fitted.int,0.75)-quantile(SURV$fitted.int,0.25)))
exp(confint(model.1)*(quantile(SURV$fitted.int,0.75)-quantile(SURV$fitted.int,0.25)))

