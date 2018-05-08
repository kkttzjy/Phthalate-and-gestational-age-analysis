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


beta1<-c(1.95E-01,
         -7.88E-02,
         1.84E-01,
         -1.24E-01,
         6.81E-02,
         6.49E-02)
exposure1<-rawdata[, c(19,27,31,35,39,43)]
exposure1<-data.matrix(exposure1)
ERS_1<-exposure1%*%beta1
beta2<-c(9.13E-02,
         -9.87E-02,
         2.00E-01,
         -2.25E-02,
         8.41E-02,
         -1.29E-01)
exposure2<-rawdata[, c(20,28,32,36,40,44)]
exposure2<-data.matrix(exposure2)
ERS_2<-exposure2%*%beta2
beta3<-c(2.18E-01,
         1.19E-01,
         1.74E-01,
         -2.34E-01,
         2.75E-02,
         -1.33E-03)
exposure3<-rawdata[, c(21,29,33,37,41,45)]
exposure3<-data.matrix(exposure3)
ERS_3<-exposure3%*%beta3
beta4<-c(9.09E-02,
         -6.22E-02,
         4.97E-01,
         -5.04E-01,
         6.22E-03,
         -1.97E-03)
exposure4<-rawdata[, c(22,30,34,38,42,46)]
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

### logistic
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
temp.mod <- lm(log_final_ga ~ fitted.int+age+race_cat_new+edu_cat_new+insur_new, data = temp.dat, weights = weight)
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

