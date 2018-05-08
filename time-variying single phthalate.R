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

######## logistic regression ###########
or<-rep(0,10)
confint<-matrix(0,nrow=10,ncol=2)
IQR<-rep(0,10)
# mehp
require(lme4)
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata[,c(2,3,55,65,67)]
temp.dat <- na.omit(temp.dat)
temp.mod <- lmer(mehp ~ gat + sg + ( 1| id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
mod.twostage1 <- vector("list", 2)
temp.dat <- cbind(dat.mean, fitted.int = pred.int[, 1], weight=continousdata$weight)
temp.mod <- glm(preterm ~ fitted.int+age+race_cat_new+edu_cat_new, data = temp.dat, family=binomial(link="logit"))
mod.twostage1[[1]] <- temp.mod
or[1]<-exp(coef(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[2]
confint[1,]<-exp(confint(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[2,]
IQR[1]<-quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)


# mehhp
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(mehhp ~gat + sg + (1 | id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
mod.twostage1 <- vector("list", 2)
temp.dat <- cbind(dat.mean, fitted.int = pred.int[, 1], weight=continousdata$weight)
temp.mod <- glm(preterm ~ fitted.int+age+race_cat_new+edu_cat_new, data = temp.dat, family=binomial(link="logit"))
mod.twostage1[[1]] <- temp.mod
mod.twostage1
summary(temp.mod)
or[2]<-exp(coef(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[2]
confint[2,]<-exp(confint(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[2,]
IQR[2]<-quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)



# meohp
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(meohp ~gat + sg + (1 | id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
mod.twostage1 <- vector("list", 2)
temp.dat <- cbind(dat.mean, fitted.int = pred.int[, 1], weight=continousdata$weight)
temp.mod <- glm(preterm ~ fitted.int+age+race_cat_new+edu_cat_new, data = temp.dat, family=binomial(link="logit"))
mod.twostage1[[1]] <- temp.mod
mod.twostage1
summary(temp.mod)
or[3]<-exp(coef(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[2]
confint[3,]<-exp(confint(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[2,]
IQR[3]<-quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)



# mecpp
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(mecpp ~gat + sg + (1 | id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
mod.twostage1 <- vector("list", 2)
temp.dat <- cbind(dat.mean, fitted.int = pred.int[, 1], weight=continousdata$weight)
temp.mod <- glm(preterm ~ fitted.int+age+race_cat_new+edu_cat_new, data = temp.dat, family=binomial(link="logit"))
mod.twostage1[[1]] <- temp.mod
mod.twostage1
summary(temp.mod)
or[4]<-exp(coef(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[2]
confint[4,]<-exp(confint(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[2,]
IQR[4]<-quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)



#dehpsum
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(dehpsum ~gat + sg + (1 | id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
mod.twostage1 <- vector("list", 2)
temp.dat <- cbind(dat.mean, fitted.int = pred.int[, 1],  weight=continousdata$weight)
temp.mod <- glm(preterm ~ fitted.int+age+race_cat_new+edu_cat_new, data = temp.dat, family=binomial(link="logit"))
mod.twostage1[[1]] <- temp.mod
mod.twostage1
summary(temp.mod)
or[5]<-exp(coef(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[2]
confint[5,]<-exp(confint(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[2,]
IQR[5]<-quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)


#mbzp
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(mbzp ~gat + sg + (1 | id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
mod.twostage1 <- vector("list", 2)
temp.dat <- cbind(dat.mean, fitted.int = pred.int[, 1],  weight=continousdata$weight)
temp.mod <- glm(preterm ~ fitted.int+age+race_cat_new+edu_cat_new+insur_new, data = temp.dat, family=binomial(link="logit"))
mod.twostage1[[1]] <- temp.mod
mod.twostage1
summary(temp.mod)
or[6]<-exp(coef(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[2]
confint[6,]<-exp(confint(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[2,]
IQR[6]<-quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)


#mbp
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(mbp ~gat + sg + (1 | id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
mod.twostage1 <- vector("list", 2)
temp.dat <- cbind(dat.mean, fitted.int = pred.int[, 1], weight=continousdata$weight)
temp.mod <- glm(preterm ~ fitted.int+age+race_cat_new+edu_cat_new+insur_new, data = temp.dat, family=binomial(link="logit"))
mod.twostage1[[1]] <- temp.mod
mod.twostage1
summary(temp.mod)
or[7]<-exp(coef(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[2]
confint[7,]<-exp(confint(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[2,]
IQR[7]<-quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)


#mibp
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(mibp ~gat + sg + (1 | id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
mod.twostage1 <- vector("list", 2)
temp.dat <- cbind(dat.mean, fitted.int = pred.int[, 1],  weight=continousdata$weight)
temp.mod <- glm(preterm ~ fitted.int+age+race_cat_new+edu_cat_new+insur_new, data = temp.dat, family=binomial(link="logit"))
mod.twostage1[[1]] <- temp.mod
mod.twostage1
summary(temp.mod)
or[8]<-exp(coef(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[2]
confint[8,]<-exp(confint(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[2,]
IQR[8]<-quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)


#mep
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(mep ~gat + sg + (1| id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
mod.twostage1 <- vector("list", 2)
temp.dat <- cbind(dat.mean, fitted.int = pred.int[, 1], weight=continousdata$weight)
temp.mod <- glm(preterm ~ fitted.int+age+race_cat_new+edu_cat_new+insur_new, data = temp.dat, family=binomial(link="logit"))
mod.twostage1[[1]] <- temp.mod
mod.twostage1
summary(temp.mod)
or[9]<-exp(coef(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[2]
confint[9,]<-exp(confint(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[2,]
IQR[9]<-quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)



#mcpp
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(mcpp ~gat + sg + (1 | id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
mod.twostage1 <- vector("list", 2)
temp.dat <- cbind(dat.mean, fitted.int = pred.int[, 1], weight=continousdata$weight)
temp.mod <- glm(preterm ~ fitted.int+age+race_cat_new+edu_cat_new+insur_new, data = temp.dat, family=binomial(link="logit"))
mod.twostage1[[1]] <- temp.mod
mod.twostage1
summary(temp.mod)
or[10]<-exp(coef(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[2]
confint[10,]<-exp(confint(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[2,]
IQR[10]<-quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)





###### AFT model #######
dat.mean <- aggregate(longdata[, c(53,7:10,65,66)], list(id = longdata$id), mean)
dat.mean$race_cat_new <- as.factor(dat.mean$race_cat_new)
dat.mean$edu_cat_new <- as.factor(dat.mean$edu_cat_new)
changerate<-rep(0,10)
confint<-matrix(0,nrow=10,ncol=2)
IQR<-rep(0,10)
# mehp
require(lme4)
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata[,c(2,54,55,65,67)]
temp.dat <- na.omit(temp.dat)
temp.mod <- lmer(mehp ~ gat + sg + ( 1| id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
mod.twostage1 <- vector("list", 2)
temp.dat <- cbind(dat.mean, fitted.int = pred.int[, 1], weight=continousdata$weight)
temp.mod <- lm(log_final_ga ~ fitted.int+age+race_cat_new+edu_cat_new, data = temp.dat, weights = weight)
mod.twostage1[[1]] <- temp.mod
mod.twostage1
summary(temp.mod)
changerate[1]<-(exp(coef(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))-1)[2]
confint[1,]<-(exp(confint(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))-1)[2,]
IQR[1]<-quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)



# mehhp
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(mehhp ~gat + sg + (1 | id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
mod.twostage1 <- vector("list", 2)
temp.dat <- cbind(dat.mean, fitted.int = pred.int[, 1], weight=continousdata$weight)
temp.mod <- lm(log_final_ga ~ fitted.int+age+race_cat_new+edu_cat_new, data = temp.dat, weights = weight)
mod.twostage1[[1]] <- temp.mod
mod.twostage1
summary(temp.mod)
changerate[2]<-(exp(coef(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))-1)[2]
confint[2,]<-(exp(confint(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))-1)[2,]
IQR[2]<-quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)

# meohp
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(meohp ~gat + sg + (1 | id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
mod.twostage1 <- vector("list", 2)
temp.dat <- cbind(dat.mean, fitted.int = pred.int[, 1], weight=continousdata$weight)
temp.mod <- lm(log_final_ga ~ fitted.int+age+race_cat_new+edu_cat_new, data = temp.dat, weights = weight)
mod.twostage1[[1]] <- temp.mod
mod.twostage1
summary(temp.mod)
changerate[3]<-(exp(coef(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))-1)[2]
confint[3,]<-(exp(confint(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))-1)[2,]
IQR[3]<-quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)

# mecpp
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(mecpp ~gat + sg + (1 | id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
mod.twostage1 <- vector("list", 2)
temp.dat <- cbind(dat.mean, fitted.int = pred.int[, 1], weight=continousdata$weight)
temp.mod <- lm(log_final_ga ~ fitted.int+age+race_cat_new+edu_cat_new, data = temp.dat, weights = weight)
mod.twostage1[[1]] <- temp.mod
mod.twostage1
summary(temp.mod)
changerate[4]<-(exp(coef(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))-1)[2]
confint[4,]<-(exp(confint(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))-1)[2,]
IQR[4]<-quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)

#dehpsum
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(dehpsum ~gat + sg + (1 | id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
mod.twostage1 <- vector("list", 2)
temp.dat <- cbind(dat.mean, fitted.int = pred.int[, 1],  weight=continousdata$weight)
temp.mod <- lm(log_final_ga ~ fitted.int+age+race_cat_new+edu_cat_new, data = temp.dat, weights = weight)
mod.twostage1[[1]] <- temp.mod
mod.twostage1
summary(temp.mod)
changerate[5]<-(exp(coef(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))-1)[2]
confint[5,]<-(exp(confint(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))-1)[2,]
IQR[5]<-quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)

#mbzp
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(mbzp ~gat + sg + (1 | id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
mod.twostage1 <- vector("list", 2)
temp.dat <- cbind(dat.mean, fitted.int = pred.int[, 1],  weight=continousdata$weight)
temp.mod <- lm(log_final_ga ~ fitted.int+age+race_cat_new+edu_cat_new+insur_new, data = temp.dat, weights = weight)
mod.twostage1[[1]] <- temp.mod
mod.twostage1
summary(temp.mod)
changerate[6]<-(exp(coef(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))-1)[2]
confint[6,]<-(exp(confint(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))-1)[2,]
IQR[6]<-quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)

#mbp
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(mbp ~gat + sg + (1 | id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
mod.twostage1 <- vector("list", 2)
temp.dat <- cbind(dat.mean, fitted.int = pred.int[, 1], weight=continousdata$weight)
temp.mod <- lm(log_final_ga ~ fitted.int+age+race_cat_new+edu_cat_new+insur_new, data = temp.dat, weights = weight)
mod.twostage1[[1]] <- temp.mod
mod.twostage1
summary(temp.mod)
changerate[7]<-(exp(coef(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))-1)[2]
confint[7,]<-(exp(confint(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))-1)[2,]
IQR[7]<-quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)

#mibp
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(mibp ~gat + sg + (1 | id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
mod.twostage1 <- vector("list", 2)
temp.dat <- cbind(dat.mean, fitted.int = pred.int[, 1],  weight=continousdata$weight)
temp.mod <- lm(log_final_ga ~ fitted.int+age+race_cat_new+edu_cat_new+insur_new, data = temp.dat, weights = weight)
mod.twostage1[[1]] <- temp.mod
mod.twostage1
summary(temp.mod)
changerate[8]<-(exp(coef(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))-1)[2]
confint[8,]<-(exp(confint(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))-1)[2,]
IQR[8]<-quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)

#mep
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(mep ~gat + sg + (1| id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
mod.twostage1 <- vector("list", 2)
temp.dat <- cbind(dat.mean, fitted.int = pred.int[, 1], weight=continousdata$weight)
temp.mod <- lm(log_final_ga ~ fitted.int+age+race_cat_new+edu_cat_new+insur_new, data = temp.dat, weights = weight)
mod.twostage1[[1]] <- temp.mod
mod.twostage1
summary(temp.mod)
changerate[9]<-(exp(coef(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))-1)[2]
confint[9,]<-(exp(confint(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))-1)[2,]
IQR[9]<-quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)

#mcpp
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(mcpp ~gat + sg + (1 | id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
mod.twostage1 <- vector("list", 2)
temp.dat <- cbind(dat.mean, fitted.int = pred.int[, 1], weight=continousdata$weight)
temp.mod <- lm(log_final_ga ~ fitted.int+age+race_cat_new+edu_cat_new+insur_new, data = temp.dat, weights = weight)
mod.twostage1[[1]] <- temp.mod
mod.twostage1
summary(temp.mod)
changerate[10]<-(exp(coef(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))-1)[2]
confint[10,]<-(exp(confint(temp.mod)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))-1)[2,]
IQR[10]<-quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)


continousdata$birth<-1
## Cox
dat.mean <- aggregate(longdata[, c(6:10,65,66)], list(id = longdata$id), mean)
dat.mean$race_cat_new <- as.factor(dat.mean$race_cat_new)
dat.mean$edu_cat_new <- as.factor(dat.mean$edu_cat_new)
hr<-rep(0,10)
confint<-matrix(0,nrow=10,ncol=2)
IQR<-rep(0,10)
# mehp
require(lme4)
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(mehp ~gat + sg + (1 | id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
mod.twostage1 <- vector("list", 2)
temp.dat <- cbind(dat.mean, fitted.int = pred.int[, 1],  birth=continousdata$birth, weight=continousdata$weight)
SURV<-temp.dat
cut.points <- unique(SURV$final_ga[SURV$birth == 1])
library("survival")
model.1 <- coxph(Surv(final_ga, birth) ~ fitted.int+age+race_cat_new+edu_cat_new, data = SURV, weights = weight)
summary(model.1)
hr[1]<-exp(coef(model.1)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[1]
confint[1,]<-exp(confint(model.1)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[1,]
IQR[1]<-quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)



# mehhp
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(mehhp ~gat + sg + (1 | id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
mod.twostage1 <- vector("list", 2)
temp.dat <- cbind(dat.mean, fitted.int = pred.int[, 1],  birth=continousdata$birth, weight=continousdata$weight)
SURV<-temp.dat
cut.points <- unique(SURV$final_ga[SURV$birth == 1])
library("survival")
model.1 <- coxph(Surv(final_ga, birth) ~ fitted.int+age+race_cat_new+edu_cat_new, data = SURV, weights = weight)
summary(model.1)
hr[2]<-exp(coef(model.1)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[1]
confint[2,]<-exp(confint(model.1)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[1,]
IQR[2]<-quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)


# meohp
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(meohp ~gat + sg + (1 | id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
mod.twostage1 <- vector("list", 2)
temp.dat <- cbind(dat.mean, fitted.int = pred.int[, 1],  birth=continousdata$birth, weight=continousdata$weight)
SURV<-temp.dat
cut.points <- unique(SURV$final_ga[SURV$birth == 1])
library("survival")
model.1 <- coxph(Surv(final_ga, birth) ~ fitted.int+age+race_cat_new+edu_cat_new, data = SURV, weights = weight)
summary(model.1)
hr[3]<-exp(coef(model.1)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[1]
confint[3,]<-exp(confint(model.1)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[1,]
IQR[3]<-quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)


# mecpp
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(mecpp ~gat + sg + (1 | id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
mod.twostage1 <- vector("list", 2)
temp.dat <- cbind(dat.mean, fitted.int = pred.int[, 1],  birth=continousdata$birth, weight=continousdata$weight)
SURV<-temp.dat
cut.points <- unique(SURV$final_ga[SURV$birth == 1])
library("survival")
model.1 <- coxph(Surv(final_ga, birth) ~ fitted.int+age+race_cat_new+edu_cat_new, data = SURV, weights = weight)
summary(model.1)
hr[4]<-exp(coef(model.1)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[1]
confint[4,]<-exp(confint(model.1)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[1,]
IQR[4]<-quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)


#dehpsum
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(dehpsum ~gat + sg + (1 | id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
mod.twostage1 <- vector("list", 2)
temp.dat <- cbind(dat.mean, fitted.int = pred.int[, 1],  birth=continousdata$birth, weight=continousdata$weight)
SURV<-temp.dat
cut.points <- unique(SURV$final_ga[SURV$birth == 1])
library("survival")
model.1 <- coxph(Surv(final_ga, birth) ~ fitted.int+age+race_cat_new+edu_cat_new, data = SURV, weights = weight)
summary(model.1)
hr[5]<-exp(coef(model.1)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[1]
confint[5,]<-exp(confint(model.1)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[1,]
IQR[5]<-quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)


#mbzp
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(mbzp ~gat + sg + (1 | id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
mod.twostage1 <- vector("list", 2)
temp.dat <- cbind(dat.mean, fitted.int = pred.int[, 1],  birth=continousdata$birth, weight=continousdata$weight)
SURV<-temp.dat
cut.points <- unique(SURV$final_ga[SURV$birth == 1])
library("survival")
model.1 <- coxph(Surv(final_ga, birth) ~ fitted.int+age+race_cat_new+edu_cat_new+insur_new, data = SURV, weights = weight)
summary(model.1)
hr[6]<-exp(coef(model.1)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[1]
confint[6,]<-exp(confint(model.1)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[1,]
IQR[6]<-quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)


#mbp
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(mbp ~gat + sg + (1 | id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
mod.twostage1 <- vector("list", 2)
temp.dat <- cbind(dat.mean, fitted.int = pred.int[, 1],  birth=continousdata$birth, weight=continousdata$weight)
SURV<-temp.dat
cut.points <- unique(SURV$final_ga[SURV$birth == 1])
library("survival")
model.1 <- coxph(Surv(final_ga, birth) ~ fitted.int+age+race_cat_new+edu_cat_new+insur_new, data = SURV, weights = weight)
summary(model.1)
hr[7]<-exp(coef(model.1)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[1]
confint[7,]<-exp(confint(model.1)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[1,]
IQR[7]<-quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)


#mibp
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(mibp ~gat + sg + (1 | id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
mod.twostage1 <- vector("list", 2)
temp.dat <- cbind(dat.mean, fitted.int = pred.int[, 1],  birth=continousdata$birth, weight=continousdata$weight)
SURV<-temp.dat
cut.points <- unique(SURV$final_ga[SURV$birth == 1])
library("survival")
model.1 <- coxph(Surv(final_ga, birth) ~ fitted.int+age+race_cat_new+edu_cat_new+insur_new, data = SURV, weights = weight)
summary(model.1)
hr[8]<-exp(coef(model.1)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[1]
confint[8,]<-exp(confint(model.1)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[1,]
IQR[8]<-quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)


#mep
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(mep ~gat + sg + (1 | id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
mod.twostage1 <- vector("list", 2)
temp.dat <- cbind(dat.mean, fitted.int = pred.int[, 1],  birth=continousdata$birth, weight=continousdata$weight)
SURV<-temp.dat
cut.points <- unique(SURV$final_ga[SURV$birth == 1])
library("survival")
model.1 <- coxph(Surv(final_ga, birth) ~ fitted.int+age+race_cat_new+edu_cat_new+insur_new, data = SURV, weights = weight)
summary(model.1)
hr[9]<-exp(coef(model.1)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[1]
confint[9,]<-exp(confint(model.1)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[1,]
IQR[9]<-quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)


#mcpp
pred.int <- matrix(0, length(unique(longdata$id)), 2)
temp.dat <- longdata
temp.mod <- lmer(mcpp ~gat + sg + (1 | id), data = temp.dat)
pred.int[, 1] <- ranef(temp.mod)$id[, 1]
mod.twostage1 <- vector("list", 2)
temp.dat <- cbind(dat.mean, fitted.int = pred.int[, 1],  birth=continousdata$birth, weight=continousdata$weight)
SURV<-temp.dat
cut.points <- unique(SURV$final_ga[SURV$birth == 1])
library("survival")
model.1 <- coxph(Surv(final_ga, birth) ~ fitted.int+age+race_cat_new+edu_cat_new+insur_new, data = SURV, weights = weight)
summary(model.1)
hr[10]<-exp(coef(model.1)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[1]
confint[10,]<-exp(confint(model.1)*(quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)))[1,]
IQR[10]<-quantile(pred.int[, 1],0.75)-quantile(pred.int[, 1],0.25)

