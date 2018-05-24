# Load data/packages
load("C:/Users/hirshgupta/Desktop/Data Science stuff/Courses/Coursera/Data/ames_train.RData")
load("C:/Users/hirshgupta/Desktop/Data Science stuff/Courses/Coursera/Data/ames_test.RData")
load("C:/Users/hirshgupta/Desktop/Data Science stuff/Courses/Coursera/Data/ames_validation.RData")

library(statsr)
library(dplyr)
library(BAS)
library(ggplot2)
library(MASS)
library(corrplot)

# Expoloratory data analysis
ames_train %>% 
  ggplot(aes(x=Year.Built, y=log(price))) + geom_point() +
  ggtitle("Log of House Prices vs Year Built") + theme(plot.title = element_text(hjust = 0.5,face = "bold"))

ames_train %>% 
  ggplot(aes(x=Year.Built, y=Overall.Cond)) + geom_smooth(se=F) +
  ggtitle("Overall Condition vs Year Built") + theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ames_train %>% 
  ggplot(aes(x=Year.Built, y=Overall.Qual)) + geom_smooth(se=F) +
  ggtitle("Overall Quality vs Year Built") + theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ames_train %>% 
  ggplot(aes(x=log(Lot.Area), y=log(price), color=Neighborhood)) + geom_point() +
  ggtitle("Log of House Prices vs Log of Lot Area") + theme(plot.title = element_text(face = "bold", hjust = 0.5))

ames_train.fac <- ames_train
ames_train.fac$Overall.Cond <- as.factor(as.numeric(ames_train.fac$Overall.Cond))
ames_train.fac$Overall.Qual <- as.factor(as.numeric(ames_train.fac$Overall.Qual))

ames_train.fac %>% 
  ggplot(aes(x=Overall.Cond, y=log(price))) + geom_boxplot()

ames_train %>% 
  group_by(Overall.Cond) %>% 
  summarise(count=n())

ames_train.fac %>% 
  ggplot(aes(x=Overall.Qual, y=log(price))) + geom_boxplot()

ames_train %>% 
  group_by(Overall.Qual) %>% 
  summarise(count = n())

# Initial model development
mod1 <- ames_train %>% 
  lm(formula = log(price) ~ log(Lot.Area) + Neighborhood + Bldg.Type + Overall.Cond + Overall.Qual + Year.Built + Bedroom.AbvGr + Sale.Condition)

summary(mod1)

mod2 <- ames_train %>% 
  lm(formula = log(price) ~ log(Lot.Area) + Neighborhood + Overall.Cond + Overall.Qual + Year.Built + Bedroom.AbvGr + Sale.Condition)

summary(mod2)

#compare variable selection methods
mod2.bas <- bas.lm(log(price) ~ log(Lot.Area) + Neighborhood + Overall.Cond + Overall.Qual + Year.Built + Bedroom.AbvGr + Sale.Condition, data = ames_train, prior = "AIC", modelprior = uniform())
summary(mod2.bas)
coefficients(mod2.bas)

mod2.aic <- stepAIC(mod2, k=2)

#residuals
plot(mod2.aic$residuals^2)
plot(mod2.aic, which = 2)

#calculate RMSE
rmse.aic.train <- sqrt(mean((ames_train$price - exp(mod2.aic$fitted.values))^2))
rmse.aic.train

ames_train2 <- ames_train[-428,]
mod2.new <- ames_train2 %>% 
  lm(formula = log(price) ~ log(Lot.Area) + Neighborhood + Overall.Cond + Overall.Qual + Year.Built + Bedroom.AbvGr + Sale.Condition)
summary(mod2.new)

mod2.aic.new <- stepAIC(mod2.new, k=2)
rmse.aic.train.new <- sqrt(mean((ames_train2$price - exp(mod2.aic.new$fitted.values))^2))
rmse.aic.train.new

#using model on test dataset
ames_test <- ames_test[!(ames_test$Neighborhood == "Landmrk"),]
pred.test <- predict(mod2.aic.new, ames_test)

rmse.pred.test <- sqrt(mean((ames_test$price - exp(pred.test))^2))
rmse.pred.test
rmse.aic.train.new
round((rmse.pred.test - rmse.aic.train.new) / rmse.aic.train.new,3)

# Developing final model
mod3 <- ames_train2 %>% 
  lm(formula = log(price) ~ log(Lot.Area) + Neighborhood + Overall.Cond + 
       Overall.Qual + Year.Built + Bedroom.AbvGr + Sale.Condition + 
       X1st.Flr.SF + X2nd.Flr.SF + Foundation + Bldg.Type + House.Style + 
       area)
summary(mod3)
stepAIC(mod3, k=2)

mod4 <- ames_train2 %>% 
  lm(formula = log(price) ~ log(Lot.Area) + Neighborhood + Overall.Cond + 
       Overall.Qual + Year.Built + Bedroom.AbvGr + Sale.Condition + 
       X1st.Flr.SF + Foundation + Bldg.Type + House.Style + 
       area)
summary(mod4)

train <- ames_train[,c("price", "Lot.Area", "Overall.Qual", "Overall.Cond", "Year.Built", "Bedroom.AbvGr", "Sale.Condition", "X1st.Flr.SF", "Foundation", "Bldg.Type", "area", "House.Style")]

pairs(train)
train.num <- select_if(train, is.numeric)
cor(x=train.num, y=train$area)

mod5 <- ames_train2 %>% 
  lm(formula = log(price) ~ log(Lot.Area) + Neighborhood + Overall.Cond + 
       Overall.Qual + Year.Built + Bedroom.AbvGr + Sale.Condition + 
       X1st.Flr.SF + Foundation + Bldg.Type + House.Style)
summary(mod5)
stepAIC(mod5)

mod6 <- ames_train2 %>% 
  lm(formula = log(price) ~ log(Lot.Area) + Neighborhood + Overall.Cond + 
       Overall.Qual + Year.Built + Sale.Condition + 
       X1st.Flr.SF + Foundation + Bldg.Type + House.Style)
summary(mod6)

train2 <- ames_train[,c("price", "Lot.Area", "Neighborhood", "Overall.Qual", "Overall.Cond", "Year.Built", "Sale.Condition", "X1st.Flr.SF", "Foundation", "Bldg.Type", "House.Style")]

pairs(train2)

mod7 <- ames_train2 %>% 
  lm(formula = log(price) ~ log(Lot.Area) + Neighborhood + Overall.Cond + 
       Overall.Qual + Year.Built + Sale.Condition + 
       X1st.Flr.SF + Foundation + Bldg.Type + House.Style + X1st.Flr.SF:Overall.Qual)
summary(mod7)
plot(mod7, which=4)

ames_train2 <- ames_train2[-310,] #remove outlier

final.model <- ames_train2 %>% 
  lm(formula = log(price) ~ log(Lot.Area) + Neighborhood + Overall.Cond + 
       Overall.Qual + Year.Built + Sale.Condition +
       X1st.Flr.SF + Foundation + Bldg.Type + House.Style + X1st.Flr.SF:Overall.Qual)
summary(final.model)
plot(final.model)

# Assess final model
ames_test2 <- ames_test[!(ames_test$Foundation == "Wood"),]
ames_test2 <- ames_test2[!(ames_test$House.Style == "2.5Fin"),]
pred.test <- predict(final.model, ames_test2)
resid.test <- exp(pred.test) - ames_test2$price

plot(x=exp(pred.test), y=resid.test)
plot(x=exp(pred.test), y=ames_test2$price)

exp(pred.test)[exp(pred.test) > 400000]

ames_test2.price <- na.omit(ames_test2$price)
pred.price <- na.omit(pred.test)
rmse.test.final <- sqrt(mean((ames_test2.price - exp(pred.price))^2))
rmse.test.final
rmse.pred.test

round((rmse.test.final - rmse.pred.test)/rmse.pred.test,4)

# Test final model on validation dataset
ames_validation <- ames_validation[!(ames_validation$Foundation == "Wood"),]
ames_validation <- ames_validation[!(ames_validation$House.Style == "2.5Fin"),]
pred.val <- predict(final.model, ames_validation, invterval = "prediction")
resid.val <- exp(pred.val) - ames_validation$price
rmse.resid.val <- sqrt(mean(resid.val^2))
rmse.resid.val

plot(x=exp(pred.val), y=ames_validation$price)
plot(x=exp(pred.val), y=resid.val)

pred.int <- exp(predict(final.model, ames_validation, interval = "prediction"))

coverage.prob <- mean(ames_validation$price > pred.int[,"lwr"] &
                        ames_validation$price < pred.int[,"upr"])
coverage.prob #96.6% of credible intervals contain the true price of the home




