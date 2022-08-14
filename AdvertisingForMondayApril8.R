install.packages("ggplot2","GGally","caret","car","e1071")

library("e1071")
library("ggplot2")
library("GGally")
library("caret")
library("car")
Advertising <- read.csv("Advertising.csv")
names(Advertising)
#As them which variable they need to remove
#make sure they perform a summary statistic, and confirm that all the
# variables are numerical
#Remove it
Advertising <- Advertising[,-1]
#Perform a summary Statistics
summary(Advertising)
#If you want to remove missing value use na.omit.  If you want to find
#near zero variables use nearZero to find the index of these variables in the 
# dataframe and then remove them from the dataframe

# here is the ggally plot.  Now ask them to describe what they see.  Notice the 
# the relation between TV and Sales (non linear).  This relation means that 
# TV need to be transformed using square 
#You will notice that Newspaper distribution is skewed.  There are many ways of dealing
#with skewness, one way is to find a power transform that will make the distribution more
#symmetrical.  
ggpairs(Advertising)
#Build a full model
lm.fit <- lm(Sales~(TV+Radio+Newspaper)^2,data=Advertising)
summary(lm.fit)
#Rebuild the model with only the significant variables
lm.fit2 <- lm(Sales~TV+Radio+Newspaper+TV:Radio+TV:Newspaper,data=Advertising)
summary(lm.fit2)
#Compare the models.  You will notice no differences between the 2 models.  Hence 
# we can stay with the smaller model
anova(lm.fit,lm.fit2)
#Use BoxCox to find the power required to make variables distributions symmetrical
#Diagnosis
par(mfrow =c(2,2))
plot(lm.fit2, which=c(1,3))
qqPlot(lm.fit2)
influencePlot(lm.fit2)
#Only one observation that is influential 131
# Remove it.  What happens when you remove the observation
lm.fit3 <- lm(Sales~TV+Radio+Newspaper+TV:Radio+TV:Newspaper,data=Advertising[-131,])
summary(lm.fit3)

lm.fit4 <- lm(Sales~TV+Radio+TV:Radio,data=Advertising[-131,])
summary(lm.fit4)

par(mfrow =c(2,2))
plot(lm.fit4, which=c(1,3))
qqPlot(lm.fit4)
influencePlot(lm.fit4)

#Can we do better?
#Remove Newspaper
advert <- Advertising[,-3]
#We noticed that the relationship betwee TV and Sales was not linear 
#Which transformation can we do the make it linear
par(mfrow =c(2,2))
plot(Advertising$TV,Advertising$Sales)
plot(Advertising$TV,log(Advertising$Sales))
plot(log(Advertising$TV),Advertising$Sales)
plot(log(advert$TV),log(advert$Sales))
lm.fit <- lm(I(log(Sales))~ (I(log(TV))+Radio)^2,data=Advertising[-c(131,156),])
summary(lm.fit)
par(mfrow =c(2,2))
plot(lm.fit, which=c(1,3))
qqPlot(lm.fit)
#which observations are influential? (they are 9 and 156)
influencePlot(lm.fit)

#Here is the best model
lm.fit3 <- lm(I(Sales^0.6)~I(log(TV))+Radio+TV:Radio,data=Advertising[-c(131,156),])
summary(lm.fit3)
par(mfrow=c(2,2))
plot(lm.fit3,which=c(1,3))
qqPlot(lm.fit3,simulate=FALSE)
influencePlot(lm.fit3)
# at this point they can use their own number to make new prediction
result <- predict(lm.fit3,data=Advertising[-c(131,156),])
#See graphically how good is the model at matching
# the response variable


