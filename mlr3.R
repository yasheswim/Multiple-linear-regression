install.packages("readr")
library(readr)
install.packages("dummy")
library(dummy)
mlr3<-read.csv(file.choose())
View(mlr3)
mlr3<-mlr3[,-1]
mlr3$cds<-NA
mlr3$cds[mlr3$cd=="yes"]=1
mlr3$cds[mlr3$cd=="no"]=2
mlr3$multis<-NA
mlr3$multis[mlr3$multi=="yes"]=1
mlr3$multis[mlr3$multi=="no"]=2
mlr3$premiums<-NA
mlr3$premiums[mlr3$premium=="yes"]=1
mlr3$premiums[mlr3$premium=="no"]=2
mlr3_new<-mlr3[,-c(6,7,8)]
View(mlr3_new)
install.packages("car")
library(car)
install.packages("corpcor")
library(corpcor)
cor2pcor(cor(mlr3_new))
mlr32<-mlr3_new[complete.cases(mlr3_new),]
##Model Building##
comp<-lm(price~speed+hd+ram+screen+ads+trend+cds+multis+premiums,data=mlr3_new)
summary(comp)
install.packages("psych")
library(psych)
pairs.panels(mlr3_new)
influence.measures(comp)
influenceIndexPlot(comp,id.n=5)
influencePlot(comp,id.n=5)
##The 1441th and 1701th observation in the hat values is causing insignificance in probailities##
comp1<-lm(price~speed+hd+ram+screen+ads+trend+cds+multis+premiums,data=mlr3_new[-c(1441,1701),])
summary(comp1)
vif(comp)
avPlots(lm(price~speed+hd+ram+screen+ads+trend+cds+multis+premiums,data=mlr3_new))
comp2<-lm(price~speed+hd+ram+screen+ads+cds+multis+trend+premiums,data=mlr3_new[-c(1441,1701),])
summary(comp2)
##Exponential model
comp_exp<-lm(log(price)~speed+hd+ram+screen+ads+cds+multis+trend+premiums,data=mlr3_new[-c(1441,1701),])
summary(comp_exp)
##Logarathamic model
comp_log<-lm(price~log(speed)+log(hd)+log(ram)+log(screen)+log(ads)+log(cds)+log(multis)+log(trend)+log(premiums),data=mlr3_new[-c(1441,1701),])
summary(comp_log)
##Quadratic model
comp_quad<-lm(log(price)~speed+I(speed^2)+hd+I(hd^2)+ram+I(ram^2)+screen+I(screen^2)+ads+I(ads^2)+cds+I(cds^2)+multis+I(multis^2)+trend+I(trend^2)+premiums+I(premiums^2),data=mlr3_new[-c(1441,1701),])
summary(comp_quad)
##Cubic model
comp_poly<-lm(log(price)~speed+I(speed^2)+I(speed^3)+hd+I(hd^2)+I(hd^3)+ram+I(ram^2)+I(ram^3)+screen+I(screen^2)+I(screen^3)+ads+I(ads^2)+I(ads^3)+cds+I(cds^2)+I(cds^3)+multis+I(multis^2)+I(multis^3)+trend+I(trend^2)+I(trend^3)+premiums+I(premiums^2)+I(premiums^3),data=mlr3_new[-c(1441,1701),])
summary(comp_poly)
##The cubic model gives the best r squared value and thus we will condider the above poly cubic model##
comp_poly$coefficients
pred1<-comp_poly$fitted.values
View(pred1)
qqnorm(comp_poly$residuals)
?qqplot
