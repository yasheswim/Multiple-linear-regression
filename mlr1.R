##Toyota Corolla dataset##
install.packages("readr")
library(readr)
mlr1<-read.csv(file.choose())
View(mlr1)
mlr1<-mlr1[-c(10:29)]
View(mlr1)
attach(mlr1)
summary(mlr1)
pairs(mlr1)
plot(mlr1)
cor(mlr1)
install.packages("car")
install.packages("corpcor")
library(car)
library(corpcor)
cor2pcor(cor(mlr1))
##Model Building##
toyota <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data=mlr1)
summary(toyota)
##cc and doors varaiables have probabilities more than 0.05##
corolla1<-lm(Price~cc)
summary(corolla1)
corolla2<-lm(Price~Doors)
summary(corolla2)
corolla3<-lm(Price~cc+Doors)
summary(corolla3)
install.packages("psych")
library(psych)
pairs.panels(mlr1)
influence.measures(toyota)
?influenceIndexPlot
influenceIndexPlot(toyota,id.n=3)
influencePlot(toyota,id.n=3)
##The 81st observation in the hat values is causing insignificance in probailities##
##Deleting the 81st observation##
toyota1<-lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data=mlr1[-81,])
summary(toyota1)
##After removing 81st observation, probability of cc becomes significant##
toyota2<-lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data=mlr1[-c(81,222,961),])
summary(toyota2)
##After removing the 222nd and 961st observation, we obtain significant values of cc as well as doors##
vif(toyota)
?avPlots
avPlots(lm(Price~HP+cc+Doors+Gears,))
##From the av plot anf vif observation, it is concluded that the doors variable should be removed##
final_model<-lm(Price~Age_08_04+KM+HP+cc+Gears+Quarterly_Tax+Weight,data=mlr1[-81,])
summary(final_model)
plot(final_model)

