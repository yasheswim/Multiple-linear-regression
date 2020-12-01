install.packages("readr")
library(readr)
install.packages("dummy")
library(dummy)
startups<-read.csv(file.choose())
View(startups)
class(startups$State)
View(startups)
startups$copyOfState<-NA
startups$copyOfState[startups$State=="New York"]=1
startups$copyOfState[startups$State=="California"]=0
startups$copyOfState[startups$State=="Florida"]=2
View(startups$copyOfState)
startups<-startups[,-4]
install.packages("car")
library(car)
install.packages("corpcor")
library(corpcor)
cor2pcor(cor(startups))
##Model Building##
startup_50<- lm(Profit~R.D.Spend+Administration+Marketing.Spend,data=startups)
summary(startup_50)
##Admisnistration varaiables have probabilities more than 0.05##
startup1<-lm(Profit~Administration,data=startups)
summary(startup1)
install.packages("psych")
library(psych)
pairs.panels(startups)
influence.measures(startup_50)
?influenceIndexPlot
influenceIndexPlot(startup_50,id.n=5)
influencePlot(startup_50,id.n=5)
##The 50th observation in the hat values is causing insignificance in probailities##
##Deleting the 50th observation##
startup22<-lm(Profit~R.D.Spend+Administration+Marketing.Spend,data=startups[-50,])
summary(startup22)
startup222<-lm(Profit~R.D.Spend+Administration+Marketing.Spend,data=startups[-c(50,49),])
summary(startup222)
##After removing the 49th and 50th observation, we obtain significant values of administration##
vif(startup_50)
?avPlots
avPlots(lm(Profit~R.D.Spend+Administration+Marketing.Spend))
##From the av plot anf vif observation, it is concluded that the administration variable should be removed##
final_model2<-lm(Profit~R.D.Spend+Marketing.Spend,data=startups[-50,])
summary(final_model2)
plot(final_model2)
##Plotting different transformation models for better r squared value
##Exponential model
startup_new<-startups[,-2]
startup_new<-startup_new[-50,]
startup_dummy<-startup_new[,-4]
View(startup_new)
startup_exp<-lm(log(Profit)~R.D.Spend+Marketing.Spend,data=startup_new)
summary(startup_exp)
y1<-is.infinite
sum(is.na(startup_new$R.D.Spend))
startup_1<-subset(startup_new,R.D.Spend!=0.00)
startup_1<-subset(startup_new,Marketing.Spend!=0.00)
##Logarithamic model
startup_log<-lm(Profit~log(R.D.Spend)+log(Marketing.Spend),data=startup_1)
summary(startup_log)
##Quadratic model
startup_quad<-lm(log(Profit)~(R.D.Spend+I(R.D.Spend^2))+(Marketing.Spend+I(Marketing.Spend^2)),data=startup_new)
summary(startup_quad)
##Cubic model
startup_poly<-lm(log(Profit)~R.D.Spend+I(R.D.Spend^2)+I(R.D.Spend^3)+Marketing.Spend+I(Marketing.Spend^2)+I(Marketing.Spend^3),data=startup_new)
summary(startup_poly)
#After running al, the transformation models, we find
#that the best r squared value is obtained by applying 
#no transformation models
##R squared values for all transformation models
model<-c("No transformation","Exponential model","Logarathamic model","Quadratic model","Cubic polynomial model")
rsquared_values<-c(0.9611,0.8944,0.612,0.9342,0.9491)
Rsquared_table<-data.frame(model,rsquared_values)
View(Rsquared_table)
