library(e1071)
library(caTools)
## importation du dataset
setwd("C:/Users/Abderrahmane/Documents")
assu2<-read.csv2("assurance.csv", header = TRUE, sep = ";", dec=",")
summary(assu2)
dim(assu2)
assu2$code_usage<-factor(assu2$code_usage,level=c('A', 'C','D'), labels =c(0,1,2))
assu2$code_usage<-as.numeric(as.character(assu2$code_usage))
assu2$combustion<-factor(assu2$combustion,level=c('E','D'), labels =c(0,1))
assu2$combustion<-as.numeric(as.character(assu2$combustion))
nb_annee<-assu2$exercie-assu2$DMC
nb_annee<-((nb_annee-mean(nb_annee))/sd(nb_annee))
assu2$combustion<-((assu2$combustion-mean(assu2$combustion))/sd(assu2$combustion))
assu2$code_usage<-((assu2$code_usage-mean(assu2$code_usage))/sd(assu2$code_usage))
assu2$PF<-((assu2$PF-mean(assu2$PF))/sd(assu2$PF))
assu3<-data.frame(assu2,nb_annee)
summary(assu3)
########################

split = sample.split(assu3$montant_de_la_prime, SplitRatio = 0.8)
data_apprentissage = subset(assu3, split == TRUE)
data_test = subset(assu3, split == FALSE)

model1 <- lm(montant_de_la_prime ~ nb_annee + combustion + code_usage + PF, data=data_apprentissage)
summary(model1)
confint(model1)
#############

pred = predict(model1, newdata =data_test)
pred1 = predict(model1, newdata =data_test,interval = "confidence")
pred2 = predict(model1, newdata =data_test,interval = "prediction")


