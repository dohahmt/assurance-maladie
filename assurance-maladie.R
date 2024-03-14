rm(list=ls())
library(caTools)
library(tidyverse)
library(dplyr)
library(caret)
library(ggplot2)
library(lmtest)
library(car)
library(carData)
dataset<-read.csv("C:/Users/doha_/Downloads/insurance.csv")
view(dataset)
head(dataset)
tail(dataset)
summary(dataset)
#convertir les 2 variables sex et smoking en variables binaires (0 ou 1)
data1 <- dataset %>%
  mutate(smoking = if_else(smoker == "no",0,1)) %>%
  mutate(sex = if_else(sex == "male",1,0))
#le nouveau dataset sans la var qualitative region et les vars sex et smoking converties
new_data=data1[,c('charges','bmi','age', 'children','smoking','sex')]
plot(new_data)
hist(new_data$charges)
hist(new_data$bmi)
hist(new_data$age)
hist(new_data$children)
hist(new_data$sex)
hist(new_data$smoking)
#Vérification de la corrélation entre les variables explicatives et les charges
cor_bmi<-cor(new_data$charges,new_data$bmi)
print(cor_bmi)
#il y a une faible de correlation entre bmi et charges
cor_smoking<-cor(new_data$charges,new_data$smoking)
print(cor_smoking)
#il y a une forte correlation entre smoking et charges
cor_age<-cor(new_data$charges,new_data$age)
print(cor_age)
#il y a une faible correlation entre age et charges
cor_children<-cor(new_data$charges,new_data$children)
print(cor_children)
#il y n'a presque pas de correlation entre children et charges
cor_sex<-cor(new_data$charges,new_data$sex)
print(cor_sex)
#il n'y a presque pas de correlation entre sexe et charges

#effectuer la regression lineaire multiple
reg_lin1<-lm(charges ~ age+bmi+children+sex+smoking,data = new_data)
summary(reg_lin1)
#extraire les coefficients du modele de regression lineaire multiple  
coef_model1<-coef(reg_lin1)
print(coef_model1)
#trouver les intervalles de confiance du modele
int_conf1<-confint(reg_lin1)
print(int_conf1)

#effectuer la regression lineaire multiple sans la variable sex
reg_lin<-lm(charges ~ age+bmi+children+smoking,data = new_data)
summary(reg_lin)
#extraire les coefficients du modele de regression lineaire multiple  
coef_model<-coef(reg_lin)
print(coef_model)
#trouver les intervalles de confiance du modele
int_conf<-confint(reg_lin)
print(int_conf)
#extraire les valeurs ajustees du modele 
fit<-fitted(reg_lin)
print(fit)
#extraire les residus du modele
residus<-resid(reg_lin)
print(residus)
#visualisation des plots de la regression lineaire
par(mfrow=c(2,2))
plot(reg_lin)

#visualisation des residus
plot(residus,main="Résidus")
abline(h=0,col="red")

#previsions du modele
prevision <- data.frame(age=32,bmi=28.880,children=0,sex=1,smoking=0)
prev <- predict(reg_lin,prevision)
round(prev, digits=2)

#test de la colinearite des variables
colinearite<-vif(reg_lin)
print(colinearite)
#les valeus sont inferieures a 10, donc on a pas de probleme de de colinearite
#test de l'homoscedasticite
homoscedasti<-bptest(reg_lin)
print(homoscedasti)
#test de la normalite des residus
shapiro<-shapiro.test(reg_lin$residuals)
print(shapiro)
#test de l'independance
acf(resid(reg_lin), main="Autocorrélogramme des résidus")

#determiner les points leviers
alpha <- 0.05
n <- dim(new_data)[1]
print(n)
p <- 5
analyses <- data.frame(obs=1:n)
analyses$levier <- hat(model.matrix(reg_lin))
seuil_levier <- 2*p/n

# Visualisation des points leviers
ggplot(data=analyses,aes(x=obs,y=levier))+
  geom_bar(stat="identity",fill="steelblue")+
  geom_hline(yintercept=seuil_levier,col="red")+
  theme_minimal()+
  xlab("Observation")+
  ylab("Leviers")+
  scale_x_continuous(breaks=seq(0,n,by=5))

# Selectionner les points leviers
idl1 <- analyses$levier>seuil_levier
idl1
analyses$levier[idl1]
pts_leviers<-which(analyses$levier>seuil_levier)
print(pts_leviers)
#trouver la distance de cook
influence <- influence.measures(reg_lin)
names(influence)
colnames(influence$infmat)
analyses$dcook <- influence$infmat[,"cook.d"]  
#le seuil de la distance de Cook est de n-p donc:
seuil_dcook <- 4/(n-p) 
#visualisation de la distance de cook
ggplot(data=analyses,aes(x=obs,y=dcook))+
  geom_bar(stat="identity",fill="steelblue")+
  geom_hline(yintercept=seuil_dcook,col="red")+
  theme_minimal()+
  xlab("Observation")+
  ylab("Distance de cook")+
  scale_x_continuous(breaks=seq(0,n,by=5))
#Selectionner les points de cook
idl2 <- analyses$dcook>seuil_dcook
idl2
analyses$dcook[idl2]
pts_cook<-which(analyses$dcook>seuil_dcook)
print(pts_cook)

#on elimine les points influents et on reevalue le modele
data_without_outli<-new_data[-pts_cook,]
Train = data_without_outli[1:900,]
Test= data_without_outli[901:1338,]
reg_outlier <- lm(charges~age+bmi+children+smoking, data=Train)
summary(reg_outlier)
Prediction2 <- predict(reg_outlier,Test[,-6])
Prediction2_result<-as.data.frame(Prediction2)
print(Prediction2_result)
#anova avec un facteur
anovaa<-aov(charges ~ region,data = dataset)
summary(anovaa)
list(anovaa)
int_confidence<-confint(anovaa)
print(int_confidence)

