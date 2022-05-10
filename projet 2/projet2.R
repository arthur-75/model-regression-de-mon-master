#telecharger les package
#install.packages("corrplot")
#install.packages("RColorBrewer")
#install.packages("caret")
#install.packages("MASS")
#install.packages("survey")
#install.packages("knitr")
#install.packages("ROCR")
library(corrplot) # pour correlation
library(RColorBrewer)  #pour graph correlation
library(caret) # Pour test et train 
library(MASS) # pour Stepwise 
library(survey) # wald test 
library(knitr)# pour tables 
library(ROCR) # performance de cutOff
#Question 1
#--------------------------------------------------------------------
#on donne un nom pour chaque variable
noms_variables <- c("status_account","duration","history","purpose",
                    "amount","savings","employment","installement_rate",
                    "status_personnal","other_debtors","residence_since",
                    "property","age","other_installment","housing",
                    "nb_credits","job","liable","telephone","foreign","class")

credit0 <- read.table("GermanCredit.txt", col.names = noms_variables)
credit0$class <- as.factor(credit0$class-1) # rendre les valures de class à 0, 1
credit <- subset(credit0, select=-c(foreign))#supprimer la variable foreign

any(!is.na(credit))# pas de manque de données

#changer de char à factor pour les variables qualitatives 
credit1<- lapply(credit, function(x) if(class(x) == "character") as.factor(x) else x)
credit1 <- data.frame(credit1) # rendre en data.frame
head(credit1,4) #pour avoir une idée
str(credit1$class)
#Classe est un variable qualitative binaire.
#C’est une variable à expliquer qui représente la daufât 
#de crédit soit un bon (Good) risque crédit ou un mauvais 
#risque de crédit (Bad). Et 0 est signifié Good et 1 est signifié Bad.
#"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

#Question 2
#--------------------------------------------------------------------
#Cette functinn va decrire les données et va faire une repésenation graphiques 
decr <- function(data){
  str(data)# bref des varibales 
  print("Les fréquences marginals : ")
  for (i in 1:dim(data)[2]){
    if (class(data[,i]) == "factor"){ #pour les variables qualitatives 
      tab =table(data[,i])#fréquences
      barplot(tab,col=1:length(tab),main=names(data)[i])
      
      print(names(data)[i]) # nom de variable
      print(prop.table(tab)) #fréquences marginals
      print("")
    }
    else { #pour les variables quantitatives
      boxplot(data[,i],main=names(data)[i])
    }
  }
  print("")
  summary(data) #bref statistique
 
}
decr(credit1) # function pour décrire le jeu de données

qunt=credit1[,c(2,5,8,11,13,16,18)]# que les variables quantitatives
cov(qunt)#covaraince et varaince  

#correlation
corrplot(cor(qunt), type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))
#"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


#question 3
#--------------------------------------------------------------------
modReg <- (glm(class ~ .,data = credit1,family = binomial(link='logit')))

#question 4
#--------------------------------------------------------------------
summary(modReg)
#-------------------------------------------------------------------

#Ensuit on veut savoir si les varaibles sont representives 
for(g in 1:dim(credit1)[2]){
  if (class(credit1[,g])=="factor"){
  print(names(credit1[g]))
  print(xtabs(~class+ credit1[,g],data=credit1)) #representive ?
  print('')}
}
#Il faut noter que certain valuers dans les variables :
#purpose, history, other_debtors job , other_installment
# ne sont pas tres repesentives par rapport aux autres variable 
#-------------------------------------------------------------------
# wald test 
t=1
while(t < 20){ 
print(regTermTest(modReg,names(credit1)[t]));  t=t+1 ; print('')}

#Un test de Wald est utilisé pour évaluer la signification statistique de 
#chaque coefficient dans le modèle et est calculé en prenant le rapport
#du carré du coefficient de régression au carré de l'erreur standard 
#du coefficient. L'idée est de tester l'hypothèse que le coefficient
#d'une variable indépendante dans le modèle est significativement différent
#de zéro. Si le test ne rejette pas l'hypothèse nulle, cela suggère que
#la suppression de la variable du modèle ne nuira pas substantiellement
#à l'ajustement de ce modèle.
#donc il est possible d'eliminer ces varaibles :
# employment - age residence_since - property - 
#housing - nb_credits- job - liable - telephone


#-------------------------------------------------------------------


# McFadden's Pseudo R^2 
ll.null <- modReg$null.deviance/-2
ll.propse<- modReg$deviance/-2
Pseudo_R2 <-  (ll.null-ll.propse )/ll.null
Pseudo_R2 # et donc tres faible juste 26% represent le modele
#-------------------------------------------------------------------

#P-value pour chi2 
Chi2_R2 <- 1- pchisq(2*(ll.propse-ll.null),df=length(modReg$coefficients)-1)
Chi2_R2 # 0 
#ca veut dire que p-value est tres petit et que 
# la valeur de R^2 n'est pas lié au hassard

#la relation entre la classe et les autres variables 
#n'est pas due au hasard, et la valeur R2, 
# 0.26 nous indique la taille de l'effet de cette relation


#-------------------------------------------------------------------


#Question 5



anova(modReg, test="Chisq")

       
namess<- c("gen.AIC","gen.R2","gen.Acc","BIC_gen",
           "setp.AIC","setp.R2","setp.Acc","BIC_setp",
           "AIC_BIC","R2_BIC","Acc_BIC","BIC_BIC",
           "anova.AIC","anova.R2","anova.Acc","BIC_anova")
#-----------------------------------------------------------------------------
Trying_all <- function(data1,methode,n.iter){
  namess<- c("gen.AIC","gen.R2","gen.Acc","BIC_gen",
                 "setp.AIC","setp.R2","setp.Acc","BIC_setp",
                 "AIC_BIC","R2_BIC","Acc_BIC","BIC_BIC",
                 "anova.AIC","anova.R2","anova.Acc","BIC_anova")
  data_sel<- data.frame(matrix(0,0,16))
  names(data_sel)<- namess
  #Loooop
  for (i in 1:n.iter){
    # tout d'abord Il est important de deviser les données en deux parties 
    # * Train 80% de données
    # * Test 20% de données
    #Pour bien evaluer le modele par rapport l'Accuracy;
    set.seed(123+i)
    train.samples=createDataPartition(data1$class,p =.8, list =F)#train 80% 
    train.data1<- data1[train.samples, ] # 80% 
    test.data1 <- data1[-train.samples, ] # 20%
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    #General Methode où il y a toutes les variables 
    model_gen <- glm(class ~., data = train.data1,
                     family = binomial(link=methode)) #modelisation 
    probabilities <- predict(model_gen,test.data1,
                             type = "response")#prediction le test 20%
    predicted.class <- ifelse(probabilities > 0.5, 1, 0)# Good ou Bad credit 
    R2_gen=(model_gen$null.deviance-model_gen$deviance)/model_gen$null.deviance #Pseudo  R2
    Acc_gen =mean(predicted.class==test.data1$class) #Accuracy
    AIC_gen=model_gen$aic #AIC
    BIC_gen= BIC(model_gen) #BIC
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    #Methode Stepwise minimiser AIC sur le modele : model_general
    model_setp<- stepAIC(model_gen, direction = "both",
                         trace = FALSE) #modelisation 
    probabilities <- predict(model_setp,test.data1,
                             type = "response")#prediction le test 20%
    predicted.class <- ifelse(probabilities > 0.5, 1, 0)# Good ou Bad credit 
    R2_setp=(model_setp$null.deviance-model_setp$deviance)/model_setp$null.deviance#Pseudo  R2
    Acc_setp=mean(predicted.class==test.data1$class)#Accuracy
    AIC_setp=model_setp$aic #AIC
    BIC_setp= BIC(model_setp)#BIC
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    #Methode Stepwise minimiser BIC sur le modele : model_general
    model.BIC <- stepAIC(model_gen, k = log(nrow(train.data1)),
                         trace = FALSE) #modelisation 
    probabilities <- predict(model.BIC,test.data1,
                  direction = "both", type = "response")#prediction le test 20%
    predicted.class <- ifelse(probabilities > 0.5, 1, 0)# Good ou Bad credit 
    R2_BIC=(model.BIC$null.deviance-model.BIC$deviance)/model.BIC$null.deviance #Pseudo  R2
    Acc_BIC= mean(predicted.class==test.data1$class)#Accuracy
    AIC_BIC=model.BIC$aic #AIC
    BIC_BIC= BIC(model.BIC)#BIC
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    #Methode Anova personnal  
    #Analyse apres utilser toute variables ont une Deviance plus elevé et que 
    #les variables signficants d'apres wald test
    model_anova <-(glm(class ~status_account+duration+history+purpose+savings
          +installement_rate+status_personnal+other_debtors+ other_installment,
          data = train.data1, family = binomial(link=methode))) #modelisation 
    probabilities <- predict(model_anova,test.data1,
                             type = "response")#prediction le test 20%
    predicted.class <- ifelse(probabilities > 0.5, 1, 0) # Good ou Bad credit 
    R2_anova=(model_anova$null.deviance-model_anova$deviance)/model_anova$null.deviance#Pseudo  R2
    Acc_anova= mean(predicted.class==test.data1$class)#Accuracy
    AIC_anova=model_anova$aic #AIC
    BIC_anova= BIC(model_anova)#BIC
    #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    #Mettre la data ensemeble 
    new.data <- data.frame(AIC_gen,R2_gen,Acc_gen,BIC_gen
                           ,AIC_setp,R2_setp,Acc_setp,BIC_setp,
                           AIC_BIC,R2_BIC,Acc_BIC,BIC_BIC,
                           AIC_anova,R2_anova,Acc_anova,BIC_anova)
    names(new.data)<-namess
    data_sel<-rbind(data_sel,new.data )
    }
  return(data_sel)
} 

#ICI pour activer la fonction de simulation de 100 fois avec les 3 link function
#------------------------------------------------------------------------------

# Monto Carlo de logit
data_sel.logit<-Trying_all(credit1,"logit",n.iter=100)#Attention ca durer 8 min
data_sel.logit
# mean  de logit 
mean_data_sel.logit 	= data.frame(lapply(data_sel.logit,mean))
#97.5% de logit 
Up_data_sel.logit	= data.frame(lapply(data_sel.logit,
                                      function(x) quantile(x,0.975)))
#2.5% de logit 
Low_data_sel.logit =data.frame(lapply(data_sel.logit,
                                      function(x) quantile(x,0.025)))
#----------------------------------------------------------------------`

# Monto Carlo de probit 
data_sel.probit<-Trying_all(credit1,"probit",n.iter=100)#Attention ca durer 8 min
data_sel.probit
#mean de probit 
mean_data_sel.probit 	= data.frame(lapply(data_sel.probit,mean))
#97.5% de probit 
Up_data_sel.probit	= data.frame(lapply(data_sel.probit,
                                       function(x) quantile(x,0.975)))
#2.5% de probit 
Low_data_sel.probit=data.frame(lapply(data_sel.probit,
                                      function(x) quantile(x,0.025)))
#----------------------------------------------------------------------

# Monto Carlo de cloglog
data_sel.cloglog<-Trying_all(credit1,"cloglog",n.iter=100)#Attention ca durer 8 min
data_sel.cloglog
# meanet de cloglog 
mean_data_sel.cloglog 	= data.frame(lapply(data_sel.cloglog,mean))
# 97.5% de cloglog 
Up_data_sel.cloglog 	= data.frame(lapply(data_sel.cloglog,
                                         function(x) quantile(x,0.975)))
#2.5% de cloglog 
Low_data_sel.cloglog =data.frame(lapply(data_sel.cloglog,
                                        function(x) quantile(x,0.025)))
#"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

#Comparison 

# mean de tous les link fonction ensemble 
data.mean=rbind(mean_data_sel.logit,mean_data_sel.probit,mean_data_sel.cloglog)
row.names(data.mean)<-c("logit","probit","cloglog") 
#premiere 8 valeurs (pour mieux voir)
knitr::kable(data.mean[1:8],digits=4,
  caption = "Mean pour les Modeles : general x 4, stepwise min AIC x 4")
#deuixeme 8 valeurs (pour mieux voir)
knitr::kable(data.mean[9:16],digits=4,
  caption ="Mean pour les Modeles :stepwise min BIC x 4 , perso Anova x 4 ")
#"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
#Quantile "97.5%" 
data.97.5=rbind(Up_data_sel.logit,Up_data_sel.probit ,Up_data_sel.cloglog)
row.names(data.97.5)<-c("logit","probit","cloglog")
knitr::kable(data.97.5[1:8],digits=4,
 caption = "Quantile 97.5% pour les Modeles : general x 4, stepwise min AIC x 4")
knitr::kable(data.97.5[9:16],digits=4,
 caption ="Quantile 97.5% pour les Modeles :stepwise min BIC x 4 , perso Anova x 4")
#"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
#Quantile "2.5%"
data.2.5=rbind(Low_data_sel.logit,Low_data_sel.probit ,Low_data_sel.cloglog)
row.names(data.2.5)<-c("logit","probit","cloglog")
knitr::kable(data.2.5[1:8],digits=4,
 caption = "Quantile 2.5% pour les Modeles : general x 4, stepwise min AIC x 4")
knitr::kable(data.2.5[9:16],digits=4,
 caption ="Quantile 2.5% pour les Modeles :stepwise min BIC x 4 , perso Anova x 4")
#"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""



#Nous avons décidé de utilser trois technique de link function sous forme de Quatre methodes
# - Technique logit 
# - Technique probit
# - Technique cloglog
#et dans chaque technique il y a Quatre methode pour modeliser
# - tous les variablesd'une manier géneral
# - criter stepwise pour minimiser l'AIC avec un direction forward and backward selection
# - criter stepwise pour minimiser la BIC 
# - et personle methode depdent d'Anova par selecationner toutes variables ont une Deviance plus de 15.
# Donc on a 12 methodes/technique de modeliseation à comparer pour choisir les varaibles 
# pour comparer on prende en compte 4 indeicaterures 
# * AIC 
# * Pseudo R2  
# * Accuracy
# * BIC
#Mais avant comparer nous sommes inspiré par la simulation de monte Carlo 
#nous avons décidé de simuler 100 fois d'une manier aleatoire 80% de données pour
# pour constrir le modele et utiliser 20% pour prédire 
# ensuite on prende le moyeen d'AIC, Pseudo R2 et Accuracy et BIC pour
#les 100 similations pour choisir la miellure methode 
# et technique tout. 



#Plot ##################################@
#Accurancy
plot(time(data_sel.logit$gen.Acc),data_sel.logit$gen.Acc,type="l",
     main="Comparison Accurancy modelisation Logite")
lines(time(data_sel.logit$gen.Acc),data_sel.logit$setp.Acc,type="l",col=2,lwd=.5)
lines(time(data_sel.logit$gen.Acc),data_sel.logit$Acc_BIC,type="l",col=3,lty=2)
lines(time(data_sel.logit$gen.Acc),data_sel.logit$anova.Acc,type="s",col=4,lty=3)
legend("topright",col = c(1,2,3,4),
       legend = c("General","Min AIC","Min BIC","Perso Anova"),lty =c(1,1,2,3))

#-----------------------------
#Comparison entre le modèle Step-min-AIC logit et le modèle Anova-perso- cloglog 
par(mfrow=c(2,2))
plot(time(data_sel.logit$gen.Acc),data_sel.logit$setp.Acc,type="l",
     main="Comparison Accurancy 100 iterations  ",ylab = "Accurancy",xlab = "")
lines(time(data_sel.logit$gen.Acc),data_sel.cloglog$anova.Acc,type="l",col=4,lty=2)
legend("bottomleft",col = c(1,4),
       legend = c("Min AIC Logite","Perso Anova cloglog"),lty =c(1,2))

plot(time(data_sel.logit$gen.Acc),data_sel.logit$setp.R2,type="l",
     main="Comparison R2 100 iterations ",ylab = "R2",xlab = "")
lines(time(data_sel.logit$gen.Acc),data_sel.cloglog$anova.R2,type="l",col=4,lty=2)
legend("topleft",col = c(1,4),
       legend = c("Min AIC Logite","Perso Anova cloglog"),lty =c(1,2))

plot(time(data_sel.logit$gen.Acc),data_sel.logit$setp.AIC,type="l",
     main="Comparison AIC 100 iterations ",ylab = "AIC",xlab = "")
lines(time(data_sel.logit$gen.Acc),data_sel.cloglog$anova.AIC,type="l",col=4,lty=2)
legend("bottomleft",col = c(1,4),
       legend = c("Min AIC Logite","Perso Anova cloglog"),lty =c(1,2))

plot(time(data_sel.logit$gen.Acc),data_sel.logit$BIC_setp,type="l",
     main="Comparison BIC 100 iterations ",ylab = "BIC",xlab = "")
lines(time(data_sel.logit$gen.Acc),data_sel.cloglog$BIC_anova,type="l",col=4,lty=2)
legend("topright",col = c(1,4),
       legend = c("Min AIC Logite","Perso Anova cloglog"),lty =c(1,2))


#----------------------------------------------------------
#modeles Finals



#modèle d'apres  Step-min_AIc logit
model_Step <- glm(formula = class ~ status_account + duration + history + purpose + 
                    amount + savings + employment + installement_rate + status_personnal + 
                    other_debtors + age + other_installment + housing, family = binomial(link = "logit"), 
                  data = credit1)
# modèle d'apres anova et Wald test 
model_Anova_perso <-glm(class ~status_account+duration+history+purpose+savings
                     +installement_rate+status_personnal+other_debtors+ other_installment,
                     data = credit1, family = binomial(link="cloglog"))
  
summary(model_Step)
summary(model_Anova_perso)

#On remarque l’AIC et Pseudo R2 sont un peu meilleurs pour 
#le modèle Step-min-AIC logit que le modèle Anova-perso- cloglog 
#et inversement pour l’ Accuracy et BIC. 

#Mais comme on ne veut pas que le modèle ne soit parafait que 
#pour ces données on va donner l’Accuracy un priorité 
#puis Pseudo R2 puis AIC et BIC

#Donc on va choisir le modèle Anova-perso 
#qui a une link fonction de type cloglog
model_Anova_perso


#----------------------------------------------------------

#----------------------------------------------------------
# Question 6


par(mfrow=c(1,1))
set.seed(132)
train.samples1=createDataPartition(credit1$class,p =.8, list =F)#train 80% 
train1<- credit1[train.samples1, ] # 80% 
test1 <- credit1[-train.samples1, ] # 20%

model_Anova_perso <-glm(class ~status_account+duration+history+purpose+savings
                        +installement_rate+status_personnal+other_debtors+ other_installment,
                        data = train1, family = binomial(link="cloglog"))


probabilities <- predict(model_Anova_perso,test1, type = "response")#prediction le test 20%

predicted.class <- ifelse(probabilities > 0.5, 1, 0)# Good ou Bad credit 
Acc_gen =mean(predicted.class==test1$class) #Accuracy
Acc_gen
pred.rocr <- prediction(probabilities, test1$class)
eval <- performance(pred.rocr, "acc")
plot(eval)
max <- which.max(slot(eval, "y.values")[[1]])
acc <- slot(eval, "y.values")[[1]][max] #y.values are accuracy 
#measures
cut <- slot(eval, "x.values")[[1]][max] #x.values are cutoff 
#measures
print(c(Accuracy = acc, Cutoff = cut))

#et donc pour mieux prdire il faut que p > 0.6361595
predicted.class <- ifelse(probabilities > 0.5173151 , 1, 0)
Acc_gen =mean(predicted.class==test1$class) #Accuracy
Acc_gen #p =0.5173151


