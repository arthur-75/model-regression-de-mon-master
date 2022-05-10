# Projet Régression linéaire multiple
# Anissa Goulif - Radwan SARMINI DET SATOUF - Sirine Gabriel


# Jeu de données 

dat <- read.table("bodyfat.txt", header=T)
fit1 <- lm(Bodyfat~Triceps+Thigh+Midarm, data=dat)
fit0 <- lm(Bodyfat~1, data=dat)
fit2 <- lm(Bodyfat~., data=dat)
str(dat)


# Question 2
pairs(dat)

#On voit des corrélations positives assez fortes entre :
# Triceps et Thigh
# Triceps et Bodyfat
# Thigh et Bodyfat
 
#Nous pouvons confirmer cela à l'aide du tableau donnant les corrélations :

cor(dat)

#Les autres graphes représentent des points dispersés de manière insignificante.



# Question 3

# fit1 
fit1["coefficients"]

summary(fit1)

#forme matricielle
v1=rep(1,20)
Tr=dat$Triceps
Th=dat$Thigh
Md=dat$Midarm
X=cbind(v1,Tr,Th,Md)

E=fit1$residuals

A=as.matrix(fit1$coefficients)

Y=X%*%A+E

#Y-dat$Bodyfat proche de 0 

#fit0
fit0["coefficients"]
X0=rep(1,20)
E0=fit0$residuals
Interc=fit0$coefficients
Y = X0*Interc + E
Y
#Y-dat$Bodyfat donne des résidus importants, la qualité du modèle est mauvaise

# Question 4

#L'équation de prédiction selon fit1 est :
# Bodyfat = 4.334*Triceps - 2.857*Thigh - 2.186 *Midarm + 117.085

#On implémente donc la fonction réalisant ces prédictions :

predictbodyfatfit1 <- function(Triceps,Thigh,Midarm){
  prediction <- (4.334*Triceps) - (2.857*Thigh) - (2.186 *Midarm) + 117.085
  print(prediction)



# Question 5
summary(fit1)

#la t value pour Thigh est donnée par Estimate/Std. Error de Thigh, la valeur manquante ici est donc :

coef(summary(fit1))["Thigh","Estimate"]/coef(summary(fit1))["Thigh","Std. Error"]


# Pr(>|t|) pour Triceps est donné par 2*pt(abs(t), ddl, lower.tail = FALSE), la valeur manquante est donc :

2*pt(abs(summary(fit1)$coefficients["Triceps","t value"]) , 16, lower.tail = FALSE)


# Question 6

vcov(fit1)


#Pour calculer la matrice  $(X^{t}X)^{−1}$ comme sur le site : 

#Tout d'abord on calcule Mean standard errors avec 4 degrés de liberté vu qu'ils ont 4 variables  :


MES= sum((fit1$fitted.values-dat$Bodyfat)^2)/(length(dat$Bodyfat)-4)
MES


#On divise la matrice de covariance par le MSE, on trouve :

vcov(fit1)/MES

#Voila donc la matrice $(X^{t}X)^{−1}$.

# Question 7

#Pour recalculer les valeurs fournies dans la colonne Std. Error de l'objet fit1, on calcule, pour chaque valeur, la racine carrée de chaque coefficient correspondant dans la matrice variance covariance de fit1 (en utilisant donc la fonction vcov).


sqrt(vcov(fit1)[1,1])
sqrt(vcov(fit1)[2,2])
sqrt(vcov(fit1)[3,3])
sqrt(vcov(fit1)[4,4])


#On retrouve bien les valeurs fournies dans la colonne std. error de l'objet fit1.

# Question 8

#Thigh semble jouer un rôle significatif car il y a un correlation positive significative entre Thigh et Bodyfat.


cor(dat$Thigh,dat$Bodyfat)

#De plus si on construit un modele de regression linéaire simple entre Bodyfat et Thigh : 

fitT <- lm(Bodyfat~Thigh, data=dat)
summary(fitT)


#On remarque que R-squared = 0.771 donc 77.1% de données de Thigh explique Bodyfat
#et avec p-value << 0.05 où on est amené à rejeter l'hypothèse d'indépendance.

#On en conclut que la variable Thigh joue un role significatif.


# Question 9

#On va tout d'abord utiliser la méthode stepwise pour voir quelles variables sont plus significatives par rapport à d'autres.
#Penchons nous en premier lieu sur la méthode ascendante de la méthode stepwise, c'est-à-dire qu'on part en premier lieu d'un modèle sans variable (ici fit0) pour y ajouter au fur et à mesure des variables qui diminuent le critère (ici l'AIC). La méthode s'arrête lorsque l'ajout d'une nouvelle variable ne diminue plus le critère (l'AIC ici).


step(fit0, scope=list(lower=fit0,upper=fit1),data=dat,direction="forward")


#Ici on remarque donc que le modèle s'arrête après l'ajout de thigh. R considère alors que l'ajout de triceps ou de midarm va faire augmenter l'AIC (donc que ces variables vont "détériorer" le modèle car moins significative).

#Testons maintenant la méthode déscendante. En effet, pour cette méthode on part d'un modèle modèle complet, donc un modèle avec toutes les variables (ici fit1) et R enlève au fur et à mesure la variable qui diminue le plus le critère AIC. Le programme s'arrête lorsqu'on ne diminue plus le critère.
                                                                                                                                                                                                                                                                                                                                           
step(fit1, scale = 0, data=dat,direction="backward")                                                                                                                                                                                               
                                                                                                                                                                                                                                                                                                                                           
#Encore une fois ici on en conclue que ni triceps ni midarm ne diminue le critère d'AIC significativement par rapport à thigh.

#Grace à ces deux méthodes que nous venons de réaliser, nous pouvons en conclure que triceps et midarm ne semblent pas jouer un rôle siginificatif.

# Question 10

## Le modèle

#On remarque, grace à la question précédente, que les modèle ayant le critère AIC le plus faible (donc le meilleur modèle au sens de l'AIC) sont les modèles avec uniquement Thigh comme variable (AIC = 38.71) et le modèle avec uniquement Triceps et Midarm (AIC = 39.34).
                                                                                                                                                                                                                                       
#Interessons-nous au deuxième modèle :
                                                                                                                                                                                                                                         
model.fiale =lm(formula = Bodyfat ~ Triceps + Midarm, data = dat)
summary(model.fiale)
#Les variables sont plutôt bien pour la P value et 76.1% de données de Triceps et Midarm explique Bodyfat avec 2.496 de residual standard error.
                                                                                                                                                                                                                                       
#Faisons les mêmes tests avec le premier modèle :
                                                                                                                                                                                                                                         
model.fiale2 =lm(formula = Bodyfat ~ Thigh, data = dat)
summary(model.fiale2)
#Les variables sont plutôt bien pour la P value et 75.83% de données de Thigh explique Bodyfat avec 2.51 de residual standard error. L'erreur résiduelle standard est donc plus élevée pour ce modèle là.

#On choisit donc de prendre le modèle avec les variables Triceps et Midarm ($R^2$ plus élevé et erreur résiduelle standard plus faible).

#Et pour valider notre modèle il faut que l'on teste le modèle, nous allons diviser notre jeu de données : 85% de données pour entraîner le modele et 15% pour tester le modèle.
                                                                                                                                                                                                                                       
## Premier test

x_train=dat[-c(2,11,19),]
fitFinal <- lm(Bodyfat~ I(Triceps)+ I(Midarm) , data=x_train)
x_test= dat[c(2,11,19),c(1,2,3)]
y_test= dat[c(2,11,19),4]
prdict_mod = predict(fitFinal,x_test)
MSE=sqrt(sum((prdict_mod- y_test)^2)/(length(dat$Bodyfat)-3))
MSE

## Deuxieme Test

x_train= dat[-c(3,11,20),]
fitFinal <- lm(Bodyfat~ I(Triceps)+ I(Midarm) , data=x_train)
x_test= dat[c(3,11,20),c(1,2,3)]
y_test= dat[c(3,11,20),4]
prdict_mod = predict(fitFinal,x_test)
MSE = sqrt(sum((prdict_mod- y_test)^2)/(length(dat$Bodyfat)-3))
MSE

#Après avoir testé et comparé plusieures combinaisons des variables pour construire le meilleur modèle, on confirme que Triceps et Midarm sont les meilleurs candidats pour exprimer Bodyfat.
                                                                                                                                                                                                                                      
                                                                                                                                                                                                                                       
                                                                                                                                                                                                                                       
                                                                                                                                                                                                                                       
                                                                                                                                                                                                                                       
