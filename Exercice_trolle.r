# Exerxcice trolle


#######################################################
# 1. Anneaux dans la fleur

install.packages("dplyr")
library(dplyr)
library(vegan)


########### Question 1: 

# la forme de la fleur est inmpliquée dans la specialization, en attirant spécifiquement les chiastochètes ??
# est-ce que le numbre de visites est different du au type de pollinisateur et de traitement ??

# telecharger les donnée
load("/Users/magalicorti/Desktop/Savoie Mont Blanc/OUTI-901 Analyses de données/exercice_trolle/chiasto3.RData")


## Présenter graphique des données
library(ggplot2)

ggplot(chiasto3, aes(y=vis, x=ann, fill=poll)) + 
  geom_boxplot() + 
  labs(x="Type of treatment", y="Number of visits", fill="Type of pollinator") +
  scale_fill_discrete(labels=c(al="Alternative", ch="Chiastocheta"))

# aes = définit colonnes à utiliser dans le jeu de données
# geom = définit la maière de présenter les données
# fill = poll permet de colorer différemment en fonction de la colonne poll


## Construction modèle linéaire
# Si on veut prendre en compte interactions entre les variables explicatives, utiliser le * et non le +
mod1 <-lm(formula=vis~poll*ann, data = chiasto3)
summary(mod1)
# Intercept = premier niveau de chacun des deux variables explicatives = valeur prédite par le modèle 
# interaction significative entre type de pollinisateur et type de traitement 

hist(resid(mod1), main="", xlab="", breaks = 20) 
shapiro.test(resid(mod1))
# les residues ne sont pas distribué normalment, hypotèse de normalité non respectéè

## test anova d'analyse des variances 
aov1 <- anova(mod1)
aov1

# R square du modéle = variance expliquè par lo modéle
1-aov1[4,2]/sum(aov1[,2]) # 0.3273748 = 32.7%
# Residuals = cet que n'est pas expliqué
# Rsquare = R^2 du modèle = explained variation / total variation = 1 - (unexplained variation/total variation) = 1 - (sum sq of the residuals/total sum of the sum sq)

# proportion de variance expliquéè par l'interaction
aov1[3,2]/sum(aov1[,2]) # 0.09879011 = 9.9%
# La probabilité d’obtenir cette proportions de variance (ou plus) par hasard est d’environ très très basse -> Pr(>F) de ligne poll:ann très significatif
# on juge que la différence entre la visites des pollinisateurs dépend significativement du traitement 


########### Question 2:

# quelle est l'influences des étage sur les visites ?? 
# (pas les meme communautée de poll das étages differents)

# visualization graphique
ggplot(chiasto3, aes(y=vis, x=etage, fill=poll)) + 
  geom_boxplot() + 
  labs(x="Stage", y="Number of visits", fill="Type of pollinator") +
  scale_fill_discrete(labels=c(al="Alternative", ch="Chiastocheta"))

## construction du modèle linéaire
mod2 <-lm(formula=vis~poll*etage, data = chiasto3)
summary(mod2)
# interaction significative entre type de pollinisateur et etage 
# apparament l'etage influence significativement le numbre de visites des chastoquètes

hist(resid(mod2), main="", xlab="", breaks = 20) 
# les residues semble distribué normalment, hypotèse de normalité respectéè

## test anova d'analyse des variances 
aov2 <- anova(mod2)
aov2

# R square du modéle
1-aov2[4,2]/sum(aov2[,2]) 
# le modéle explique le 0.3591594 = 36% de la variation

# proportion de variance expliquéè par l'interaction
aov2[3,2]/sum(aov2[,2]) 
# 0.1004015 = 10%
# La probabilité d’obtenir cette proportions de variance (ou plus) par hasard est d’environ très très basse -> Pr(>F) de ligne poll:etage très significatif
# on juge que la différence entre la visites des pollinisateurs dépend significativement de l'étage


########### Question 3:

# est-ce que la réponse à la question 1 dépend de l'étage ??
# est-ce que le fait que le numbre de visites est different du au type de pollinisateur et traitement depend de l'étage ??

# visualization graphique
ggplot(chiasto3, aes(y=vis, x=ann, fill=poll, color=etage)) + 
  geom_boxplot() + 
  labs(x="Stage", y="Number of visits", fill="Type of pollinator", color="Stage") +
  scale_fill_discrete(labels=c(al="Alternative", ch="Chiastocheta")) +
  scale_color_brewer(palette="Dark2")

## construction du modèle linéaire
mod3 <-lm(formula=vis~poll*ann*etage, data = chiasto3)
summary(mod3)
# interaction significative entre type de pollinisateur et etage 
# apparament l'etage influence significativement le numbre de visites des chastoquètes

hist(resid(mod3), main="", xlab="", breaks = 20) 
# les residues semble distribué normalment, hypotèse de normalité respectéè

## test anova d'analyse des variances 
aov3 <- anova(mod3)
aov3

# R square du modéle
1-aov3[8,2]/sum(aov3[,2]) 
# le modéle explique le 0.5185842 = 51.8% de la variation

# proportion de variance expliquéè par l'interaction des trois variable explicatives  
aov3[7,2]/sum(aov3[,2]) 
# 0.006588966 = 0,6%
# Pr(>F) de ligne poll:ann:etage pas significatif >> 0.05







########################################### Exerxcice trolle ###########################################



#######################################################
# 1. Anneaux dans la fleur

install.packages("dplyr")
library(dplyr)
library(vegan)


########### Question 1: 

# la forme de la fleur est inmpliquée dans la specialization, en attirant spécifiquement les chiastochètes ??
# est-ce que le numbre de visites est different du au type de pollinisateur et de traitement ??

# telecharger les donnée
load("/Users/magalicorti/Desktop/Savoie Mont Blanc/OUTI-901 Analyses de données/exercice_trolle/chiasto3.RData")


## Présenter graphique des données
library(ggplot2)

ggplot(chiasto3, aes(y=vis, x=ann, fill=poll)) + 
  geom_boxplot() + 
  labs(x="Type of treatment", y="Number of visits", fill="Type of pollinator") +
  scale_fill_discrete(labels=c(al="Alternative", ch="Chiastocheta"))

# aes = définit colonnes à utiliser dans le jeu de données
# geom = définit la maière de présenter les données
# fill = poll permet de colorer différemment en fonction de la colonne poll

# distribution asymmetryque, on aurà une que


## Construction modèle linéaire
# Si on veut prendre en compte interactions entre les variables explicatives, utiliser le * et non le +
mod1 <-lm(formula=vis~poll*ann, data = chiasto3)
summary(mod1)
# Intercept = premier niveau de chacun des deux variables explicatives = valeur prédite par le modèle 
# interaction significative entre type de pollinisateur et type de traitement 

hist(resid(mod1), main="", xlab="", breaks = 20) 
shapiro.test(resid(mod1))
# les residues ne sont pas distribué normalment, hypotèse de normalité non respectéè

## test anova d'analyse des variances 
aov1 <- anova(mod1)
aov1

# R square du modéle = variance expliquè par lo modéle
1-aov1[4,2]/sum(aov1[,2]) # 0.3273748 = 32.7%
# Residuals = cet que n'est pas expliqué
# Rsquare = R^2 du modèle = explained variation / total variation = 1 - (unexplained variation/total variation) = 1 - (sum sq of the residuals/total sum of the sum sq)

# proportion de variance expliquéè par l'interaction
aov1[3,2]/sum(aov1[,2]) # 0.09879011 = 9.9%
# La probabilité d’obtenir cette proportions de variance (ou plus) par hasard est d’environ très très basse -> Pr(>F) de ligne poll:ann très significatif
# on juge que la différence entre la visites des pollinisateurs dépend significativement du traitement 


########### Question 2:

# quelle est l'influences des étage sur les visites ?? 
# (pas les meme communautée de poll das étages differents)

# visualization graphique
ggplot(chiasto3, aes(y=vis, x=etage, fill=poll)) + 
  geom_boxplot() + 
  labs(x="Stage", y="Number of visits", fill="Type of pollinator") +
  scale_fill_discrete(labels=c(al="Alternative", ch="Chiastocheta"))

# pour mantenir première type de visualizzation et ajouter l'info de l'étage
# on visualizze 2 colonne,  1 etage et pour chaqun on visualize le nombre de visites pour type de traitementet et de pollinizateur
ggplot(chiasto3, aes(y=vis, x=ann, fill=poll)) + 
  geom_boxplot() + 
  labs(x="Type of treatment", y="Number of visits", fill="Type of pollinator") +
  scale_fill_discrete(labels=c(al="Alternative", ch="Chiastocheta")) +
  facet_grid(.~etage)


## construction du modèle linéaire
mod2 <-lm(formula=vis~poll*etage, data = chiasto3)
summary(mod2)
# interaction significative entre type de pollinisateur et etage 
# apparament l'etage influence significativement le numbre de visites des chastoquètes

hist(resid(mod2), main="", xlab="", breaks = 20) 
# les residues semble distribué normalment, hypotèse de normalité respectéè

## test anova d'analyse des variances 
aov2 <- anova(mod2)
aov2

# R square du modéle
1-aov2[4,2]/sum(aov2[,2]) 
# le modéle explique le 0.3591594 = 36% de la variation

# proportion de variance expliquéè par l'interaction
aov2[3,2]/sum(aov2[,2]) 
# 0.1004015 = 10%
# La probabilité d’obtenir cette proportions de variance (ou plus) par hasard est d’environ très très basse -> Pr(>F) de ligne poll:etage très significatif
# on juge que la différence entre la visites des pollinisateurs dépend significativement de l'étage


########### Question 3:

# est-ce que la réponse à la question 1 dépend de l'étage ??
# est-ce que le fait que le numbre de visites est different du au type de pollinisateur et traitement depend de l'étage ??

# visualization graphique
ggplot(chiasto3, aes(y=vis, x=ann, fill=poll, color=etage)) + 
  geom_boxplot() + 
  labs(x="Stage", y="Number of visits", fill="Type of pollinator", color="Stage") +
  scale_fill_discrete(labels=c(al="Alternative", ch="Chiastocheta")) +
  scale_color_brewer(palette="Dark2")

## construction du modèle linéaire
mod3 <-lm(formula=vis~poll*ann*etage, data = chiasto3)
summary(mod3)
# interaction significative entre type de pollinisateur et etage 
# apparament l'etage influence significativement le numbre de visites des chastoquètes

hist(resid(mod3), main="", xlab="", breaks = 20) 
# les residues semble distribué normalment, hypotèse de normalité respectéè

## test anova d'analyse des variances 
aov3 <- anova(mod3)
aov3

# R square du modéle
1-aov3[8,2]/sum(aov3[,2]) 
# le modéle explique le 0.5185842 = 51.8% de la variation

# proportion de variance expliquéè par l'interaction des trois variable explicatives  
aov3[7,2]/sum(aov3[,2]) 
# 0.006588966 = 0,6%
# Pr(>F) de ligne poll:ann:etage pas significatif >> 0.05





#######################################################
# 2. Poudre dans la fleur


load("/Users/magalicorti/Desktop/Savoie Mont Blanc/OUTI-901 Analyses de données/exercice_trolle/poudre.RData")


########### Question 1: 

# Les fleurs fermées exportent-elles plus de pollen que les fleurs ouvertes, indépendamment de la couleur de la poudre ?

# carp = 1 -> receveuse à recu poudre
# poud = couleur de la poudre = J/R

# dans le plot A la poudre jaune vien des émettrices ouvertes (A)
# dans le plot A la poudre rouge vien des émettrices fermées (T)


# filtering the data we are interested in
d1 <- data.frame(poudre$plot, poudre$carp, poudre$poud)
d1


(f1 <- poudre %>%
    group_by(poud) %>%
    summarize(carp)
)


rm(f1)








########################################### CORRECTION ###########################################


library(dplyr)
library(ggplot2)
library(lmerTest)
library(car)
library(MASS)
library(MuMIn)


# 1. visualization
# 2. confronter et cjoisir un modèle
# 3. frase conclusive


###### 1. visualization ######

# pour mantenir première type de visualizzation et ajouter l'info de l'étage
# on visualizze 2 colonne,  1 etage et pour chaqun on visualize le nombre de visites pour type de traitementet et de pollinizateur
ggplot(chiasto3, aes(y=vis, x=ann, fill=poll)) + 
  geom_boxplot() + 
  labs(x="Type of treatment", y="Number of visits", fill="Type of pollinator") +
  scale_fill_discrete(labels=c(al="Alternative", ch="Chiastocheta")) +
  facet_grid(.~etage)


###### 2. confronter et choisir un modèle ######

# moyennes

# la valeur de reference du model mixte dans notre cas est le A, ca serait mieux si la valeur de reference ca soit le T 
# transformer en facteur et preciser l'ordre des niveaux
chiasto3$ann <- factor(chiasto3$ann, levels = c("T", "A", "TA"))
d1 <- chiasto3 %>%
  filter(poll%in%c("ch"))

(s1 <- d1 %>%
    group_by(ann) %>%
    summarize(Moyenne=mean(vis)))

(s1[1,2]-s1[2,2])/s1[1,2] # Différence A et T en pourcentage

# En moyenne, les fleurs A sont visitées 1.76 fois par heure et
# les T et TA 7.56 et 6.6 fois respectivement par des chiasto
# T recoit en moyenne 330% de visite en plus que A par des chiasto

# coef de Variation
chiasto3 %>%
  group_by(poll, ann) %>%
  summarize(CV=sd(vis)/mean(vis))

# La coef de var est autour de 100% pour chaque traitement,
# la différence de 330% entre A et T est forte par rapport à la variabilité de 100%.
# Il y a donc peu de chances d’avoir obtenu ce résultat par hasard.

#modele LM
#Pour justifier notre impression par une mesure quantitative, nous allons
# maintenant savoir quelle est la probabilité d’obtenir par hasard une différence
# de 330% sous l’hypothèse nulle H0 : "Les deux traitements ont le même nb de visite par ch"

lm1 <- lm(data=d1, vis~ann)
summary(lm1)
par(mfrow=c(2,2));plot(lm1)
# 3 diff residues -> T A TA
# variance differentes pour les 3 traitement, ca ne va pas  


# les hp du modele lineare ne sont pas respectèe, on ne le peut pas utilizer
# on dois utilizer un modele mixte

# modéle mixte
# les donnée doivent etre independentes -> ce n'est pas le cas, diff popo d'insect diff densitée poll, petites diff dans les condition metéo -> effets aleatoirs
# 2 effet  aleatoires, et il sont emboité -> on doit le prendre en compte


# package for Fit Linear Mixed-Effects Models
library(lmerTest)


# function lmer pour tenir compte de l'effet aleatoir dans ce cas de l'emboitement de la pop dans le plot

lmm1 <- lmer(data=d1, vis~ann + (1|pop/plot))
summary(lmm1)
# reguarder effect aleatoir dovu au plot
# residues symm et centrè en 0
plot(lmm1)

# on a une grosse diff entre T et A, petite diff entre T et TA avec un petit effet negatif prob due au petite diff en forme du au anneau mais 

# Variance = quelle est la variance de l'intercept pour les diff plot all'interieur de la population


fitted(lmm1)

# on vois que la variance est differente

# vizualizer ditrib des données
hist(d1$vis)
# mod lineare ne va pas marché


# #hypotheses du modele
# pas independence: effet "plot"

# distribution normale résidus hypothèse de normalité)
res<-resid(lm1)
hist(res, breaks=50)
plot(res)



install.packages("MuMIn")
library(MuMIn)

r.squaredGLMM(lmm1)
# R2 mieux (0.11) mais tj faible
# La probabilité que la différence de 330% entre A et T soit liée au hasard
#est moins faible (p-val=0.008483  ) mais toujours faible. Je décide donc de rejeter H0:
# Les fleurs des deux traitements n'ont pas le même nb de visite par les ch


# on à 3 fois plus de visites entre T et A


# L'EFFET DU TRAITMENT DEPEND DE L'ETAGE??

# modele lieare mixte qui tiens en compte l'effect de l'etage
# je met l'etage en effect fixte et je reguarde l'iteraction 
lmm2 <- lmer(data=d1, vis~ann*etage + (1|pop/plot))
anova(lmm2) # anova test l'intercation entre les paramentres
summary(lmm2) # summary test chaque parametre independement

# ecart au montagnard est inferieur de l'ecart a subalpin
# entre montagnard et subalpin on a un ecart
# std  Error - marge d'erreur
# df - on a pal le memes degrees de liberté
# grosse diff df ass au plot, et df ass à la pop
# stat pas trop sigif si df tres bas
# t-value = estimate / std error


# T et Montagnard sont les parametre de reference
# on a un effet positif de l'etage sur les vsites (etageSA 8.9512)

# tableau ou je summarize les moyennes des differents etage en relation des traitments
d1 %>%
  group_by(etage, ann) %>%
  summarize(moy=mean(vis))

# la variance augmente avec les valeur predictes
# on peut solver le probleme en transformant tous en log

# modele ave donnèe en log
lmm3 <- lmer(data=d1, log(vis+1)~ann*etage + (1|pop/plot))
anova(lmm3) 
summary(lmm3)
plot(lmm3)
hist(resid(lmm3)) # distribution residues meilleure

# esseyaer avec racine carrèe
lmm4 <- lmer(data=d1, sqrt(vis)~ann*etage + (1|pop/plot))
anova(lmm4) 
summary(lmm4)
plot(lmm4)
hist(resid(lmm4))

# on a 2 diff resultats
anova(lmm2) 
anova(lmm3) 


###### 3. phrase conclusive ######

# la forme en globe de la fleur semble etra implique dan la specialization des pollinisateurs
# en effet le nombre de vusites des chiastoquette est significativement superieur dal le cas du  temoin et temoin-anneau en respec du traitement avec l'anneau qui determine l'ouverture de la fleure

# l'etage  semble influencer le nombre absolut des visites
# à l'etage montagnard il y a un nombre de visites inferieur que dans le subalpin
# cela peut dependre des differentes population et densitè de chiastoquettes presentes au differents etages

# il n'y a pas de interaction significative entre l'etage et le type de traitment operé sur la fleur
# le nombre de visites des chiastoquettes ne semble pas influencé par un interaction d'auquen type entre etage et type de traitment
# par contre il est influecé singulierment soit du traitment soit de l'etage






########################################### SUJET DE LA SEMAINE ###########################################

# on a de donnée de contage

# processus de Poisson
# loi de Poisson -> 1 seul  parametre lamda qui correspond soit a la moyenne soit a la variance

# quand on à des donnée de contage la variance augmente avec la moyenne

######  Modèle Poisson ###### 

#### Donnèe chiastoquettes


######  modele lineare generalizè ###### 

glm1 <- glmer(vis~ann*etage +(1|pop/plot), family = "poisson",
              data = subset(chiasto3, poll=="ch"))
plot(glm1)

library(car) 
Anova(glm1) # pour avoir les P-value
summary(glm1)
summary(residuals(glm1, type="pearson")) # par default il nous donne les residues  de deviance

# dans le  p-value le t dans Pr(>|t|) subst par z
# on cannais deja la variance des le depart
# il y a plu la colonne de df (dans la t student on a besoin des df, la non)
# z meme valeur de t de student mais ne suis pas une distribution de t student mais de normal
# vraiment impo c'est le signe de la valuere estmate et le p-value

# histogramme avec la distribution des visites
hist(d1$vis, breaks = 20)
# surposing poisson loi distribution
# lines(dpois(x=c(0,35, by 1), lambda=mean(d1$vis)), col="red")
# on vois que la distribution de poisson ne descris pas bien la distribution des données
# ca existe aussi la distribution applée zero-inflating Poisson (parce que dans nostre cas on a beaucoup de valeur a zero)


###### modele quasi-poisson ######

install.packages("MASS")
library(MASS)
mr4 <- glmmPQL(vis~ann*etage, random= ~1|pop/plot, family="quasipoisson",
               data=subset(chiasto3, poll=="ch"))
summary(mr4)
# l'effet de l'anneau ne depend pas de l'etage
# parce que on a toujor un effet positive de l'etage et negatif de l'anneau -> regarder p-value signif

Anova(mr4) # Analysis of Deviance Table (Type II tests)
# pas signif interct ann:etage


# modèle qui ne prend pas en compte l'interaction
mr5 <- glmmPQL(vis~ann+etage, random= ~1|pop/plot, family="quasipoisson",
               data=subset(chiasto3, poll=="ch"))
summary(mr5)
Anova(mr5) 

d1 %>%
  group_by(ann) %>%
  summarize(moy=mean(vis))
7.56/1.76 # 4.29

###### 3. phrase conclusive 1 ######

# La presence de l'anneau diminue le nombre de visites d'un facteur 4 (p<0.001)
# Le témoin controle (petit anneau) ne modifie pas  le nombre de  vusites (p=0.17).
# ou
# Nous  n'avon pas detecté d'effet du témoin controle, par  rapport au témoin (p=0.17)


d1 %>%
  group_by(etage) %>%
  summarize(moy=mean(vis))
9.87/3.44 # 2.87

summary(mr5)

# Au subalpin, les visites sont presque 3 fois plus importantes que au montagnard (p=0.028).


summary(mr4)

#  En revance  l'effet de l'anneau ne depend pas du milieu (p=0.22).



mr3 <- glmmPQL(vis~ann*etage+ann*poll, random= ~1|pop/plot, family="quasipoisson",
               data=chiasto3)
summary(mr3)
Anova(mr3)

library(emmeans)
pairs(emmeans(mr3, ~ann|poll))


pairs(emmeans(mr3, ~poll|etage))

# emmeans = Estimated marginal means (Least-squares means)
# Compute estimated marginal means (EMMs) for specified factors or factor combinations in a linear model; and optionally, comparisons or contrasts among them. 
# EMMs are also known as least-squares means.

# on fait la comparaison 2 a 2






###### summary des modele esseyé ######

# ann*etage

# lmm sans log:             interction        incorrect, dissacord
# lmm avec log (ou sqrt):   X
# glmm poisson:             X                 problème de surdispersion
# glmm quasi-poisson:       X

# il faut choisir un modèle qui est en accord avec les autres
# modèle avec mois de problèmes








######## 22/09/2022 ########


###### phrase conclusive 2 ######


pairs(emmeans(mr3, ~ann|poll))

#### question 1

#  le traitment A impacte les visites des insectes  de maniere differente selon leur nature (chiastoquette ou poll alternatif) (p<0.00, cf ligne 4 de Anova mr3)
# en effet pour les chiastoquette l'anneau diminue les visites d'un facteur 4(p<0.0001),
# tandis que pour les pollinizateur alternatifs, l'anneau augmente les visites d'un facteur 8 (p<0.0001)
# dans les 2 cas le traitement de TA ne modifie pas les visites par rapport a T (p=0.43 et 0.75 respectivement)


mr6 <- glmmPQL(vis~ann*poll+etage*poll, random= ~1|pop/plot, family="quasipoisson",
               data=chiasto3)

# estimation de 3 parametre -> 2 parametre supplementaire au modele nulle
# test multiple ->  plus haut nombre de test on fait plus de probabilitè d'avoir par hazard le resultat significatif
# on a besoin d'un test de tukey parce que 3>2


#### question 2

pairs(emmeans(mr3, ~poll|etage))

chiasto3 %>%
  group_by(etage,poll) %>%
  summarize(moy=mean(vis))

Anova(mr3)

#  par ailleur , l'ecart entre le nombre de visites de chiastochetes et d'autres polinizateurs depend de l'etage (p<0.0001, ligne 6 de Anova mr6)
# au montagnard les chiastochetes vistent 3 fois plus que les autres (p<0.0001)
#  tandis que au subalpin elle visitent 50 fois plus (p<0.0001)



#### question 3
  
# en revanche l'effet du traitemnet (T,TA, A) sur les visites ne depend pas de l'etage (p=0.48)






########################################### CORRECTION POUDRE ###########################################


########### visialization ###########
ggplot(poudre, aes (x = poud ,  fill=carp ))+
  geom_bar()




# au montagnard l'export depollen ca a pas trop bien marché parce que il y a moin de visites
# dans tous le cas (J ou R) les fleure ouvertes export beaucoup moins de poudre



# calcule des proportions
# proportion des fleurs  receveuses qui ont recu de la  podre -> moyenne pour chaque group
prop <- group_by(poudre, poud, trai, etage) %>% 
  summarize(prop=mean(carp))


prop1 <- group_by(poudre, trai) %>% 
  summarize(prop=mean(carp))



# esseyer avec chi-test --> pas optimal

poudre %>%
  group_by(etage, trai) %>%
  summarize(somme=sum(carp))


#table de contingence
conting <- poudre %>%
  group_by(trai) %>%
  summarize(succes=sum(carp), echecs=length(carp)-sum(carp))
chisq.test(conting[,-1])



########### modele avec 1 predicteur ###########


glm0 <- glmer(carp ~ (1|pop/plot), data = poudre, family = "binomial") # modele null avec que les effet aleatoires
glm1 <- glmer(carp ~ trai+ (1|pop/plot), data = poudre, family = "binomial")
summary(glm1)
plot(glm1) # residue pas top

# estimate positive
# p-value faible

# disperion par rapport  à la variabilitée qu'il y a dans les données
# quand données très disp les parapmetre nous permet de faire des predictions pas precise et on a une grande deviance
# surdirpspersion quand deviance >> valeur residuelle
# ici on a sousdispersion deviance << df. residual -> dans un cas on a des valeur tres variable


AIC(glm0, glm1)
# diff entre le AICs de 69, on  considere très grosse diff (echelle esponentielle -> a partir de 25 grande  diff)
# valeurde AIC toutjour pour comnparer modele 
# il faut  avoir la meme structure aleatoire



# combien de valeur predites?
# 20  en  theorie
poudre %>%
  group_by(plot) %>%
  summarize(sum(carp)/length(carp))

 
#
poudre %>%
  group_by(trai) %>%
  summarize(p=sum(carp)/length(carp))

# on a le 15% de probabilitè d'avoir de la poudre qui viens  d'une fleeure fermée vs 2% ouverte
# on peut arriver à la meme conclusion utilisant la valeur Estimate de traitT dans les FIxed effects de summary(glm1) en utilisant la fonction lien


#
m_quasi1 <- glmmPQL(carp ~ trai,
                     random = ~1|pop/plot, data = poudre, family = "quasibinomial")
summary(m_quasi1)

# modele complete interaction des effects
mq1 <- glmmPQL(carp ~ trai*poud*etage,
                    random = ~1|pop/plot, data = poudre, family = "quasibinomial")
summary(mq1)
Anova(mq1)

# il n'y a pas d'influence de la couleur de la poudre


# modele avec que les effect prencipaux sans interaction
mq2 <- glmmPQL(carp ~ trai + etage + poud,
               random = ~1|pop/plot, data = poudre, family = "quasibinomial")
summary(mq2)
Anova(mq2)

###### phrase conclusive 3 ######

Anova(mq1)

# les fleure ouvertes exportent 7 fois plus de poudre que les fermées (15% contre 2%, p<0.0001)
# cet effet ne depend pas de l'etage (p=0.47) meme si l'xport de poudre est plus frequnt au subalpin que au montgnard (12% vs 2%, p=0.0001)

# la podre joune est exportée 2  fois  mieux (11 vs 6%, p=0.002), mais l'effet de l'anneau ne depend  pas de la couleur de la poudre (p=0.90)




############################ MODALITE' EVALUATION ############################

# using
library(rmarkdown)


# deposer 1 decembre sur moodle





 
