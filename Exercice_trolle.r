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


