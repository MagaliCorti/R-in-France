# EXERCICES R DE M1


############################################
# ANOVA A DEUX FACTEURS

# telechergher les donnéè
cn <- read.csv2("CN_Vercors.csv")
cn
head(cn) # return the first 6 lines

# Evaluate an Expression in a Data Environment
with(cn, table(site, comm, cotyl))
?with
# gives back a table with syntehtic results
# dans chaque site on a 5 parcelles, chaque site correspond à un type de communautè végétale, meme nombre de mesures pour les deux gr functionels
# au sein du meme siste  on retrouve chacun des 2 groupes fonctionels de a meme maniere


library(ggplot2)

### analyse graphique
# creating a boxplot to see the ratio of C:N per community type, devided into dicots and monocots
ggplot(cn, aes((y=plantCN), x=comm, fill=cotyl)) + 
  geom_boxplot() + 
  labs(x="Plant community type", y="Plant C:N ratio", fill="Plant \nfuntional group") +
  scale_fill_discrete(labels=c(D="Dicots", M="Monocots"))
# graphiquement on vois que l'écart entre les deux groupes fonctionels est fort entre les prairies sèches et faible dans les mégaphorbiaies

### creation du modèl lineaire
lm1 <- lm(data = cn, plantCN~cotyl*comm)
lm1
(aov1 <- anova(lm1))
aov1
# Analysis of Variance Table 

# R square du modéle = variance expliquè par lo modéle
1-aov1[4,2]/sum(aov1[,2]) # 0.6879419 = 69%
# Residuals = cet que n'est pas expliqué
# Rsquare = R^2 du modèle = explained variation / total variation = 1 - (unexplained variation/total variation) = 1 - (sum sq of the residuals/total sum of the sum sq)

# proportion de variance expliquéè par l'interaction
aov1[3,2]/sum(aov1[,2]) # 0.04917296 = 5%
# La probabilité d’obtenir cette proportions de variance (ou plus) par hasard est d’environ 0.0009 -> Pr(>F) = 0.0009122 significatif
# on juge que la différence entre dicotylédones et monocotylédones dépend effectivement de la communauté végétale.

# On pourrait se demander quelle est la proportion de variance expliquée par le facteur cotyl
aov1[1,2]/sum(aov1[,2]) # [1] 0.4093329
# Mais le problème, c’est qu’une partie de la variance expliquée par cotyl l’est à travers son interaction avec comm. 
# Lorsque deux facteurs interagissent (au sens statistique), on ne peut pas vraimment les séparer


#### les hypothèses du modèle

# 1. normalité des résidus
hist(resid(lm1), main="", xlab="") # distribution visuellement  semble normale

# 2. homogénéité des variances
plot(y=resid(lm1), x=fitted(lm1))  # La variance des résidus ne semble pas dépendre des valeurs prédites

# 3. independance des résidus
install.packages("lmerTest")
library(lmerTest)
?lmerTest
# Tests in Linear Mixed Effects Models
# The lmerTest package provides p-values in type I, II or III anova and summary tables for linear mixed models via Satterthwaite's degrees of freedom method
lmm1 <- lmer(data=cn, plantCN~cotyl*comm + (1|site/parcelle)) 
anova(lmm1)


#### différences 2  à 2

### entre gropes fonctionnels
# dans chaque communauté végétale, il existe une différence entre les deux groupes fonctionnels ??
install.packages("emmeans")
library(emmeans)
?emmeans
# Estimated marginal means (Least-squares means)
# Compute estimated marginal means (EMMs) for specified factors or factor combinations in a linear model; and optionally, comparisons or contrasts among them. EMMs are also known as least-squares means.
em1 <- emmeans(lm1, ~cotyl|comm) 
pairs(em1)
# on peut sans problème rejeter “H0 : il n’y a pas de différence entre groupes fonctionnels” dans tous les cas, même si dans les mégaphorbiaies (Forbs) la différence est plus faible et la p-value plus élevée (<<0.05)
?pairs
# Scatterplot Matrices: A matrix of scatterplots is produced

### entre communautés 
# pour chaque groupe fonctionnel, il existe des différences entre communautés végétales ?? 
# Dans ce cas, le nombre de comparaisons 2 à 2 étant plus grand que le nombre de communautés (6 > 4), on réalise un test de Tukey
em2 <- emmeans(lm1, ~comm|cotyl) 
pairs(em2)
pwpp(em2)
?pwpp
# Pairwise P-value plot: Constructs a plot of P values associated with pairwise comparisons of estimated marginal means.
# graphique permet de visualiser les p-values des comparaisons 2 à 2,
# pour les dicotylédones seules les prairies intensives sont jugées différentes de toutes les autres
# pour les monocotylédones ce sont les prairies sèches

### entre groupes fonctionnels et entre communautés 
# il existe des cas où, en comparant les communautés entre elles, le C:N de monocotylédones de certaines communautés peut ne pas être différent du C:N de dicotylédones d’autres communautés ??
em3 <- emmeans(lm1, ~comm*cotyl) 
pairs(em3)
pwpp(em3)
# les dicotylédones des prairies sèches ne sont pas jugées différentes des monocotylédones des mégaphorbiaies et des prairies intensives.






