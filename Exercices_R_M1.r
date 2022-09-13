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





##########################################################
# COMPARISON DES MOYENNES

install.packages("dplyr")
library(dplyr)
library(ggplot2)

# telecharger donnée
ortho <- read.csv2("masses_orthos.csv")
# visualization de 10 ligne au hazard
ortho %>%
  sample_n(size = 10)

#### les femelles de l'esp 1 ont-elle la meme masse que celle de l'esp 2 ??
# deux espèces, un site d'étude, que les femelles

# filtering the data we are interested in
d1 <- ortho %>%
  filter(sp%in%c("Gom_vag", "Euc_dec"), lieu=="Yenne", sex=="Female")
d1

# visualizer boxplot avec mass des  femelelles de 2 espèces
(bxp1 <- ggplot(d1, aes(y=mass, x=sp)) +
    geom_boxplot() +
    labs(x="Species", y="Mass (mg)") +
    theme_light() 
  )

# creating a data frame with the two spp and the corresponding mean
(s1 <- d1 %>%
    group_by(sp) %>%
    summarize(Moyenne=mean(mass))
  )

# calcule de la defference entre les moyennes
s1[1,2]-s1[2,2] # en absolu
(s1[1,2]-s1[2,2])/s1[1,2] # En pourcentage (combien l'espece 1 est plus grande en respect de la 2)
# espèce 1 est 4.9% plus grande que l’espèce 2

# cette différence est-elle suffisamment importante, compte-tenu de la variabilité à l’intérieur de chaque espèce ??
# on peut se faire une idée visuelle en regardant le boxplot bxp1

# table 2x2 with coefficent of variation for each spp
# coefficient of variation = CV = standar deviation (sd) / mean
d1 %>%
  group_by(sp) %>%
  summarize(CV=sd(mass)/mean(mass))
# Les deux coefficients de variation sont très proches, environ 21%
# ce qui est grand par rapport à l’écart des moyennes de 5%

# quelle est la probabilité d’obtenir par hasard une différence de 5% ??
# hypothèse nulle H0 : "Les deux espèces ont la même masse"
# Yik = μi + Eik
# μi sont les vraies moyennes des deux espèces
# H0 : μ1 = μ2
# Eik sont des variables aléatoire indépendantes et identiquement distribuées selon Eik ∼ N (0, σ2)

# Cela reflète les trois hypothèses du modèle:
#  1. Les résidus sont indépendants les uns des autres (hypothèse d’indépendance)
#  2. Ils suivent une distribution normale (hypothèse de normalité) 
#  3. Ils ont la même variance (hypothèse d’homoscédasticité)

# verification de l'hp 3 avec le test de Bartlett
bartlett.test(mass~sp, data = d1)
?bartlett.test
# Bartlett Test of Homogeneity of Variances
# Performs Bartlett's test of the null that the variances in each of the groups (samples) are the same
# we reject H0 if p-value < 0.05 --> not ythe case, p-value very high (0.847) -> cannot reject H0 that the variances are the same

## estimation du modéle lineaire
m1 <- lm(data = d1, log(mass)~sp)
m1

# check the normal ditribution of the residues (hp 2 de normalitè)
# visually
hist(resid(m1), main = "")
# with shapiro-wilk normality test
shapiro.test(resid(m1))
# The null hypothesis is that the data distribution is normal.
# The p-value is not significant, so the data are normally distributed.

# tester H0 par une analyse de variance appliquée au modéle
anova(m1)
# Pr(>F) = 0.4783
# il y a 48% de chances pour que la différence de moyenne de 3.375 (ou plus) entre les deux espèces soit dûe au hasard
# risque de faux positif étant très grand
# nous décidons de ne pas rejeter H0

# Nous n’avons pas détecté de différence de poids entre les femelles de Euchorthippus declivus et Gomphocerrippus vagans 
# (ANOVA, F1,37 = 0.55, p=0.46).






