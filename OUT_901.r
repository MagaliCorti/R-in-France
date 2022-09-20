
######################################## ANALYSE MULTIVARIE' ########################################

# export image en vecteur
# coller  sur power point -> dissocier -> modifier legende et autres aspects

library(ade4)

# telechearger données dan ade4 package
data("meaudret")
meaudret
meaudret$env

?meaudret

# analyse multivar
acp1 <- dudi.pca(meaudret$env)

# il me montre le graphique de pois de chacque axes
# combien axes je veout considerer?
# choisir nombre de axes:
# 1. on regarde les changements de pente <<<---
# 2. on regarde les valeur de pois

# on considère les deux premier axes
# You can reproduce this result non-interactively with: dudi.pca(df = meaudret$env, scannf = FALSE, nf = 2)
# scannf = FALSE -> je ne veut pas que on me montre l'histogramme des pois

# carte factorielle des colonnes -> var env
s.corcircle(acp1$co)

# carte factorielle des ligne -> sites
s.label(acp1$li)

# visializer ensemble
par(mfrow=c(2,1))
s.corcircle(acp1$co)
s.label(acp1$li)

# pour expliciter variabilitèe axplique pour chaqe axe
acp1$eig/sum(acp1$eig)


### interpretation du cercle de correlation et de la carte des  factoriels

# cercle
# variable proche correlée ensemble
# plus la fleshe est grande plus la variable explique la variabilitée du jeu de donnée

# carte
# represente individu statistique
# si proche sa veout dire qu'il sont similaire entre eux

# on correèle  cercle et table
# au_4, au_5, au_1 ...  vont avoir valeurs de Nitr elevés, et de Temp et Flow bas


# regrupper visuellement  les season en forme de classe
# design pour ass chaque echantillon a une season ou a un site
s.class(acp1$li, meaudret$design$season)
s.corcircle(acp1$co)

# regrupper visuellement  les season en forme de classe
s.class(acp1$li, meaudret$design$site)
s.corcircle(acp1$co)



# acp ni centré ni normé
# si le variable ont les meme unités -> ne  pas center et pas scale pour montrer tote la variabilitè
acp2 <- dudi.pca(df = meaudret$env, center = F, scale = F, scannf = FALSE, nf = 2)
par(mfrow=c(2,1))
s.corcircle(acp2$co)
s.label(acp2$li)



####### Analyse Between-Within #######


# variabilitè entre les site en eliminant l'effet de la variabilitè de la season (comme dans les model mixte)

acpSeason <- wca(acp1, meaudret$design$season)
par(mfrow=c(3,1))
s.corcircle(acpSeason$co)
s.class(acpSeason$li, meaudret$design$season)
s.class(acpSeason$li, meaudret$design$site)
acpSeason$ratio # % de variabilitè du jeu de donnée à lintereur des season lie a l'interation season x site 0.6277314

# comme se distribvue les variable env pour discriminesr les sites
# sites bien discriminé
# discrimation de ce qui est spatiale en eliminant l'effet temporal



# variabilitè entre les season en eliminant l'effet de la variabilitè des sites 
# tous les baricentre des sites sont sur zero

acpSite <- wca(acp1, meaudret$design$site)
par(mfrow=c(3,1))
s.corcircle(acpSite$co)
s.class(acpSite$li, meaudret$design$season)
s.class(acpSite$li, meaudret$design$site)
acpSite$ratio # var à l'interieur des sites liée a la season = 0.6194885

# flow ass au printemps avec la fonte des neige
# on été et automne on a un plus grande concentration des elemnts parce que on a mois de diluition







