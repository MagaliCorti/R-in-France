# MINI PROJECT ECOMONT 2022


########## loading packages ##########

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(vegan)
library(ggvegan)
library(BiodiversityR)
library(car)
library(lmerTest)
library(lme4)
library(rsq)
library(MASS)


########## data loading ##########

CTM <- read.csv("Data/PARLAC_CTM.csv")

pigments <- read_excel(path = "Data/PARLAC-Pigments.xlsx")
pigments <- subset(pigments[1:54,])

LC_Fit <- read.table("Data/PARLAC_LC_Fit.txt", header=T)


########## data structuration ##########

##### LC_Fit

# selecting only Absolute protocol
LC_Fit <- filter(LC_Fit, Protocol == "Abs")

# subsetting
LC_Fit_alpha <- filter(LC_Fit, Param == "alpha")
LC_Fit_ETRm <- filter(LC_Fit, Param == "ETRmax")
LC_Fit_Ik <- filter(LC_Fit, Param == "Ik")

##### pigments

# changing column names
colnames(pigments) <- c("Project", "Name", "Date", "Sampling", "Treatment", "Replicate", "Chla", "Carot")


##### CTM

# erasing useless columns
CTM <- CTM[,-c(12,13)]

# creating a Name column in CTM
CTM <-  CTM %>% 
  mutate(Name = paste(Replicate, Sampling, sep = "_"))

# creting data frame with means values
Means <- CTM %>%
  group_by(Name) %>%
  summarize(Temp=mean(Temp), Cond=mean(Cond), Turb=mean(Turb), O2_sat=mean(O2_sat), DO.mg=mean(DO.mg))

# creating column Photosynthetic Coefficient
Coef_P <- CTM %>%
  group_by(Replicate) %>%
  summarize(Coef_P=PAR/max(PAR))
# binding to CTM df
CTM <- cbind(CTM, Coef_P[,2])

# selecting PAR at ~50cm
# selectting data in a range
PAR50 <- CTM$depth >= 0.49 & CTM$depth <= 0.51
# subsetting orginal df
PAR50sub <- subset(CTM, PAR50)
# computing means 
CoefM <- PAR50sub %>%
  group_by(Name) %>%
  summarize(Coef_P=mean(Coef_P))

# mertging df
CTM <- merge(CTM, Means, by="Name")
CTM <- merge(CTM, CoefM, by="Name")
# erasing useless columns
CTM <- CTM[,-c(6:13)]
# keeping just  one row (erasing repeated rows)  
CTM <- unique(CTM)

# cleanig environment
rm(PAR50)
rm(PAR50sub)
rm(Means)
rm(Coef_P)
rm(CoefM)

# renaming columns
colnames(CTM) <- c("Name","Date" , "Sampling", "Treatment", "Replicate","Temp", "Cond", "Turb", "O2_sat", "DO.mg", "Coef_P")




########## PCA ##########

#### pigments

# assigning to rows the name of the column Name
pigments <- as.data.frame(pigments)
rownames(pigments) <- pigments$Name
str(pigments)
head(pigments)

# running PCA
pigments_pca <- rda(pigments[,c(7:8)])
pigments_pca
summary(pigments_pca)
PCAsignificance(pigments_pca)

# plotting
ordiplot(pigments_pca, type = "t")

# ggplot
pig_fort <- fortify(pigments_pca, axes = 1:2)
ggplot() +
  geom_point(data = subset(pig_fort, Score =="sites"), # points representing site
             mapping = aes(x = PC1, y = PC2, colour = CTM$Treatment),
             alpha=0.5) + # alpha = transparency
  geom_segment(data = subset(pig_fort, Score =="species"), # arrows representing species
               mapping = aes(x = 0, y = 0, xend = PC1, yend = PC2), # setting beginning and ending points of arrows
               arrow = arrow(length = unit(0.03, "npc"),
                             type = "closed"),
               colour = "darkgray",
               size = 0.8) +
  geom_text(data = subset(pig_fort, Score =="species"),
            mapping = aes(label = Label, x = PC1 * 1.1, y = PC2 * 1.1)) +
  geom_abline(intercept = 0, slope = 0, linetype="dashed", size=0.8, colour="gray") + # adding coordinated and Origin
  geom_vline(aes(xintercept=0), linetype="dashed", size=0.8,  colour="gray") +
  xlab("PC1 (91%)") +
  ylab("PC2 (9%)") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color="black"))

# fitting env variables 
pig_fit <- envfit(pigments_pca, CTM[6:11])
pig_fit

scores(pig_fit, "vectors")

# adding fitted vectors to an ordination using plot command
plot(pigments_pca, display = "sites")
plot(pig_fit)



# sqrt transformation
pca2 <- rda(sqrt(pigments[,c(7:8)]))
pig_fit <- envfit(pca2, CTM[6:11])
pig_fit
# log transformation
pca2 <- rda(log(pigments[,c(7:8)]))
pig_fit <- envfit(pca2, CTM[6:11])
pig_fit
# log2 transformation
pca2 <- rda(log2(pigments[,c(7:8)]))
pig_fit <- envfit(pca2, CTM[6:11])
pig_fit
# log10 transformation
pca2 <- rda(log10(pigments[,c(7:8)]))
pig_fit <- envfit(pca2, CTM[6:11])
pig_fit





#### variable PAM



### alpha ~ CTM variables

# assigning to rows the name of the column Name
rownames(LC_Fit_alpha) <- LC_Fit_alpha$Name
al_pca <- LC_Fit_alpha[,8:9]

# removing NAs

CTM2 <- CTM
rownames(CTM2) <- CTM2$Name

# identify the location of NAs values in al_PCA dataset
index_na <- which(is.na(al_pca), arr.ind = T) # with the argument arr.ind = T array indices are returned
index_na
index_na <- index_na[, 1] # subetting the first column (containig the row numbers of NAs)

# remove NA values
al_pca <- na.omit(al_pca)  # omitting the rows containing NAs in al_pca
CTM2 <- CTM2[-index_na, ]      # omitting the corresponding rows in CTM2, using index_na
# now we can see the number of rows (observations) has changed and is equal to 52


# running PCA
alpha_pca <- rda(al_pca)
alpha_pca
summary(alpha_pca)
PCAsignificance(alpha_pca)

# plotting
ordiplot(alpha_pca, type = "t")

# fitting env variables 
a_fit <- envfit(alpha_pca, CTM2[,6:11], permu=999)
a_fit
scores(a_fit, "vectors")
# adding fitted vectors to an ordination using plot command
plot(alpha_pca, display = "sites")
plot(a_fit)


### alpha ~ pigments variables

pigments2 <- pigments
al_pca <- LC_Fit_alpha[,8:9]

# removing NAs
index_na <- which(is.na(al_pca), arr.ind = T) 
index_na
index_na <- index_na[, 1] 
al_pca <- na.omit(al_pca)  
pigments2 <- pigments2[-index_na, ]      

# fitting env variables 
a_fitp <- envfit(alpha_pca, pigments2[,7:8], permu=999)
a_fitp
scores(a_fitp, "vectors")
# adding fitted vectors to an ordination using plot command
plot(alpha_pca, display = "sites")
plot(a_fitp)


### ETRmax ~ CTM variables

# assigning to rows the name of the column Name
rownames(LC_Fit_ETRm) <- LC_Fit_ETRm$Name


# removing NAs

al_pca <- LC_Fit_ETRm[,8:9]
CTM2 <- CTM
rownames(CTM2) <- CTM2$Name

index_na <- which(is.na(al_pca), arr.ind = T) 
index_na
index_na <- index_na[, 1] 
al_pca <- na.omit(al_pca)  
CTM2 <- CTM2[-index_na, ]      

# running PCA
ETRm_pca <- rda(al_pca)
ETRm_pca
summary(ETRm_pca)
PCAsignificance(ETRm_pca)

# plotting
ordiplot(ETRm_pca, type = "t")

# fitting env variables 
e_fit <- envfit(ETRm_pca, CTM2[,6:11], permu=999)
e_fit
scores(e_fit, "vectors")
# adding fitted vectors to an ordination using plot command
plot(ETRm_pca, display = "sites")
plot(e_fit)


### ETRmax ~ pigments variables

pigments2 <- pigments
al_pca <- LC_Fit_ETRm[,8:9]

# removing NAs
index_na <- which(is.na(al_pca), arr.ind = T) 
index_na
index_na <- index_na[, 1] 
al_pca <- na.omit(al_pca)  
pigments2 <- pigments2[-index_na, ]      

# fitting env variables 
e_fitp <- envfit(ETRm_pca, pigments2[,7:8], permu=999)
e_fitp
scores(e_fitp, "vectors")
# adding fitted vectors to an ordination using plot command
plot(ETRm_pca, display = "sites")
plot(e_fitp)



### Ik ~ CTM variables

# assigning to rows the name of the column Name
rownames(LC_Fit_Ik) <- LC_Fit_Ik$Name


# removing NAs

al_pca <- LC_Fit_Ik[,8:9]
CTM2 <- CTM
rownames(CTM2) <- CTM2$Name

index_na <- which(is.na(al_pca), arr.ind = T) 
index_na
index_na <- index_na[, 1] 
al_pca <- na.omit(al_pca)  
CTM2 <- CTM2[-index_na, ]      

# running PCA
Ik_pca <- rda(al_pca)
Ik_pca
summary(Ik_pca)
PCAsignificance(Ik_pca)

# plotting
ordiplot(Ik_pca, type = "t")

# fitting env variables 
i_fit <- envfit(Ik_pca, CTM2[,6:11], permu=999)
i_fit
scores(i_fit, "vectors")
# adding fitted vectors to an ordination using plot command
plot(Ik_pca, display = "sites")
plot(i_fit)


### Ik ~ pigments variables

pigments2 <- pigments
al_pca <- LC_Fit_Ik[,8:9]

# removing NAs
index_na <- which(is.na(al_pca), arr.ind = T) 
index_na
index_na <- index_na[, 1] 
al_pca <- na.omit(al_pca)  
pigments2 <- pigments2[-index_na, ]      

# fitting env variables 
i_fitp <- envfit(Ik_pca, pigments2[,7:8], permu=999)
i_fitp
scores(i_fitp, "vectors")
# adding fitted vectors to an ordination using plot command
plot(Ik_pca, display = "sites")
plot(i_fitp)






########## NMDS ##########

# running nmds
nmds1 <- metaMDS(pigments[,c(7:8)], autotransform = T) 
nmds1

# plotting
ordiplot(nmds1, type = "t")

# fitting env variables 
pig_fit2 <- envfit(nmds1, CTM[6:11])
pig_fit2

scores(pig_fit2, "vectors")

# adding fitted vectors to an ordination using plot command
plot(nmds1, display = "sites")
plot(pig_fit2)


adonis2(pigments[,c(7:8)] ~ Temp + Cond + Turb + O2_sat + DO.mg + Coef_P, data=CTM)



########## Constrained ordination ##########

# Redundancy analysis
# using model formula:  community_data ~ equation_for_constraints, dataset_contraints
rda1 <- rda(pigments[,c(7:8)] ~ Temp + Cond + Turb + O2_sat + DO.mg + Coef_P, CTM)
rda1
# plotting  results
plot(rda1)

anova(rda1)





########## data visualization ##########

# converting  Treatmint as factor
pigments$Treatment <- factor(pigments$Treatment, levels = c("C", "M", "H"))
CTM$Treatment <- factor(CTM$Treatment, levels = c("C", "M", "H"))
LC_Fit$Treatment <- factor(LC_Fit$Treatment, levels = c("C", "M", "H"))

# plotting Chla
ggplot(pigments, aes(y=Chla, x=Sampling, fill=Treatment)) +
  geom_boxplot()

# plotting Cartoneoids
ggplot(pigments, aes(y=Carot, x=Sampling, fill=Treatment)) +
  geom_boxplot()

# env conditions 
CTM %>%
  pivot_longer(.,Temp:Coef_P, names_to="env", values_to = "val") %>%
  ggplot() +
  geom_boxplot(aes(x=Sampling, y=val, fill=Treatment)) +
  facet_wrap(~ env, scales = "free_y")

# creating single table with env var and pigments data
dfanova <- cbind(CTM, pigments[,c(7,8)])

# Chla en function des variable env
ggplot(dfanova, aes(y=Chla, x=Temp)) + 
  geom_point() 
ggplot(dfanova, aes(y=Chla, x=Cond)) + 
  geom_point() 
ggplot(dfanova, aes(y=Chla, x=Turb)) + 
  geom_point() 
ggplot(dfanova, aes(y=Chla, x=O2_sat)) + 
  geom_point() 
ggplot(dfanova, aes(y=Chla, x=DO.mg)) + 
  geom_point() 
ggplot(dfanova, aes(y=Chla, x=Coef_P)) + 
  geom_point() 

# Carot en function des variable env
ggplot(dfanova, aes(y=Carot, x=Temp)) + 
  geom_point() 
ggplot(dfanova, aes(y=Carot, x=Cond)) + 
  geom_point() 
ggplot(dfanova, aes(y=Carot, x=Turb)) + 
  geom_point() 
ggplot(dfanova, aes(y=Carot, x=O2_sat)) + 
  geom_point() 
ggplot(dfanova, aes(y=Carot, x=DO.mg)) + 
  geom_point() 
ggplot(dfanova, aes(y=Carot, x=Coef_P)) + 
  geom_point() 



########## ANOVA ##########

# Effect of treatment on Chla concentration
ggplot(pigments, aes(y=Chla, x=Replicate, fill=Treatment)) + 
  geom_boxplot() 

# mixed linear model
lmm1 <- lmer(data=pigments, Chla~Treatment + (1|Replicate))
summary(lmm1)
rsq(lmm1)
anova(lmm1) 

lm1 <- lm(data = pigments, Chla~Treatment)
lm1
summary(lm1)
# anova
(aov1 <- aov(lm1))
TukeyHSD(aov1)

lm3 <- lm(data = pigments, Chla~Sampling)
lm3
summary(lm3)
# anova
(aov3 <- anova(lm3))



# Effect of treatment on Carotenoids concentration
ggplot(pigments, aes(y=Carot, x=Replicate, fill=Treatment)) + 
  geom_boxplot() 



# mixed linear model
lmm2 <- lmer(data=pigments, Carot~Treatment + (1|Replicate))
summary(lmm2)
rsq(lmm2)
anova(lmm2) 
plot(lmm2)


hist(Carot)
hist(pigments$Chla)


### linear model 
lm2 <- lm(data = pigments, Carot~Treatment+Sampling)
lm2
summary(lm2)
# anova
(aov2 <- anova(lm2))
# not normal distribution of residuals
hist(resid(lm2), main="", xlab="", breaks = 20) 
shapiro.test(resid(lm2))




# Effect of environmental variables on Chla concentration

lmm3 <- lmer(data=dfanova, Chla ~ Temp + Cond + Turb + O2_sat + DO.mg + Coef_P + (1|Replicate))
summary(lmm3)
rsq(lmm3)
anova(lmm3) 
plot(lmm3)

# log trasf
lmm3 <- lmer(data=dfanova, log(Chla) ~ Temp + Cond + Turb + O2_sat + DO.mg + Coef_P + (1|Replicate))
anova(lmm3) 

# log trasf
lmm3 <- lmer(data=dfanova, sqrt(Chla) ~ Temp + Cond + Turb + O2_sat + DO.mg + Coef_P + (1|Replicate))
anova(lmm3) 

glm1 <- glmer(Chla ~ Temp + Cond + Turb + O2_sat + DO.mg + Coef_P +(1|Replicate), family = "poisson", data = dfanova)
glm1
glmQP1 <- glmmPQL(Chla ~ Temp + Cond + Turb + O2_sat + DO.mg + Coef_P, random= ~ 1|Replicate, family="quasipoisson", data=dfanova)
glmQP1
Anova(glmQP1)



# Effect of environmental variables on Carot concentration

lmm4 <- lmer(data=dfanova, Carot ~ Temp + Cond + Turb + O2_sat + DO.mg + Coef_P + (1|Replicate))
summary(lmm4)
rsq(lmm4)
anova(lmm4) 
plot(lmm4)










