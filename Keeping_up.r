# KEEP UP WITH R!!


############################################## dplyr package ##############################################

library(dplyr)

# creating an example dataset
data <- data.frame(x1 = 1:6,
                   x2 = c(1, 2, 2, 3, 1, 2),
                   x3 = c("F", "B", "C", "E", "A", "D"))
data


###### Example 1: arrange Function 

# ordering data sets according to a certain column of our data
# sort our data according to the variable x3
arrange(data, x3)


###### Example 2: filter Function 

# extracts rows of our data by a logical condition
# creates a subset of our original data frame, in which only rows with the value 2 in the variable x2 are retained
filter(data, x2 == 2)


###### Example 3: mutate Function 

# transforms variables into new variables
# create a new variable x4, which is containing the sums of each row of the variables x1 and x2
mutate(data, x4 =  x1 + x2)


###### Example 4: pull Function 

# extracts certain columns of our data frame and converts them into a vector
# extracts the variable x2
pull(data, x2)


###### Example 5: rename Function

# changes the name of certain columns
# change the name of the third column from x3 to new_name
rename(data, new_name = x3)


###### Example 6: sample_n Function 

# randomly samples N cases from our data frame
# sample three rows of our original data without replacement
# set seed for reproducibility
set.seed(765)
sample_n(data, 3)


###### Example 7: select Function

# extracts certain columns from a data frame
# creates a subset with the columns x2 and x3
select(data, c(x2, x3))

# https://www.geeksforgeeks.org/dplyr-package-in-r-programming/ 





##########################################################################################
# Introduction to Coding in R Part 2


# install.packages("EBImage")
# library(EBImage)
# img = readImage("path/to/your/image/file or URL")
# display(img, method = "raster")

library(raster)
help <- raster("screen/help.png")
plot(help)


###### Question 8: how to handle missing data

incomplete <- c(1, 2, 3, 4, 5, 6, NA, 8, 9, 10)
incomplete
max(incomplete)
max(incomplete, na.rm = T)

# use na.rm = T to remove missing values


###### Question 10: how to code dates and time

?strptime
# function that transform numbers in character dates
class_time <- strptime(c("20210209113000", 
                         "20210209124500"),
                       format = "%Y%m%d%H%M%S")
class_time



##########################################################################################
# Data Management in R


# starting with excel and then convert it in .csv format so that we can use it in R
# create a file excel and save it as .csv -> replace it, problems of tabs, record only the active one

# setting working directory faster: Session -> Chose working directory
# copy-paste code appeared in the console

# read.csv authomatic in R no packages needed
# header = T recognise first line as header

# merging fishes and  date of collection in sites 
# function merge: fishes_site <- merge(fishes, sites, by="site" )
# when merging doesn't repeat site but just add the time column



##########################################################################################
# R for Data Science Lecture


# saving file as cdv or txt is the best thing to do
# useful package containing many other packages: tidyverse
install.packages("tidyverse")
library(tidyverse)

# read.csv
# read.table
# read.delim
# write.csv
# write.table

import <- raster("screen/import.png")
plot(import)

import_comp <- raster("screen/import_comparison.png")
plot(import_comp)

# observation lines
# variable columns
# values cells



##########################################################################################
# Applied Statistics in R Part 1


############ applied statistics terms

# parametric -  assuming idea of normality in the distribution (student t-test, anova analysis of variance)
# vs
# non-parametric - non normal distribution, skewed distribution, more frequent

# independence - describe situa in which obs are not related to each other, on obs convey no info about the next
# independent variables - predictor variable or x-variables representing the experimental variable that is being manibulated in a manipulative study or observed across gradients to asses their effect
# dependent variable - response variable or y-variable

# binomial data -- data that can be class into two discrete categories

# homoscedasticity - homogeneity of variances

# replication - increase sample size, influence inferences (pseudoreplication when replication are not independent)

# nesting - some prdictor variables or observation of the response variable can be broken down into finer groups within groups (hyerarchy)

# mixed effect models - mixed models - used to account for non-independence among observation  by including random effects taht capture the relationship among observations and fixed effects that pertain to the hypotesis being tested
# generalized linear models - models  fit to data to that do not meet the assumption of normal distribution
# non normal distribution - binomial (two outcome only) vs Poisson (only non negative, mean and variance are equal) vs negative binomial vs zero inflated Poisson
# additive models - model in which we do not have a linear or curvy relationship, very dynamic relations


############ testing for differences versus relationships


##### tests of differences

# see screenshots 

##### tests of relationships

# see screenshots


############ working in R

library(ggplot2)
data(mpg)

# plotting histogram of city fuel economy (cty)
hist(mpg$cty)
ggplot(mpg, aes(cty))  + geom_bar()

# test if distrib is normal with shapiro-wilking test
shapiro.test(mpg$cty)
# W = test  statistics
# p-value = very low -> data differ signif from a normal distrib
#data are not normally distributed

# try to make the data more near to a normal  distrib
hist(log(mpg$cty))
shapiro.test(log(mpg$cty))
# p-value is greater, closerto the ass of normal distrib, can use parametric test (even if best use non parametric)

# test kurtosis (test if sample normally distributed)
install.packages("moments")
library(moments)
anscombe.test(mpg$cty) # test if we have a normal distrib
# kurtosis value of three = 3 =  normal distrib (mesokortic)
# value < 3 (platykutic)
# value > 3 (leptokurtic) -> peak higher than normal, tail lower than normal

# test for skeweness 
agostino.test(mpg$cty)
# skew value = positive
# p-value very low -> reject h0 -> data  are positively or right skewd


######### goodness of fit test

### chi-square test

# test for diff in the number of cars in each class bw 1999 and 2008
table(mpg$class, mpg$year)
# is there a diff in class of cars produced in the two years ? is  there a preference or dependence ?
# do  the obs data match with the expected data ?
chisq.test(table(mpg$class, mpg$year))
# looking at the diff in the two distrib
# X-square = around 1 -> on avarage 1 value of difference in the distribution on the number  of cars
# p-value very high -> probability of finding a value as extreme  or more extreme of the chi-square value
# distribution did not differ bw the two years



##########################################################################################
# Applied Statistics in R Part 2


library(ggplot2)

# create a dataframe for car classes by years
car_classes <- data.frame(table(mpg$class, mpg$year))
# rename the columns
colnames(car_classes) <- c("class", "year", "numbercars")
# create a ggplot
ggplot(car_classes, aes(class, numbercars)) + 
  geom_bar(stat="identity") +
  facet_grid(~year) # overlayng facet grid with 2 different year

# rare occorences can coause strange behaviors

# non-parametric version of Chi-square is Kolmogorov-Smirnov aka ks test
car_table <- table(mpg$class, mpg$year)
ks.test(car_table[,1],  car_table[,2])
# D = test statistics, scale from 0 to 1, D=0.14286, pretty low
# p-value = 1 prob to find  value greater than D is pretty high
# cars in 1999 fromn a stat point of view same than 2008
# they come  from the same  distribution


### test of differences in cty among drivetypes
ggplot(mpg, aes(drv, cty)) +
  geom_boxplot()

# test we  might want to use with anova
# anova = analysis of variance, looks for variation within each grops and among differnt groups

# different type visualization, more intuitive
ggplot(mpg, aes(drv, cty)) +
  geom_violin()
# simmetrical distribution of frequencies
# how data are distributed with respect to a continues variable (cty)

# assumptions that would go into an anova type test
# considering if there  are signif diff among 4 f and r for city fuel economy
# anova = parametric tool to compare means for more than two groups
# having two mean we use a t-test


# anova have assumption of normality

# test for normal distribution of the response variable using shapiro test
shapiro.test(mpg$cty)
# p-value signif = distrib is skewed, differ signif from normal distrib

# test for homogeneity of variances (homoschedasticity)
install.packages("car")
library(car) # companienion to applyied regression
leveneTest(cty~drv, data=mpg)
# p-value  very high, we meet assumption of homogeinity of variances

# just for comparison, let's run an ANOVA
t1 <- oneway.test(cty~drv, data = mpg, var.equal = T)
# check if two ore more samples from the normal distribution have the same mean
# it does not necesserly need to be equal
# if you violate the assumption of unequal variances you could still run an anova using oneway.test and putting var.equal = F
# that means that  if the levene test comes out significat, you've got unequal variances, you can use the type 3 sums of squares instead of the type one sums of squares and still run an anova
t1
# one way analysis of means = one way analysis of variances = one way anova
# F (test) statistics
# p-value very small, signif, we have a difference

# second way to do an anova
t2 <- aov(cty~drv, data = mpg)
summary(t2) # when use aov function you must use smmary
# very similar to oneway.test
# same conclusions: there are diff at least bw two gr but we don't know which are diff from each other
# we must use post-hoc  testing or  paiwise differences

# we can use a t-test to comapre two gr if we have normally  distributed data

# determine pairwise differeces
TukeyHSD(t2)
# run tukey test to see which gr are signif diff
# comp f vs 4 -> diff 5.64, p-value adj signif -> real difference
# comp r vs 4 -> diff -1.92, p-value adj very high notsignif -> no difference
# comp r vs f -> diff -5.89, p-value adj signif -> real difference

# alternative pairwise  method
attach(mpg)
pairwise.t.test(cty, drv ,p.adjust.method = "bonf") # Bonferroni method
# comparing 4 to  f p-value signif
# comparing 4 to  r p-value not signif
# comparing f to  r p-value signif
detach()


### non-parametric comparison of more than two means
# to dhis you use Kruskal-Wallis
kruskal.test(cty~drv, data = mpg)
# p-value equal to conclusion of anova but kruskal is a more appropriate test

# non-parametric post-hoc  test or pairwise  comparison
install.packages("FSA")
library(FSA)
dunnTest(cty~drv, data = mpg, method = "bh") # Benjamini-Hochberg method
# adj p-value in  first case 6.9e-24 (6.9 to the negative 24 very signif)
# second p-value 7.119264e-01 = 0.7119 not signif
# tird p.adj = 2.962794e-11 signif
# same conclusion




############################################## Applied Statistics in R Part 3 ##############################################

####### regression models #######

library(ggplot2)
library(car)

# load data
data(mpg)


###### linear model #######

# cty fuel economy (response variable) as a function of engine size (predictor variable)
model1 <- lm(cty~displ, data=mpg)
summary(model1)
# Residuals: check if residuals normally distributed 
# Coefficients: of the model y=ax+b (a slope, b intercept) -> Estimate (Intercepts) = b, displ = a slope
# for each coeff we have a p-value (significantly different from 0)
# Multiple R-squared:  0.6376 - coefficient of determination -> 64% of variation in cty can be explained by displaycement

# is that an appropriate model for our data?

# 1. check assumption of homosscedasticity
residualPlot(model1)
# dashed line = 0 distance bw residuals (observed data - predicted)
# residual are curvilinear -> not good fit for nature of the data
# hp violated -> heteroscedasticity

# 2. check for normal distrib of residuals
densityPlot(model1$residuals)
# almost follow a normal distrib, but positive skewed and two pics

# 3. assumption of independence
# observation displacement of the data in the data frame are independent from one-another
durbinWatsonTest(model1)
# there is signifacant level of autocorrelation in the model?
# p-value = 0, some observation are more correlated with each other than others
# observation are not independent

# 4. check for linear correlation
ggplot(mpg, aes(displ, cty)) + 
  geom_point() +
  geom_smooth(method = "lm", formula = y~x)
# not linear correlation
# negative slope (as we saw in the formula at the beginning)
# we do not have a uniform distribution of residuals
# vertical points -> observation are not totally independent from one-another (autocorrelation)

# model1 is terrible

# test if model for hwy fuel economy as a function of cty economy is better 
answer1 <- lm(hwy~cty, data=mpg)
summary(answer1)
# p-value=0.084 says that intercept not significantly different from 0 (but p-value near to 0.05)

# 1. check assumption of homoscedasticity
residualPlot(answer1) # not homoscedasticy

# 2. check for normal distrib of residuals
densityPlot(answer1$residuals) # not bad, quite normal distrib, little skewed to both sides

# 3. assumption of independence
durbinWatsonTest(answer1)
# data not independent

# 4. check for linear correlation
ggplot(mpg, aes(cty, hwy)) + 
  geom_point() +
  geom_smooth(method = "lm", formula = y~x)
# quite linear
# some points are in vertical -> autocorrelation



###### generalized linear model #######

# new model: cty fuel economy (response variable) as a function of engine size (predictor variable)
model2 <- glm(cty~displ, family = poisson(link = "log"), data=mpg)
summary(model2)

# AIC - can be used to compare differents models
# we do not get R-square value -> use package rsq

install.packages("rsq")
library(rsq)

# rsq function compute R-square
rsq(model2)

?glm
# binomial(link = "logit") -> to make a binomial distrib
# gaussian(link = "identity") -> same model used for the lm
# poisson(link = "log") -> as a number very distant from 0 is a normal distribution, but going toward 0 data become very skewed

# plotting glm
ggplot(mpg, aes(displ, cty)) + 
  geom_point() +
  geom_smooth(method = "glm", formula = y~x, method.args = list(family = "poisson"))
# now model looks like a curving line, better fitting the data

densityPlot(model2$residuals)
residualPlot(model2) # better -> residuals are flatter (getting closer to 0)

# what about relationship bw displ and hwy?
answer2 <- glm(hwy~displ, family = poisson(link = "log"), data=mpg)
summary(answer2)
rsq(answer2)
ggplot(mpg, aes(displ, hwy)) + 
  geom_point() +
  geom_smooth(method = "glm", formula = y~x, method.args = list(family = "poisson"))
densityPlot(answer2$residuals)
residualPlot(answer2) 
durbinWatsonTest(answer2) # still autocorrelation



###### generalized linear mixed model #######

library(lme4)

# fixed effects - the ones we are generally interested in
#                 we want to know how displaycement affects either cty anf hwy

# random effects - creating autocorrelation (variable you want to accont for)
#                  due to manufacturer of engine

model3 <- glmer(cty~displ+(1|manufacturer), data = mpg, family = poisson(link = "log"))
# displ = fixed effects
# + (1|manufacturer) = random effects 
model3
# intercept change according to manufacturer (random effects)
rsq(model3)
# $model [1] 0.7145693
# $fixed [1] 0.67642
# $random [1] 0.03814928

# plotting model3
ggplot(mpg, aes(displ, cty)) + 
  geom_point() +
  geom_smooth(method = "glm", formula = y~x, method.args = list(family = "poisson")) +
  facet_grid(~manufacturer) # relationship bw displ and cty but for each manufacturer

# random effects -> the rel vary amoung manufacturer?


# can you fit a glmn with:
# fuel type ("fl") as a random effect
# cty fuel economy as the responsive variable
# displ as fixed effect

anwer3 <- glmer(cty~displ+(1|fl), data = mpg, family = poisson(link = "log"))
anwer3
ggplot(mpg, aes(displ, cty)) + 
  geom_point() +
  geom_smooth(method = "glm", formula = y~x, method.args = list(family = "poisson")) +
  facet_grid(~fl) 
# there is a lot of non-independence in the observations -> not most appropriate random effect


###### generalized additive model #######

library(mgcv)

# additive -> fit the data moving along it (s = smooth)
model4 <- gam(cty~s(displ), data = mpg)
summary(model4)
rsq(model4)

# plotting gam
ggplot(mpg, aes(displ, cty)) + 
  geom_point() +
  geom_smooth(method = "gam", formula = y~s(x))
# we cannot  interpret the relationship -> no prediction outside data range
# you need a lot of data to fit it




############################################## Introduction to Ordination Lecture ##############################################

# Ordination = ordering objects that are characterized by multiple attributes (multivariate objects) in a smaller number of dimentions (two or three)
# - indirect ord. - indirect gradient analysis or unconstrained ord
# - direct ord. - direct gradient analysis or constrained ordination


###### Principal Component Analysis

# PCA -  reduce the dimensionality of dataset by crating a linear combination of the original variable
# by fitting an eigenvector through the longest access of a data cloud
# than goes 90° and fits another one (impo PCA2 should be independent from PCA1)
# eigenvalue represent proportion of variance explained by the vector (axe)
# ouput - rotation that allow PCA1 to be x and PCA2 to be y

# if we had 15 variable we'll have 15 PCAs axes


###### Non-metric Multi-Dimensional Scaling

# NMDS - work with community data, rather then env data
# method begins with a raw data matrix represents species (column), by sampling sites (rows)
# data matrix can be abundance data or presence/absence data
# from this data matrix dissimilarity indices (distance metrics) are calculated
# for abundance data most common dissimilarity index is Bray-Curtis Distance

# 1. bf interpretation look at stress value
# if you have very high stress (S>0.2) you might want to explain your data with one additional dimension
# if stress<0.1 great representation in 2 dimensional space

# points near to each other similar community composition
# can overlay environmental variable correlated with community structure

# testing with permanova






##########################################################################################
# 'vegan' Package Lecture

# Loading vegan library
library(vegan)
install.packages("devtools")
library(devtools)
devtools::install_github("gavinsimpson/ggvegan")
library(ggvegan)

#loading mite data
data("mite") # community data on mites from 70 soil cores
data("mite.env") # environmental data collected from core locations
data("mite.pcnm") # principal component of neighbor matrices


##############################
# unconstrained ordination

# principal component analysis (linear comp of data to come up with variuable reduction)
pca <- rda(mite.env[,1:2]) 
# rda - redundances analysis, consrained or direct ordination in wich we have two matrices(cmmunity and env data) how much variation can community matrix can explain with env data 
# with 1 matrix we run a PCA
summary(pca) # linear comb of original data
# inertia = variance
# most variation expl by first axis - cumulative proportion: 99% explained by PC1
autoplot(pca, arrows = T)
# sites positive in PC1 have high content in water
# sites negative in PC1 have lower content in water
# low density positive in PC2, smalla gradient


############################
# constrained ordination

rda <- rda(mite~SubsDens+WatrCont, data=mite.env)
# left of ~ is the response variable
# right of ~ is the independent or predictor variable
# we are asking to the model how much variation in the community comp of mites can we explain with subtr density AND = + water content 
rda
# constrained inertia = amount  of variation explained by env var
# unconstrained = residuel not explained
# 21% explaine
# 78% not  explained
autoplot(rda, arrows = T)

# you can use more than one predictor variable matrix
rda2 <- rda(mite~.+as.matrix(mite.pcnm[,1:3]), data=mite.env)
# . use every predictor variable
rda2
# to decide how many RDAs keep dep on velocity  with which inertia fall
autoplot(rda2, arrows = T)
# how  can partition spatial measurement and env variables, how to get contribution space vs physical habitat --> use  varpart function
v.part <- varpart(mite, mite.env[,1:2], mite.pcnm[,1:3]) # resp matrix + two matrices of predictor separated by a comma 
v.part
# pure env effect 18% (explained 18% of variation in community just with env var)
# spatial effect 1,5%
# interaction effect 1%
plot(v.part)
# local env >> impo that spatial structure
# try with variables divided into more groups
v.part2 <- varpart(mite, ~SubsDens+WatrCont, ~Substrate+Shrub+Topo, mite.pcnm[,1:3], data = mite.env, transfo = "hel")
# transfo for tranforming community data, bc very abundant taxa or very rare taxa can have large influence on the ordinatio, transfo downwheig their influence
plot(v.part2)
# continuous env variable 7%
# categorical variable env 8%
# spatial 3%
# relative contribution of different  classes of env or spacial predictor variable that  have influence  on community


######################################
# Non-metric Multidimensional scaling

# starts with a scatter done with PCA and then goes through interaction of mooving the point  a little bit at  the time and measuring how well they are represented in two dimensional scale
# iterative process to represent all variations in two dimension
# brake-curtis distance of 0 = community in A has same spp composition and abundance than B
# distance increase 0 -> 1 = higher turnover or loss of spp (beta-diversity)

nmds <- metaMDS(mite) 
autoplot(nmds) # k =  number of dimension
# two dimesional stress < 0.2 ok (try to find  solution to reduce stress, if no convergence use > trymax)
# axis NMDS 1 and 2, numbers are number of sites, in green name of the taxa
# along NMDS if the community is positive those community would tend to have higher abundances of taxa that were positive along NMDS 1
# if we go negative on NMDS1 collection from these  sites would have higher abundances of these organisms or  taxa
# we have gradients in community composition
# 1st axis explains the most

#  alternative plots
# hp: community differs in location that have few many or no shrub
# we can test hp with NMDS
# three level with three colors (can do the sma with shapes)
col=c("red", "blue", "green")
shape = c(18, 20, 15)
plot(nmds$points, col=col[mite.env$Shrub], pch=shape[mite.env$Shrub], cex=1.2, main = "Mite Shrub Groups", xlab="NMDS 1", ylab ="NMDS 2")
# points = x, y coordinates  for the sites
# sites with different levels of shrobs
ordispider(nmds, groups = mite.env$Shrub, label = T)
# function that can overlap a plot over a already existing plot
# spider graph where all member  of the group is connected togheter
# obvious ecological gradiend along NMDS1 (from many to few to none)

# test if groups is signifincantly different from each other
test1 <- adonis(mite~Shrub, data = mite.env, permutations = 999, method = "bray")
# method to create baris curdis distances
test1
# df = degree of fredom n-1
# F. Model = test statistics, larger = more likely significant finding
# Pr(>F) = P-value = probability to have a value equal or greater than F = 9 - signifance threshold 0,05, p 0.001<<0.05 -> high significance
# R2 = Rsquare = 21% in var in mite community structure explained by shrub factor
# there is sign diff in mite community struct among sites with many few and no shrubs

# critical assumption that comes with running adonis test per multivariate analysis of variance = variance within each group is equal (amount of variation within the few gr, many, and none gr should be equal in order for this test to work appropriarly)
# we can test for that
distances.mite <- vegdist(mite)
# vegdist create pairwise distances, create break distances bw all 70 locations (value range from 0same to 1diff)
test2 <- anova(betadisper(distances.mite,mite.env$Shrub))
# betadispersal func = are there diff in dispersion of the data within this distance value that we created
# dispersion within this gr is equal bc p-value > 0.5 (0.3689) - > ok to run the adonis
test2

# comparing to continuous variable -> test for significant correlation with continuos variable using envfit function
test3 <- envfit(nmds~SubsDens+WatrCont, mite.env)
test3
# two env predictors with  coord in NMDS1 and  2 r2 and significance (p-value)
# test significat correlation bw env predictor and community structure
# signif 0.001, explaided 70% for water content
plot(test3)
# just like ordispider this plot  is overlapped

# concluding that we have variation in the community of mites across teh landscape, related to shub density, and are distributed along a gradient of water content


