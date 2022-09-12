# KEEP UP WITH R!!


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


