##################################################### VEGAN TUTOR #####################################################


################################## 2 Ordination : basic method ##################################


################# 2.1 Non-metric Multidimensional scaling #################

library(vegan)
library(MASS)

# loading  data
data(varespec)
# generating dissimilariry input
vare.dis  <- vegdist(varespec)
vare.mds0 <- isoMDS(vare.dis)
# result - list of points and stress
# stress S ois a statistic goodness of fit

# inspect NMDS maps
stressplot(vare.mds0, vare.dis)
# draws Shepard plot where ordination distances are plotted against community dissimilarities
# two correlation like statistics of goodness of fit:
# 1. R^2  = 1-S^2
# fit-based R^2 = correlation between the fitted values delta(d) and ordination distances d

# ordiplot function for andling results of NMDS
ordiplot(vare.mds0, type = "t")
# it shows scores of the sites (dissimilarities did not  have info about spp)

# function use several random starts and select among similar solutions with smallest stresses
vare.mds <- metaMDS(varespec, trace = F)

# did not calculate dissimilarities in separate step, original data matrix as input
plot(vare.mds, type ="t")
#  range of data values too  large -> data are square root trannsormed and submitted to Wisnconsin double standardization (spp divided by their  max)
# function used Bray-Curtis dissimilarities
# function run isoMDS with several random starts
# stopped either after a certain number of tries, or after finding two similar configurations with minimum stress
# function rotated solution so that largest variance of site scores will  be on the first axis
# function scaled solution so that one unit correspond to halving of community similarity from the replicate similarity
# Function found species scores as weighted averages of site scores, but expanded them so that species and site scores have equal variances. 

?metaMDS
# Nonmetric Multidimensional Scaling with Stable Solution from Random Starts, Axis Scaling and Species Scores
# Function metaMDS performs Nonmetric Multidimensional Scaling (NMDS), and tries to find a stable solution using several random starts. 
# In addition, it standardizes the scaling in the result, so that the configurations are easier to interpret, and adds species scores to the site ordination. 
# The metaMDS function does not provide actual NMDS, but it calls another function for the purpose. Currently monoMDS is the default choice, and it is also possible to call the isoMDS (MASS package).



################# 2.2 Community dissimilarities #################

data("varechem")
# rankindex used  to study which of the indices best separates comunnities along known gradients  using rank correlation as default
rankindex(scale(varechem), varespec, c("euc", "man", "bray", "jac", "kul"))
# identical rank order for the three recommended indices

# to standardize sites  to  equal sum of squares or to their vector norm -> function decostand
dis <- vegdist(decostand(varespec, "norm"), "euclid")
# max limit of sqrt(2) when there are no shared species between  two sites

dis <- vegdist(decostand(varespec, "hell"), "euclidean")

# use designdist to def own  dissimilarity indices by writing its equation
d <- vegdist(varespec, "bray", binary = T)
d <- designdist(varespec, "(A+B-2*J)/(A+B)")
d <- designdist(varespec, "(b+c)/(2*a+b+c)", abcd = T)
# easy to make errors -> better using betadiver function definig binary similarity



################# 2.3 Comparing ordinations: Procrustes rotation #################

# two ordination can be similar but difficult to see bc axes have  diff ordination and scaling
# compare ordination with Procrustes rotation -> function procrustes
tmp <- wisconsin(sqrt(varespec))
dis <- vegdist(tmp)
vare.mds0 <- isoMDS(dis, trace = 0)
pro <- procrustes(vare.mds, vare.mds0)
pro
plot(pro)
# small distances, in interactive session use identity function to identify points where differences are concentrated
# asking a plot of residual differences only
plot(pro, kind = 2)



################# 2.4 Eigenvenctor methods #################

# 1. Principal Component Analysis - PCA

vare.pca <- rda(varespec)
vare.pca
plot(vare.pca)
# output tell tahta total inertia is 1826, inertia is variance
# sum of all 23 rank eigenvalues = total inertia
# total variance is decomposed into linear components

# check that the variance is equal to total inertia
sum(apply(varespec, 2, var))
# apply funct applies function var (variance) to dimention 2 (columns = species)
# sum take the sum of these values
# inetia = sum of all spp variances

# eigenvalues each explains a certain proportion of the total variance
# proportion of total variance explained by the first axis
983/1826
# = 53.8%

# using biplot arrows for species and sites
biplot(vare.pca, scaling = -1)
# scaling = -1 means that the results are scaled only when they are accessed
# The negative values mean that species scores are divided by the species standard deviations 
# so that abundant and scarce species will be approximately as far away from the origin.


# to have a more balanced ordination
# standardizin all spp to unit variance or use correlation coefficients instead of covariances
vare.pca <-rda(varespec, scale = T)
vare.pca
plot(vare.pca, scaling = 3)
# Now inertia is correlation, and the correlation of a variable with itself is one.
# total inertia is equal to the number of variables (species)
# rank is one less tahn smaller number of spp or sites
dim(varespec)
# percentage explained by the first axis dcreased -> we are not just explaining abundant spp with hih variances, but all spp equally


# 2. Correspondence Analysis (CA)

vare.ca <- cca(varespec)
vare.ca
# inertia is  mean squared contingency coeffincient (scaled Chi-square)
# = Chi-sq statistic of a data matrix standardized to unit total
chisq.test(varespec/sum(varespec))
plot(vare.ca)
# spp scores are weighted averages of site scores

# displaying site scores as weighted avarages of spp scores
plot(vare.ca, scaling = 1)

# scaling = 3 or symmetric scaling in pca
# species are weighted averages of sites (scaling = 2)
# sites are weighted averages of species (scaling = 1)



################# 2.5 Detrended Correspondence Analysis #################

# correct drawbacks of correspk9ndece analysis with long ecological gradients: detrend, rescale, downweight

# correction incorporated in function decorana
vare.dca <- decorana(varespec)
vare.dca
plot(vare.dca, display = "sites")
# In heterogeneous data with a clear arc effect the changes are more dramatic
# default analysis is without downweighting of rare species
?decorana
# Detrended Correspondence Analysis and Basic Reciprocal Averaging
# downweight is an independent function that can be used with cca as well



################# 2.6 Ordination graphics #################

# how to draw better representation of ordination

# loading data od Barro Colorado Island (difficult to plot - 225 spp)
data(BCI)

mod <- decorana(BCI)
plot(mod)

# dataset uses full spp names
names(BCI)[1:5]
# use utility function make.cepnames to abbreviate Latin names
shnam <- make.cepnames(names(BCI))
shnam[1:5]

# selectively label spp using interactive identify function (label of the point you click on)
pl <- plot(mod, dis="sp")
identify(pl, "sp", labels=shnam)

# function ordtorp labels item only if without overwriting to previous label
# processes from margin to center or according to priority (> abundant spp)
stems <- colSums(BCI)
plot(mod, dis="sp", type="n")
sel <-  orditorp(mod, dis="sp", lab=shnam, priority = stems, pcol = "gray", pch = "+")

# using function ordilabel
plot(mod, dis="sp", type="n")
ordilabel(mod, dis="sp", lab=shnam, priority = stems)

# using function ordipointlabel
plot(mod, dis="sp", type="n")
ordipointlabel(mod, dis="species", lab=shnam, priority = stems,  pch = "+")

# function orditkplot to edit plots



################################## 3 Environmental interpretation ##################################


# using external environmental variables to interpret ordination



################# 3.1 Vector fitting #################

# most commonly used method <- fitting environmental vectors onto ordination
# fitted vectors are arrows

# arrow points to direction of most rapid change in the env variable (direction of the gradient)
# legth of the arrow is proportional to the correlation bw ordination and env var (stregth of the gradient)

# fitting env vectors of varechem dataset to NMDS result with envfit function
data(varechem)
ef <- envfit(vare.mds, varechem, permu = 999)
ef
# first two columns give direction cosines of the vector
# r2 gives square correlation coefficient
# (for plotting axes should be scaled by the square root of r2, but function plot does it automatically)

# extracting scaled values
scores(ef, "vectors")

# adding fitted vectors to an ordination using plot command
plot(vare.mds, display = "sites")
plot(ef, p.max = 0.1)



################# 3.2 Surface fitting #################

# making vector fitting for selected variables and add fitted surfaces in the same plot
ef <- envfit(vare.mds ~ Al + Ca, varechem)
plot(vare.mds, display = "sites")
plot(ef)
tmp <- with(varechem, ordisurf(vare.mds, Al, add = T))
with(varechem, ordisurf(vare.mds, Ca, add = T, col = "green4"))

fitted(ef)



################# 3.3 Factors #################

data("dune")
data("dune.env") # env variables are factors
dune.ca <- cca(dune)

# creation of class centroids
ef <- envfit(dune.ca, dune.env, permutations = 999)
ef

plot(dune.ca, display = "sites")
plot(ef)
# names of the factor centroids = name of fcator + level (eg. UsePasture or Moisture5)
# axes show centroids for the level and R2 values are for the whole factor

# graphical display of factors
plot(dune.ca, display = "sites", type = "p")
with(dune.env, ordiellipse(dune.ca, Management, kind = "se", conf = 0.95))
with(dune.env, ordispider(dune.ca, Management, col = "blue", label = T))
with(dune.env, ordihull(dune.ca, Management, col = "blue", lty = 2))



################################## 4 Constrained ordination ##################################

# In constrained ordination we want to display only the variation that can be explained by the used environmental variables, or constraints
# The constrained ordination is non-symmetric: we have “independent” variables or constraints and we have “dependent” variables or the community.

# - Constrained analysis of proximities (cap)
#   function capscale
#   related to metric scaling (cmdscale)

# - Redundancy analysis (rda)
#   function rda
#   related to principal components analysis

# - Constrained correspondence analysis (cca)
#   function cca
#   related to correspondence analysis


################# 4.1 Model specification #################

# using model formula:  community_data ~ equation_for_constraints, dataset_contraints
vare.cca <- cca(varespec ~ Al + P + K, varechem)
vare.cca
# plotting  results
plot(vare.cca)
# arrow points to the direction of gradient
# length indicates the strength of the variable in the dimensionality of the solution


# to have 3D plots
library(vegan3d)
ordiplot3d(vare.cca, type = "h")
# ordirgl function to inspect 3D dynamic plots


# formula works also with factors
dune.cca <- cca(dune ~ Management, dune.env)
dune.cca
plot(dune.cca)
# Factor variable Management had four levels (BF, HF, NM, SF)
# Internally R expressed these four levels as three contrasts
# The basic plot function displays class centroids instead of vectors for factors

# ordered factors ->  Moisture of dune.env
vare.cca <- cca(dune ~ Moisture, dune.env)
vare.cca
plot(vare.cca)
# plot shows both the centroids of factor levels and the contrasts
# If we could change the ordered factor to a continuous vector, only the linear effect arrow would be important


################# 4.2 Permutation tests #################

# The significance of all terms together can be assessed using permutation tests: 
# the data are permuted randomly and the model is refitted. 
# When constrained inertia in permutations is nearly always lower than observed constrained inertia, 
# we say that constraints are significant.

# using anova function

anova(vare.cca)
# Model - contrained component 
# Residual - unconstrained component
# ChiSquare - corresponding inertia
# Df - corresponding rank
# test statistics F - ratio of ChiSquare and Df

# can analyse single terms or axes by setting argument by
# analysing all terms separetly in a sequential "Type I" test
mod <- cca(varespec ~ Al + P + K, varechem)
anova(mod, by = "term", step=200)

# “Type III” tests analyse the marginal effects when each term is eliminated from the model containing all other terms
anova(mod, by="margin", perm=500)
# marginal effects are independent of the order of the terms
# correlated terms will get higher "worse" P-values

# can also ask for a test of individual axes
anova(mod, by="axis", perm=1000)


################# 4.3 Model building #################

# very popular to perform constrained ordination using all available constraints simultaneously. 
# Increasing the number of constraints actually means relaxing constraints: 
# the ordination becomes more similar to the unconstrained one.

# If we do not have strict constraints, it may be better to use unconstrained ordination with vector fitting (or surface fitting)
# In constrained ordination it is best to reduce the number of constraints to just a few
# say three to five.

# using all env variables as constraints (not encouraged) -> .
mod1 <- cca(varespec ~ ., varechem)
mod1
# results similiar to unconstrained ordination
plot(procrustes(cca(varespec), mod1))


# vegan can be used to automatically select constraints into model using function step
# step uses Akakike's infrmation criterion (AIC) as a selection criterion
# starting with an unconstrained model
mod0 <- cca(varespec ~ 1, varechem)
mod <- step(mod0, scope = formula(mod1), test = "perm")
mod

# model building with step is fragile
# strarting with mod1 (largest model) the final model will be different
modb <- step(mod1, scope = list(lower = formula(mod0), upper = formula(mod1)))
modb

# checking steps of function step
modb$anova
# Variable Al was the first to be selected in the model in forward selection
# but it was the second to be removed in backward elimination. 
# Variable Al is strongly correlated with many other explanatory variables.

# obvious when looking at the variance inflation factors (vif) in the full model mod1:
# vif > 10 indicates that a variable is strongly dependent on others and does not have independent information.
vif.cca(mod1)
# comparing with  first model
vif.cca(mod)
# we see that in this case vif=1,012 << 10



################# 4.4 Linear combination and weighted avarages #################

# two kind of site scores in constrained ordinations:
# 1. Linear combination scores LC of costraining variables
# 2. Weighted averages scores WA of species scores

# their (weighted) correlation is called the species–environment correlation:
# displaying species-env weighted correlation
spenvcor(mod)
# corr coeff is very sensitive to single entreme values
# tells that axis 3 best correlates bc having extremes points

# opinions are divided on using lc or wa as primary results in ordination graphics
# prefer wa scores is that they are more robust against random error in environmental variables

dune.cca <- cca(dune ~ Management, dune.env)
plot(dune.cca, display = c("lc", "wa"), type = "p")
ordispider(dune.cca, col="blue")
# lc scores give the predicted class centroids, and wa scores give the predicted values
# for distinct classes, there is no overlap among groups
# length of ordispider segments is a visual image of species–environment correlation.


################# 4.5 Biplot arrows and environmental calibration #################

# arrows are based on (weighted) correlation of lc scores and environmental variables
# scaled to unit length in the constrained ordination of full rank

# With default scaling = 2, the biplot arrows have optimal relation to sites
# with scaling = 1 they rather are related to species

# a site should be perpendicularly projected onto the arrow to predict the value of the variable.

# using calibrate.cca function
pred <- calibrate(mod)
head(pred)

# fitting
# showing residual plot of prediction
with(varechem, plot(Al, pred[,"Al"] - Al, ylab="Prediction Error"))
abline(h=0, col="grey")

plot(mod, display = c("bp", "wa", "lc"))
ef <- with(varechem, ordisurf(mod, Al, display = "lc", add = TRUE))
plot(ef)


################# 4.6 Conditioned or partial models #################

# The effect of some environmental variables can be removed from the ordination before constraining with other variables
# formula for constrained ordination can contain a Condition 
# which specifies the variable or variables whose effect is removed from the analysis before constraining with other variables

# effect of designed Management after removing the natural variation caused by Moisture
dune.cca <- cca(dune ~ Management + Condition(Moisture), dune.env)
plot(dune.cca)
dune.cca
# Now the total inertia is decomposed into three components: 
# - inertia explained by conditions
# - inertia explained by constraints 
# - remaining unconstrained inertia

# We previously fitted a model with Management as the only constraint
# constrained inertia was clearly higher than now. 
# It seems that different Management was practised in different natural conditions
# and the variation we previously attributed to Management may be due to Moisture.

# can perform permutation tests:

#for Management in conditioned model 
anova(dune.cca, perm.max = 2000)
# for Management alone
anova(cca(dune ~ Management, dune.env))

# alone, Management seemed to be very significant P-value = 0.002
# situation much less clear after removing the variation due to Moisture P-value = 0.059

# anova function can be restricted so that permutation are made only within strata or within a level of a factor variable:
with(dune.env, anova(dune.cca, strata = Moisture))

# Conditioned or partial models are sometimes used for decomposition of inertia into various components attributed to different sets of environmental variables.
# higher-order dependencies are almost certain to appear with high number of variables and high number of groups.



################################## 5 Dissimilarities and environment ##################################

# Sometimes we may wish to analyse vegetation–environment relationships without ordination, or in full space. 
# Typically these methods use the dissimilarity matrix in analysis.


################# 5.1 adonis: Multivariate ANOVA based on dissimilarities #################

# finction adonis()

# adonis studied the differences in the group means

# implements a multivariate analysis of variances using distance matrices
# can handle both continuous and factor predictors

# partitions dissimilarities for the sources of variation, 
# and uses permutation tests to inspect the significances of those partitions.

# adonis to study beta diversity between Management classes in the dune meadow data

# what's beta diversity?
# beta diversity is the slope of species-area curve, 
# or the exponent z of the Arrhenius model
# For pairwise comparison of sites the slope z can be found from the number of species shared between two sites (a) 
# and the number of species unique to each sites (b and c). 
# It is commonly regarded that z ≈ 0.3 implies random sampling variability
# only higher values mean real systematic differences

# finding Arrhenius z with function betadiver
betad <- betadiver(dune, "z")

adonis(betad ~ Management, data = dune.env, perm = 200)
adonis(betad ~ A1*Management, data = dune.env, perm = 200)


################# 5.2 Homogeneity of groups and beta diversity #################

# betadisper studies the differences in group homogeneities
# analogous to Levene’s test of the equality of variances.

# function can only use one factor as an independent variable
# we need to attach the data frame or use with to make the factor visible to the function

mod <- with(dune.env, betadisper(betad, Management))
mod

# graphical display
plot(mod)
boxplot(mod)

# significance of the fitted model analysed:
# - parametrica anova
# - permutation tests (permutest)
anova(mod)
permutest(mod)

# analysis of pairwise differences bw groups with parametric Tukey's HSD test
TukeyHSD(mod)


################# 5.3 Mantel test #################

# compares two sets of dissimilarities
# correlation between dissimilarity entries

# normal signifiocance test not applicable
# function mantel() uses permutation tests

# how well the lichen pastures (varespec) correspond to the environment
# ordination and environment may be non-linearly correlated

# 1. perform a PCA of environmental variables
# 2. compute dissimilarities for first prin- cipal components

# using prcomp ()
# (princomp(), rda(), scores() work as well)
pc <- prcomp(varechem, scale = T)
pc <- scores(pc, display = "sites", choices = 1:4)
edis <- vegdist(pc, method = "euclid")
vare.dis <- vegdist(wisconsin(sqrt(varespec)))
mantel(vare.dis, edis)

# bioenv() -> selecting optimal subset for comparing ordination and environment

# plotting two dissimilarities matrices one against the other
plot(vare.dis, edis)


################# 5.4 Procrustes test #################

# protest() -> compares two ordinations using symmetric Procrustes analysis

# repeating the analysis with the solution of metaMDS and two first PC of the environmental analysis
pc <- scores(pc, choices = 1:2)
pro <- protest(vare.mds, pc)
pro
plot(pro)
# significance is assessed by permutation tests

# used in assessing similarities between different community ordinations
# analysis of congruence










