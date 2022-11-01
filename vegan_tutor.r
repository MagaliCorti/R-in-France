##################################################### VEGAN TUTOR #####################################################


################################## Ordination : basic method ##################################


################# Non-metric Multidimensional scaling #################

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



################# Community dissimilarities #################

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



################# Comparing ordinations: Procrustes rotation #################

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



################# Eigenvenctor methods #################

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



################# Detrended Correspondence Analysis #################

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



################# Ordination graphics #################

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



################################## Environmental interpretation ##################################






