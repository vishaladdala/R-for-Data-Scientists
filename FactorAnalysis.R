# We will load a sample dataset to practice on
# Load dataset_EFA.csv
myData = dataset_EFA

# First we use the factanal() function to do FA ...
?factanal

# Let's do FA with 2 factors to start ...
fit <- factanal(myData, 2)
print(fit, digits=2, cutoff=.3, sort=TRUE)

# Note that there is a chi-square test; the null hypothesis is
# that the two factors are sufficient to explain the variance
# in the data. There is not enough evidence to reject this.
names(fit)
load <- fit$loadings[,1:2]
plot(load,type="n")
text(load,labels=names(myData),cex=.7)

# Let's apply this to USAArrests
myData = USArrests

# We will use a new library to try to see how many factors
# to look for ... attach 'nFactors'
# Here we get the eigenvalues for the correlation matrix
ev <- eigen(cor(myData))

# Let's plot and see how many we need ...
ap <- parallel(subject=nrow(myData),var=ncol(myData),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

# Two factors may be enough ...
fit <- factanal(myData, 2)
fit <- factanal(myData, 1)
print(fit, digits=2, cutoff=.3, sort=TRUE)

# This problem does not have many variables to begin with -
# May be hard to extract a few factors
# Try this using the Breast Cancer Dataset from University of 
# Wisconsin - load in the dataset.
# We will drop the first two columns and simply do a factor
# analysis on the remaining information.
myData = wdbc[,-1]
myData = myData[,-1]
dim(myData)
ev <- eigen(cor(myData))
ap <- parallel(subject=nrow(myData),var=ncol(myData),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

# Let's try 2 factors to start. This would make sense -
# there are two different conditions (M and B)
fit <- factanal(myData, 2, lower = 0.1)
?factanal
print(fit, digits=2, cutoff=.3, sort=TRUE)
fit <- factanal(myData, 3, lower = 0.1)
# Notice adding a third factor only accounted for a slight
# increase in the total variance explained - we can get by 
# with two.

# We will look at a dataset included in the 'psych' package
data("bfi")

# The data are the results of a study of 1000 individuals
# who were asked to rate themselves on a Lickert scale of 1 to 
# 6.
dim(bfi)
head(bfi)
hist( bfi$A3, breaks=c(0.5:1:6.5) )
summary( bfi$age )
myData = na.omit(bfi)
dim(bfi)
dim(myData)
ev <- eigen(cor(myData))
ap <- parallel(subject=nrow(myData),var=ncol(myData),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

# We will try to find 5 latent factors
fit <- factanal(myData, 5)
print(fit, digits=2, cutoff=.3, sort=TRUE)

# We can associate these factors with the questions, i.e.
# Factor1 is related to 'grumpiness' while Factor2 seems
# to be related to how introverted/extroverted the person is
# There are other ways to find the factors ...
out <- factanal( covmat=cor( myData, use="complete.obs" ), factors=5, rotation="varimax" )
print( out$loadings, cutoff=0.3 )

# It is interesting to note that these factors only account
# for about 38% of the total variance.
