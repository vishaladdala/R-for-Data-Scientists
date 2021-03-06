#Heirarchical Clustering
# First we do hclustering with the iris dataset

iris.scaled = scale(iris[,-5])

# We will do three different dendograms, for three different
# linkage methods

iris.complete = hclust(dist(iris.scaled), method="complete")
iris.average = hclust(dist(iris.scaled), method="average")
iris.single = hclust(dist(iris.scaled), method="single")

# We plot the dendograms side by side for comparison

par(mfrow=c(1,3))
plot(iris.complete,main="Complete Linkage", xlab="", sub="", cex=.9,labels = FALSE)
plot(iris.average, main="Average Linkage", xlab="", sub="", cex=.9,labels = FALSE)
plot(iris.single, main="Single Linkage", xlab="", sub="", cex=.9,labels = FALSE)

# Here is how we can access the clustering for any number of
# clusters

cutree(iris.complete,2)
cutree(iris.average,3)

# Let's see how the method worked ...

table(cutree(iris.complete,3), iris[,5])
table(cutree(iris.average,3), iris[,5])
table(cutree(iris.single,3), iris[,5])

# Answer: Not well. But interestingly, if we use fewer
# variables ...

iris.average.restr = hclust(dist(iris.scaled[,3:4]), method="average")
table(cutree(iris.average.restr,3), iris[,5])

# Note the final result does not tell us much

iris.average

# Here we use correlation between predictors for our clustering
# We look at historical stock prices for the SP500. We will
# try to cluster stocks based upon correlation.
# First, load the dataset sp500hst.txt

View(sp500hst)

# We need to do some manipulations, and we will use the 
# tidyr package.

myData = sp500hst
colnames(myData)=c("Date","Stock","Open","High","Low","Close","Volume")
View(myData)

# attach package "tidyr"

myData2 = myData[,c("Date","Stock","Close")]
dim(myData2)

# The spread function allows us to rehape the data so we
# have a closing stock price for each stock for each day
# It makes a long dataset wide

myData3 = spread(myData2, Stock, Close, fill = NA, convert = FALSE, drop = TRUE,
                 sep = NULL)
dim(myData3)
myData4 = na.omit(myData3)
dim(myData4)

# We will need to be more careful on removing NAs!
# First, we need to do more cleanup - we can remove
# the date column, and and then we need to transpose
# the data to get the stocks as rows

myData4 = myData3
myData4$Date=NULL
myData5 = t(myData4)  
dim(myData5)

# Not all rows are complete - some stocks joined and others 
# left the SP500 during the time period. We correct for this

myData6 = myData5[complete.cases(myData5),]
dim(myData6)

# We could have also done an na.omit on myData5 at this point.

# There are 379 stocks that were in the index for the entire
# year. Next we convert the data to a percentage daily
# change, rather than a closing price.

myData6[1,1]
myData7 = myData6
for(i in 1:244){
  myData7[,i] = (myData6[,i+1] - myData6[,i])/myData6[,i]}
dim(myData7)
myData7[1,1]
myData[1,245]

# We drop the last column - note that myData is a matrix, not
# a dataframe at this point.

myData8 = myData7[,-245]

# We can now compute a distance matrix for myData8 using
# correlation ...

SP500_corr=as.dist(1-cor(t(myData8)))
SP500.Ward = hclust(SP500_corr, method="ward.D2")
par(mfrow=c(1,1))
plot(SP500.Ward, labels = FALSE)

# Maybe 6 groups of stocks? Let's try to find the groups

SP500.Groups=cutree(SP500.Ward,6)
SP500.Groups==1
x =as.vector(names(SP500.Groups))
x[1]
GP_1 = x[SP500.Groups==1]
GP_1
sink("hiclusteringop.txt")
sink()

wine.complete = hclust(dist(myData_Scaled), method="complete")
 wine.average = hclust(dist(myData_Scaled), method="average")
 wine.single = hclust(dist(myData_Scaled), method="single")
 
 # We plot the dendograms side by side for comparison
par(mfrow=c(1,1))
 plot(wine.complete,main="Complete Linkage", xlab="", sub="", cex=.9,labels = FALSE)
plot(wine.average, main="Average Linkage", xlab="", sub="", cex=.9,labels = FALSE)
 plot(wine.single, main="Single Linkage", xlab="", sub="", cex=.9,labels = FALSE)




