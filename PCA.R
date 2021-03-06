# We'll start with the USArrests dataset
?USArrests
summary(USArrests)
myData = scale(USArrests)
pca_arrests = prcomp(myData)
?prcomp
pca_arrests

# This gives the PC loading vectors
# Let's do a biplot for the first two PCs

biplot(pca_arrests,scale=0)
?biplot

# Not very readable ... let's try some alternatives
biplot(pca_arrests, expand=10, xlim=c(-0.30, 0.0), ylim=c(-0.1, 0.1))
biplot(pca_arrests, expand=10, xlim=c(-0.60, 0.0), ylim=c(-0.1, 0.1))
biplot(pca_arrests,cex=c(1/2,1/2), scale=0)
biplot(pca_arrests,cex=c(1/3,1/2), scale=0)

# Next load the Turkey Student Evaluation Data

myData=turkiye.student.evaluation_R_Specific
summary(myData)

# We drop the first column (instructor ID?)

myData = turkiye.student.evaluation_R_Specific[-1]

# Do the PCA, scale the data 

pca_studeval = prcomp(myData, scale=TRUE, center=TRUE)

# Let's try some plots

biplot(pca_studeval,cex=c(1/3,1/2), scale=0)
biplot(pca_studeval, expand=10, xlim=c(-0.60, 0.0), ylim=c(-0.1, 0.1))
biplot(pca_studeval, expand=10, xlim=c(-0.60, 0.6), ylim=c(-0.6, 0.1))
biplot(pca_studeval, expand=10, xlim=c(-0.60, 0.6), ylim=c(-0.2, 0.1))
pca_studeval
