sink("output.txt")
library(readr)
Car_Worth_Train <- read_csv("~/Desktop/R/Lecture-2-Data/Car Worth - Train.csv")
View(Car_Worth_Train)
myData = Car_Worth_Train
myData = as.data.frame(unclass(myData))
dim(myData)
head(myData)
names(myData)
str(myData)
myData[,3]
sink(
  
)
library(readr)
AmesHousing <- read_csv("~/Desktop/R/Lecture-2-Data/AmesHousing.csv")
View(AmesHousing)
myData = AmesHousing
myData = as.data.frame(unclass(myData))
dim(myData)
names(myData)
summary(myData)
myData$Misc.Feature = NULL
myData$Fence = NULL
myData$Pool.QC=NULL
myData$Fireplace.Qu=NULL
myData$Alley=NULL
myDataClean = na.omit(myData)
dim(myDataClean)
summary(myDataClean)
attach(myDataClean)
plot(SalePrice~Mo.Sold)
myDataClean$Mo.Sold = as.factor(myDataClean$Mo.Sold)
sink()

