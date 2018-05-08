library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")

#Using Random Forest
library("randomForest", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")

agaricus.lepiota.data <- read.csv("~/Desktop/R Final Project/agaricus-lepiota.data.txt", header=FALSE)
View(agaricus.lepiota.data)
myData = agaricus.lepiota.data
names(myData)<-c("classes","cap_shape","cap_surface","cap_color","bruises","odor", "gill_attachment","gill_spacing","gill_size","gill_color","stalk_shape","stalk_root","stalk_surface_above_ring","stalk_surface_below_ring","stalk_color_above_ring","stalk_color_below_ring","veil_type","veil_color","ring_number","ring_type","spore_print_color","population","habitat")

#Removing columns with NAs
myData$veil_type = NULL;
myData$stalk_root = NULL;

#Simple cross validation
#Splitting training data into 75% 25% split
data <- sample(2, nrow(myData), replace = T, prob = c(0.75, 0.25))
traindata <- myData[data == 1,]
testdata <- myData[data == 2,] 

# Applying the algorithm
treeRF <- randomForest(classes~., data = traindata, ntree=100, proximity = T)

# Importance of each variable
dataimp <- varImpPlot(treeRF, main = "Importance of each variable")

# Predictive ability of the model 
testPredRF <- predict(treeRF, newdata = testdata)
prediction_table <- table(testPredRF, testdata$classes)
print(prediction_table)


# Model Accuracy
Accuracy <- sum(testPredRF == testdata$classes)/ length(testdata$classes)*100
print(Accuracy)



