library("randomForest", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")

communities.data <- read.csv("~/Desktop/R Final Project/communities.data.txt", header=FALSE)
View(communities.data)

myData = communities.data

names(myData)<-c("state","county","community","communityname","fold","population","householdsize","racepctblack","racePctWhite","racePctAsian","racePctHisp","agePct12t21","agePct12t29","agePct16t24","agePct65up","numbUrban","pctUrban","medIncome","pctWWage","pctWFarmSelf","pctWInvInc","pctWSocSec","pctWPubAsst","pctWRetire","medFamInc","perCapInc","whitePerCap","blackPerCap","indianPerCap","AsianPerCap","OtherPerCap","HispPerCap","NumUnderPov","PctPopUnderPov","PctLess9thGrade","PctNotHSGrad","PctBSorMore","PctUnemployed","PctEmploy","PctEmplManu","PctEmplProfServ","PctOccupManu","PctOccupMgmtProf","MalePctDivorce","MalePctNevMarr","FemalePctDiv","TotalPctDiv","PersPerFam","PctFam2Par","PctKids2Par","PctYoungKids2Par","PctTeen2Par","PctWorkMomYoungKids","PctWorkMom","NumIlleg","PctIlleg","NumImmig","PctImmigRecent","PctImmigRec5","PctImmigRec8","PctImmigRec10","PctRecentImmig","PctRecImmig5","PctRecImmig8","PctRecImmig10","PctSpeakEnglOnly","PctNotSpeakEnglWell","PctLargHouseFam","PctLargHouseOccup","PersPerOccupHous","PersPerOwnOccHous","PersPerRentOccHous","PctPersOwnOccup","PctPersDenseHous","PctHousLess3BR","MedNumBR","HousVacant","PctHousOccup","PctHousOwnOcc","PctVacantBoarded","PctVacMore6Mos","MedYrHousBuilt","PctHousNoPhone","PctWOFullPlumb","OwnOccLowQuart","OwnOccMedVal","OwnOccHiQuart","RentLowQ","RentMedian","RentHighQ","MedRent","MedRentPctHousInc","MedOwnCostPctInc","MedOwnCostPctIncNoMtg","NumInShelters","NumStreet","PctForeignBorn","PctBornSameState","PctSameHouse85","PctSameCity85","PctSameState85","LemasSwornFT","LemasSwFTPerPop","LemasSwFTFieldOps","LemasSwFTFieldPerPop","LemasTotalReq","LemasTotReqPerPop","PolicReqPerOffic","PolicPerPop","RacialMatchCommPol","PctPolicWhite","PctPolicBlack","PctPolicHisp","PctPolicAsian","PctPolicMinor","OfficAssgnDrugUnits","NumKindsDrugsSeiz","PolicAveOTWorked","LandArea","PopDens","PctUsePubTrans","PolicCars","PolicOperBudg","LemasPctPolicOnPatr","LemasGangUnitDeploy","LemasPctOfficDrugUn","PolicBudgPerPop","ViolentCrimesPerPop")


#install.packages("naniar")
library("naniar", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")

myData = myData %>% replace_with_na_all(condition = ~.x == '?')
View(myData)
NumRows = dim(myData)[1]
NumCols = dim(myData)[2]
tol = .25
for(c in NumCols:1)
{
if(sum(is.na(myData[,c])) > tol*NumRows) myData[,c]=NULL
}
summary(myData)


normalize <- function(x) {
    return((x - min(x))/(max(x) - min(x)))
}


#####################################################################
set.seed(2)
myData$OtherPerCap = NULL
notneededFeatures <- c("PctSpeakEnglOnlyCat", "PctNotSpeakEnglWellCat", 
    "PctHousOccupCat", "RentQrange")
possible_predictors = colnames(myData)[!(colnames(myData) %in% 
    notneededFeatures)]
myData = myData[, names(myData) %in% possible_predictors]
myData_normalised <- as.data.frame(lapply(myData, normalize))

data <- sample(2, nrow(myData_normalised), replace = T, prob = c(0.75, 0.25))
traindata_normalised <- myData_normalised[data == 1,]
testdata_normalised <- myData_normalised[data == 2,] 

df.norm.rf <- randomForest(ViolentCrimesPerPop ~ ., traindata_normalised, importance = TRUE, proximity = TRUE)
rf.pred <- predict(df.norm.rf, testdata_normalised)
mean((testdata_normalised$ViolentCrimesPerPop - rf.pred) ^ 2)
#[1] 0.01524507 //MSE



