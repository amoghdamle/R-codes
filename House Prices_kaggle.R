#loading the data 
train<-read.csv(file.choose(),header = TRUE)
test<-read.csv(file.choose(),header = TRUE)
## Viewing the train and test data ##
View(train)
View(test)

##structure of train and test data##
str(train)
str(test)

##dimension of train and test data ##
dim(train)
dim(test)

##summary of train and test data##
summary(train)
summary(test)

## to check if there exists a duplicate row in train data##
duplicates<-nrow(train)-nrow(unique(train))
duplicates

## count of missing data across columns##
colSums(is.na(train))
colSums(is.na(test))

##percentage of missing data in train and test respectively##
percent_missing_data_train<-sum(is.na(train))/(nrow(train)*ncol(train))
percent_missing_data_train
percent_missing_data_test<-sum(is.na(test))/(nrow(test)*ncol(test))
percent_missing_data_test

##Visualising count of missing data in train and test data##
install.packages("VIM")
library(VIM)
aggr_plot_train <- aggr(train, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(train), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
aggr_plot_test<-aggr(test, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(train), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

##removing columns which have high percentage of missing values in train and test data##
train1<-train[,-c(7,73,74,75)]
View(train1) 
test1<-test[,-c(7,73,74,75)]
View(test1)

##Imputing numerical missing values##
train1$LotFrontage[is.na(train1$LotFrontage)]<-mean(train1$LotFrontage,na.rm = TRUE)
train1$MasVnrArea[is.na(train1$MasVnrArea)]<-mean(train1$MasVnrArea,na.rm = TRUE)
train1$GarageYrBlt[is.na(train1$GarageYrBlt)]<-mean(train1$GarageYrBlt,na.rm = TRUE)

##test missing values handling##
test1$LotFrontage[is.na(test1$LotFrontage)]<-mean(test1$LotFrontage,na.rm = TRUE)
test1$MasVnrArea[is.na(test1$MasVnrArea)]<-mean(test1$MasVnrArea,na.rm = TRUE)
test1$GarageYrBlt[is.na(test1$GarageYrBlt)]<-mean(test1$GarageYrBlt,na.rm = TRUE)
test1$GarageCars[is.na(test1$GarageCars)]<-mean(test1$GarageCars,na.rm = TRUE)
test1$GarageArea[is.na(test1$GarageArea)]<-mean(test1$GarageArea,na.rm = TRUE)
test1$BsmtFinSF1[is.na(test1$BsmtFinSF1)]<-mean(test1$BsmtFinSF1,na.rm = TRUE)
test1$BsmtFinSF2[is.na(test1$BsmtFinSF2)]<-mean(test1$BsmtFinSF2,na.rm = TRUE)
test1$BsmtUnfSF[is.na(test1$BsmtUnfSF)]<-mean(test1$BsmtUnfSF,na.rm = TRUE)
test1$TotalBsmtSF[is.na(test1$TotalBsmtSF)]<-mean(test1$TotalBsmtSF,na.rm = TRUE)
test1$BsmtFullBath[is.na(test1$BsmtFullBath)]<-mean(test1$BsmtFullBath,na.rm = TRUE)
test1$BsmtHalfBath[is.na(test1$BsmtHalfBath)]<-mean(test1$BsmtHalfBath,na.rm = TRUE)

##Imputing categorical missing values##
train1$MasVnrType<-replace(train1$MasVnrType,is.na(train1$MasVnrType),"None")
train1$BsmtQual<-replace(train1$BsmtQual,is.na(train1$BsmtQual),"TA")
train1$BsmtCond<-replace(train1$BsmtCond,is.na(train1$BsmtCond),"TA")
train1$BsmtExposure<-replace(train1$BsmtExposure,is.na(train1$BsmtExposure),"No")
train1$BsmtFinType1<-replace(train1$BsmtFinType1,is.na(train1$BsmtFinType1),"Unf")
train1$BsmtFinType2<-replace(train1$BsmtFinType2,is.na(train1$BsmtFinType2),"Unf")
train1$Electrical<-replace(train1$Electrical,is.na(train1$Electrical),"SBrkr")
train1$FireplaceQu<-replace(train1$FireplaceQu,is.na(train1$FireplaceQu),"Gd")
train1$GarageType<-replace(train1$GarageType,is.na(train1$GarageType),"Attchd")
train1$GarageFinish<-replace(train1$GarageFinish,is.na(train1$GarageFinish),"Unf")
train1$GarageQual<-replace(train1$GarageQual,is.na(train1$GarageQual),"TA")
train1$GarageCond<-replace(train1$GarageCond,is.na(train1$GarageCond),"TA")

##test categorical data missing calues handling##
test1$MSZoning<-replace(test1$MSZoning,is.na(test1$MSZoning),"RL")
test1$Utilities<-replace(test1$Utilities,is.na(test1$Utilities),"AllPub")
test1$Exterior1st<-replace(test1$Exterior1st,is.na(test1$Exterior1st),"VinylSd")
test1$Exterior2nd<-replace(test1$Exterior2nd,is.na(test1$Exterior2nd),"VinylSd")
test1$MasVnrType<-replace(test1$MasVnrType,is.na(test1$MasVnrType),"None")
test1$BsmtQual<-replace(test1$BsmtQual,is.na(test1$BsmtQual),"TA")
test1$BsmtCond<-replace(test1$BsmtCond,is.na(test1$BsmtCond),"TA")
test1$BsmtExposure<-replace(test1$BsmtExposure,is.na(test1$BsmtExposure),"No")
test1$BsmtFinType1<-replace(test1$BsmtFinType1,is.na(test1$BsmtFinType1),"Unf")
test1$BsmtFinType2<-replace(test1$BsmtFinType2,is.na(test1$BsmtFinType2),"Unf")
test1$Electrical<-replace(test1$Electrical,is.na(test1$Electrical),"SBrkr")
test1$KitchenQual<-replace(test1$KitchenQual,is.na(test1$KitchenQual),"TA")
test1$Functional<-replace(test1$Functional,is.na(test1$Functional),"Typ")
test1$FireplaceQu<-replace(test1$FireplaceQu,is.na(test1$FireplaceQu),"Gd")
test1$GarageType<-replace(test1$GarageType,is.na(test1$GarageType),"Attchd")
test1$GarageFinish<-replace(test1$GarageFinish,is.na(test1$GarageFinish),"Unf")
test1$GarageQual<-replace(test1$GarageQual,is.na(test1$GarageQual),"TA")
test1$GarageCond<-replace(test1$GarageCond,is.na(test1$GarageCond),"TA")
test1$SaleType<-replace(test1$SaleType,is.na(test1$SaleType),"WD")

##confirm all NA's have been replaced##
colSums(is.na(train1))
colSums(is.na(test1))

##numeric variables from the train data##
numeric_var <-which(sapply(train1, is.numeric))
numeric_var_test<-which(sapply(test1,is.numeric))

##factor variables from train data##
factor_var <-which(sapply(train1, is.factor))
factor_var_test <-which(sapply(test1, is.factor))
## correlation between numeric variables##
install.packages("corrplot")
library(corrplot)
correlation<- cor(train1[c(1,2,4,5,17,18,19,20,26,34,36,37,38,43,44,45,46,47,48,49,50,51,52,54,56,59,61,62,66,67,68,69,70,71,72,73,74,77)])
corrplot(correlation,method = "circle")

## plotting sales price histogram and taking log to normalise it##
hist(train1$SalePrice)
hist(log(train1$SalePrice))

plot(train1$GarageCars,train1$GarageArea)
plot(train1$LotArea,train1$SalePrice)
plot(train1$LotFrontage,train1$SalePrice)
plot(train1$YearBuilt,train1$SalePrice)

## regression model for test data##
install.packages("ModelMetrics")
library(ModelMetrics)
model1<-lm(SalePrice~ MSSubClass+MSZoning+LotFrontage+LotArea+Street+LotShape+LandContour+Utilities+LotConfig+LandSlope+Neighborhood+Condition1+Condition2+BldgType+HouseStyle+OverallQual+OverallCond+YearBuilt+YearRemodAdd+RoofStyle+RoofMatl+Exterior1st+Exterior2nd+MasVnrType+MasVnrArea+ExterQual+ExterCond+Foundation+BsmtQual+BsmtCond+BsmtExposure+BsmtFinType1+BsmtFinSF1+BsmtFinType2+BsmtFinSF2+BsmtUnfSF+TotalBsmtSF+Heating+HeatingQC+CentralAir+Electrical+X1stFlrSF+X2ndFlrSF+LowQualFinSF+GrLivArea+BsmtFullBath+BsmtHalfBath+FullBath+HalfBath+BedroomAbvGr+KitchenAbvGr+KitchenQual+TotRmsAbvGrd+Functional+Fireplaces+FireplaceQu+GarageType+GarageYrBlt+GarageFinish+GarageCars+GarageQual+GarageCond+PavedDrive+WoodDeckSF+OpenPorchSF+EnclosedPorch+X3SsnPorch+ScreenPorch+PoolArea+MiscVal+MoSold+YrSold+SaleType+SaleCondition,train1)
model1
summary(model1)
p<-predict(model1,test1)
p
Output<-model_output <- cbind(test1, p)
Output

write.csv(Output,file = "House_Prices.csv",sep = ',')
getwd()

