library(MASS)
library(rpart)
library(e1071)
library(randomForest)
library(adabag)

superstore <- read.csv("datasets/Superstore.csv")

info <- c(
  "Year_Birth", "Complain", "Dt_Customer", "Education",
  "Kidhome", "Marital_Status", "Teenhome", "Income"
)
behav <- c(
  "MntWines", "MntFruits", "MntMeatProducts", "MntFishProducts",
  "MntSweetProducts", "MntGoldProds", "NumDealsPurchases",
  "NumWebPurchases", "NumCatalogPurchases", "NumStorePurchases",
  "NumWebVisitsMonth", "Recency"
)

superstore <- superstore[complete.cases(superstore), ]

superstore$Dt_Customer <- as.Date(superstore$Dt_Customer, format = "%m/%d/%Y")
superstore$Dt_Customer <- as.numeric(
  difftime(max(superstore$Dt_Customer), superstore$Dt_Customer, units = "days")
)

hist(superstore$Income)
hist(superstore$Year_Birth)
hist(superstore$Dt_Customer)
hist(superstore$NumWebVisitsMonth)
hist(superstore$Recency)
hist(superstore$NumWebPurchases)
hist(superstore$NumStorePurchases)
hist(superstore$NumCatalogPurchases)
hist(superstore$NumDealsPurchases)
hist(superstore$MntWines)
hist(superstore$MntFruits)
hist(superstore$MntMeatProducts)
hist(superstore$MntFishProducts)
hist(superstore$MntSweetProducts)
hist(superstore$MntGoldProds)

lda_model <- lda(Response ~ ., data = superstore[, c(behav, "Response")])
lda_pred <- predict(lda_model, superstore[, behav])
mat <- table(lda_pred$class, superstore$Response)
mat
1 - sum(diag(mat)) / sum(mat)

qda_model <- qda(Response ~ ., data = superstore[, c(behav, "Response")])
qda_pred <- predict(qda_model, superstore[, behav])
mat <- table(qda_pred$class, superstore$Response)
mat
1 - sum(diag(mat)) / sum(mat)

tree.par <- rpart.control(minsplit = 25, minbucket = 5, cp = 0, xval = 10, maxdepth = 30)

tree_model <- rpart(
  Response ~ .,
  data = superstore[, c(behav, "Response")],
  method = "class",
  control = tree.par
)
tree_pred <- predict(tree_model, superstore[, behav], type = "class")
mat <- table(tree_pred, superstore$Response)
mat
1 - sum(diag(mat)) / sum(mat)

svm_model <- svm(
  Response ~ .,
  data = superstore[, c(behav, "Response")],
  kernel = "radial",
  scale = FALSE,
  type = "C"
)
svm_pred <- predict(svm_model, superstore[, behav], type = "class")
mat <- table(svm_pred, superstore$Response)
mat
1 - sum(diag(mat)) / sum(mat)

tree_model_all <- rpart(
  Response ~ .,
  data = superstore[, c(info, behav, "Response")],
  method = "class",
  control = tree.par
)
tree_pred_all <- predict(tree_model_all, superstore[, c(info, behav)], type = "class")
mat <- table(tree_pred_all, superstore$Response)
mat
1 - sum(diag(mat)) / sum(mat)

svm_model_all <- svm(
  Response ~ .,
  data = superstore[, c(info, behav, "Response")],
  kernel = "radial",
  scale = FALSE,
  type = "C"
)
svm_pred_all <- predict(svm_model_all, superstore[, c(info, behav)], type = "class")
mat <- table(svm_pred_all, superstore$Response)
mat
1 - sum(diag(mat)) / sum(mat)

superstore$Response <- as.factor(superstore$Response)

rf_model <- randomForest(
  Response ~ .,
  data = superstore[, c(behav, "Response")],
  ntree = 600,
  mtry = 3,
  importance = TRUE,
  proximity = TRUE
)
rf_pred <- predict(rf_model, superstore[, behav], type = "class")
mat <- table(rf_pred, superstore$Response)
mat
1 - sum(diag(mat)) / sum(mat)

superstore$Marital_Status <- as.factor(superstore$Marital_Status)
superstore$Education <- as.factor(superstore$Education)

ada.par <- rpart.control(maxdepth = 4)
adaboost_model <- boosting(
  Response ~ .,
  data = superstore[, c(behav, "Response")],
  mfinal = 500,
  control = ada.par
)
adaboost_pred <- predict.boosting(adaboost_model, superstore[, behav], type = "class")
mat <- table(adaboost_pred$class, superstore$Response)
mat
1 - sum(diag(mat)) / sum(mat)
