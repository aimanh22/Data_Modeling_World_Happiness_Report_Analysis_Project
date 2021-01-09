library(arm)
library(pROC)
library(e1071)
library(caret)

predprobs <- fitted(my_model_d[[1]]) 
predprobs[1:3,]
rawresid1 <- (d[[1]]$happinesslevel=="Low") -  predprobs[,1]
rawresid2 <- (d[[1]]$happinesslevel=="Mid") -  predprobs[,2]
rawresid3 <- (d[[1]]$happinesslevel=="High") -  predprobs[,3]

par(mfcol = c(1,3))
(binnedplot(d[[1]]$Log.GDP, rawresid1, xlab = "GDP", ylab = "Raw residuals", main = "Binned plot: viewcat = 1"))
(binnedplot(d[[1]]$Log.GDP, rawresid2, xlab = "GDP", ylab = "Raw residuals", main = "Binned plot: viewcat = 2"))
(binnedplot(d[[1]]$Log.GDP, rawresid3, xlab = "GDP", ylab = "Raw residuals", main = "Binned plot: viewcat = 3"))

(binnedplot(d[[1]]$Life.Expectancy, rawresid1, xlab = "LE", ylab = "Raw residuals", main = "Binned plot: viewcat = 1"))
(binnedplot(d[[1]]$Life.Expectancy, rawresid2, xlab = "LE", ylab = "Raw residuals", main = "Binned plot: viewcat = 2"))
(binnedplot(d[[1]]$Life.Expectancy, rawresid3, xlab = "LE", ylab = "Raw residuals", main = "Binned plot: viewcat = 3"))


(binnedplot(d[[1]]$Social.support, rawresid1, xlab = "SS", ylab = "Raw residuals", main = "Binned plot: viewcat = 1"))
(binnedplot(d[[1]]$Social.support, rawresid2, xlab = "SS", ylab = "Raw residuals", main = "Binned plot: viewcat = 2"))
(binnedplot(d[[1]]$Social.support, rawresid3, xlab = "SS", ylab = "Raw residuals", main = "Binned plot: viewcat = 3"))

(binnedplot(d[[1]]$Perceptions.Corruption, rawresid1, xlab = "PC", ylab = "Raw residuals", main = "Binned plot: viewcat = 1"))
(binnedplot(d[[1]]$Perceptions.Corruption, rawresid2, xlab = "PC", ylab = "Raw residuals", main = "Binned plot: viewcat = 2"))
(binnedplot(d[[1]]$Perceptions.Corruption, rawresid3, xlab = "PC", ylab = "Raw residuals", main = "Binned plot: viewcat = 3"))

(binnedplot(d[[1]]$Freedom.Choices, rawresid1, xlab = "FC", ylab = "Raw residuals", main = "Binned plot: viewcat = 1"))
(binnedplot(d[[1]]$Freedom.Choices, rawresid2, xlab = "FC", ylab = "Raw residuals", main = "Binned plot: viewcat = 2"))
(binnedplot(d[[1]]$Freedom.Choices, rawresid3, xlab = "FC", ylab = "Raw residuals", main = "Binned plot: viewcat = 3"))

(binnedplot(d[[1]]$Generosity, rawresid1, xlab = "Generosity", ylab = "Raw residuals", main = "Binned plot: viewcat = 1"))
(binnedplot(d[[1]]$Generosity, rawresid2, xlab = "Generosity", ylab = "Raw residuals", main = "Binned plot: viewcat = 2"))
(binnedplot(d[[1]]$Generosity, rawresid3, xlab = "Generosity", ylab = "Raw residuals", main = "Binned plot: viewcat = 3"))

pred_classes <- predict(my_model_d[[1]])
Conf_mat <- confusionMatrix(as.factor(pred_classes),as.factor(d[[1]]$happinesslevel))
Conf_mat$table
Conf_mat$overall["Accuracy"];
Conf_mat$byClass[,c("Sensitivity","Specificity")]


## Individual ROC curves for the different levels
#here we basically treat each level as a standalone level
par(mfcol = c(1,3))
roc((d[[1]]$happinesslevel=="Low"),predprobs[,1],plot=T,.thres="best",legacy.axes=T,.auc =T,
    col="red3",percent=T,main="Low Happiness Group")
roc((d[[1]]$happinesslevel=="Mid"),predprobs[,2],plot=T,.thres="best",legacy.axes=T,.auc =T,
    col="gray3",percent=T,main="Mid Happiness Group")
roc((d[[1]]$happinesslevel=="High"),predprobs[,3],plot=T,.thres="best",legacy.axes=T,.auc =T,
    col="green3",percent=T,main="High Happiness Group")
