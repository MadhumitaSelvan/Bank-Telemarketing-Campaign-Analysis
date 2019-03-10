data <- read.csv(file="C:/Users/Madhumita/Downloads/bank-additional-full.csv",header=TRUE,sep=';')
train <- data
names(train)
library(caTools)
library(ROCR)
library(e1071) 
set.seed(88)
split <- sample.split(train$y, SplitRatio = 0.75)
Train <- subset(train, split == TRUE)
Test <- subset(train, split == FALSE)

#Logistic regression model
model <- glm (y ~ ., data = Train, family = binomial)
summary(model)
predict <- predict(model, type = 'response')
#confusion matrix
d<-table(Train$y, predict > 0.5)
sum(diag(d))/sum(d)
1-sum(diag(d))/sum(d)
ROCRpred <- prediction(predict, Train$y)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
print(ROCRpred)

#SVM
model_svm <- svm(y ~ . , data=Train)
pred <- predict(model_svm, Test, type="prob")
pred_val <-prediction(as.numeric(pred), Test$y)
perf_val <- performance(pred_val,"auc")
perf_val
perf_val <- performance(pred_val, "tpr", "fpr")
plot(perf_val, col = "green", lwd = 1.5)
ks <- max(attr(perf_val, "y.values")[[1]] - (attr(perf_val, "x.values")[[1]]))
ks