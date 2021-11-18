#Load packages


#STEP 1
#a)
df <- read.csv('Titanic_updated.csv', stringsAsFactors = TRUE)
df[ ,c(1,2,7,8)] <- lapply(df[,c(1,2,7,8)], factor)
str(df) #does not count toward line count according to Piazza

#b)
lapply(lapply(df,is.na), sum)
df$Age[is.na(df$Age)] <- median(df$Age, na.rm = TRUE)

#STEP 2

#c)
library(caret)
training_indices <- createDataPartition(df$Pclass, p = 0.75)$Resample1
train <- df[training_indices,]
test <- df[-training_indices,]

#STEP 3

#d)
library(e1071)
survived_nb <- naiveBayes(Survived ~., data = train)

#e)
library(tree)
survived_tree <- tree(Survived ~., data = train)

#STEP 4

#f)
library(caret)
predicted_vals_nb <- predict(survived_nb, newdata = test, type = "class")
predicted_vals_tree <- predict(survived_tree, newdata = test, type = "class")
confusionMatrix(data = predicted_vals_nb, reference = test$Survived)
confusionMatrix(data = predicted_vals_tree, reference = test$Survived)

#g)
library(ROCR)
create_ROC <- function(model, test_data){
  test_pred <- predict(model, newdata=test_data, type="class")
  predicted <- prediction(as.numeric(test_pred),as.numeric(test_data$Survived))
  performance <- performance(predicted,"tpr","fpr")
}
perform_nb <- create_ROC(survived_nb, test)
perform_tree <- create_ROC(survived_tree, test)
plot(perform_nb, col = "blue", main = "Naive Bayes vs. Tree")
lines(perform_tree@x.values[[1]], perform_tree@y.values[[1]], col="red")
abline(a = 0, b = 1, lty = 2) #diagonal corresponding to a "random guess"
legend("bottomright",c("Naive Bayes", "Tree"), lty=1, col=c("blue","red"),
       cex = 0.7)

#h)
library(caret)
training_indices2 <- createDataPartition(df$Pclass, p = 0.25)$Resample1
train2 <- df[training_indices2,]
test2 <- df[-training_indices2,]

training_indices3 <- createDataPartition(df$Pclass, p = 0.50)$Resample1
train3 <- df[training_indices3,]
test3 <- df[-training_indices3,]

#i)
library(tree)
survived_tree2 <- tree(Survived ~., data = train2)
survived_tree3 <- tree(Survived ~., data = train3)

#j)
library(caret)
predicted_vals_tree2 <- predict(survived_tree2,newdata = test2,type = "class")
predicted_vals_tree3 <- predict(survived_tree3,newdata = test3,type = "class")
confusionMatrix(data = predicted_vals_tree2, reference = test2$Survived)
confusionMatrix(data = predicted_vals_tree3, reference = test3$Survived)

perform_tree2 <- create_ROC(survived_tree2, test2)
perform_tree3 <- create_ROC(survived_tree3, test3)

plot(perform_tree, col = "blue", 
     main = "Results of Varying Train/Test Proportions")
lines(perform_tree2@x.values[[1]], perform_tree2@y.values[[1]], col="red")
lines(perform_tree3@x.values[[1]], perform_tree3@y.values[[1]], col="green")
abline(a = 0, b = 1, lty = 2) #diagonal corresponding to a "random guess"
legend("bottomright",c("75/25", "25/75", "50/50"), lty=1, 
       col=c("blue","red", "green"), cex = 0.7) #Proportions are Training/Test
