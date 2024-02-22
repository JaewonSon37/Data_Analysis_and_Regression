## Basic KNN
library(tidyverse)
library(ggplot2)
library(caret)
library(mlbench)

data("PimaIndiansDiabetes")
head(PimaIndiansDiabetes)

set.seed(337)
ctrl <- trainControl(method = "cv", number = 10)
knnFit <- train(diabetes ~ ., data = PimaIndiansDiabetes,
                method = "knn", trControl = ctrl,
                preProcess = c("center", "scale"))
knnFit

set.seed(338)
ctrl <- trainControl(method = "cv", number = 10)
knnFit <- train(diabetes ~ ., data = PimaIndiansDiabetes,
                method = "knn", trControl = ctrl,
                preProcess = c("center", "scale"),
                tuneLength = 15)
plot(knnFit)

## Distance Functions
library(kknn)

tuneGrid <- expand.grid(kmax = 3:7, kernel = c("rectangular", "cos"),
                        distance = 1:3)
kknn_fit <- train(diabetes ~ ., data = PimaIndiansDiabetes,
                  method = "kknn", trControl = ctrl,
                  preProcess = c("center", "scale"),
                  tuneGrid = tuneGrid)
kknn_fit

# Applying the Model
pred_knn <- predict(kknn_fit, PimaIndiansDiabetes)
confusionMatrix(PimaIndiansDiabetes$diabetes, pred_knn)

# Extracting the Results Table
knn_results <- kknn_fit$results
head(knn_results)

knn_results <- knn_results %>%
  group_by(kmax, kernel) %>%
  mutate(avgacc = mean(Accuracy))
head(knn_results)

ggplot(knn_results, aes(x = kmax, y = avgacc, color = kernel)) + 
  geom_point(size = 3) + geom_line()