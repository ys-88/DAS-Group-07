library(dplyr)
library(tidyr)
library(rpart)  
library(rpart.plot)
library(randomForest)
library(ggplot2)
library(readr)
library(moderndive)
library(skimr)

film<-read.csv("C:/Users/kqy/Desktop/Data Analysis Skills/Project 2/dataset07.csv")
# Use the mean to fill in missing values (NA) 
film3 <- film %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

film3 %>%
  select(year,length,budget,votes,rating)%>%
  cor()

#Create a binary variable to represent rating
film3$rating_binary <- ifelse(film3$rating > 7, 1, 0)

#ANOVA
anova_result <- aov(rating_binary ~ year + length + budget + votes + genre, data = film3)
summary(anova_result)

#Model selection
# No-linear relationship
# Decision Tree Model
tree_model <- rpart(rating_binary ~ year + length + budget + votes + genre, 
                    data = film3, method = "class")
rpart.plot(tree_model, type=2, extra=4) #visualise the tree

#Random Forest Model
rf_model <- randomForest(rating_binary ~ year + length + budget + votes + genre, 
                         data = film3, ntree = 500, importance = TRUE)
print(rf_model)
importance(rf_model)
varImpPlot(rf_model)

#Reduction of features into Principal Components
#Principal Component Analysis (PCA)
film3$genre <- as.factor(film3$genre)
film3$genre_num <- as.numeric(film3$genre)
film2<-film3%>%
  select(year,length,budget,votes,genre_num)
film2.pca<-princomp(film2, cor=T)
summary(film2.pca)
film2.pca_scores <- film2.pca$scores
#Combine PCA scores with the binary variable for modeling
film2.pca_scores<- as.data.frame(film2.pca_scores)
film2_pca <- cbind(film2.pca_scores, rating_binary = film3$rating_binary)
#Logistic GLM using PCA components
PCA_model <- glm(rating_binary~., data = film2_pca,family = binomial(link = "logit"))
summary(PCA_model)

#logistic GLM
logistic_model <- glm(rating_binary ~ year+budget + length + votes + genre, data = film3, family = binomial(link = "logit"))
summary(logistic_model)
#333









