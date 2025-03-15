library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(moderndive)
library(skimr)

film<-read.csv("C:/Users/kqy/Desktop/Data Analysis Skills/Project 2/dataset07.csv")


#Exploratory data analysis

film$genre <- factor(film$genre, levels = c("Romance", "Short", "Documentary", "Animation", "Comedy", "Drama", "Action"))
ggplot(data = film, mapping = aes(x = genre, y = rating)) +
  geom_boxplot(fill = "steelblue") +
  labs(x = "Genre", y = "IMDB rating", title = "Film Rating by Genre") +
  scale_x_discrete(labels = c("Romance", "Short", "Documentary", "Animation", "Comedy", "Drama", "Action")) +
  geom_hline(yintercept = 7, linetype = "dashed", color = "red", size = 1)


#logistic GLM
film$rating_above_7 <- ifelse(film$rating > 7, 1, 0)
logistic_model <- glm(rating_above_7 ~ budget + length + votes + genre, data = film, family = "binomial")
summary(logistic_model)