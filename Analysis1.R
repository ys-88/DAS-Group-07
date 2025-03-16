library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(moderndive)
library(skimr)

film<-read.csv("C:/Users/kqy/Desktop/Data Analysis Skills/Project 2/dataset07.csv")
head(film)
glimpse(film)
summary(film)
#remove missing values
film_clean <- na.omit(film)

film_clean %>%
  select(year,length,budget,votes,rating)%>%
  cor()


#Exploratory data analysis

#Histograms of IMDB ratings 
ggplot(film_clean, aes(x = rating)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "black") +
  labs(x = "IMDB Rating", y = "Frequency", title = "Distribution of IMDB Ratings")

#Barplot of Film Counts by Genre
ggplot(film_clean, aes(x = factor(genre))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(x = "Genre", y = "Count of Films", title = "Count of Films by Genre")

#Scatterplot of Budget vs Rating
ggplot(film_clean, aes(x = budget, y = rating)) +
  geom_point(color = "steelblue") +
  labs(x = "Budget (Millions)", y = "IMDB Rating", title = "Rating vs Budget")+
  geom_hline(yintercept = 7, linetype = "dashed", color = "red", size = 1)

#Scatterplot of Votes vs Rating
ggplot(film_clean, aes(x = votes, y = rating)) +
  geom_point(alpha = 0.5) +
  labs(title = "Votes vs IMDB Rating", x = "Number of Votes", y = "IMDB Rating")+
  geom_hline(yintercept = 7, linetype = "dashed", color = "red", size = 1)
#Scatterplot  of Log(Votes) vs Rating
ggplot(film_clean, aes(x = log(votes), y = rating)) +
  geom_point(alpha = 0.5) +
  labs(title = "Log(Votes) vs IMDB Rating", x = "Log(Votes)", y = "IMDB Rating")+
  geom_hline(yintercept = 7, linetype = "dashed", color = "red", size = 1)

#Scatterplot of Film Length vs Rating
ggplot(film_clean, aes(x = length, y = rating)) +
  geom_point(alpha = 0.5) +
  labs(title = " Film Length vs IMDB Rating", x = " Film Length (Minutes)", y = "IMDB Rating")+
  geom_hline(yintercept = 7, linetype = "dashed", color = "red", size = 1)

#Boxplot of Year vs Rating by duration
film_clean$year_group <- cut(film_clean$year, 
                             breaks = c(1894, 1904, 1914, 1924, 1934, 1944, 1954, 1964, 1974, 1984, 1994,2006), 
                             labels = c(1:11),right=FALSE)  
ggplot(film_clean, aes(x = year_group, y = rating, fill = year_group)) +
  geom_boxplot(na.rm = TRUE) +
  labs(title = "IMDB Rating by Year Group", x = "Year Group", y = "IMDB Rating") +
  geom_hline(yintercept = 7, linetype = "dashed", color = "red", size = 1)+
  scale_fill_discrete(name = "Year Group", 
                      labels = c("1894-1904", 
                                 "1904-1914", 
                                 "1914-1924", 
                                 "1924-1934", 
                                 "1934-1944", 
                                 "1944-1954", 
                                 "1954-1964", 
                                 "1964-1974", 
                                 "1974-1984", 
                                 "1984-1994", 
                                 "1994-2005")) 


# Boxplot of Binary rating
film_clean$rating_group <- ifelse(film_clean$rating > 7, "Above 7", "Below or Equal to 7")
ggplot(film_clean, aes(x = rating_group, y = rating, fill = rating_group)) +
  geom_boxplot() +
  labs(title = "IMDB Rating by Rating Group", x = "Rating Group", y = "IMDB Rating") 



#Model selection

#Creating a binary variable 'rating_above_7' 
film_clean$rating_above_7 <- ifelse(film_clean$rating > 7, 1, 0)
film_clean$genre <- as.factor(film_clean$genre) 

#Principal Component Analysis (PCA)
film_clean$genre_num <- as.numeric(film_clean$genre)
film2<-film_clean%>%
  select(year,length,budget,votes,genre_num)
film2.pca<-princomp(film2, cor=T)
summary(film2.pca)
film2.pca_scores <- film2.pca$scores
#Combine PCA scores with the binary variable for modeling
film2.pca_scores<- as.data.frame(film2.pca_scores)
film2_pca <- cbind(film2.pca_scores, rating_above_7 = film_clean$rating_above_7)
#Logistic GLM using PCA components
model <- glm(rating_above_7~., data = film2_pca,family = binomial(link = "logit"))
summary(model)


#logistic GLM
logistic_model <- glm(rating_above_7 ~ year+budget + length + votes + genre, data = film_clean, family = binomial(link = "logit"))
summary(logistic_model)

