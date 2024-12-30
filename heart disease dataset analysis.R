library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
heart.data <- read.csv("C:/Users/Paolo/Downloads/testi/heart.data.csv", header = TRUE)
summary(lm(heart.disease ~ biking, data = heart.data))
summary(lm(heart.disease ~ smoking, data = heart.data))
heart.disease.lm <- lm(heart.data$heart.disease ~ heart.data$biking + heart.data$smoking, data = heart.data)
summary(heart.disease.lm)
plotting.data<-expand.grid(
  biking = seq(min(heart.data$biking), max(heart.data$biking), length.out = 30),
  smoking = c(min(heart.data$smoking), mean(heart.data$smoking), max(heart.data$smoking))
)
plotting.data
predictions <- predict(heart.disease.lm,plotting.data)
predictions
plotting.data[2]<-round(plotting.data[2],digit=2)
smokingfactor <- as.factor(plotting.data[2])
ggplot(heart.data, aes(x = biking, y = heart.disease)) +
  geom_point(aes(color = smoking)) + geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Biking", y = "Rate of Heart Disease", size = "Heart Disease") + theme_minimal()