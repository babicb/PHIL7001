#First, download the data
#second, create a new script,  copy and paste, try not omitting any 
#third, modify the file path (check your own one)
#finally, run the codes

install.packages("tidyverse") # if you have already installed it, then you do not have to include this line, but it is ok if you install it for the second time
library(tidyverse)
library(dplyr)
library(stats)
library(ggplot2)
library(readr)
titanic <- read_csv('/Users/townking/Desktop/week2/train_and_test2.csv') # check your own path, and modify the codes accordingly
head(titanic)
nrow(titanic)
titanic_cleaned <- titanic %>% 
  rename("Age"="Age", "Sex"="Sex", "Class"="Pclass", "Survived"="2urvived") %>%
  select("Age","Sex","Class","Survived")
head(titanic_cleaned)
unique(titanic_cleaned$Sex)
table(titanic_cleaned$Sex)
#ggplot(data = xx, mapping =  aes(x=exercise, y=body_fat))
ggplot(titanic_cleaned, aes(x = factor(Sex, labels = c("Male", "Female")), fill = as.factor(Survived))) +
  geom_bar(position = "stack") +
  labs(x = "Sex", y = "Count", fill = "Survived")+
  scale_fill_discrete(labels = c("No", "Yes"), name = "Survived") 
model <- glm(Survived ~ Sex + Age + Class, data = titanic_cleaned, family = binomial())
summary(model)
