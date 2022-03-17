library(gridExtra) 
library(dplyr)
library(ggplot2)
library(janitor)
library(moderndive)
library(infer)
library(broom)
library(readr)

imdb <- read.csv("dataset4.csv")

ggplot(imdb,aes(x=year))+
  geom_histogram()
ggplot(imdb,aes(x=length))+
  geom_histogram()
ggplot(imdb,aes(x=budget))+
  geom_histogram()
ggplot(imdb,aes(x=votes))+
  geom_histogram()

ggplot(imdb,aes(x=rating))+
  geom_histogram()
