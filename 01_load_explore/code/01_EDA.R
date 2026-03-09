## Session 1: Load and explore

# 3/23/2026


### Load  Packages ----
library(tidyverse)
library(here)

## load Data ----
movies <- read.csv("data/movie_data_2010-2015_clean_subset.csv")

### Explore data ----
head(movies)

colnames(movies)
 
str(movies)
 
lapply(movies,class) # many variables we expect to be numeric are character

summary(movies)


#sample(movies, 10) - this doesn't display well since it has so many long columns

movies_cleaned |> # this doesn't capture the blank character entries - adjust
  summarize(across(everything(), ~sum(is.na(.))))
sum(is.na(movies))

unique(movies$genre)

## simple histogram or distribution for variables related to the questions - budget, rating
# clean accordingly