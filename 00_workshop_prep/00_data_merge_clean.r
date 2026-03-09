# Data wrangling to prep for workshop (Intersession S26)

# Data Prep outside script ---

### Data source: https://www.kaggle.com/datasets/raedaddala/imdb-movies-from-1960-to-2023?resource=download
### Use the merged files
### Saved the 2010-2015 merged files in data/raw folder

# Load libraries ----
library(tidyverse)
library(here)

# Test format & exploring in a single file ----

d2010 <- read.csv("data/raw/merged_movies_data_2010.csv")

summary(d2010) # all variables but Year, Rating, and méta_score are chr


# Merge datasets ----

filenames <- list.files("data/raw", pattern="*.csv", full.names=TRUE)
list_of_years <- map(filenames,read.csv)
movies_initial <- list_rbind(list_of_years)

# Pre-workshop cleaning ----

## Title has "#. Title" format (eg. "1. Toy Story 3")

movies_cleaned <- movies_initial |>
  mutate(Title = str_replace(Title, "^\\S* ", ""))

## Time is in "1h 42m" format - convert to minutes

movies_cleaned <- movies_cleaned |>
  mutate(
    duration_minutes = {
      hours <- as.numeric(str_extract(Duration, "\\d+(?=h)"))
      mins  <- as.numeric(str_extract(Duration, "\\d+(?=m)"))
      (replace_na(hours, 0) * 60) + replace_na(mins, 0)
    }
)

## Column names have varied naming patterns(Movie.Link, genres, Title, 
  ## opening_weekend_Gross, grossWorldWWide) - fix a few for now

movies_cleaned <- movies_cleaned |>
  rename(gross_worldwide = grossWorldWWide,
         gross_opening_wknd = opening_weekend_Gross,
         meta_score = méta_score)

## Budget ----

# Remove everything not in USD ($ at front of string) & the (estimated) part of the string
# Keep missing entries 

## write up the full clean data ---
  
write.csv(movies_cleaned,"data/movie_data_2010-2015_clean.csv", row.names = FALSE)


# Subset to fewer columns ----

## Exclude: Votes,"méta_score" ,"description","Movie.Link","writers","countries_origin","filming_locations","production_company"  

movies_subset <- movies_cleaned |>
  select(Title, 
         Year, 
         duration_minutes,
         MPA,
         Rating,
         meta_score,
         directors,
         stars,
         budget,
         gross_opening_wknd,
         gross_worldwide,
         gross_US_Canada,
         release_date,
         awards_content,
         genres,
         Languages
         )


write.csv(movies_subset,"data/movie_data_2010-2015_clean_subset.csv", row.names = FALSE)

# Additional EDA findings to clean during session? ----
## All variables but Year, Rating, and méta_score are chr
## "(estimated)" in budget column
## Budget column has different currencies (eg, euro and other currency symbols,"CA$10,000) <- maybe subset to 
      ## only those with a $ to start the string (i.e. US films with budget data)?
## Column names have varied naming patterns
## Lots of missing data, especially note in financial columns (eg in 2010, half the films don't have budget)



# Possible question ideas ----
## Something about when in the year a movie opens and revenue?
## Awards column - create a "did it win an Oscar?" variable?
## public vs critic opinion (meta-score)?
