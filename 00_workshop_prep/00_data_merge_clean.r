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

### optional - extract and review all non-number sequences of characters in budget column
tokens <- movies_cleaned |> 
  pull(budget) |>
  str_extract_all("[^0-9,. ]+") |>
  unlist() |>
  tibble(token = _) |>
  count(token, sort = TRUE)

### extract non-numbers from budget column
### if "(estimated)" set `budget_estimated` = TRUE
### other non-number text is placed into `budget_currency`
movies_cleaned <- movies_cleaned |>
  mutate(
    budget_estimated = str_detect(budget, fixed("(estimated)")),
    budget_value = budget |>
      str_remove_all("[^0-9.]") |>
      as.numeric(),
    budget_currency = budget |>
      str_extract("[^0-9,. (estimated)]+") |>
      str_trim()
  )

### do the same thing with gross_worldwide column
movies_cleaned <- movies_cleaned |>
  mutate(
    grossww_value = gross_worldwide |>
      str_remove_all("[^0-9.]") |>
      as.numeric(),
    # grossw_estimate = str_detect(gross_worldwide, fixed("(estimated)")),
    grossww_currency = gross_worldwide |>
      str_extract("[^0-9,. (estimated)]+") |>
      str_trim()
  )

## Split date column
### will produce warning about some dates failing to parse
### coalesce fills incomplete date values with "01-01"
### date_partial = TRUE to flag these cases
movies_cleaned <- movies_cleaned |>
  mutate(
    # parse "June 18, 2010" → proper Date object
    date   = mdy(release_date) |> coalesce(ymd(str_c(release_date, "-01-01"))),
    year   = year(date),
    month  = month(date),
    day    = day(date),
    daynum = yday(date),      # day of year, 1–366
    date_partial  = is.na(mdy(release_date)) & !is.na(date),  # TRUE = year-only entry
  )

## write up the full clean data ---
  
write.csv(movies_cleaned,"data/movie_data_2010-2025_clean.csv", row.names = FALSE)

colnames(movies_cleaned)



# Subset to fewer columns ----

## Exclude: Votes,"description","Movie.Link","writers","countries_origin","filming_locations","production_company"  

movies_subset <- movies_cleaned |>
  select(Title,
         # Year, 
         duration_minutes,
         MPA,
         Rating,
         meta_score,
         genres,
         date,
         year,
         month,
         day,
         daynum,
         date_partial,
         # budget,
         # gross_opening_wknd,
         # gross_worldwide,
         # gross_US_Canada,
         # release_date,
         budget_estimated,
         budget_value,
         budget_currency,
         grossww_value,
         grossww_currency,
         directors,
         stars,
         awards_content,
         Languages
         )

write.csv(movies_subset,"data/movie_data_2010-2025_clean_subset.csv", row.names = FALSE)

# FILTER DATASET - KEEP ONLY FILMS WITH "$" CURRENCY FOR 
# BUDGET AND GROSS_WORLDWIDE
## note: non-US $ are separate, i.e. "CA$" but we should review
movies_subset_dollars <- movies_subset |>
  filter(budget_currency == "$" & grossww_currency == "$")

write_csv(movies_subset_dollars, "data/movie_data_2010-2025_clean_dollars.csv")

# Additional EDA findings to clean during session? ----
## All variables but Year, Rating, and méta_score are chr
## [x] "(estimated)" in budget column
## [x] Budget column has different currencies (eg, euro and other currency symbols,"CA$10,000) <- maybe subset to 
      ## only those with a $ to start the string (i.e. US films with budget data)?
## Column names have varied naming patterns
## Lots of missing data, especially note in financial columns (eg in 2010, half the films don't have budget)



# Possible question ideas ----
## Something about when in the year a movie opens and revenue?
## Awards column - create a "did it win an Oscar?" variable?
## public vs critic opinion (meta-score)?
