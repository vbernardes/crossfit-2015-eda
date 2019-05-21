# DATA CLEANING

library(dplyr)
library(readr)
setwd('/Users/vitorsb/Documents/Cursos/Udacity Data Analyst/P4 - EDA/project/crossfit')

# Read original data
athletes <- read.csv('original_data/bgadoci-crossfit-data/data/athletes.csv')
leaderboard <- read.csv('original_data/bgadoci-crossfit-data/data/leaderboard_15.csv')
# join on athlete ids
cf <- leaderboard %>%
  inner_join(athletes, by="athlete_id")

# Remove unwanted columns
cf_clean <- cf[-c(1, 7, 29:33, 35)]

# Change division
colnames(cf_clean)[1] <- 'unclean_division'
cf_clean$division <- ifelse(cf_clean$unclean_division == 1, 'Male', 'Female')
cf_clean$division <- as.factor(cf_clean$division)
cf_clean <- cf_clean[, c(28, 2:27)] # remove 'unclean_division' column

# Create 'Rx vs scaled' category
cf_clean$category <- ifelse(cf_clean$scaled == 'false', 'Rx', 'Scaled')
cf_clean$category <- as.factor(cf_clean$category)
cf_clean <- cf_clean[, c(1:5, 27:28, 6:26)]    # reorder columns

# Clean gender
cf_clean$gender[cf_clean$gender == '--'] <- NA
cf_clean$gender <- factor(cf_clean$gender)

# Clean howlong
valid_howlong <- ordered(c('Less than 6 months|', '6-12 months|', '1-2 years|', '2-4 years|', '4+ years|'))
cf_clean[!(cf_clean$howlong %in% valid_howlong), ]$howlong <- NA
cf_clean$howlong <- factor(cf_clean$howlong)
levels(cf_clean$howlong) <- valid_howlong

# Clean negative pullup values
cf_clean$pullups[cf_clean$pullups < 0] <- NA

# Set benchmarks to 'NA' if they are 0
cf_clean[, c(17:28)][cf_clean[, c(17:28)] == 0] <- NA

# Clean weight that might have been input as kg instead of lbs
cf_clean[cf_clean$division == 'Male' &
           !is.na(cf_clean$weight) &
           cf_clean$weight <= 120, ]$weight <- NA
cf_clean[cf_clean$division == 'Female' &
           !is.na(cf_clean$weight) &
           cf_clean$weight <= 90, ]$weight <- NA

# Write out cleaned data
write_csv(cf_clean, 'crossfit2015.csv')
