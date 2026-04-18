# Name: Edward Agyemang | Date: 2024 | Class: ALY 6000
# ============================================================
# ALY 6000 - Project 2: Exploratory Data Analysis
# ============================================================

rm(list = ls())
setwd("C:/Users/elitebook folio/Documents/EDWARD PROJECTS/ALY6000/Project 2")

library(pacman)
p_load(tidyverse, janitor, testthat)

# ============================================================
# PROBLEM 1 - Read the 2015.csv file into R
# ============================================================

# read_csv (tidyverse version) loads the CSV as a tibble - a modern data frame
data_2015 <- read_csv("2015.csv")
head(data_2015)

# ============================================================
# PROBLEM 2 - View all column names
# ============================================================

# names() lists every column name in the dataset
names(data_2015)

# ============================================================
# PROBLEM 3 - View dataset in a separate tab
# ============================================================

# View() opens the dataset like a spreadsheet - DO NOT include in final submission
View(data_2015)

# ============================================================
# PROBLEM 4 - Glimpse the dataset
# ============================================================

# glimpse() shows columns sideways with data types - great for wide datasets
glimpse(data_2015)

# ============================================================
# PROBLEM 5 - Clean column names using janitor
# ============================================================

# clean_names() converts messy names like "Happiness Score" to "happiness_score"
# Removes spaces, special characters, makes everything lowercase
data_2015 <- clean_names(data_2015)
data_2015

# ============================================================
# PROBLEM 6 - Select 4 columns and store as happy_df
# ============================================================

# select() picks only the columns you want from a data frame
happy_df <- data_2015 %>%
  select(country, region, happiness_score, freedom)
happy_df

# ============================================================
# PROBLEM 7 - Get first 10 rows and store as top_ten_df
# ============================================================

# slice() extracts specific row numbers - slice(1:10) gets rows 1 through 10
top_ten_df <- happy_df %>%
  slice(1:10)
top_ten_df

# ============================================================
# PROBLEM 8 - Filter countries with freedom under 0.20
# ============================================================

# filter() keeps only rows that meet your condition
# freedom < 0.20 means keep only countries with very low freedom scores
no_freedom_df <- happy_df %>%
  filter(freedom < 0.20)
no_freedom_df

# ============================================================
# PROBLEM 9 - Sort by freedom highest to lowest
# ============================================================

# arrange() sorts rows. desc() means descending (highest first)
best_freedom_df <- happy_df %>%
  arrange(desc(freedom))
best_freedom_df

# ============================================================
# PROBLEM 10 - Add a new calculated column gff_stat
# ============================================================

# mutate() adds a new column. gff_stat = family + freedom + generosity
data_2015 <- data_2015 %>%
  mutate(gff_stat = family + freedom + generosity)
data_2015

# ============================================================
# PROBLEM 11 - Group by region and compute regional statistics
# ============================================================

# group_by() splits the data into groups (one per region)
# summarise() computes statistics for each group
regional_stats_df <- happy_df %>%
  group_by(region) %>%
  summarise(
    country_count = n(),                      # n() counts rows in each group
    mean_happiness = mean(happiness_score),   # average happiness per region
    mean_freedom   = mean(freedom)            # average freedom per region
  )
regional_stats_df

# ==========================================
# PART 2 — Baseball Dataset (Problems 12–19)
# ==========================================

# ============================================================
# PROBLEM 12 - Load baseball.csv
# ============================================================

baseball <- read_csv("baseball.csv")
head(baseball)
glimpse(baseball)

# ============================================================
# PROBLEM 13 - Explore the baseball dataset
# ============================================================

summary(baseball)    # statistical overview of all columns
nrow(baseball)       # total number of players
ncol(baseball)       # total number of columns
names(baseball)      # column names

# ============================================================
# PROBLEM 14 - Remove players with 0 at bats (AB)
# ============================================================

# filter() keeps only players who actually batted (AB > 0)
baseball <- baseball %>%
  filter(AB > 0)
nrow(baseball)   # should show 726

# ============================================================
# PROBLEM 15 - Calculate Batting Average (BA)
# ============================================================

# BA = Hits divided by At Bats
# mutate() adds the new BA column to baseball
baseball <- baseball %>%
  mutate(BA = H / AB)
baseball

# ============================================================
# PROBLEM 16 - Calculate On-Base Percentage (OBP)
# ============================================================

# OBP = (Hits + Walks) / (At Bats + Walks)
# More complete than BA because it includes walks (BB = Base on Balls)
baseball <- baseball %>%
  mutate(OBP = (H + BB) / (AB + BB))
baseball

# ============================================================
# PROBLEM 17 - Find the 10 players who struck out the most
# ============================================================

# arrange(desc(SO)) sorts by strikeouts highest first
# slice(1:10) takes just the top 10
strikeout_artist <- baseball %>%
  arrange(desc(SO)) %>%
  slice(1:10)
strikeout_artist

# ============================================================
# PROBLEM 18 - Keep only award-eligible players
# ============================================================

# Eligible = at least 300 AB OR at least 100 games played (G)
# The | symbol means OR - either condition makes them eligible
eligible_df <- baseball %>%
  filter(AB >= 300 | G >= 100)
nrow(eligible_df)   # should show 251

# ============================================================
# PROBLEM 19 - Histogram of batting average for eligible players
# ============================================================

ggplot(eligible_df, aes(x = BA)) +
  geom_histogram(fill = "green", color = "darkgreen", bins = 20) +
  labs(
    title = "Distribution of Batting Average — Eligible Players (1986)",
    x     = "BA",
    y     = "count"
  ) +
  theme_gray()

# Automatically saves to your Project 2 folder
ggsave("batting_average_histogram.png", width = 8, height = 6, dpi = 300)

