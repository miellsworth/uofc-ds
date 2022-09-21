#Load Packages
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)

# Load Data ----

# Set the working directory where your files are saved
setwd("/Users/Ellsworth/Documents/School/Data\ 602/Project")

# Load in the data
trees <- read_csv("Public_Trees.csv")
comm_info <- read_csv("City_of_Calgary_Community_Areas.csv")

# Clean Data ----

# Change column names in community info to upper case
names(comm_info) <- toupper(names(comm_info))

# Select columns to keep in each dataset
trees_clean <- trees %>%
  select(DECIDUOUS_EVERGREEN,
         COMMON_NAME,
         GENUS,
         SPECIES,
         DBH_CM,
         MATURE_SIZE,
         RATING,
         TREE_CONDITION_RATING_PERC,
         HERITAGE_TREES,
         COMM_CODE)

comm_info_clean <- comm_info %>%
  select(CLASS,
         COMM_CODE,
         NAME,
         SECTOR,
         SRG,
         AREA_SQ_M)

# Merge community information and trees
public_trees <- inner_join(trees_clean, comm_info_clean, by = "COMM_CODE")

# Convert all character strings to upper case
public_trees$DECIDUOUS_EVERGREEN <- str_to_upper(public_trees$DECIDUOUS_EVERGREEN)
public_trees$COMMON_NAME <- str_to_upper(public_trees$COMMON_NAME)
public_trees$GENUS <- str_to_upper(public_trees$GENUS)
public_trees$SPECIES <- str_to_upper(public_trees$SPECIES)
public_trees$MATURE_SIZE <- str_to_upper(public_trees$MATURE_SIZE)
public_trees$CLASS <- str_to_upper(public_trees$CLASS)

# Mean Tree Condition ----

# Tally number of trees per Species type
species_tally <- public_trees %>%
  filter(!is.na(SPECIES)) %>% # Filters NAs from species column
  group_by(SPECIES) %>%
  tally()
head(species_tally)

# Mean tree condition per Species type
tree_cond_per_species <- public_trees %>%
  select(SPECIES, TREE_CONDITION_RATING_PERC) %>% # Selects the species and tree condition columns
  filter(!is.na(SPECIES), # Filters NAs from species column
         !is.na(TREE_CONDITION_RATING_PERC), # Filters NAs from tree condition column
         TREE_CONDITION_RATING_PERC < 100) %>% # Keeps tree condition values that are under 100
  group_by(SPECIES) %>%
  summarise(mean = mean(TREE_CONDITION_RATING_PERC))
head(tree_cond_per_species)

# Mean tree condition for Heritage / Non-Heritage trees
tree_cond_heritage <- public_trees %>%
  select(HERITAGE_TREES, TREE_CONDITION_RATING_PERC) %>% # Selects the species and tree condition columns
  filter(!is.na(HERITAGE_TREES), # Filters NAs from species column
         !is.na(TREE_CONDITION_RATING_PERC), # Filters NAs from tree condition column
         TREE_CONDITION_RATING_PERC < 100) %>% # Keeps tree condition values that are under 100
  group_by(HERITAGE_TREES) %>%
  summarise(mean = mean(TREE_CONDITION_RATING_PERC))
head(tree_cond_heritage)

# Mean tree condition based on mature size
tree_cond_mature_size <- public_trees %>%
  select(MATURE_SIZE, TREE_CONDITION_RATING_PERC) %>% # Selects the species and tree condition columns
  filter(!is.na(MATURE_SIZE), # Filters NAs from species column
         !is.na(TREE_CONDITION_RATING_PERC), # Filters NAs from tree condition column
         TREE_CONDITION_RATING_PERC < 100) %>% # Keeps tree condition values that are under 100
  group_by(MATURE_SIZE) %>%
  summarise(mean = mean(TREE_CONDITION_RATING_PERC))
head(tree_cond_mature_size)

# Relationships ----

# Tree Condition vs Tree Rating
public_trees %>%
  filter(!is.na(TREE_CONDITION_RATING_PERC), !is.na(RATING)) %>%
  filter(TREE_CONDITION_RATING_PERC < 100, RATING > 50) %>%
  ggplot(aes(x = RATING, y = TREE_CONDITION_RATING_PERC)) +
  geom_point(alpha = 0.1)

# Tree Condition vs Diameter of Trunk
public_trees %>%
  filter(!is.na(TREE_CONDITION_RATING_PERC), !is.na(DBH_CM)) %>%
  filter(TREE_CONDITION_RATING_PERC < 100,
         TREE_CONDITION_RATING_PERC > 0,
         DBH_CM > 0) %>%
  ggplot(aes(x = DBH_CM, y = TREE_CONDITION_RATING_PERC)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm")
