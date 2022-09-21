# Load Packages ----
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(mosaic)

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

# Means ----
common_name_means <- public_trees %>%
  filter(!is.na(COMMON_NAME), !is.na(TREE_CONDITION_RATING_PERC)) %>%
  group_by(COMMON_NAME) %>%
  summarise(mean = mean(TREE_CONDITION_RATING_PERC)) %>%
  arrange(desc(mean))
head(common_name_means)

GA_mean <- pull(common_name_means %>% filter(COMMON_NAME == "GREEN ASH"), mean)

total_name_mean <- public_trees %>%
  filter(!is.na(COMMON_NAME), !is.na(TREE_CONDITION_RATING_PERC)) %>%
  summarise(mean = mean(TREE_CONDITION_RATING_PERC))



# Proportions ----
# Tally number of trees per Species type
species <- public_trees %>%
  filter(!is.na(SPECIES)) %>% # Filters NAs from species column
  group_by(SPECIES) %>%
  tally() %>%
  arrange(desc(n))

species_plot <- species %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(SPECIES, n), y = n / 1000)) +
  geom_col() +
  xlab("Tree Species") +
  ylab("Total Count (Thousands) - Calgary") +
  ggtitle("Tree Count By Species - Top 10") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Tally number of trees per Genus type and plot
genus <- public_trees %>%
  filter(!is.na(GENUS)) %>% # Filters NAs from species column
  group_by(GENUS) %>%
  tally() %>%
  arrange(desc(n))
head(genus)

genus_plot <- genus %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(GENUS, n), y = n / 1000)) +
  geom_col() +
  xlab("Tree Species") +
  ylab("Total Count (Thousands) - Calgary") +
  ggtitle("Tree Count By Species - Top 10") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Tally number of trees per tree name and plot
tree_names <- public_trees %>%
  filter(!is.na(COMMON_NAME)) %>% # Filters NAs from species column
  group_by(COMMON_NAME) %>%
  tally() %>%
  arrange(desc(n))

tree_names_plot <- tree_names %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(COMMON_NAME, n), y = n / 1000)) +
  geom_col() +
  xlab("Tree Name") +
  ylab("Total Count (Thousands) - Calgary") +
  ggtitle("Tree Count By Common Name - Top 10") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Tally and proportion of trees per community type
name_tally <- public_trees %>%
  filter(!is.na(COMMON_NAME), #Filters NAs from name column
         !is.na(SRG), # Filters NAs from community type column
         SRG != "N/A") %>% # Filters "N/A"s from community type column
  group_by(SRG, COMMON_NAME) %>%
  tally() %>%
  arrange(desc(n)) %>%
  mutate(proportion = n / sum(n))
head(name_tally)

# Tally and proportion of trees per community type
genus_tally <- public_trees %>%
  filter(!is.na(GENUS), #Filters NAs from name column
         !is.na(SRG), # Filters NAs from community type column
         SRG != "N/A") %>% # Filters "N/A"s from community type column
  group_by(SRG, GENUS) %>%
  tally() %>%
  arrange(desc(n)) %>%
  mutate(proportion = n / sum(n))
head(genus_tally)

# Tally and proportion of tree ratings per community type
rating_tally <- public_trees %>%
  filter(!is.na(RATING), #Filters NAs from name column
         RATING != 0, #Filters out 0 RATING trees
         !is.na(SRG), # Filters NAs from community type column
         SRG != "N/A") %>% # Filters "N/A"s from community type column
  group_by(SRG, RATING) %>%
  tally() %>%
  arrange(desc(n)) %>%
  mutate(proportion = n / sum(n))
head(rating_tally)

# Tally and proportion of mature size per community type
mature_tally <- public_trees %>%
  filter(!is.na(MATURE_SIZE), #Filters NAs from name column
         !is.na(SRG), # Filters NAs from community type column
         SRG != "N/A") %>% # Filters "N/A"s from community type column
  group_by(SRG, MATURE_SIZE) %>%
  tally() %>%
  arrange(desc(n)) %>%
  mutate(proportion = n / sum(n))
head(mature_tally)

# Bootstrap Proportions (names) ----

# Proportion of Green Ash in Developing or Built Out Communities

# Number of Green Ash trees in "Built-Out" Communities
X_GA_built <- pull(name_tally %>% filter(SRG == "BUILT-OUT", COMMON_NAME == "GREEN ASH"), n)

# Number of Green Ash trees in "Developing" Communities
X_GA_developing <- pull(name_tally %>% filter(SRG == "DEVELOPING", COMMON_NAME == "GREEN ASH"), n)

# Total number of trees in "Built-Out" Communities
n_built <- public_trees %>%
  filter(!is.na(COMMON_NAME),
         !is.na(SRG),
         SRG != "N/A") %>%
  group_by(SRG) %>%
  tally() %>%
  filter(SRG == "BUILT-OUT") %>%
  pull(n)

# Total number of trees in "Developing" Communities
n_developing <- public_trees %>%
  filter(!is.na(COMMON_NAME),
         !is.na(SRG),
         SRG != "N/A") %>%
  group_by(SRG) %>%
  tally() %>%
  filter(SRG == "DEVELOPING") %>%
  pull(n)

# Build bootstrap distribution for Green Ash in "Built-Out" Communities
GA_built_vector <- c(rep(0, n_built - X_GA_built), rep(1, X_GA_built))
bootstrap_GA_built <- do(1000) * mean(resample(GA_built_vector, n = n_built))
bootstrap_GA_built_plot <- bootstrap_GA_built %>% 
  ggplot(aes(x = mean)) +
  geom_histogram(col = 'blue', fill = 'red', binwidth = 0.0001) +
  xlab("Bootstrap Statistic - Sample Proportion of Green Ash in Built-Out Communities") +
  ylab("Counts") +
  ggtitle("Distribution of Bootstrap Statistic - Sample Proportion of Green Ash in Built-Out Communities")

# Generate statistics and 95% confidence interval
favstats(~ mean, data = bootstrap_GA_built)
qdata(~mean, c(0.025, 0.975), data = bootstrap_GA_built)

# Build bootstrap distribution for Green Ash in "Developing" Communities
GA_developing_vector <- c(rep(0, n_developing - X_GA_developing), rep(1, X_GA_developing))
bootstrap_GA_developing <- do(1000) * mean(resample(GA_developing_vector, n = n_developing))
bootstrap_GA_developing_plot <- bootstrap_GA_developing %>% 
  ggplot(aes(x = mean)) +
  geom_histogram(col = 'blue', fill = 'red', binwidth = 0.0001) +
  xlab("Bootstrap Statistic - Sample Proportion of Green Ash in Developing Communities") +
  ylab("Counts") +
  ggtitle("Distribution of Bootstrap Statistic - Sample Proportion of Green Ash in Developing Communities")

# Generate statistics and 95% confidence interval
favstats(~ mean, data = bootstrap_GA_developing)
qdata(~mean, c(0.025, 0.975), data = bootstrap_GA_developing)

# Build bootstrap distribution for difference in Green Ash proportion
bootstrap_GA_diff <- do(1000) * (mean(resample(GA_developing_vector, n = n_developing)) - mean(resample(GA_built_vector, n = n_built)))
bootstrap_GA_diff_plot <- bootstrap_GA_diff %>% 
  ggplot(aes(x = result)) +
  geom_histogram(col = 'blue', fill = 'red', binwidth = 0.0001) +
  xlab("Bootstrap Statistic - Difference in Sample Proportions of Green Ash") +
  ylab("Counts") +
  ggtitle("Distribution of Bootstrap Statistic - Difference in Sample Proportions of Green Ash")

# Generate statistics and 95% confidence interval
favstats(~ mean, data = bootstrap_GA_diff)
qdata(~mean, c(0.025, 0.975), data = bootstrap_GA_diff)

# Bootstrap Proportions (genus) ----

# Proportion of Picea in Developing or Built Out Communities

# Number of Picea trees in "Built-Out" Communities
X_picea_built <- pull(genus_tally %>% filter(SRG == "BUILT-OUT", GENUS == "PICEA"), n)

# Number of Picea trees in "Developing" Communities
X_picea_developing <- pull(genus_tally %>% filter(SRG == "DEVELOPING", GENUS == "PICEA"), n)

# Total number of trees in "Built-Out" Communities
n_built_genus <- public_trees %>%
  filter(!is.na(GENUS),
         !is.na(SRG),
         SRG != "N/A") %>%
  group_by(SRG) %>%
  tally() %>%
  filter(SRG == "BUILT-OUT") %>%
  pull(n)

# Total number of trees in "Developing" Communities
n_developing_genus <- public_trees %>%
  filter(!is.na(GENUS),
         !is.na(SRG),
         SRG != "N/A") %>%
  group_by(SRG) %>%
  tally() %>%
  filter(SRG == "DEVELOPING") %>%
  pull(n)

# Build bootstrap distribution for Green Ash in "Built-Out" Communities
picea_built_vector <- c(rep(0, n_built_genus - X_picea_built), rep(1, X_picea_built))
bootstrap_picea_built <- do(1000) * mean(resample(picea_built_vector, n = n_built_genus))
bootstrap_picea_built_plot <- bootstrap_picea_built %>% 
  ggplot(aes(x = mean)) +
  geom_histogram(col = 'blue', fill = 'red', binwidth = 0.0001) +
  xlab("Bootstrap Statistic - Sample Proportion of Picea in Built-Out Communities") +
  ylab("Counts") +
  ggtitle("Distribution of Bootstrap Statistic - Sample Proportion of Picea in Built-Out Communities")

# Generate statistics and 95% confidence interval
favstats(~ mean, data = bootstrap_picea_built)
qdata(~mean, c(0.025, 0.975), data = bootstrap_picea_built)

# Build bootstrap distribution for Green Ash in "Developing" Communities
picea_developing_vector <- c(rep(0, n_developing_genus - X_picea_developing), rep(1, X_picea_developing))
bootstrap_picea_developing <- do(1000) * mean(resample(picea_developing_vector, n = n_developing_genus))
bootstrap_picea_developing_plot <- bootstrap_picea_developing %>% 
  ggplot(aes(x = mean)) +
  geom_histogram(col = 'blue', fill = 'red', binwidth = 0.0005) +
  xlab("Bootstrap Statistic - Sample Proportion of Picea in Developing Communities") +
  ylab("Counts") +
  ggtitle("Distribution of Bootstrap Statistic - Sample Proportion of Picea in Developing Communities")

# Generate statistics and 95% confidence interval
favstats(~ mean, data = bootstrap_picea_developing)
qdata(~mean, c(0.025, 0.975), data = bootstrap_picea_developing)

# Build bootstrap distribution for difference in Green Ash proportion
bootstrap_picea_diff <- do(1000) * (mean(resample(picea_developing_vector, n = n_developing_genus)) - mean(resample(picea_built_vector, n = n_built_genus)))
bootstrap_picea_diff_plot <- bootstrap_picea_diff %>% 
  ggplot(aes(x = result)) +
  geom_histogram(col = 'blue', fill = 'red', binwidth = 0.0005) +
  xlab("Bootstrap Statistic - Difference in Sample Proportions of Picea") +
  ylab("Counts") +
  ggtitle("Distribution of Bootstrap Statistic - Difference in Sample Proportions of Picea")

# Generate statistics and 95% confidence interval
favstats(~ mean, data = bootstrap_picea_diff)
qdata(~mean, c(0.025, 0.975), data = bootstrap_picea_diff)