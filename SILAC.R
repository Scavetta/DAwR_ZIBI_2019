# SILAC analysis
# Rick Scavetta
# 25.11.2019
# DAwR workshop for ZIBI - Case study

# clear workspace
rm(list = ls())

# Load packages
library(tidyverse)

# Exercise 8.1 (Import and Examine) ----
# Import data:
protein.df <- read.delim("Protein.txt", stringsAsFactors = FALSE)

# Explore the data
ncol(protein.df)
names(protein.df)
glimpse(protein.df)
str(protein.df)
summary(protein.df)

# Convert to a tibble
protein.df # pretty bad default print method

protein.df <- as_tibble(protein.df)
protein.df # now it prints nicely

# Exercise 9.1 (Remove contaminants) ----
protein.df %>%
  filter(Contaminant == "+") %>% 
  nrow()

sum(protein.df$Contaminant == "+")
table(protein.df$Contaminant)
summary(protein.df$Contaminant)

# proportion of contaminants:
sum(protein.df$Contaminant == "+")/nrow(protein.df)

# percentage?
sum(protein.df$Contaminant == "+")/nrow(protein.df) * 100

# Remove them?
protein.df %>% 
  filter(Contaminant != "+") -> protein.df

# plot the ratios (raw)
ggplot(protein.df, aes(Ratio.H.M)) +
  geom_density()



# Exercise 8.2 (Clean-up and Transform) ----
# Log10 transformation of the 3 intensity columns (H, M and L)
protein.df$Intensity.H <- log10(protein.df$Intensity.H)
protein.df$Intensity.M <- log10(protein.df$Intensity.M)
protein.df$Intensity.L <- log10(protein.df$Intensity.L)

# Add transformed intensities (H + M and M + L)
protein.df$Intensity.H.M <- protein.df$Intensity.H + protein.df$Intensity.M
protein.df$Intensity.M.L <- protein.df$Intensity.M + protein.df$Intensity.L

# log2 transformation of the 2 ratios of interest (H.M and M.L)
protein.df$Ratio.H.M <- log2(protein.df$Ratio.H.M)
protein.df$Ratio.M.L <- log2(protein.df$Ratio.M.L)

# Exercise 9.2 (Find protein values) (use filter()) ----
# Exercise 10.1 (Find protein values) (use [])
# Define a vector for proteins of interest 
myProteins <- c("GOGA7", "PSA6", "S10AB")

# Add _MOUSE ----
# to all values in myProteins
myProteins <- paste0(myProteins, "_MOUSE")

protein.df %>%
  filter(Uniprot %in% myProteins) %>% 
  select(Uniprot, Ratio.H.M, Ratio.M.L)


# Removing _MOUSE ----
protein.df$Uniprot_2 <- str_remove_all(protein.df$Uniprot, "_MOUSE")
myProteins <- c("GOGA7", "PSA6", "S10AB")

protein.df %>%
  filter(Uniprot_2 %in% myProteins) %>% 
  select(Uniprot_2, Ratio.H.M, Ratio.M.L)

# Basic pattern matching ----
# Can we pattern match the Uniprot names?
# One items:
protein.df %>% 
  filter(str_detect(Uniprot, "Q3UF01"))

# Two items:
protein.df %>% 
  filter(str_detect(Uniprot, "Q3UF01|Q3T9P2"))


# Exercise 9.3 (Find significant hits) (use filter()) ----
# Exercise 10.2 (Find significant hits) (use [])

# Exercise 9.4 (Find extreme values) (use filter()) ----
# Exercise 10.3 (Find extreme values) (use [])

# Exercise 10.4 (Find top 20 values) ----
# (see section 10.5 for functions)
protein.df %>% 
  arrange(desc(Ratio.H.M)) %>% 
  slice(1:20)
# alternatively, instead of slice
# head(20)

protein.df %>% 
  top_n(20, Ratio.H.M) -> topHM
(2792-2721)/12
2720*3  
protein.df %>% 
  top_n(20, Ratio.M.L) -> topML

# Exercise 10.5 (Find intersections) ----
# (see section 10.6 for functions)
intersect(topML, topHM) # 2 values
union(topML, topHM) # 38 values
