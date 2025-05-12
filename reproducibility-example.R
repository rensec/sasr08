# ------------------------------------------------------------------------------ |
# Simple R- script to demonstrate some principles of reprodicible research for SaSR-08
# Rense Corten, Utrecht Universtity
# First version: 12-5-2025

# Knecht data, Wave V

# R version 4.4.0
# haven_2.5.4     igraph_2.1.1    lubridate_1.9.3 forcats_1.0.0   stringr_1.5.1   dplyr_1.1.4    
# purrr_1.0.2     readr_2.1.5     tidyr_1.3.1     tibble_3.2.1    ggplot2_3.5.1   tidyverse_2.0.0

# ------------------------------------------------------------------------------ |

# ------------------------------------------------------------------------------ |
# preliminaries
# ------------------------------------------------------------------------------ |

library(tidyverse) # for basic data handling
library(igraph) # for network analysis
library(haven) # for reading Stata files

set.seed(1234) # for reproducibility

# ------------------------------------------------------------------------------ |
# Data preparation
# ------------------------------------------------------------------------------ |

# Load data
PupilsWaveV <- read_dta(file = "PupilsWaveV.dta")

# keep only one class for demonstration purposes
class12b <- PupilsWaveV %>% 
  filter(schoolnr == "12b")  

# Create a network object

# Make an edgelist
elist <- class12b %>% 
  select(namenr, friend1:friend12) %>%   # select only the " best friends"  network
  pivot_longer(c(friend1:friend12)) %>% 
  filter(!is.na(value)) %>% # drop the missings
  rename(from ="namenr", to = "value", sourcevar= "name") %>% #just nice for interpretation
  relocate(to, .after=from) #move around the columns


# create the nodeslist

nodelist <- class12b %>% 
  select(namenr,sex, age) %>% 
  rename(name = "namenr")


classnet <- graph_from_data_frame(elist, vertices = nodelist)

# ------------------------------------------------------------------------------ |
# Analysis
# ------------------------------------------------------------------------------ |



# Plot the network
plot(classnet, vertex.label = V(classnet)$name, vertex.size = 5, vertex.color = V(classnet)$sex,
     edge.arrow.size = 0.5, edge.color = "gray", layout = layout_with_fr)

