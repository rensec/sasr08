library(igraph)
library(tidyverse)
library(reshape2)

# Load the data
url1 <- "https://github.com/rensec/sasr08/raw/main/toy_name_generator_data.csv"
df <- read.csv(file = url1)
table(df$schoolnr)

# Create a graph for class 1a
cls <- "1a"
edge_list <- df %>% 
  filter(schoolnr == cls) %>% 
  select(namenr, friend1:friend2) %>% 
  melt(id.vars = "namenr") %>% #make long
  filter(!is.na(value)) %>% # drop the missings
  rename(from ="namenr", to = "value", sourcevar= "variable") %>% #just nice for interpretation
  relocate(to, .after=from) #move around the columns
edge_list

nodelist <- df %>% 
  filter(schoolnr == cls) %>% 
  select(namenr,age)

g1a <- graph_from_data_frame(edge_list, vertices = nodelist)
plot(g1a)

# create a dyad-as-case dataset from this graph, with one row per combination of nodes, and a variable indicating whether they are connected.
dyads <- get.data.frame(g1a, what = "edges") # start with all edges
dyads$connected <- 1 # add a variable indicating that they are connected

dyads <- dyads %>% 
  full_join(expand.grid(from = V(g1a)$name, to = V(g1a)$name), by = c("from", "to")) %>% # create a list of all combinations of nodes, and match this to whether they are connected
  mutate(connected = ifelse(is.na(connected), 0, connected)) %>%  # if there is no connection, set connected to 0
  select(from, to, connected) # keep only from, to and connected


