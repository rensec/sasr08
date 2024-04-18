library(igraph)
library(tidyverse)
library(reshape2)
library(haven)

df <- read_dta(file = "PupilsWaveV.dta")

#df <- df %>% 
#  filter(schoolnr == '01a' | schoolnr == '01b' | schoolnr == '01c')


df <- df %>% 
  mutate(id_pupil = paste0(schoolnr,namenr))

edge_list <- df %>% 
  select(id_pupil, schoolnr, friend1:friend12) %>% 
  melt(id.vars = c("id_pupil","schoolnr")) %>% #make long
  filter(!is.na(value)) %>% # drop the missings
  rename(from ="id_pupil", to = "value", sourcevar= "variable") %>% #just nice for interpretation
  relocate(to, .after=from) #move around the columns


edge_list <- edge_list %>% 
  mutate(to = paste0(schoolnr,to))


all_vertices <- edge_list %>%
  select(id_pupil = from, schoolnr) %>%
  bind_rows(edge_list %>%
              select(id_pupil = to, schoolnr)) %>%
  unique() %>%
  rename(schoolnr_from_edges = schoolnr)


nodelist <- df %>% 
  select(id_pupil,age, schoolnr) %>% 
  merge(all_vertices, by = "id_pupil", all.y = T, all.x = T)

nodelist <- nodelist %>% 
  mutate(schoolnr = coalesce(schoolnr,schoolnr_from_edges)) %>% 
  select(-schoolnr_from_edges)


g2 <- graph_from_data_frame(edge_list, vertices = nodelist) 




plot(g2,
     layout = layout_nicely(g2),
     vertex.label = NA,
     edge.arrow.size = .1,
     vertex.size = 5,
     margin = 0,
)
