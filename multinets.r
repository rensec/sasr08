
# attempt to process multiple networks at once. Does not work yet!

PupilsWaveV <- PupilsWaveV %>% 
  mutate(id_pupil = paste(schoolnr,as.character(namenr),sep = "")) %>%
  mutate(id_pupil = gsub(" ","",id_pupil)) %>% 
  relocate(id_pupil, .after = namenr)



lelist <- PupilsWaveV %>%
  filter(schoolnr == "12b") %>% 
  select(id_pupil, friend1:friend12, schoolnr) %>%    # select only the " best friends"  network
  mutate(across(starts_with("friend"), ~ paste0(schoolnr, as.character(.)))) 
%>% # not sure this is correct
  pivot_longer(c(friend1:friend12)) %>% 
  filter(!is.na(value)) %>% # drop the missings
  rename(from ="id_pupil", to = "value", sourcevar= "name") %>% #just nice for interpretation
  relocate(to, .after=from) #move around the columns

lnodelist <- PupilsWaveV %>% 
  filter(schoolnr == "12b") %>% 
  select(id_pupil,sex, age) %>% 
  rename(name = "id_pupil")

allclassnet <- graph_from_data_frame(lelist, vertices = lnodelist) # how to handle missing nominates
