# Sometimes it's useful to have a small dataset to test your data handling procedures.
# This script produces a dataset in the format that you might get from "name generator" survey questions:
# The first column (id) contains the id of the respondent
# The column "age" is an arbitrary respondent characteristic
# The columns "friend1" and "friend2" contain the id's of other respondents that the respondent named as friends.
# This structure is similar to the "Knecht data" that we use in the course (for one class)


toy_data <- data.frame(
  id = c(1,2,3,4,5),
  age = c(20,21,25,NA,21),
  friend1 = c(2,3,1,NA,NA),
  friend2 = c(4,NA,NA,NA,NA))
