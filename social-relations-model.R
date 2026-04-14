remotes::install_github("alexanderrobitzsch/SRMData")

# Paper: Back, M. D., Schmukle, S. C., & Egloff, B. (2011). A closer look at first sight: Social relations lens model analysis of personality and interpersonal attraction at zero acquaintance. European Journal of Personality, 25(3), 225–238


# Load necessary packages
library(lme4)
library(SRMData)
library(srm)

# Load the dataset

data(data.back)




# Basic multilevel SRM model
model_srm <- lmer(y ~ 1 + 
                      (1 | Actor) +       # actor effect
                      (1 | Partner) +     # partner effect
                      (1 | Dyad),   # unique dyadic effect
                      data = data.back)

# Summary of the model
summary(model_srm)

var_comps <- as.data.frame(VarCorr(model_srm))
var_comps

# Total variance (sum of all components)
total_var <- sum(var_comps$vcov)

# Proportion of variance explained by each component
variance_decomposition <- data.frame(
  Component = var_comps$grp,
  Variance = var_comps$vcov,
  Proportion = round(var_comps$vcov / total_var, 3)
)

variance_decomposition

# does not lead to the same results as the paper, dataset seems different (not the same variables)