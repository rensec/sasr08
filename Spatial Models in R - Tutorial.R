###SPATIAL MODELS IN R###

##CONSTRUCTING A SPATIAL WEIGHT MATRIX##

#We need the spdep package for the spatial weight matrix and for the Moran's I test
#install.packages("spdep")
#We need the spatialreg package for the spatial lag and spatial error models
#install.packages("spatialreg")

library(spdep)
library(spatialreg)

#We continue with the example from the lecture: 
#We are interested in whether household income has an effect on the crime rate using neighborhood data.
#We control for house prices as house prices are correlated with both household income and crime rates.

#We use Anselin’s Columbus data for crime rates of neighborhoods in Columbus, Ohio, USA in 1980.
#The codebook can be found here: https://geodacenter.github.io/data-and-lab/columbus/

#The data are already part of the spdep package and we obtain the data as follows:
data(columbus)

#We will call these data 'mydata'
mydata <- columbus
#Instead of using the tedious dollar sign every time we type a variable, 
#we can attach the data and just type the variable
attach(mydata)
#There are 49 neighborhoods in the data
#Our dependent variable is the crime rate, measured as residential burglaries and vehicle thefts per 1000 households: CRIME
#Our independent variable is the household income in 1,000 USD: INC
#Our control variable is the housing value in 1,000 USD: HOVAL

#Col.gal.nb provides an adjacency list of neighbors
#We give it a better name:
neighbors <- col.gal.nb
#We check the class of the adjacency list
class(neighbors)
#Note that it is an 'nb' or 'neighborhood' object
#In spatial regression analysis, you need a so-called 'weighted list - listw' object
#To arrive to a listw object, often it is easiest to first convert a dataframe to 'nb'
#You can do this simply with the command class(dataframe) <- "nb" and then use nb2listw(nblist)
#Another approach (which is more suitable when having isolates) is to first make an adjacency matrix
#And then you can use mat2listw(adjacencymatrix)

#In this case, however, we already have an nb object
#So the only thing we need to do is to go to a listw object
#We specify style 'W' to row-standardize
listw <- nb2listw(neighbors, style = "W")
summary(listw)
#We see that an average region has about 5 neighbors

##LINEAR REGRESSION ANALYSIS##

#We now turn to a linear regression model using OLS
#ALWAYS START WITH THE EASIEST MODEL! 
#The easiest model is more often than not sufficient for the main analyses
#Before we estimate the model, let's first summarize the variables:
summary(CRIME)
summary(INC)
summary(HOVAL)
summary(neighbors)
#Nothing seems out of the ordinary
#We see that an average region has about 5 neighbors

#Now we would like to plot the neighborhoods to visualize the network
#Coordinates of each neighborhood are provided in 'coords'
summary(neighbors)
plot(neighbors, coords)
#We observe that some neighborhoods serve as bridges between neighborhoods that are distant from each other

#Let's now run a linear regression model:
olsreg <- lm(CRIME ~ INC + HOVAL)
summary(olsreg)
#Higher household income and higher housing values lead to a significantly lower crime rate
#However, we did not take the spatial dependence into account and our hypothesis tests may lead to erroneous conclusions

##SPATIAL ANALYSIS##

#Moran's I test
moran.test(CRIME, listw)
#p-value is very small so we reject the null hypothesis of no spatial dependence
#We should not use a linear regression

#Lagrange multiplier tests for spatial lag and spatial error dependencies
lm.LMtests(olsreg, listw, test=c("LMlag", "LMerr"))
#We reject both hypotheses
#It appears that we should use both a spatial lag and a spatial error model

#Spatial lag model
slm <- lagsarlm(CRIME ~ INC + HOVAL, data = mydata, listw, Durbin = FALSE)
summary(slm)
#Again a significantly negative effect of household income and housing values on the crime rate
#Coefficients are very similar to the OLS coefficients
#This is often the case given that OLS coefficients are most often still unbiased
#The spatial parameter rho is positive and significant showing dependence

#We now turn to the direct and indirect effects
#For this purpose, we follow the approximation method by Lesage and Pase (2009) 
#We specify 'zstats = TRUE' to obtain p-values
#We use a simulation with 200 iterations to obtain p-values
im<-impacts(slm, listw = listw, zstats=TRUE, R=200)
sums<-summary(im,  zstats=T)
data.frame(sums$res)
data.frame(sums$pzmat)

#Although both total effects are significant, they are mainly driven by direct effects

#Spatial error model
sem <- errorsarlm(CRIME ~ INC + HOVAL, data = mydata, listw, Durbin = FALSE)
summary(sem)
#Again a significantly negative effect of household income and housing values on the crime rate
#The spatial parameter lambda is positive and significant showing dependence

#Spatial autoregressive combined model
sac <- sacsarlm(CRIME ~ INC + HOVAL, data = mydata, listw, Durbin = FALSE)
summary(sac)

#Similar results, but rho is now not significant

#Spatial Durbin model
sdm <- lagsarlm(CRIME ~ INC + HOVAL, data = mydata, listw, Durbin = TRUE)
summary(sdm)

#We see that the results are similar and the lags are not significant

#Spatial Durbin error model
sdem <- errorsarlm(CRIME ~ INC + HOVAL, data = mydata, listw, Durbin = TRUE)
summary(sdem)

#Results are again similar, but now the lag of household income is significant

#General nested model
gnm <- sacsarlm(CRIME ~ INC + HOVAL, data = mydata, listw, Durbin = TRUE)
summary(gnm)

#Similar results again
