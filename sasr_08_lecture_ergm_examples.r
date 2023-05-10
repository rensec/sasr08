library(ergm)

data(florentine) 

m1 <-ergm(flomarriage~edges+triangle) 

summary(m1)

m1.gof <- gof(m1)

m2 <-ergm(flomarriage~edges+nodecov('wealth'))

summary(m2)

m2.gof <-gof(m)

summary(m1.gof)
plot(m1.gof)

m2.gof <-gof
plot(m2.gof)
