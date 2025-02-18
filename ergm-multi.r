library(ergm.multi)


data(samplk)


monks <- Networks(list(samplk1, samplk2, samplk3))
fit <- ergm(monks ~ N(~edges + mutual))
summary(fit)


# incorrect method for GoF:

goff1 <-gof(fit)

plot(goff1)
summary(goff1)


# correct method for GoF:
goff2 <-gofN(fit)

plot(goff2)

summary(goff2)
 