## Construct artificial data-set ####

A <- factor(rep(c("low", "med", "high"), each = 2))
B <- factor(rep(c("yes", "no"), 3))

set.seed(12)

art <- data.frame(A = A,
                  B = B, 
                  y = 10 + ifelse(B == "yes", 6, 0) + rnorm(6)) ## only effect of B

art

table(art$A, art$B) ## overview of number of observations

fit <- aov(y ~ A + B, data = art)
summary(fit)

## what happens if we try to fit the *full* model?
fit.full <- aov(y ~ A * B, data = art)
summary(fit.full) ## no error term left, hence *no* tests reported
