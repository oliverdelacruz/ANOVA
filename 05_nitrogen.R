## Example 8.1 from Kuehl

## Define data ####
y <- c(34.98, 41.22, 36.94, 39.97,
       40.89, 46.69, 46.65, 41.90,
       42.07, 49.42, 52.68, 42.91,
       37.18, 45.85, 40.23, 39.20,
       37.99, 41.99, 37.61, 40.45, 
       34.89, 50.15, 44.57, 43.29)

block     <- factor(rep(1:4, times = 6))
treatment <- factor(rep(1:6, each = 4))

nitro <- data.frame(y, block, treatment)

## Analyze data ####
fit <- aov(y ~ block + treatment, data = nitro)
summary(fit)
coef(fit)
dummy.coef(fit)

## Perform residual analysis ####
par(mfrow = c(1, 2))
plot(fit, which = 1:2)

## Simulate a few 'nice' Tukey-Anscombe plots to check
## whether our TA-plot is still ok
set.seed(12)
par(mfrow = c(4, 5))
for(i in 1:20){
  plot(fitted(fit), rnorm(nrow(nitro)), main = i)
  abline(h = 0)
}

## Relative efficiency ####
## See Oehlert, p. 323
crd <- (3 * 65.67 + (5 + 15) * 7.2) / (3 + 5 + 15)
crd
crd / 7.2
