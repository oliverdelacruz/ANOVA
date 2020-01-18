## Define data-set ####
cement <- data.frame(time  = factor(rep(c(-1, 1), 4)),
                     temp  = factor(rep(c(-1, 1), each = 2)), ## uses recycling!
                     press = factor(rep(c(-1, 1), each = 4)),
                     y = c(297, 300, 106, 131, 177, 178, 76, 109))
cement
str(cement)

## Ensure that factors are encoded using sum-to-zero constraint
options(contrasts = c("contr.sum", "contr.sum"))

## Fit standard ANOVA model with *all* interactions (= full model)
fit <- aov(y ~ time * temp * press, data = cement)

## Have a look at coefficients, compare with "manual" estimates
dummy.coef(fit)
coef(fit)

coef(fit)[1] ## intercept (global mean)
abs.est <- abs(2 * coef(fit)[-1]) ## all other coefficients

## Pareto chart
est.sort <- sort(abs.est)
## now get rid of 1's in name (cosmetics)
names(est.sort)
names(est.sort) <- gsub("1", "", names(est.sort)) 
names(est.sort)

opar <- par(mar = c(5, 7, 3, 1)) ## increase margin on the left-hand side
barplot(est.sort, horiz = TRUE, main = "Pareto Chart", las = 1, cex.names = 0.9,
        xlab = "effect size")
par(opar) ## set margins to old values (see ?par)

## Half-normal plot
p     <- 0.5 + 0.5 * ppoints(length(est.sort))
quant <- qnorm(p)

plot(quant, est.sort, xlim = c(0, 2.5), ylim = c(0, 150),
     xlab = "Theoretical Quantiles",
     ylab = "Absolute Effect", 
     main = "Half-normal Plot")
text(quant, est.sort, labels = names(est.sort), pos = 4, cex = 0.8) ## see ?text
abline(a = 0, b = median(est.sort) / median(quant))

## If we drop the 3-way interaction we have 1 (!) df left for the error term
fit2 <- aov(y ~ (time + temp + press)^2, data = cement)
summary(fit2)

## What are the model prediction for the 4 possible temp x pressure settings
## if we only consider temp and pressure (and the interaction) in our model
fit.tmp <- aov(y ~ temp * press, data = cement)
summary(fit.tmp)
cbind(cement, pred = predict(fit.tmp))
