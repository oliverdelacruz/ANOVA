## Create data-set ####
weight <- c(61, 100, 56, 113, 99, 103, 75, 62,
            75, 102, 95, 103, 98, 115, 98, 94,
            58, 60, 60, 57, 57, 59, 54, 100,
            57, 56, 67, 59, 58, 121, 101, 101, 
            59, 46, 120, 115, 115, 93, 105, 75)

sire <- factor(rep(1:5, each = 8))

animals <- data.frame(weight, sire)
str(animals)

stripchart(weight ~ sire, vertical = TRUE, pch = 20, cex = 2, data = animals)

## Classical approach ####
fit <- aov(weight ~ sire, data = animals)
summary(fit)

## Modern approach ####
library(lmerTest)

fit.lme <- lmer(weight ~ (1 | sire), data = animals)
summary(fit.lme)

coef(fit.lme)
ranef(fit.lme) ## "estimated" (predicted) random effects

## Confidence intervals and tests
confint(fit.lme, oldNames = FALSE)
rand(fit.lme) ## tests for random components (conservative)

## exact tests
library(RLRsim)
exactRLRT(fit.lme)

## Residual analysis
plot(fit.lme) ## Tukey-Anscombe plot

par(mfrow = c(1,2)) ## re-set plotting options
## QQ-plots of random effects
qqnorm(ranef(fit.lme)$sire[,1], main = "sire")
## QQ-plots of residuals
qqnorm(resid(fit.lme), main = "residuals")
par(mfrow = c(1, 1))