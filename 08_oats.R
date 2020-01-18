## Get idea of data-set ####

data(oats, package = "MASS")

str(oats)
oats
xtabs(~ N + V + B, data = oats)

plot.design(oats)
interaction.plot(oats$N, oats$V, oats$Y) ## averaging over blocks

## Analyze using split-plot model ####

library(lmerTest)

## fit proper mixed effects model
fit.lme <- lmer(Y ~ B + V * N + (1 | V:B), data = oats)
#anova(fit.lme)
summary(fit.lme)

## a wrong model would be
fit <- aov(Y ~ B + V * N, data = oats)
summary(fit)
## here, everything is tested against MS_E which is wrong,
## as we don't have independent observations!

## Residual analysis ####
plot(fit.lme)
par(mfrow = c(1, 2))
qqnorm(ranef(fit.lme)$'B:V'[,1])
qqnorm(resid(fit.lme))
par(mfrow = c(1, 1))