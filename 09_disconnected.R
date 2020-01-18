dat <- data.frame(block = factor(rep(1:6, each = 2)),
                  treat = factor(c(1, 2,
                                   1, 3,
                                   2, 3,
                                   4, 5,
                                   4, 6,
                                   5, 6)),
                  y     = rnorm(12)) ## no signal

str(dat)
dat

## get overview of used combinations
table(dat$block)
table(dat$treat)

xtabs(~ treat + block, data = dat)

fit <- aov(y ~ block + treat, data = dat)
drop1(fit, test = "F")

dummy.coef(fit)
summary.lm(fit)

library(lmerTest)
fit.lme <- lmer(y ~ treat + (1 | block), data = dat)
anova(fit.lme)
summary(fit.lme)

## what happens if we want to test a specific contrast?

library(multcomp)
summary(glht(fit, linfct = mcp(treat = rbind(c(1, 0, 0, 0, 0, -1)))))
summary(glht(fit.lme, linfct = mcp(treat = rbind(c(1, 0, 0, 0, 0, -1)))))
