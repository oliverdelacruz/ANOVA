library(lmerTest)

data("Pastes", package = "lme4")
?Pastes ## info about data set
str(Pastes)
head(Pastes, 10)

## Visualize data ####
library(ggplot2)
ggplot(Pastes, aes(y = cask, x = strength)) + geom_point() + facet_grid(batch ~.)

## Get overview of number of observations ####
table(Pastes$batch, Pastes$cask)

## lmer approaches (all equivalent) ####
fm1 <- lmer(strength ~ (1 | batch/cask), data = Pastes)
summary(fm1)
confint(fm1, oldNames = FALSE)

fm2 <- lmer(strength ~ (1 | batch) + (1 | cask:batch), data = Pastes)
summary(fm2)
rand(fm2)

## sample is already encoded in "nested" form
fm3 <- lmer(strength ~ (1 | batch) + (1 | sample), Pastes)
summary(fm3)
rand(fm3)

## the following is *not* appropriate here, why?
fm4 <- lmer(strength ~ (1 | batch) + (1 | cask), Pastes)
summary(fm4)
rand(fm4)

## residual analysis ####
plot(fm1) ## Tukey-Anscombe plot
par(mfrow = c(2,2))
qqnorm(ranef(fm1)$batch[,1], main = "batch")
qqnorm(ranef(fm1)$'cask:batch'[,1], main = "cask")
qqnorm(resid(fm1), main = "residuals")

## classical appraoch with aov ####

## model everything as fixed
fit <- aov(strength ~ batch + cask %in% batch, data = Pastes)
fit <- aov(strength ~ batch/cask, data = Pastes) ## equivalent formulation
summary(fit)

pf(27.489 / 17.545, 9, 20, lower.tail = FALSE)