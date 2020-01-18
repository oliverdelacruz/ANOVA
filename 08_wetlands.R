## Load data from website ####
wetland <- read.table("http://users.stat.umn.edu/~gary/book/fcdae.data/exmpl16.7", skip = 6,
                      header = TRUE, colClasses = c(rep("factor", 6), "numeric"))

str(wetland)

plot.design(wetland)
interaction.plot(wetland$nitrogen, wetland$weed, wetland$pct.nonweed.biomass)
## etc.

## Analyze data with proper mixed effects model ####
library(lmerTest)

fit <- lmer(pct.nonweed.biomass ~ table + nitrogen + (1 | tray) +
              weed * nitrogen + (1 | wetland) + 
              weed * nitrogen * clipping, data = wetland)
anova(fit)

## interaction plot to get an idea about the interaction
interaction.plot(wetland$nitrogen, wetland$weed, wetland$pct.nonweed.biomass)

## Residual analysis ####
plot(fit)
par(mfrow = c(1, 3))
qqnorm(ranef(fit)$tray[,1])
qqnorm(ranef(fit)$wetland[,1])
qqnorm(resid(fit))
par(mfrow = c(1, 1))