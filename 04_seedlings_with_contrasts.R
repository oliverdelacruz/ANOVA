## Define data set ####
combinations <- expand.grid(location = c("Taglieda", "Pfyn", "Rheinau"),
                            exposure = c("short", "long", "permanent"))
data <- data.frame(rbind(combinations, combinations), 
                   y = c(25, 45, 50, 42, 62, 52, 62, 80, 88, 
                         25, 42, 50, 38, 58,62, 55, 75, 95))
## Visualize data set ####

## mean value per factor level (completely ignoring the other factor)
plot.design(y ~ ., data = data)

## Plot for every combination
## interaction command combines two factors into one
## (have a look at interaction(data$exposure, data$location))
stripchart(y ~ interaction(exposure, location), data = data, pch = 1, vertical = TRUE,
           method = "jitter") ## jitter because of replicates

## better: use interaction plot
interaction.plot(x.factor = data$exposure, 
                 trace.factor = data$location, 
                 response = data$y)
## conclusions?

## Fit two-way ANOVA model ####
fit <- aov(y ~ location * exposure, data = data)
fit <- aov(y ~ location + exposure + location:exposure, data = data) ## equivalent version

summary(fit)

coef(fit)
dummy.coef(fit)

## compare with options(contrasts = c("contr.sum", "contr.sum"))

## Calculate some contrasts ####
tukey <- TukeyHSD(fit)
tukey
tukey$'location:exposure'

plot(TukeyHSD(fit, which = "location:exposure"))

## individual comparisons using function glht of package multcomp
library(multcomp)

## create hyper-factor for all combinations
data$exp.loc <- interaction(data$location, data$exposure)
levels(data$exp.loc)

fit.alt <- aov(y ~ exp.loc, data = data) ## cell means model
coef(fit.alt)

## testing a specific contrast: Pfyn.short vs. Taglieda.long
M <- matrix(c(0, 1, 0, -1, rep(0, 5)), nrow = 1)
fit.gl <- glht(fit.alt, linfct = M)
summary(fit.gl) 

## could also do Tukey with glht ...


