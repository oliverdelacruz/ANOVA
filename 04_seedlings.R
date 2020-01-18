## Define data set ####
combinations <- expand.grid(location = c("Taglieda", "Pfyn", "Rheinau"),
                            exposure = c("short", "long", "permanent"))
data <- data.frame(rbind(combinations, combinations), 
                   y = c(25, 45, 50, 42, 62, 52, 62, 80, 88, 
                         25, 42, 50, 38, 58,62, 55, 75, 95))
## Visualize data set ####

## mean value per factor level (completely ignoring the other factor)
plot.design(y ~ ., data = data)

## Boxplot for every combination
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
