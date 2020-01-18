## Create data ####
season  <- factor(rep(c("Spring", "Summer"), each = 6))
density <- factor(rep(c(6, 12, 24), each = 3))
y <- c(1.17, 0.50, 1.67, 1.50, 0.83, 1.00, 0.67, 0.67, 0.75,
       4.00, 3.83, 3.83, 3.33, 2.58, 2.75, 2.54, 1.83, 1.63)

design <- expand.grid(density = factor(c(6, 12, 24)), season = c("Spring", "Summer"))
snails <- data.frame(design[rep(1:nrow(design), each = 3),],
                     y = y)

## Have a look at interaction plot ####
interaction.plot(snails$density, snails$season, snails$y)

## Fit two-way ANOVA model with interaction ####
options(contrasts = c("contr.treatment", "contr.treatment"))
fit <- aov(y ~ season * density, data = snails)
summary(fit)

## Fit a model per season ####
fit.spring <- aov(y ~ density, data = subset(snails, season == "Spring"))
fit.summer <- aov(y ~ density, data = subset(snails, season == "Summer"))

summary(fit.spring)
summary(fit.summer)

0.1722 / 0.144

pf(1.2, 2, 12, lower.tail = FALSE)

coef(fit)
coef(fit.spring)
coef(fit.summer)
