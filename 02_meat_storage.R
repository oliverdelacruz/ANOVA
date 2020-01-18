## Import data, here manually ####
meat <- data.frame(steak.id = c(1, 6, 7, 12, 5, 3, 10, 9, 2, 8, 4, 11),
                   treatment = rep(c("Commercial", "Vacuum", "Mixed", "CO2"), each = 3),
                   y = c(7.66, 6.98, 7.80, 5.26, 5.44, 5.80, 
                         7.41, 7.33, 7.04, 3.51, 2.91, 3.66))

meat
str(meat)
levels(meat$treatment)

## if we want to change the reference level, we can do this by using the 
## function 'relevel' (uncomment the following lines)
## meat$treatment <- relevel(meat$treatment, ref = "Commercial")
## str(meat)
## levels(meat$treatment)

## Visualize data ####
stripchart(y ~ treatment, data = meat, pch = 1, vertical = TRUE)

## conclusion?

## Boxplot, if more data is available per group [*other* dataset]
boxplot(count ~ spray, data = InsectSprays) ## from ?boxplot, ?InsectSprays

## Side remark: if we had to assign the treatments randomly ourselves
treat <- rep(c("Commercial", "Vacuum", "Mixed", "CO2"), each = 3)
treat
sample(treat, replace = FALSE) ## random allocation

## Fit one-way ANOVA model ####
#options(contrasts = c("contr.sum", "contr.poly"))
options(contrasts = c("contr.treatment", "contr.poly"))

fit <- aov(y ~ treatment, data = meat)
summary(fit) ## ANOVA table including F-test

## Extract coefficients
coef(fit)       ## be careful with interpretation
dummy.coef(fit) ## easier to interpret

## Parameters including standard errors
summary.lm(fit) ## be careful with interpretation
confint(fit)

## Confidence intervals for group means \mu_i
predict(fit, newdata = data.frame(treatment = c("Commercial", "Vacuum", "Mixed", "CO2")), 
        interval = "confidence")

## Residual analysis ####
plot(fit, which = 2)
plot(fit, which = 1)
