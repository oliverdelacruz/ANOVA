## Meat Storage Data ####

meat <- data.frame(steak.id = c(1, 6, 7, 12, 5, 3, 10, 9, 2, 8, 4, 11),
                   treatment = rep(c("Commercial", "Vacuum", "Mixed", "CO2"), each = 3),
                   y = c(7.66, 6.98, 7.8, 5.26, 5.44, 5.8, 7.41, 7.33, 7.04, 3.51, 2.91, 3.66))

levels(meat$treatment) ## check order of levels (important!)

###############
## Contrasts ##
###############

## Define individual contrast ####
contrasts(meat$treatment) <- c(1, -1, 1, -1) ## new vs. old

fit <- aov(y ~ treatment, data = meat)

## Use built-in R functionality. Can be dangerous if
## non-orthogonal contrasts are involved.
## Multiple contrasts can be combined with *columns*.
summary(fit, split = list(treatment = list(new.vs.old = 1)))

## Use R-package multcomp ####
library(multcomp)

## Test many contrasts at the same time.
## Each contrast defines a *row* of the matrix M
M <- rbind(c(1, -1, 1, -1), ## new vs. old
           c(1, 0, -1, 0))  ## CO2 vs. mixed
fit.mc <- glht(fit, linfct = mcp(treatment = M))
summary(fit.mc, test = adjusted('none')) ## individual tests

## Construct confidence intervals
confint(fit.mc, calpha = univariate_calpha()) ## *individual* CIs
## check CI with own (manual) calculations!

##########################
## Pairwise comparisons ##
##########################

## All pairwise t-tests using pooled sd ####
## *No* control of FWER
with(meat, pairwise.t.test(y, treatment, p.adjust.method = "none"))

## TukeyHSD ####
TukeyHSD(fit) ## R function uses aov object
plot(TukeyHSD(fit))

## Using the package multcomp
confint(glht(fit, linfct = mcp(treatment = "Tukey")))

###############################
## Comparison with a Control ##
###############################

## Dunnett ####
fit.dunnett <- glht(fit, linfct = mcp(treatment = "Dunnett"))
summary(fit.dunnett) ## CO2 is reference level

