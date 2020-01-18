## Load data ####
dish <- read.table("http://users.stat.umn.edu/~gary/book/fcdae.data/exmpl14.2", skip = 14,
                    header = TRUE, colClasses = c(rep("factor", 2), "numeric"))

str(dish)
levels(dish$session)

dish$session <- factor(dish$session, levels = 1:12)
str(dish)
levels(dish$session)

## Get overview ####
plot.design(dish)
stripchart(dishes ~ session, vertical = TRUE, pch = 1, data = dish)

## Fit ANOVA model ####
fit <- aov(dishes ~ session + detergent, data = dish)
summary(fit)
drop1(fit, test = "F")

summary.lm(fit)

## For illustrational reasons only, have a look at all pairwise comparisons ####
## (without controling for multiple testing)
library(multcomp)

C <- contrMat(n = table(dish$detergent), type = 'Tukey') 
C
contr <- glht(fit, linfct = mcp(detergent = C))
summary(contr, test = adjusted('none')) 

## now test a 'fancier' contrast (the same as in the book)
## base I vs base II
contr <- glht(fit, linfct = mcp(detergent = rbind(c(rep(1/4, 4), rep(-1/4, 4), 0))))
summary(contr) ## compare with book on p.361.

## Residual analysis ####
par(mfrow = c(1, 2))
plot(fit, which = 1:2)

## If we had to find the BIBD ourselves ####
library(crossdes)
m <- find.BIB(9, 12, 3)
isGYD(m) ## need to do this check
