## Load data ####
lith <- read.table("http://users.stat.umn.edu/~gary/book/fcdae.data/exmpl14.5", skip = 13,
                   header = TRUE, colClasses = c(rep("factor", 3), rep("numeric", 2)))
str(lith)

## Have a look at data ####
plot.design(lith[,-4])
interaction.plot(lith$period, lith$treatment, lith$hour.12)

## Fit ANOVA model ####
options(contrasts = c("contr.sum", "contr.sum"))
fit <- aov(hour.12 ~ subject + period + treatment, data = lith)
drop1(fit, test = "F")

## also check interaction period * treatment
fit2 <- aov(hour.12 ~ subject + period * treatment, data = lith)
drop1(fit2, test = "F")

## Residual analysis ####
par(mfrow = c(1, 2))
plot(fit, which = 1:2)

## try to analyze design
library(crossdes)

period  <- as.numeric(as.character(lith$period))
subject <- as.numeric(as.character(lith$subject))
## compare with as.numeric(lith$subject)
treat   <- as.numeric(as.character(lith$treatment))


m <- matrix(NA, nrow = 2, ncol = 12)
m[cbind(period, subject)] <- treat
##m[period, subject] <- treat ## will not work

m
isGYD(m)
