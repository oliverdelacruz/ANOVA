## Generate data ####
options(contrasts = c("contr.treatment", "contr.treatment"))

set.seed(12)

y <- round(c(rnorm(10, mean = 45, sd = 3),
             rnorm(10, mean = 52, sd = 3),
             rnorm(10, mean = 55, sd = 3),
             rnorm(40, mean = 60, sd = 3)), 1)

y[1:10]
y[11:20]
y[21:30]
y[31:70]

matrix(y[31:70], ncol = 4)

gender <- factor(c(rep("male", 20), rep("female", 50)), levels = c("male", "female"))
drink  <- factor(rep(c("A", "B", "A", "B"), times = c(10, 10, 10, 40)))

table(gender, drink)

## Visualize data ####
interaction.plot(drink, gender, y)

m <- aggregate(y, by = list(gender, drink), function(x) return(c(length(x), mean(x))))

symbols(as.numeric(m[,2]), m$x[,2], circles = sqrt(m$x[,1]), xlab = "", ylab = "", inch = 0.5,
        xaxt = "n", ylim = c(40, 65), bg = c("blue", "red", "blue", "red"), fg = "white")
axis(1, at = c(1, 2), labels = c("A", "B"))

## Analyze data ####

## Type I sum of squares
fit <- aov(y ~ gender * drink)
summary(fit)

fit2 <- aov(y ~ drink * gender)
summary(fit2) ## sum of squares change!

coef(fit)
coef(fit2) ## the coefficients are of course the same

## Type II sum of squares
library(car)
Anova(fit, type = "II")

## Type III sum of squares
drop1(fit, scope = ~., test = "F")
## main effects tests typically not very meaningful
## if we have a significant interaction in the model

## interaction effect can also be tested as follows
## (using the model comparison approach)
fit.A <- aov(y ~ gender + drink)
fit.B <- aov(y ~ gender * drink)
anova(fit.A, fit.B)

