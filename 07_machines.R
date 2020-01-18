library(lmerTest)

## Get data + overview #####
data("Machines", package = "nlme")

str(Machines)
## technical detail:
Machines$Worker <- factor(Machines$Worker, ordered = FALSE)

head(Machines)
xtabs(~ Worker + Machine, data = Machines)

## Visualize data set ####
interaction.plot(x.factor = Machines$Machine,
                 trace.factor = Machines$Worker,
                 response = Machines$score)

## a fancier plot
library(ggplot2)
ggplot(Machines, aes(x = Machine, y = score, group = Worker, col = Worker)) + geom_point() +
  stat_summary(fun.y = mean, geom = "line")

## Fit mixed effects model ####
fit <- lmer(score ~ Machine + (1 | Worker) + (1 | Worker:Machine), 
            data = Machines)
summary(fit)

anova(fit)
rand(fit)

confint(fit, oldNames = FALSE)

## Residual analysis ####
plot(fit)
par(mfrow = c(2, 2))
qqnorm(ranef(fit)$Worker[,1])
qqnorm(ranef(fit)$'Worker:Machine'[,1])
qqnorm(resid(fit))

## Compare with fitting a classical fixed effects model
fit2 <- aov(score ~ Machine * Worker, data = Machines)
summary(fit2)

coef(fit2) 
## fixed effects coefficients look the same!
## but the fixed effects model is much more significant

## old approach
fit3 <- aov(score ~ Machine + Error(Worker + Machine:Worker), data = Machines)
summary(fit3)
coef(fit3)

## equivalent formulation
fit4 <- aov(score ~ Machine + Error(Worker/Machine), data = Machines)
summary(fit4)
## now aov get's the sampe p-value for the main-effect of machine



