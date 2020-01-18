## Simulate data ####

## 2 backgrounds: rural, urban
## 10 raters per background
## 4 cheese types
## 2 replicates per cheese type and rater combination

background <- factor(c("rural", "urban"))
rater <- factor(1:10)
cheese  <- factor(c("A", "B", "C", "D"))
set.seed(123)

## main effect of background
background.eff <- c(-5, 5); names(background.eff) <- levels(background)

## main effect of cheese
cheese.eff <- c(5, -15, 8, 2); names(cheese.eff) <- levels(cheese)

## no interaction background:cheese

## random effects of rater
rater.eff <- rnorm(20, sd = 5)

## random interaction
cheese.rater <- rnorm(20 * 4, sd = 4) ## adjust sd.?

m <- expand.grid(cheese = cheese, rater = rater, background = background)

y <- 70 + rep(background.eff, each = 40) + 
          rep(rater.eff, each = 4) + 
          rep(cheese.eff, times = 20) + 
          cheese.rater
m2 <- cbind(m, y)

cheese.data   <- m2[rep(1:nrow(m), each = 2),] 
cheese.data$y <- round(cheese.data$y + rnorm(nrow(cheese.data), sd = 3), 0)

str(cheese.data)
head(cheese.data)

## Visualize data ####
plot.design(cheese.data)
interaction.plot(x.factor = cheese.data$cheese,
                 trace.factor = interaction(cheese.data$background, cheese.data$rater),
                 response = cheese.data$y, col = rep(c("blue", "red"), times = 10))

## Analyze data with lmer ####
library(lmerTest)

fit <- lmer(y ~ background * cheese + (1 | rater:background) + 
              (1 | rater:background:cheese), data = cheese.data)
summary(fit)
anova(fit) ## also compare with interaction plot!
    

confint(fit, oldNames = FALSE)

## Residual Analysis
plot(fit)
qqnorm(ranef(fit)$"rater:background"[,1])
qqnorm(ranef(fit)$"rater:background:cheese"[,1])
qqnorm(resid(fit))