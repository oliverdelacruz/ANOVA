## Power analysis with simulation

## Settings ####
## Sample size per group
n <- 100 ## try out 4, 5, 6, 7

## Assumptions about the model under the alternative
means <- c(7, 13, 10, 10, 10)
sigma <- sqrt(7.5)

g <- length(means) ## number of groups

## Create data-frame
group <- factor(rep(LETTERS[1:g], each = n))
group

## Test run: *single* data-set ####
y <- rnorm(n * g, mean = rep(means, each = n), sd = sigma)
       
data <- data.frame(y = y, group = group)
head(data)

## Visualization
stripchart(y ~ group, data = data, vertical = TRUE, pch = 1)

## Fit one-way ANOVA model
fit  <- aov(y ~ group, data = data)
summary(fit)
summary(fit)[[1]][1, "Pr(>F)"] ## extract p-value of global F-test

## Now do *many* simulations ####

## This was *one* data set. 
## Now have a look at *many* of such data sets.
## We simulate 1000 data sets.
## For every data set we check whether the global null hypothesis
## is being rejected or not.
## We store the test decision in a vector of length 1000.

results <- numeric(1000)

for(i in 1:1000){
  ## simulate new response
  y <- rnorm(n * g, mean = rep(means, each = n), sd = sigma)
  
  data <- data.frame(y = y, group = group)
  fit  <- aov(y ~ group, data = data)
  results[i] <- summary(fit)[[1]][1, "Pr(>F)"]
}

mean(results)
## = proportion of cases where we actually reject H_0

## Results:
# n = 4: 0.54
# n = 5: 0.69
# n = 6: 0.80
# n = 7: 0.87