library(lmerTest)
library(multcomp)

## Get data & overview #####
data("ergoStool", package = "nlme")

str(ergoStool)
## technical detail:
ergoStool$Subject <- factor(ergoStool$Subject, ordered = FALSE)

head(ergoStool)
xtabs(~ Subject + Type, data = ergoStool) ## could also use table

interaction.plot(x.factor = ergoStool$Type,
                 trace.factor = ergoStool$Subject,
                 response = ergoStool$effort)

## Fit mixed effects model ####
fit <- lmer(effort ~ Type + (1 | Subject), data = ergoStool)
summary(fit) ## no dummy.coef function available here

anova(fit)
rand(fit)
confint(fit, oldNames = FALSE)

## Residual analysis ####
plot(fit)
qqnorm(ranef(fit)$Subject[,1])
qqnorm(resid(fit))

## side remark: can also use multcomp package with lmer-objects
mult <- glht(fit, linfct = mcp(Type = "Tukey"))
confint(mult)

## Fit classical fixed effects model with aov
fit2 <- aov(effort ~ Type + Subject, data = ergoStool)
summary(fit2)
coef(fit2)

## the following will be useful later on
fit3 <- aov(effort ~ Type + Error(Subject), data = ergoStool)
summary(fit3)