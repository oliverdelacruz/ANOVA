## Visualize F-density ####

## Play around with parameters
curve(df(x, df1 = 3, df2 = 8), from = 0, to = 5)

## Calculate 95% quantiles ####
den.df <- 2:20
num.df <- 2:20

mq <- matrix(0, nrow = length(den.df), ncol = length(num.df))

for(i in seq_along(den.df)){
  mq[i,] <- qf(0.95, num.df, den.df[i])
}

matplot(den.df, mq, type = "l", xlab = "den.df", ylab = "95%-quantile")
legend("topright", legend = 2:20, lty = 1:19, col = 1:19, cex = 0.75,
       title = "num.df")
