## Plot 4 normal densities with 
## - individual means
## - common variance (1)
## See ?curve for help

curve(dnorm(x, mean = 3), xlim = c(-10, 10), ylab = "Densities", lwd = 3)
abline(v = 3)

curve(dnorm(x, mean = 0), xlim = c(-10, 10), add = TRUE, col = "red", lwd = 3)
abline(v = 0, col = "red")

curve(dnorm(x, mean = -7), xlim = c(-10, 10), add = TRUE, col = "blue", lwd = 3)
abline(v = -7, col = "blue")

curve(dnorm(x, mean = 6), xlim = c(-10, 10), add = TRUE, col = "orange", lwd = 3)
abline(v = 6, col = "orange")
