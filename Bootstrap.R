library("ggplot2")

x <- list(x = c(160, 152, 167, 132, 167, 239, 122, 142, 130, 133))
y <- list(y = c(132, 146, 178, 158, 147, 267, 163, 135, 150, 198))
df <- data.frame(x, y)
df$differ <- df$x - df$y

bootstrap <- function(x, y, statistic, n, alpha){
  boot_length <- max(length(x), length(y))
  boot_data <- rep(NA, n)
  for (i in 1:n){
    x_sample <- sample(x, size = boot_length, replace = T)
    y_sample <- sample(y, size = boot_length, replace = T)
    boot_data[i] <- statistic(x_sample - y_sample)
  }
  quants <- quantile(boot_data, probs = c(alpha/2, 1 - alpha/2))
  p_1 <- pnorm(q = 0, mean = mean(boot_data), sd = sd(boot_data), lower.tail = F)
  p_2 <- pnorm(q = 0, mean = -mean(boot_data), sd = sd(boot_data), lower.tail = T)
  print("Confidence interval:")
  print(quants)
  print("p-value:")
  print(2 * min(c(p_1, p_2)))
  return(ggplot(data.frame(boot_data), aes(boot_data, 
           fill = (boot_data < quants[1]) | boot_data > quants[2])) + 
           geom_histogram(show.legend = F, bins = 50, col = "black"))
}

bootstrap(df$x, df$y, median, 1000, 0.05)
