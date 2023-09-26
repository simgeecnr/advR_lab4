standardized_residuals <- res / sd(res)
y <- sqrt(abs(standardized_residuals))

data3 <- data.frame(Fitted = fitted_val, StdRes = y)
colnames(data3) <- c("Fitted", "StdRes")


# Create a residuals vs. fitted values plot using ggplot2
ggplot(data3, aes(x = fitted_val, y = y)) +
  geom_point() +
  stat_summary(aes(y = y, group = 1), fun=median, color ="red", geom="line", group=1) +
  labs(x = "Fitted Values", y = "Standardized Residuals", title = "Scale-Location")