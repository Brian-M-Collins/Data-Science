x <- c("20-30", "30-40", "40-50", "50-60", "60-70", "70-80")
f <- c(6,18,11,11,3,1)
mid <- c(25,35,45,55,65,75)
calculation <- data.frame("x"=x, "f"=f, "Midpoint"=mid)
calculation$"f*x" <- f*mid

mean <- sum(calculation$"f*x")/sum(calculation$f)

calculation$"x-M" <- calculation$Midpoint-mean
calculation$"Abs(x-M)" <- abs(calculation$"x-M")
calculation$"f*Abs(x-M)" <- calculation$f * calculation$"Abs(x-M)"
calculation$"(x-M)^2" <- calculation$"x-M"^2
calculation$"f*(X-M)^2" <- calculation$"(x-M)^2"*calculation$f

mean_absolute_deviation <- sum(calculation$"f*Abs(x-M)")/sum(calculation$f)
variance <- sum(calculation$"f*(X-M)^2")/sum(calculation$f)
standard_deviation <- sqrt(variance)

output_table <- data.frame("Mean"=mean, "Mean Absolute Deviation"=mean_absolute_deviation, "Variance"=variance, "Standard Deviation"=standard_deviation)