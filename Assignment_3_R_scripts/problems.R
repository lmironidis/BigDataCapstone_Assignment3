#Number 3  Sulfate Concentration
# Sulfate Minimum Mean: 0.4321995 File: 226.csv 
# Sulfate Maximum Mean: 7.362803 File: 233.csv 
# Sulfate Average Mean: 3.138878 File: 100.csv 
> par(mfrow = c(1,3))
> boxplot(mydata$sulfate, outline = TRUE, notch = TRUE, col = "green", 
          +         xlab = "Level of Sulfate PM in the air (micrograms per cubic meter)", 
          +         main = "Simple Statistics for Minimum Mean Sulfate: File - 226.csv")
> abline(h = min(mydata$sulfate, na.rm = TRUE), col = "Blue")
> abline(h = max(mydata$sulfate, na.rm = TRUE), col = "Yellow")
> abline(h = median(mydata$sulfate, na.rm = TRUE), col = "Green")
> abline(h = quantile(mydata$sulfate, na.rm = TRUE, c(0.25, 0.75)), col = "Red")
> 
  > boxplot(mydata1$sulfate, outline = TRUE, notch = TRUE, col = "green", 
            +         xlab = "Level of Sulfate PM in the air (micrograms per cubic meter)", 
            +         main = "Simple Statistics for Maximum Mean Sulfate: File - 233.csv")
> abline(h = min(mydata1$sulfate, na.rm = TRUE), col = "Blue")
> abline(h = max(mydata1$sulfate, na.rm = TRUE), col = "Yellow")
> abline(h = median(mydata1$sulfate, na.rm = TRUE), col = "Green")
> abline(h = quantile(mydata1$sulfate, na.rm = TRUE, c(0.25, 0.75)), col = "Red")
> 
  > boxplot(mydata2$sulfate, outline = TRUE, notch = TRUE, col = "green", 
            +         xlab = "Level of Sulfate PM in the air (micrograms per cubic meter)", 
            +         main = "Simple Statistics for Average Mean Sulfate: File - 100.csv")
> abline(h = min(mydata2$sulfate, na.rm = TRUE), col = "Blue")
> abline(h = max(mydata2$sulfate, na.rm = TRUE), col = "Yellow")
> abline(h = median(mydata2$sulfate, na.rm = TRUE), col = "Green")
> abline(h = quantile(mydata2$sulfate, na.rm = TRUE, c(0.25, 0.75)), col = "Red")


#Number 4   Nitrate Concentration
# Nitrate Minimum Mean: 0.1819444 File: 332.csv 
# Nitrate Maximum Mean: 8.919331 File: 029.csv 
# Nitrate Average Mean: 1.584008 File: 323.csv 
par(mfrow = c(1,3))
> boxplot(mydata$nitrate, outline = TRUE, notch = TRUE, col = "blue",
          +         xlab = "Level of Nitrate PM in the air (micrograms per cubic meter)",
          +         main = "Simple Statistics for Minimum Mean Nitrate: File - 332.csv")
> abline(h = min(mydata$nitrate, na.rm = TRUE), col = "Blue")
> abline(h = max(mydata$nitrate, na.rm = TRUE), col = "Yellow")
> abline(h = median(mydata$nitrate, na.rm = TRUE), col = "Green")
> abline(h = quantile(mydata$nitrate, na.rm = TRUE, c(0.25, 0.75)), col = "Red")
>
  > boxplot(mydata1$nitrate, outline = TRUE, notch = TRUE, col = "blue",
            +         xlab = "Level of Nitrate PM in the air (micrograms per cubic meter)",
            +         main = "Simple Statistics for Maximum Mean Nitrate: File - 029.csv")
> abline(h = min(mydata1$nitrate, na.rm = TRUE), col = "Blue")
> abline(h = max(mydata1$nitrate, na.rm = TRUE), col = "Yellow")
> abline(h = median(mydata1$nitrate, na.rm = TRUE), col = "Green")
> abline(h = quantile(mydata1$nitrate, na.rm = TRUE, c(0.25, 0.75)), col = "Red")
>
  > boxplot(mydata2$nitrate, outline = TRUE, notch = TRUE, col = "blue",
            +         xlab = "Level of Nitrate PM in the air (micrograms per cubic meter)",
            +         main = "Simple Statistics for Average Mean Nitrate: File - 323.csv")
> abline(h = min(mydata2$nitrate, na.rm = TRUE), col = "Blue")
> abline(h = max(mydata2$nitrate, na.rm = TRUE), col = "Yellow")
> abline(h = median(mydata2$nitrate, na.rm = TRUE), col = "Green")
> abline(h = quantile(mydata2$nitrate, na.rm = TRUE, c(0.25, 0.75)), col = "Red")