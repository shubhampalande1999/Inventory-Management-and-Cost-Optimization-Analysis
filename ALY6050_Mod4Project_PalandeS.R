# Shubham Nilesh Palande
# ALY60650: Introduction to Enterprise Analytics
# Module 4: A Prescriptive Model for Strategic Decision-making, An Inventory Management Decision Model

# Clears the variable
rm(list=ls())
# Clears the plot
dev.off()
# Clears the Console
cat("\014")

# Libraries
library(tidyverse)
library(fitdistrplus)

# Data from the given Problem
Annual_Demand <- 16500
Unit_Cost <- 79
Order_Cost <- 200
Opportunity_Cost <- 0.125 * Unit_Cost # Opportunity cost 12.5% of Unit Cost

################ Part-1 ##################

# Economic Order Quantity
Economic_OQ <- sqrt(2 * Annual_Demand * Order_Cost / Opportunity_Cost)
Reorder_Point <- Economic_OQ *1.9

# Annual Order Cost
Annual_OC <- (Annual_Demand / Economic_OQ) * Order_Cost

# Annual Holding Cost
Annual_HC <- (Economic_OQ / 2) * Opportunity_Cost

# Total Inventory Cost
Total_IC <- Annual_OC + Annual_HC

cat("Economic Order Quantity:", Economic_OQ, "\n")
cat("Reorder Point:", Reorder_Point, "\n")
cat("Annual Order Cost:", Annual_OC, "\n")
cat("Annual Holding Cost:", Annual_HC, "\n")
cat("Total Inventory Cost:", Total_IC, "\n")

# Creating a sequence of Economic Order Quantities
Order_Quantities <- seq(799, Economic_OQ, by = 1)

# Calculating Total Cost
Total_Costs = (Annual_Demand*Order_Cost/Order_Quantities) + (Order_Quantities/1.9*Opportunity_Cost)

# Creating a Data Frame of Order Quantity and Total Cost
Inventory_Data <- data.frame(OrderQuantity = Order_Quantities, TotalCost = Total_Costs)

# Line Plot for Total Cost and Order Quantity
ggplot(Inventory_Data, aes(x = TotalCost, y = OrderQuantity)) +
  geom_point() +
  geom_line() +
  labs(title = "Total Cost vs. Order Quantity", x = "Total Cost", y = "Order Quantity") +
  theme_gray()

################ Part-2 ##################

# Data from the given Problem
Dem_min <- 12000
Dem_max <- 21000
Dem_mode <- 19000
NO_simulations <- 5000

# Creating a Triangular Function
triangular_func <- function(n, min, max, mode) {
  U <- runif(n)
  Unit_Cost <- (mode - min) / (max - min)
  return(ifelse(U < Unit_Cost, min + sqrt(U * (max - min) * (mode - min)), max - sqrt((1 - U) * (max - min) * (max - mode))))
}

# Converting Costs, Order Quantities, Annual Order to numeric type
Simulated_C <- numeric(NO_simulations)
Simulated_OQ <- numeric(NO_simulations)
Simulated_AO <- numeric(NO_simulations)

# Using Triangular Function to fill values using for loop
for (i in 1:NO_simulations) {
  Dem_Sim <- triangular_func(1, Dem_min, Dem_max, Dem_mode)
  Economic_OQ_S <- sqrt(2 * Dem_Sim * Order_Cost / Opportunity_Cost)
  Total_IC_S <- (Dem_Sim / Economic_OQ_S) * Order_Cost + (Economic_OQ_S / 2) * Opportunity_Cost
  Simulated_C[i] <- Total_IC_S
  Simulated_OQ[i] <- Economic_OQ_S
  Simulated_AO[i] <- Dem_Sim / Economic_OQ_S
}

# Calculation of Cost, Order Quantities, Annual Orders at Confidence Interval of 95%
Confidence_C <- mean(Simulated_C) + c(-1, 1) * 1.96 * sd(Simulated_C) / sqrt(NO_simulations)
Confidence_OQ <- mean(Simulated_OQ) + c(-1, 1) * 1.96 * sd(Simulated_OQ) / sqrt(NO_simulations)
Confidence_AO <- mean(Simulated_AO) + c(-1, 1) * 1.96 * sd(Simulated_AO) / sqrt(NO_simulations)

cat("Confidence Interval for Minimum Total Cost:", Confidence_C, "\n")
cat("Confidence interval for Order Quantity:", Confidence_OQ, "\n")
cat("Confidence interval for Annual Number of Orders:", Confidence_AO, "\n")

# Histograms for Simulated Minimum Total Costs, Simulated Order Quantities, and Simulated Annual Number of Orders
hist(Simulated_C, 
     main = "Histogram of Simulated Minimum Total Costs", 
     xlab = "Total Cost", 
     ylab = "Frequency", 
     col = "lightyellow", 
     border = "darkgreen")

hist(Simulated_OQ, 
     main = "Histogram of Simulated Order Quantities", 
     xlab = "Order Quantity", 
     ylab = "Frequency", 
     col = "lightyellow", 
     border = "darkgreen")

hist(Simulated_AO, 
     main = "Histogram of Simulated Annual Number of Orders", 
     xlab = "Annual Number of Orders", 
     ylab = "Frequency", 
     col = "lightyellow", 
     border = "darkgreen")

# Q-Q Plot for Simulated Minimum Total Costs, Simulated Order Quantities, and Simulated Annual Number of Orders
qqnorm(Simulated_C, main = "Q-Q Plot of Simulated Minimum Total Costs")
qqline(Simulated_C, col = "red", lwd = 2)
qqnorm(Simulated_OQ, main = "Q-Q Plot of Simulated Order Quantities")
qqline(Simulated_OQ, col = "red", lwd = 2)
qqnorm(Simulated_AO, main = "Q-Q Plot of Simulated Annual Number of Orders")
qqline(Simulated_AO, col = "red", lwd = 2)

# Validating with the Goodness of Fit Test
gof1 <- fitdist(Simulated_C, "weibull", method="mle")
gofstat(gof1)
gof2 <- fitdist(Simulated_OQ,"weibull", method="mle")
gofstat(gof2)
gof3 <- fitdist(Simulated_AO,"weibull", method="mle")
gofstat(gof3)

# Summary of Goodness of Fit Test Results
summary(gof1)
summary(gof2)
summary(gof3)

# Plotting the Goodness of Fit Test Results
plot(gof1)
plot(gof2)
plot(gof3)

# Clears the variable
rm(list=ls())
# Clears the plotcat
dev.off()
# Clears the Console
cat("\014")

