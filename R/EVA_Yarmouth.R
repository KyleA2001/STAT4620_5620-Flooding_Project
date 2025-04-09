##EVA wind YArmouth##

# Calculating the maximum storm surges for Yarmouth:
# Using full data
rm(list=ls())
full.data = read.csv("~/Documents/Dalhousie/Data Analysis/Project/EVA/linear_interpolation_df.csv") #This part need to be switched to the full dataset created by Thu

# Extract two columns by name
full.max.data = full.data[, c("date", "location","avg_wind_speed")]
full.max.data.yarmouth = full.max.data[full.max.data$location == "Yarmouth", ]

library(data.table)
# Convert to data.table
setDT(full.max.data.yarmouth)

# Extract the annual maximum
annual.maximas.yarmouth = full.max.data.yarmouth[, .(max_wind = max(avg_wind_speed, na.rm = TRUE)),
                                                 by = .(year = year(date))]
#28 years
plot(annual.maximas.yarmouth, xlab = "years", ylab= "Storm surge annual maxima")
title("Yarmouth annual maxima storm surge (1956-2024")

# Fit these yarmouth annual maximas using the GEV.
library(ismev)
library(evd)

gev.surge = gev.fit(annual.maximas.yarmouth$max_wind, )
gev.diag(gev.surge)

# Extract the GEV parameters from the fitted model
mu = gev.surge$mle[1]    # Location parameter (mu)
sigma = gev.surge$mle[2] # Scale parameter (sigma)
xi = gev.surge$mle[3]    # Shape parameter (xi)
V = gev.surge$cov

# Define the return periods (e.g., 2, 5, and 10 years)
T_values = c(2, 5, 10, 20, 50, 100)
p = 1/T_values
yp = -log(1 - p)

# Calculate return levels for each return period
return_levels = sapply(T_values, function(T) {
  return_level = qgev(1/T, loc = mu, scale = sigma ,shape = xi, lower.tail = FALSE)
  return_level
})

grad = matrix(0,nrow = length(p), ncol = 3)
var_return_levels = rep(0,length(p))
for (i in 1:length(p)) {
  # Compute Gradient Vector (∇zp) of Return Levels
  grad[i,] = c(1, -xi^(-1) * (1 - yp[i]^(-xi)), sigma * xi^(-2) * (1 - yp[i]^(-xi)) - sigma * xi^(-1) * yp[i]^(-xi) * log(yp[i]))
  # Compute Variance of Return Levels
  var_return_levels[i] = t(grad[i,]) %*% V %*% grad[i,]
}

# Compute Standard Errors of Return Levels
se_return_levels =sqrt(var_return_levels)

# 95% Confidence Interval
alpha = 0.05
z_value = qnorm(1 - alpha / 2)
lower.bounds = return_levels - z_value * se_return_levels
upper.bounds = return_levels + z_value * se_return_levels

lower.bounds
return_levels
upper.bounds

# Plot the return levels for the specified return periods
plot(T_values, return_levels, type = "b", col = "blue", pch = 19,
     xlab = "Return Period (Years)", ylab = "Return Level",
     main = "Yarmouth Return Levels for Different Return Periods",ylim = c(min(lower.bounds), max(upper.bounds)))

lines(T_values,lower.bounds, type = "o", pch = 19, col = "red",lty = 2)
lines(T_values,upper.bounds, type = "o", pch = 19, col = "red",lty = 2)



###############################################################################
#Same analysis but with an other data set less complete

##Calculating the maximum storm surges for yarmouth:
rm(list=ls())
full.data = read.csv("~/Documents/Dalhousie/Data Analysis/Project/EVA/combination_df_v1.csv") #This part need to be switched to the

# Extract two columns by name
full.max.data = full.data[, c("date", "location","avg_wind_speed")]
full.max.data.yarmouth = full.max.data[full.max.data$location == "Yarmouth", ]

library(data.table)

# Convert to data.table
setDT(full.max.data.yarmouth)

# Extract the annual maximum
annual.maximas.yarmouth = full.max.data.yarmouth[, .(max_wind = max(avg_wind_speed, na.rm = TRUE)),
                                                 by = .(year = year(date))]
#28 years
plot(annual.maximas.yarmouth)

# Fit these yarmouth annual maximas using the GEV.
library(ismev)
library(evd)

gev.surge = gev.fit(annual.maximas.yarmouth$max_wind, )
gev.diag(gev.surge)

# Extract the GEV parameters from the fitted model
mu = gev.surge$mle[1]    # Location parameter (mu)
sigma = gev.surge$mle[2] # Scale parameter (sigma)
xi = gev.surge$mle[3]    # Shape parameter (xi)
V = gev.surge$cov

# Define the return periods (e.g., 2, 5, and 10 years)
T_values = c(2, 5, 10, 20, 50, 100)
p = 1/T_values
yp = -log(1 - p)
# Calculate return levels for each return period
return_levels = sapply(T_values, function(T) {
  return_level = qgev(1/T, loc = mu, scale = sigma ,shape = xi, lower.tail = FALSE)
  return_level
})

grad = matrix(0,nrow = length(p), ncol = 3)
var_return_levels = rep(0,length(p))
for (i in 1:length(p)) {
  # Compute Gradient Vector (∇zp) of Return Levels
  grad[i,] = c(1, -xi^(-1) * (1 - yp[i]^(-xi)), sigma * xi^(-2) * (1 - yp[i]^(-xi)) - sigma * xi^(-1) * yp[i]^(-xi) * log(yp[i]))
  # Compute Variance of Return Levels
  var_return_levels[i] = t(grad[i,]) %*% V %*% grad[i,]
}

# Compute Standard Errors of Return Levels
se_return_levels =sqrt(var_return_levels)

# 95% Confidence Interval
alpha = 0.05
z_value = qnorm(1 - alpha / 2)
lower.bounds = return_levels - z_value * se_return_levels
upper.bounds = return_levels + z_value * se_return_levels

lower.bounds
return_levels
upper.bounds

# Plot the return levels for the specified return periods
plot(T_values, return_levels, type = "b", col = "blue", pch = 19,
     xlab = "Return Period (Years)", ylab = "Return Level",
     main = "Yarmouth Return Levels for Different Return Periods",ylim = c(min(lower.bounds), max(upper.bounds)))

lines(T_values,lower.bounds, type = "o", pch = 19, col = "red",lty = 2)
lines(T_values,upper.bounds, type = "o", pch = 19, col = "red",lty = 2)
