### EVA Halifax : storm surge + wind speed as covariate

#Calculating the maximum storm surges for Halifax:

rm(list=ls())
full.data = read.csv("~/Documents/Dalhousie/Data Analysis/Project/EVA/linear_interpolation_df.csv") #This part need to be switched to the full dataset created by Thu

names(full.data)
# Extract two columns by name
full.max.data = full.data[, c("date", "location","avg_water_surge", "avg_wind_speed")]
full.max.data.Halifax = full.max.data[full.max.data$location == "Halifax", ]

library(data.table)

# Convert to data.table
setDT(full.max.data.Halifax)

# Extract the annual maximum for storm surge
annual.maximas.Halifax_ss = full.max.data.Halifax[, .(max_storm_surge = max(avg_water_surge, na.rm = TRUE)),
                                                  by = .(year = year(date))]

# Extract the annual maximum for wind speed
annual.maximas.Halifax_ws = full.max.data.Halifax[, .(max_wind_speed = max(avg_wind_speed, na.rm = TRUE)),
                                                  by = .(year = year(date))]
#do i have to scale/ standardized
#28 years
plot(annual.maximas.Halifax_ss)
plot(annual.maximas.Halifax_ws)

#Fit these Halifax annual maximas using the GEV model with storm surge + wind speed

# Load required libraries
library(ismev)
library(evd)


#transform wind speed as data frame
max_wind_speed <- data.frame(max_wind_speed = annual.maximas.Halifax_ws$max_wind_speed)
# Fit GEV model with a covariate (here wind speed affects the location parameter)
# Using 'mul = 1' indicates that the location parameter (mu) is modeled as:
# mu = beta0 + beta1 * (wind speed)
gev.surge_wind <- gev.fit(xdat = annual.maximas.Halifax_ss$max_storm_surge,
                          ydat = max_wind_speed,
                          mul = 1)

# Run diagnostics on the fitted model
gev.diag(gev.surge_wind)

# Extract the parameters from the fitted model:
# Now, the parameter vector is:
#   mle[1] = mu intercept (beta0)
#   mle[2] = mu covariate coefficient (beta1)
#   mle[3] = sigma (scale parameter)
#   mle[4] = xi (shape parameter)
mu_intercept <- gev.surge_wind$mle[1]    # Intercept for mu
mu_slope     <- gev.surge_wind$mle[2]      # Effect of wind speed on mu
sigma        <- gev.surge_wind$mle[3]      # Scale parameter (must be > 0)
xi           <- gev.surge_wind$mle[4]      # Shape parameter
V            <- gev.surge_wind$cov         # Covariance matrix for parameters

# Choose a reference value for the covariate (e.g., mean wind speed)
mean_wind <- mean(annual.maximas.Halifax_ws$max_wind_speed, na.rm = TRUE)

# Compute the location parameter mu for the reference wind speed
mu <- mu_intercept + mu_slope * mean_wind

# Define the return periods (in years)
T_values <- c(2, 5, 10, 20, 50, 100)
p <- 1 / T_values
yp <- -log(1 - p)

# Calculate return levels for each return period using the qgev function.
return_levels <- sapply(T_values, function(T) {
  qgev(1/T, loc = mu, scale = sigma, shape = xi, lower.tail = FALSE)
})


#LOOK THIS PART
# Calculate the variance of the return levels via the delta method.
# Since we now have 4 parameters, we define a gradient vector of length 4.
grad <- matrix(0, nrow = length(p), ncol = 4)
var_return_levels <- rep(0, length(p))

for (i in 1:length(p)) {
  # Here, we assume the derivative with respect to mu_slope is equal to the
  # reference covariate value (mean_wind).
  grad[i, ] <- c(1, mean_wind,
                 -xi^(-1) * (1 - yp[i]^(-xi)),
                 sigma * xi^(-2) * (1 - yp[i]^(-xi)) - sigma * xi^(-1) * yp[i]^(-xi) * log(yp[i]))
  # Compute variance using the delta method
  var_return_levels[i] <- t(grad[i, ]) %*% V %*% grad[i, ]
}

# Compute Standard Errors for the return levels
se_return_levels <- sqrt(var_return_levels)

# 95% Confidence Intervals for return levels
alpha <- 0.05
z_value <- qnorm(1 - alpha / 2)
lower_bounds <- return_levels - z_value * se_return_levels
upper_bounds <- return_levels + z_value * se_return_levels

# Print the computed bounds and return levels
print(lower_bounds)
print(return_levels)
print(upper_bounds)

# Plot the return levels with their confidence intervals
plot(T_values, return_levels, type = "b", col = "blue", pch = 19,
     xlab = "Return Period (Years)", ylab = "Return Level",
     main = "Halifax Return Levels (With Wind Speed Covariate)",
     ylim = c(min(lower_bounds), max(upper_bounds)))

lines(T_values, lower_bounds, type = "o", pch = 19, col = "red", lty = 2)
lines(T_values, upper_bounds, type = "o", pch = 19, col = "red", lty = 2)
legend("bottomright", legend = c("Return Levels", "95% Confidence Interval"),
       col = c("blue", "red"), lty = c(1, 2), pch = c(19, 19))


data.frame(Return_Period = T_values, Return_Level = return_levels)

################################################################################
#Same things with an other data set
#Calculating the maximum storm surges for Halifax:
rm(list=ls())
full.data = read.csv("~/Documents/Dalhousie/Data Analysis/Project/EVA/combination_df_v1.csv") #This part need to be switched to the full dataset created by Thu

names(full.data)
# Extract two columns by name
full.max.data = full.data[, c("date", "location","avg_water_surge", "avg_wind_speed")]
full.max.data.Halifax = full.max.data[full.max.data$location == "Halifax", ]

library(data.table)

# Convert to data.table
setDT(full.max.data.Halifax)

# Extract the annual maximum for storm surge
annual.maximas.Halifax_ss = full.max.data.Halifax[, .(max_storm_surge = max(avg_water_surge, na.rm = TRUE)),
                                                  by = .(year = year(date))]

# Extract the annual maximum for wind speed
annual.maximas.Halifax_ws = full.max.data.Halifax[, .(max_wind_speed = max(avg_wind_speed, na.rm = TRUE)),
                                                  by = .(year = year(date))]
#do i have to scale/ standardized
#28 years
plot(annual.maximas.Halifax_ss)
plot(annual.maximas.Halifax_ws)

#Fit these Halifax annual maximas using the GEV model with storm surge + wind speed
# Load required libraries
library(ismev)
library(evd)


#transform wind speed as data frame
max_wind_speed <- data.frame(max_wind_speed = annual.maximas.Halifax_ws$max_wind_speed)
# Fit GEV model with a covariate (here wind speed affects the location parameter)
# Using 'mul = 1' indicates that the location parameter (mu) is modeled as:
# mu = beta0 + beta1 * (wind speed)
gev.surge_wind <- gev.fit(xdat = annual.maximas.Halifax_ss$max_storm_surge,
                          ydat = max_wind_speed,
                          mul = 1)

# Run diagnostics on the fitted model
gev.diag(gev.surge_wind)

# Extract the parameters from the fitted model:
# Now, the parameter vector is:
#   mle[1] = mu intercept (beta0)
#   mle[2] = mu covariate coefficient (beta1)
#   mle[3] = sigma (scale parameter)
#   mle[4] = xi (shape parameter)
mu_intercept <- gev.surge_wind$mle[1]    # Intercept for mu
mu_slope     <- gev.surge_wind$mle[2]      # Effect of wind speed on mu
sigma        <- gev.surge_wind$mle[3]      # Scale parameter (must be > 0)
xi           <- gev.surge_wind$mle[4]      # Shape parameter
V            <- gev.surge_wind$cov         # Covariance matrix for parameters

# Choose a reference value for the covariate (e.g., mean wind speed)
mean_wind <- mean(annual.maximas.Halifax_ws$max_wind_speed, na.rm = TRUE)

# Compute the location parameter mu for the reference wind speed
mu <- mu_intercept + mu_slope * mean_wind

# Define the return periods (in years)
T_values <- c(2, 5, 10, 20, 50, 100)
p <- 1 / T_values
yp <- -log(1 - p)

# Calculate return levels for each return period using the qgev function.
return_levels <- sapply(T_values, function(T) {
  qgev(1/T, loc = mu, scale = sigma, shape = xi, lower.tail = FALSE)
})


#LOOK THIS PART
# Calculate the variance of the return levels via the delta method.
# Since we now have 4 parameters, we define a gradient vector of length 4.
grad <- matrix(0, nrow = length(p), ncol = 4)
var_return_levels <- rep(0, length(p))

for (i in 1:length(p)) {
  # Here, we assume the derivative with respect to mu_slope is equal to the
  # reference covariate value (mean_wind).
  grad[i, ] <- c(1, mean_wind,
                 -xi^(-1) * (1 - yp[i]^(-xi)),
                 sigma * xi^(-2) * (1 - yp[i]^(-xi)) - sigma * xi^(-1) * yp[i]^(-xi) * log(yp[i]))
  # Compute variance using the delta method
  var_return_levels[i] <- t(grad[i, ]) %*% V %*% grad[i, ]
}

# Compute Standard Errors for the return levels
se_return_levels <- sqrt(var_return_levels)

# 95% Confidence Intervals for return levels
alpha <- 0.05
z_value <- qnorm(1 - alpha / 2)
lower_bounds <- return_levels - z_value * se_return_levels
upper_bounds <- return_levels + z_value * se_return_levels

# Print the computed bounds and return levels
print(lower_bounds)
print(return_levels)
print(upper_bounds)

# Plot the return levels with their confidence intervals
plot(T_values, return_levels, type = "b", col = "blue", pch = 19,
     xlab = "Return Period (Years)", ylab = "Return Level",
     main = "Halifax Return Levels (With Wind Speed Covariate)",
     ylim = c(min(lower_bounds), max(upper_bounds)))

lines(T_values, lower_bounds, type = "o", pch = 19, col = "red", lty = 2)
lines(T_values, upper_bounds, type = "o", pch = 19, col = "red", lty = 2)
legend("bottomright", legend = c("Return Levels", "95% Confidence Interval"),
       col = c("blue", "red"), lty = c(1, 2), pch = c(19, 19))


data.frame(Return_Period = T_values, Return_Level = return_levels,
           Lower_Bound = lower_bounds, Upper_Bound = upper_bounds)
