# install.packages("vars")
library(vars)
library(tseries)
library(forecast)

linear_interpolation_df = read.csv("data/linear_interpolation_df.csv")

# This will be the same for max/min water surge and avg/max/min water levels.
new_halifax_df_interpolation = subset(linear_interpolation_df, linear_interpolation_df$location == "Halifax")
new_yarmouth_df_interpolation = subset(linear_interpolation_df, linear_interpolation_df$location == "Yarmouth")

halifax_var <- new_halifax_df_interpolation[, c("avg_water_surge", "avg_temperature", "avg_wind_speed", "rain")]
head(halifax_var)

# ADF test for each time series
adf_test <- apply(halifax_var, 2, adf.test)
adf_test
# All time series are stationary

# AIC(n) 20 HQ(n) 19 SC(n) 11 FPE(n) 20
# The higher the lag the smaller AIC
VARselect(halifax_var, lag.max = 20, type = "none")

# Fit for all climatic factors
halifax_var_model <- VAR(halifax_var, p = 7, type = "none")

# R square = 0.6262, AIC = -39278.16

summary(halifax_var_model$varresult$avg_water_surge)
AIC(halifax_var_model$varresult$avg_water_surge)

# Residual ACF plot
png("visualization/hf_VAR_acf_res.png", width=800, height=600)
par(mfrow=c(1, 1))
acf(residuals(halifax_var_model)[,1], main="VAR ACF Residuals Model 1 - Halifax")
dev.off()

# Extract the coefficients for avg_water_surge
halifax_var_surge <- coef(halifax_var_model)$avg_water_surge

# Order the coefficients by p-value in descending order (lowest p-value first)
halifax_ordered_surge <- halifax_var_surge[order(halifax_var_surge[, "Pr(>|t|)"], decreasing = FALSE), , drop = FALSE]

# View the ordered coefficients and p-values
halifax_ordered_surge

# Model RMSE
# Train RMSE = 0.08396238
rmse_model = sqrt(mean(halifax_var_model$varresult$avg_water_surge$residuals^2))

# --- Check overfitting ---
# Time-series cross-validation using rolling windows
tsCV_VAR <- function(data, start, h = 7, p = 2) {
  n <- nrow(data)
  e <- numeric(n - start - h + 1)

  for (i in 1:(n - start - h + 1)) {
    # Training data
    train <- data[1:(start + i - 1), ]

    # Test data (h rows starting at position start+i)
    test <- data[(start + i):(start + i + h - 1), , drop = FALSE]

    # Fit model
    model <- VAR(train, p = p, type = "none")

    # Generate h-step ahead forecasts
    pred <- predict(model, n.ahead = h)

    # Extract forecasted values for water surge (all h steps)
    forecasts <- pred$fcst$avg_water_surge[1:h, "fcst"]

    # Extract actual values
    actuals <- test$avg_water_surge

    # Calculate error for this forecast window (mean across all h steps)
    e[i] <- mean(actuals - forecasts)
  }

  # Calculate RMSE
  rmse <- sqrt(mean(e^2, na.rm = TRUE))

  return(list(e = e, rmse = rmse))
}

# Example
cv_res_1 = tsCV_VAR(halifax_var, start = 18000, h = 1, p = 7)
# Test RMSE = 0.09233205
cv_res$rmse

# ---- Fit storm surge and wind speed ----
halifax_var2 <- halifax_var[c("avg_water_surge", "avg_wind_speed")]
halifax_var_model2 <- VAR(halifax_var2, p = 1, type = "none")

# R square = 0.5908, AIC = -37666.21
summary(halifax_var_model2$varresult$avg_water_surge)
AIC(halifax_var_model2$varresult$avg_water_surge)

# Residual ACF plot
png("visualization/hf_VAR_model2_acf_res.png", width=800, height=600)
par(mfrow=c(1, 1))
acf(halifax_var_model2$varresult$avg_water_surge$residuals, main="VAR ACF Residuals Model 2 - Halifax")
dev.off()

# ---- Fit only the past value of the storm surge ----
halifax_var_model3 = arima(halifax_var$avg_water_surge, order=c(1,0,0))
summary(halifax_var_model3)

# Get fitted values
fitted_values <- halifax_var$avg_water_surge - residuals(halifax_var_model3)

# Calculate R-squared
SSE <- sum(residuals(halifax_var_model3)^2)
SST <- sum((halifax_var$avg_water_surge - mean(halifax_var$avg_water_surge))^2)
r_squared <- 1 - SSE/SST
print(r_squared)

# R square = 0.4712638, AIC = -38196.53
AIC(halifax_var_model3)

png("visualization/hf_acf_res_surgel1.png", width=800, height=600)
par(mfrow=c(1, 1))
acf(na.omit(halifax_var_model3$resid), main="VAR ACF Residuals Model 3 - Halifax")
dev.off()

