# install.packages("vars")
library(vars)
library(tseries)

linear_interpolation_df = read.csv("data/linear_interpolation_df.csv")

# This will be the same for max/min water surge and avg/max/min water levels.
new_halifax_df_interpolation = subset(linear_interpolation_df, linear_interpolation_df$location == "Halifax")
new_yarmouth_df_interpolation = subset(linear_interpolation_df, linear_interpolation_df$location == "Yarmouth")

yarmouth_var <- new_yarmouth_df_interpolation[, c("avg_water_surge", "avg_temperature", "avg_wind_speed", "rain")]
head(yarmouth_var)

# ADF test for each time series
adf_test <- apply(yarmouth_var, 2, adf.test)
adf_test
# All time series are stationary

# AIC(n) 20 HQ(n) 19 SC(n) 11 FPE(n) 20
# The higher the lag the smaller AIC
VARselect(yarmouth_var, lag.max = 20, type = "none")

# Fit for all climatic factors
yarmouth_var_model <- VAR(yarmouth_var, p = 7, type = "none")

# R square = 0.5598, AIC = -40582.28
summary(yarmouth_var_model$varresult$avg_water_surge)
AIC(yarmouth_var_model$varresult$avg_water_surge)

# Residual ACF plot
png("visualization/ym_VAR_acf_res.png", width=800, height=600)
par(mfrow=c(1, 1))
acf(residuals(yarmouth_var_model)[,1], main="VAR ACF Residuals Model 1 - Yarmouth")
dev.off()

# Extract the coefficients for avg_water_surge
yarmouth_var_surge <- coef(yarmouth_var_model)$avg_water_surge

# Order the coefficients by p-value in descending order (lowest p-value first)
yarmouth_ordered_surge <- yarmouth_var_surge[order(yarmouth_var_surge[, "Pr(>|t|)"], decreasing = FALSE), , drop = FALSE]

# View the ordered coefficients and p-values
yarmouth_ordered_surge

# ---- Fit storm surge and wind speed ----
yarmouth_var2 <- yarmouth_var[c("avg_water_surge", "avg_wind_speed")]
yarmouth_var_model2 <- VAR(yarmouth_var2, p = 1, type = "none")

# R square = 0.5031, AIC = -38267.76
summary(yarmouth_var_model2$varresult$avg_water_surge)
AIC(yarmouth_var_model2$varresult$avg_water_surge)

# Residual ACF plot
png("visualization/ym_VAR_model2_acf_res.png", width=800, height=600)
par(mfrow=c(1, 1))
acf(yarmouth_var_model2$varresult$avg_water_surge$residuals, main="VAR ACF Residuals Model 2 - Yarmouth")
dev.off()

# ---- Fit only the past value of the storm surge ----
yarmouth_var_model3 = arima(yarmouth_var$avg_water_surge, order=c(1,0,0))
summary(yarmouth_var_model3)

# Get fitted values
fitted_values <- yarmouth_var$avg_water_surge - residuals(yarmouth_var_model3)

# Calculate R-squared
SSE <- sum(residuals(yarmouth_var_model3)^2)
SST <- sum((yarmouth_var$avg_water_surge - mean(yarmouth_var$avg_water_surge))^2)
r_squared <- 1 - SSE/SST
print(r_squared)

# R square = 0.5005421, AIC = -38167
AIC(yarmouth_var_model3)

png("visualization/ym_acf_res_surgel1.png", width=800, height=600)
par(mfrow=c(1, 1))
acf(na.omit(yarmouth_var_model3$resid), main="VAR ACF Residuals Model 3 - Yarmouth")
dev.off()
