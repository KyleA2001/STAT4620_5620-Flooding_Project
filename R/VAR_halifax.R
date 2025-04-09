# install.packages("vars")
library(vars)
library(tseries)

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
VARselect(halifax_var, lag.max = 20, type = "none")

# Fit for all climatic factors
halifax_var_model <- VAR(halifax_var, p = 7, type = "none")
summary(halifax_var_model)

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

# Fit only the past value of the storm surge
# AIC = -90797.18
halifax_var_lm2 = lm(halifax_var$avg_water_surge[2:nrow(halifax_var)] ~ halifax_var$avg_water_surge[1:(nrow(halifax_var)-1)])
summary(halifax_var_lm2)
step(halifax_var_lm2)

par(mfrow=c(2, 2))
plt(halifax_var_lm2)
dev.off()

png("visualization/hf_acf_res_surgel1.png", width=800, height=600)
par(mfrow=c(1, 1))
acf(halifax_var_lm2$residuals, main="VAR ACF Residuals Model 2 - Halifax")
dev.off()

# APPLY FOR LAG DATA
# Define lag order
p <- 2  # Example: 2 lags

# Create lagged dataset but exclude lags of avg_water_surge
halifax_data_lags <- embed(as.matrix(halifax_var), p + 1)  # Embed function creates lagged matrix

# Convert to a dataframe
halifax_lagged_df <- as.data.frame(halifax_data_lags)

# Rename columns for clarity
colnames(halifax_lagged_df) <- c("avg_water_surge", "avg_temperature", "avg_wind_speed", "rain",
                                 "avg_water_surge.l1", "avg_temperature.l1", "avg_wind_speed.l1", "rain.l1",
                                 "avg_water_surge.l2", "avg_temperature.l2", "avg_wind_speed.l2", "rain.l2")

# Fit linear model without avg_water_surge lags
halifax_var_custom <- lm(
  avg_water_surge ~ avg_temperature.l1 + avg_wind_speed.l1 + rain.l1 +
                    avg_temperature.l2 + avg_wind_speed.l2 + rain.l2,
  data = halifax_lagged_df
)

# AIC = -80206.12
summary(halifax_var_custom)  # Check results
step(halifax_var_custom)
# does not have much difference between the AIC of the best and the original model

# Residual Check
par(mfrow=c(2, 2))
plot(halifax_var_custom)

png("visualization/hf_acf_res_covariates_lag.png", width=800, height=600)
par(mfrow=c(1, 1))
acf(halifax_var_custom$residuals, main="VAR ACF Residuals Model 3 - Halifax")
dev.off()

# Fit linear model without avg_water_surge lags
halifax_var_custom_v2 <- lm(avg_water_surge ~ avg_water_surge.l1 + avg_wind_speed.l1,
                            data = halifax_lagged_df)

# AIC = -91162.98
summary(halifax_var_custom_v2)  # Check results
step(halifax_var_custom_v2)

# Residual Check
png("visualization/hf_res_analysis_surge_wind_l1.png", width=800, height=600)
par(mfrow=c(2, 2))
plot(halifax_var_custom_v2)
dev.off()

png("visualization/hf_acf_res_surge_wind_l1.png", width=800, height=600)
par(mfrow=c(1, 1))
acf(halifax_var_custom_v2$residuals, main="VAR ACF Residuals Model 4 - Halifax")
dev.off()
