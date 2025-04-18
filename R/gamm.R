library(nlme)
library(mgcv)

new_df_linear_interpolation = read.csv("data/linear_interpolation_df.csv")
new_df_linear_interpolation$date_num = as.numeric(as.Date(new_df_linear_interpolation$date))

names(new_df_linear_interpolation)

location_gamm <- gamm(
  avg_water_surge ~
    s(avg_temperature) +
    s(avg_wind_speed) +
    s(rain) +
    date_num,
  random = list(location = ~ 1),
  # Random effect for location
  data = new_df_linear_interpolation
)

# Random Effect
summary(location_gamm$lme)

# Smooth Term
summary(location_gamm$gam)

png("visualization/location_gamm_acf_res.png", width=1200, height=500)
par(mfrow=c(1, 2))
acf(residuals(location_gamm$lme), main="ACF of residuals of Random Effect part in GAMM")
acf(residuals(location_gamm$gam), main="ACF of residuals of Smooth part in GAMM")
dev.off()

# Extract residuals of Smooth
png("visualization/location_gamm_res_gam.png", width=1200, height=500)
par(mfrow=c(1, 2))
residuals_gamm_location_gam <- resid(location_gamm$gam)
residuals_pearson_location_gam <- residuals(location_gamm$gam, type = "pearson")
# Seem to scatter around 0
plot(fitted(location_gamm$gam), residuals_gamm_location_gam,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

# Not normal
qqnorm(residuals_gamm_location_gam, main = "QQ Plot of Residuals")
qqline(residuals_gamm_location_gam, col = "red")
dev.off()

# Extract residuals of Random
png("visualization/location_gamm_res_lme.png", width=1200, height=600)
par(mfrow=c(1, 2))
residuals_gamm_location_lme <- resid(location_gamm$lme)
residuals_pearson_location_lme <- residuals(location_gamm$lme, type = "pearson")
# Seem to scatter around 0
plot(fitted(location_gamm$lme), residuals_gamm_location_lme,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

# Not normal
qqnorm(residuals_gamm_location_lme, main = "QQ Plot of Residuals")
qqline(residuals_gamm_location_lme, col = "red")
dev.off()
