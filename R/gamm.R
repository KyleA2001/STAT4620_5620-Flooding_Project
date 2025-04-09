library(nlme)
library(mgcv)

new_df_linear_interpolation = read.csv("data/linear_interpolation_df.csv")
new_df_linear_interpolation$date_num = as.numeric(as.Date(new_df_linear_interpolation$date))

names(new_df_linear_interpolation)

location_gamm <- gamm(
  avg_water_surge ~
    s(avg_temperature) +
    s(avg_wind_speed) +
    s(rain),
  random = list(location = ~ 1),
  # Random effect for location
  data = new_df_linear_interpolation
)

# Random Effect and Smooth Term
summary(location_gamm$lme)

png("visualization/location_gamm_acf_res_lme.png", width=800, height=600)
acf(residuals(location_gamm$lme), main="ACF of residuals of GAMM - LME")
dev.off()

# Random Effect
summary(location_gamm$gam)

png("visualization/location_gamm_acf_res_gam.png", width=800, height=600)
acf(residuals(location_gamm$gam), main="ACF of residuals of GAMM - GAM")
dev.off()




