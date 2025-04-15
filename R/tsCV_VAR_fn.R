#' Time Series Cross-Validation for Vector Autoregression
#'
#' Performs rolling-origin cross-validation on a multivariate time series using a
#' Vector Autoregression (VAR) model. At each iteration, a VAR model is trained on
#' an expanding window of the training data, and h-step ahead forecasts are generated.
#' Forecast accuracy is evaluated by comparing predictions to the actual values
#' of a specified target variable.
#'
#' @param data A data frame or time series object containing the variables used in the VAR model.
#' @param start An integer specifying the starting index for cross-validation.
#' @param h Integer. Forecast horizon — the number of steps ahead to predict at each iteration. Default is 7.
#' @param p Integer. The lag order for the VAR model. Default is 2.
#' @param target_var String. The name of the target variable to evaluate forecasting accuracy.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{e}{A numeric vector of mean forecast errors across the forecast horizon for each iteration.}
#'   \item{rmse}{Root Mean Squared Error (RMSE) computed from the forecast errors.}
#' }
#'
#' @examples
#' result <- tsCV_VAR(my_data, start = 100, h = 7, p = 2, target_var = "avg_water_surge")
#' print(result$rmse)
#' @export

tsCV_VAR <- function(data, start, h = 7, p = 2, target_var = "") {
  if (!target_var %in% names(data)) {
    stop("The specified target_var is not a column in the data.")
  }

  n <- nrow(data)
  e <- numeric(n - start - h + 1)

  for (i in 1:(n - start - h + 1)) {
    # Training data
    train <- data[1:(start + i - 1), ]

    # Test data (h rows starting at position start + i)
    test <- data[(start + i):(start + i + h - 1), , drop = FALSE]

    # Fit VAR model
    model <- VAR(train, p = p, type = "none")

    # Forecast h steps ahead
    pred <- predict(model, n.ahead = h)

    # Safely extract forecast for target variable
    target_pred <- as.data.frame(pred$fcst[[target_var]])
    forecasts <- target_pred[1:h, "fcst"]

    # Actual observed values
    actuals <- test[[target_var]]

    # Mean error over forecast horizon
    e[i] <- mean(actuals - forecasts, na.rm = TRUE)
  }

  # Compute RMSE
  rmse <- sqrt(mean(e^2, na.rm = TRUE))

  return(list(e = e, rmse = rmse))
}

