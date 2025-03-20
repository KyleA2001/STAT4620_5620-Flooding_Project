#' Apply linear interpolation to fill missingness of a specific interval in data.
#'
#' @param data_vec A numeric data vector.
#' @param start_missing_indices The position where the missingness interval begins.
#' @param end_missing_indices The position where the missingness interval ends.
#' @returns A numeric vector with the specified missing interval filled.
#' @examples
#' linear_interpolation(c(1, 2, 3, NA, NA, 6, 7), 4, 5)
#' @export

linear_interpolation <- function(data_vec, start_missing_indices, end_missing_indices){
  # Calculate the missing gap
  missing_size = end_missing_indices - start_missing_indices + 1

  # Use the end and begin data points to compute the slope of the missing gap
  slope = (data_vec[end_missing_indices+1]-data_vec[start_missing_indices-1])/(missing_size+1)

  # Generate the interpolated data
  filling_data = rep(NA, missing_size)
  for (i in 1:missing_size){
    # Use the slope to derive the missing value
    filling_data[i] = data_vec[start_missing_indices-1] + slope * i
  }

  return(filling_data)
}
