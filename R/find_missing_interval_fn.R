#' Identify and Document Missing Data Intervals in a Data Frame.
#'
#' @param input_vec A numeric data vector.
#'
#' @return A data frame with three columns:
#'   - `start_missing_index`: Starting positions of missing data intervals.
#'   - `end_missing_index`: Ending positions of missing data intervals.
#'   - `missing_period`: Lengths of the missing data intervals.
#'
#' @examples
#' find_missing_index(c(1, 2, 3, NA, NA, 6, 7))
#'
#' @export

find_missing_index <- function(input_vec){
  # Generate empty vector for storing values
  start_missing_index <- c()
  end_missing_index <- c()

  # Identify the missing value indices
  missing_values <- which(is.na(input_vec))

  start_missing_index[1] <- missing_values[1]

  # Identify separate intervals of missing values
  for (i in 2:(length(missing_values))){
    if (missing_values[i]-missing_values[i-1] > 1){
      start_missing_index <- c(start_missing_index, missing_values[i])
      end_missing_index <- c(end_missing_index, missing_values[i-1])
    }
  }

  # Add the final end index
  end_missing_index <- c(end_missing_index, missing_values[length(missing_values)])

  # Calculate missing periods
  missing_period <- end_missing_index - start_missing_index + 1

  # Put everything into a data frame
  input_vec_missingness_index <- as.data.frame(cbind(start_missing_index, end_missing_index, missing_period))
  return(input_vec_missingness_index)
}
