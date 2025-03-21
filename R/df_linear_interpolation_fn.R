#' Apply linear interpolation to fill missing values in specified columns of a dataframe.
#'
#' @param df A data frame containing numeric data.
#' @param col_names A character vector of column names to process.
#' @param cut_off The maximum length of consecutive missing values that can be filled with linear interpolation.
#' @return A data frame with missing values in the specified columns filled using linear interpolation.
#' @export


linear_interpolation_df <- function(df, col_names, cut_off){
  # Process each specified column
  for (col_name in col_names){
    input_vec <- df[[col_name]]
    # Find missing intervals
    input_vec_missingness_index <- find_missing_index(input_vec)

    for (i in 1:nrow(input_vec_missingness_index)){
      if (input_vec_missingness_index$missing_period[i] <= cut_off){
        # Starting position of missing interval
        start = input_vec_missingness_index$start_missing_index[i]
        # Ending position of missing interval
        end = input_vec_missingness_index$end_missing_index[i]

        # Use linear interpolation technique to fill the gap
        filling_gap <- linear_interpolation(data_vec = input_vec,
                                            start_missing_indices = start,
                                            end_missing_indices = end)
        # Return the gap with filled values
        input_vec[start:end] <- filling_gap
        # Return back to column in dataframe
        df[[col_name]] <- input_vec
      }
    }
  }
  return(df)
}
