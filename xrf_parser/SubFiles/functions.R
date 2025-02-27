##### Normalisation of Rows function #####

normalize_rows <- function(data) {
  # Input validation
  if (!is.data.frame(data))
    stop("Input must be a data frame.")
  if (ncol(data) < 3)
    stop("Data must have at least 3 columns.")
  
  # Extracting IDs and readings
  id <- data[[1]]       # Assuming first column is ID
  reading <- data[[2]]  # Assuming second column is Reading
  
  # Normalizing rows
  normalized_data <- t(apply(data[, -c(1, 2), drop = FALSE], 1, function(row) {
    row_sum <- sum(row, na.rm = TRUE)
    if (row_sum == 0) {
      return(rep(0, length(row)))  # Handle rows that sum to 0
    }
    normalized_row <- round((row / row_sum * 100), digits = 2)
    return(normalized_row)
  }))
  
  # Combining results into a data frame
  normalized_data <- data.frame(id, reading, normalized_data)
  colnames(normalized_data) <- c(colnames(data)[1:2], colnames(data)[-c(1, 2)])
  
  return(normalized_data)
}

##### Skewness ######

skewness <- function(x) {
  abs(mean(x, na.rm = TRUE) - median(x, na.rm = TRUE))
}