#' Title stdev()
#'
#' @description This function calculates the standard deviation of a vector of numbers, automatically removing any missing values. The input must be numeric, or the function will throw an error.
#'
#' @param w a numeric vector, named 'w' in keeping with conventions for vectors
#'
#' @return The function returns the standard deviation of the vector. The mean and length of the vector are also printed as the function runs, to double-check that the values used for calculating the standard deviation are correct.
#' @export
#'
#' @examples
#' #Calculate standard deviation of a vector
#' vec1 <- c(1:20)
#' stdev(vec1)
#'
#' #Calculate standard deviation of a vector with missing values
#' vec2 <- c(2,3,4,5,NA,NA,12,17)
#' stdev(vec2)
#'
#' #Calculate standard deviation of a numeric columns within a data frame
#' var1 <- c(1:10)
#' var2 <- c(3:12)
#' df <- data.frame(var1, var2)
#' stdev(df$var2)
#'
#'
stdev <- function(w) {
  stopifnot(is.numeric(w))
  w = w[!is.na(w)]
  mu = mean(w)
  print(paste("Mean:", mu))
  n = length(w)
  print(paste("Length:", n))
  sum_sq = 0
  for (i in 1:length(w)) {
    sum_sq = sum_sq + (w[i] - mu)^2
  }
  sd_sq = sum_sq/(n - 1)
  stdeviation = sqrt(sd_sq)
  print("Standard deviation:")
  return(stdeviation)
}
