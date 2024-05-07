#' Prepare dataframe for polygon plot
#'
#' @param dataframe classic data.frame with numeric values
#'
#' @return `data.frame` object ready for the polygon plot function
#' @export
#'
#' @examples
#' classic_df <- data.frame(
#'   data1=c(-300, 500, 300, 350),
#'   data2=c(4, 14, -2, 12),
#'   data3=c(0, 1000, 10, 50),
#'   data4=c(0, 50, 20, 40)
#' )
#' 
#' df <- prepare_dataframe(classic_df)
#' df
prepare_dataframe <- function(dataframe){
  m <- as.data.frame(dataframe)
  
  ax_min <- apply(m,2,min, na.rm = TRUE)
  ax_max <- apply(m,2,max, na.rm = TRUE)
  
  # m <- rbind(ax_max, m)
  # m <- rbind(ax_min, m)
  
  # Axis min and max will be computed when plotting
  m <- rbind(rep(NA, ncol(m)), m)
  m <- rbind(rep(NA, ncol(m)), m)
  
  offset <- nrow(m) - 3
  info <- c("axis_min", "axis_max", "data", rep(NA, offset))
  m <- cbind(info, m)
  
  return(m)
}
