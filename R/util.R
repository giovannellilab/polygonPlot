
# Creation of the internal object
.internal_obj <- function(dataframe){
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
