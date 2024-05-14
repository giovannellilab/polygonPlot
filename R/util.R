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

#' Calculates the length of each side of the polygon
#' 
#' @param df Data frame containing the coordinates in two columns: x and y
#' 
#' @return The list of lengths as a numeric vector.
#' 
#' @examples
#' .get_lengths(data.frame(x=c(8, 8, 22, 24), y=c(12, 15, 8, 8)))
#' 
#' @seealso [polygonPlot::.get_perimeter()]
#' 
#' @importFrom stats dist
#' @import checkmate
.get_lengths = function(df) {
  checkmate::assertDataFrame(x=df, col.names="named", ncols=2)
  
  len_list = c()
  
  # WARNING: this code assumes the order of the points is correct!
  # Get the points for each axis 2 by 2 (corresponding to each side)
  for (i in seq(from=1, to=nrow(df), by=2)) {
    len_list = c(len_list, stats::dist(df[i:(i+1),]))
  }
  
  return(len_list)
}

#' Calculates the perimeter of the polygon
#' 
#' @param df Data frame containing the coordinates in two columns: x and y
#' 
#' @return The perimeter as numeric.
#' 
#' @examples
#' .get_perimeter(data.frame(x=c(8, 8, 22, 24), y=c(12, 15, 8, 8)))
#' 
#' @seealso [polygonPlot::.get_area()]
#' 
#' @import checkmate
.get_perimeter = function(df) {
  checkmate::assertDataFrame(x=df, col.names="named", ncols=2)
  
  return(sum(.get_lengths(df)))
}

#' Collapses the original polygon by linking each segment to the last vertex
#' of the previous segment, basically connecting all sides to the base.
#' 
#' @param df Data frame containing the coordinates in two columns: x and y
#' 
#' @return The coordinates of the collapsed polygon as a data frame.
#' 
#' @examples
#' .collapse_polygon(data.frame(x=c(8, 8, 22, 24), y=c(12, 15, 8, 8)))
#' 
#' @seealso [polygonPlot::.get_area()]
#' 
.collapse_polygon = function(df) {

  # WARNING: this code assumes the order of the points is correct!
  # Connect each segment to the base (first vertex after base is number 3)
  for (i in seq(from=3, to=nrow(df), by=2)) {
    # Get translation in the Cartesian plane
    diff_vector = df[(i),] - df[i-1,]
    
    # Connect segment to the last vertex of the previous segment
    df[(i),] = df[(i),] - diff_vector # Same as setting as df[i-1,]
    df[(i+1),] = df[(i+1),] - diff_vector
  }
  
  # Remove duplicated vertices caused by connecting segments
  df = unique(df)
  
  return(df)
}

#' Calculates the area of the polygon.
#' 
#' @param df Data frame containing the coordinates in two columns: x and y
#' 
#' @return The area as numeric.
#' 
#' @examples
#' .get_area(data.frame(x=c(8, 8, 22, 24), y=c(12, 15, 8, 8)))
#' 
#' @seealso [polygonPlot::.collapse_polygon()]
#' 
#' @import checkmate
#' @importFrom pracma polyarea
.get_area = function(df) {
  checkmate::assertDataFrame(x=df, col.names="named", ncols=2)
  
  df = .collapse_polygon(df)
  
  return(pracma::polyarea(x=df$x, y=df$y))
}
