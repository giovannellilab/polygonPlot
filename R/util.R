
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
#' @import checkmate
.get_lengths = function(df) {
  checkmate::assertDataFrame(x=df, col.names="named", ncols=2)
  
  len_list = c()
  
  # WARNING: this code assumes the order of the points is correct!
  # Get the points for each axis 2 by 2 (corresponding to each side)
  for (i in seq(from=1, to=nrow(df), by=2)) {
    len_list = c(len_list, dist(df[i:(i+1),]))
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

#' Calculates the area of the polygon. It serves as a wrapper for the different 
#' functions for each polygon type.
#' 
#' @param df Data frame containing the coordinates in two columns: x and y
#' @param shape Integer defining the type of polygon
#' 
#' @return The area as numeric.
#' 
#' @examples
#' .get_area(data.frame(x=c(8, 8, 22, 24), y=c(12, 15, 8, 8)), shape=4)
#' 
#' @seealso [polygonPlot::.get_area_triangle()]
#' @seealso [polygonPlot::.get_area_square()]
#' 
#' @import checkmate
.get_area = function(df, shape) {
  checkmate::assertDataFrame(x=df, col.names="named", ncols=2)
  checkmate::assertInt(x=shape)
  checkmate::assertChoice(x=shape, choices=3:6)
  
  area = case_when(
    shape == 3 ~ .get_area_triangle(df),
    shape == 4 ~ .get_area_square(df)
  )
  
  return(area)
}

#' Calculates the area of the triangle.
#' 
#' @param df Data frame containing the coordinates in two columns: x and y
#' 
#' @return The area as numeric.
#' 
#' @examples
#' .get_area_triangle(data.frame(x=c(8, 8, 22, 24), y=c(12, 15, 8, 8)))
#' 
#' @seealso [polygonPlot::.get_area()]
#' 
#' @import checkmate
.get_area_triangle = function(df) {
  
  # WARNING: this code assumes the order of the points is correct!
  sides = .get_lengths(df)
  
  checkmate::checkNumeric(
    x=sides,
    min.len=3,
    max.len=3
  )
  
  # Calculate area using Heron's formula
  area = 1/4 * sqrt(
    ( sides[1] + sides[2] + sides[3]) * 
    (-sides[1] + sides[2] + sides[3]) * 
    ( sides[1] - sides[2] + sides[3]) * 
    ( sides[1] + sides[2] - sides[3])
  )
  
  return(area)
}

#' Calculates the area of the square. Returns an upper bound estimation of the 
#' area since the construction of an irregular trapezoid cannot be ensured in
#' general.
#' 
#' @param df Data frame containing the coordinates in two columns: x and y
#' 
#' @return The area as numeric.
#' 
#' @examples
#' .get_area_square(data.frame(x=c(8, 8, 22, 24), y=c(12, 15, 8, 8)))
#' 
#' @seealso [polygonPlot::.get_area()]
#' 
#' @import checkmate
.get_area_square = function(df) {
  
  # WARNING: this code assumes the order of the points is correct!
  sides = .get_lengths(df)
  
  checkmate::checkNumeric(
    x=sides,
    min.len=4,
    max.len=4
  )
  
  # Sort by value and get the top three
  sides = sort(sides, decreasing=TRUE)[1:3]
  
  # Use top two values as bases, third as height
  area = sum(sides[1:2]) * sides[3] / 2
  
  return(area)
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
