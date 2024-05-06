
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
#' @import dplyr
.get_lengths = function(df) {
  checkmate::assertDataFrame(x=df, col.names="named", ncols=2)
  
  len_list = c()
  
  # WARNING: this code assumes the order of the points is correct!
  # Get the points for each axis 2 by 2 (corresponding to each side)
  for (i in seq(from=1, to=nrow(df), by=2)) {
    len_list = c(
      len_list,
      dist(df %>% slice(i, i+1))
    )
  }
  
  return(len_list)
}

#' Calculates the perimeter of the polygon
#' 
#' @param df Data frame containing the coordinates in two columns: x and y
#' 
#' @return The perimeter as float.
#' 
#' @examples
#' .get_perimeter(data.frame(x=c(8, 8, 22, 24), y=c(12, 15, 8, 8)))
#' 
#' @seealso [polygonPlot::.get_area()]
#' 
#' @import checkmate
#' @import dplyr
.get_perimeter = function(df) {
  checkmate::assertDataFrame(x=df, col.names="named", ncols=2)
  
  return(sum(.get_lengths(df)))
}

#' Calculates the area of the polygon. It serves as a wrapper for the different 
#' functions for each polygon type.
#' 
#' @param df Data frame containing the coordinates in two columns: x and y
#' 
#' @return The area as float.
#' 
#' @examples
#' .get_area(data.frame(x=c(8, 8, 22, 24), y=c(12, 15, 8, 8)), type="square")
#' 
#' @seealso [polygonPlot::.get_area_square()]
#' @seealso [polygonPlot::.get_perimeter()]
#' 
#' @import checkmate
#' @import dplyr
.get_area = function(df, type="square") {
  checkmate::assertDataFrame(x=df, col.names="named", ncols=2)
  
  area = NA
  
  if (type == "square") {
    area = .get_area_square(df)
  }
  else stop(paste("[!] Polygon type", polygon_type, "is not implemented!"))
  
  return(area)
}

#' Calculates the area of the square. Returns an upper bound estimation of the 
#' area since the construction of an irregular trapezoid cannot be ensured in
#' general.
#' 
#' @param df Data frame containing the coordinates in two columns: x and y
#' 
#' @return The area as float.
#' 
#' @examples
#' .get_area_square(data.frame(x=c(8, 8, 22, 24), y=c(12, 15, 8, 8)))
#' 
#' @seealso [polygonPlot::.get_area()]
#' 
#' @import dplyr
.get_area_square = function(df) {
  
  # WARNING: this code assumes the order of the points is correct!
  sides = .get_lengths(df)
  
  # Sort by value and get the top three
  sides = sort(sides, decreasing=TRUE)[1:3]
  
  # Use top two values as bases, third as height
  area = sum(sides[1:2]) * sides[3] / 2
  
  return(area)
}
