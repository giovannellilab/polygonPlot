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
#' @export
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
#' .get_perimeter(x=x, y=y)
#' 
#' @seealso [polygonPlot::.get_area()]
#' 
#' @import checkmate
#' @import dplyr
#' @export
.get_perimeter = function(df) {
  checkmate::assertDataFrame(x=df, col.names="named", ncols=2)
  
  return(sum(.get_lengths(df)))
}
