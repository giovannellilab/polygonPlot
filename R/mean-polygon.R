#' Calculates the 
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
#' @import dplyr
#' @export
.get_mean_coords = function(df) {
  
  mean_df = data.frame()
  
  # WARNING: this code assumes the order of the points is correct!
  for (i in seq(from=1, to=nrow(df_coords), by=2)) {
    mean_df = mean_df %>%
      bind_rows(
        df_coords %>% slice(i, i+1) %>% summarise(x=mean(x), y=mean(y))
      )
  }
  
  return(mean_df)
}

#' Utility function to calculate the length of each side of the polygon
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
#' @import ggplot2
#' @export
.draw_mean_polygon = function(df) {
  checkmate::assertDataFrame(x=df, col.names="named", ncols=2)
  
  # Get mean coordinates from original ones
  mean_df = .get_mean_coords(df)
  
  p <- ggplot2::ggplot(mean_df)
  p <- p + ggplot2::geom_polygon(
    aes(x=x, y=y),
    fill="darkgrey",
    colour="darkgrey",
    alpha=0.35,
    linetype=1,
    linewidth=0.5
  )
  
  return(p)
}
