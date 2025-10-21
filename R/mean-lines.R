#' Calculates the mean coordinates for further plotting the mean polygon
#' 
#' @param df Data frame containing the coordinates in two columns: x and y
#' 
#' @return A data.frame with the mean coordinates.
#' 
#' @seealso [polygonPlot::.get_perimeter()]
#' 
#' @import dplyr
.get_mean_coords = function(df) {
  mean_df = data.frame()
  
  # WARNING: this code assumes the order of the points is correct!
  for (i in seq(from=1, to=nrow(df), by=2)) {
    mean_df = mean_df %>%
      bind_rows(
        df %>% slice(i, i+1) %>% summarise(x=mean(get("x")), y=mean(get("y")))
      )
  }
  
  # Close polygon by repeating first point
  df_closed <- rbind(mean_df, mean_df[1, ])
  
  return(df_closed)
}

#' Plots the mean lines
#' 
#' @param df Data frame containing the coordinates in two columns: x and y
#' 
#' @return The mean lines as a `ggplot2` object.
#' 
#' @seealso [polygonPlot::.get_perimeter()]
#' 
#' @import checkmate
#' @import ggplot2
.draw_mean_lines = function(df, mean_line_color) {
  checkmate::assertDataFrame(x=df, col.names="named", ncols=2)
  
  # Get mean coordinates from original ones
  mean_df = .get_mean_coords(df)
  
  p <- ggplot2::geom_path(data=mean_df,
                            aes(x=x, y=y),
                            colour=mean_line_color,
                            linetype=1,
                            linewidth=0.5)
  
  return(p)
}
