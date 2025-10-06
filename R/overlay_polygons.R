
#' Overlay different polygon plots on the same axis
#'
#' @param plot_list is a list object containing a series of polygon plots 
#' (ggplot2 object). All the polygon plot should be drawn on the same axis range
#' @param label_list is a vector of strings. Must have the same length of 
#' `plot_list` object
#' @param matching_color_points is a boolean specifying whether the points
#' should be colored according to `label_list`
#'
#' @return a ggplot2 object
#' @export
#'
#' @import checkmate
#' @import ggplot2

overlay_polygons <- function(plot_list, label_list, 
                             matching_color_points=FALSE,
                             legend_title = "Overlay Polygons",
                             include_mean_lines = FALSE) {
  checkmate::assertList(plot_list, types = "ggplot")
  checkmate::assertCharacter(label_list, len = length(plot_list), 
                             null.ok = FALSE, any.missing = FALSE)
  checkmate::assertString(legend_title)
  checkmate::assertFlag(include_mean_lines)
  checkmate::assertFlag(matching_color_points)
  
  
  first_plot <- plot_list[[1]]
  # Get the ggplot build from the first plot
  gb1 <- ggplot2::ggplot_build(first_plot)
  
  # Removing Geom Points from the first plot. Now Polygon will be the first layer
  first_plot$layers[[1]] <- NULL
  # Removing Geom Polygon from the first plot. 
  first_plot$layers[[1]] <- NULL
  
  # WARNING: Remove Mean Line Layer which is the last one
  if(!include_mean_lines) {
    first_plot$layers[[length(first_plot$layers)]] <- NULL
  }
  
  m <- gb1$data[[2]]
  m$plot_labels <- label_list[[1]]
  
  df_meanline <- data.frame()
  
  for(i in seq.int(from=2, to=length(plot_list))){
    gbX <- ggplot2::ggplot_build(plot_list[[i]])
    gbX$data[[2]]$plot_labels <- label_list[[i]]
    
    # Append polygon and point info
    m <- rbind(m, gbX$data[[2]])
    
    if (include_mean_lines) {
      # Appena meanline info
      df_meanline <- rbind(df_meanline, gbX$data[[length(gbX$data)]])
    }
  }
  
  fil <- as.character(m$fill)
  names(fil) <- as.character(m$plot_labels)
  
  al = m$alpha
  names(al) <- as.character(m$plot_labels)
  
  col <- as.character(m$colour)
  names(col) <- as.character(m$plot_labels)

  master_plot <- first_plot +
    ggplot2::geom_polygon(
      data=m,
      aes(
        x=get("x"),
        y=get("y"),
        fill=get("plot_labels"),
        alpha=get("plot_labels"),
        colour=get("plot_labels")
      ), 
      linewidth = unique(m$linewidth)
    ) +
    ggplot2::geom_point(
      data=m,
      aes(x=get("x"), y=get("y"), color=get("plot_labels"))
    ) +
    ggplot2::scale_fill_manual(values = fil, name = legend_title) + 
    ggplot2::scale_alpha_manual(values = al, name = legend_title) + 
    ggplot2::scale_colour_manual(values = col, name = legend_title)
  
  if(matching_color_points) {
    master_plot <- master_plot + 
      ggplot2::geom_point(
        data=m,
        aes(x=get("x"), y=get("y"), color=get("plot_labels"))
      ) + 
      ggplot2::scale_color_manual(values = fil)
  } else {
    master_plot <- master_plot + 
      ggplot2::geom_point(data=m, aes(x=get("x"), y=get("y")))
  }
  
  if (include_mean_lines) {
    master_plot <- master_plot + 
      ggplot2::geom_polygon(data=df_meanline,
                            aes(x=get("x"), y=get("y")), 
                            fill="darkgrey",
                            colour="darkgrey",
                            alpha=0.0,
                            linetype=1,
                            linewidth=0.5)
  }
  
  master_plot <- master_plot + ggplot2::ggtitle("")
  
  return(master_plot)
}
