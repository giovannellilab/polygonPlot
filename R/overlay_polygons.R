
#' Overlay different polygon plots on the same axis
#'
#' @param plot_list is a list object containing a series of polygon plots 
#' (ggplot2 object). All the polygon plot should be drawn on the same axis range
#' @param label_list is a vector of strings. Must have the same length of 
#' `plot_list` object
#' @param legend_title title of the legend
#' @param include_mean_lines logical flag indicating whether to include mean 
#' lines in the overlaid polygons
#'
#' @return a ggplot2 object
#' @export
#'
#' @importFrom stats setNames
#' @import checkmate
#' @import ggplot2

overlay_polygons <- function(plot_list, label_list, 
                             legend_title = "Overlay Polygons", 
                             include_mean_lines = FALSE) {
  checkmate::assertList(plot_list, types = "ggplot")
  checkmate::assertCharacter(label_list, len = length(plot_list), 
                             null.ok = FALSE, any.missing = FALSE)
  checkmate::assertString(legend_title)
  checkmate::assertFlag(include_mean_lines)
  
  first_plot <- plot_list[[1]]
  
  # Removing Geom Points from the first plot. Now Polygon will be the first layer
  first_plot$layers[[1]] <- NULL
  # Removing Geom Polygon from the first plot. 
  first_plot$layers[[1]] <- NULL
  
  if(!include_mean_lines) {
    first_plot$layers[[length(first_plot$layers)]] <- NULL
  }
  
  all_polygons <- list()
  all_meanlines <- list()
  
  for(i in seq.int(from=1, to=length(plot_list))) {
    gbX <- ggplot_build(plot_list[[i]])
    
    # Assumiamo che il poligono sia nel secondo layer
    m <- gbX$data[[2]]  
    m$plot_labels <- label_list[[i]]  # etichetta
    
    all_polygons[[i]] <- m
    
    df_meanline <- gbX$data[[length(gbX$data)]]
    df_meanline$plt_labels <- label_list[[i]]
    all_meanlines[[i]] <- df_meanline
  }
  
  master_plot <- first_plot
  
  for(i in seq.int(from=1, to=length(plot_list))) {
    df <- all_polygons[[i]]
    df_ml <- all_meanlines[[i]]
    
    master_plot <- master_plot +
      ggplot2::geom_polygon(data=df,
                            aes(x=get("x"), y=get("y"),group=get("plot_labels"), 
                                fill=get("plot_labels"),
                                colour=get("plot_labels"),
                                alpha=get("plot_labels")),
                            linewidth = unique(df$linewidth)) +
      ggplot2::geom_point(data=df,
                          aes(x=get("x"), y=get("y"), color=get("plot_labels")),
                          show.legend = FALSE) +
      if (include_mean_lines) geom_path(data=df_ml, 
                                        aes(x=get("x"), y=get("y"),
                                            group=get("plt_labels")), 
                                        colour=df_ml$colour,
                                        linetype=1,
                                        linewidth=0.5,
                                        show.legend = FALSE) else NULL
  }
  
  fil <- setNames(sapply(all_polygons, function(d) unique(d$fill)), label_list)
  col <- setNames(sapply(all_polygons, function(d) unique(d$colour)),label_list)
  al <- setNames(sapply(all_polygons, function(d) unique(d$alpha)), label_list)
  
  master_plot <- master_plot +
    ggplot2::scale_fill_manual(values = fil, name = legend_title) +
    ggplot2::scale_colour_manual(values = col, name = legend_title) +
    ggplot2::scale_alpha_manual(values = al, name = legend_title)
  
  return(master_plot)
}
