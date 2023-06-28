#' Draw polygon plot
#'
#' @param df data.frame with all the axis values
#' @param shape string to specify the shape of the polygon (Triangle, Square, Pentagon, Hexagon)
#' @param extra axis range extension
#' @param fillcolor fill color
#' @param alpha alpha value
#' @param linecolor line color
#' @param linetype line type
#' @param lwd linewidth
#' @param title title
#'
#'
#' @return A `ggplot2` object.
#'
#' @import checkmate
#' @export
polygonplot <- function(df, shape, 
                        extra = 0.5,
                        axis_order = list("axis1" = 1, "axis2" = 2, "axis3" = 3, 
                                     "axis4" = 4, "axis5" = 5, "axis6" = 6),
                        fillcolor = "black", 
                        alpha = 0.5,
                        linecolor = "black", 
                        linetype = "solid", 
                        lwd = 0.8, 
                        title = "Polygon Plot", 
                        fix_aspect_ratio = TRUE){
  
  # Check params type
  checkmate::assertInt(shape, lower = 3, upper = 6)
  checkmate::assertDouble(extra, lower = 0, upper = 1)
  checkmate::assertList(axis_order, types = "numeric")
  axis_check_names <- c()
  for (i in seq.int(1, shape)) {
    axis_check_names <- c(axis_check_names, paste0("axis", i))
  }
  checkmate::assertNames(names(axis_order), must.include = axis_check_names)
  checkmate::assertDouble(alpha, lower = 0, upper = 1)
  checkmate::assertChoice(linetype, c("blank", "solid", "dashed", "dotted", 
                                      "dotdash", "longdash", "twodash"))
  checkmate::assertDouble(lwd)
  checkmate::assertFlag(fix_aspect_ratio)
  
  

  adj_shape <- shape+1

  data_flags <- c()
  for (i in seq.int(2, adj_shape)) {
      data_flags <- c(data_flags, sum(!is.na(df[i])))
  }

  l <- .gMinMax(df, shape)
  mindata <- l[[1]]
  maxdata <- l[[2]]

  # Ranges for each axis
  if (shape == 3) {
    min_range <- c(df[1,2], df[1,3], df[1,4])
    max_range <- c(df[2,2], df[2,3], df[2,4])
  }
  else if (shape == 4) {
    min_range <- c(df[1,2], df[1,3], df[1,4], df[1,5])
    max_range <- c(df[2,2], df[2,3], df[2,4], df[2,5])
  } else if (shape == 5) {
    min_range <- c(df[1,2], df[1,3], df[1,4], df[1,5], df[1,6])
    max_range <- c(df[2,2], df[2,3], df[2,4], df[2,5], df[2,6])
  } else {
    min_range <- c(df[1,2], df[1,3], df[1,4], df[1,5], df[1,6], df[1,7])
    max_range <- c(df[2,2], df[2,3], df[2,4], df[2,5], df[2,6], df[2,7])
  }

  ## If min/max range is missing, calculate the range based on values
  x_extra = extra
  for (i in seq.int(1,shape)) {
      if(data_flags[i] > 0) {
          if(is.na(mindata[i]) && is.na(maxdata[i])) {
              next
          }
          min_temp = min(c(mindata[i], maxdata[i]), na.rm=TRUE)
          max_temp = max(c(mindata[i], maxdata[i]), na.rm=TRUE)
          if(is.na(min_range[i])){
              min_range[i] = min_temp - x_extra*(max_temp-min_temp)
          }
          if(is.na(max_range[i])){
              max_range[i] = max_temp + x_extra*(max_temp-min_temp)
          }
      }
  }
  # Names for each dataset
  axis_labels <- c()
  for (i in seq.int(2,adj_shape)) {
      axis_labels <- c(axis_labels, names(df)[i])
  }

  # Calculate ticks for each axis and return NA if min/max range not available
  ticks <- list()
  for (i in seq.int(1, shape)) {
      ticks[[i]] <- if(!is.na(min_range[i]) && !is.na(max_range[i])) .ticks_number(i, min_range, max_range) else NA
  }

  if(shape == 3) {
    p <- .triangle(df,
                   axis_order,
                   ticks,
                   min_range, max_range,
                   mindata, maxdata,
                   axis_labels,
                   fillcolor, alpha, linecolor,
                   linetype, lwd, title)

  } else if (shape == 4){
    p <- .square(df,
                 axis_order,
                 ticks,
                 min_range, max_range,
                 mindata, maxdata,
                 axis_labels,
                 fillcolor, alpha, linecolor,
                 linetype, lwd, title)

  }
  else if (shape == 5) {
    p <- .pentagon(df,
                   axis_order,
                   ticks,
                   min_range, max_range,
                   mindata, maxdata,
                   axis_labels,
                   fillcolor, alpha, linecolor,
                   linetype, lwd, title)

  }
  else {
    p <- .hexagon(df,
                  axis_order,
                  ticks,
                  min_range, max_range,
                  mindata, maxdata,
                  axis_labels,
                  fillcolor, alpha, linecolor,
                  linetype, lwd, title)

  }
  
  p <- p + .clean_theme()
  
  if (fix_aspect_ratio) {
    p <- p + theme(aspect.ratio=1)
  }
  
  return(p)
}
