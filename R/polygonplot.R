#' Draw polygon plot
#'
#' @param df data.frame containing the data. The first column of the dataframe
#' must contain the strings `axis_min`, `axis_max` and `data` in that order.
#' `axis_min` and `axis_max` are for the axis ranges. If left empty, the 
#' axis_min/max will be automatically calculated based on data given and 
#' extension will be decided based on the Axes range extension 
#' (`extra` parameter).
#' From the second column to the last, they need to contains the value 
#' of `axis_min`, `axis_max`, and `data` for all the variables of interest. 
#' From the third row, each cell can contain data values for the corresponding
#' column variable. Column names will be used as labels of the axis 
#' (starting from the second one). Change them in order to change the labels, 
#' but don't leave them empty. 
#' @param shape integer value to specify the shape of the polygon (3=Triangle, 
#' 4=Square, 5=Pentagon, 6=Hexagon)
#' @param extra axis range extension
#' @param fillcolor fill color of the polygon
#' @param alpha alpha value of the fill color
#' @param linecolor line color of the polygon border
#' @param linetype line type of the polygon border
#' @param lwd line width of the polygon border
#' @param labels_axis vector with the desired labels of the axis
#' @param title title of the plot
#' @param fix_aspect_ratio Boolean flag to fix the aspect ratio of the plot as 
#' `1`. It is strongly recommended to leave it as default value `TRUE`. 
#' NOTE: If you are going to change the theme of the returned ggplot object, 
#' remember to put in the `theme` function the following code 
#' `aspect.ratio = 1` in order to keep the text and the relative 
#' ticks aligned on the axis.
#'
#'
#' @return A `ggplot2` object.
#' 
#' 
#' @examples
#' df <- data.frame(
#'   info=c("axis_min", "axis_max", "data", ""),
#'   data1=c(-300, 500, 300, 350),
#'   data2=c(4, 14, -2, 12),
#'   data3=c(0, 1000, 10, 50),
#'   data4=c(0, 50, 20, 40)
#' )
#' 
#' p <- polygonplot(df, shape=4, fillcolor = "dodgerblue", linecolor = "blue")
#' p 
#'
#' @import checkmate
#' @export
polygonplot <- function(df, shape, 
                        extra = 0.5,
                        fillcolor = "black", 
                        alpha = 0.5,
                        linecolor = "black", 
                        linetype = "solid", 
                        lwd = 0.8, 
                        labels_axis = NULL,
                        annotation_tick_size = 3.5,
                        annotation_label_size = 5,
                        title = NULL, 
                        fix_aspect_ratio = TRUE){
  
  # Check params type
  checkmate::assertInt(shape, lower = 3, upper = 6)
  checkmate::assertDataFrame(df, min.cols = 1+shape, col.names = "named")
  
  checkmate::assertDouble(extra, lower = 0, upper = 1)
  checkmate::assertDouble(alpha, lower = 0, upper = 1)
  checkmate::assertChoice(linetype, c("blank", "solid", "dashed", "dotted", 
                                      "dotdash", "longdash", "twodash"))
  checkmate::assertDouble(lwd)
  checkmate::assertVector(labels_axis, len = shape, null.ok = TRUE)
  checkmate::assertCharacter(title, null.ok = TRUE)
  checkmate::assertFlag(fix_aspect_ratio)
  
  axis_order = list("axis1" = 1, "axis2" = 2, "axis3" = 3, 
                    "axis4" = 4, "axis5" = 5, "axis6" = 6)

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
  
  axis_labels <- c()
  # Names for each dataset
  if (is.null(labels_axis)) {
    for (i in seq.int(2,adj_shape)) {
      axis_labels <- c(axis_labels, names(df)[i])
    }
  } else {
    axis_labels <- labels_axis
  }
  
  # Calculate ticks for each axis and return NA if min/max range not available
  ticks <- list()
  for (i in seq.int(1, shape)) {
      ticks[[i]] <- if(!is.na(min_range[i]) && !is.na(max_range[i])) .ticks_number(i, min_range, max_range) else NA
  }

  if(shape == 3) {
    p <- .triangle(axis_order,
                   ticks,
                   min_range, max_range,
                   mindata, maxdata,
                   axis_labels,
                   fillcolor, alpha, linecolor,
                   linetype, lwd, annotation_tick_size, annotation_label_size,
                   title)
  } 
  else if (shape == 4){
    p <- .square(axis_order,
                 ticks,
                 min_range, max_range,
                 mindata, maxdata,
                 axis_labels,
                 fillcolor, alpha, linecolor,
                 linetype, lwd, annotation_tick_size, annotation_label_size,
                 title)
  }
  else if (shape == 5) {
    p <- .pentagon(axis_order,
                   ticks,
                   min_range, max_range,
                   mindata, maxdata,
                   axis_labels,
                   fillcolor, alpha, linecolor,
                   linetype, lwd, annotation_tick_size, annotation_label_size, 
                   title)
  }
  else {
    p <- .hexagon(axis_order,
                  ticks,
                  min_range, max_range,
                  mindata, maxdata,
                  axis_labels,
                  fillcolor, alpha, linecolor,
                  linetype, lwd, title)
  }
  
  p <- p + ggplot2::theme_void()
  
  if (fix_aspect_ratio) {
    p <- p + theme(aspect.ratio=1)
  }
  
  if (!is.null(title)){
    p <- p + ggplot2::ggtitle(title)
  }

  return(p)
}
