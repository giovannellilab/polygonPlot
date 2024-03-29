
#' @import ggplot2
.square <- function(df, axis_order, ticks, min_range, max_range, mindata, 
                    maxdata, axis_labels, fillcolor, alpha, linecolor, 
                    linetype, lwd, title){
  
  # Plot axes number
  a1 <- as.integer(axis_order$axis1)
  a2 <- as.integer(axis_order$axis2)
  a3 <- as.integer(axis_order$axis3)
  a4 <- as.integer(axis_order$axis4)

  sq_norm <- function(i, j, val, islog=NULL) {
    # i means draw on which axis and j means which set of data
    ratio = 20* (val - min_range[j])/(max_range[j] - min_range[j])
    if(i == 1) {
        res = c(10 + ratio, 8)
    }
    else if(i == 2) {
        res = c(32, 10+ratio)
    }
    else if(i == 3) {
        res = c(10+ratio, 32)
    }
    else {
        res = c(8, 10+ratio)
    }
    return(res)
  }

  ## Draw the lines
  ##
  t = 0.5
  line1 = c(10  ,  8-t, 30  , 8-t)
  line2 = c(32+t, 10  , 32+t, 30)
  line3 = c(10  , 32+t, 30  , 32+t)
  line4 = c( 8-t, 10  ,  8-t, 30)

  #### draw polygon ####
  point_min1 <- sq_norm(1, a1, mindata[a1])
  point_max1 <- sq_norm(1, a1, maxdata[a1])
  point_min2 <- sq_norm(2, a2, mindata[a2])
  point_max2 <- sq_norm(2, a2, maxdata[a2])
  point_min3 <- sq_norm(3, a3, mindata[a3])
  point_max3 <- sq_norm(3, a3, maxdata[a3])
  point_min4 <- sq_norm(4, a4, mindata[a4])
  point_max4 <- sq_norm(4, a4, maxdata[a4])

  ## Data points on axis1
  x <- c()
  y <- c()

  if(!is.na(point_min1[1]) && !is.na(point_min1[2])) {
      x <- c(x, point_min1[1])
      y <- c(y, point_min1[2])
  }
  if(!is.na(point_max1[1]) && !is.na(point_max1[2])) {
      x <- c(x, point_max1[1])
      y <- c(y, point_max1[2])
  }

  ## Data points on axis2
  if(!is.na(point_min2[1]) && !is.na(point_min2[2])) {
      x <- c(x, point_min2[1])
      y <- c(y, point_min2[2])
  }
  if(!is.na(point_max2[1]) && !is.na(point_max2[2])) {
      x <- c(x, point_max2[1])
      y <- c(y, point_max2[2])
  }

  ## Data points on axis3
  if(!is.na(point_max3[1]) && !is.na(point_max3[2])) {
      x <- c(x, point_max3[1])
      y <- c(y, point_max3[2])
  }
  if(!is.na(point_min3[1]) && !is.na(point_min3[2])) {
      x <- c(x, point_min3[1])
      y <- c(y, point_min3[2])
  }

  ## Data points on axis4
  if(!is.na(point_max4[1]) && !is.na(point_max4[2])) {
      x <- c(x, point_max4[1])
      y <- c(y, point_max4[2])
  }
  if(!is.na(point_min4[1]) && !is.na(point_min4[2])) {
      x <- c(x, point_min4[1])
      y <- c(y, point_min4[2])
  }

  p <- ggplot2::qplot(x,y)
  p <- p + ggplot2::geom_polygon(aes(x=x, y=y), fill = fillcolor, alpha = alpha, 
                        colour = linecolor, linetype = linetype, lwd = lwd)

  p <- p + ggplot2::geom_line(aes(x=c(line1[1], line1[3]), 
                                  y=c(line1[2],line1[4])))
  p <- p + ggplot2::geom_line(aes(x=c(line2[1], line2[3]), 
                                  y=c(line2[2],line2[4])))
  p <- p + ggplot2::geom_line(aes(x=c(line3[1], line3[3]), 
                                  y=c(line3[2],line3[4])))
  p <- p + ggplot2::geom_line(aes(x=c(line4[1], line4[3]), 
                                  y=c(line4[2],line4[4])))

  ## print labels for each axis ##
  p <- p + ggplot2::annotate(geom="text", 
                             x=(line1[1]+line1[3])/2, 
                             y=(line1[2]+line1[4])/2-2, 
                             label=axis_labels[a1], 
                             size=5)
  
  p <- p + ggplot2::annotate(geom="text", 
                             x=(line2[1]+line2[3])/2+2, 
                             y=(line2[2]+line2[4])/2, 
                             label=axis_labels[a2], 
                             size=5, 
                             angle=90)
  
  p <- p + ggplot2::annotate(geom="text", 
                             x=(line3[1]+line3[3])/2, 
                             y=(line3[2]+line3[4])/2+2, 
                             label=axis_labels[a3], 
                             size=5)
  
  p <- p + ggplot2::annotate(geom="text", 
                             x=(line4[1]+line4[3])/2-2, 
                             y=(line4[2]+line4[4])/2, 
                             label=axis_labels[a4], 
                             size=5, 
                             angle=90)

  ### Add ticks for each axis ###
  ## Axis 1##
  if(!all(is.na(ticks[[a1]]))) {
      for (tick in ticks[[a1]]) {
          point <- sq_norm(1, a1, tick)
          # All ticks
          p <- p + ggplot2::annotate(geom="segment", 
                                     x=point[1], 
                                     xend = point[1], 
                                     y=point[2]-t+0.05, 
                                     yend = point[2]-t-0.3)
          p <- p + ggplot2::annotate(geom="text", 
                                     x=point[1], 
                                     y=point[2]-t-0.7, 
                                     label=tick, 
                                     size=3.5)
      }
  }

  #Axis 2#
  if(!all(is.na(ticks[[a2]]))) {
      for (tick in ticks[[a2]]) {
          point <- sq_norm(2, a2, tick)
          # All ticks
          p <- p + ggplot2::annotate(geom="segment", 
                                     x=point[1]-(0.05-t), 
                                     xend = point[1]+(0.3+t), 
                                     y=point[2], 
                                     yend = point[2])
          p <- p + ggplot2::annotate(geom="text", 
                                     x=point[1]+(0.7+t), 
                                     y=point[2], 
                                     label=tick, 
                                     size=3.5, 
                                     angle=90)
      }
  }

  #Axis 3#
  if(!all(is.na(ticks[[a3]]))) {
      for (tick in ticks[[a3]]) {
          point <- sq_norm(3, a3, tick)
          p <- p + ggplot2::annotate(geom="segment", 
                                     x=point[1], 
                                     xend = point[1], 
                                     y=point[2]-(0.05-t), 
                                     yend = point[2]+(0.3+t))
          p <- p + ggplot2::annotate(geom="text", 
                                     x=point[1], 
                                     y=point[2]+(0.75+t), 
                                     label=tick, 
                                     size=3.5)
      }
  }

  # Axis 4 #
  if(!all(is.na(ticks[[a4]]))) {
      for (tick in ticks[[a4]]) {
          point <- sq_norm(4, a4, tick)
          p <- p + ggplot2::annotate(geom="segment", 
                                     x=point[1]+(0.05-t), 
                                     xend = point[1]-(0.3+t), 
                                     y=point[2], 
                                     yend = point[2])
          p <- p + ggplot2::annotate(geom="text", 
                                     x=point[1]-(0.7+t), 
                                     y=point[2], 
                                     label=tick, 
                                     size=3.5, 
                                     angle=90)
      }
  }

  # Add title and remove figure backgrounds
  p <- p + ggplot2::ggtitle(title)
  p + ggplot2::theme(plot.title = element_text(size=16, face='bold',hjust=0.5),
                     axis.line=element_blank(),
                     axis.text.x=element_blank(),
                     axis.text.y=element_blank(),
                     axis.ticks=element_blank(),
                     axis.title.x=element_blank(),
                     plot.margin = margin(20,20,20,20),
                     axis.title.y=element_blank(),
                     legend.position="none",
                     panel.background=element_blank(),
                     panel.border=element_blank(),
                     panel.grid.major=element_blank(),
                     panel.grid.minor=element_blank(),
                     plot.background=element_blank())

  return(p)
}
