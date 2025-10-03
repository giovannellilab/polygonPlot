
#' @import ggplot2
.hexagon <- function(axis_order, ticks, min_range, max_range, mindata, 
                     maxdata, axis_labels, fillcolor, alpha, linecolor,
                     linetype, lwd, annotation_tick_size, annotation_label_size, 
                     title) {
  
  # Plot axes number
  a1 <- as.integer(axis_order$axis1)
  a2 <- as.integer(axis_order$axis2)
  a3 <- as.integer(axis_order$axis3)
  a4 <- as.integer(axis_order$axis4)
  a5 <- as.integer(axis_order$axis5)
  a6 <- as.integer(axis_order$axis6)

  s60 <- sin(pi/3)
  c60 <- cos(pi/3)

  p11 <- c(10, 5)
  p12 <- c(30, 5)
  p21 <- c(     30+5*s60,      10-5*c60)
  p22 <- c(p21[1]+20*c60, p21[2]+20*s60)
  p31 <- c(     40+5*s60, 10+20*s60+5*c60)
  p32 <- c(p31[1]-20*c60,   p31[2]+20*s60)
  p41 <- c(10, 10+40*s60+5)
  p42 <- c(30, 10+40*s60+5)
  p51 <- c(      0-5*s60, 10+20*s60+5*c60)
  p52 <- c(p51[1]+20*c60,   p51[2]+20*s60)
  p61 <- c(     10-5*s60,      10-5*c60)
  p62 <- c(p61[1]-20*c60, p61[2]+20*s60)

  # Normalize rectangle data
  hex_norm <- function(i, j, val, islog=NULL) {
      ratio = 20*(val - min_range[j])/(max_range[j] - min_range[j])
      if(i == 1) {
          res = c(p11[1]+ratio, p11[2])
      }
      else if(i == 2) {
          res = c(p21[1]+ratio*c60, p21[2]+ratio*s60)
      }
      else if(i == 3) {
          res = c(p31[1]-ratio*c60, p31[2]+ratio*s60)
      }
      else if(i == 4) {
          res = c(p41[1]+ratio, p41[2])
      }
      else if(i == 5) {
          res = c(p51[1]+ratio*c60, p51[2]+ratio*s60)
      }
      else {
          res = c(p61[1]-ratio*c60, p61[2]+ratio*s60)
      }
      return(res)
  }

  ## Draw the axes for the polygon
  t = 0.9
  line1 = c(p11[1]      , p11[2]-t    , p12[1]      , p12[2]-t   )
  line2 = c(p21[1]+t*s60, p21[2]-t*c60, p22[1]+t*s60, p22[2]-t*c60)
  line3 = c(p31[1]+t*s60, p31[2]+t*c60, p32[1]+t*s60, p32[2]+t*c60)
  line4 = c(p41[1]      , p41[2]+t    , p42[1]      , p42[2]+t   )
  line5 = c(p51[1]-t*s60, p51[2]+t*c60, p52[1]-t*s60, p52[2]+t*c60)
  line6 = c(p61[1]-t*s60, p61[2]-t*c60, p62[1]-t*s60, p62[2]-t*c60)

  #### draw polygon ####
  point_min1 <- hex_norm(1, a1, mindata[a1])
  point_max1 <- hex_norm(1, a1, maxdata[a1])
  point_min2 <- hex_norm(2, a2, mindata[a2])
  point_max2 <- hex_norm(2, a2, maxdata[a2])
  point_min3 <- hex_norm(3, a3, mindata[a3])
  point_max3 <- hex_norm(3, a3, maxdata[a3])
  point_min4 <- hex_norm(4, a4, mindata[a4])
  point_max4 <- hex_norm(4, a4, maxdata[a4])
  point_min5 <- hex_norm(5, a5, mindata[a5])
  point_max5 <- hex_norm(5, a5, maxdata[a5])
  point_min6 <- hex_norm(6, a6, mindata[a6])
  point_max6 <- hex_norm(6, a6, maxdata[a6])

  ## Data points on all axes
  x <- c()
  y <- c()

  ## Data points on axis1
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
  if(!is.na(point_min3[1]) && !is.na(point_min3[2])) {
      x <- c(x, point_min3[1])
      y <- c(y, point_min3[2])
  }
  if(!is.na(point_max3[1]) && !is.na(point_max3[2])) {
      x <- c(x, point_max3[1])
      y <- c(y, point_max3[2])
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
  ## Data points on axis5
  if(!is.na(point_max5[1]) && !is.na(point_max5[2])) {
      x <- c(x, point_max5[1])
      y <- c(y, point_max5[2])
  }
  if(!is.na(point_min5[1]) && !is.na(point_min5[2])) {
      x <- c(x, point_min5[1])
      y <- c(y, point_min5[2])
  }

  ## Data points on axis6
  if(!is.na(point_max6[1]) && !is.na(point_max6[2])) {
      x <- c(x, point_max6[1])
      y <- c(y, point_max6[2])
  }
  if(!is.na(point_min6[1]) && !is.na(point_min6[2])) {
      x <- c(x, point_min6[1])
      y <- c(y, point_min6[2])
  }

  df_coord <- data.frame(x=x, y=y)
  
  ## Add points delimiting range in the axis
  p <- ggplot2::ggplot(data = df_coord, aes(x, y)) +
    ggplot2::geom_point(show.legend = FALSE)
  
  p <- p + ggplot2::geom_polygon(aes(x=x, y=y), fill = fillcolor, alpha = alpha,
                                 colour = linecolor, linetype = linetype, 
                                 lwd = lwd, show.legend = TRUE)

  df_coord_axis <- data.frame(
    x_line1 = c(line1[1], line1[3]),
    y_line1 = c(line1[2], line1[4]),
    
    x_line2 = c(line2[1], line2[3]),
    y_line2 = c(line2[2],line2[4]),
    
    x_line3 = c(line3[1], line3[3]),
    y_line3 = c(line3[2], line3[4]),
    
    x_line4 = c(line4[1], line4[3]),
    y_line4 = c(line4[2], line4[4]),
    
    x_line5 = c(line5[1], line5[3]),
    y_line5 = c(line5[2], line5[4]),
    
    x_line6 = c(line6[1], line6[3]),
    y_line6 = c(line6[2], line6[4])
  )
  
  ## print lines for each axis ##
  p <- p + ggplot2::geom_line(data = df_coord_axis,
                              aes(x=get("x_line1"), y=get("y_line1")),
                              show.legend = FALSE)
  
  p <- p + ggplot2::geom_line(data = df_coord_axis,
                              aes(x=get("x_line2"), y=get("y_line2")),
                              show.legend = FALSE)
  
  p <- p + ggplot2::geom_line(data = df_coord_axis,
                              aes(x=get("x_line3"), y=get("y_line3")),
                              show.legend = FALSE)
  
  p <- p + ggplot2::geom_line(data = df_coord_axis,
                              aes(x=get("x_line4"), y=get("y_line4")),
                              show.legend = FALSE)
  
  p <- p + ggplot2::geom_line(data = df_coord_axis,
                              aes(x=get("x_line5"), y=get("y_line5")),
                              show.legend = FALSE)
  
  p <- p + ggplot2::geom_line(data = df_coord_axis,
                              aes(x=get("x_line6"), y=get("y_line6")),
                              show.legend = FALSE)
  

  # Axis labels
  p <- p + ggplot2::annotate(geom="text", 
                             x=(line1[1]+line1[3])/2, 
                             y=(line1[2]+line1[4])/2-3, 
                             label=axis_labels[a1], 
                             size=annotation_label_size)
  
  p <- p + ggplot2::annotate(geom="text", 
                             x=(line2[1]+line2[3])/2+3*s60,
                             y=(line2[2]+line2[4])/2-3*c60, 
                             label=axis_labels[a2], 
                             size=annotation_label_size, 
                             angle=60)
  
  p <- p + ggplot2::annotate(geom="text", 
                             x=(line3[1]+line3[3])/2+3*s60,
                             y=(line3[2]+line3[4])/2+3*c60, 
                             label=axis_labels[a3], 
                             size=annotation_label_size, 
                             angle=300)
  
  p <- p + ggplot2::annotate(geom="text", 
                             x=(line4[1]+line4[3])/2, 
                             y=(line4[2]+line4[4])/2+3, 
                             label=axis_labels[a4], 
                             size=annotation_label_size)
  
  p <- p + ggplot2::annotate(geom="text", 
                             x=(line5[1]+line5[3])/2-3*s60,
                             y=(line5[2]+line5[4])/2+3*c60, 
                             label=axis_labels[a5], 
                             size=annotation_label_size, 
                             angle=60)
  
  p <- p + ggplot2::annotate(geom="text", 
                             x=(line6[1]+line6[3])/2-3*s60,
                             y=(line6[2]+line6[4])/2-3*c60, 
                             label=axis_labels[a6], 
                             size=annotation_label_size, 
                             angle=300)

  ### Add ticks for each axis ###
  ## Axis 1##
  if(!all(is.na(ticks[[a1]]))) {
      for (tick in ticks[[a1]]) {
          point <- hex_norm(1, a1, tick)
          p <- p + ggplot2::annotate(geom="segment", 
                                     x=point[1], 
                                     xend = point[1], 
                                     y=point[2]-t+0.07, 
                                     yend = point[2]-t-0.45)
          
          p <- p + ggplot2::annotate(geom="text", 
                                     x=point[1], 
                                     y=point[2]-t-1.2, 
                                     label=tick, 
                                     size=annotation_tick_size)
      }
  }

  #Axis 2#
  if(!all(is.na(ticks[[a2]]))) {
      for (tick in ticks[[a2]]) {
          point <- hex_norm(2, a2, tick)
          p <- p + ggplot2::annotate(geom="segment", 
                                     x=point[1]-(0.07-t)*s60, 
                                     xend = point[1]+(0.45+t)*s60,
                                     y=point[2]+(0.07-t)*c60, 
                                     yend = point[2]-(0.45+t)*c60)
          
          p <- p + ggplot2::annotate(geom="text", 
                                     x=point[1]+(1.2+t)*s60, 
                                     y=point[2]-(1.2+t)*c60, 
                                     label=tick, 
                                     size=annotation_tick_size, 
                                     angle=60)
      }
  }

  #Axis 3#
  if(!all(is.na(ticks[[a3]]))) {
      for (tick in ticks[[a3]]) {
          point <- hex_norm(3, a3, tick)
          p <- p + ggplot2::annotate(geom="segment", 
                                     x=point[1]-(0.07-t)*s60, 
                                     xend = point[1]+(0.45+t)*s60,
                                     y=point[2]-(0.07-t)*c60, 
                                     yend = point[2]+(0.45+t)*c60)
          
          p <- p + ggplot2::annotate(geom="text", 
                                     x=point[1]+(1.2+t)*s60, 
                                     y=point[2]+(1.2+t)*c60, 
                                     label=tick, 
                                     size=annotation_tick_size, 
                                     angle=300)
      }
  }

  # Axis 4 #
  if(!all(is.na(ticks[[a4]]))) {
      for (tick in ticks[[a4]]) {
          point <- hex_norm(4, a4, tick)
          p <- p + ggplot2::annotate(geom="segment", 
                                     x=point[1], 
                                     xend = point[1], 
                                     y=point[2]-(0.07-t), 
                                     yend = point[2]+(0.45+t))
          
          p <- p + ggplot2::annotate(geom="text", 
                                     x=point[1], 
                                     y=point[2]+(1.2+t), 
                                     label=tick, 
                                     size=annotation_tick_size)
      }
  }

  # Axis 5 #
  if(!all(is.na(ticks[[a5]]))) {
      for (tick in ticks[[a5]]) {
          point <- hex_norm(5, a5, tick)
          p <- p + ggplot2::annotate(geom="segment", 
                                     x=point[1]+(0.07-t)*s60, 
                                     xend = point[1]-(0.45+t)*s60,
                                     y=point[2]-(0.07-t)*c60, 
                                     yend = point[2]+(0.45+t)*c60)
          
          p <- p + ggplot2::annotate(geom="text", 
                                     x=point[1]-(1.2+t)*s60, 
                                     y=point[2]+(1.2+t)*c60, 
                                     label=tick, 
                                     size=annotation_tick_size, 
                                     angle=60)
      }
  }

  # Axis 6 #
  if(!all(is.na(ticks[[a6]]))) {
      for (tick in ticks[[a6]]) {
          point <- hex_norm(6, a6, tick)
          p <- p + ggplot2::annotate(geom="segment", 
                                     x=point[1]+(0.07-t)*s60, 
                                     xend = point[1]-(0.45+t)*s60,
                                     y=point[2]+(0.07-t)*c60, 
                                     yend = point[2]-(0.45+t)*c60)
          
          p <- p + ggplot2::annotate(geom="text", 
                                     x=point[1]-(1.2+t)*s60, 
                                     y=point[2]-(1.2+t)*c60, 
                                     label=tick, 
                                     size=annotation_tick_size, 
                                     angle=300)
      }
  }

  # Add mean polygon
  p = p + .draw_mean_polygon(df_coord)
  
  # Print perimeter and area
  message(sprintf("Perimeter: %5.2f", .get_perimeter(df_coord)))
  message(sprintf("Area:      %5.2f", .get_area(df_coord)))


  return(p)
}
