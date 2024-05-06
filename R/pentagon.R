
#' @import ggplot2
.pentagon <- function(axis_order, ticks, min_range, max_range, mindata, 
                      maxdata, axis_labels, fillcolor, alpha, linecolor,
                      linetype, lwd, title){
  # Plot axes number
  a1 <- as.integer(axis_order$axis1)
  a2 <- as.integer(axis_order$axis2)
  a3 <- as.integer(axis_order$axis3)
  a4 <- as.integer(axis_order$axis4)
  a5 <- as.integer(axis_order$axis5)

  s36=sin(pi/5)
  c36=cos(pi/5)
  s72=sin(2*pi/5)
  c72=cos(2*pi/5)

  p11 = c(10, 5)
  p12 = c(30, 5)
  p21 = c(     30+5*s72,      10-5*c72)
  p22 = c(p21[1]+20*c72, p21[2]+20*s72)
  p31 = c(30+20*c72+5*s36, 10+20*s72+5*c36)
  p32 = c(  p31[1]-20*c36,   p31[2]+20*s36)
  p41 = c(10-20*c72-5*s36, 10+20*s72+5*c36)
  p42 = c(  p41[1]+20*c36,   p41[2]+20*s36)
  p51 = c(     10-5*s72,      10-5*c72)
  p52 = c(p51[1]-20*c72, p51[2]+20*s72)

  # Normalize rectangle data
  pt_norm <- function(i, j, val, islog=NULL) {
    # i means draw on which axis and j means which set of data
    ratio = 20*(val - min_range[j])/(max_range[j] - min_range[j])
    if(i == 1) {
        res = c(p11[1]+ratio, p11[2])
    }
    else if(i == 2) {
        res = c(p21[1]+ratio*c72, p21[2]+ratio*s72)
    }
    else if(i == 3) {
        res = c(p31[1]-ratio*c36, p31[2]+ratio*s36)
    }
    else if(i == 4) {
        res = c(p41[1]+ratio*c36, p41[2]+ratio*s36)
    }
    else {
        res = c(p51[1]-ratio*c72, p51[2]+ratio*s72)
    }
    return(res)
  }

  ## Draw the lines
  ##
  t = 0.7
  line1 = c(p11[1]      , p11[2]-t    , p12[1]      , p12[2]-t   )
  line2 = c(p21[1]+t*s72, p21[2]-t*c72, p22[1]+t*s72, p22[2]-t*c72)
  line3 = c(p31[1]+t*s36, p31[2]+t*c36, p32[1]+t*s36, p32[2]+t*c36)
  line4 = c(p41[1]-t*s36, p41[2]+t*c36, p42[1]-t*s36, p42[2]+t*c36)
  line5 = c(p51[1]-t*s72, p51[2]-t*c72, p52[1]-t*s72, p52[2]-t*c72)

  #### draw polygon ####
  point_min1 <- pt_norm(1, a1, mindata[a1])
  point_max1 <- pt_norm(1, a1, maxdata[a1])
  point_min2 <- pt_norm(2, a2, mindata[a2])
  point_max2 <- pt_norm(2, a2, maxdata[a2])
  point_min3 <- pt_norm(3, a3, mindata[a3])
  point_max3 <- pt_norm(3, a3, maxdata[a3])
  point_min4 <- pt_norm(4, a4, mindata[a4])
  point_max4 <- pt_norm(4, a4, maxdata[a4])
  point_min5 <- pt_norm(5, a5, mindata[a5])
  point_max5 <- pt_norm(5, a5, maxdata[a5])

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
    y_line5 = c(line5[2], line5[4])
  )
  
  ## print lines for each axis ##
  p <- p + ggplot2::geom_line(data = df_coord_axis,
                              aes(x=x_line1, y=y_line1),
                              show.legend = FALSE)
  
  p <- p + ggplot2::geom_line(data = df_coord_axis,
                              aes(x=x_line2, y=y_line2),
                              show.legend = FALSE)
  
  p <- p + ggplot2::geom_line(data = df_coord_axis,
                              aes(x=x_line3, y=y_line3),
                              show.legend = FALSE)
  
  p <- p + ggplot2::geom_line(data = df_coord_axis,
                              aes(x=x_line4, y=y_line4),
                              show.legend = FALSE)
  
  p <- p + ggplot2::geom_line(data = df_coord_axis,
                              aes(x=x_line5, y=y_line5),
                              show.legend = FALSE)
  
  # Axis label
  p <- p + ggplot2::annotate(geom="text", 
                             x=(line1[1]+line1[3])/2, 
                             y=(line1[2]+line1[4])/2-3, 
                             label=axis_labels[a1], 
                             size=5)
  
  p <- p + ggplot2::annotate(geom="text", 
                             x=(line2[1]+line2[3])/2+3*s72,
                             y=(line2[2]+line2[4])/2-3*c72, 
                             label=axis_labels[a2], 
                             size=5, 
                             angle=72)
  
  p <- p + ggplot2::annotate(geom="text", 
                             x=(line3[1]+line3[3])/2+3*s36,
                             y=(line3[2]+line3[4])/2+3*c36, 
                             label=axis_labels[a3], 
                             size=5, 
                             angle=324)
  
  p <- p + ggplot2::annotate(geom="text", 
                             x=(line4[1]+line4[3])/2-3*s36,
                             y=(line4[2]+line4[4])/2+3*c36, 
                             label=axis_labels[a4], 
                             size=5, 
                             angle=36)
  
  p <- p + ggplot2::annotate(geom="text", 
                             x=(line5[1]+line5[3])/2-3*s72,
                             y=(line5[2]+line5[4])/2-3*c72, 
                             label=axis_labels[a5], 
                             size=5, 
                             angle=288)

  ### Add ticks for each axis ###
  ## Axis 1##
  if(!all(is.na(ticks[[a1]]))) {
      for (tick in ticks[[a1]]) {
          point <- pt_norm(1, a1, tick)
          p <- p + ggplot2::annotate(geom="segment", 
                                     x=point[1], xend = point[1], 
                                     y=point[2]-t+0.07, yend = point[2]-t-0.45)
          
          p <- p + ggplot2::annotate(geom="text", 
                                     x=point[1], 
                                     y=point[2]-t-1.1, 
                                     label=tick, size=3.5)
      }
  }

  #Axis 2#
  if(!all(is.na(ticks[[a2]]))) {
      for (tick in ticks[[a2]]) {
          point <- pt_norm(2, a2, tick)
          p <- p + ggplot2::annotate(geom="segment", 
                                     x=point[1]-(0.07-t)*s72, 
                                     xend = point[1]+(0.45+t)*s72,
                                     y=point[2]+(0.07-t)*c72, 
                                     yend = point[2]-(0.45+t)*c72)
          
          p <- p + ggplot2::annotate(geom="text", 
                                     x=point[1]+(1.1+t)*s72, 
                                     y=point[2]-(1.1+t)*c72, 
                                     label=tick, 
                                     size=3.5, 
                                     angle=72)
      }
  }

  #Axis 3#
  if(!all(is.na(ticks[[a3]]))) {
      for (tick in ticks[[a3]]) {
          point <- pt_norm(3, a3, tick)
          p <- p + ggplot2::annotate(geom="segment", 
                                     x=point[1]-(0.07-t)*s36, 
                                     xend = point[1]+(0.45+t)*s36,
                                     y=point[2]-(0.07-t)*c36, 
                                     yend = point[2]+(0.45+t)*c36)
          
          p <- p + ggplot2::annotate(geom="text", 
                                     x=point[1]+(1.1+t)*s36, 
                                     y=point[2]+(1.1+t)*c36, 
                                     label=tick, 
                                     size=3.5, 
                                     angle=324)
      }
  }

  # Axis 4 #
  if(!all(is.na(ticks[[a4]]))) {
      for (tick in ticks[[a4]]) {
          point <- pt_norm(4, a4, tick)
          p <- p + ggplot2::annotate(geom="segment", 
                                     x=point[1]+(0.07-t)*s36, 
                                     xend = point[1]-(0.45+t)*s36,
                                     y=point[2]-(0.07-t)*c36, 
                                     yend = point[2]+(0.45+t)*c36)
          
          p <- p + ggplot2::annotate(geom="text", 
                                     x=point[1]-(1.1+t)*s36, 
                                     y=point[2]+(1.1+t)*c36, 
                                     label=tick, 
                                     size=3.5, 
                                     angle=36)
      }
  }

  # Axis 5 #
  if(!all(is.na(ticks[[a5]]))) {
      for (tick in ticks[[a5]]) {
          point <- pt_norm(5, a5, tick)
          p <- p + ggplot2::annotate(geom="segment", 
                                     x=point[1]+(0.07-t)*s72, 
                                     xend = point[1]-(0.45+t)*s72,
                                     y=point[2]+(0.07-t)*c72, 
                                     yend = point[2]-(0.45+t)*c72)
          
          p <- p + ggplot2::annotate(geom="text", 
                                     x=point[1]-(1.1+t)*s72, 
                                     y=point[2]-(1.1+t)*c72, 
                                     label=tick, 
                                     size=3.5, 
                                     angle=288)
      }
  }
  
  # Add mean polygon
  p = p + .draw_mean_polygon(df_coord)

  return(p)
}
