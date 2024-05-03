
#' @import ggplot2
.triangle <- function(axis_order, ticks, min_range, max_range, mindata, 
                      maxdata, axis_labels, fillcolor, alpha, linecolor,
                      linetype, lwd, title){
  # Plot axes number
  a1 <- as.integer(axis_order$axis1)
  a2 <- as.integer(axis_order$axis2)
  a3 <- as.integer(axis_order$axis3)

  s60 = sin(pi/3)
  c60 = cos(pi/3)

  p11 = c(10, 8)
  p12 = c(30, 8)
  p21 = c(     30+2*s60,      10+2*c60)
  p22 = c(p21[1]-20*c60, p21[2]+20*s60)
  p31 = c(10-2*s60, 10+2*c60)
  p32 = c(p31[1]+20*c60,   p31[2]+20*s60)

  ### Function to normalize data into polygon positions ###
  tri_norm <- function(i, j, val){
    # i means draw on which axis and j means which set of data
    ratio = 20*(val - min_range[j])/(max_range[j] - min_range[j])
    if(i == 1) {
      res = c(p11[1]+ratio, p11[2])
    }
    else if(i == 2) {
      res = c(p21[1]-ratio*c60, p21[2]+ratio*s60)
    }
    else {
      res = c(p31[1]+ratio*c60, p31[2]+ratio*s60)
    }
    return(res)
  }

  ### draw axes ####
  ### (10, 10), (30, 10),
  t = 0.5
  line1 = c(p11[1],           p11[2]-t,       p12[1],     p12[2]-t)
  line2 = c(p21[1]+t*s60, p21[2]+t*c60, p22[1]+t*s60, p22[2]+t*c60)
  line3 = c(p31[1]-t*s60, p31[2]+t*c60, p32[1]-t*s60, p32[2]+t*c60)

  #### draw polygon ####
  point_min1 <- tri_norm(1, a1, mindata[a1])
  point_max1 <- tri_norm(1, a1, maxdata[a1])
  point_min2 <- tri_norm(2, a2, mindata[a2])
  point_max2 <- tri_norm(2, a2, maxdata[a2])
  point_min3 <- tri_norm(3, a3, mindata[a3])
  point_max3 <- tri_norm(3, a3, maxdata[a3])

  ## Data points for drawing polygons
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
  if(!is.na(point_max3[1]) && !is.na(point_max3[2])) {
    x <- c(x, point_max3[1])
    y <- c(y, point_max3[2])
  }
  if(!is.na(point_min3[1]) && !is.na(point_min3[2])) {
    x <- c(x, point_min3[1])
    y <- c(y, point_min3[2])
  }
  ## Plot triangles ##
  
  df_coord <- data.frame(x=x, y=y)
  
  # p <- ggplot2::qplot(x,y)
  
  
  p <- ggplot2::ggplot(data = df_coord, aes(x, y)) +
    ggplot2::geom_point(show.legend = FALSE)
  
  p <- p + ggplot2::geom_polygon(aes(x=x, y=y), fill = fillcolor, alpha = alpha,
                                 colour= linecolor, linetype = linetype, 
                                 lwd = lwd, show.legend = TRUE)

  df_coord_axis <- data.frame(
    x_line1 = c(line1[1], line1[3]),
    y_line1 = c(line1[2], line1[4]),
    
    x_line2 = c(line2[1], line2[3]),
    y_line2 = c(line2[2],line2[4]),
    
    x_line3 = c(line3[1], line3[3]),
    y_line3 = c(line3[2], line3[4])
  )
  
  ## Add axis for each boundary
  p <- p + ggplot2::geom_line(data = df_coord_axis,
                              aes(x=x_line1, y=y_line1),
                              show.legend = FALSE)
  
  p <- p + ggplot2::geom_line(data = df_coord_axis,
                              aes(x=x_line2, y=y_line2),
                              show.legend = FALSE)
  
  p <- p + ggplot2::geom_line(data = df_coord_axis,
                              aes(x=x_line3, y=y_line3),
                              show.legend = FALSE)

  ## Add labels for each axis ##
  p <- p + ggplot2::annotate(geom="text", 
                             x=(p11[1]+p12[1])/2, 
                             y=(p11[2]+p12[2])/2-2, 
                             label= axis_labels[a1], 
                             size=5)
  
  p <- p + ggplot2::annotate(geom="text",  
                             x=(p21[1]+p22[1])/2+2*s60, 
                             y=(p21[2]+p22[2])/2+2*c60, 
                             label=axis_labels[a2], 
                             size=5, 
                             angle=300)
  
  p <- p + ggplot2::annotate(geom="text", 
                             x=(p31[1]+p32[1])/2-2*s60, 
                             y=(p31[2]+p32[2])/2+2*c60,
                             label=axis_labels[a3], 
                             size=5, 
                             angle=60)

  ### Add ticks for each axis ###
  ## Axis 1##
  if(!all(is.na(ticks[[a1]]))) {
    for (tick in ticks[[a1]]) {
      point <- tri_norm(1, a1, tick)
      p <- p + ggplot2::annotate(geom="segment", 
                                 x=point[1], xend = point[1], 
                                 y=point[2]-t+0.05, yend = point[2]-t-0.3)
      
      p <- p + ggplot2::annotate(geom="text", x=point[1], y=point[2]-t-0.65, 
                                 label=tick, size=3.5)
    }
  }

  #Axis 2#
  if(!all(is.na(ticks[[a2]]))) {
    for (tick in ticks[[a2]]) {
      point <- tri_norm(2, a2, tick)
      p <- p + ggplot2::annotate(geom="segment", 
                                 x=point[1]-(0.05-t)*s60, 
                                 xend = point[1]+(0.3+t)*s60,
                                 y=point[2]-(0.05-t)*c60, 
                                 yend = point[2]+(0.3+t)*c60)
      
      p <- p + ggplot2::annotate(geom="text", 
                                 x=point[1]+(0.65+t)*s60, 
                                 y=point[2]+(0.65+t)*c60, 
                                 label=tick, size=3.5, angle=300)
    }
  }

  #Axis 3#
  if(!all(is.na(ticks[[a3]]))) {
    for (tick in ticks[[a3]]) {
      point <- tri_norm(3, a3, tick)
      p <- p + ggplot2::annotate(geom="segment", 
                                 x=point[1]+(0.05-t)*s60, 
                                 xend = point[1]-(0.3+t)*s60,
                                 y=point[2]-(0.05-t)*c60, 
                                 yend = point[2]+(0.3+t)*c60)
      
      p <- p + ggplot2::annotate(geom="text", 
                                 x=point[1]-(0.65+t)*s60, 
                                 y=point[2]+(0.65+t)*c60, 
                                 label=tick, size=3.5, angle=60)
    }
  }

  return(p)
}
