
.ticks_interval <- function(minv, maxv, steps=8) {
  a <- 0
  range <- maxv - minv
  tempstep <- range / steps
  exponent <- as.integer(log10(tempstep))
  while(exponent < 1) {
    a <- a + 1
    range <- range * 10
    tempstep <- range / steps
    exponent <- as.integer(log10(tempstep))
  }
  magnitude <- 10 ** exponent

  magMsd <- as.integer(tempstep/magnitude + 0.5)

  if(magMsd > 5.0) {
    magMsd <- 10.0
  } else if (magMsd > 2.0) {
    magMsd <- 5.0
  } else {
    magMsd <- 2.0
  }
  return((magMsd * magnitude / (10**a)))
}

# Get tick numbers for each axis
.ticks_number <- function(i, min_range, max_range) {
  interval <- .ticks_interval(min_range[i], max_range[i])
  eachtick <- min_range[i]
  allticks <- c()

  while(eachtick < max_range[i]) {
    allticks <- c(allticks, eachtick)
    eachtick <- eachtick + interval
  }
  allticks <- c(allticks, max_range[i])
  return(allticks)
}
