
# Get min and max values for each dataset
.gMinMax <- function(df, shape) {
  adj_shape <- shape+1
  rownum <- nrow(df)
  mindata <- c()
  maxdata <- c()
  for (i in seq.int(2,adj_shape)) {
    minval <- min(df[3:rownum,i],na.rm=TRUE)
    maxval <- max(df[3:rownum,i],na.rm=TRUE)
    # If all values are missing in the column, set min/max as NA
    minval <- if(is.infinite(minval)) NA else minval
    maxval <- if(is.infinite(maxval)) NA else maxval
    # Combine the data as a
    mindata <- c(mindata, minval)
    maxdata <- c(maxdata, maxval)
  }
  return (list(mindata, maxdata))
}
