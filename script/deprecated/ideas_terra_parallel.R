prich <- function(filein, fileout) {
  r <- rast(filein)
  richard <- function(x) {
    k <- 0.2
    v <- 0.3
    a <- 200
    y0 <- 2
    y <- k / v * x * (1 - ((x / a)^v)) + y0
    y[y < 0] <- 0
    return(y)
  }
  x <- app(masked, richard, filename = fileout, overwrite = TRUE)
  return(TRUE)
}

# A SpatRaster cannot be serialized, you cannot send it to parallel compute nodes.
#
# I think you cannot send SpatRaster or SpatVector objects to nodes as these
# objects cannot be serialized. So you need to to create the objects in the nodes.
# My plan it to make this all easy by providing built-in support for
# parallelization but that is not the highest priority right now;
# but I expect to add that this year.
