

#---------------------------------------------------------------------
#' Construct a polygon from elementary triangles. Returns these as a combined list.
#'
#' @param x,y,z Three vectors containing the 3 euclidean coordinates of 4 or more points to be combined.
#' @param properties Package-independent object defining additional properties.
#'     Default NA
#'
#' @return List of triangles, constructed from elements 1,2 and 3; then 1,3 and 4, then 1,4 and 5; and so on.
#'     The function will check you've supplied at least 3 points. If you supply exactly three, it'll simply return a triangle object.
#'     The function is designed to mostly be used to construct polygons within a common plane, but will run on any sets of points regardless.
#' 
#' @export
#'
#' @family constructors
#'
#' @examples
#'   p <- Spc.MakePolygon(x=c(0,0,1,1), y=c(0,1,1,0), z=c(0,0,0,0), "square1")
#'   Spc.MakePolygon(x=c(0,0,1,1,2,2), y=c(0,2,2,1,1,0), z=c(0,0,0,0,0,0), "L-shaped flat object")

Spc.MakePolygon <- function (x,y,z, properties=NA) {

  if ((typeof(x) != "double") || (length(x) < 3) ||
      (typeof(y) != "double") || (length(y) < 3) ||
      (typeof(z) != "double") || (length(z) < 3) ||
      (length(x) != length(y)) || (length(x) != length(z))) {
    print("Spc.MakePolygon: x, y and z should be equal length vectors of at least 3 elements")
    return(NA)
  }

  r <- list()

  for (i in 2:(length(x) - 1)) {
    t <- Spc.MakeTriangle(c(x[1],y[1],z[1]),
                          c(x[i],y[i],z[i]),
                          c(x[i+1],y[i+1],z[i+1]))

    if (length(x) == 3) {
      if (!is.na(properties))
        attr(t,"properties") <- properties
      return(t)
    }
 
    r <- append(r,list(t))
  }

  class(r) = append(class(r),"SpcCompound")

  if (!is.na(properties))
    attr(r,"properties") <- properties

  return(r)
} 

