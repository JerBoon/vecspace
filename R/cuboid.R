
#--------------------------------------------------------------------------------

#should be reasonably obvious. Makes a cuboid aligned to the x,y,z plane, centred
#on /centre/
#dimensions defined the cuboid dimesnions in the x, y and z plane in that order
#names faces face1.. face6, sequenced in the following order
# +x, -x, +y, -y, +z, -z

#---------------------------------------------------------------------
#' Return a cuboid object, constructed from elementary spacial objects
#'
#' @param centre Centre of cuboid
#' @param dimensions Basic dimensions of the cuboid, as a vector of length 3
#'    in (x,y,z) dimensions.
#' @param properties Package-independent object defining additional cuboid properties
#'
#' @return A compound cuboid object, comprising of elementary triangle objects
#'   in a grouped hierarchical object, with a surrounding bounding sphere
#'   used for spacial indexing.
#' 
#' @export
#'
#' @family constructors
#'
#' @examples
#'   w <- Spc.MakeCuboid(c(0,0,0), c(2,2,2), surface_props)

Spc.MakeCuboid <- function (centre, dimensions, properties=NA, bound=TRUE) {

  if ((typeof(centre) != "double") || (length(centre) != 3)) {
    print("Spc.MakeCuboid: centre should be a 3 number vector")
    return(NA)
  }

  if ((typeof(dimensions) != "double") || (length(dimensions) != 3) || (sum(dimensions > 0) != 3)) {
    print("Spc.MakeCuboid: dimensions should be a 3 positive number vector")
    return(NA)
  }

  d <- dimensions / 2

  f1 <- Spc.MakePolygon(c(d[1],d[1],d[1],d[1]),
                        c(d[2],-d[2],-d[2],d[2]),
                        c(d[3],d[3],-d[3],-d[3]))

  f2 <- Spc.MakePolygon(c(-d[1],-d[1],-d[1],-d[1]),
                        c(d[2],d[2],-d[2],-d[2]),
                        c(d[3],-d[3],-d[3],d[3]))

  f3 <- Spc.MakePolygon(c(d[1],d[1],-d[1],-d[1]),
                        c(d[2],d[2],d[2],d[2]),
                        c(d[3],-d[3],-d[3],d[3]))

  f4 <- Spc.MakePolygon(c(d[1],-d[1],-d[1],d[1]),
                        c(-d[2],-d[2],-d[2],-d[2]),
                        c(d[3],d[3],-d[3],-d[3]))

  f5 <- Spc.MakePolygon(c(d[1],-d[1],-d[1],d[1]),
                        c(d[2],d[2],-d[2],-d[2]),
                        c(d[3],d[3],d[3],d[3]))

  f6 <- Spc.MakePolygon(c(d[1],d[1],-d[1],-d[1]),
                        c(d[2],-d[2],-d[2],d[2]),
                        c(-d[3],-d[3],-d[3],-d[3]))

  ff <- list(f1,f2,f3,f4,f5,f6)

  r <- Spc.Combine(ff, properties=properties, bound=bound)

  r <- .Spc.Translate(r, centre)

  return(r)
}

