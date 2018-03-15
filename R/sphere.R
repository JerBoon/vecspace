

#---------------------------------------------------------------------
#' Return an elementary sphere object
#'
#' @param centre Centre of sphere
#' @param radius of sphere
#' @param properties Package-independent object defining additional sphere properties
#'     Default NA
#'
#' @return Elementary sphere object
#' 
#' @export
#'
#' @family constructors
#'
#' @examples
#'   s <- Spc.MakeSphere(c(0,0,0), 2, surface_props)

Spc.MakeSphere <- function (centre, radius, properties=NA) {

  if ((typeof(centre) != "double") ||
      length(centre) != 3) {
    print("Spc.MakeSphere: centre should be a 3 number vector")
    return(NA)
  }
  if ((typeof(radius) != "double") ||
      length(radius) != 1) {
    print("sphere.make: radius should be a number")
    return(NA)
  }

  r <- list(centre=centre,radius=radius)
  class(r) = append(class(r),"SpcSphere")

  if (!is.na(properties))
    attr(r,"properties") <- properties

  return(r)
} 

#------------------------------------------------------------------------------

.Spc.Translate.SpcSphere <- function(vector, sphere) {

  sphere$centre <- sphere$centre + vector

  if (length(sphere$objects) == 0)
    return(sphere)

  sphere$objects <- .Spc.Translate(vector,sphere$objects)
  return(sphere)
}

#------------------------------------------------------------------------------

.Spc.BoundRec.SpcSphere <- function(sphere) {

  return(list(sphere$centre+sphere$radius, sphere$centre-sphere$radius))
}


#==============================================================================

.Spc.Polylines.SpcSphere <- function(sphere) {

  circ <- seq(0,2*pi,length.out=25)

  #a list of polylines for all the different cicrles making up the line drawn circle
  #think "Panorama"...
  r <- list()

  #Add 5 cicles in horizontal space. equator, plus 30deg and 60deg tropics

  for (y in seq(-pi/3,pi/3,length.out=5)) {
    hc <- matrix(c(sin(circ) * sphere$radius * cos(y) + sphere$centre[1],
                   0*circ + sphere$centre[2] + sin(y) * sphere$radius,
                   cos(circ) * sphere$radius * cos(y) + sphere$centre[3]),
                 ncol=3)
    r <- append(r, list(hc))
  }

  #And now another bunch of longitude circles

  for (l in seq(0, 5*2*pi/12, length.out=6))
  {
    hc <- matrix(c(sin(l) * cos(circ) * sphere$radius + sphere$centre[1],
                   sphere$centre[2] + sin(circ) * sphere$radius,
                   cos(l) * cos(circ) * sphere$radius + sphere$centre[3]),
                 ncol=3)
    r <- append(r, list(hc))
  }

  #if sphere is a bounding sphere, also add its contents
  if (length(sphere$objects) > 0)
    r <- append(r, .Spc.Polylines(sphere$objects))

  return(r)
}





