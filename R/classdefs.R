

#---------------------------------------------------------------------
#' Move the object hierarchy to a different point in space, without performing rotation or any other trsnformation
#'
#' @param move.direction A directional vector containing the distance (x,y,z) by which the object should be moved
#' @param object The object (elementary or compound) to be moved
#'
#' @return The same object, but with all appropriate spacial points updated accordingly
#'
#' @export
#'
#' @family transforms
#'
#' @examples
#'   my_object <- Spc.Translate (c(0,20,0), my_object)

Spc.Translate <- function(move.direction,object) {
  .Spc.Translate(move.direction,object)
}

# ------- And the corresponding class function definition
# Declared as separate function, since the elementary classes want to be hiiden, and roxygen
# if I understand correctly requires a separate definition (??)

.Spc.Translate <- function(move.direction,object) {
  UseMethod(".Spc.Translate", object)
}


#---------------------------------------------------------------------
#' Rotate an object by c(ax,ay,az) in the x y and z planes, about the point c(x,y,z) 
#' Uses the right-hand coordinate system (this is the only functin in this package which is handed in this way).
#' Rotates about first the x axis, then y, then z. This operation is not commutable (i.e. if you rotate by the various
#' angles about first z, then x then y, you'd get a different result).
#'
#' @param pivot.point Vector of the point in space to rotate about. Defaults to c(0,0,0)
#' @param pivot.angle Vector of angles to rotate about (x,y,z) - in degrees
#' @param object The object (elementary or compound) to be rotated
#'
#' @return The same object, but with all appropriate spacial points updated accordingly
#'
#' @export
#'
#' @family transforms
#'
#' @examples
#'   my_object <- Spc.Rotate(c(0,0,0),c(45,0,0),my_object)

Spc.Rotate <- function(pivot.point=c(0,0,0), pivot.angle, object) {

  #Degrees to radians constant
  pic <- (2 * pi) / 360

  #From https://en.wikipedia.org/wiki/Rotation_matrix
  rotx <- matrix(c(1,0,0,0,cos(pivot.angle[1]*pic),sin(pivot.angle[1]*pic),0,-sin(pivot.angle[1]*pic),cos(pivot.angle[1]*pic)),ncol=3)
  roty <- matrix(c(cos(pivot.angle[2]*pic),0,-sin(pivot.angle[2]*pic),0,1,0,sin(pivot.angle[2]*pic),0,cos(pivot.angle[2]*pic)),ncol=3)
  rotz <- matrix(c(cos(pivot.angle[3]*pic),sin(pivot.angle[3]*pic),0,-sin(pivot.angle[3]*pic),cos(pivot.angle[3]*pic),0,0,0,1),ncol=3)

  #Combine as rotx, then roty, then rotz
  # This represents a transformation matrix which can be applied to any vector to perform the whole deal
  # To transform point in space, need to first subtract pivit.point, then apply [rot], then add pivot.point back
  # Since the various elementary objects are defined by a mix of points in space, and direction vectors, we'll just pass this 
  # rotMatrix down through the various transform methods.. 
  rot <- rotz %*% roty %*% rotx

  .Spc.Rotate(pivot.point, rot, object)
}

# ------- And the corresponding class function definition
# Declared as separate function, since the elementary classes want to be hiiden, and roxygen
# if I understand correctly requires a separate definition (??)

.Spc.Rotate <- function(pivot.point, pivot.rotMatrix, object) {
  UseMethod(".Spc.Rotate", object)
}


#---------------------------------------------------------------------
#' Find the distance at which a ray first intersects with an object, plus any relevant object
#' properties at the point of intersection
#'
#' @param ray.origin A positional vector of where the ray originates
#' @param ray.direction A directional vector for the ray's direction
#' @param object The object (elementary or compound) we're testing for intersection
#'
#' @return A list of intersect properties, consisting of:
#'    distance = intersect distance (as proportin of the length of ray.direction);
#'    properties = the *properties* attribute from the intersected object (or NA if none)
#'
#' @export
#'
#' @examples
#'   Spc.Intersect (c(0,0,0), c(0,1,0),  my_world)

Spc.Intersect <- function(ray.origin,ray.direction,object) {
  .Spc.Intersect(ray.origin,ray.direction,object) 
}

.Spc.Intersect <- function(ray.origin,ray.direction,object) {

  UseMethod(".Spc.Intersect", object)
}

# -----------------------------------------------------------------------

#Boundrec returns the "upper" (max(x,y,z)) and "lower" corners of an
#xyz aligned cuboid which minimully surrounds the given object
# returns:
#  list(c(x,y,z), c(-z,-y-z))
#  *OR* NA if the object cannot be bound - e.g. for a plane

.Spc.BoundRec <- function(obj) {
  UseMethod(".Spc.BoundRec", obj)
}


# -----------------------------------------------------------------------

#polylines returns a list of matrices, where each matrix has x y and z columns
#defining a point in space, and a row for each point to connect, so as to complete a line.
#Since the polylne may or may not want to be a closed shape, the final row should be
#a repeat of the first row in orderto facility closure.
#It's a list, since complex objects may be defined by a number of different
#polylines - e.g. think of a sphere, which will be drawn as various latitudes
#together with various  lines of longitude.

#---------------------------------------------------------------------
#' Outputs all object coordinates as a list of 3 column matrices corresponding
#' to (x,y,z) coordinates of relevant points. Treats spheres and planes as special cases,
#' since these obviously can't be accurately reproduced in wire-frame form
#'
#' @param object The object (elementary or compound) to be plotted
#'
#' @return List of matrices
#'
#' @export
#'
#' @family outputs
#'
#' @examples
#'   Spc.AsPolylines (my_world)

Spc.AsPolylines <- function(obj) {
  .Spc.Polylines(obj)
}

.Spc.Polylines <- function(obj) {

  UseMethod(".Spc.Polylines", obj)
}


