

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

Spc.AsPolylines <- function(obj) {
  .Spc.Polylines(obj)
}

.Spc.Polylines <- function(obj) {

  UseMethod(".Spc.Polylines", obj)
}


