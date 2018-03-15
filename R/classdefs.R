

# -----------------------------------------------------------------------

#translate - move given object by an (x,y,z) vector

Spc.Translate <- function(move.direction,obj) {
  .Spc.Translate(move.direction,obj)
}

.Spc.Translate <- function(move.direction,obj) {

  UseMethod(".Spc.Translate", obj)
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


