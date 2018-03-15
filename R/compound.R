
#compound onjects are basically just a list ofother objects with no particular
#hierarchy. The compound is not an object itself, so each of these functions
#need to iterate through the members in the list, etc ,etc..

#------------------------------------------------------------------------

.Spc.Translate.SpcCompound <- function(move.direction, objects) {

  for (i in 1:length(objects)) {
    objects[[i]] <- .Spc.Translate(move.direction, objects[[i]])
  }
  return (objects)
}

#------------------------------------------------------------------------------

.Spc.BoundRec.SpcCompound <- function(objects) {

  for (i in 1:length(objects)) {

    #print(i)
    r <- .Spc.BoundRec(objects[[i]])

    if (is.na(r)[1])
      return (NA)

    if (i == 1) {
      r1 <- r[[1]]
      r2 <- r[[2]]
    } else {
      #print("else")
      r1 <- pmax(r[[1]],r1)
      r2 <- pmin(r[[2]],r2)
      #print("endelse")
    }
    #print("r")
    #print(r1)
    #print(r2)
  }

  return(list(r1,r2))
}


.Spc.Polylines.SpcCompound <- function(objects) {

  r <- list()

  for (i in 1:length(objects)) {
    r <- append(r, .Spc.Polylines(objects[[i]]))
  }

  return(r)
}
