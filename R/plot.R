
#---------------------------------------------------------------------
#' Plot an orthographic plan view along x, y and z axes
#'
#' @param object The object (elementary or compound) to be plotted
#'
#' @export
#'
#' @family outputs
#'
#' @examples
#'   Spc.Plot (my_world)


Spc.Plot <- function (object) {

  #Set up a 2x2 plot region
  par(mfcol=c(2,2))
  par(mai=c(0.5,0.5,0.1,0.1))
  par(mai=c(1,1,1,1))
  par(mar=c(1,1,1,1))
  par(oma=c(0,0,0,0))
  par(tck=0.01)
  par(cex.axis=0.7)
  par(mgp=c(0,-1,0))

  #calculate axes - we want to plot as a cube, so work out the bounding area of the object
  #then expand the shorter edges to make it a cube, kind of thing

  bndRec <- .Spc.BoundRec(object)

  centre <- bndRec[[2]] + (bndRec[[1]] - bndRec[[2]]) /2 
  length <- max(bndRec[[1]] - bndRec[[2]])

  xl <- c(centre[1] - length/2, centre[1] + length/2)
  yl <- c(centre[2] - length/2, centre[2] + length/2)
  zl <- c(centre[3] - length/2, centre[3] + length/2)

  py <- Spc.AsPolylines(object)

  #Plot x-y
  plot(NA, xlim=xl, ylim=yl, xlab="X", ylab="Y")
  for (i in 1:length(py)) points(py[[i]][,c(1,2)], type="l")
  #Plot x-z
  plot(NA, xlim=xl, ylim=zl, xlab="X", ylab="Z")
  for (i in 1:length(py)) points(py[[i]][,c(1,3)], type="l")
  #Plot z-y
  plot(NA, xlim=zl, ylim=yl, xlab="Z", ylab="Y")
  for (i in 1:length(py)) points(py[[i]][,c(3,2)], type="l")
}  


