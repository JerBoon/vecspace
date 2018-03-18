
#---------------------------------------------------------------------
#' Plot an orthographic plan view along x, y and z axes
#'
#' @param object The object (elementary or compound) to be plotted
#' @param view.axis Set to "X","Y" or "Z" to only plot the view along that axis.
#'    Otherwise will plot all 3 views together.
#'  @param print.bounds Should it display bounding volumes? Default = TRUE
#'
#' @export
#'
#' @family outputs
#'
#' @examples
#'   Spc.Plot (my_world)


Spc.Plot <- function (object, view.axis=NA, print.bounds=TRUE) {

  if (!(is.na(view.axis) || view.axis %in% c("x","y","z","X","Y","Z")))
  {
    print("Spc.Plot: view.axis can be either x,y or z online.")
    return()
  }

  #Set up a 2x2 plot region?
  if (is.na(view.axis)) {
    par(mfcol=c(2,2))
  } else {
    par(mfcol=c(1,1))
  }

  #display formats
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

  # A wrapper for the call to the points() function
  # with selection on colour, types of object, etc
  Spc.points <- function (py, cols) {
    lty <- "solid"
    if (is.null(attr(py,"plot.type")) || attr(py,"plot.type") == "object") {
      col <- "black"
    } else if (attr(py,"plot.type") == "bound") {
      if (!print.bounds)
        return()
      col <- "blue"
      lty <- "dotted"
    } else {
      col <- "green"
    }
    points(py[,cols], type="l", col=col ,lty=lty)
  }

  #Plot x-y
  if (is.na(view.axis) || view.axis %in% c("z","Z")) {
    plot(NA, xlim=xl, ylim=yl, xlab="X", ylab="Y")
    for (i in 1:length(py)) 
      Spc.points(py[[i]],c(1,2))
  }
  #Plot x-z
  if (is.na(view.axis) || view.axis %in% c("y","Y")) {
    plot(NA, xlim=xl, ylim=zl, xlab="X", ylab="Z")
    for (i in 1:length(py)) 
      Spc.points(py[[i]],c(1,3))
  }
  #Plot z-y
  if (is.na(view.axis) || view.axis %in% c("x","X")) {
    plot(NA, xlim=zl, ylim=yl, xlab="Z", ylab="Y")
    for (i in 1:length(py)) 
      Spc.points(py[[i]],c(3,2))
  }
}  


