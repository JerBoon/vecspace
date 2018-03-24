


library("vecspace")

# Constructing a table from elementary shapes

# Start with a table top, and a leg - don't add bounding volumes at this stage, since
# a spherical bounding volume is not especially efficient for these objects

top <- Spc.MakeCuboid(c(0,10,0),c(10,0.5,10),bound=FALSE)
leg1 <- Spc.MakeCuboid(c(4,4.875,4),c(0.5,9.75,0.5), bound=FALSE)

# Let's see what that looks like...

tab <- Spc.Combine(list(top,leg1))
Spc.Plot(tab)

# Add some more legs. It's not much of a table without!

leg2 <- Spc.MakeCuboid(c(4,4.875,-4),c(0.5,9.75,0.5), bound=FALSE)
leg3 <- Spc.MakeCuboid(c(-4,4.875,-4),c(0.5,9.75,0.5), bound=FALSE)
leg4 <- Spc.MakeCuboid(c(-4,4.875,4),c(0.5,9.75,0.5), bound=FALSE)

# Let's see what it looks like now. We've combined it WITH a bounding
# sphere. A sphere is a pretty good fit to our relatively cubic table

tab <- Spc.Combine(list(top,leg1,leg2,leg3,leg4))
Spc.Plot(tab)

# Note that we're displaying surface normals. And note that on a cuboid they're
# all facing OUTWARDS - this is relevant, since when we're ray tracing with these
# objects we can then use the intersection direction to determine if we're
# INSIDE or OUTSIDE the object. We could then, for example, make a glass table!!

# Display just the object itself, without the extra information

Spc.Plot(tab,print.normals=FALSE,print.bounds=TRUE)

# And check what it looks like from some different angles

Spc.Plot(Spc.Rotate(tab,pivot.angle=c(30,15,0)),print.normals=FALSE,print.bounds=FALSE)

# And of course now we have an elementary table, we can reuse it...

lots.of.tabs <- Spc.Combine(list(tab,Spc.Translate(tab,c(20,0,0)),Spc.Translate(tab,c(0,0,20)),Spc.Translate(tab,c(20,0,20))),bound=FALSE)
Spc.Plot(Spc.Rotate(lots.of.tabs,pivot.angle=c(-20,10,0)),print.normals=FALSE)

# We can REALLY reuse it!

lots.of.tabs <- Spc.Combine(list(lots.of.tabs,Spc.Translate(lots.of.tabs,c(40,0,0)),Spc.Translate(lots.of.tabs,c(0,0,40)),Spc.Translate(lots.of.tabs,c(40,0,40))),bound=FALSE)
Spc.Plot(Spc.Rotate(lots.of.tabs,pivot.angle=c(-20,10,0)),print.normals=FALSE)
