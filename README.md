# vecspace
A library of generic Euclidean functions in 3D space

![orthogonal thingybob](https://user-images.githubusercontent.com/23141865/37862927-18a88094-2f4d-11e8-862d-db58431ddb49.png)

This is a work-in-progess at the moment. It's essentially part of my R "hello world" program, so be gentle with me!

I'm learning to build data structures and packages in R. To do this, I'm chucking a slightly ridiculous project
at it - which is to revisit my final year Uni project from many, many years ago, and write a functional ray tracing
program.

For this project, it feels like there are two different bits of functionality:

[1] This part, which is the maths around the generic work in Euclidean space.
  Defining elementary and compound objects in space, calculating intersections and so on; and

[2] The actual ray tracing part, which'll cover the camera modelling, surface properties, lighting, and all
  the recursive shinanegins around that. That'll be a separate package.
  
### Components of vecspace

* Some generic "utility" vector functions. They probably exist elsewhere, but since I'm working from the bottom up it seems
  reasonable to start with these.
  
* Functions to create and manipulate elementary objects in 3D space. Currently there are 3: triangle, sphere, and plane. For this, I shall
  also make use of object oriented programming, which is kind of cool!

* Functions to build and manipulate collections of elementary objects in to more complex "compound" objects, a.k.a. "worlds".
  There are two basic drivers for this: (1) Modular code and reusability, naturally; and (2) spacial indexing - to build
  hierarchies of objects which include bounding objects, so that you don't have to check if every ray intersects with
  *every* object, *every* time. You get the point.
  
* Generic *what does this directional vector intersect with in the world?* functionality.

### The hierarchical world

Everything in my world is (currently) made of triangles, spheres, and planes.

I've also included a simple compound object (the cuboid) which will hopefully demonstrate how this stuff works.

A cuboid is a rectangular generalisation of a cube, which is made of six rectangular faces. In my world, a rectangular face can
be constructed by adding 2 abutting triangles. The collection of the six faces can be grouped into a list, and common properties
applied to either the cube as a whole, each individual face, or (if you really want to) each individual triangle.

Finally, a "bounding volume" can be constructed around the cuboid, which in my world in a sphere which fully encloses all of
the components of the cube. It's therefore a hierarchical tree of objects, which can be scanned by a lightray (i.e. to test
for intersection) by testing first against the bounding sphere, and then only if it intersects *that* do we need to test for
intersection with each elementary component.

Also, transformation methods (OOP, remember) can be build for each elementary and compound object, such that you can simply say
(e.g.) "rotate my cube" and it'll run through the hierarchy of objects and rotate them all in the same geometry by parsing
the object hierarchy.

Further general constructors will allow you to, say, build a table from 5 simple cuboid objects, and then
be able to simply say "rotate my table" in a single statement. (And or course, the table itself can have a bounding sphere container)

![table](https://user-images.githubusercontent.com/23141865/37863006-9b27cef2-2f4e-11e8-89f9-a4203fde7501.png)

### Ray tracing

So the main reason for building this package is to facilitate ray tracing. You'll have to have a
look at my **raytrace** package to get the code for that. That's still very much a work in progress, 
but it's for the most part proven the design of this package. The way this package hasn't needed to care
about how surface properties are defined has proved most useful.

Here's a quick demo image from the ray tracing package as it stands. I still need to add in shadowing, trsnparency and
a heap of stuff like that, but it's definitely coming along nicely now....

![screen shot 2018-04-02 at 21 52 13](https://user-images.githubusercontent.com/23141865/38215573-32285308-36c0-11e8-9515-24f77de3ac88.png)
