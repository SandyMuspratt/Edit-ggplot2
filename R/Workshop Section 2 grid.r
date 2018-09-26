
###############################################################
##
## Section 2: A quick introduction to the grid graphics system
##
###############################################################



###############################################################
##
## Lecture 3: Why Grid
##
###############################################################



###############################################################
##
## Lecture 4: Drawing graphical object and controlling their appearance
##
##   Drawing graphical objects
##   Change the appearance of graphical objects
##   The gp slot
##   npc units
##
###############################################################

library(grid)

# Create a grob (graphic object)
circle <- circleGrob()

# Draw the grob
grid.draw(circle)

# The circle fills the whole graphics window;
# The circle has a colour (the black boundary);
# but it appears to have no fill. 
# Colour and fill here have the same meaning as they have in ggplot2:
#     'colour' refers to the colour of lines, text and boundaries;
#     'fill' refers to the colour of areas inside boundaries - the interior colours.

# First, I'll colour the circle - that is, set a fill colour
circle <- circleGrob(gp = gpar(fill = "purple"))
grid.newpage()
grid.draw(circle)

# Two things to notice here:
# First, grid does not clear the graphics device of the previous drawing.
# The grid.newpage() function clears the previous drawing. 

# Second, notice the gp parameter and its setting.
# What does gp = gpar() mean?
# This is where the appearance of the grob can be controlled.
# Ask for help on the gpar function. Look to the DETAILS section of the help page. 
# You will see something like:
#   "All grid viewports and (predefined) graphical objects have a slot called gp,
#     which contains a "gpar" object."
# You will also see a list of parameter names - the things that can be changed.
# What are the parameters and what are their default settings?
str(get.gpar())

# Among other things, notice that the fill is "transparent", and that colour is "black".
# Now, go to the gp slot in the circle grob.

str(circleGrob())
# The circle grob contains a list of six objects.
# Second from the bottom is the gp slot.
# It contains an empty list, which means the grob is using the default settings.

# Change col (the boundary colour), fill (the interior colour) and lwd (width of boundary line)
circle <- circleGrob(gp = gpar(col = "red", fill = "orange", lwd = 20))
str(circle$gp)
# Now, the gp slot contains a list of three objects: col, fill and lwd.
# The other settings remain with their default values

# Draw the grob:
grid.newpage()
grid.draw(circle)


## A few other grobs:
# lines, rectangles, text, segments, polygons, and points

# A line
line <- linesGrob()
grid.newpage()
grid.draw(line)

# An orange rectangle
rect <- rectGrob(gp = gpar(fill = "orange"))
grid.newpage()
grid.draw(rect)

# A text grob
text <- textGrob(label = "Learning some grid", gp = gpar(cex = 4))
grid.newpage()
grid.draw(text)

# The segment grob needs to know the limits of the segment line:
# x and y give the location of the start point - (0, 1) - top left corner 
# x1 and y1 give the location of the end point - (0.5, 0.5) - the middle
segment <- segmentsGrob(x = 0, y = 1, x1 = 0.5, y1 = 0.5, gp = gpar(lwd = 5))
grid.newpage()
grid.draw(segment)

# A green polygon. The x and y vectors give the locations of the polygon's vertices
polygon <- polygonGrob(x = c(0, 0.5, 1, 0.5), 
                       y = c(0.5, 1, 0.5, 0),
                       gp = gpar(fill = "green"))
grid.newpage()
grid.draw(polygon)

# The points grob needs the locations of the points; the x and y vectors contain the locations. 
# A brief note about "units". 
# There are many different unit systems.
# But the default for all the grobs so far has been "npc" - stands for "Normalised Parent Coordinates".
# In npc units, the bottom left corner of the graphics device has the location (0, 0), 
# and the top right corner has the location (1, 1).
# Thus, locations are relative to the graphics window. 
# However, the default unit for the points grob is "native".
# I change the default to "npc" just to be consistent with the other grobs.
# I will come back to units later.
points <- pointsGrob(x = seq(0, 1, 0.1), y = seq(0, 1, 0.1), 
                     default.units = "npc")
grid.newpage()
grid.draw(points)
 
## One more points before moving on:

# The difference between grid.circle() and circleGrob()
grid.circle(gp = gpar(col = "red", fill = "orange", lwd = 10))
circle = circleGrob(gp = gpar(col = "red", fill = "orange", lwd = 10))

# There are two formats for expressing grobs:
#   grid.*()     for instance, grid.circle()
# and
#   *Grob()      for instance circleGrob()

# Both versions create the grob, 
# but the first version, in addition, draws the grob.
# When using the second version, the grob is assigned to a variable name, 
# then we draw the grob later using the grid.draw() function.
# For the purpose of setting up gtables, the second version is what we want most of the time. 


# The main points
#   Drawing grobs (lines, points, corcles, rectangles, polygons, text);
#   Changing their appearance by assigning values to parameters in the gp slot;
#   Getting the default values for graphical parameters;
#   Getting the values that have been assigned to graphical parameters;
#   Using npc unit
#   That there are two formats for constructing grobs.


###############################################################
##
## Lecture 5: Creating and controlling graphic regions
##
##   The vp slot
##   Creating graphics regions - viewports
##   Changing the appearance of the region
##   The concept of 'clipping'
##
###############################################################

# Draw a circle, with an orange fill, and red boundary line with a line width of 20
circle <- circleGrob(gp = gpar(col = "red", fill = "orange", lwd = 20))
grid.newpage()
grid.draw(circle)

# Notice that with the wide boundary line, 
# the boundary line does not fit into the graphics window.
# One way to fix this is to reduce the radius of the circle.

# What is the current radius?
# Well first, what are the named variables in the circle object?
# And what are the values of the first three?
names(circle)
circle[1:3]

# x and y give the location of the centre (in npc units), and
# r is the radius (in npc units)
# This can be confirmed by asking for help - ?circleGrob 

# So, the current centre is located at x = 0.5 and y = 0.5, and the radius is 0.5

# Change the radius to something smaller, say 0.45.
circle <- circleGrob(r = 0.45,
                gp = gpar(col = "red", fill = "orange", lwd = 20))
grid.newpage()
grid.draw(circle)


# Another way to fix it is to take advantage of another slot in the grob structure - the vp slot.

str(circleGrob())

# You'll find the vp slot immediately below the gp slot.
# The vp slot contains parameters describing a viewport.
# A viewport is a rectangular region within the graphics window.
# The parameters give the viewport's location, size, and units, among other things.

# The default location is the centre of the containing graphics window.  
# In npc units, the default location is given by x = 0.5 and y = 0.5. 
# The size of the viewport is given by width and height. 
# The default size in npc units is given by width = 1 and height = 1.
# For the viewport to contain the circle grob,
# I'll leave the centre of viewport at the default values,
# but I'll reduce the width and the height to 0.75.
# That is, I set a smaller viewport within which to place the circle grob.

circle <- circleGrob(gp = gpar(col = "red", fill = "orange", lwd = 20),
                       vp = viewport(width = 0.75, height = 0.75))
grid.newpage()
grid.draw(circle)


str(circle)

# I show the boundary of the viewport by drawing a rectangle 
# that has the same size and location as the viewport that contains the circle.
grid.rect(vp = viewport(width = 0.75, height = 0.75, gp = gpar(lty = "dashed", fill = NA)))

# Notice the boundary line for the circle is still too big for the viewport,
# but it doesn't matter because the boundary line can spill over the viewport boundary.
# In the language of grid, clipping is turned off.
# That is, within the vp slot notice that there is a clip parameter, and that it is set to FALSE.


# What happens when the clip parameter is set to TRUE or "on"?
circle <- circleGrob(gp = gpar(col = "red", fill = "orange", lwd = 20),
                       vp = viewport(clip = "on", width = 0.75, height = 0.75))
grid.newpage()
grid.draw(circle)
grid.rect(vp = viewport(width = 0.75, height = 0.75), gp = gpar(lty = "dashed", fill = NA))
# Then once again, the bits of the boundary line that spill over the viewport boundary are clipped. 


# The main points
#   Controlling graphics regions;
#   Setting values to parameters in the vp slot;
#   Turning clipping on and off





###############################################################
##
## Lecture 6: Hierarchies of graphic objects
##
##  Construct hierarchies of grobs.
##  so that the grobs have parent-child relationships.
##  Setting graphics and viewport parameters at the head of the structure.  
##
###############################################################

library(grid)

# Construct three grobs.
# But this time, I give them names.
# The names will be used shortly.
rect <- rectGrob(gp = gpar(fill = "orange"),
                  name = "gRect")

polygon <- polygonGrob(x = c(0, 0.5, 1, 0.5), y = c(0.5, 1, 0.5, 0), 
                        gp = gpar(fill = "burlywood3", lwd = 3),
                        name = "gPolygon")

text <- textGrob(label = "Learning some grid",
                  gp = gpar(cex = 3),
                  name = "gText")

# The three grobs can be drawn in a layered fashion:
# the rectangle first;
# then the polygon;
# then the text;

grid.newpage()
grid.draw(rect)
grid.draw(polygon)
grid.draw(text)

# But it is more convenient to combine several grobs into a single grob using:
#  lists, or 
#  trees.

# First, the gList() function constructs a list. 
# Again, order is important. 

myList <- gList(rect, polygon, text)
grid.newpage()
grid.draw(myList)

# That's about all there is to a list.
# A tree can be constructed is a similar way, using the grobTree() function
myTree <- grobTree(rect, polygon, text)
grid.newpage()
grid.draw(myTree)

# grobTree is just a convenient wrapper for gTree when the only components 
# of the gTree are grobs.
# What does the more complete gTree construction look like?

# Specify the parent-child structure of the grob
myTree <- gTree(name = "gTree", children = gList(rect, polygon, text))
grid.newpage()
grid.draw(myTree)

# There is a structure to the grob;
# There is the myTree grob, which has been given the name gTree;
# and there are its children in a gList: 
#     the rect grob, 
#     the polygon grob, 
#     and text grob.

# The children are grobs, and they in turn can have children.
# For instance, first combine the polygon and the text grobs into a single grob.

PolygonText <- gTree(name = "gPolygonText", children = gList(polygon, text))

# There is the PolygonText grob, and its two children:
#    the polygon grob;
#    and the text grob.

# Then combine the rect grob and the PolygonText grob into a single grob.
myTree2 <- gTree(name = "gTree", children = gList(rect, PolygonText))

# There is the myTree2 grob, and its two children:
#    the rect grob;
#    and the PolygonText grob.

#    The polygonText grob in turn has two children:
#       the polygon grob;
#       and the text grob.

# Draw the myTree2 grob.
grid.newpage()
grid.draw(myTree2)

# or draw one of its children, the PolygonText grob
grid.newpage()
grid.draw(PolygonText)



### The diagrams on slides 11 and 12 (Lecture 6) were drawn using grid.
# Se below for the code.



# How does setting graphic parameters (gp = gpar()) at the head of the tree affect the children?
# Here, colour and line width are set at the head of the tree.
# Colour was not set in any of the children,
# so colour should flow through to the children.
# However, line width was set for gPolygon, 
# but it was not set for gRect
myTree <- gTree(name = "gTree", gp = gpar(col = "white", lwd = 10),  children = gList(rect, polygon, text))
grid.newpage()
grid.draw(myTree)
# Note that colour flows through to the children as expected.
# But also note that line width flows only to gRect. 
# gPolygon already had a line width set, 
# and thus it is not affected by a line width set higher up the tree.

# In this example, fill is set at the head of the tree.
myTree <- gTree(name = "gTree", gp = gpar(col = "white", fill = "red", lwd = 10),  
                children = gList(rect, polygon, text))
grid.newpage()
grid.draw(myTree)
# But it will have no affect on the figure.
# Fill was already set for gRect and gPolygon,
# and text does not have a fill. 


# But multipliers behave differently.
# cex is a multiplier of fontsize.
# It was set in gText,
# and it is set at the head of the tree.
myTree <- gTree(name = "gTree", 
               gp = gpar(col = "white", fill = "red", lwd = 10, cex = 1.5), 
               children = gList(rect, polygon, text))
grid.newpage()
grid.draw(myTree)
# cex is cumulative.
# The text in the final figure has a cex of 4.5,
# that is, 3 X 1.5
# There is a multiplier for line width that behaves in the same way.
# If the cumulative affect is not wanted, then use the absolute parameters: fonsize and lwd

# How does the setting of a viewport flow though to the children?
# It is reasonable that a change in location or size of a parent viewport 
# will drag the children with it.

# Now, I construct a viewport for the parent gTree
# that has height and width set to 0.5, 
# and has its centre moved to x = 0.4, y = 0.6.
myTree <- gTree(name = "gTree", 
               vp = viewport(x = 0.4, y = 0.6, height = 0.5, width = 0.5), 
               children = gList(rect, polygon, text))
grid.newpage()
grid.draw(myTree)
# Note that the children have been dragged to the new location with the parent.
# Note also that gPolygon and gRect fit into the resized parent viewport.

# Text size is not affected by viewport size;
# and the text itself is not constrained to the parent's viewport - clipping is turned off.
# If clipping is turned on in the parent viewport,
# then the text is clipped.
myTree <- gTree(name = "gTree", 
               vp = viewport(x = 0.4, y = 0.6, clip = "on", height = .5, width = .5), 
               children = gList(rect, polygon, text))
grid.newpage()
grid.draw(myTree)



# The main points
# Grobs can be arranged in hierarchical, parent-child, relationships.
# using a gTree and listing the children in a gList.
# Graphics parameters set at the head of a tree flow through to the children, 
# with two exceptions: 
# Graphics parameters will not flow through to the children if that parameter 
# has been set in the child grob; and
# Multipliers are cumulative.  
# Viewport parameters set at the head affect the parent grob and its children. 




#### Diagrams - based on code written by Murrell to draw Fig 7.3 (p. 232) in R Graphics (2nd Ed.).
# Diagram 1
library(grid)

labels <- c("\"gTree\"\nmyTree", "\"gRect\"\nrect grob", 
            "\"gPolygon\"\npolygon grob", "\"gText\"\ntext grob")

# height and width of boxes
rectheight <- unit(2, "line")
rectwidth <- unit(2.5, "cm")

# boxes, mid-box lines, and arrows between boxes
roundrect <- roundrectGrob(height = rectheight, width = rectwidth, 
                  r = unit(2, "mm"), gp = gpar(lwd = 1.5, fill = NA))
lines <- linesGrob(unit(0.5, "npc") + unit.c(-0.5*rectwidth, 0.5*rectwidth),
           0.5, gp=gpar(col = "grey70"))
arrowsfrom <- moveToGrob(x = 0.5, y = unit(0.5, "npc") - 0.5*rectheight)
arrowsto <- lineToGrob(x = 0.5, y = unit(0.5, "npc") + 0.5*rectheight, gp = gpar(fill = "black"),
             arrow = arrow(angle=20, length = unit(3, "mm"), type = "closed"))

# function to draw boxes
drawbox <- function(i, row, col) {
   pushViewport(viewport(layout.pos.row = row, layout.pos.col = col))
   grid.draw(lines)
   grid.draw(roundrect)
   grid.text(labels[i], gp = gpar(cex = .75))
   popViewport()
}

# function to draw arrows
drawarrow <- function(fromrow, fromcol, torow, tocol) {
   pushViewport(viewport(layout.pos.row = fromrow, layout.pos.col = fromcol))
   grid.draw(arrowsfrom)
   popViewport()
   pushViewport(viewport(layout.pos.row = torow, layout.pos.col = tocol))
   grid.draw(arrowsto)
   popViewport()
}

pushViewport(viewport(layout = grid.layout(2, 3)))

drawbox(1, 1, 2)
drawbox(2, 2, 1)
drawbox(3, 2, 2)
drawbox(4, 2, 3)

drawarrow(1, 2, 2, 1)
drawarrow(1, 2, 2, 2)
drawarrow(1, 2, 2, 3)


# Diagram 2
grid.newpage()

labels <- c("\"gTree\"\nmyTree2", 
      "\"gRect\"\nrect grob", "\"gPolygonText\"\nPolygonText",
                   "\"gPolygon\"\npolygon grob", "\"gText\"\ntext grob")

pushViewport(viewport(layout = grid.layout(3, 4,
   widths = unit(c(.8, .8, 1.2, 1.2), rep("null", 4)))))

drawbox(1, 1, 2:3)
drawbox(2, 2, 1:2)
drawbox(3, 2, 3:4)
drawbox(4, 3, 3)
drawbox(5, 3, 4)

drawarrow(1, 2:3, 2, 1:2)
drawarrow(1, 2:3, 2, 3:4)
drawarrow(2, 3:4, 3, 3)
drawarrow(2, 3:4, 3, 4)



###############################################################
##
## Lecture 7: Editing graphic objects
##
##  Edit a simple grob
##  Edit a hierarchically constructed grob:
##    Edit the parent grob
##    Edit child grobs  
##
###############################################################

library(grid)

## The editGrob() function allows modifications to a grob.
## To edit, use the gp and vp slots in the same way they are used to create grobs.
## Begin with text grob.
## I want a copy of the grob to centred at (0.7, 0.3).
text <- grid.text(label = "Learning some grid", name = "gText",
   gp = gpar(cex = 2))
grid.newpage()
grid.draw(text)

text1 <- editGrob(text, vp = viewport(x = 0.7, y = 0.3))
grid.draw(text1)


# The vp slot in text is NULL
# The x and y parameters in the vp slot in text1 are set to 0.7 and 0.3
# The other parameters remain with their default values.

# The gp slot remains unchanged.

str(text$vp)
str(text1$vp)

text$gp
text1$gp




## More complex edit:
# Edit the grob so that: 
#     it is centred at (0.7, 0.3),
#     and angled at 45 degrees,
#     and it has an orange colour,
#     and a text size three times larger than default.
text <- textGrob(label = "Learning some grid", name = "gText", 
           gp = gpar(cex = 2))
grid.newpage()
grid.draw(text)

text2 = editGrob(text, name = "gText2",
   gp = gpar(col = "orange", cex = 3),
   vp = viewport(x = 0.7, y = 0.3, angle = 45))
grid.draw(text2)

# This time, the x, y, and angle parameters in the vp slot of text2 are changed.
# The cex parameter in the gp slot in text was 2,
# but is text2 it is changed to 3.
# Also the col parameter is set to orange.

str(text$vp)
str(text2$vp)

text$gp
text2$gp



## Editing a gTree - the whole tree
## First create a gTree object

rect = rectGrob(gp = gpar(fill = "orange"),
                  name = "gRect")

polygon = polygonGrob(x=c(0, 0.5, 1, 0.5), y=c(0.5, 1, 0.5, 0), 
                      gp = gpar(fill = "burlywood3", lwd = 3),
                      name = "gPolygon")

text = textGrob(label = "Learning some grid",
                gp = gpar(cex = 2),
                name = "gText")

PolygonText = gTree(name = "gPolygonText", children = gList(polygon, text))

myTree = gTree(name = "gTree", 
               vp = viewport(height = 0.5, width = 0.5), 
               children = gList(rect, PolygonText))
grid.newpage()
grid.draw(myTree)


## To edit the tree using gpar parameters. 
# Change colour to red, and line width to 20.

myTree2 = editGrob(myTree, gp = gpar(col = "red", lwd = 20))
grid.newpage()
grid.draw(myTree2)

# The edit sets gpar parameters at the head of the tree.
# Those parameters will flow through to the children 
# provided they have not been set in the children.

# Colour was not set in any of the children, 
# so colour will flow to all the children

# Line width was set for the ploygon,
# so line width will have no affect on the polygon.
# But it will change the width of the boundary line of the rectangle.



## To edit the tree using viewport parameters.
## Viewport parameters from original unedited version are not remembered.
myTree3 = editGrob(myTree, vp = viewport(height = 0.5, width = 0.5, angle = 20))
grid.newpage()
grid.draw(myTree3)

# The edit sets viewport parameters at the head of the tree.
# The whole grob is rotated.


## To edit one of the children.
# Name the grob to be edited (myTree), 
# give the name assigned to the child ("gPolygonText").  
myTree4 = editGrob(myTree, "gPolygonText", 
   gp = gpar(col = "red"),
   vp = viewport(angle = 20))
grid.newpage()
grid.draw(myTree4)

# The edit is set at the level of the PolygonText.
# The edit does not the affact the whole grob,
# nor does it affact the rectangle.
# It affects the PloygonText (the polygon and the text) only.
myTree4$gp #NULL
myTree4$children$gRedt$gp # NULL
myTree4$children$gPolygonText$gp

# Note that there were no vp parameters set in the PolygonText,
# thus the only parameter that needs to be set in the angle.
str(myTree4$vp)
str(myTree4$children$gPolygonText$vp)



## To edit one of the children.
# Again, name the grob to be edited (myTree), 
# give the name assigned to the child ("gText").
myTree5 = editGrob(myTree, "gText", 
   gp = gpar(col = "red"), 
   vp = viewport(angle = 20))
grid.newpage()
grid.draw(myTree5)







