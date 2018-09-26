
library(grid)
library(Cairo)
#CairoWin(width = 5, height = 5, unit = "in")
#grid.newpage()


labels <- c("\"gTree\"\nmyTree", "\"gRect\"\nrect grob", 
            "\"gPolygon\"\npolygon grob", "\"gText\"\ntext grob")

# height and width of boxes
rectheight <- unit(4.5, "line")
rectwidth <- unit(5.5, "cm")

# boxes, mid-box lines, and arrows between boxes
roundrect = roundrectGrob(height = rectheight, width = rectwidth, 
                  r = unit(2, "mm"), gp = gpar(lwd = 1.5, fill = NA))
lines = linesGrob(unit(0.5, "npc") + unit.c(-0.5*rectwidth, 0.5*rectwidth),
           0.5, gp=gpar(col = "grey70"))
arrowsfrom = moveToGrob(x = 0.5, y = unit(0.5, "npc") - 0.5*rectheight)
arrowsto = lineToGrob(x = 0.5, y = unit(0.5, "npc") + 0.5*rectheight, gp = gpar(fill = "black"),
             arrow = arrow(angle=20, length = unit(3, "mm"), type = "closed"))

# function to draw boxes
drawbox = function(i, row, col) {
   pushViewport(viewport(layout.pos.row = row, layout.pos.col = col))
   grid.draw(lines)
   grid.draw(roundrect)
   grid.text(labels[i], gp = gpar(cex = 1.7))
   popViewport()
}

# function to draw arrows
drawarrow = function(fromrow, fromcol, torow, tocol) {
   pushViewport(viewport(layout.pos.row = fromrow, layout.pos.col = fromcol))
   grid.draw(arrowsfrom)
   popViewport()
   pushViewport(viewport(layout.pos.row = torow, layout.pos.col = tocol))
   grid.draw(arrowsto)
   popViewport()
}

# CairoPDF(file = "Diagram1.pdf", width = 5, height = 5)
 CairoPNG(filename = "Diagram1.png", height = 600, width = 600, unit = "px")

pushViewport(viewport(layout = grid.layout(2, 3)))

drawbox(1, 1, 2)
drawbox(2, 2, 1)
drawbox(3, 2, 2)
drawbox(4, 2, 3)

drawarrow(1, 2, 2, 1)
drawarrow(1, 2, 2, 2)
drawarrow(1, 2, 2, 3)

dev.off()


library(grid)
library(Cairo)
# CairoWin(width = 6, height = 5, unit = "in")

grid.newpage()


CairoPNG(filename = "Diagram2.png", height = 700, width = 600)

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

dev.off()






## 1
library(grid)
library(Cairo)

grid.roundrect(width = .25)
grid.text("faa")


## 2
pushViewport(viewport(width = .25))
grid.roundrect()
grid.text("faa", x = unit(2, "mm"), y = unit(1.5, "lines"), just = "left")
grid.text("...", x = unit(2, "mm"), y = unit(1, "lines"), just = "left")
popViewport()


## 3
labels = c("faa", "...")
vp = viewport(
   width = max(stringWidth(labels)) + unit(2, "cm"),
   height = unit(length(labels), "lines") + unit(1, "lines"))
pushViewport(vp)
grid.roundrect()
grid.text(labels,
          y = unit(2:1, "lines"))
popViewport()


## 4 With clipping so that white and grey regions appear
labels = c("faa", "...")
vp = viewport(
   width = max(stringWidth(labels)) + unit(2, "cm"),
   height = unit(length(labels), "lines") + unit(1, "lines"))
pushViewport(vp)
grid.roundrect()
grid.clip(y = unit(.5, "npc"), height = unit(.5, "npc"), just = "bottom")
grid.roundrect(gp = gpar(fill = "grey80"))
grid.clip()
grid.text(labels,
          y = unit(c(0.75,0.35), "npc"))
popViewport()


## 5  Function to draw boxes
tableBox = function(labels, x = .5, y = .5) {
   vp = viewport(x = x, y = y, 
      width = max(stringWidth(labels)) + unit(2, "cm"),
      height = unit(length(labels), "lines") + unit(1, "lines"))
	pushViewport(vp)
	grid.roundrect()
	grid.clip(y = unit(.5, "npc"), height = unit(.5, "npc"), just = "bottom")
	grid.roundrect(gp = gpar(fill = "grey80"))
	grid.clip()

   ifelse(labels[length(labels)] == "...", 
      grid.text(labels, y = unit(c(0.75,0.35), "npc")), 
      grid.text(labels, y = unit(c(0.75,0.25), "npc")))
popViewport()
}

tableBox(c("faa", "..."), x = .25, y = .7)
tableBox(c("tailnum", "..."), x = .25, y = .3)
tableBox(c("carrier", "names"), x = .75, y = .3)


## 6 Function to draw boxes with title (and line separating labels
tableBox = function(BoxTitle, labels, x = .5, y = .5) {
   vp = viewport(x = x, y = y, 
      width = max(stringWidth(labels), stringWidth(BoxTitle) + unit(2, "cm")),
      height = unit(length(labels), "lines") + unit(1, "lines"))
   pushViewport(vp)
   grid.roundrect(y = unit(0, "npc"), 
      height = unit(2/3, "npc"),
      just = "bottom")

	grid.clip(y = unit(1/3, "npc"), height = unit(1/3, "npc"), just = "bottom")
    grid.roundrect(y = unit(0, "npc"), 
      height = unit(2/3, "npc"),
      just = "bottom", 
      gp = gpar(fill = "grey90"))

	grid.clip()
    grid.lines(x = unit(c(0, 1), "npc"), y = unit(1/3, "npc"))
    grid.text(BoxTitle, y = unit(5/6, "npc"))

   ifelse(labels[length(labels)] == "...", 
      grid.text(labels, y = unit(c(3/6, 1/6 + .5*1/6), "npc")), 
      grid.text(labels, y = unit(c(3/6, 1/6), "npc")))
   popViewport()
}

tableBox("airport", c("faa", "..."), x = .25, y = .7)
tableBox("planes", c("tailnum", "..."), x = .25, y = .3)
tableBox("airlines", c("carrier", "names"), x = .75, y = .3)



## 7 Function to draw boxes with title (and line separating labels)
# with any number of grey and white labels
tableBox = function(BoxTitle, NumGrey, labels, x = .5, y = .5) {
 
   N = length(labels)
   Ngrey = NumGrey
   Nwhite = N - Ngrey

   vp = viewport(x = x, y = y, 
      width = max(stringWidth(labels), stringWidth(BoxTitle)) + unit(1.5, "cm"),
      height = unit(1.5*length(labels), "lines") + unit(1.5*1, "lines"))
   pushViewport(vp)

   grid.roundrect(y = unit(0, "npc"), 
      height = unit(N/(N+1), "npc"),
      r=unit(1, "mm"),
      just = "bottom", 
      gp = gpar(lwd = 1))

	grid.clip(y = unit(Nwhite/(N+1), "npc"), height = unit(Ngrey/(N+1), "npc"), just = "bottom")
    grid.roundrect(y = unit(0, "npc"), 
      height = unit(N/(N+1), "npc"),
      r=unit(1, "mm"),
      just = "bottom", 
      gp = gpar(fill = "grey90", lwd = 1))
	grid.clip()

    for(i in 1:(N-1))    
        grid.lines(x = unit(c(0, 1), "npc"), y = unit(i/(N+1), "npc"), gp=gpar(lwd = 1))

    grid.text(BoxTitle, y = unit(N/(N+1) + .4*1/(N+1), "npc"))

    for(i in 1:(N-1)) grid.text(labels[i], y = unit((N-i)/(N+1) + .5*1/(N+1), "npc"))

    ifelse(labels[length(labels)] == "...", 
      grid.text(labels[N], y = unit(0/(N+1) + .65*1/(N+1), "npc")), 
      grid.text(labels[N], y = unit(0/(N+1) + .5*1/(N+1), "npc")))      

   popViewport()
}

# CairoWin()

Cairo(width = 7, height = 7, file = "p.png", type = "png", 
      units = "in", dpi = 650)

tableBox("weather", 5, c("year", "month", "day", "hour", "origin", "..."), 
          x = .9, y = .7)

tableBox("flights", 5, 
   c("year", "month", "day", "hour", "flight", 
     "origin", "dest", "tailnum", "carrier", "..."))

tableBox("airport", 1,c("faa", "..."), x = .15, y = .7)
tableBox("planes", 1, c("tailnum", "..."), x = .15, y = .3)
tableBox("airlines", 1, c("carrier", "names"), x = .85, y = .3)

dev.off()




## 8 curved lines: With arrow and half circle on the ends,
#   adjoining a rectangle
x3 <- c(0.7, 0.8, 0.8, 0.9, 0.9)
y3 <- c(0.2, 0.2, 0.3, 0.3, 0.2)
grid.xspline(x3, y3, shape=.3,
      arrow = arrow(type = "closed", angle = 15, length=unit(3, "mm")),
     gp=gpar(fill="black", lwd = 1.5) )


grid.clip(x = unit(x3[1]-.001, "npc"), y = unit(y3[1], "npc"), just = "left")

grid.points(x3[1], y3[1], default.units = "npc", pch = 21, size = unit(2.5,"mm"), 
      gp=gpar(fill = "white", lwd = 1.5, pch = 21))

grid.clip()


grid.rect(x = unit(x3[1], "npc"), y = unit(y3[1], "npc"), just = "right",
          height = unit(.25, "npc"), width = unit(.25, "npc"))




## 9 Defining graphical objects

# Define draw.details method for new class - "box" class

# boxGrob - creates a graphic object of class "grob".
# Calls grob() function and supplies information about
# BoxTitle, NumGrey, labels, and x and y coordinates 
 
 boxGrob <- function(BoxTitle, NumGrey, labels, x=.5, y=.5) {
grob(BoxTitle = BoxTitle, NumGrey = NumGrey, labels=labels, x=x, y=y, cl="box")
}

## Method function for draw.details() function
## To do so, it calls tableBox function for point 7 above.
drawDetails.box <- function(x, ...) {
tableBox(x$BoxTitle, x$NumGrey, x$labels, x$x, x$y)
}

# Get the boxes - call boxGrob() function
weatherLabels = c("year", "month", "day", "hour", "origin", "...")
weather = boxGrob("weather", 5, weatherLabels, 
          x = .9, y = .7)

flightsLabels = c("year", "month", "day", "hour", "flight", 
     "origin", "dest", "tailnum", "carrier", "...")
flights = boxGrob("flights", 5, flightsLabels, y = .6)

airportLabels = c("faa", "...")
airport = boxGrob("airport", 1, airportLabels, x = .15, y = .7)

planesLabels = c("tailnum", "...")
planes = boxGrob("planes", 1, planesLabels, x = .15, y = .3)

airlinesLabels = c("carrier", "names")
airlines = boxGrob("airlines", 1, airlinesLabels, x = .85, y = .3)


# CairoWin()

Cairo(width = 7, height = 7, file = "p.png", type = "png", 
      units = "in", dpi = 650)

# Draw the boxes - equivalent of tableBox but now I have a grid graphical object
grid.draw(weather)
grid.draw(flights)
grid.draw(airport)
grid.draw(planes)
grid.draw(airlines)

dev.off()



## 10  Getting useful information from the grid graphical objects
#  We get the boundaries of the boxes, 
#  so that we know where to position the curved lines.

xDetails.box <- function(x, theta) {
   nlines <- length(x$labels)
   height <- unit(1.5*length(x$labels), "lines") + unit(1.5*1, "lines")
   width <- max(stringWidth(x$labels), stringWidth(x$BoxTitle)) + unit(1.5, "cm")
   grobX(roundrectGrob(x=x$x, y=x$y, width=width, height=height), theta)
}

yDetails.box <- function(x, theta) {
   nlines <- length(x$labels)
   height <- unit(1.5*length(x$labels), "lines") + unit(1.5*1, "lines")
   width <- max(stringWidth(x$labels), stringWidth(x$BoxTitle)) + unit(1.5, "cm")
   grobY(rectGrob(x=x$x, y=x$y, width=width, height=height), theta)
}





## 11 Draw one curve
x3 <- unit.c(grobX(box3, "east"),   
             .5*(grobX(box3, "east") + grobX(box2, "west")),
             .5*(grobX(box3, "east") + grobX(box2, "west")),
             grobX(box2, "west"))

y3 <- unit.c(grobY(box3, "south") + unit(2.25, "lines"), 
             grobY(box3, "south") + unit(2.25, "lines"),
             grobY(box2, "south") + unit(4*1.5+.75, "lines"),
             grobY(box2, "south") + unit(4*1.5+.75, "lines"))

grid.xspline(x3, y3, shape=.3,
      arrow = arrow(type = "closed", angle = 15, length=unit(3, "mm")),
     gp=gpar(fill="black", lwd = 1.5) )

grid.points(grobX(box3, "east"), grobY(box3, "south") + unit(2.25, "lines"),
      default.units = "npc", pch = 21, size = unit(2,"mm"), 
      gp=gpar(fill = "white", lwd = 1.5, pch = 21))

grid.newpage()


## 12 Function to get curve position 

LabelPos = function(Box) {
labels = get(Box[2])
N = length(labels)
pos = which(labels == Box[1])
pos = N-pos+1    # Count from the bottom
Lines = (pos-1)*1.5 + .75   # Each label height is 1.5 lines 
Lines
}

faa = LabelPos(c("faa", "airportLabels"))
forigin = LabelPos( c("origin", "flightsLabels"))
dest = LabelPos(c("dest", "flightsLabels"))

ptailnum = LabelPos(c("tailnum", "planesLabels"))
ftailnum = LabelPos(c("tailnum", "flightsLabels"))

acarrier = LabelPos(c("carrier", "airlinesLabels"))

fyear = LabelPos(c("year", "flightsLabels"))
fmonth = LabelPos(c("month", "flightsLabels"))
fday = LabelPos(c("day", "flightsLabels"))
fhour = LabelPos(c("hour", "flightsLabels"))
forigin = LabelPos(c("origin", "flightsLabels"))
fcarrier = LabelPos(c("carrier", "flightsLabels"))

wyear = LabelPos(c("year", "weatherLabels"))
wmonth = LabelPos(c("month", "weatherLabels"))
wday = LabelPos(c("day", "weatherLabels"))
whour = LabelPos(c("hour", "weatherLabels"))
worigin = LabelPos(c("origin", "weatherLabels"))



# faa > origin
x3 <- unit.c(grobX(airport, "east"),   
             .5*(grobX(airport, "east") + grobX(flights, "west")),
             .5*(grobX(airport, "east") + grobX(flights, "west")),
             grobX(flights, "west"))

y3 <- unit.c(grobY(airport, "south") + unit(faa, "lines"), 
             grobY(airport, "south") + unit(faa, "lines"),
             grobY(flights, "south") + unit(forigin, "lines"),
             grobY(flights, "south") + unit(forigin, "lines"))

grid.xspline(x3, y3, shape=.15,
      arrow = arrow(type = "closed", angle = 15, length=unit(2, "mm")),
     gp=gpar(fill="black", lwd = 1) )

grid.points(grobX(airport, "east"), grobY(airport, "south") + unit(faa, "lines"),
      default.units = "npc", pch = 21, size = unit(2,"mm"), 
      gp=gpar(fill = "white", lwd = 1.5, pch = 21))



# faa > dest
x3 <- unit.c(grobX(airport, "east"),   
             .5*(grobX(airport, "east") + grobX(flights, "west")),
             .5*(grobX(airport, "east") + grobX(flights, "west")),
             grobX(flights, "west"))

y3 <- unit.c(grobY(airport, "south") + unit(faa, "lines"), 
             grobY(airport, "south") + unit(faa, "lines"),
             grobY(flights, "south") + unit(forigin, "lines"),
             grobY(flights, "south") + unit(forigin, "lines"))

grid.xspline(x3, y3, shape=.15,
      arrow = arrow(type = "closed", angle = 15, length=unit(2, "mm")),
     gp=gpar(fill="black", lwd = 1) )

grid.points(grobX(airport, "east"), grobY(airport, "south") + unit(faa, "lines"),
      default.units = "npc", pch = 21, size = unit(2,"mm"), 
      gp=gpar(fill = "white", lwd = 1.5, pch = 21))



# tailnum > tailnum
x3 <- unit.c(grobX(planes, "east"),   
             .5*(grobX(planes, "east") + grobX(flights, "west")),
             .5*(grobX(planes, "east") + grobX(flights, "west")),
             grobX(flights, "west"))

y3 <- unit.c(grobY(planes, "south") + unit(ptailnum, "lines"), 
             grobY(planes, "south") + unit(ptailnum, "lines"),
             grobY(flights, "south") + unit(ftailnum, "lines"),
             grobY(flights, "south") + unit(ftailnum, "lines"))

grid.xspline(x3, y3, shape=.15,
      arrow = arrow(type = "closed", angle = 15, length=unit(2, "mm")),
     gp=gpar(fill="black", lwd = 1) )

grid.points(grobX(planes, "east"), grobY(planes, "south") + unit(ptailnum, "lines"),
      default.units = "npc", pch = 21, size = unit(2,"mm"), 
      gp=gpar(fill = "white", lwd = 1.5, pch = 21))


# carrier > carrier
x3 <- unit.c(grobX(airlines, "west"),   
             .5*(grobX(airlines, "west") + grobX(flights, "east")),
             .5*(grobX(airlines, "west") + grobX(flights, "east")),
             grobX(flights, "east"))

y3 <- unit.c(grobY(airlines, "south") + unit(acarrier, "lines"), 
             grobY(airlines, "south") + unit(acarrier, "lines"),
             grobY(flights, "south") + unit(fcarrier, "lines"),
             grobY(flights, "south") + unit(fcarrier, "lines"))

grid.xspline(x3, y3, shape=.15,
      arrow = arrow(type = "closed", angle = 15, length=unit(2, "mm")),
     gp=gpar(fill="black", lwd = 1) )

grid.points(grobX(airlines, "west"), grobY(airlines, "south") + unit(acarrier, "lines"),
      default.units = "npc", pch = 21, size = unit(2,"mm"), 
      gp=gpar(fill = "white", lwd = 1.5, pch = 21))


# weather box to midpoint
x3 <- unit.c(grobX(weather, "west"),   
             grobX(weather, "west") - .333*(grobX(weather, "west") - grobX(flights, "east")),
             grobX(weather, "west") - .333*(grobX(weather, "west") - grobX(flights, "east")),
             .5*(grobX(weather, "west") + grobX(flights, "east")))

y3 <- unit.c(grobY(weather, "south") + unit(wyear, "lines"), 
             grobY(weather, "south") + unit(wyear, "lines"),
             grobY(flights, "south") + unit(fday, "lines"),
             grobY(flights, "south") + unit(fday, "lines"))

grid.xspline(x3, y3, shape=.15,
      #arrow = arrow(type = "closed", angle = 15, length=unit(3, "mm")),
     gp=gpar(fill="black", lwd = 1) )

grid.points(grobX(weather, "west"), grobY(weather, "south") + unit(wyear, "lines"),
      default.units = "npc", pch = 21, size = unit(2,"mm"), 
      gp=gpar(fill = "white", lwd = 1.5, pch = 21))



x3 <- unit.c(grobX(weather, "west"),   
             grobX(weather, "west") - .333*(grobX(weather, "west") - grobX(flights, "east")),
             grobX(weather, "west") - .333*(grobX(weather, "west") - grobX(flights, "east")))

y3 <- unit.c(grobY(weather, "south") + unit(wmonth, "lines"), 
             grobY(weather, "south") + unit(wmonth, "lines"),
             grobY(flights, "south") + unit(fday, "lines"))

grid.xspline(x3, y3, shape=.15,
      #arrow = arrow(type = "closed", angle = 15, length=unit(3, "mm")),
     gp=gpar(fill="black", lwd = 1) )

grid.points(grobX(weather, "west"), grobY(weather, "south") + unit(wmonth, "lines"),
      default.units = "npc", pch = 21, size = unit(2,"mm"), 
      gp=gpar(fill = "white", lwd = 1.5, pch = 21))



x3 <- unit.c(grobX(weather, "west"),   
             grobX(weather, "west") - .333*(grobX(weather, "west") - grobX(flights, "east")),
             grobX(weather, "west") - .333*(grobX(weather, "west") - grobX(flights, "east")))

y3 <- unit.c(grobY(weather, "south") + unit(wday, "lines"), 
             grobY(weather, "south") + unit(wday, "lines"),
             grobY(flights, "south") + unit(fday, "lines"))

grid.xspline(x3, y3, shape=.15,
      #arrow = arrow(type = "closed", angle = 15, length=unit(3, "mm")),
     gp=gpar(fill="black", lwd = 1) )

grid.points(grobX(weather, "west"), grobY(weather, "south") + unit(wday, "lines"),
      default.units = "npc", pch = 21, size = unit(2,"mm"), 
      gp=gpar(fill = "white", lwd = 1.5, pch = 21))



x3 <- unit.c(grobX(weather, "west"),   
             grobX(weather, "west") - .333*(grobX(weather, "west") - grobX(flights, "east")),
             grobX(weather, "west") - .333*(grobX(weather, "west") - grobX(flights, "east")))

y3 <- unit.c(grobY(weather, "south") + unit(whour, "lines"), 
             grobY(weather, "south") + unit(whour, "lines"),
             grobY(flights, "south") + unit(fday, "lines"))

grid.xspline(x3, y3, shape=.15,
      #arrow = arrow(type = "closed", angle = 15, length=unit(3, "mm")),
     gp=gpar(fill="black", lwd = 1) )

grid.points(grobX(weather, "west"), grobY(weather, "south") + unit(whour, "lines"),
      default.units = "npc", pch = 21, size = unit(2,"mm"), 
      gp=gpar(fill = "white", lwd = 1.5, pch = 21))



x3 <- unit.c(grobX(weather, "west"),   
             grobX(weather, "west") - .333*(grobX(weather, "west") - grobX(flights, "east")),
             grobX(weather, "west") - .333*(grobX(weather, "west") - grobX(flights, "east")))

y3 <- unit.c(grobY(weather, "south") + unit(worigin, "lines"), 
             grobY(weather, "south") + unit(worigin, "lines"),
             grobY(flights, "south") + unit(fday, "lines"))

grid.xspline(x3, y3, shape=.15,
      #arrow = arrow(type = "closed", angle = 15, length=unit(3, "mm")),
     gp=gpar(fill="black", lwd = 1) )

grid.points(grobX(weather, "west"), grobY(weather, "south") + unit(worigin, "lines"),
      default.units = "npc", pch = 21, size = unit(2,"mm"), 
      gp=gpar(fill = "white", lwd = 1.5, pch = 21))


grid.points(.5*(grobX(weather, "west") + grobX(flights, "east")), grobY(flights, "south") + unit(fday, "lines"),
      default.units = "npc", pch = 21, size = unit(2,"mm"), 
      gp=gpar(fill = "black", lwd = 1.5, pch = 21))



# midpoint to flight box
x3 <- unit.c(.5*(grobX(weather, "west") + grobX(flights, "east")),   
             grobX(weather, "west") - .667*(grobX(weather, "west") - grobX(flights, "east")),
             grobX(weather, "west") - .667*(grobX(weather, "west") - grobX(flights, "east")),
             grobX(flights, "east"))

y3 <- unit.c(grobY(flights, "south") + unit(fday, "lines"), 
             grobY(flights, "south") + unit(fday, "lines"),
             grobY(flights, "south") + unit(fyear, "lines"),
             grobY(flights, "south") + unit(fyear, "lines"))

grid.xspline(x3, y3, shape=.15,
      arrow = arrow(type = "closed", angle = 15, length=unit(2, "mm")),
     gp=gpar(fill="black", lwd = 1) )



x3 <- unit.c(  
             grobX(weather, "west") - .667*(grobX(weather, "west") - grobX(flights, "east")),
             grobX(weather, "west") - .667*(grobX(weather, "west") - grobX(flights, "east")),
             grobX(flights, "east"))

y3 <- unit.c(
             grobY(flights, "south") + unit(fday, "lines"),
             grobY(flights, "south") + unit(fmonth, "lines"),
             grobY(flights, "south") + unit(fmonth, "lines"))

grid.xspline(x3, y3, shape=.15,
      arrow = arrow(type = "closed", angle = 15, length=unit(2, "mm")),
     gp=gpar(fill="black", lwd = 1) )


x3 <- unit.c(  
             grobX(weather, "west") - .667*(grobX(weather, "west") - grobX(flights, "east")),
             grobX(weather, "west") - .667*(grobX(weather, "west") - grobX(flights, "east")),
             grobX(flights, "east"))

y3 <- unit.c(
             grobY(flights, "south") + unit(fday, "lines"),
             grobY(flights, "south") + unit(fday, "lines"),
             grobY(flights, "south") + unit(fday, "lines"))

grid.xspline(x3, y3, shape=.15,
      arrow = arrow(type = "closed", angle = 15, length=unit(2, "mm")),
     gp=gpar(fill="black", lwd = 1) )



x3 <- unit.c(
             grobX(weather, "west") - .667*(grobX(weather, "west") - grobX(flights, "east")),
             grobX(weather, "west") - .667*(grobX(weather, "west") - grobX(flights, "east")),
             grobX(flights, "east"))

y3 <- unit.c(
             grobY(flights, "south") + unit(fday, "lines"),
             grobY(flights, "south") + unit(fhour, "lines"),
             grobY(flights, "south") + unit(fhour, "lines"))

grid.xspline(x3, y3, shape=.15,
      arrow = arrow(type = "closed", angle = 15, length=unit(2, "mm")),
     gp=gpar(fill="black", lwd = 1) )



x3 <- unit.c(   
             grobX(weather, "west") - .667*(grobX(weather, "west") - grobX(flights, "east")),
             grobX(weather, "west") - .667*(grobX(weather, "west") - grobX(flights, "east")),
             grobX(flights, "east"))

y3 <- unit.c(
             grobY(flights, "south") + unit(fday, "lines"),
             grobY(flights, "south") + unit(forigin, "lines"),
             grobY(flights, "south") + unit(forigin, "lines"))

grid.xspline(x3, y3, shape=.15,
      arrow = arrow(type = "closed", angle = 15, length=unit(2, "mm")),
     gp=gpar(fill="black", lwd = 1) )


