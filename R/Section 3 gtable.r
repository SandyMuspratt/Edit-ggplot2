
###############################################################
##
## Section 3: Constructing and editing gtables
##
###############################################################



###############################################################
##
## Lecture 8: What is gtable
##
###############################################################



###############################################################
##
## Lecture 9: Constructing a gtable and adding grobs to the gtable
##
##   Construct arrays of grobs
##   Construct an empty gtable
##   Populate a gtable with grobs
##
###############################################################

# gtable can be constructed as 
#    a column,
#    a row,
#    an array.

# First, create some grobs.
library(grid)

rect <- rectGrob(gp = gpar(fill = "orange"),
                  name = "gRect")

polygon <- polygonGrob(x = c(0, 0.5, 1, 0.5), y = c(0.5, 1, 0.5, 0), 
                        gp = gpar(fill = "burlywood3", lwd = 3),
                        name = "gPolygon")

text <- textGrob(label = "Learning gtable",
                  gp = gpar(cex = 1.2),
                  name = "gText")

PolygonText <- gTree(name = "gPolygonText", children = gList(polygon, text))

myTree <- gTree(name = "gTree", children = gList(rect, PolygonText))



# Set up a gtable with one column
# Insert grobs
library(gtable)
gCol <- gtable_col(name = "gCol", list(text, polygon, rect))
grid.newpage()
grid.draw(gCol)

# Set up a gtable with one row
# Insert grobs
gRow  <- gtable_row("gRow", list(text, polygon, rect))
grid.newpage()
grid.draw(gRow)


# Alternatively,  we can create a gtable as a matrix
# The grobs need to be in a list,
# and collected into a matrix.
# The last grob in the list is a null grob,
# In the matrix, it is positioned at the bottom right cell.
# Widths and heights need to be set.
# Note the unit function:
#     number and unit of measurement.
# There are two widths because there are two columns;
# There are two heights because there are two rows.

# The null unit:
# Indicates relative widths and heights.
# widths = unit(c(1,1), "null") means two equal widths.
# widths = unit(c(1,2), "null") means the second width is twice the first width.
 
mat <- matrix(list(rect, polygon, text, nullGrob()), nrow = 2)
gMat <- gtable_matrix("gMat", mat, 
	                    widths = unit(c(1, 1), "null"), 
	                    heights = unit(c(1,1), "null"))
grid.newpage()
grid.draw(gMat) 



## Another way - gtable
# Construct the table,
# then add grobs to selected cells within the table.
# Use gtable() function to construct the gtable;
# here, a 3 X 3 array of cells (note: three heights and three widths)
gt <- gtable(name = "myGtable", 
	           heights = unit(c(1, 1, 1), "null"), 
	           widths = unit(c(1, 1, 1), "null"))

gt <- gtable_add_grob(gt, text, t=1, l=1, name = text$name)
gt <- gtable_add_grob(gt, PolygonText, t=2, l=2, name = PolygonText$name)
gt <- gtable_add_grob(gt, myTree, t=3, l=3, name = myTree$name)
grid.newpage()
grid.draw(gt)

# Use gtable_add_grob() function to add the grobs:
#   gtable to which grob will be added;
#   grob to be added
#   t = top, l = left, b = bottom, and r = right indicate the cells where the grob will be added.
# If t = b, and/or l = r, there then b and r can be omitted.

 

## Diagram 1 (Slide 10) What the layout looks like.
library(grid)
pushViewport(viewport(width = .95, h = .95))
#grid.rect()
pushViewport(viewport(x = .5-1/3, y = .5+1/3, w =1/3, h = 1/3))
grid.rect(gp = gpar(fill = "grey"))
grid.text(paste(c("row 1", "col 1"), collapse = "\n"))
upViewport()
pushViewport(viewport(x = .5, y = .5, w = 1/3, h = 1/3))
grid.rect(gp = gpar(fill = "grey"))
grid.text(paste(c("row 2", "col 2"), collapse = "\n"))
upViewport()
pushViewport(viewport(x = .5+1/3, y = .5-1/3, w = 1/3, h = 1/3))
grid.rect(gp = gpar(fill = "grey"))
grid.text(paste(c("row 3", "col 3"), collapse = "\n"))
upViewport()
pushViewport(viewport(x = .5, y = .5+1/3, w = 1/3, h = 1/3))
grid.rect()
upViewport()
pushViewport(viewport(x = .5, y = .5-1/3, w = 1/3, h = 1/3))
grid.rect()
upViewport()
pushViewport(viewport(x = .5-1/3, y = .5, w = 1/3, h = 1/3))
grid.rect()
upViewport()
pushViewport(viewport(x = .5-1/3, y = .5-1/3, w = 1/3, h = 1/3))
grid.rect()
upViewport()
pushViewport(viewport(x = .5+1/3, y = .5, w = 1/3, h = 1/3))
grid.rect()
upViewport()
pushViewport(viewport(x = .5+1/3, y = .5+1/3, w = 1/3, h = 1/3))
grid.rect()
upViewport()
upViewport()


## Variation 1
# The grob in the middle row is to span three columns.
gt <- gtable(name = "myGtable", 
	           heights = unit(c(1, 1, 1), "null"), 
	           widths = unit(c(1, 1, 1), "null"))


gt <- gtable_add_grob(gt, text, t=1, l=1, name = text$name)

#  To position PolygonText to span across the middle row, set l = 1 and r = 3
gt <- gtable_add_grob(gt, PolygonText, t=2, l=1, r=3, name = PolygonText$name)

gt <- gtable_add_grob(gt, myTree, t=3, l=3, name = myTree$name)
grid.newpage()
grid.draw(gt)


## Diagram 2 (Slide 14) - What the layout looks like.
library(grid)
pushViewport(viewport(width = .95, h = .95))
#grid.rect()
pushViewport(viewport(x = .5-1/3, y = .5+1/3, w =1/3, h = 1/3))
grid.rect(gp = gpar(fill = "grey"))
grid.text(paste(c("row 1", "col 1"), collapse = "\n"))
upViewport()
pushViewport(viewport(x = .5+1/3, y = .5-1/3, w = 1/3, h = 1/3))
grid.rect(gp = gpar(fill = "grey"))
grid.text(paste(c("row 3", "col 3"), collapse = "\n"))
upViewport()
pushViewport(viewport(x = .5-1/3, y = .5-1/3, w = 1/3, h = 1/3))
grid.rect()
upViewport()
pushViewport(viewport(x = .5+1/3, y = .5+1/3, w = 1/3, h = 1/3))
grid.rect()
upViewport()
pushViewport(viewport(x = .5, y = .5-1/3, w = 1/3, h = 1/3))
grid.rect()
upViewport()
pushViewport(viewport(x = .5, y = .5+1/3, w = 1/3, h = 1/3))
grid.rect()
upViewport()
pushViewport(viewport(x = .5, y = .5, w = 1, h = 1/3))
grid.rect(gp = gpar(fill = "grey", col="red", lwd = 5))
grid.text(paste(c("row 2", "cols 1, 2, 3"), collapse = "\n"))
upViewport()
upViewport()



# Variation 2
# The centre cell is to have twice the width and twice the height of the other cells.
gt <- gtable(heights = unit(c(1, 2, 1), "null"), widths = unit(c(1, 2, 1), "null"))

gt <- gtable_add_grob(gt, text, t=1, l=1, name = text$name)
gt <- gtable_add_grob(gt, PolygonText, t=2, l=2, name = rect$name)
gt <- gtable_add_grob(gt, myTree, t=3, l=3, name = polygon$name)
grid.newpage()
grid.draw(gt)

## Diagram 3 (Slide 17) - What the layout looks like.
pushViewport(viewport(width = .95, h = .95))
pushViewport(viewport(x = .5-3/8, y = .5+3/8, w =1/4, h = 1/4))
grid.rect(gp = gpar(fill = "grey"))
grid.text(paste(c("row 1", "col 1"), collapse = "\n"), gp = gpar(fontsize = 15))
upViewport()
pushViewport(viewport(x = .5, y = .5, w = 1/2, h = 1/2))
grid.rect(gp = gpar(fill = "grey"))
grid.text(paste(c("row 2", "col 2"), collapse = "\n"), gp = gpar(fontsize = 15))
upViewport()
pushViewport(viewport(x = .5+3/8, y = .5-3/8, w = 1/4, h = 1/4))
grid.rect(gp = gpar(fill = "grey"))
grid.text(paste(c("row 3", "col 3"), collapse = "\n"), gp = gpar(fontsize = 15))
upViewport()
pushViewport(viewport(x = .5, y = .5+3/8, w = 1/2, h = 1/4))
grid.rect()
upViewport()
pushViewport(viewport(x = .5, y = .5-3/8, w = 1/2, h = 1/4))
grid.rect()
upViewport()
pushViewport(viewport(x = .5-3/8, y = .5, w = 1/4, h = 1/2))
grid.rect()
upViewport()
pushViewport(viewport(x = .5-3/8, y = .5-3/8, w = 1/4, h = 1/4))
grid.rect()
upViewport()
pushViewport(viewport(x = .5+3/8, y = .5, w = 1/4, h = 1/2))
grid.rect()
upViewport()
pushViewport(viewport(x = .5+3/8, y = .5+3/8, w = 1/4, h = 1/4))
grid.rect()
upViewport()
upViewport()


## By way of completeness - Doing the same using grid.
layout <- grid.layout(3, 3,
                     heights = unit(c(1, 2, 1), "null"), 
                     widths = unit(c(1, 2, 1), "null"))
pushViewport(viewport(layout = layout))

pushViewport(viewport(layout.pos.col=1, layout.pos.row=1))
grid.draw(text)
upViewport()

pushViewport(viewport(layout.pos.col=1:3, layout.pos.row=2))
grid.draw(PolygonText)
upViewport()

pushViewport(viewport(layout.pos.col=3, layout.pos.row=3))
grid.draw(myTree)
upViewport(2)


# The main points
# Use the gtable_col(), gtable_row(), and gtable_matrix() to construct tables of grobs.
# Use gtable() to construct an empty gtable.
# Use gtable_add_grob() to add grobs to selected cell in the gtable. 
# Selected cells using t = top, l = left, b = bottom, and r = right. 
# Set cell sizes by setting widths and heights.




###############################################################
##
## Lecture 10: Exploring the gtable
##
##   Functions for exploring a gtable
##     gtable_show_layout
##     str
##     names
##       grobs
##       heights; widths
##       layout
##     grid.ls
##
###############################################################


# First, set up a gtable
library(gtable)
library(grid)
 
rect <- rectGrob(gp = gpar(fill = "orange"),
                  name = "gRect")

polygon <- polygonGrob(x = c(0, 0.5, 1, 0.5), y = c(0.5, 1, 0.5, 0), 
                        gp = gpar(fill = "burlywood3", lwd = 3),
                        name = "gPolygon")

text <- textGrob(label = "Learning gtable",
                  gp = gpar(cex = 1.2),
                  name = "gText")

PolygonText <- gTree(name = "gPolygonText", children = gList(polygon, text))

myTree <- gTree(name = "gMyTree", children = gList(rect, PolygonText))

gt <- gtable(name = "myGtable", heights = unit(c(1, 1, 1), "null"), 
                                widths = unit(c(1, 1, 1), "null"))

gt <- gtable_add_grob(gt, text, t=1, l=1, name = text$name)
gt <- gtable_add_grob(gt, PolygonText, t=2, l=2, name = PolygonText$name)
gt <- gtable_add_grob(gt, myTree, t=3, l=3, name = myTree$name)
grid.newpage()
grid.draw(gt)



## Get a diagram of the gtable layout
gtable_show_layout(gt)

# The numbers in the cells are the row and column numbers.
# The red number and unit are the widths and heights.


## Structure of the gtable
str(gt)

# Note: Three grobs: gText, gPolygonText, and gMyTree
# gPolygonText contains two children: gPolygon and gText
# gMyTree contains two children: gRect and gPolygonText;
#    and gPolygonText in turn contains two children: gPolygon and gTex
# At the end: layout and widths. More later


## Components of the gtable
names(gt)

# grobs: A list of grobs - what type of grob, and its name.
# layout: A dataframe giving the position of each grob in the layout.
# widths: Size (number and unit) of each column of the layout.
# heights: Size (number and unit) of each row of the layout.
# respect: Parameter indicating whether dimensions are linked (fixed aspect ratio).
# colnames: Rows can be given names.
# rownames: Columns can be given names.
# name: Name of the whole table - defaults to "layout".
# gp: Graphics parameters, but they cannot be set via the gp slot for the gtable as a whole.
# vp: Viewport defining location, size and orientation of the gtable - Graphics parameters for the whole table can be set here.
# children: List of children - applies only to the grobs that have children.
# childrenOrder: Order in which children are drawn - applies only to the grobs that have children.


## What are the values for some of these components?
gt$name
gt$widths
gt$heights
gt$grobs


## A closer look at layout
gt$layout

# A dataframe.
# name:    The names of the grobs.
# z:       The order in which the grobs are drawn.
# clip:    Whether or not the grobs are clipped to their viewports.
#          Note: the default for gtable is "on".
# t,l,b,r: Locations of the grobs in the layout. 
#          They should agree with t,l,b,r used to populate the gtable with grobs.
#          That is, they should agree with the values set in gtable_add_grob().


gt <- gtable(name = "myGtable", 
         heights = unit(c(1, 1, 1), "null"), widths = unit(c(1, 1, 1), "null"))
gt <- gtable_add_grob(gt, text, t=1, l=1, name = text$name)

gt <- gtable_add_grob(gt, PolygonText,
           t=2, l=1, r=3,
           name = PolygonText$name)

gt <- gtable_add_grob(gt, myTree, t=3, l=3, name = myTree$name)

gtable_show_layout(gt)
gt$layout




## Use grid functions to explore the gtable.
# grid.ls() returns a list of grobs.
# Unfortunately, grid.ls sees just one grob.
grid.newpage()
grid.draw(gt)
grid.ls()

# Apply grid.force first,
# then grid.ls sees the grobs
grid.ls(grid.force())


## current.vpTree() returns the viewports
grid.newpage()
grid.draw(gt)
current.vpTree()


# Function to make the output of current.vpTree() more readable
# Call it like this ...
#   formatVPTree(current.vpTree())
formatVPTree <- function(x, indent=0) {
	  x = gsub("viewport\\[", "", x) 
    x = gsub("\\]", "", x) 	
    end <- regexpr("[)]+,?", x)
    sibling <- regexpr(", ", x)
    child <- regexpr("[(]", x)
    if ((end < child || child < 0) && (end < sibling || sibling < 0)) {
        lastchar <- end + attr(end, "match.length")
        cat(paste0(paste(rep("  ", indent), collapse=""), 
                   substr(x, 1, end - 1), "\n"))
        if (lastchar < nchar(x)) {
            formatVPTree(substring(x, lastchar + 1), 
                         indent - attr(end, "match.length") + 1)
        }
    }
    if (child > 0 && (sibling < 0 || child < sibling)) {
        cat(paste0(paste(rep("  ", indent), collapse=""), 
                   substr(x, 1, child - 3), "\n"))
        formatVPTree(substring(x, child + 1), indent + 1)
    }
    if (sibling > 0 && sibling < end && (child < 0 || sibling < child)) {
        cat(paste0(paste(rep("  ", indent), collapse=""), 
                   substr(x, 1, sibling - 1), "\n"))
        formatVPTree(substring(x, sibling + 2), indent)
    }
}

formatVPTree(current.vpTree())



# The main points
# gtable_show_layout() returns a diagram of the layout of the gtable.
# str() returns the structure of the gtable: lists within lists.
# names() returns the names of the objects within the gtable.
#    gtable$names returns the names of the grobs.
#    gtable$widths; gtable$heights returns the widths and heights of the columns and rows.
#    gtable$layout returns a dataframe of layout information.
# grid.ls(grid.force()) - a grid function for getting the names of the grobs.
# current.vpTree() - a grid function for getting the names of the viewports.




###############################################################
##
## Lecture 11: A first go at editing gtable
##
##   Change the shape and layout of a given gtable
##    Adjusting widths, heights, and layout
## 
###############################################################

# Three ways to think about editing a gtable:

#   1. Changing the shape or layout of a given table
#   2. Adding to or subtracting from a gtable
#   3. Editing the grobs that populate a gtable


#  This lecture: Changing width, height and layout parameters 
#  to change the shape and layout of a given gtable. 

# Lecture 12: Use gtable functions to add to or substract from a gtable.
# Lecture 13: Editing the grobs.

# Set up a gtable
library(gtable)
library(grid)
 
rect <- rectGrob(gp = gpar(fill = "orange"),
                  name = "gRect")

polygon <- polygonGrob(x = c(0, 0.5, 1, 0.5), y = c(0.5, 1, 0.5, 0), 
                        gp = gpar(fill = "burlywood3", lwd = 3),
                        name = "gPolygon")

text <- textGrob(label = "Learning gtable",
                  gp = gpar(cex = 1.2),
                  name = "gText")

PolygonText <- gTree(name = "gPolygonText", children = gList(polygon, text))

myTree <- gTree(name = "gMyTree", children = gList(rect, PolygonText))

gt <- gtable(name = "myGtable", heights = unit(c(1, 1, 1), "null"), 
                                widths = unit(c(1, 1, 1), "null"))

gt <- gtable_add_grob(gt, text, t=1, l=1, name = text$name)
gt <- gtable_add_grob(gt, PolygonText, t=2, l=2, name = PolygonText$name)
gt <- gtable_add_grob(gt, myTree, t=3, l=3, name = myTree$name)
grid.newpage()
grid.draw(gt)



# Recall the objects:
#   widths and heights
#   layout
#
# In Lecture 10, I re-drew a gtable.
# That is not always possible.
# Here, I edit the gtable by adjusting:
#    width; heights
#    layout


# Consider these edits:
#   The middle cell, the cell that contains gPolygonText, is to have twice the height and twice the width of its surrounding cells;
#   The content of the middle cell, gPolygonText to extend across all three columns;
#   The content of the middle cell, gPolygonText to fill the whole table;
#   Swap the positions of gText and gMyTree.


## EDIT 1: widths and heights to change the size of the cell that contains a grob.
gt1 <- gt   # So I keep the original
gt1$heights
gt1$widths

# The second height and the second width need to be doubled.

gt1$heights[2] <- unit(2, "null")
gt1$widths[2] <- unit(2, "null")

grid.newpage()
grid.draw(gt1)


## EDIT 2: t, l, b, r to change the extent of a grob
# layout is a data frame - Indexing
gt2 <- gt    # So I keep the original
gt2$layout

gt2$layout[2, 2] <- 1
gt2$layout[2, 4] <- 3
gt2$layout

grid.newpage()
grid.draw(gt2)


## Alternative EDIT 2:
# gt$layout is a data frame - 
# If the grobs have a name, select the grob using its name.
# Select appropriate t, l, b, r columns. 
gt2 <- gt    # So I keep the original
gt2$layout

gt2$layout$name == "gPolygonText"

gt2$layout[gt2$layout$name == "gPolygonText", c("l", "r") ] <- c(1, 3)
gt2$layout

grid.newpage()
grid.draw(gt2)


# 2nd alternative EDIT 2:
# gt$layout is a data frame - subset to select the rows and columns,
# I use regular expression to select the row.
gt2 <- gt    # So I keep the original
gt2$layout

row <- grepl("Poly", gt2$layout$name)

gt2$layout[row, c("l", "r")] <- c(1, 3)
gt2$layout

grid.newpage()
grid.draw(gt2)



## Another EDIT 2: Change the order of printing
# gt$layout is a data frame - subset to select the rows and columns.
# I use regular expression to select the row.
gt3 = gt    # So I keep the original
gt3$layout

row = grepl("Tree", gt3$layout$name)

gt3$layout[row, c("t", "l")] <- c(1, 1)
gt3$layout

grid.newpage()
grid.draw(gt3)

# Consider column z in the layout. 
gt3$layout[3, "z"] <- 0
grid.newpage()
grid.draw(gt3)


## EDIT 3:
# gt$layout is a data frame - subset to select the rows and columns,
# gPolygonText to occupy 3,3
# gMyTree to occupy 1,1

gt4 = gt    # So that I keep the original
gt4$layout


gt4$layout[grepl("gText", gt4$layout$name), c("t", "l", "b", "r")] <- 3
gt4$layout[grepl("Tree", gt4$layout$name), c("t", "l", "b", "r")] <- 1
gt4$layout

grid.newpage()
grid.draw(gt4)


## The main points
# Adjust values and units in \texttt{widths} and \texttt{heights} to change the size of cells.
# Adjust values for t, l, b, r in the layout data frame to change the extent of a particular grob. 
# Adjust values for t, l, b, r in the layout data frame to change the position of a particular grob. 
# Adjust value for z in the layout data frame to change the order in which grobs are drawn. 
# Select the relevant rows and columns of the layout data frame using index notation.  
# Select the relevant rows and columns of the layout data frame using names and/or regular expressions.




###############################################################
##
## Lecture 12: Adding to and subtracting from the gtable
##
## Adding rows and columns to a gtable
## Combining gtables
## Extracting grobs from a gtable, 
## and putting them somewhere else in the gtable
##
###############################################################

# Modifying the table
# Editing function in gtable:

#   gtable_add_rows, gtable_add_cols: add rows and columns to the gtable
#   cbind, rbind - combine gtables
#   gtable_filter - extract cells 

#   Indexing - select cells from a table - or drop cells from a table

#   Not a function but it would be good if there was one - delete grobs but not the cells


# Set up a gtable
library(gtable)
library(grid)
 
rect <- rectGrob(gp = gpar(fill = "orange"),
                  name = "gRect")

polygon <- polygonGrob(x=c(0, 0.5, 1, 0.5), y=c(0.5, 1, 0.5, 0), 
                        gp = gpar(fill = "burlywood3", lwd = 3),
                        name = "gPolygon")

text <- textGrob(label = "Learning gtable",
                  gp = gpar(cex = 1.2),
                  name = "gText")

PolygonText <- gTree(name = "gPolygonText", children = gList(polygon, text))

myTree <- gTree(name = "gMyTree", children = gList(rect, PolygonText))

gt <- gtable(name = "myGtable", heights = unit(c(1, 1, 1), "null"), 
                                widths = unit(c(1, 1, 1), "null"))

gt <- gtable_add_grob(gt, text, t=3, l=3, name = text$name)
gt <- gtable_add_grob(gt, PolygonText, t=2, l=2, name = PolygonText$name)
gt <- gtable_add_grob(gt, myTree, t=1, l=1, name = myTree$name)
grid.newpage()
grid.draw(gt)




## Adding rows and columns to the gtable

# Use the gtable_add_rows() function,
# or the gtable_add_cols() function.

# Parameters are:
#    The gtable to be modified;
#    The height of the row (or the width of the column);
#    Position in the table where the new row (or column) is to be added.

# The height is a unit vector - as before. 
# So far, I used the null unit.
# All grid units are available:
#   absolute units such as: in, cm, mm, point;
#   relative units such as: null, npc, native;
#   units that adjust to the size of a grob, such as: grobwidth, strwidth, lines.

# Position is a number - the numbers you see in gtable_show_layout.
# The new row (or column) is added below (or to the right of) the position number.
# Exceptions: pos=0    new row added to the top (or the far left).
#             pos=-1   new row added to the bottom (or the far right).


gt1 <- gtable_add_rows(gt, unit(3, "lines"), 0)

gtable_show_layout(gt)   # Original
gtable_show_layout(gt1)  # Modified

gt$layout
gt1$layout


grid.newpage()
grid.draw(gt)

grid.newpage()
grid.draw(gt1)





# The new row is empty.
# Any grob can be added.
# I'll add a title - that is, a textGrob

# Title grob
Title <- textGrob("Some grobs in a gtable")

# Add Title grob to row 1
gt2 <- gtable_add_grob(gt1, Title, t = 1, l = 1, r = 3, name = "gTitle")

# Draw the gtable
grid.newpage()
grid.draw(gt2)



# The new row had its height set with "lines" unit. 
# The height of a "line" is determined by fontsize and cex already in place in the table.
# I want the new row's height to adjust its height according to fontsize of the Title grob.

# A title, but the row it goes into adjusts to the title's height.
Title <- textGrob("Some grobs in a gtable", 
               gp = gpar(fontface = "bold", fontsize = 18))

convertHeight(grobHeight(Title), "cm")

Title <- textGrob("Some grobs in a gtable", 
               gp = gpar(fontface = "bold", fontsize = 40))

convertHeight(grobHeight(Title), "cm")




# A little extra space for the Title grob
padding <- unit(1, "lines")

# Create the grob to be added to the new row
Title <- textGrob("Some grobs in a gtable", 
               gp = gpar(fontface = "bold", fontsize = 18))

# Add a new row to the gtable
gt1 <- gtable_add_rows(gt, 
               heights = grobHeight(Title) + padding, 0)

# Add the new grob to the new row
gt2 <- gtable_add_grob(gt1, Title, 
               t = 1, l = 1, r = 3, name = "gTitle")

# Draw the new gtable
grid.newpage()
grid.draw(gt2)



## A title that is itself a combinations of grobs - a rectangle and text.

# Create the separate grobs to be added to the new row
TitleText <- textGrob("Some grobs in a gtable", 
            gp = gpar(fontface = "bold", fontsize = 18))
TitleRect <- rectGrob(gp = gpar(fill = "skyblue"))

# Combine the two
Title <- gTree(name = "gTitle", 
             children = gList(TitleRect, TitleText))

padding <- unit(2.5, "lines")
# Add a new row to the gtable.
# But with some care.
# The Title grob doesn't have a height, or rather it has the default height.
# To set the height to the height of the text, I need to get the grob height of the text height.
gt1 <- gtable_add_rows(gt,  grobHeight(TitleText) + padding, 0)

# Add the new grob to the new row
gt2 <- gtable_add_grob(gt1, Title, t = 1, l = 1, r = 3, name = "gTitle")

# Draw the new gtable
grid.newpage()
grid.draw(gt2)


# Suppose the height needs to be a little larger; say twice the padding
# I could redraw the gtable with the new padding;
# or, use the gtable's height parameter to set the required height.
# Which height needs adjusting?
gt2$heights
# The first height.
gt2$heights[1] <- grobHeight(TitleText) + 2*padding
grid.newpage()
grid.draw(gt2)




## Extra for experts
# The title was centred within the blue rectangle.
# Another approach to title - the ggplot2 approach.
# Titles appear often in ggplot: plot titles, axis titles, tick mark labels, strip labels.
# In ggplot, titles have margins.
# And of course, the margins can differ.

# The text is set up as before
text_grob <- textGrob("Some grobs in a gtable", gp = gpar(fontface = "bold", fontsize = 18))
 
# There are three heights: upper and lower margins, and the height of the text.
# The upper margin is "1 lines", the lower margin is "2 lines".
# The three heights are combined - concatenated - in the sense of the c() function;
# but it's the unit.c() function that concatenates units.
heights <- unit.c(unit(1, "lines"), unit(1, "grobheight", text_grob), unit(2, "lines"))

# Some pure grid. 
# Set up a column of three viewports.
# Recall from Lecture 9: a matrix of viewports can be set up using the grid.layout function;
# analogous to the gtable function. 
vp <- viewport(layout = grid.layout(3, 1,    # 3 rows, 1 column
                   heights = heights), name = "vp1")

# The middle row, where the title text will appear, is named as 'child_vp'.
child_vp <- viewport(layout.pos.row = 2,    # select 2nd row
                     name = "vp2")

# I have set up a column of three viewports.
# But only the middle viewport will take the text.
# The upper and lower viewports, the margin viewports, will remain empty.

# Recall that the gTree function is used to set up parent-child structures for grobs.
# The gTree function can also set up parent-child structures for viewports.
# 
# Analogous to children = gList() for parent-child grob structures,
# parent-child viewport structures are set up using: vp = vpTree();
# vpTree takes two arguments: the parent viewport, and the child viewport. 

# One child grob (in gList); one child viewport (in vpList);
# thus the child grob is drawn in the child viewport.
# Also, the heights of the three viewports are set here. 
# (For this drawing, it's not necessary to set the heights here in the gTree function.
# But ggplot does, and thus there are two places where the heights are set.) 

TitleText <- gTree(children = gList(text_grob),
                   vp = vpTree(vp, vpList(child_vp)), heights = heights)

# Combine the TitleText with the blue rectangle, as before.
TitleRect <- rectGrob(gp = gpar(fill = "skyblue"))

Title <- gTree(name = "gTitle", 
             children = gList(TitleRect, TitleText))

# Add a new row to the gtable
gt1 <- gtable_add_rows(gt, 
               heights = sum(heights), 0)

# Add the new Title grob to the new row
gt1 <- gtable_add_grob(gt1, Title, 
               t = 1, l = 1, r = 3, name = "gTitle")

grid.newpage()
grid.draw(gt1)




#  Using cbind, rbind to combine gtables

# When combining by columns (cbind), the two gtables must have the same number of rows.
# When combining by rows (rbind), the two gtables must have the same number of columns.
# Use the gt gtable constructed earlier.

# Consider row binding:
# The tables might have the same number of columns,
# but suppose the widths of the columns in the two tables differ.

# rbind aligns the two tables so that the widths are the same.

# Use the gt gtable constructed earlier.
# Set up a second version that has different widths
gt1 <- gt
gt1$widths <- unit(c(1, 3, 1), c("inches", "inches", "null"))
gt2 <- gt
gt2$widths <- unit(c(3, 1, 1), c("inches", "inches", "null"))
# Check the widths for the two tables
gt1$widths
gt2$widths

grid.newpage()
grid.draw(gt1)

grid.newpage()
grid.draw(gt2)

# Stack the two gtables, but use gt1's widths
gtFirst <- rbind(gt1, gt2, size = "first")
grid.newpage()
grid.draw(gtFirst)

# Stack the two gtables, but use gt'2 widths
gtLast <- rbind(gt1, gt2, size = "last")
grid.newpage()
grid.draw(gtLast)

# Stack the two gtables, but use maximum widths
gtMax <- rbind(gt1, gt2, size = "max")
grid.newpage()
grid.draw(gtMax)


## Set clipping - I haven't yet considered the 'clip' column in the layout data frame.
# Notice that the text no longer fits into the viewport set up for gPolygonText.
gtLast$layout
# That's because clip is set to "on".
# That is, any part of the grob that overflows the viewport is clipped.
# Turn clipping off, and we should see all the text.
# That is, allow the text to extend beyond the boundaries of the viewport.
gtLast$layout$clip <- "off"
grid.newpage()
grid.draw(gtLast)





##   Extract the content of a cell using gtable_filter.
# Why would I want to extract a grob?
# To move it somewhere else.
# To edit the grob then put it back into the gtable.
# But I deal with editing of grobs in the next lecture.

# Using the gt gtable constructed before
MiddleGrob <- gtable_filter(gt, "gPolygonText")

gtNew <- gtable_add_grob(gt, MiddleGrob, t = 2, l = 1)
gtNew <- gtable_add_grob(gtNew, MiddleGrob, t = 2, l = 3)

# Or, add the grobs as a list of grobs
 gtNew <- gtable_add_grob(gt, rep(list(MiddleGrob), 2), t = 2, l = c(1, 3))

grid.newpage()
grid.draw(gtNew)

# Notice I was a little lazy and didn't give the new grobs names when they entered the table.
# So they get default names.
gtNew$layout


# But I could have given them names
gtNew <- gtable_add_grob(gt, rep(list(MiddleGrob), 2), t = 2, l = c(1, 3),
            name = paste0("gPolygonText", c(1, 3)))
gtNew$layout



# Alternatively, I can select a cell and its contents using indexing.
# It works, but for complex gtables, it is better to use the grobs names
MiddleGrob <- gt[2,2]
grid.newpage()
grid.draw(MiddleGrob)


# Another alternative: select the grob from the list of grobs.
# Again it works, but for complex tables, 
# it can be difficult finding the grob.
gt$grobs
# I want the second grob
MiddleGrob <- gt$grobs[[2]]
grid.newpage()
grid.draw(MiddleGrob)



## Drop cells from a table using indexing
# For instance, drop the middle column from gtNew
gtNew1 <- gtNew[, -2]
grid.newpage()
grid.draw(gtNew1)


# Suppose I want to delete the grob but not the cells.
# I need to drop the grob from the list of grobs,
# AND I need to drop the appropriate row from the layout,

gtNew$grobs
gtNew$layout

gtNew2 <- gtNew
gtNew2$grobs <- gtNew2$grobs[-2]
gtNew2$layout <- gtNew2$layout[-2, ]

grid.newpage()
grid.draw(gtNew2)


# Using grep to select the position of the grob:
# 2nd in the list of grobs, 2nd in the layout data frame;
# grep returns a vector of indices.
gtNew3 <- gtNew

pos <- gtNew3$layout$name == "gPolygonText"
pos

gtNew3$grobs <- gtNew3$grobs[!pos]
gtNew3$layout <- gtNew3$layout[!pos, ]

gtNew3$grobs
gtNew3$layout

grid.newpage()
grid.draw(gtNew3)



# But, to delete a grob but not the cells,
# it's probably easier to replace a grob with the nullGrob().
 
gtNew4 <- gtNew

pos <- gtNew4$layout$name == "gPolygonText"
pos

gtNew4$grobs[[which(pos)]] <- nullGrob()

gtNew4$grobs
gtNew4$layout

grid.newpage()
grid.draw(gtNew4)

# or

gtNew4 <- gtNew

pos <- gtNew4$layout$name == "gPolygonText"
pos

gtNew4$grobs[pos] <- list(nullGrob())

gtNew4$grobs
gtNew4$layout

grid.newpage()
grid.draw(gtNew4)




###############################################################
##
## Lecture 13: Editing the grobs that populate a gtable
##
##  Editing using grid functions
##  Editing the slot within a gtable
##  Exracting the grob first, then editing
##  Push to a viewport then editing
##
###############################################################

# Grobs
# Some with hierarchical structures
# within a gtable

# Method 1. Edit using grid edit functions (using lessons learned in Lecture 7)
# Method 2. Edit the slot within the gtable
# Method 3. Extract the grob, edit it, then put it back
# Method 4. Push to the viewport that contains the grob, then edit

# First, create the gtable
library(gtable)
library(grid)
 
rect <- rectGrob(gp = gpar(fill = "orange"),
                  name = "gRect")

polygon <- polygonGrob(x = c(0, 0.5, 1, 0.5), y = c(0.5, 1, 0.5, 0), 
                        gp = gpar(fill = "burlywood3", lwd = 3),
                        name = "gPolygon")

text <- textGrob(label = "Learning gtable",
                  gp = gpar(cex = 1.2),
                  name = "gText")

PolygonText <- gTree(name = "gPolygonText", children = gList(polygon, text))

myTree <- gTree(name = "gMyTree", children = gList(rect, PolygonText))

gt <- gtable(name = "myGtable", heights = unit(c(1, 1, 1), "null"), 
                                widths = unit(c(1, 1, 1), "null"))

gt <- gtable_add_grob(gt, text, t=3, l=3, name = text$name)
gt <- gtable_add_grob(gt, PolygonText, t=2, l=2, name = PolygonText$name)
gt <- gtable_add_grob(gt, myTree, t=1, l=1, name = myTree$name)
grid.newpage()
grid.draw(gt)




## Remember from Lecture 10:
# grid.ls() shows the list of grobs,
# but grid.ls() sees only one grob in a gtable.
# Apply grid.force() first, then the grobs are visible to grid.ls()


## Also, recall from Lecture 4:
# There two versions of the commands for obtaining grobs:
#   *Grob     - Creates a graphic object but does not produce output
#    grid.*   - Creates a graphic object and produces output

# Same goes for the editing function:
#    editGrob   - Edits the object but does not produce output 
#    grid.edit  - Edits the object and outputs the result

# That is, 
# I can edit on-screen, using grid.edit

# Or, I can edit off-screen, using editGrob. 



### Method 1. Edit using grid edit functions (using lessons learned in Lecture 7)

# It treats the gtable as if it is a grid object, 
# with grobs arranged in hierarchical structures. 
# I need to be able to name the relevant grob; 
# and I need the grobs to be visible to the editing function.

## First, some on-screen editing.

gt1 <- gt
grid.newpage()
grid.draw(gt1)

# Editing the grobs within the gtable
grid.ls(grid.force())

grid.edit("gPolygonText.2-2-2-2", gp = gpar(col = "red"))



## Tedious and prone to error having to type full name

grid.newpage(); grid.draw(gt1)
grid.force()
grid.edit("gPolygonText", grep = TRUE, gp = gpar(col = "red"))



## There are two polygonTexts in the gtable.
# Suppose I want the colour for both to be set to "red"

grid.newpage(); grid.draw(gt1)
grid.force()
grid.edit("gPolygonText", grep = TRUE, global = TRUE, gp = gpar(col = "red"))


## grid.gedit function - g for global and g for grep

grid.newpage(); grid.draw(gt1)
grid.force()
grid.gedit("gPolygonText", gp = gpar(col = "red"))


## But what if I want the second PolygonText to be coloured "red".
# I need to provide a path to the relevant grob

grid.newpage(); grid.draw(gt1)
grid.force()
grid.edit("gMyTree.1-1-1-1::gPolygonText", gp = gpar(col = "red"))


## grid.gedit works with the path.
# and the path can be generated in the gPath() function
grid.newpage(); grid.draw(gt1)
grid.force()
grid.gedit(gPath("gMyTree", "Poly"), gp = gpar(col = "red"))


## Now change the line width
grid.gedit(gPath("gMyTree", "Poly"), gp = gpar(lwd = 20))

# The command has no effect because the change to the gp slot is applied to gPolygonText
# But when I set up gPolygonText, 
# line width was applied to one of its children, to gPolygon.
# Remember the rule: A graphic parameter applied to a parent 
# would normally flow through to the children 
# except for the children that already have that parameter set.
# I need to get to the gPloygon grob to change the line width.
# The path begins with gMyTree, 
# then proceeds to gPolygonText, 
# on its way to gPolygon.
# In the code below, grid.gedit means partial names will work:
#    The first "Poly" stands for "gPolygonText", 
#    The second "Poly" stands for "gPolygon".
# These are the names of the child or grand child of "gMyTree", at the head of the tree

grid.gedit(gPath("gMyTree", "Poly", "Poly"), gp = gpar(lwd = 20))





# All this so far is editing on-screen.
# What about editing off-screen
# Because we're dealing with a gtable, 
# the grobs in the gtable are invisible to editing functions. 
# Apply grid.force().

gt1 <- gt
grid.ls(grid.force(gt1))  # list of grobs

# Apply the two edits:
#    In gMyTree, change colour of text and line to red
#    and increase line width to 40

gt1 <- editGrob(grid.force(gt1), gPath("gMyTree", "Poly"), grep = TRUE, 
         gp = gpar(col = "red"))

gt1 <- editGrob(grid.force(gt1), gPath("gMyTree", "Poly", "Poly"), grep = TRUE, 
         gp = gpar(lwd = 20))

grid.newpage(); grid.draw(gt1)



### Method 2. Edit a slot within the gtable

grid.newpage()
grid.draw(gt)

# I want to set the colour of the text in PolygonText to white.
# Within the gt gtable there is a gp slot for the text child of the PolygonText grob.
# Can it be found?
# It can found. But if the gtable is complex, 
# it will be tedious finding the path to the relevant slot - 
# as shown in what follows.

gt2 <- gt 

# The gp slot will be in the grobs
gt2$grobs

# The PolygonText grob is the second grob
gt2$grobs[[2]]

# I need to see the structure
str(gt2$grobs[[2]])
# gText is in the children
gt2$grobs[[2]]$children

# Two children. gText is the required child
gt2$grobs[[2]]$children$gText

# I need to see the structure
str(gt2$grobs[[2]]$children$gText)

# I set colour in the gp slot. I need to gp slot
gt2$grobs[[2]]$children$gText$gp

# cex is already set. I need to set col to "white"
gt2$grobs[[2]]$children$gText$gp$col <- "white"

# Draw the modified gtable
grid.newpage()
grid.draw(gt2)


### An aside:
# Why go all the way to the gp slot of gText?
# Why not change colour at the gp slot for gPolygonText?
# Because that would change not only the colour of the text,
# but also the colour of the ploygon's boundary.
# Remember: gpar parameters flow down to all children 
# (provided the child does not already have that parameter set). 
gt2 <- gt 
gt2$grobs[[2]]$gp = gpar(col = "white")

grid.newpage()
grid.draw(gt2)






# Method 3. Extract the grob, edit it, then put it back
# Do some editing on PolygonText grob
# Change the font colour to white

# Get the grob
# Two ways to get the grob

Grob1 <- gtable_filter(gt, "gPolygonText")

Grob2 <- gt$grobs[[grep(pattern = "gPolygonText", gt$layout$name)]]

Grob3 <- gt[2, 2]

# Problem is, they are not the same
# They are all grobs
is.grob(Grob1); is.grob(Grob2); is.grob(Grob3)

# But the first and third are also a gtables
is.gtable(Grob1); is.gtable(Grob2); is.gtable(Grob3)
# For want of better terminology, Grob1 and Grob3 are gtable grobs,
# Grob2 is a pure grob, or a grid grob

## In the discussion to follow, Grob3 behaves in the same way as Grob1. So, only
## Grob1 and Grob2 are considered in the Lecture 13. But I will include Grob3 here.

# Don't forget to apply grid.force() so that the grobs are visible to grid.ls()
grid.newpage()
grid.draw(Grob1)
grid.ls(grid.force())

grid.newpage()
grid.draw(Grob3)
grid.ls(grid.force())

# Grob2 is not a gtable.
# Therefore, grid.ls() (without grid.force) sees all the grobs
grid.newpage()
grid.draw(Grob2)
grid.ls()

# The same applies when obtaining the 
# structure of an off-screen gtable object.
 
grid.ls(grid.force(Grob1))
grid.ls(grid.force(Grob3))
grid.ls(Grob2)


# So, take care when editing grobs.
# If the grob is a gtable,
# apply grid.force first.

# If the grob is a pure grid grob,
# There is no need to apply grid.force
# but no harm if grid.force is applied



### Start again Method 1.
## Edit off-screen
EGrob1 <- editGrob(grid.force(Grob1), "gText", gp = gpar(col = "white"))
grid.newpage()
grid.draw(EGrob1)

EGrob2 <- editGrob(Grob2, "gText", gp = gpar(col = "white"))
grid.newpage()
grid.draw(EGrob2)

EGrob3 <- editGrob(grid.force(Grob3), "gText", gp = gpar(col = "white"))
grid.newpage()
grid.draw(EGrob3)

## Getting the edited grob back into the original gtable
gt3 <- gt
pos <- grep(pattern = "gPolygonText", gt3$layout$name)
gt3$grobs[[pos]] = EGrob3
grid.newpage()
grid.draw(gt3)


## Edit on-screen: grid.edit()
# Apply grid.force to gtable grobs
# gtable 
grid.newpage()
grid.draw(Grob1)
grid.force()
grid.edit("gText", gp = gpar(col = "white"))

# Not a gtable
grid.newpage()
grid.draw(Grob2)
grid.edit("gText", gp = gpar(col = "white"))

# gtable
grid.newpage()
grid.draw(Grob3)
grid.force()
grid.edit("gText", gp = gpar(col = "white"))


# Putting the edited grob back into the original gtable
EGrob3 <- grid.grab()
gt3 <- gt
pos <- grep(pattern = "gPolygonText", gt3$layout$name)
gt3$grobs[[pos]] = EGrob3
grid.newpage()
grid.draw(gt3)






# Method 4. Push to the viewport, then add grobs to the viewport
# It's not editing a grob
# It adds grobs to a viewport
gt4 <- gt
grid.newpage()
grid.draw(gt4)

# Get the names of the viewports.
# Recall: The format function makes the list a little easier to read
formatVPTree(current.vpTree())

# The PolygonText grob is in the PolygonText viewport
# Move "down" to that viewport
downViewport("gMyTree.1-1-1-1")

grid.rect(width = 0.7, height = 0.2, gp = gpar(fill = "red"))
grid.text("Learning gtable", gp = gpar(col = "white", cex = 1.2))

upViewport(0)



Tree <- as.character(current.vpTree())
text <- "tree"
pattern <- paste0("^.*\\[(.*", text, ".*?)\\].*$")
Tree <- gsub(pattern, "\\1", Tree, ignore.case = TRUE)
downViewport(Tree)

grid.rect(width = 0.7, height = 0.2, gp = gpar(fill = "red"))
grid.text("Learning gtable", gp = gpar(col = "white", cex = 1.2))

upViewport(0)



# Revisiting Method 3:
# Use gtable_add_grob to put the edited grob back into the gtable.
# A lot of the time it doesn't matter that the original remains.
# But sometimes, it does.
# If the edited version does not exactly overlap the original, 
# then the original can show through.
gt5 <- gt
pos <- grep(pattern = "Poly", gt5$layout$name)
Grob <- gt5$grobs[[pos]]
Grob <- editGrob(Grob, vp = viewport(angle = 30))
gt5 <- gtable_add_grob(gt5, Grob, t = 2, l = 2)

grid.newpage()
grid.draw(gt5)



# If it matters that the original shows through,
# remove the original.
gt5 <- gt
pos <- grep(pattern = "Poly", gt5$layout$name)

Grob <- gt5$grobs[[pos]]
Grob <- editGrob(Grob, vp = viewport(angle = 30))

gt5$grobs[[pos]] <- nullGrob()

gt5 <- gtable_add_grob(gt5, Grob, t = 2, l = 2)
grid.newpage()
grid.draw(gt5)




#### Finished Section 3





