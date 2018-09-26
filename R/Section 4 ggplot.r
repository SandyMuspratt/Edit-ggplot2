
###############################################################
## Section 4: ggplot gtables
###############################################################



###############################################################
## Lecture 14: Getting a ggplot gtable
###############################################################

# Draw a single panel graph

# Get the corresponding gtable

# Initial exploration of the gtable

# At some stage move to multipanel ggplot2


# Load packages
library(stringr)
library(gtable)
library(ggplot2)
library(grid)

# Draw graph
set.seed(64534)
p1 <- ggplot(data.frame(x = rnorm(100), y = rnorm(100), 
                       f1 = rep(c("A", "B"), 50),
                       f2 = rep(c("C", "D"), each = 50)), 
            aes(x, y)) + 
      geom_point() +
      theme_grey(base_size = 18) 


# To get the ggplot gtable, use the ggplot2 function, ggplotGrob()
g1 <- ggplotGrob(p1)

# I do indeed have a grob and a gtable
is.grob(g1)
is.gtable(g1)

# Draw the grob
grid.newpage()
grid.draw(g1)





# To see the layout, use gtable_show_layout()
gtable_show_layout(g1)


# What can be learned?
#  The layout has ten rows and seven columns.
#  The cell in the middle is the plot panel.
#  The other rows and columns contain axis material, or are margins, or they are empty.
#  Note the margins: They are 9 points wide
# It is impossible to read the heights and widths in the layout diagram
# Beginning at the top and moving down through the rows, there is:
#     a margin
#     four rows of zero height
#     the plot panel
#     two rows for axis material: tick marks and tick mark labels in row 7; axis label in row 8
#     another row of zero height
#     bottom margin

# Beginning at the left and moving right through the columns, there is:
#     a margin
#     two columns for axis material: axis label; and tick marks and tick mark labels
#     the plot panel
#     two columns of zero width
#     right margin





# Same plot but with a title

p2 <- p1 + ggtitle("A Scatterplot") 

# To get the ggplot gtable, use the ggplot2 function, ggplotGrob()
g2 <- ggplotGrob(p2)
gtable_show_layout(g2)

# Note: the second row has expanded to accommodate the title





# Easier to read heights and widths by inspecting the values of these components of the gtable
g1$heights
# Heights (and widths) are more complex measures than the measures used in gtable I created
# Heights of rows:
# Again, starting at the top:
#   Top margin of 5.5 points
#   Four rows each 0 cm row for the title:
#   The plot panel:
#         the null unit means that the panel takes up whatever space is left over after all the axis material and margin are in place
#   The axis material:
#         there is space for the tick marks (2.75 points), 
#         space for the tick mark label - grobheight unit allows for different font sizes
#   The axis label:
#         1 grobheight 
#   One row: 0 cm
#   Bottom margin of 5.5 points

g1$widths
# Widths of columns:
# Left margin
# Axis label
# Tick marks and tick mark labels
# Plot panel
# Two Columns of zero width
# Right margin



# The layout data frame of the gtable
g1$layout





## More complex ggplot gtables
# Draw a plot with a legend
p3 <- p1 + aes(colour = f1)
p3

g3 <- ggplotGrob(p3)

gtable_show_layout(g3)
g3$layout

a = g3$widths
for(i in seq_along(g3$widths)) {
cat(paste0("[", i, "] ", str_wrap(g3$widths[i], 30, indent = 0, exdent = 5), "\n"))
}


# Add facetting - facet_grid
p4 <- p1 + facet_grid(. ~ f1)
p4

g4 <- ggplotGrob(p4)

gtable_show_layout(g4)
g4$layout
g4$heights


# Add facetting - facet_wrap
p5 <- p1 +  facet_wrap(~ f1)
p5

g5 <- ggplotGrob(p5)

gtable_show_layout(g5)
g5$layout  
g5$heights


# facetting (facet_grid) and a legend
p6 <- p1 + aes(colour = f2) + facet_grid(. ~ f1)
p6

g6 <- ggplotGrob(p6)

g6$layout
g6$widths
gtable_show_layout(g6)


# Add two-way facetting - facet_grid
p7 <- p1 + facet_grid(f1 ~ f2)
p7

g7 <- ggplotGrob(p7)

g7$layout
g7$widths
gtable_show_layout(g7)



## grid functions to explore the gtable
# grid.ls()

p3
grid.ls(grid.force())

grid.ls(grid.force())

grid.newpage()
grid.draw(g1)
grid.ls()

grid.ls(g3)

grid.ls(grid.force(g1))

grid.ls(grid.force(g1))

nestedListing()

# current.vpTree()

x = current.vpTree()
x

x = gsub("viewport\\[GRID\\.VP.+]), ", "", x)

formatVPTree(x)


grid.ls(grid.force(), viewports=TRUE, grob = FALSE)



grid.ls(grid.force(gt2), view = T, grob = F, print = pathListing)

grid.ls(grid.force(gt2), view = T, grob = T, print = grobPathListing)

grid.ls(grid.force(gt2), view = T, grob = F, print = nestedListing)

grid.ls(grid.force(g1), view = F, grob = T, print = grobPathListing)

grid.ls(grid.force(gt), view = F, grob = T, print = nestedListing)

grid.ls(grid.force(gt), view = F, grob = T)


sink("temp.txt")
str(g1)
sink()

test2 <- readLines("temp.txt", n = 10)

str(g1)






###############################################################
## Lecture 15: First edit - Turn off clipping
###############################################################

# ggplot plot panels have clipping turned on.
# That is, any graphic objects placed outside the panel will not appear.

# Scenario 1
# Dot plot (developed by Cleveland - superior alternative to bar chart)
# Should begin at zero:   expand = c(0,0) takes care of that. 
# But this means that part of the dot for zero counts / proportions / percentages 
# will be cut off.
# In the chart, School C has a zero count
# and only half the dot appears.
library(ggplot2)
library(gtable)
library(grid)

data <- data.frame(School = c("A", "B", "C", "D", "E"),
                  Prop = c(0.27, 0.36, 0, 0.15, 0.29))
data$School <- factor(data$School, levels = rev(data$School))

p1 <- ggplot(data , aes(Prop, School)) +
     geom_segment(aes(xend = 0, yend = School), linetype = 3, size = .5) +
     geom_point(size = 3) +
     scale_x_continuous("Proportion of Students", limits = c(0, 1.1*max(data$Prop)), expand = c(0,0)) +
     theme_bw() +
     theme(panel.grid = element_blank(),
           panel.border = element_rect(colour = "black"))

p1

# Get the ggplot grob and check the layout data frame.
# Note: Clip is on for the panel
g1 <- ggplotGrob(p1)
g1$layout

# Turn clip off 
# and check the layout data frame.
# Note: clip is off for the panel
g1$layout$clip[g1$layout$name == "panel"] = "off"
g1$layout

# Draw the graph
grid.newpage()
grid.draw(g1)



# What follows is a series of examples, 
# each of which positions a plot element outside the plot panel,
# and thus each need the panel clipping turned off.
# The first uses hjust to position text outside the plot panel.
# The others use annotation_custom to position grobs outside the plot panel.
# So there is little by way of new material being presented here. 
# But the examples review material on creating grobs (using grid)
# or extracting grobs from other ggplot2 plots.
# 



## Scenario 2
# Some people like to add the counts to the chart, but want to keep the panel clear of text.
# That is the counts are place to the right.

### Ver 1: Place text grobs at "Inf", that is, on the edge of the panel,
# then move them a little further to the right using "hjust".
# But they will be outside the panel, and therefore, not visible because clipping is turned on.
# Turn clipping off, and the text should be visible.
# Also, widen the right margin so that the text will fit.

p2 <- p1 +  
      geom_text(aes(label = Prop, x = Inf, y = School), hjust = -.5, size = 3) +
      theme(plot.margin = unit(c(1, 3, .5, .5), "lines"))

g2 <- ggplotGrob(p2)

g2$layout$clip[g2$layout$name == "panel"] = "off"
grid.newpage()
grid.draw(g2)


# hjust values of 0, .5, or 1 work well; even with strings of differing lengths.
# but hjust values less then zero or greater than one will not align strings of differing lengths.
# hjust leaves space that is a multiple of the number of characters in the string.
ggplot(data.frame(x = 1, y = 1:3, text = c("B", "BBBB", "BBBBBB")), 
       aes(x=x,y=y,label = text)) +
   geom_point() +
   geom_text(hjust = -2)

# So for that reason, I would not use the hjust method for publication quality graphs.




### Ver 2
# Draw a separate plot containing just the text,
# Then extract the plot panel (Lecture 12)
# Then add the plot panel to the original dot plot using annotation_custom.

# dot plot
# The original plot but with some space to the right for the annotation
p2 = p1 + theme(plot.margin = unit(c(1, 3.5, .5, .5), "lines"))

# The chart containing the text
p3 = ggplot(data, aes(x = 1, y = School, label = Prop)) +
     geom_text(size = 3) +
     theme_bw() +
     theme(panel.grid = element_blank(),
           panel.border = element_rect(colour = NA))																

p3

# Get the ggplot grob
g3 = ggplotGrob(p3)
g3$layout

# How to extract? Lecture 13
# Two ways: Filter or select the panel grob from the list of grobs

# Ver 2a - filter
g4 = gtable_filter(g3, "panel")
# But sometimes a little care is needed.
# g4 itself is a plot panel. It will have clipping turned on. 
# Best to turn this clipping off.
g4$layout
g4$layout$clip[g4$layout$name == "panel"] = "off"

grid.newpage()
grid.draw(g4)
g4 = editGrob(grid.force(g4), "text", grep=TRUE, hjust = 1)

# A variation of Ver 2a
# Select the panel from the layout.
# But again, g5 is a plot panel. Best to turn off the clipping

# Ver 2b
g5 = g3[6, 4]
# OR
pos = g3$layout[g3$layout$name == "panel", c("t", "l")]
g5 = g3[pos[[1]], pos[[2]]]
g5$layout$clip[g5$layout$name == "panel"] = "off"     

# Extract the pure grid grob.
# Pure grid grob - clipping is already turned off.
# Sometimes, I think it is preferable to extract the pure grid grob. 
# Recall: the order of the grobs in the list of grobs is the same as the order in the layout dataframe.

# Ver 2c
g6 = g3$grobs[[grep("panel", g3$layout$name)]]
g6 = editGrob(g6, "text", grep=TRUE, hjust = 1)
grid.newpage()
grid.draw(g6)

# Complete the plot
# I add the grob contain the text (g4, g5 or g6) to the original dot plot using annotation_custom.
# Recall: annotation_custom positions grobs using data coordinates.
# But unlike other grobs, the plot panel does not expand if I use coordinates outside the initial plot panel.
# That is, I give annotation_custom an x-coordinate that is just a little larger than the maximum x-coordinate.
# The y-coordinates are easy. I want the new grob to span the full height of the data plot panel;
# that is, "-Inf" to "Inf".

p7 = p2 + 
     annotation_custom(grob = g4, 
                xmin = 1.15*max(data$Prop), xmax = 1.15*max(data$Prop), 
                ymin = -Inf, ymax = Inf)    

# The annotation is of an annotation of the dot plot's panel,
# but it is positioned outside the dot plot's panel.
# Therefore, I need to turn off the clipping.
# Get the ggplot grob for the completed chart, and turn off clipping.
g7 <- ggplotGrob(p7)
g7$layout$clip[g7$layout$name == "panel"] = "off"

# Draw the chart
grid.newpage()
grid.draw(g7)


# The alignment is better. The numbers are centre justified.
# I've left an additional line of code in the supplementary material 
# that will allow different alignment. 
# It involved borrowing a bit deeper into the grob.
# I deal with this in later lectures, but I leave the code here for those in a rush. 
 
# A point to note: I'm forcing the annotation into a space of zero width.
# Thus the grob will spill outside the space available.
# And therefore, it is important to turn off clipping if using g4 (or g5) - 
# the grobs that are themselves plot panels. 
# I didn't have to turn off clipping.
# But then I would have had to select xmin and xmax values that would have allowed the grob to fit.
# It was easier, I think, to turn off the clipping.
# Arguably, easier still is to extract a pure grid grob from the grob list - g6.



## Ver 3
# Create the grobs on the run. 
# Another option is to position each string in its own annotation_custom.
# That is, each string is its own textGrob.
# Now it will be easy to justify the numbers. 
# I use the "just" slot in the textGrob function.
# There are five numbers; 
# thus five grobs to be created; 
# and five anotation_custom statements.
# Here, I use a loop.
# Each time through the loop, the y-coordinate is obtained from the data dataframe,
# and the x-coordinate is just a little larger than the maximum x-coordinate. 

p2 <- p1 + theme(plot.margin = unit(c(2, 5, 3, 1), "lines"))

for (i in 1:length(data$Prop))  {
p2 <- p2 + annotation_custom(
      grob = textGrob(label = data$Prop[i], just = "right", gp = gpar(cex = .75)),
      ymin = data$School[i],         # Vertical position of the textGrob
      ymax = data$School[i],
      xmin = 1.17*max(data$Prop),    # Note: The grobs are positioned outside the plot area
      xmax = 1.17*max(data$Prop))
   } 

# And another annotation_custom to position the label "P". 
p2 <- p2 +  annotation_custom(
             grob = textGrob(label = "P", just = "right", vjust = 2),  
             xmin = 1.16*max(data$Prop),   
             xmax = 1.16*max(data$Prop),
             ymin = Inf,
             ymax = Inf) 
   
# The annotations are not yet visible
p2

# Get the ggplot grob, turn off clipping on the plot panel
g2 <- ggplotGrob(p2)
g2$layout$clip[g2$layout$name == "panel"] = "off"

# draw the chart
grid.newpage()
grid.draw(g2)

###################
####### add grob to new column created using gtable_add_column
####### add grob with viewport

# Like ver 2, in that a second plot is drawn to contain just the text,
# and then extract the plot panel.
# But unlike ver 2, the panel is added to the original dot plot
# using gtable functions: gtable_add_col(), gtable_add_grob

# dot plot
# The original plot but with some space to the right for the annotation
p2 = p1 + theme(plot.margin = unit(c(1, 3.5, .5, .5), "lines"))

# The chart containing the text
p3 = ggplot(data, aes(x = 1, y = School, label = Prop)) +
     geom_text(size = 3) +
     theme_bw() +
     theme(panel.grid = element_blank(),
           panel.border = element_rect(colour = NA))																

p3

# Get the ggplot grob
g3 = ggplotGrob(p3)
g3$layout







### First ggplot edit
# Example 3
# Scenario
# I want to add an annotation on the horizontal axis,
# indicating the location of a "Critical" proportion.
# The annotation contains a dotted line across the plot,
# a tick mark at the critical value;
# text indicating the critical value (0.33); and
# text indicating that the value is critical ("Critical").

# I construct the grob (Lesson ??).
# I first construct the four grobs separately,
# then combine the four grobs in a gTree structure.
# I will use annotation_custom to position the grob, 
# but I will let annotation span the full height of the plot panel;
# that is, in the annotation_custom, ymax and ymin are "Inf" and "-Inf" respectively.
# I position the individual grobs when I create them using the default npc units:
# the two text grobs ("0.33", and "Critical") just below the bottom boundary,
# the tick mark spans the bottom boundary,
# and the line spans the plot panel.
# I don't worry about the x-coordinates when constructing the grobs;
# the x-coordinates will be taken care of with the placement of the annotation_custom. 

gtext1 = textGrob("0.33", y = -.055, gp = gpar(col = "red"))
gtext2 = textGrob("Critical", y = -.09, gp = gpar(col = "red"))
gtick = linesGrob(y = c(-.01, .01),  gp = gpar(col = "red", lwd = 3)) 
gsegment = linesGrob(y = c(0, 1), gp = gpar(col = "red", lty = 3, lwd = 1))

ggrob = gTree("ggrob", children = gList(gtext1, gtext2, gsegment, gtick))

# Position the grob using annotation_custom.
# The x-coordinate can now be set (in data coordinates).
# Note the y-coordinates (-Inf, and Inf).
p3 = p2 + annotation_custom(ggrob, xmin=0.33, xmax=0.33, ymin=-Inf, ymax=Inf) 

# Once again, elements are outside the plot panel:
# the dot for school C;
# the original annotations to the right of the plot; and,
# part of the new annotation.

g3 = ggplotGrob(p3)
g3$layout$clip[g3$layout$name=="panel"] <- "off"
grid.draw(g3)

# In this way, the placement of the grob in the horizontal direction is in terms of data coordinates,
# but the positioning in the vertical direction is in terms of relative (npc) coordinates.

# It's like we're able to mix unit systems.
# What's the advantage of doing it this way? 
# I could have used data coordinates to fix the y-coordinates of the grob,
# But if the range of y-values changes (a school is added, or a school is dropped),
# the grob would no longer be nicely positioned.
# Doing the positioning in relative coordinates 
# means that it doesn't matter if schools are added or dropped -
# the vertical positioning of the grob will exactly as it appears here.


# Complex grobs can be constructed, 
# and with careful selection of x and y coordinates, 
# and being careful about where the coordinates are set (in the grob construction or in the annotation_custom),
# A lot can be achieved with annotation_custom.

# But a note of caution - in a multi-panel plot (facet_grid or facet_wrap), 
# the annotation will be drawn in every panel.
# If you don't want that effect, a different approach is required. 
# Coming up in a later lesson. 





###############################################################
## Lecture 15: Adding grobs to rows and columns of the gtable
###############################################################

# In Lecture 14, grobs were added using annotation_custom() function.
# This meant that the grob was added to the plot panel
# By selecting coordinates beyond the bounds of the plot panel,
# the grobs could nevertheless be positioned outside the panel.
# Thus, the first gtable edit - turn off clipping of the plot panel.

# In this lecture - the grobs are positioned using gtable functions,
# sometimes adding rows and columns to receive the grobs.

# Example 1
# Colour the border
# A contrived example: A want the border to be coloured red.
# Draw graph
set.seed(64534)
p1 <- ggplot(data.frame(x = rnorm(100), y = rnorm(100), 
                       f1 = rep(c("A","B"), 50),
                       f2 = rep(c("C", "D"), each = 50)), 
            aes(x,y)) + geom_point() 
p1

# To get the ggplot gtable, use the ggplot2 function, ggplotGrob()
g1 <- ggplotGrob(p1)

gtable_show_layout(g1)
g1$layout

# Construct the grob - a red rectangle
rectR = rectGrob(gp = gpar(col = "red", fill = "red"))

# Add the rectabgle grob to the plot using 
# Recall: gtable_add_grob from Lecture ??
# Four borders, four statements.
# What differs in the four statments are the t, l, b, and r parameters.
g1 = gtable_add_grob(g1, rectR, t=1, l=1, b=1, r=5)
g1 = gtable_add_grob(g1, rectR, t=1, l=5, b=6, r=5)
g1 = gtable_add_grob(g1, rectR, t=6, l=1, b=6, r=5)
g1 = gtable_add_grob(g1, rectR, t=1, l=1, b=6, r=1)

grid.newpage()
grid.draw(g1)



# Example 2
## Inspired by http://stackoverflow.com/questions/28652284/how-to-change-color-of-facet-borders-when-using-facet-grid/28669873#28669873
# In a facetted plot, colour gaps between panels

# Get the plot
p <- ggplot(mtcars, aes(mpg, wt)) + geom_point() + 
     facet_grid(am ~ cyl)

## Get the plot grob
gt <- ggplotGrob(p)

# I could get the columns and rows from the layout
gtable_show_layout(gt)

# Vertical gaps are in columns 5 and 7
# and span rows 3 to 6.
# Horizontal gap is in row 5
# and spans columns 4 to 9

# Create an orange rectangle
rectG = rectGrob(gp = gpar(col = NA, fill = "orange"))

# Position the rectangle grob into the rows and columns.
# I could position the rectangles, one at a time, into the relevant columns and rows

gt = gtable_add_grob(gt, rectG, t = 3, l = 5, b = 6)  
gt = gtable_add_grob(gt, rectG, t = 3, l = 7, b = 6)
gt = gtable_add_grob(gt, rectG, t = 5, l = 4, r = 9)

grid.newpage()
grid.draw(gt)



# If there were lots of panels, this could get tedious.
# I could be a bit more efficient.
# I'll use a loop.

gt <- ggplotGrob(p)

## Add orange rectangles into the vertical gaps
for(i in c(5, 7)) 
   gt <- gtable_add_grob(gt, 
         list(rectGrob(gp = gpar(col = NA, fill = "orange"))), 
         t=3, l=i, b=6, r=i)

## Add orange rectangles into the horizontal gaps
for(j in c(5) )
   gt <- gtable_add_grob(gt, 
         list(rectGrob(gp = gpar(col = NA, fill = "orange"))), 
         t=j, l=4, b=j, r=9)

grid.newpage()
grid.draw(gt)


# But I still need to get the relevant row and column indices.

## To automate the selection of the relevant rows and columns:
# I begin with the panels.
# The panels are named in the layout dataframe, 
# and therefore I can get the t, l, b, and r values for each panel.
# See panels below

# I can get the columns that the horizontal gaps span:
# they span from the left-hand panel through to one more than than the right-hand panel.
# Why "one more"? I want the colour to extend into the gap between the strips.
# See Cmin and Cmax below

# Similarly, I can get the rows that the vertical gaps span:
# they span from one row above the top panel through to the bottom panel.
# Why "one row above"? Again, I want the colour to extend into the gap between the strips.
# See Rmin and Rmax below

# The vertical gaps are one column to the right of the panels' r index (except the right most panel)
# See gapsC below

# The horizontal gap are one below the panels' b index (except the bottom most panel)
# See gapsR below

## Get the plot and the plot grob
p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + 
     facet_grid(gear ~ cyl)
gt <- ggplotGrob(p)

# From the layout dataframe, use the subset() function to select t to r columns for "panels" only
panels = subset(gt$layout, grepl("panel", gt$layout$name), t:r)

# Vertical gaps span columns:
Cmin <- min(panels[,"l"])
Cmax <- max(panels[,"r"]) + 1 #(+1 because I want the colour to extend into the gap between the strips

# Horizontal gaps span rows:
Rmin <- min(panels[,"t"]) - 1 # (-1 because I want the colour to extend into the gap between the strips
Rmax <- max(panels[,"b"])

panelsC <- unique(panels[, "r"])
#Vertical gaps are in:
gapC <- panelsC[-length(panelsC)] + 1  # gaps are one column to the right of the panels, except the right most panel

panelsR <- unique(panels[, "b"])
# Horizontal gaps are it:
gapR <- panelsR[-length(panelsR)] + 1  # gaps are one row below the panels, except the bottom most panel.


## Add orange rectangles into the vertical and horizontal gaps
# for(i in gapC) 
#   gt <- gtable_add_grob(gt, 
#         rectGrob(gp = gpar(col = NA, fill = "burlywood")), 
#         Rmin, i, Rmax, i)

# for(j in gapR) 
#   gt <- gtable_add_grob(gt, 
#         rectGrob(gp = gpar(col = NA, fill = "burlywood")), 
#         j, Cmin, j, Cmax)

## Draw it
# grid.newpage()
# grid.draw(gt)


# A function to position the rectangle into the gtable
ColourGap = function(t, l, b, r, g, fill = "grey98") {
   gtable_add_grob(g, 
   rectGrob(gp = gpar(col = NA, fill = fill)), 
   t, l, b, r)
}

# The colour of the gaps
c = "burlywood"

# A loop to colour each vertical gap
for(i in gapC) 
   gt = ColourGap(Rmin, i, Rmax, i, gt, c)

# A loop to colour each horizontal gap
for(j in gapR) 
   gt = ColourGap(j, Cmin, j, Cmax, gt, c)

# Draw the chart
grid.newpage()
grid.draw(gt)



## Example 3
# Continuing with the previous example:
# The labels in the strips are not terrifically informative.
# I know I can add the variable name to the strips quite easily using regular ggplot commands.
# But inspired by these stackoverflow questions:
# http://stackoverflow.com/questions/11353287/how-do-you-add-a-general-label-to-facets-in-ggplot2/12660057#12660057
# http://stackoverflow.com/questions/22818061/annotating-facet-title-as-strip-over-facet/22825447#22825447# 
# http://stackoverflow.com/questions/29311772/ggplot2-more-complex-faceting/29323739#29323739

# It is straightforward to place the variable name in its own strip

# Take the  plot created in the previous example.
gt1 = gt
grid.newpage()
grid.draw(gt1)


# The strips that contain 4, 6, and 8 represent the number of cylinders,
# and the strips that contain 3, 4, and 5 represent the number of forward gears.
# I want to create another strip that spans across the three strips at the top but above them,
# to contain the text "Number of Cylinders".
# Similarly, I want a strip to the right of the vertical strips 
# to contain the text "Number of "Gears". 

#  The layout data frame contains the t, l, b and R values for the two sets of strips.
# From the layout dataframe, use the subset() function to select t to r columns for "panels" only

gtable_show_layout(gt1)

# The upper horizontal strip is in row 3, and span across columns 4, 6 and 8.
# The right vertical strip is in column 9, and spans down rows 4, 6 and 8.

# Thus I add a new row above row 3,
# except I have to add a row BELOW a position - row 2.
# And, I add a new column to the right of column 9

gt1 = gtable_add_rows(gt1, unit(0.6, "cm"), pos = 2)
gt1 = gtable_add_cols(gt1, unit(0.6, "cm"), pos = 9)


HStrip = gTree("HStrip", children = gList(
   #rectGrob(gp = gpar(fill = "grey85", col = NA)),
   textGrob(label = "Number of Cylinders", gp = gpar(cex = .9))))

gt1 = gtable_add_grob(gt1, HStrip, t=3, l=4, r=8)

VStrip = gTree("VStrip", children = gList(
   #rectGrob(gp = gpar(fill = "grey85", col = NA)),
   textGrob(label = "Number of Gears", rot = -90, gp = gpar(cex = .9))))

gt1 = gtable_add_grob(gt1, VStrip, t=5, l=10, b=9)


# Add small gap between strips
gt1 <- gtable_add_rows(gt1, unit(2/10, "line"), 3)
gt1 <- gtable_add_cols(gt1, unit(2/10, "line"), 9)

grid.newpage()
grid.draw(gt1)


# Get the positions of the right strips in the layout: t = top, l = left, ...
stripR <-c(subset(gt$layout, name == "strip-right", select = t:r))






# Example 4: Returning to the school plot of Lecture 14
## In Lecture 14, a minor problem with the school plot:
# if the number of schools changes, 
# or the proportions change,
# positioning the annotation according to data units could be a problem. 
# Remember, the annotation is positioned as a multiple of the maximum.
# If, in a modified dataframe, the maximum is larger than in the original,
# the annotation will be place further from the right boundary.
# In what follows, I use lessons from Lecture ??:
# Add a column to the ggplot layout;
# Then position the grob into the new column.

# A solution
# Get the original plot
data <- data.frame(School = c("A", "B", "C", "D", "E"),
                  Prop = c(0.27, 0.36, 0, 0.15, 0.29))
data$School <- factor(data$School, levels = rev(data$School))

p1 <- ggplot(data , aes(Prop, School)) +
     geom_segment(aes(xend = 0, yend = School), linetype = 3, size = .5) +
     geom_point(size = 3) +
     scale_x_continuous("Number of Students", limits = c(0, 1.1*max(data$Prop)), expand = c(0,0)) +
     theme_bw() +
     theme(panel.grid = element_blank(),
           panel.border = element_rect(colour = "black"))

p1

# Get the ggplot grob and check the layout dataframe.
# Note: Clip is on for the panel
g1 <- ggplotGrob(p1)
g1$layout

# Get the chart containing the text
p3 = ggplot(data, aes(x = 1, y = School, label = Prop)) +
     geom_text(size = 3) +
     theme_bw() +
     theme(panel.grid = element_blank(),
           panel.border = element_rect(colour = NA))																

# Get the ggplot grob and extract the panel.
g3 = ggplotGrob(p3)
g6 = g3$grobs[[grep("panel", g3$layout$name)]]

# Add a column to g1 ready to take the extracted panel
# Recall: gtable_add_cols from Lecture ??
g1 = gtable_add_cols(g1, width = unit(3, "lines"), pos = 4)

# Add the panel (g6) to the new column
g1 = gtable_add_grob(g1, g6, t = 3, l = 5, z = .5)

# Draw the plot
grid.newpage()
grid.draw(g1)


# Drawing the p3 plot to extract the panel - a bit tedious having the draw the plot.
# Also, difficult to change the justification of the text.
# This method creates and positions the text grobs in the new column,
# and because it creates each text grob separately, it is easy to modify the justification.

# data
data <- data.frame(School = c("A", "B", "C", "D", "E"),
                  Prop = c(0.27, 0.36, 0, 0.15, 0.29))
data$School <- factor(data$School, levels = rev(data$School))

# original plot
p1 <- ggplot(data , aes(Prop, School)) +
     geom_segment(aes(xend = 0, yend = School), linetype = 3, size = .5) +
     geom_point(size = 3) +
     scale_x_continuous("Proportion of Students", limits = c(0, 1.1*max(data$Prop)), expand = c(0,0)) +
     theme_bw() +
     theme(panel.grid = element_blank(),
           panel.border = element_rect(colour = "black"))

# Add columns to ggplot gtable
g1 <- ggplotGrob(p1)
g1 = gtable_add_cols(g1, width = unit(4, "lines"), pos = 4)

# New
# How to position the text grobs in the new column?
# Grid does not know about the discrete scale used on the y-axis.
# But ggplot does know about npc units - the unit system that ranges between 0 and 1.
# I can obtain the y positions in npc units by calling ggplot_build().
# ggplot_build contains the information that ggplot uses to construct the chart.
tab = ggplot_build(p1)
y = tab$layout$panel_ranges[[1]]$y.major
y

# Position the five text grobs into the new column
for (i in 1:length(data$Prop)) {
     grob = textGrob(label = data$Prop[i], x = .3, y = rev(y)[i], just = "left", gp = gpar(cex = .75))
     g1 = gtable_add_grob(g1, grob, t = 6, l = 5, name = as.character(i))   
}

g1 = gtable_add_grob(g1, textGrob("Prop", x = .3, y = 1, vjust = 2, just = "left",), t = 6, l = 5, name = "label")

# Still need to turn off clipping to take care of zero dot for School C
g1$layout$clip[g1$layout$name=="panel"] <- "off"

grid.newpage()
grid.draw(g1)


# Change the data
data <- data.frame(School = c("A", "B", "C", "D", "E", "F"),
                  Prop = c(0.27, 0.36, 0, 0.15, 0.29, .5))
data$School <- factor(data$School, levels = rev(data$School))




# Adding text
# http://stackoverflow.com/questions/21997715/add-ggplot-annotation-outside-the-panel-or-two-titles
p = qplot(1,1) 
g = ggplotGrob(p)

g = gtable_add_grob(g, grobTree(textGrob("left", x=0, hjust=0), 
                                textGrob("right", x=1, hjust=1)), 
                    t=1, l=4)




# http://stackoverflow.com/questions/18033568/gtable-add-grob-linesgrob-not-displayed
# gtable_add_grobs is vectorised

g <- gtable(widths = unit(c(1,1), "null"),
            heights = unit(c(1,1), "null"))


cell <- function(ii)
  grobTree(rectGrob(), 
           linesGrob(1:4, 1:4, default.units="native"), 
           textGrob(ii),
           vp=dataViewport(c(1,4), c(1,4)))

gl <- lapply(1:4, cell)

xy <- expand.grid(1:2, 1:2)

g <- gtable_add_grob(g, gl, 
                      l=xy[,1],
                      r=xy[,1],
                      t=xy[,2],
                      b=xy[,2])

grid.newpage()
grid.draw(g)





## Adding plot to vacant panel in facet_wrap
plot1 <- ggplot(mtcars, aes(factor(mtcars$gear))) + 
    geom_bar() + 
    facet_wrap(~cyl, ncol=2) + 
    coord_flip()

plot2 <- ggplot(mtcars, aes(as.factor(am))) + 
    geom_bar(aes(fill=factor(cyl)))

g1 <- ggplotGrob(plot1)
g2 <- ggplotGrob(plot2)

g1 <- gtable::gtable_add_grob(g1, g2, t = 8, l=7)
grid.newpage()
grid.draw(g1)



## adding plot to margins
# http://stackoverflow.com/questions/31587088/perfect-fit-of-ggplot2-plot-in-plot/31644681#31644681
library(rms)

# Data
data(pbc)
d <- pbc
rm(pbc)
d$status <- ifelse(d$status != 0, 1, 0)

dd = datadist(d)
options(datadist='dd')

f <- cph(Surv(time, status) ~  rcs(age, 4), data=d)
p <- Predict(f, fun=exp)
df <- data.frame(age=p$age, yhat=p$yhat, lower=p$lower, upper=p$upper)

# X-axis
breaks <- boxplot.stats(p[,"age"])$stats

# Main plot
MP <- ggplot(data=df, aes(x=age, y=yhat)) + geom_line(size=1) +
     geom_ribbon(data=df, aes(ymin=lower, ymax=upper), alpha=0.5, linetype=0, fill='#FFC000') +
     theme_bw() +
     scale_x_continuous(breaks=breaks) +
     xlab("Age") +
     ylab("Hazard Ratio") +
     theme(axis.line = element_line(color='black', size=1),
       axis.ticks = element_line(color='black', size=1),
       panel.grid.minor = element_blank())

# Boxplot
BP <- ggplot(data=df, aes(x=factor(1), y=age)) + 
      geom_boxplot(width = 1, outlier.shape=NA, size=1) + 
      geom_jitter(position = position_jitter(width = .2), size = 1) +
      scale_y_continuous(breaks=breaks) +
      coord_flip()  + 
      theme_bw() +
      theme(panel.border=element_blank(),
            panel.grid=element_blank())

####  Set up the grobs and gtables  here
library(gtable)

h = 1/10  # height of boxplot panel relative to main plot panel

# Get ggplot grobs
gMP = ggplotGrob(MP)
BPg = ggplotGrob(BP)
BPg = gtable_filter(BPg, "panel")  # from the boxplot, extract the panel only

# In the main plot, get position of panel in the layout
pos = gMP$layout[gMP$layout$name == "panel", c('t', 'l')]

# In main plot, set height for boxplot
gMP$heights[pos$t-1] = list(unit(h,  "null"))

# Add boxplot to main plot
gMP = gtable_add_grob(gMP, BPg, t=pos$t-1, l=pos$l)

# Draw it 
grid.newpage()
grid.draw(gMP)



######################
### http://stackoverflow.com/questions/17492230/how-to-place-grobs-with-annotation-custom-at-precise-areas-of-the-plot-region/17493256#17493256
library(gtable)
library(ggplot2)
library(plyr)

set.seed(1)
d <- data.frame(x=rep(1:10, 5),
                y=rnorm(50),
                g = gl(5,10))

# example plot
p <- ggplot(d, aes(x,y,colour=g)) +
  geom_line() +
  scale_x_continuous(expand=c(0,0))+
  theme(legend.position="top",
        plot.margin=unit(c(1,0,0,0),"line"))

# dummy data for the legend plot
# built with the same y axis (same limits, same expand factor)
d2 <- ddply(d, "g", summarise, x=0, y=y[length(y)])
d2$lab <- paste0("line #", seq_len(nrow(d2)))

plegend <- ggplot(d, aes(x,y, colour=g)) +
  geom_blank() +
  geom_segment(data=d2, aes(x=2, xend=0, y=y, yend=y), 
               arrow=arrow(length=unit(2,"mm"), type="closed")) +
  geom_text(data=d2, aes(x=2.5,label=lab), hjust=0) +
  scale_x_continuous(expand=c(0,0)) +
  guides(colour="none")+
  theme_minimal() + theme(line=element_blank(),
                          text=element_blank(),
                          panel.background=element_rect(fill="grey95", linetype=2))

# extract the panel only, we don't need the rest
gl <- gtable_filter(ggplotGrob(plegend), "panel")

# add a cell next to the main plot panel, and insert gl there
g <- ggplotGrob(p)
index <- subset(g$layout, name == "panel")
g <- gtable_add_cols(g, unit(1, "strwidth", "line # 1") + unit(1, "cm"))
g <- gtable_add_grob(g, gl, t = index$t, l=ncol(g), 
                     b=index$b, r=ncol(g))
grid.newpage()
grid.draw(g)



## adding plot to main plot panel








###############################################################
## Lecture ??: A closer look ggplot gtable
###############################################################

# Locating plot title

# Locating axis labels

# Locating facet strips

# Locating legends

# Locating axes

# Locating the plot panel





# Load packages
library(ggplot2)
library(gtable)

# Draw graph
set.seed(64534)
p1 <- ggplot(data.frame(x = rnorm(100), y = rnorm(100), 
                       f1 = rep(c("A","B"), 50),
                       f2 = rep(c("C", "D"), each = 50)), 
            aes(x,y)) + 
        geom_point() +
        ggtitle("A Scatterplot")
p1

# To get the ggplot gtable, use the ggplot2 function, ggplotGrob()
g1 <- ggplotGrob(p1)



gtitle1 = g1$grobs[[grep("title", g1$layout$name)]]
gtitle2 = gtable_filter(g1, "title")

str(gtitle1)

grid.newpage()
grid.draw(gtitle1)

ggtitle1 = grid.grab()
grid.ls()
