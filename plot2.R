library(ggplot2)
library(zoo)
library(gtable)
library(gridExtra)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

plotStuff <- function(myfile, path) {

rhythm <- paste(myfile, "rhythmStat.csv", sep="")
gestures <- paste(myfile, "gestures.csv", sep="")

df3 <- read.table(rhythm, header=T,sep=",")
#data3 <- df3[!apply(df3, 1, function(x) {any(x == 4000)}),]
data3 <- df3
df4 <- read.table(gestures, header=T,sep=",")
data4 <- df4

p1 <- ggplot(data3, aes(time/(1000*60))) + geom_line(aes(y=meanWnbd, color= c("meanWnbd"))) + scale_x_continuous(name="time in minutes",limits=c(0, 20)) + opts(title = myfile) 
p2 <- ggplot(data4, aes(time/(1000*60))) + geom_line(aes(y=lenMilli/1000.0, color= c("g-dur. (s)"))) + scale_x_continuous(name="time in minutes", limits=c(0, 20))

 
pdf(path)
#multiplot(p1,p2)
gA <- ggplot_gtable(ggplot_build(p1))
gB <- ggplot_gtable(ggplot_build(p2))

# The parts that differs in width
leg1 <- with(gA$grobs[[8]], grobs[[1]]$widths[[4]])
leg2 <- with(gB$grobs[[8]], grobs[[1]]$widths[[4]])

# Set the widths
gA$widths <- gB$widths

# Add an empty column of "abs(diff(widths)) mm" width on the right of 
# legend box for gA (the smaller legend box)
gA$grobs[[8]] <- gtable_add_cols(gA$grobs[[8]], unit(abs(diff(c(leg1, leg2))), "mm"))

# Arrange the two charts
grid.newpage()
grid.arrange(gA, gB, nrow = 2)
dev.off()
}
myfile <- commandArgs()[6]
path <- commandArgs()[7]

print(commandArgs())
plotStuff(myfile, path)

#plotStuff("/home/lodsb/Dokumente/development/intellij_projects/evaltools/logs21/logs21", "/tmp/trashplot666.pdf")
