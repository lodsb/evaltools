library(ggplot2)
library(zoo)

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
metro <- paste(myfile, "metronome.csv", sep="")

df3 <- read.table(rhythm, header=T,sep=",")
#data3 <- df3[!apply(df3, 1, function(x) {any(x == 4000)}),]
data3 <- df3
df4 <- read.table(metro, header=T,sep=",")
data4 <- df4

p1 <- ggplot(data3, aes(time/(1000*60))) + geom_line(aes(y=10*meanWnbd, color= c("meanWnbd"))) + geom_line(aes(y=meanOdd, colour="meanOdd")) + geom_line(aes(y=length/1, colour="length")) + scale_x_continuous(name="time in minutes",limits=c(0, 20)) +  scale_y_continuous(limits=c(0, 100)) + opts(title = myfile)
p2 <- ggplot(data4, aes(time/(1000*60))) + geom_line(aes(y=dur, color= c("duration"))) + scale_x_continuous(name="time in minutes", limits=c(0, 20))
pdf(path)
multiplot(p1,p2)
dev.off()
}
myfile <- commandArgs()[6]
path <- commandArgs()[7]

print(commandArgs())
plotStuff(myfile, path)

#plotStuff("/home/lodsb/Dokumente/development/intellij_projects/evaltools/logs21/logs21", "/tmp/trashplot666.pdf")