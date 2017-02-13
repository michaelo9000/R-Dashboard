library(shiny)
library(png)
library(lattice)
library(MASS)

ui <- fluidPage(
  plotOutput("basicScatter", height="800px"),
  plotOutput("coloredScatter", height="800px"),
  plotOutput("groupedScatters")
)

server <- function(input, output) {
  maindf <- data.frame()
  #loop populates large table for all files that exist
  for (i in 1:99){
    fileName <- paste("data\\data",i,".csv", sep="")
    if (!file.exists(fileName)){break}
    else {
      #add [i] table to the main table
      loopdf <- read.csv(fileName)
      maindf <- rbind(maindf,loopdf)
    }
  }
  #single color scatter with screen image background
  output$basicScatter <- renderPlot ({plot(0:4000, xlim=c(0,4000), ylim=c(0,4000), type="n", ylab="Y", xlab="X")
    img <- readPNG("appscreen.png")
    lim <- par()
    rasterImage(img, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
    points(maindf$touchX, maindf$touchY, col=rainbow(1,alpha=.5), pch=16, cex=3)})
  #scatter colored according to question number
  output$coloredScatter <- renderPlot(xyplot(touchY ~ touchX, maindf, groups = maindf$question,
                                             pch=16, cex=3, col=rainbow(10,alpha=.5), xlim=c(0,4000), ylim=c(0,4000)))
  #scatters grouped according to question number
  output$groupedScatters <- renderPlot(xyplot(touchY ~ touchX | question, maindf, groups = maindf$question,
                                              pch= 16, cex=3, col=rainbow(4,alpha=.5), xlim=c(0,4000), ylim=c(0,4000)))
}

shinyApp(ui = ui, server = server)