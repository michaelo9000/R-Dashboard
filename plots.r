library(shiny)
library(png)
library(lattice)
library(MASS)

ui <- fluidPage(
  sliderInput(inputId = "dataSelect", 
              label = "Select the data file to analyse", 
              min = 1, max = 60, value = 1, step = 1, width = "100%"),
  
  textOutput("reactiveLabel"),
  
  fluidRow(
    column(2, textOutput("totalTouches")),
    column(2, textOutput("totalTime")),
    column(2, textOutput("touchRate")),
    column(2, textOutput("totalQuestions")),
    column(2, textOutput("questionRate")),
    column(2, textOutput("score"))
  ),
  plotOutput("timeline", height="150px"),
  fluidRow(
    column(6, plotOutput("basicScatter"),
           plotOutput("coloredScatter")),
    column(6, plotOutput("density"),
           plotOutput("groupedScatters"))
  )
)

server <- function(input, output) {
  observeEvent(input$dataSelect, {
    fileName <- paste("data\\data", input$dataSelect, ".csv", sep="")
    if(file.exists(fileName)){
      df <- read.csv(fileName)
      output$reactiveLabel <- renderText(paste("Visualising file: data\\data", input$dataSelect, ".csv, for participant #",input$dataSelect, sep=""))
      #descriptive values
      questionTime <- max(df$touchEnd)/(max(df$question)-1)/1000
      output$totalTime <- renderText(paste("Total Time (min):", round(max(df$touchEnd)/60000, digits=2)))
      output$touchRate <- renderText(paste("Touches per Minute:",round((length(df$touchX)/max(df$touchEnd))*60000, digits=2)))
      output$totalTouches <- renderText(paste("Total Touches:", length(df$touchX)))
      output$totalQuestions <- renderText(paste("Questions Answered:",max(df$question)-1))
      output$questionRate <- renderText(paste("Average Time per Question (s):",round(questionTime, digits=2)))
      output$score <- renderText(paste("Time-Weighted Score:",round(((1/questionTime)*((sum(df$touchType == 1))/(sum(df$touchType == 0))))*100, digits=2)))
      #plots
      output$basicScatter <- renderPlot ({plot(0:4000, xlim=c(0,4000), ylim=c(0,4000), type="n", ylab="Y", xlab="X")
        img <- readPNG("start.png")
        lim <- par()
        rasterImage(img, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
        points(df$touchX, df$touchY, col=rainbow(1,alpha=.5), pch=16, cex=3)})
      output$coloredScatter <- renderPlot(xyplot(touchY ~ touchX, df, main="Touches colored by Question", groups = df$question,
                                                 pch=16, cex=3, col=rainbow(10,alpha=.5), xlim=c(0,4000), ylim=c(0,4000)))
      output$groupedScatters <- renderPlot(xyplot(touchY ~ touchX | question, df, main="Touches grouped by Question",
                                                  pch= 16, cex=2, col=rainbow(1,alpha=.5), xlim=c(0,4000), ylim=c(0,4000)))
      dens <- kde2d(df$touchX, df$touchY, h=500, n=25, lims = c(0,4000,0,4000))
      output$density <- renderPlot({dens <- kde2d(df$touchX, df$touchY, h=500, n=25, lims = c(0,4000,0,4000))
      filled.contour(dens)})
      output$timeline <- renderPlot ({df$question <- as.factor(df$question)
      plot(c(df$touchStart[1], tail(df$touchEnd,1)), c(0.4,0.65), 
           type='n', main="Timeline of Touches colored by Question#", xlab='Time (ms)', ylab='', axes=FALSE)	 
      for(i in 1:nrow(df)){
        polygon(x=c(rep(df$touchStart[i],2), rep(df$touchEnd[i],2)), 
                y=c(0.6,0.4,0.4,0.6), col=rainbow(10,alpha=0.5)[df$question[i]],
                border=rainbow(10)[df$question[i]])
      }
      axis(1, labels=df$touchEnd, at=df$touchEnd)})
    }
    else{output$reactiveLabel <- renderText("file does not exist")}
  })
}

shinyApp(ui = ui, server = server)