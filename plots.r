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
    column(3, textOutput("touchRate")),
    column(3, textOutput("totalQuestions")),
    column(3, textOutput("questionRate")),
    column(3, textOutput("touchesPerQ"))
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
      output$reactiveLabel <- renderText(paste("visualising file: data\\data", input$dataSelect, ".csv", sep=""))
      #descriptive values
      output$touchRate <- renderText(paste("touches per minute:",(length(df$touchX)/max(df$touchEnd))*60000))
      output$totalQuestions <- renderText(paste("questions attempted:",max(df$question)))
      output$questionRate <- renderText(paste("average time per question (s):",max(df$touchEnd)/max(df$question)/1000))
      output$touchesPerQ <- renderText(paste("average attempts per question:",(length(df$touchX)-max(df$question))/(2*max(df$question))))
      #plots
      output$basicScatter <- renderPlot ({plot(0:4000, xlim=c(0,4000), ylim=c(0,4000), type="n", ylab="Y", xlab="X")
          img <- readPNG("appscreen.png")
          lim <- par()
          rasterImage(img, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
          points(df$touchX, df$touchY, col=rainbow(1,alpha=.5), pch=16)})
      output$coloredScatter <- renderPlot(xyplot(touchY ~ touchX, df, groups = df$question,
          pch=16, col=rainbow(10,alpha=.5), xlim=c(0,4000), ylim=c(0,4000)))
      output$groupedScatters <- renderPlot(xyplot(touchY ~ touchX | question, df, 
          pch= 16, col=rainbow(1,alpha=.5), xlim=c(0,4000), ylim=c(0,4000)))
      dens <- kde2d(df$touchX, df$touchY, h=500, n=25, lims = c(0,4000,0,4000))
      output$density <- renderPlot({dens <- kde2d(df$touchX, df$touchY, h=500, n=25, lims = c(0,4000,0,4000))
          filled.contour(dens)})
      output$timeline <- renderPlot ({df$question <- as.factor(df$question)
          plot(c(df$touchStart[1], tail(df$touchEnd,1)), c(0.4,0.65), 
              type='n', xlab='Time (ms)', ylab='', axes=FALSE)	 
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