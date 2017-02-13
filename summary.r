library(shiny)
library(png)
library(lattice)
library(MASS)

ui <- fluidPage(
  #barplot options
  selectInput("dataSelect", "Select the parameter to plot", 
               choices=c("Score"=1, 
                         "Total Time"=2, 
                         "Total Touches"=3, 
                         "Rate of Touches"=4, 
                         "Questions Answered"=5, 
                         "Question Time"=6)),
  #barplot
  plotOutput("barplot"),
  #display timelines
  uiOutput("plots")
)

server <- function(input, output) {
  #loop checks how many data files exist
  for (i in 1:99){
    fileName <- paste("data\\data",i,".csv", sep="")
    if (!file.exists(fileName)){break}
    else {totalFiles <- i}
  }
  #something
  output$plots <- renderUI({
    plot_output_list <- lapply(1:totalFiles, function(i) {
      plotName <- paste("timeline", i, sep="")
      plotOutput(plotName, width="100%", height="150px")
    })
    do.call(tagList, plot_output_list)
  })
  #loop produces timelines for each data file
  fulldf <- read.csv("data\\summary.csv")
  for (i in 1:totalFiles){
    local({
      localI <- i
      fileName <- paste("data\\data",localI,".csv", sep="")
      df <- read.csv(fileName)
      score <- round(fulldf$score[localI], digits=2)
      plotName <- paste("timeline",localI,sep="")
      output[[plotName]] <- renderPlot ({df$question <- as.factor(df$question)
        plot(c(df$touchStart[1], tail(df$touchEnd,1)), c(0.4,0.65), 
           type='n', xlab=paste("File: data",localI,".csv. Time-weighted Score: ",score,sep=""), ylab='', axes=FALSE)	 
        for(n in 1:nrow(df)){
          polygon(x=c(rep(df$touchStart[n],2), rep(df$touchEnd[n],2)), 
                  y=c(0.6,0.4,0.4,0.6), col=rainbow(10,alpha=0.5)[df$question[n]],
                  border=rainbow(10)[df$question[n]])
        }
        axis(1, labels=df$touchEnd, at=df$touchEnd)
      })
    })
  }
  #histograms of summary file columns
  observeEvent(input$dataSelect,{
    if (input$dataSelect == 1){data <- fulldf$score}
    else if (input$dataSelect == 2){data <- fulldf$totalTime}
    else if (input$dataSelect == 3){data <- fulldf$totalTouches}
    else if (input$dataSelect == 4){data <- fulldf$touchRate}
    else if (input$dataSelect == 5){data <- fulldf$questionTotal}
    else {data <- fulldf$questionTime}
    output$barplot <- renderPlot(barplot(data, names.arg=c(1:totalFiles), main="", xlab="Participant #", col=rainbow(10)))
  })
}

shinyApp(ui = ui, server = server)