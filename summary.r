library(shiny)
library(png)
library(lattice)
library(MASS)

ui <- fluidPage(
  plotOutput("score", width="100%"),
  plotOutput("totalTimeHist", width="50%"),
  plotOutput("questionTotal", width="50%"),
  plotOutput("questionTime", width="50%"),
  plotOutput("touchRate", width="50%")
)

server <- function(input, output) {
  #loop checks how many data files exist
  totalFiles <- 0
  for (i in 1:9){
    fileName <- paste("data\\data",i,".csv", sep="")
    if (!file.exists(fileName)){break}
    else {totalFiles <- i}
  }
  #loop produces timelines for each data file
  
  #histograms of summary file columns
  fulldf <- read.csv("data\\summary.csv")
  output$score <- renderPlot(hist(fulldf$score, main="Score Ratings", xlab="Score", xlim=c(0,2), col="red"))
  output$totalTimeHist <- renderPlot(hist(fulldf$totalTime, main="Total Time Using the App", xlab="Minutes", xlim=c(0,2), col="red"))
  output$questionTotal <- renderPlot(hist(fulldf$questionTotal, main="Total Questions Answered", xlab="Number of Questions", xlim=c(0,10), col="red"))
  output$questionTime <- renderPlot(hist(fulldf$questionTime, main="Average Time to Answer a Question", xlab="Seconds", xlim=c(10,20), col="red"))
  output$touchRate <- renderPlot(hist(fulldf$touchRate, main="Average Touches per Minute", xlab="# of Touches/Minute", xlim=c(20,40), col="red"))
  
}

shinyApp(ui = ui, server = server)