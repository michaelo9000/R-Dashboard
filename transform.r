library(shiny)

ui <- fluidPage(
  fileInput(inputId = "upload", label = "select data files", multiple = TRUE),
  tableOutput("dataTable"),
  actionButton(inputId = "transform", label = "Transform this data")
  
)

server <- function(input, output) {
  output$dataTable <- renderTable({input$upload})
  observeEvent(input$transform, {
    ifelse(!dir.exists(file.path("data")), dir.create(file.path("data")), FALSE)
    dfDF <- input$upload
    for (n in 1:(length(input$upload)-1)){
      text = readLines(dfDF$datapath[n])
      text <- tail(text, length(text)-4)
      text <- head(text, length(text)-2)
      drop <- grepl("key", text)
      text <- text[!drop]
      drop <- grepl("touchU", text)
      text <- text[!drop]
      drop <- grepl("touchM", text)
      text <- text[!drop]
      text <- sub("touchDown 0 ", "", text)
      text <- strsplit(text, " ")
      
      sleepCur <- 0
      sleepPrev <- 0
      question <- 1
      df <- data.frame()
      for (i in 1:length(text)){
        sleepTest <- grepl("sleep", text[i])
        if(sleepTest == TRUE){
          sleepAdd <- as.numeric(gsub("\\D", "", text[i]))
          sleepCur <- sleepCur + sleepAdd
        } else {
          coords <- unlist(text[i])
          touchX <- as.numeric(coords[1])
          touchY <- 4000-as.numeric(coords[2])
          if (touchY < 0){touchY <- 0}
          if (touchX > 3250 && touchY > 3250){question <- question + 1}
          df <- rbind(df, c(sleepPrev, sleepCur, touchX, touchY, question))
          sleepPrev <- sleepCur
        }
      }
      colnames(df) <- c("touchStart", "touchEnd", "touchX", "touchY", "question")
      newFile <- paste("data\\data", n, ".csv", sep="")
      write.csv(df, file=newFile, row.names=F)
    }
  })
}

shinyApp(ui = ui, server = server)