library(shiny)

ui <- fluidPage(
  #upload, table, transform button and alert label objects
  fileInput(inputId = "upload", label = "The app will transform and name data files in the 
            order you enter them. Please order your files as necessary when you upload them.", 
            multiple = TRUE),
  textOutput("alert"),
  tableOutput("dataTable"),
  actionButton(inputId = "transform", label = "Transform this data")
  
)

server <- function(input, output) {
  output$alert <- renderText("")
  #draw file details table
  output$dataTable <- renderTable({input$upload})
  #transform method, activated by transform button
  observeEvent(input$transform, {
    #check folder exists and if not, create it
    ifelse(!dir.exists(file.path("data")), dir.create(file.path("data")), FALSE)
    #set dataframes for file details table and summary table
    dfDF <- input$upload
    fulldf <- data.frame()
    #loop through all uploaded files
    for (n in 1:(length(dfDF)-1)){
      endPress <- 0
      text = readLines(dfDF$datapath[n])
      #cut superfluous rows
      text <- tail(text, length(text)-4)
      text <- head(text, length(text)-2)
      #remove rows containing strings "touchUp" and "touchMove" and variants of "key"
      drop <- grepl("Up", text)
      text <- text[!drop]
      drop <- grepl("Move", text)
      text <- text[!drop]
      #remove string "touchDown 0" from remaining rows
      text <- sub("touchDown 0 ", "", text)
      text <- sub("keyDown ", "", text)
      #divide into sections according to spaces in the text
      text <- strsplit(text, " ")
      #set time and question variables
      sleepCur <- 0
      sleepPrev <- 0
      question <- 1
      #create dataframe for this log file
      df <- data.frame()
      #loop through all rows
      for (i in 1:length(text)){
        #set test variables for sleep rows and home/back button rows
        sleepTest <- grepl("sleep", text[i])
        hbTest <- grepl("K_", text[i])
        #if row is contains time information, extract it
        if(sleepTest == TRUE){
          #pull only numeric values from row
          sleepAdd <- as.numeric(gsub("\\D", "", text[i]))
          #continually sum all time values
          sleepCur <- sleepCur + sleepAdd
        }
        #if row indicates a home or back button press
        else if (hbTest == TRUE){
          #If more than 30 seconds have elapsed and the user presses Home or Back more than once, end
          if (sleepCur > 30000 && endPress > 0){
            break;
          }
          else if (sleepCur > 30000){
            endPress = endPress + 1
          }
        }
        #otherwise the row contains touch information
        else {
          #set variable as a single string of the row, not a list
          coords <- unlist(text[i])
          #set coordinate values to numeric type
          touchX <- as.numeric(coords[1])
          #reverse the Y axis as the log captures it upside down
          touchY <- 4000-as.numeric(coords[2])
          #control for negative Y values as a result of the previous step
          if (touchY < 0){touchY <- 0}
          #find 'response' touches
          if (touchX > 800 && touchX < 3200 && touchY > 600 && touchY < 1500){touchType = 0}
          #find 'start' button touch
          else if (touchX > 1400 && touchX < 2600 && touchY > 2300 && touchY < 3000){touchType = 3}
          #find 'next screen' touches
          else if (touchX > 3250 && touchY > 3250){touchType = 1
            question = question + 1}
          #blanket value for all other touches
          else {touchType = 2}
          #create new row in dataframe
          df <- rbind(df, c(sleepPrev, sleepCur, touchX, touchY, question, touchType))
          #update touchStart variable
          sleepPrev <- sleepCur
        }
      }
      #naming columns, cutting start rows out and writing to file
      colnames(df) <- c("touchStart", "touchEnd", "touchX", "touchY", "question", "touchType")
      cutIndex <- which(df$touchType == 3)
      df <- tail(df, nrow(df)-cutIndex[1])
      newFile <- paste("data\\data", n, ".csv", sep="")
      write.csv(df, file=newFile, row.names=F)
      #writing averages to fulldf
      touchRate <- (length(df$touchX)/max(df$touchEnd))*60000
      questionTotal <- max(df$question)-1
      questionTime <- max(df$touchEnd)/(max(df$question)-1)/1000
      totalTime <- max(df$touchEnd)/60000
      score <- ((1/questionTime)*((sum(df$touchType == 1))/(sum(df$touchType == 0))))*100
      totalTouches <- length(df$touchX)
      fulldf <- rbind(fulldf, c(touchRate, questionTotal, questionTime, totalTime, score, totalTouches))
    }
	#naming columns
    colnames(fulldf) <- c("touchRate", "questionTotal", "questionTime", "totalTime", "score", "totalTouches")
    #creating file from dataframe
	write.csv(fulldf, file="data\\summary.csv", row.names=F)
    workingD <- getwd()
    output$alert <- renderText(paste("Transformation complete! Data output located in ",workingD, "/data.", sep=""))
  })
}

shinyApp(ui = ui, server = server)