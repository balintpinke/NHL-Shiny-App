NHL <- read.csv("NHL_cleaned.csv", sep =";", header = TRUE)

server <- function(input, output) {
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- NHL
    
    if (input$name != "All") {
      data <- data[data$Last.Name == input$name,]
    }
    if (input$nat != "All") {
      data <- data[data$Nat == input$nat,]
    }
    if (input$team != "All") {
      data <- data[data$Team == input$team,]
    }
    data
  }))}

# output$downloadData <- downloadHandler(
#   filename = function() {
#     paste(data, ".csv", sep = "")
#   },
#   content = function(file) {
#     write.csv(data, file, row.names = TRUE)})




ui <- fluidPage(
  titlePanel("NHL Stats, Season 2016/17"),
  
  # Button
  downloadButton("downloadData", "Download"),
  
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(4,
           selectInput("name",
                       "Last Name:",
                       c("All",
                         unique(as.character(NHL$Last.Name))))
    ),
    column(4,
           selectInput("nat",
                       "Nationality:",
                       c("All",
                         unique(as.character(NHL$Nat))))
    ),
    column(4,
           selectInput("team",
                       "Team:",
                       c("All",
                         unique(as.character(NHL$Team))))
    )
  ),
  # Create a new row for the table.
  fluidRow(
    DT::dataTableOutput("table")
  )
)

shinyApp(ui = ui, server = server)
