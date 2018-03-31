library(ggplot2)
library(shiny)

NHL <- read.csv("NHL_cleaned.csv", sep =";", header = TRUE)
NHL_num <- read.csv("NHL_num.csv", sep =";", header = TRUE)

# output$mainplot <- renderPlot({
#   
#   p <- ggplot(dataset[dataset$price <= 326,], aes(x = carat, y = color))
#   p <- p + geom_point()
#   print(p)
# })
# })


server <- function(input, output) {
  output$plot <- renderPlot({
    p <- ggplot(NHL, 
           aes(x = NHL_num[,input$xcol],
               y = NHL_num[,input$ycol],
               color = NHL_num[,input$filter] > input$number)) 
    p <- p + geom_point()
    p <- p + geom_smooth(method = "lm")
    print(p)
  })
  
  output$summary <- renderPrint({
    summary(NHL)
  })
  
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

ui <- navbarPage("NHL Statistics 2016/17",
                           tabPanel("Interactive Plot",
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput("xcol", "X Variable", 
                                                    choices=colnames(NHL_num)),
                                        selectInput("ycol", "Y Variable", 
                                                    choices=colnames(NHL_num)),
                                        selectInput("filter", "Filter Numeric Variable", 
                                                    choices=colnames(NHL_num)),
                                        numericInput('number', 'Numeric value for filter Numeric Variable', 3,    ###vmilyen szûrési lehetõség
                                                     min = 1, max = 100)
                                      ),
                                      mainPanel(
                                        plotOutput("plot")
                                      )
                                    )
                           ),
                           tabPanel("Descriptive Stats",
                                    verbatimTextOutput("summary")
                           ),
                           tabPanel("Search in the data",
                                    tabPanel("Table",
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
                                             DT::dataTableOutput("table"),
                                             downloadButton("downloadData", "Download")
                                    )))

shinyApp(server = server, ui = ui)
