library(ggplot2)
library(dplyr)
library(shiny)
library(shinythemes)
library(radarchart)
library(d3heatmap)


NHL <- read.csv("NHL_cleaned.csv", sep =";", header = TRUE)

server <- function(input, output) {
  output$plot <- renderPlot({
    p <- ggplot(NHL, 
           aes(x = select_if(NHL, is.numeric)[,input$xcol],
               y = select_if(NHL, is.numeric)[,input$ycol],
               color = select(NHL, Age, GP:PlusMinus, TOI.GP, PIM)[,input$filter] > input$number)) 
    p <- p + geom_point()
    p <- p + geom_smooth(method = "lm") 
    p <- p + labs(title = "2D plot with filter variable")
    p <- p + xlab(paste(input$xcol, collapse = " "))
    p <- p + ylab(paste(input$ycol,collapse = " "))
    #p <- p + labs(paste(input$filter > input$number, collapse = " " ))
    print(p)
  })
  
  output$radar <- renderChartJSRadar({
    chartJSRadar(NHL[, c("Label", input$player1)], 
                 maxScale = 10, showToolTipLabel=TRUE)
  })
  
  output$heatmap <- renderD3heatmap({
    d3heatmap(
      scale(),
      colors = input$palette,
      dendrogram = if (input$cluster) "both" else "none",
      k_row = input$cluster_row,
      k_col = input$cluster_col
    )
  })
  
  output$summary <- renderPrint({
    summary(NHL)
  })
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <- NHL[,1:12]
    if (input$name != "All") {
      data <- data[data$Name == input$name,]
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



ui <- fluidPage(theme = shinytheme("united"),
      navbarPage("NHL Statistics 2016/17",
                           tabPanel("Interactive Plot",
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput("xcol", "X Variable", 
                                                    choices=colnames(select_if(NHL, is.numeric))),
                                        selectInput("ycol", "Y Variable", 
                                                    choices=colnames(select_if(NHL, is.numeric))),
                                        selectInput("filter", "Filter Variable", 
                                                    choices=colnames(select(NHL, Age, GP:PlusMinus, TOI.GP, PIM))),
                                        sliderInput('number', 'set value for filter variable', 25,
                                                     min = 0, max = 100)
                                      ),
                                      mainPanel(
                                        plotOutput("plot")
                                      )
                                    )
                           ),
                           tabPanel("Radar plot",
                                    selectInput("player1", "Player 1", 
                                                choices=NHL$Name),
                                    selectInput("player2", "Player 2", 
                                                choices=NHL$Name),
                                    selectInput("plaer3", "Player 3", 
                                                choices=NHL$Name),
                                    mainPanel(
                                      chartJSRadarOutput("radar", width = "450", height = "300"), width = 7
                                    )
                             
                             ),
                          tabPanel("A heatmap demo",
                                    selectInput("palette", "Palette", c("YlOrRd", "RdYlBu", "Greens", "Blues")),
                                    checkboxInput("cluster", "Apply clustering"),
                                    sliderInput('cluster_row', 'Set group number for rows', 2,
                                                 min = 1, max = 10),
                                    sliderInput('cluster_col', 'Set group number for columns', 4,
                                                 min = 1, max = 10),
                                    d3heatmapOutput("heatmap")
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
                                                                  "Name:",
                                                                  c("All",
                                                                    unique(as.character(NHL$Name))))
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
                                    ))))

shinyApp(server = server, ui = ui)
