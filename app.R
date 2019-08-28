library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(radarchart)
library(d3heatmap)
library(plotly)

# # img <- png::readPNG("D:/R_WD/NHL-Shiny-App/nhl.png")
# 
# # Plot with background image
# ggplot(iris, aes(Species, Sepal.Length))+
#   background_image(img)+
#   geom_boxplot(aes(fill = Species), color = "white")+
#   fill_palette("jco")



NHL <- read.csv("NHL_cleaned.csv", sep = ";", header = TRUE)


#rownames(NHL) <- NHL[,1]

Goals <- select(NHL,Name, G.Bkhd:G.Wrst)
rownames(Goals) <- Goals[,1]
Goals[,1] <- NULL
Goals2 <- data.frame(t(Goals))      
Goals2 <- data.frame(Label = row.names(Goals2), Goals2)
rownames(Goals2) <- NULL


top10_Points <- select(NHL, Name, Age, Goal, Assist, Points, Plus.Minus, TOI.GP, PIM, X.FOT:ozFOL) %>% 
  top_n(10, Points)
#rownames(top10_Points) <- top10_Points[,1]
#top10_Points[,1] <- NULL

ui <- dashboardPage(
  dashboardHeader(title = "NHL 2016/17"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Compare teams", tabName = "compare_teams", icon = icon("dashboard")),
      menuItem("Interactive plot", tabName = "interactive_plot", icon = icon("crosshairs")),
      menuItem("Radar chart", tabName = "radar_chart", icon = icon("th")),
      menuItem("Heatmap", tabName = "heatmap", icon = icon("map"), badgeLabel = "error", badgeColor = "red"),
      menuItem("Discover the data", tabName = "discover_data", icon = icon("table"), badgeLabel = "new", badgeColor = "green"),
      menuItem("About", tabName = "about", icon = icon("cogs"))
    )
  ),
  dashboardBody(
    tabItems(
      # 1. tab content
      tabItem("compare_teams",
              
                fluidRow(
                  box(title = "Team 1", width = 6, solidHeader = TRUE, status = "primary",
                      fluidRow(
                        box(width = 6,
                            selectInput("team_stats_1", "Choose team 1",
                                        choices = unique(NHL$Team))
                        ),
                box(width = 6,
               selectInput("team_stats_2", "Statistics on histogram",
                           choices = colnames(
                             select(NHL, Age, Game.Played, Goal, Assist, Points, Plus.Minus, Salary, TOI.GP, 
                                    PIM, sDist, SA, Grit))
                           )
                )
               ),

                plotlyOutput("summaryplot")),

                  box(title = "Team 2", width = 6, solidHeader = TRUE, status = "primary", 
                   fluidRow(
                     box(width = 6,
                       selectInput("team_stats_3", "Choose team 2",
                             choices = unique(NHL$Team))
                       ),
                     box(width = 6,
                       selectInput("team_stats_4", "Choose statistics",
                             choices = colnames(
                               select(NHL, Age, Game.Played, Goal, Assist, Points, Plus.Minus, Salary, TOI.GP, 
                                      PIM, sDist, SA, Grit))
                       )
                       )
                     ),

                 plotlyOutput("team_summaryplot")
               
                  )
               ),
                   selectInput("stats2", "Statistics by positions",
                               choices = colnames(select_if(NHL, is.numeric))),
                   mainPanel(
                     plotlyOutput("boxplot", width = "100%"),
                     
                     selectInput("stats3", "Statistics",
                                 choices = colnames(select_if(NHL, is.numeric))),
                     selectInput("team", "Statistics by teams",
                                 choices = unique(NHL$Team))),
                   mainPanel(
                     plotOutput("violin", width = "100%")
                   )
               )
              ),
      # 2. tab content
      tabItem("interactive_plot",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("xcol", "X Variable", 
                               choices = colnames(select_if(NHL, is.numeric))),
                   selectInput("ycol", "Y Variable", 
                               choices = colnames(select_if(NHL, is.numeric))),
                   selectInput("filter", "Filter Variable", 
                               choices = c("None", colnames((select(NHL, Age, Game.Played:Plus.Minus, TOI.GP, PIM))))),
                   sliderInput('number', 'Filter variable value is higher than this number', 25,
                               min = 0, max = 100),
                   selectInput('color', 'Color', c('None', 'Position1')),
                   checkboxInput('linear', 'Linear'),
                   checkboxInput('smooth', 'Smooth'),
                   selectInput('facet_row', 'Facet Row', c(None = '.', "Position1"))
                 ),
                 mainPanel(
                   plotOutput("plot", width = "100%")
                 )
               )
      ),
      # 3. tab content
      tabItem("radar_chart",
               selectInput("player1", "Player 1", 
                           choices = names(Goals2)[2:889]),
               selectInput("player2", "Player 2", 
                           choices = names(Goals2)[2:889]),
               selectInput("player3", "Player 3", 
                           choices = names(Goals2)[2:889]),
               mainPanel(
                 chartJSRadarOutput("radar", width = "450", height = "300"), width = 9
               )
               
      ),
      # 4. tab content
      tabItem("heatmap",
               selectInput("palette", "Palette", c("YlOrRd", "RdYlBu", "Greens", "Blues")),
               selectInput("top10", "Select the top 10 player in that stat", 
                           choices = colnames(select_if(NHL, is.numeric))),
               checkboxInput("cluster", "Apply clustering"),
               sliderInput('cluster_row', 'Set group number for rows', 2,
                           min = 1, max = 10),
               sliderInput('cluster_col', 'Set group number for columns', 4,
                           min = 1, max = 10),
               d3heatmapOutput("heatmap")
      ),
      
      # 5. tab content
      tabItem("discover_data",
               tabPanel("Table",
                        # Create a new Row in the UI for selectInputs
                        fluidRow(
                          column(4,
                                 selectInput("player_name",
                                             "Name:",
                                             c("All",
                                               unique(as.character(NHL$Name))))
                          ),
                          column(4,
                                 selectInput("player_nat",
                                             "Nationality:",
                                             c("All",
                                               unique(as.character(NHL$Nat))))
                          ),
                          column(4,
                                 selectInput("team_DT",
                                             "Team:",
                                             c("All",
                                               unique(as.character(NHL$Team))))
                          )
                        ),
                        DT::dataTableOutput("table"),
                        downloadButton("downloadData", "Download")
               )),
      # 5. tab content
      tabItem("about",
               fluidRow(
                 column(6,
                        includeMarkdown("README.md")
                 ),
                 column(3,
                        tags$iframe(src="https://giphy.com/embed/l3q2SMNXwyd2hJsAM", height=500, width=500, frameborder=0, seamless="seamless")
                 ) 
               ))
    ))



server <- function(input, output) {
  
  # output$summaryplot <- renderPlotly({
  #   k <- ggplot(NHL, aes(x = select(NHL, Age, Game.Played, Goal, Assist, Points, Plus.Minus, Salary, TOI.GP, PIM, sDist,
  #                                   SA, Grit)[,input$stats]))
  #   k <- k + geom_bar()
  #   k <- k + xlab(paste(input$stats, collapse = " "))
  #   ggplotly(k, tooltip = "count")
  #   #https://stackoverflow.com/questions/34605919/formatting-mouse-over-labels-in-plotly-when-using-ggplotly?rq=1
  #   # https://stackoverflow.com/questions/45948926/ggplotly-text-aesthetic-causing-geom-line-to-not-display
  #   })
  
  output$summaryplot <- renderPlotly({
    
    NHL=NHL %>% 
      filter(Team==input$team_stats_1)
    
    k <- ggplot(NHL, aes(x = select(NHL, Age, Game.Played, Goal, Assist, Points, Plus.Minus, Salary, TOI.GP, PIM, sDist,
                                    SA, Grit)[,input$team_stats_2]))
    k <- k + geom_bar()
    k <- k + xlab(paste(input$team_stats, collapse = " "))
    ggplotly(k, tooltip = "count")
  })
  
  
  output$team_summaryplot <- renderPlotly({
    
    NHL=NHL %>% 
      filter(Team==input$team_stats_3)
    
    k <- ggplot(NHL, aes(x = select(NHL, Age, Game.Played, Goal, Assist, Points, Plus.Minus, Salary, TOI.GP, PIM, sDist,
                                    SA, Grit)[,input$team_stats_4]))
    k <- k + geom_bar()
    k <- k + xlab(paste(input$team_stats, collapse = " "))
    ggplotly(k, tooltip = "count")
  })
  
  
  output$boxplot <- renderPlotly({
    t <- ggplot(NHL, aes(Position1, y = select_if(NHL, is.numeric)[,input$stats2]), fill = Position1)
    t <- t + geom_boxplot()
    t <- t + ylab(paste(input$stats2, collapse = " "))
    ggplotly(t)

  })
  

  
  
  output$plot <- renderPlot({
    p <- ggplot(NHL, aes(x = select_if(NHL, is.numeric)[,input$xcol],
                         y = select_if(NHL, is.numeric)[,input$ycol]
                         )) + geom_point()
    
    if (input$filter != "None")
      p <- p + aes(color = select(NHL, Age, Game.Played:Plus.Minus, TOI.GP, PIM)[,input$filter] > input$number)
    
    if (input$color != 'None')
      p <- p + aes_string(color = input$color)
    
    if (input$smooth)
      p <- p + geom_smooth(method = "loess", se = FALSE, color = "Red")
    
    if (input$linear)
      p <- p + geom_smooth(method = "lm", se = FALSE)
    
    facets <- paste(input$facet_row, "~", ".")
    if (facets != '. ~ .')
    p <- p + facet_grid(facets)
    
    #p <- p + geom_smooth(method = "lm") 
    
    p <- p + xlab(paste(input$xcol, collapse = " "))
    p <- p + ylab(paste(input$ycol,collapse = " "))
    p <- p + theme(legend.position = "bottom")
    p <- p + theme(legend.title = element_blank())
    
    print(p)
  })
  
  output$radar <- renderChartJSRadar({
    chartJSRadar(Goals2[, c("Label", input$player1, input$player2, input$player3)],
                 maxScale = 20, scaleLineWidth = 2, showToolTipLabel = TRUE)
  })
  
 
  output$heatmap <- renderD3heatmap({
    d3heatmap(
      scale(top10_Points),
      colors = input$palette,
      dendrogram = if (input$cluster) "both" else "none",
      k_row = input$cluster_row,
      k_col = input$cluster_col
    )
  })
  
  # output$summary <- renderPlot({
  #   k <- ggplot(NHL, aes(x = select_if(NHL, is.factor)[,input$char],
  #                        y = select_if(NHL, is.numeric)[,input$num]
  #                        )) +
  #         geom_boxplot()
  # })
  # 
  
  
  
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <- NHL[,1:12]
    if (input$player_name != "All") {
      data <- data[data$Name == input$player_name,]
    }
    if (input$player_nat != "All") {
      data <- data[data$Nat == input$player_nat,]
    }
    if (input$team_DT != "All") {
      data <- data[data$Team == input$team_DT,]
    }
    data
    
  }))}


# output$downloadData <- downloadHandler(
#   filename = function() {
#     paste(data, ".csv", sep = "")
#   },
#   content = function(file) {
#     write.csv(data, file, row.names = TRUE)})




shinyApp(server = server, ui = ui)
