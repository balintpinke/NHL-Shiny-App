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


setwd("D:/R_WD/NHL-Shiny-App")
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
      menuItem("Player statistics", tabName = "interactive_plot", icon = icon("crosshairs")),
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
                  box(title = "Compare teams", width = 12, solidHeader = TRUE, status = "primary",
                      fluidRow(
                        box(width = 3,
                            selectInput("team_1", "Choose team 1",
                                        choices = unique(NHL$Team), selected=unique(NHL$Team)[1])
                        ),
                        box(width = 3,
                            selectInput("team_2", "Choose team 2",
                                        choices = unique(NHL$Team), selected=unique(NHL$Team)[2])
                        ),
                  box(width = 3,
                            selectInput("team_stats_1", "Statistics on histogram",
                           choices = colnames(
                             select(NHL, Age, Game.Played, Goal, Assist, Points, Plus.Minus, Salary, TOI.GP, 
                                    PIM, sDist, SA, Grit)),
                           selected = colnames(
                             select(NHL, Age, Game.Played, Goal, Assist, Points, Plus.Minus, Salary, TOI.GP, 
                                    PIM, sDist, SA, Grit))[3]
                           )
                ),
                  box(width = 3,
                            selectInput("team_stats_2", "Choose statistics",
                               choices = colnames(
                                 select(NHL, Age, Game.Played, Goal, Assist, Points, Plus.Minus, Salary, TOI.GP, 
                                        PIM, sDist, SA, Grit)),
                               selected = colnames(
                                 select(NHL, Age, Game.Played, Goal, Assist, Points, Plus.Minus, Salary, TOI.GP, 
                                        PIM, sDist, SA, Grit))[4]
                   )
               )
               ),
               plotlyOutput("compare_teams"),
               DT::dataTableOutput("two_teams_table"))
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
                   checkboxInput('linear', 'Linear')
                   # checkboxInput('smooth', 'Smooth'),
                   # selectInput('facet_row', 'Facet Row', c(None = '.', "Position1"))
                 ),
                 mainPanel(
                   plotlyOutput("plot", width = "100%")
                 )
               )
      ),
      # 3. tab content
      tabItem("radar_chart",
              box(title = "What type of shots a player score a goal", width = 12, solidHeader = TRUE, status = "primary",
                  box(width = 4,
               selectInput("player1", "Choose player 1", 
                           choices = names(Goals2)[2:889], selected = names(Goals2)[602])),
                  box(width = 4,
               selectInput("player2", "Choose player 2", 
                           choices = names(Goals2)[2:889], selected = names(Goals2)[151])),
                  box(width = 4,
               selectInput("player3", "Choose player 3", 
                           choices = names(Goals2)[2:889], selected = names(Goals2)[511])
               )),
              chartJSRadarOutput("radar", width = "450", height = "300")
              
               
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
    )



server <- function(input, output) {
  
  
  # NHL2=NHL %>%
  #   filter(Team=="CHI" | Team=="DET")
  # 
  # NHL2$Team=as.character(NHL2$Team)
  # 
  # p <- plot_ly(data = NHL2, x = ~Goal, y = ~Assist, color = ~Team, colors = c("red", "blue"))
  
  output$compare_teams <- renderPlotly({
    
      NHL2 = NHL %>%
        filter(Team==input$team_1 | Team==input$team_2) %>% 
        select(Name, Team, input$team_stats_1, input$team_stats_2)
      
      # NHL2 = NHL %>%
      #   filter(Team=="CHI" | Team=="DET") %>% 
      #   select(Name, Team, Goal, Assist)
      NHL2$Team=as.character(NHL2$Team)
      
      
      p <- plot_ly(data = NHL2, x = ~NHL2[[input$team_stats_1]], y = ~NHL2[[input$team_stats_2]], text = ~Name,
                   color = ~Team, colors = c("red", "blue")) %>% 
        layout(xaxis = list(title = input$team_stats_1) , yaxis = list(title = input$team_stats_2))
      p
    
  })
  
  output$two_teams_table <- DT::renderDataTable(DT::datatable({
    NHL2 = NHL %>%
      filter(Team==input$team_1 | Team==input$team_2)
      
    NHL2[1:10]
  }))


  
  output$plot <- renderPlotly({
    p <- ggplot(NHL, aes(x = select_if(NHL, is.numeric)[,input$xcol],
                         y = select_if(NHL, is.numeric)[,input$ycol]
                         )) + geom_point()
    
    if (input$filter != "None")
      p <- p + aes(color = select(NHL, Age, Game.Played:Plus.Minus, TOI.GP, PIM)[,input$filter] > input$number)
    
    if (input$color != 'None')
      p <- p + aes_string(color = input$color)
    
    # if (input$smooth)
    #   p <- p + geom_smooth(method = "loess", se = FALSE, color = "Red")
    # 
    if (input$linear)
      p <- p + geom_smooth(method = "lm", se = FALSE)
    # 
    # facets <- paste(input$facet_row, "~", ".")
    # if (facets != '. ~ .')
    # p <- p + facet_grid(facets)
    
    #p <- p + geom_smooth(method = "lm") 
    
    p <- p + xlab(paste(input$xcol, collapse = " "))
    p <- p + ylab(paste(input$ycol,collapse = " "))
    p <- p + theme(legend.position = "bottom")
    p <- p + theme(legend.title = element_blank())
    
    ggplotly(p)
  })
  
  output$radar <- renderChartJSRadar({
    
    goal_types = c("Backhand", "Deflections", "Slap shots", "Snap shots", "Tip shots", "Wraparound", "Wrist shot")
    Goals2$Label=goal_types
    
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
