library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(radarchart)
library(d3heatmap)
library(plotly)
library(data.table)
library(DT)
library(crosstalk)


# setwd("D:/R_WD/NHL-Shiny-App")
NHL <- read.csv("NHL_cleaned.csv", sep = ";", header = TRUE)

old_names <- c("Game.Played", "Plus_minus", "PIM", "SA", "Grit")
new_names <- c("Game Played",  "Plus-Minus", "Penalty (minutes)", 
               "Shots on goal allowed", "Grit")
            

variables <- c("Age", 'Game Played', "Goal", "Assist", "Points", 
               "Plus-Minus", "Salary", "Penalty (minutes)",
               "Shots on goal allowed",
               "Grit")

data.table::setnames(NHL, old_names, new_names)

#radarchart
Goals <- select(NHL,Name, G.Bkhd:G.Wrst)
rownames(Goals) <- Goals[,1]
Goals[,1] <- NULL
Goals2 <- data.frame(t(Goals))      
Goals2 <- data.frame(Label = row.names(Goals2), Goals2)
rownames(Goals2) <- NULL


ui <- dashboardPage(
  dashboardHeader(title = "NHL 2016/17"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Compare teams", tabName = "compare_teams", icon = icon("hockey-puck")),
      menuItem("Compare players", tabName = "interactive_plot", icon = icon("user-friends")),
      menuItem("Goal types", tabName = "radar_chart", icon = icon("bullseye")),
      menuItem("Team heatmap", tabName = "heatmap", icon = icon("map"), badgeLabel = "new", badgeColor = "green"),
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
                            selectInput("team_stats_1", "X axis",
                           choices = variables,
                           selected = variables[3]
                           )
                ),
                  box(width = 3,
                            selectInput("team_stats_2", "Y axis",
                               choices = variables,
                               selected = variables[4]
                   )
               )
               ),
               plotlyOutput("compare_teams"),
               DT::dataTableOutput("two_teams_table"))
               )
               ),
      # 2. tab content
      tabItem("interactive_plot",
              fluidRow(
                box(title = "Compare players", width = 12, solidHeader = TRUE, status = "primary",
                    fluidRow(
                      box(width = 6,
                   selectInput("player_stats_1", "X axis", 
                               choices = variables, selected = variables[6])
                   ),
                   box(width = 6,
                   selectInput("player_stats_2", "Y axis", 
                               choices = variables, selected = variables[3])
                   )),
                   plotlyOutput("plot"),
                   DT::dataTableOutput("table"))
              )
      ),
      # 3. tab content
      tabItem("radar_chart",
              box(title = "Scoring goals with different type of shots", width = 12, solidHeader = TRUE, status = "primary",
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
              fluidRow(
              box(title = "Team heatmap", width = 12, solidHeader = TRUE, status = "primary",
              fluidRow(    
                  box(width = 4,
              selectInput("team_heatmap", "Choose Team", choices=unique(NHL$Team), multiple = FALSE)
                  ),
                  box(width = 2,
               checkboxInput("cluster", "Click to apply clustering")
                  ),
               box(width = 3,
               sliderInput('cluster_row', 'Set cluster group number for players', 2,
                           min = 1, max = 10)
               ),
               box(width = 3,
               sliderInput('cluster_col', 'Set cluster group number for stats', 4,
                           min = 1, max = 10)
               )),
               d3heatmapOutput("heatmap")
              )
              )
      ),

      # 5. tab content
      tabItem("about",
               fluidRow(
                 box(title = "About", width = 12, solidHeader = TRUE, status = "primary",
                 includeMarkdown("README.md"),
                 tags$iframe(src="https://giphy.com/embed/l3q2SMNXwyd2hJsAM", height=500, width=500, frameborder=0, seamless="seamless")
               ))
      )
    ))
    )




server <- function(input, output) {
  
  
  NHL_df_2_teams <- reactive({
     NHL = NHL %>%
      dplyr::filter(Team==input$team_1 | Team==input$team_2)

     NHL
  })
  
  
  NHL_df_env <- crosstalk::SharedData$new(NHL_df_2_teams)
  
  output$two_teams_table <- DT::renderDataTable({
    
    NHL = NHL_df_2_teams()
    
    selected <- NHL[NHL_df_env$selection(),]
    dt <- DT::datatable(NHL,
                        options = list(
                          scrollX = TRUE
                        ))
    
    if (NROW(selected) == 0) {
      dt
    } else {
      selected
    }
  })
  
  output$compare_teams <- renderPlotly({
    
      p <- NHL_df_env %>% 
        plot_ly(x = ~get(input$team_stats_1), y = ~get(input$team_stats_2), text = ~Name, color = ~Team) %>%
          layout(xaxis = list(title = input$team_stats_1) , yaxis = list(title = input$team_stats_2)) %>% 
          highlight(on = "plotly_selected", off = "plotly_deselect", color = I('red'), selected = attrs_selected(name = 'Selected players'))
      p
  })
  
  

  
  # output$two_teams_table <- DT::renderDataTable(DT::datatable({
  #   NHL2 = NHL %>%
  #     filter(Team==input$team_1 | Team==input$team_2)
  # 
  #   NHL2[order(-NHL2$Goal, -NHL2$Assist),1:10]
  # }))
  
  


  
  
  
  
  
  
  
  output$plot <- renderPlotly({
    
    p <- plot_ly(data = NHL, 
                 x = ~NHL[[input$player_stats_1]], 
                 y = ~NHL[[input$player_stats_2]], 
                 text = ~Name,
                 color = ~Position) %>%  #colors = c("red", "blue")
      layout(xaxis = list(title = input$player_stats_1) , yaxis = list(title = input$player_stats_2))
    

  })
  
  output$radar <- renderChartJSRadar({
    
    goal_types = c("Backhand", "Deflections", "Slap shots", "Snap shots", "Tip shots", "Wraparound", "Wrist shot")
    Goals2$Label=goal_types
    
    chartJSRadar(Goals2[, c("Label", input$player1, input$player2, input$player3)],
                 maxScale = 20, scaleLineWidth = 2, showToolTipLabel = TRUE)
  })
  
 
  output$heatmap <- renderD3heatmap({
    
    df=NHL[NHL$Team==input$team_heatmap, c("Name",variables)]
    rownames(df)=df$Name
    df$Name=NULL
    
    d3heatmap(
      scale(df),
      color = scales::col_quantile("Blues", NULL, 5),
      dendrogram = if (input$cluster) "both" else "none",
      k_row = input$cluster_row,
      k_col = input$cluster_col
    )
  })
  
  
  output$table <- DT::renderDataTable(DT::datatable(
      NHL,
      options = list(
        scrollX = TRUE
      )
  ))
  
  }

shinyApp(server = server, ui = ui)
