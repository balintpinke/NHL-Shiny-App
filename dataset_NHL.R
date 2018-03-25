library(tidyverse)
library(shiny)
library(DT)
library(data.table)

getwd()
list.files()

NHL <- read.csv("NHL 2016_17.csv",header = TRUE, sep = ";")
class(NHL)

dim(NHL)
glimpse(NHL)
attach(NHL)
hist(GP)

names(NHL)
colnames(NHL)[23] <- "PlusMinus"

##Separate Position variable
NHL <- NHL %>%
  separate(Position, c("Position1", "Position2"), sep = "/")

##Age variable
NHL <- NHL %>%
  separate(Born, c("Year", "monthday"), sep = 4)

####convert class of variables
NHL$Year <- as.numeric(as.character(NHL$Year))

NHL <- mutate(NHL, Age = 2016 - Year)

##Create a Points/GP variable
NHL <- mutate(NHL, PointperGame = (PTS / GP))

mean(NHL$Salary, na.rm = TRUE)


NHL2<- select(NHL, First.Name, Last.Name, Age, Nat, Team, 
              GP, G, A, PTS, PlusMinus, Position1, Salary)

datatable(NHL)

write.csv2(NHL2, file="NHL_cleaned.csv", row.names = FALSE)

NHL <- read.csv("NHL_cleaned.csv", sep =";", header = TRUE)






#Meccs/Pont
ggplot(data = NHL) + 
  geom_point(mapping = aes(x = GP, y = PTS)) +
  ggtitle("dfhdhgh") + 
  theme(plot.title = element_text(size=14, ))




ggplot(data = NHL, mapping = aes(x = GP, y = PTS)) + 
  geom_point() + 
  geom_smooth()

ggplot(data = NHL) + 
  geom_point(mapping = aes(x = GP, y = PTS, color = Position1))

ggplot(data = NHL) + 
  geom_point(mapping = aes(x = GP, y = PTS, color = G > 20))



ggplot(data = NHL) + 
  geom_point(mapping = aes(x = GP, y = PTS, color = PlusMinus > 0))


ggplot(data = NHL) +
  geom_point(mapping = aes(x = Salary, y = PTS, color = Position1))

ggplot(data = NHL, mapping = aes(x = Salary, y = PTS, color = Position1)) +
  geom_point() +
  geom_smooth(method = "lm")


ggplot(data = NHL, mapping = aes(x = Age, y = Salary)) +
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(data = NHL, mapping = aes(x = Age, y = Salary, color = PTS > 40)) +
  geom_point() + 
  geom_smooth()


ggplot(data = NHL, mapping = aes(x = Salary, y = PlusMinus, color = Age > 25)) +
  geom_point()

ggplot(data = NHL, mapping = aes(x= PTS, y = Salary)) +
  geom_point(mapping = aes(color = Salary)) +
  geom_smooth(data = filter(NHL, Team == "WSH")) +
  geom_smooth(data = filter(NHL, Team == "EDM"))


ggplot(data = NHL) + 
  geom_point(mapping = aes(x = GP, y = PTS, color = Position1))

ggplot(data = NHL, mapping = aes(x = GP, y = PTS, color = Position1)) + 
  geom_point() + 
  geom_smooth()

ggplot(data = NHL) + 
  geom_point(mapping = aes(x = PTS, y = PlusMinus, color = Position1))




model1 <- lm(Salary~Age+PlusMinus+PTS+GP, data = NHL)
summary(model1)
plot(model1)

#example for filtering observations
filter(flights, month == 1, day == 1)

model2 <- lm(Salary~Age+PlusMinus+PTS+GP, data = filter(NHL, GP > 10))
summary(model2)

model2 <- lm(Salary~Age*PlusMinus*PTS, data = filter(NHL, GP > 10))


ggplot(data = filter(NHL, GP > 10), mapping = aes(x = PTS, y = PlusMinus, color = Position1)) + 
  geom_point() + 
  geom_smooth(method = "lm")



ggplot(data = NHL) + 
  geom_point(mapping = aes(x = PTS, y = Salary)) + 
  facet_wrap(~ Position1, nrow = 3)

ggplot(data = NHL) +  
  geom_point(mapping = aes(x = PTS, y = Salary)) +
  facet_grid(. ~ Position1)

ggplot(data = NHL, mapping = aes(x = PTS, y = Salary)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_wrap(~ Position1, nrow = 3)

ggplot(data = NHL, mapping = aes(x = PTS, y = Salary)) +  
  geom_point() +
  geom_smooth(method = "lm") +
  facet_grid(. ~ Position1)

ggplot(data = NHL, mapping = aes(x = PTS, y = Salary)) + 
  geom_point(mapping = aes(color = Age < 23)) + 
  geom_smooth(method = "lm") +
  facet_wrap(~ Position1, nrow = 3)

ggplot(data = NHL, mapping = aes(x = Age, y = Salary)) + 
  geom_point(mapping = aes(color = Age < 23)) + 
  geom_smooth(method = "loess") +
  facet_wrap(~ Position1, nrow = 3)





#################################################################################
NHL2<- select(NHL, First.Name, Last.Name, Nat, Team, GP, G, A, PTS, Position)
sum(is.na(NHL2))

####convert class of variables
NHL2 %>% map_if(is.factor, as.character) %>% as_data_frame -> NHL2
NHL2 %>% map_if(is.integer, as.numeric) %>% as_data_frame -> NHL2


####subset observations
filter(flights, month == 1, day == 1)

#################################################################################


server <- function(input, output) {
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- select(NHL, First.Name, Last.Name, Nat, Team, GP, G, A, PTS, PlusMinus, Position1, Salary)

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
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(data, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data, file, row.names = TRUE)})




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
