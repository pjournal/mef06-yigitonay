#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)

data <-  read_excel("foreign_students_by_nationality_2021_2022.xlsx")
colnames(data) <- c("name", "type", "city", "country", "male", "female", "total")
data$male <- as.numeric(data$male)
data$female <- as.numeric(data$female)
data$total <- as.numeric(data$total)
data <- data[!is.na(data$type),]





# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Foreign Students in Turkish Universities"),
    theme = shinythemes::shinytheme("superhero"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("uni_city",
                        "Select a City",
                        choices = sort(unique(data$city))
                          ),
            selectInput("uni_type", 
                        "Select a University Type", 
                        choices = "",
                        selected = ""
                        ),
            selectInput("uni_name", 
                        "Select a University", 
                        choices = "",
                        selected = ""
                        )
        ),

        
        mainPanel(
           plotOutput("barplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  observeEvent(input$uni_city, {
    updateSelectInput(session, "uni_type", choices = unique(data$type[data$city == input$uni_city])
  )})
  
  observeEvent(c(input$uni_city, input$uni_type),{
    updateSelectInput(session, "uni_name", choices = unique(data$name[data$city == input$uni_city & data$type == input$uni_type]))
  })
  
  output$barplot <- renderPlot({
   
    highest_country <- 
      data %>% 
      filter(city == input$uni_city,
             type == input$uni_type,
             name == input$uni_name) %>% 
      group_by(country) %>% 
      summarise(female = sum(female), 
                male = sum(male), 
                total = sum(total)) %>% 
      arrange(desc(total)) %>% 
      pivot_longer(c(-country, -total), names_to = "gender", values_to = "value") %>% 
      slice_max(total, n = 20)
    
    
    
    ggplot(highest_country, 
           aes(x = reorder(country, total), 
               y = value, 
               fill = gender)
    ) +
      geom_col() +
      coord_flip() + 
      labs(x = "Nationality", y = "Number of Students", title = "Top 10 Countries") +
      geom_text(aes(label = value), position = position_stack(vjust = 0.5), fontface = "bold")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
