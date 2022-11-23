#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(dplyr)
library(tidyr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Foreign Students in Turkish Universities"),
    theme = shinythemes::shinytheme("superhero"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("uni_city",
                        "Select City",
                        choices = data$city %>% unique() %>% sort()
                          ),
            selectInput("uni_type", 
                        "Select University Type", 
                        choices = data$type %>% unique() %>% sort()
                        ),
            selectInput("uni_name", 
                        "Select a University", 
                        choices = data$name %>% unique() %>% sort()
                        )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("barplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$barplot <- renderPlot({
        # generate bins based on input$bins from ui.R
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
        
        # draw the histogram with the specified number of bins
        
      ggplot(highest_country, 
             aes(x = reorder(country, total), 
                 y = value, 
                 fill = gender)
             ) +
        geom_col() +
        coord_flip() + 
        labs(x = "Nationality", y = "Number of Students") 
        
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
