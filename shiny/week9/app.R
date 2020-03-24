library(shiny)
library(ggplot2)
library(tidyverse)
library(psych)
library(rsconnect)

# Data
week9_tbl <- read_csv("week3.csv") %>%
  mutate(timeStart = lubridate::ymd_hms(timeStart), 
         timeEnd   = lubridate::ymd_hms(timeEnd)) %>% 
  mutate(condition = recode_factor(condition, "A" = "Block A", "B" = "Block B", "C" = "Control"),
         gender    = recode_factor(gender, "M" = "Male", "F" = "Female")) %>% 
  filter(q6 == 1) %>%
  mutate(mean_Q1_Q5  = rowMeans(select(., q1:q5)),
         mean_Q6_Q10 = rowMeans(select(., q6:q10))) 

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Week 9"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # choose gender
            radioButtons("gender", "Gender", choices = c(levels(week9_tbl$gender), "All"), selected = "All"),
            # checkbox completion time
            checkboxInput("time", "Include completion time before August 1, 2017", TRUE)
        ),
        
        
        # Show a plot and dataset
        mainPanel(
           plotOutput("plot"),
           DT::dataTableOutput("data")
        )
    )
)

# Define server logic required
server <- function(input, output) {
  
  # Filter data with reactive
  filtered_data <- reactive({
    data <- week9_tbl
    if (input$time == FALSE){
      data <- subset(data,
                     timeEnd > as.Date("2017-08-01"))
    }
    data
    if (input$gender != "All"){
      data <- subset(data,
                     gender == input$gender)
    }
    data
  })
  
  # Output plot
  output$plot <- renderPlot({
    # use filtered data variable to render output
    data <- filtered_data()
    # create scatter plot
    ggplot(data, aes(x = mean_Q1_Q5, y = mean_Q6_Q10)) +
      geom_point() + 
      geom_smooth(method = lm, se = F)
  })
  
  # Output dataset
  output$data <- DT::renderDataTable({
    data <- filtered_data()
    data
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
