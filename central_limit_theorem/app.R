library(shiny)
library(tidyverse)

ui <- fluidPage(
  titlePanel("Central Limit Theorem"),
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "pop",
                   label = "How big do you want your population",
                   min = 1,
                   value = 10),
      numericInput(inputId = "sample_size",
                   label = "How big do you want your sample",
                   min = 30,
                   value = 30),
      numericInput(inputId = "samples",
                   label = "how many samples do you want",
                   min = 1,
                   value = 100),
      selectInput(inputId = "stat",
                  label = "what summary statistic would you like to use",
                  choices = c('mean',
                              'median',
                              'sum',
                              'max',
                              'min')),
      actionButton(inputId = "go",
                   label = "Simulate")
    ),
    mainPanel(
      plotOutput("CLT")
    )
  )
  
)

server <- function(input, output, session) {
  
  data <- eventReactive(input$go, {
    mean <- replicate(input$samples,
                      mean((sample(1:input$pop,
                                   input$sample_size,
                                   replace = TRUE))))
    median <- replicate(input$samples,
                        median((sample(1:input$pop,
                                       input$sample_size,
                                       replace = TRUE))))
    sum <- replicate(input$samples,
                     sum((sample(1:input$pop,
                                 input$sample_size,
                                 replace = TRUE))))
    max <- replicate(input$samples,
                     max((sample(1:input$pop,
                                 input$sample_size,
                                 replace = TRUE))))
    min <- replicate(input$samples,
                     min((sample(1:input$pop,
                                 input$sample_size,
                                 replace = TRUE))))
    
    
    
    df <- data.frame(mean = mean,
                     median = median,
                     sum = sum,
                     max = max,
                     min = min)
    
    df
  })
  
  output$CLT <- renderPlot({
    if(input$stat == "median"){
      data() %>% 
        ggplot(aes(x = median))+
        geom_histogram(color = "white",
                       fill = "purple")+
        theme_classic()
    } else if (input$stat == "sum"){
      data() %>% 
        ggplot(aes(x =sum))+
        geom_histogram(color = "white",
                       fill = "purple")+
        theme_classic()
    } else if (input$stat == "max") {
      data() %>% 
        ggplot(aes(x = max))+
        geom_histogram(color = "white",
                       fill = "purple")+
        theme_classic()
    } else if (input$stat == "min"){
      data() %>% 
        ggplot(aes(x = min))+
        geom_histogram(color = "white",
                       fill = "purple")+
        theme_classic()
    } else {
      data() %>% 
        ggplot(aes(x = mean))+
        geom_histogram(color = "white",
                       fill = "purple")+
        theme_classic()
    }
      
    
    
  }) %>% 
    bindEvent(input$go)
  
}



shinyApp(ui = ui, server = server)