library(shiny)
library(tidyverse)


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "N",
                   label = "total pop",
                   min = 1,
                   value = 100),
      numericInput(inputId = "skew",
                   label = "skew your population",
                   min = 1,
                   value = 1),
      br(),
      br(),
      numericInput(inputId = "n",
                   label = "sample size",
                   min = 30,
                   value = 20),
      actionButton(inputId = "make_sample",
                   label = "see a sample"),
      br(),
      br(),
      numericInput(inputId = "sample_from_sample",
                   label = "how big of a sample do you want to draw from your sample",
                   min = 1,
                   value = 1),
      numericInput(inputId = "boot_reps",
                   label = "How many bootstrap reps would you like",
                   min = 1,
                   value = 20),
      numericInput(inputId = "clt_draws",
              label = "number of samples to draw from population",
              min = 1,
              value = 20)
    ),
    mainPanel(
      plotOutput(outputId = "pop_plot"),
      plotOutput(outputId = "empir_data"),
      plotOutput(outputId = "bs"),
      plotOutput(outputId = "clt")
    )
  )
  
)


server <- function(input, output, session) {
  
  pop <- reactive({
    
    a <- rgamma(input$N, shape = input$skew, rate = 1)
    
    a
    
  })
  
  output$pop_plot <- renderPlot(
    
    hist(pop())
    
  )
  
  samp <- eventReactive(input$make_sample, {
    
    sample(pop(), input$n, replace = TRUE)
    
  })
  
  output$empir_data <- renderPlot({
    
    hist(samp())
    
  })
  
  bootstrap <- reactive({
    
    replicate(input$boot_reps,
              mean(sample(samp(), input$sample_from_sample, replace = TRUE)))
    
  })
  
  output$bs <- renderPlot({
    
    hist(bootstrap())
    
  })
  
  central_limit_thereom <- reactive({
    
    replicate(input$clt_draws,
              mean(sample(pop(), input$n, replace = TRUE)))
    
  })
  
  output$clt <- renderPlot({
    
    hist(central_limit_thereom())
    
  })
  
}



shinyApp(ui = ui, server = server)