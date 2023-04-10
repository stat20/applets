library(shiny)
library(tidyverse)


ui <- fluidPage(
  br(),
  fluidRow(
    column(4,
      numericInput(inputId = "N",
                   label = "total pop",
                   min = 1,
                   value = 100),
      numericInput(inputId = "skew",
                   label = "skew your population",
                   min = 1,
                   value = 1),
      radioButtons(inputId = "invert",
                   label = "Invert the population (this only changes the distribution, allowing population to be more easily left skewd)",
                   choices = c("non - inverted",
                               "invert"))
      ),
      column(7,
        plotOutput(outputId = "pop_plot")
      )
      ),
    fluidRow(
      column(4,
             numericInput(inputId = "n",
                          label = "sample size",
                          min = 30,
                          value = 20),
             actionButton(inputId = "make_sample",
                          label = "see a sample")),
      column(7,
             plotOutput(outputId = "empir_data")
             )
             ),
  fluidRow(
    column(4,
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
                        value = 20),
           radioButtons(inputId = "switch",
                        label = "What graph would you like to see?",
                        choices = c("Bootstrap",
                                    "CLT"))
           ),
    column(7,
           plotOutput(outputId = "bs_or_clt"),
          plotOutput(outputId = "bs"),
          plotOutput(outputId = "clt"))
           )
  )


server <- function(input, output, session) {
  
  pop <- reactive({
    
    a <- rgamma(input$N, shape = input$skew, rate = 1)
    
    df <- data.frame(pop = a,
                     inv_pop = -a)
    
    df
    
  })
  
  output$pop_plot <- renderPlot({
    
    
    if(input$invert == "invert"){
      
      pop() %>% 
        ggplot(aes(x = inv_pop))+
        geom_histogram(color = "white",
                       fill = "blue")+
        theme_classic()
      
    } else {
      
      pop() %>% 
        ggplot(aes(x = pop))+
        geom_histogram(color = "white",
                       fill = "blue")+
        theme_classic()
      
    }
    
  })
  
  samp <- eventReactive(input$make_sample, {
    
    if(input$invert == "invert"){
      
      sample(pop()$inv_pop, input$n, replace = TRUE)
      
    } else {
      
      sample(pop()$pop, input$n, replace = TRUE)
      
    }
    
  })
  
  sampdf <- eventReactive(input$make_sample, {
    
    d <- sample(pop()$pop, input$n, replace = TRUE)
    e <- sample(pop()$inv_pop, input$n, replace = TRUE)
    
    df2 <- data.frame(d = d,
               e = e)
    
    df2
    
  })
  
  output$empir_data <- renderPlot({
    
    if(input$invert == "invert"){
      
      sampdf() %>% 
        ggplot(aes(x = e))+
        geom_histogram(color = "white",
                       fill = "green3")+
        theme_classic()
      
    } else {
      
      sampdf() %>% 
        ggplot(aes(x = d))+
        geom_histogram(color = "white",
                       fill = "green3")+
        theme_classic()
      
    }
    
  })
  
  bootstrap <- reactive({
    
    b <- replicate(input$boot_reps,
              mean(sample(samp(), input$sample_from_sample, replace = TRUE)))
    
    c <- data.frame(boots = b)
    
    c
    
  })
  
  central_limit_thereom <- reactive({
    
    
    replicate(input$clt_draws,
              mean(sample(pop(), input$n, replace = TRUE)))
    
  })
  
  output$bs_or_clt <- renderPlot({
    
    if(input$switch == "CLT"){
      
      hist(central_limit_thereom())
      
    } else {
      
      bootstrap() %>% 
        ggplot(aes(x = boots))+
        geom_histogram(color = "white",
                       fill = "orangered")+
        theme_classic()
      
    }
    
    
  })
  
}



shinyApp(ui = ui, server = server)