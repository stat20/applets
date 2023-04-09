library(shiny)
library(tidyverse)


ui <- fluidPage(
  titlePanel("Law of Large Numbers"),
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "obs",
                   label = "how any times would you like to do the your trials",
                   min = 2,
                   value = 30),
      numericInput(inputId = "prob",
                   label = "what is the probability of success?",
                   min = 0,
                   value = .5,
                   max = 1)
    ),
    mainPanel(
      plotOutput("plot")
    )
  )

    
)


server <- function(input, output) {

  exp <- reactive({
    
    trials <- sample(c(0,1),
                     input$obs,
                     replace = TRUE,
                     prob = c(1-input$prob, input$prob))
    
    cum_sum <- cumsum(trials)
    trials_done <- c(1:input$obs)
    cum_mean <- cum_sum / trials_done
    
    df <- data.frame(trials_done = trials_done,
                     cum_mean = cum_mean)
    
    df
    
  })
  
  output$plot <- renderPlot({
    
    exp() %>% 
      ggplot(aes(x = trials_done, y = cum_mean))+
      geom_line()+
      geom_abline(intercept = input$prob,
                  slope = 0,
                  color = "red")+
      scale_y_continuous(limits = c(0,1))+
      theme_classic()
    
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
