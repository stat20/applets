
library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

  sidebarLayout(
    sidebarPanel(
    sliderInput(inputId = "diff",
                label = "difference between hypothesis'",
                value = 1,
                min = 1,
                max = 10),
    sliderInput(inputId = "alpha",
                label = "Level of Significance (alpha)",
                value = 0.05,
                min = 0.01,
                max = 0.25),
    sliderInput(inputId = "samp_size",
                label = "sample size",
                value = 10,
                min = 10,
                max = 500),
    checkboxGroupInput(inputId = "show",
                       label = "",
                       choices = c("Alpha",
                                   "Beta",
                                   "Power"),
                       selected = c("Alpha"))
  ),
  mainPanel(
    plotOutput(outputId = "graph")
  )
    
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  df <- reactive({
    
    mew_0 <- 0
    mew_1 <- mew_0 + input$diff
    sd <- 5/(sqrt(input$samp_size))
    
    x_0 <- seq(from = mew_0 - 3.25*sd, to = mew_0 + 3.25*sd, by = 0.01)
    x_1 <- seq(from = mew_1 - 3.25*sd, to = mew_1 + 3.25*sd, by = 0.01)
    
    dist_0 <- dnorm(x = x_0, mean = mew_0, sd = sd)
    dist_1 <- dnorm(x = x_1, mean = mew_1, sd = sd)
    
    df <- data.frame(x_0,
                     dist_0,
                     x_1,
                     dist_1)
    df
    
  })

  
  output$graph <- renderPlot({
    
    mew_0 <- 0
    point <- 1 - input$alpha
    sd <- 5/(sqrt(input$samp_size))
    
    alpha <- "Alpha" %in% input$show
    beta <- "Beta" %in% input$show
    power <- "Power" %in% input$show
    
    plot <- df() %>% 
      ggplot()+
      geom_line(aes(x = x_0,
                    y = dist_0),
                color = "lightgray",
                size = .75)+
      geom_line(aes(x = x_1,
                    y = dist_1),
                color = "darkblue",
                size = .75)+
      geom_vline(xintercept = qnorm(p = point, mean = mew_0, sd = sd),
                 color = "red",
                 size = 1)+
      theme_classic()
    
    if(alpha & beta & power){
      
     plot <-  plot +
        geom_ribbon(data = df() %>% filter(x_0 >= qnorm(p = point, mean = mew_0, sd = sd)),
                    aes(x = x_0,
                        ymin = 0,
                        ymax = dist_0),
                    fill = "red",
                    alpha = .3)+
        geom_ribbon(data = df() %>% filter(x_1 <= qnorm(p = point, mean = mew_0, sd = sd)),
                    aes(x = x_1,
                        ymin = 0,
                        ymax = dist_0),
                    fill = "darkorange",
                    alpha = .3)+
        geom_ribbon(data = df() %>% filter(x_1 >=qnorm(p = point, mean = mew_0, sd = sd)),
                    aes(x = x_1,
                        ymin = 0,
                        ymax = dist_0),
                    fill = "darkgreen",
                    alpha = 0.3)
      
    }else if(alpha & beta){
      
      plot <- plot+
        geom_ribbon(data = df() %>% filter(x_0 >= qnorm(p = point, mean = mew_0, sd = sd)),
                    aes(x = x_0,
                        ymin = 0,
                        ymax = dist_0),
                    fill = "red",
                    alpha = .3)+
        geom_ribbon(data = df() %>% filter(x_1 <= qnorm(p = point, mean = mew_0, sd = sd)),
                    aes(x = x_1,
                        ymin = 0,
                        ymax = dist_0),
                    fill = "darkorange",
                    alpha = .3)
      
    }else if(alpha & power){
      
      plot <- plot+
        geom_ribbon(data = df() %>% filter(x_0 >= qnorm(p = point, mean = mew_0, sd = sd)),
                    aes(x = x_0,
                        ymin = 0,
                        ymax = dist_0),
                    fill = "red",
                    alpha = .3)+
        geom_ribbon(data = df() %>% filter(x_1 >=qnorm(p = point, mean = mew_0, sd = sd)),
                    aes(x = x_1,
                        ymin = 0,
                        ymax = dist_0),
                    fill = "darkgreen",
                    alpha = 0.3)
      
    }else if(beta & power){
      
      
      plot <- plot+
        geom_ribbon(data = df() %>% filter(x_1 <= qnorm(p = point, mean = mew_0, sd = sd)),
                    aes(x = x_1,
                        ymin = 0,
                        ymax = dist_0),
                    fill = "darkorange",
                    alpha = .3)+
        geom_ribbon(data = df() %>% filter(x_1 >=qnorm(p = point, mean = mew_0, sd = sd)),
                    aes(x = x_1,
                        ymin = 0,
                        ymax = dist_0),
                    fill = "darkgreen",
                    alpha = 0.3)
      
    }else if(alpha){
      
      plot <- plot+
        geom_ribbon(data = df() %>% filter(x_0 >= qnorm(p = point, mean = mew_0, sd = sd)),
                    aes(x = x_0,
                        ymin = 0,
                        ymax = dist_0),
                    fill = "red",
                    alpha = .3)
      
    }else if(beta){
      
      plot <- plot+
        geom_ribbon(data = df() %>% filter(x_1 <= qnorm(p = point, mean = mew_0, sd = sd)),
                    aes(x = x_1,
                        ymin = 0,
                        ymax = dist_0),
                    fill = "darkorange",
                    alpha = .3)
      
    }else if(power){
      
      plot <- plot+
        geom_ribbon(data = df() %>% filter(x_1 >=qnorm(p = point, mean = mew_0, sd = sd)),
                    aes(x = x_1,
                        ymin = 0,
                        ymax = dist_0),
                    fill = "darkgreen",
                    alpha = 0.3)
      
    }
    
    plot
    
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
