library(shiny)
library(shinycssloaders)
library(tidyverse)
library(shinydashboard)

ui <- dashboardPage(
  
  dashboardHeader(title = "Stat 20 Apps"),
  
  # ===============================================
  #                    SIDEBAR
  # ===============================================
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Law of Large Numbers",
               tabName = "LLN"),
      menuItem("Hypothesis Testing",
               tabName = "HT")
    )
  ),
  
  # ===============================================
  #     LAW OF LARGE NUMBERS UI
  # ===============================================
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "LLN",
        fluidRow(
          box(
            numericInput(inputId = "obs",
                         label = "how any times would you like to do the your trials",
                         min = 2,
                         value = 30),
            numericInput(inputId = "prob",
                         label = "what is the probability of success?",
                         min = 0,
                         value = .5,
                         max = 1),
            actionButton(inputId = "go_LLN",
                         label = "Compute"),
            width = 4
          ),
          box(
            withSpinner(plotOutput("plot")),
            width = 8
          )
        )
      ),
      
      # ===============================================
      #      HYPOTHESIS TESTING UI
      # ===============================================
      
      tabItem(
        tabName = "HT",
        fluidRow(
          valueBoxOutput("alpha"),
          valueBoxOutput("beta"),
          valueBoxOutput("power")
        ),
        fluidRow(
          box(sliderInput(inputId = "diff",
                          label = "difference between hypothesis'",
                          value = 1,
                          min = 1,
                          max = 5,
                          step = 0.1,
                          ticks = FALSE,
                          animate = TRUE),
              sliderInput(inputId = "alpha",
                          label = "Level of Significance (alpha)",
                          value = 0.05,
                          min = 0.01,
                          max = 0.25,
                          ticks = FALSE,
                          animate = TRUE,
                          step = 0.01),
              sliderInput(inputId = "samp_size",
                          label = "sample size",
                          value = 10,
                          min = 10,
                          max = 500,
                          step = 10,
                          animate = TRUE,
                          ticks = FALSE),
              checkboxGroupInput(inputId = "show",
                                 label = "",
                                 choices = c("Alpha",
                                             "Beta",
                                             "Power"),
                                 selected = c("Alpha")),
              width = 4,
              height = NULL
          ),
          box(
            plotOutput(outputId = "graph"),
            width = 8,
            height = NULL
          )
        )
        
      )
    )
  )
  
  
  
  
)



server <- function(input, output){
  
  # ===============================================
  #      LAW OF LARGE NUMBERS UI
  # ===============================================
  
  exp <- eventReactive(input$go_LLN, {
    
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
      theme_classic()+
      ylab("cumulative mean of the probabilities")+
      xlab("trials completed")
    
  }) %>% 
    bindEvent(input$go_LLN)
  
 
  
  # ===============================================
  #          HYPOTHESIS TESTING SERVER
  # ===============================================
  
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
    
    plot +
      theme(axis.title.y = element_blank(),
            axis.title.x = element_blank())
    
  })
  
  
  output$alpha <- renderValueBox({
    
    alpha_level <- input$alpha*100
    
    valueBox(
      value = paste0(alpha_level, "%"),
      subtitle = "Type I error rate (alpha)"
    )
    
  })
  
  output$beta <- renderValueBox({
    
    point <- 1 - input$alpha
    x_point <- df() %>% 
      filter(x_1 <= qnorm(p = point, mean = 0, sd = 5/(sqrt(input$samp_size)))) %>% 
      select(x_1) %>% 
      summarise(x_1 = max(x_1))
    
    beta <- round(pnorm(x_point$x_1[1], mean = input$diff, sd = 5/(sqrt(input$samp_size)))*100, 2)
    
    valueBox(
      value = paste0(beta, "%"),
      subtitle = "Type II error rate (beta)"
    )
    
  })
  
  output$power <- renderValueBox({
    
    point <- 1 - input$alpha
    x_point <- df() %>% 
      filter(x_1 >= qnorm(p = point, mean = 0, sd = 5/(sqrt(input$samp_size)))) %>% 
      select(x_1) %>% 
      summarise(x_1 = min(x_1))
    
    power <- round((1 - pnorm(x_point$x_1[1], mean = input$diff, sd = 5/(sqrt(input$samp_size))))*100, 2)
    
    valueBox(
      value = paste0(power, "%"),
      subtitle = "Power"
    )
    
  })
  
  
  
}

shinyApp(ui = ui,
         server = server)
