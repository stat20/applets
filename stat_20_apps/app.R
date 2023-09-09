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
               tabName = "HT"),
      menuItem("Distributions",
               menuSubItem("Uniform",
                           tabName = "Uniform"),
               menuSubItem("Binomial",
                           tabName = "Bin"),
               menuSubItem("Normal",
                           tabName = "norm"))
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
        
      ),
      tabItem(tabName = "Uniform",
              fluidRow(
                
                # ===============================================
                # Latex help
                # section below allows in-line LaTeX via $ in mathjax. Replace less-than-sign with < 
                # and grater-than-sign with >
                # unfortunately I can't figure out how to use () just as a parenthesis so I have to opt
                # for [] 
                # ===============================================
                
                withMathJax(),
                tags$script("
              MathJax.Hub.Config({
              tex2jax: {
              inlineMath: [['$','$'], ['\\(','\\)']],
              processEscapes: true
              }
              });"),
                
                # ===============================================
                #      UNIFORM UI
                # ===============================================
                
                tabBox(id = "tabbox1",
                       tabPanel("PDF",
                                plotOutput("unif_pdf_plot")
                       ),
                       tabPanel("CDF",
                                plotOutput("unif_cdf_plot")
                       ),
                       width = 8
                ),
                box(
                  width = 4,
                  p(uiOutput("unif_dist")),
                  numericInput(inputId = "a",
                               label = "a",
                               value = 5),
                  numericInput(inputId = "b",
                               label = "b",
                               value = 10)
                )
              ),
              fluidRow(
                box(
                  width = 4,
                  uiOutput("unif_pdf"),
                  uiOutput("unif_cdf"),
                  uiOutput("unif_exp"),
                  uiOutput("unif_var")
                ),
                box(
                  width = 8
                )
              )
      ),
      tabItem(tabName = "Bin",
              box())
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
  
  # ===============================================
  #          UNIF server
  # ===============================================
  
  
  output$unif_dist <- renderUI({
    
    withMathJax(
      sprintf("$$Uniform(a = %g, b = %g)$$",
              input$a,
              input$b)
    )
    
  })
  
  output$unif_pdf_plot <- renderPlot({
    
    x <- seq(from = input$a-2,
             to = input$b+2,
             by=0.01)
    
    dens <- dunif(x, min = input$a, max = input$b)
    
    df <- data.frame(x = x,dens = dens)
    
    df %>% 
      ggplot(aes(x = x,
                 y = dens))+
      geom_line()+
      ylim(0,1)+
      theme_classic()
    
    
  })
  
  unif_df <- reactive({
    
    a <- input$a
    b <- input$b
    
    x_x <- seq(from = a,
               to = b,
               by = 0.01)
    
    cdf_x <- (x_x - a)/(b-a)
    xxx <- seq(from = a,
               to = b ,
               length.out = length(cdf_x))
    
    udf <- data.frame(cdf_x,
                      xxx)
    udf
    
  })
  
  output$unif_cdf_plot <- renderPlot({
    
    
    unif_df() %>% 
      ggplot(aes(x = xxx,
                 y = cdf_x))+
      geom_line(size = 1)+
      theme_classic()+
      xlim(input$a-2,
           input$b+2)+
      geom_segment(aes(y=0, x=input$a -2, yend = 0, xend=input$a),
                   size = 1)+
      geom_segment(aes(y = 1, x = input$b, yend = 1, xend = input$b+2),
                   size = 1)
    
    
  })
  
  output$unif_pdf <- renderUI({
    
    withMathJax(
      sprintf("$$PDF: \\frac{1}{b-a} = \\frac{1}{%g - %g}$$",
              input$b,
              input$a)
    )
    
  })
  
  output$unif_cdf <- renderUI({
    
    withMathJax(
      sprintf("$$CDF: \\frac{x-a}{b-a} = \\frac{x-%g}{%g-%g}$$",
              input$a,
              input$b,
              input$a)
    )
    
  })
  
  output$unif_exp <- renderUI({
    
    withMathJax(
      sprintf("$$Expectation: \\frac{a+b}{2} = \\frac{%g + %g}{2}$$",
              input$a,
              input$b)
    )
    
  })
  
  output$unif_var <- renderUI({
    
    withMathJax(
      sprintf("$$Variance: \\frac{(b -a)^2}{12} = \\frac{(%g - %g)^2}{12}$$",
              input$b,
              input$a)
    )
    
  })
  
}

shinyApp(ui = ui,
         server = server)
