library(shiny)
library(tidyverse)
library(grDevices)


ui <- fluidPage(
  br(),
  fluidRow(
    h3("Population"),
    column(4,
      numericInput(inputId = "N",
                   label = "total pop",
                   min = 1,
                   value = 10000),
      numericInput(inputId = "skew",
                   label = "skew your population",
                   min = 1,
                   value = 1),
      radioButtons(inputId = "invert",
                   label = "Invert the population (this only changes the distribution, allowing population to be more easily left skewd)",
                   choices = c("non-inverted",
                               "invert"))
      ),
      column(7,
        plotOutput(outputId = "pop_plot")
      )
      ),
  br(),
    fluidRow(h3("Empiracal Data"),
      column(4,
             numericInput(inputId = "n",
                          label = "sample size",
                          min = 30,
                          value = 100),
             actionButton(inputId = "make_sample",
                          label = "see a sample")),
      column(7,
             plotOutput(outputId = "empir_data")
             )
             ),
  br(),
  fluidRow(h3("Bootstrap or Central Limit Thereom"),
    column(4,
           selectInput(inputId = "switch1",
                       label = "View bootstrap model or the Central Limit Thereom",
                       choices = c("Bootstrap",
                                   "Central Limit Thereom")),
           uiOutput(outputId = "options"),
           ),
    column(7,
           plotOutput(outputId = "bs_or_clt"),
           )
  ),
  br(),
  fluidRow(h3("Statistics"),
           column(4,
                  verbatimTextOutput(outputId = "mew")),
           column(4,
                  verbatimTextOutput(outputId = "bsmean")),
           column(4,
                  verbatimTextOutput(outputId = "cltmean"))
  ))


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
        theme_classic()+
        xlab("")+
        ggtitle("Distribution of the Poputlation")
      
    } else {
      
      pop() %>% 
        ggplot(aes(x = pop))+
        geom_histogram(color = "white",
                       fill = "blue")+
        theme_classic()+
        xlab("")+
        ggtitle("Distribution of the Poputlation")
      
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
        theme_classic()+
        xlab("")+
        ggtitle("A Sample")
      
    } else {
      
      sampdf() %>% 
        ggplot(aes(x = d))+
        geom_histogram(color = "white",
                       fill = "green3")+
        theme_classic()+
        xlab("")+
        ggtitle("A Sample")
      
    }
    
  }) %>% 
    bindEvent(input$make_sample)
  
  output$options <- renderUI({
      
    
    if(input$switch1 == "Bootstrap"){
      
      tagList(
        numericInput(inputId = "sample_from_sample",
                     label = "how big of a sample do you want to draw from your sample",
                     min = 1,
                     value = 50),
        numericInput(inputId = "boot_reps",
                     label = "How many bootstrap reps would you like",
                     min = 1,
                     value = 100),
      actionButton(inputId = "sim1",
                   label = "produce simulation"))
      
    }else if(input$switch1 == "Central Limit Thereom"){
      
      tagList(numericInput(inputId = "clt_draws",
                           label = "number of samples to draw from population",
                           min = 1,
                           value = 200),
              actionButton(inputId = "sim2",
                           label = "produce simulation"))
      
    }
    
  })
  
  bootstrap <- eventReactive(input$sim1, {
    
    b <- replicate(input$boot_reps,
                   mean(sample(samp(), input$sample_from_sample, replace = TRUE)))
    
    c <- data.frame(boots = b)
    
    c
    
  })
  
  central_limit_thereom <- eventReactive(input$sim2, {
    
    f <- replicate(input$clt_draws,
                   mean(sample(pop()$pop, input$n, replace = TRUE)))
    g <- replicate(input$clt_draws,
                   mean(sample(pop()$inv_pop, input$n, replace = TRUE)))
    
    cltdf <- data.frame(f = f,
                        g = g)
    
    cltdf
  })
  
  output$bs_or_clt <- renderPlot({
    
    if(input$switch1 == "Central Limit Thereom" & input$invert == "invert"){
      
      central_limit_thereom() %>% 
        ggplot(aes(x = g))+
        geom_histogram(color = "white",
                       fill = "purple")+
        theme_classic()+
        ggtitle("Central Limit Thereom")+
        xlab(bquote(bar(x)))
      
    }else if (input$switch1 == "Central Limit Thereom" & input$invert == "non-inverted"){
      
      central_limit_thereom() %>% 
        ggplot(aes(x = f))+
        geom_histogram(color = "white",
                       fill = "purple")+
        theme_classic()+
        ggtitle("Central Limit Thereom")+
        xlab(bquote(bar(x)))
      
    } else if(input$switch1 == "Bootstrap") {
      
      bootstrap() %>% 
        ggplot(aes(x = boots))+
        geom_histogram(color = "white",
                       fill = "orangered")+
        theme_classic()+
        ggtitle("Bootstrap Model")+
        xlab(bquote(bar(x)))
      
    }
    
    
  })
  
  
  output$mew <- renderPrint({
    
    if(input$invert == "non-inverted"){
      
      cat("µ:", mean(pop()$pop))
      
    }else if(input$invert == "invert"){
      
      cat("µ:", mean(pop()$inv_pop))
      
    }
    
  })
  
  output$cltmean <- renderPrint({
    
    if(input$switch1 == "Central Limit Thereom" & input$invert == "invert"){
      
      cat("CLT mean:", mean(central_limit_thereom()$g))
      
    } else if (input$switch1 == "Central Limit Thereom" & input$invert == "non-inverted"){
      
      cat("CLT mean:", mean(central_limit_thereom()$f))
    }
      
    
  }) %>% 
    bindCache(input$sim2)
  
  output$bsmean <- renderPrint({
    
    cat("Bootstrap mean:", mean(bootstrap()$boots))
    
  }) %>% 
    bindCache(input$sim1)
  
  
}



shinyApp(ui = ui, server = server)