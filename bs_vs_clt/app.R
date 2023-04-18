library(shiny)
library(tidyverse)
library(grDevices)


ui <- fluidPage(
  br(),
  # ===============================================
  # Population UI: uses rgamma() to show a graphic of a population.
  #Feature population skew that is tied to 'shape' argument of rgamma()
  #Features population amount N
  #gives the ability to toggle left skew or rights skew
  # ===============================================
  fluidRow(
    h3("Population"),
    column(4,
      numericInput(inputId = "N",
                   label = "total pop",
                   min = 1,
                   value = 10000),
      numericInput(inputId = "normalize",
                   label = "normalize your population",
                   min = 1,
                   value = 1),
      #toggle left-skew right-skew
      radioButtons(inputId = "skew",
                   label = "Invert the population (this only changes the distribution, allowing population to be more easily left skewd)",
                   choices = c("right-skew",
                               "left-skew"))
      ),
      column(7,
        plotOutput(outputId = "pop_plot")
      )
      ),
  br(),
  # ===============================================
  # Empirical data UI.
  # User chooses sample size and is given a button to make the sample
  # ===============================================
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
  # ===============================================
  # "god-mode" working name... this is to show a student what it would look like to
  # have infinite resources to take as many sample of size "n" (chosen in Empirical data)
  # ===============================================
  fluidRow(h3("God-mode"),
           column(4,
                  actionButton(inputId = "god",
                               label = "Simulate ideal expirement")),
           column(7,
                  plotOutput(outputId = "godmode"),)),
  br(),
  # ===============================================
  # !!!!This will be reworked to show the bootsrap model and a CLT x~N(x_bar, s/sgr(n)) overlayed
  # ===============================================
  fluidRow(h3("Bootstrap simulation"),
           column(4,
                  numericInput(inputId = "bs_samps",
                               label = "How many boot-strap repititions would you like",
                               min = 1,
                               value = 100),
                  numericInput(inputId = "bs_n",
                               label = "size of bootstrap samples",
                               value = 30),
                  actionButton(inputId = "sim_bs",
                               label = "Simulate the Bootstrap")),
           column(7,
                  checkboxInput(inputId = "clt_plot",
                                label = "Plot CLT over your bootstrap",
                                value = FALSE),
                  plotOutput(outputId = "bs_plot"))
  ),
  br(),
  # ===============================================
  #still not sure what this should be....
  # ===============================================
  fluidRow(h3("Statistics"),
           
  ))


# ===============================================
#                    SEVER
# ===============================================

server <- function(input, output, session) {
  
  # ===============================================
  # population reactive
  # creates a gamma population using rgamma(), arguments are inputs from ui
  # rate is kept at 1
  # left & right skew are made by making the rgamma() object negative
  # a population plot is then spat out with output$pop_plot
  # this renderplot object uses a if else statement to account for a right or left skew
  # ===============================================
  pop <- reactive({
    
    a <- rgamma(input$N, shape = input$normalize, rate = 1)
    
    df <- data.frame(right = a,
                     left = -a)
    
    df
    
  })
  
  output$pop_plot <- renderPlot({
    
    
    if(input$skew == "left-skew"){
      
      pop() %>% 
        ggplot(aes(x = left))+
        geom_histogram(color = "white",
                       fill = "blue")+
        theme_classic()+
        xlab("")+
        ggtitle("Distribution of the Poputlation")
      
    } else {
      
      pop() %>% 
        ggplot(aes(x = right))+
        geom_histogram(color = "white",
                       fill = "blue")+
        theme_classic()+
        xlab("")+
        ggtitle("Distribution of the Poputlation")
      
    }
    
  })
  
  # ===============================================
  # Empiracla part of the serveer
  # samp() pulls a sample from either the right or left skewed pop
  # sampdf() makes a data frame for sample from both left and right skew
  # the render plot object plots the sample from sampdf for either left or right skewed pop
  # ===============================================
  
  samp <- reactive({
    
    if(input$skew == "left-skew"){
      
      sample(pop()$left, input$n, replace = TRUE)
      
    } else if(input$skew == "right-skew") {
      
      sample(pop()$right, input$n, replace = TRUE)
      
    }
    
  }) %>% 
    bindEvent(input$make_sample)
  
  sampdf <- reactive({
    
    d <- sample(pop()$right, input$n, replace = TRUE)
    e <- sample(pop()$left, input$n, replace = TRUE)
    
    df2 <- data.frame(d = d,
               e = e)
    
    df2
    
  }) %>% 
    bindEvent(input$make_sample)
  
  output$empir_data <- renderPlot({
    
    if(input$skew == "left-skew"){
      
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
  
  # ===============================================
  # goddf() creates a df of ten thousand sample means from population
  # render plot objects then plots it
  # ===============================================
  
  goddf <- reactive({
    
    s <- replicate(10000, mean(sample(pop()$right, input$n, replace = TRUE)))
    
    t <- replicate(10000, mean(sample(pop()$left, input$n, replace = TRUE)))
    
    zdf <- data.frame(s = s,
                      t = t)
    zdf
    
  }) %>% 
    bindEvent(input$god)
  
  
  output$godmode <- renderPlot(
    
    if(input$skew == "left-skew"){
      
      goddf() %>% 
        ggplot(aes(x = t))+
        geom_histogram(color = "white",
                       fill = "purple")+
        theme_classic()
      
    } else if(input$skew == "right-skew") {
      
      goddf() %>% 
        ggplot(aes(x = s))+
        geom_histogram(color = "white",
                       fill = "purple")+
        theme_classic()
      
    }
  ) %>% 
    bindEvent(input$god)
  
  # ===============================================
  # bootstrap model & CLT overlay
  # ===============================================
  
  bootstrap <- reactive({
    
    bs <- replicate(input$bs_samps,
                    mean(sample(samp(), input$bs_n, replace = TRUE)))
    
    l <- seq(from = mean(samp()) - 3*(sd(samp())/sqrt(input$n)),
             to = mean(samp()) + 3*(sd(samp())/sqrt(input$n)),
             length = 100)
    
    m <- dnorm(l,
               mean = mean(samp()),
               sd = sd(samp())/sqrt(input$n))
    
    bsdf <- data.frame(bs = bs,
                       m = m,
                       l = l)
    
    bsdf
    
  }) %>% 
    bindEvent(input$sim_bs)
  
  
  
  
  output$bs_plot <- renderPlot({
    
    if(input$clt_plot == "TRUE"){
      
      bootstrap() %>% 
        ggplot()+
        geom_histogram(aes(x = bs,
                           y = ..density..),
                       color = "white",
                       fill = "orangered")+
        geom_line(aes(x = l,
                      y = m),
                  size = 2)+
        theme_classic()
      
    } else{
      
      bootstrap() %>% 
        ggplot()+
        geom_histogram(aes(x = bs,
                           y = ..density..),
                       color = "white",
                       fill = "orangered")+
        theme_classic()
      
    }
    
  })


}

shinyApp(ui = ui, server = server)