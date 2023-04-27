library(shiny)
library(tidyverse)
library(grDevices)
library(gganimate)

# ===============================================
#                    UI
# ===============================================

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
      sliderInput(inputId = "normalize",
                   label = "Change the skew of your population",
                   min = 0,
                   value =1,
                   max = 1),
      radioButtons(inputId = "skew",
                   label = "How would you like your skew?",
                   choices = c("right-skew",
                               "left-skew"))
      ),
      column(7,
        plotOutput(outputId = "pop_plot")
      )
      ),
  br(),
  # ===============================================
  # "god-mode" working name... this is to show a student what it would look like to
  # have infinite resources to take as many sample of size "n" (chosen in Empirical data)
  # ===============================================
  fluidRow(h3("In a world"),
           h6("If you had unlimited resources, funding, time, etc.
              you would be able to draw as many samples as you would like from this population
              here, it's 10000"),
           column(4,
                  actionButton(inputId = "god",
                               label = "Simulate ideal expirement")),
           column(7,
                  plotOutput(outputId = "godmode"),)),
  br(),
  # ===============================================
  # Empirical data UI.
  # User chooses sample size and is given a button to make the sample
  # ===============================================
    fluidRow(h3("Empirical Data"),
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
  # tab panels that show different ways to visualize, use your sample
  # ===============================================
  tabsetPanel(type = "tabs",
              tabPanel("Bootstrap Simulation",
                       br(),
                       fluidRow(column(4,
                                       p("Your bootstrap sample size:", textOutput("bs_size", inline = T)),
                                       numericInput(inputId = "bs_samps",
                                                    label = "How many boot-strap repititions would you like",
                                                    min = 1,
                                                    value = 500),
                                       actionButton(inputId = "sim_bs",
                                                    label = "Simulate the Bootstrap")
                                       ),
                                column(7,
                                       plotOutput(outputId = "bs_plot"),
                                       p("mean:",textOutput(outputId = "bs_mean", inline = T)),
                                       p("SD:", textOutput(outputId = "bs_sd", inline = T))
                                       ),
                                ),
              ),
              tabPanel("Central Limit Theorem",
                       br(),
                       actionButton(inputId = "sim_clt",
                                    label = "Simulate the Central Limit Theorem"),
                       p("The CLT follows a normal distribtion:" ,uiOutput(outputId = "clt_norm")),
                       plotOutput("CLT")
                       )
              ),
  br(),
  # ===============================================
  # !!!!This will be reworked to show the bootstrap model and a CLT x~N(x_bar, s/sgrt(n)) overlayed
  # ===============================================
  fluidRow(h3("Lets Compare!"),
           column(4,
                  checkboxGroupInput(inputId = "comp_choice",
                                     label = "Choose your overlay",
                                     choices = c("Ideal World",
                                                 "Bootstrap",
                                                 "Central Limit Theorem")
                                     )
                  ),
           column(7,
                  plotOutput("comp_graph")
                  )
  ),
  br(),
  # ===============================================
  #still not sure what this should be....
  # ===============================================
  fluidRow(h3("Statistics"),
           column(10,
                  p("A formula for a 95% confidence Interval is", uiOutput(outputId = "CI")),
                  p("95% Confidence Interval for the Idea World is", uiOutput(outputId = "CI_ideal")),
                  p("95% Confidence Interval for the Bootstrap is", uiOutput(outputId = "CI_bs")),
                  p("95% Confidence Interval for Central Limit Theorem is", uiOutput(outputId = "CI_clt"))
                  )
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
    
    a <- rgamma(input$N, shape = input$normalize*100, rate = 1)
    
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
  # goddf() creates a df of ten thousand sample means from population
  # render plot objects then plots it
  # ===============================================
  
  goddf <- reactive({
    
    if(input$skew == "right-skew"){
      
      s <- replicate(10000, mean(sample(pop()$right, input$n, replace = TRUE)))
      
    } else if(input$skew == "left-skew"){
      
      s <- replicate(10000, mean(sample(pop()$left, input$n, replace = TRUE)))
      
    }
    
    zdf <- data.frame(s = s)
    zdf
    
  }) %>% 
    bindEvent(input$god)
  
  
  output$godmode <- renderPlot(
    
    goddf() %>% 
      ggplot(aes(x = s))+
      geom_histogram(color = "white",
                     fill = "purple")+
      theme_classic()
    
  ) %>% 
    bindEvent(input$god)
  
  # ===============================================
  # Empirical part of the server
  # samp() pulls a sample from either the right or left skewed pop
  # sampdf() makes a data frame for sample from both left and right skew
  # the render plot object plots the sample from sampdf for either left or right skewed pop
  # ===============================================
  
  samp <- reactive({
    
    if(input$skew == "left-skew"){
      
      samp <- sample(pop()$left, input$n, replace = TRUE)
      
    } else if(input$skew == "right-skew") {
      
      samp <- sample(pop()$right, input$n, replace = TRUE)
      
    }
    
    samp_df <- data.frame(samp = samp)
    samp_df
    
  }) %>% 
    bindEvent(input$make_sample)
  
  output$empir_data <- renderPlot({
    
    samp() %>% 
      ggplot(aes(x = samp))+
      geom_histogram(color = "white",
                     fill = "green3")+
      theme_classic()+
      xlab("x")+
      ggtitle("A Sample")
    
  }) %>% 
    bindEvent(input$make_sample)
  
  
  # ===============================================
  # bootstrap model & CLT creation
  # ===============================================
  
  dataframe <- reactive({
    
    bs <- replicate(input$bs_samps,
                    mean(sample(samp()$samp, input$n, replace = TRUE)))
    
    l <- seq(from = mean(samp()$samp) - 3*(sd(samp()$samp)/sqrt(input$n)),
             to = mean(samp()$samp) + 3*(sd(samp()$samp)/sqrt(input$n)),
             length = 100)
    
    m <- dnorm(l,
               mean = mean(samp()$samp),
               sd = sd(samp()$samp)/sqrt(input$n))
    
    bsdf <- data.frame(bs = bs,
                       m = m,
                       l = l)
    
    bsdf
    
  }) %>% 
    bindEvent(input$sim_bs,
              input$sim_clt)
  
  # ===============================================
  # bootstrap model
  # ===============================================
  
  output$bs_size <- renderText({
    
    return(input$n)
    
  })
  
  output$bs_plot <- renderPlot({
    
      
      dataframe() %>% 
        ggplot()+
        geom_histogram(aes(x = bs,
                           y = ..density..),
                       color = "white",
                       fill = "orangered")+
        theme_classic()+
        xlab(bquote(bar(x)))+
        ylab("")
    
  }) %>% 
    bindEvent(input$sim_bs)
  
  output$bs_mean <- renderText({
    
    return(round(mean(dataframe()$bs), 2))
    
  }) %>% 
    bindEvent(input$sim_bs)
  
  output$bs_sd <- renderText({
    
    return(round(sd(dataframe()$bs), 2))
    
  }) %>% 
    bindEvent(input$sim_bs)
  
  # ===============================================
  # central limit theorem
  # ===============================================
  
  output$CLT <- renderPlot({
    
    dataframe() %>% 
      ggplot()+
      geom_line(aes(x = l,
                    y = m),
                size = 1,
                color = "black")+
      theme_classic()+
      ylab("")
    
  }) %>% 
    bindEvent(input$sim_clt)
  
  output$clt_norm <- renderUI({
    
    withMathJax(
      sprintf("$$\\mathcal{N}(\\mu = \\bar{X} = %g,\\sigma = \\frac{s}{\\sqrt{n}} = %g)$$",
              round(mean(samp()$samp), 2),
              round(sd(samp()$samp)/sqrt(input$n), 2))
    )
    
  }) %>% 
    bindEvent(input$sim_clt)

  # ===============================================
  # graph magnum opus
  # ===============================================

  output$comp_graph <- renderPlot({
    
    plot <- ggplot()
    
    if(input$comp_choice == "Ideal World"){
      
      plot <- plot +
        geom_histogram(data = goddf(),
                       mapping = aes(x = s,
                                     y = ..density..),
                       color = "white",
                       fill = "purple")
      
    } if (input$comp_choice %in% c("Ideal World","Bootstrap")){
      
      plot <- plot +
        geom_histogram(data = goddf(),
                       mapping = aes(x = s,
                                     y = ..density..),
                       color = "white",
                       fill = "purple")+
        geom_histogram(data = dataframe(),
                       mapping = aes(x = bs,
                                     y = ..density..),
                       color = "white",
                       fill = "orangered")
      
    }else{
      
      
    }
    
    
    
    plot + theme_classic()
    
  })
  
  
  # ===============================================
  # List of Confidence intervals
  # ===============================================
  
  output$CI <- renderUI({
    
    withMathJax(
      sprintf("$$(\\mu - 1.96\\frac{\\sigma}{\\sqrt{n}}, \\mu + 1.96\\frac{\\sigma}{\\sqrt{n}})$$")
    )
    
  })
  
  output$CI_ideal <- renderUI({
    
    withMathJax(
      sprintf(
        "$$(%g, %g)$$",
        round(mean(goddf()$s) - 1.96*sd(goddf()$s)/sqrt(input$n), 2),
        round(mean(goddf()$s) + 1.96*sd(goddf()$s)/sqrt(input$n), 2))
    )
    
  })
  
  output$CI_bs <- renderUI({
    
    withMathJax(
      sprintf(
        "$$(%g, %g)$$",
        round(mean(dataframe()$bs - 1.96*sd(dataframe()$bs)/sqrt(input$n)), 2),
        round(mean(dataframe()$bs + 1.96*sd(dataframe()$bs)/sqrt(input$n)), 2)
      )
    )
    
  })
  
  output$CI_clt <- renderUI({
    
    withMathJax(
      sprintf(
        "$$(%g, %g)$$",
        round(mean(samp()$samp - 1.96*sd(samp()$samp)/sqrt(input$n)), 2),
        round(mean(samp()$samp + 1.96*sd(samp()$samp)/sqrt(input$n)), 2)
      )
    )
    
  })
  
  
  
}

shinyApp(ui = ui, server = server)