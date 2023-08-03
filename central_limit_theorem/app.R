library(shiny)
library(tidyverse)
library(shinycssloaders)

ui <- fluidPage(
  
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
              });"
  ),
  
  
  titlePanel("Central Limit Theorem"),
  sidebarLayout(
    sidebarPanel(
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
                              'proportion',
                              'sum')),
      actionButton(inputId = "go",
                   label = "Simulate")
    ),
    mainPanel(
      withSpinner(plotOutput("CLT")),
      withMathJax(
        div("The Central Limit Theorem states that the distibution of the sum of (n) independent and identically distributed ((iid))
        random vairables become normally distributed as $n \\rightarrow \\infty$.
        Sample means and sample proportions are both sums of random variables times a constant, so their sampling
        distributions can be approximated as follows:")
      ),
      withMathJax(
        div("$$\\bar{X} \\sim \\mathcal{N}(\\mu = \\bar{x}, \\sigma = \\frac{s}{\\sqrt{n}})$$"),
        div("$$\\hat{P} \\sim \\mathcal{N}(\\mu = \\hat{p}, \\sigma = \\sqrt{\\frac{\\hat{p} (1-\\hat{p})}{n}})$$")
      ),
      withMathJax(
        tags$div("If you are interested in the derivation of the standard error, $\\frac{\\sigma}{\\sqrt{n}}$,
                 please click",
                 tags$a(href = "https://en.wikipedia.org/wiki/Standard_error",
                        "here"), "."))
      )
    )
  )

server <- function(input, output, session) {
  
  data <- reactive({
    
    if (input$stat == "sum"){
      sum <- replicate(input$samples,
                       sum((sample(1:1000,
                                   input$sample_size,
                                   replace = TRUE))))
      df <- data.frame(
                       sum = sum
                       )
    } else if (input$stat == "proportion"){
      
      prop <- replicate(input$samples,
                        sum(rbernoulli(input$sample_size,
                                       p = 0.5))/input$sample_size)
      
      
      df <- data.frame(prop = prop)
      
    } else {
      
      mean <- replicate(input$samples,
                        mean((sample(1:1000,
                                     input$sample_size,
                                     replace = TRUE))))
      df <- data.frame(mean = mean
                       )
    }
    
    df
  }) %>% 
    bindEvent(input$go)
  
  output$CLT <- renderPlot({
    if (input$stat == "sum"){
      data() %>% 
        ggplot(aes(x =sum))+
        geom_histogram(color = "white",
                       fill = "purple")+
        theme_classic()
    } else if (input$stat == "proportion"){
      data() %>% 
        ggplot(aes(x = prop))+
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