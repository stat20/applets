library(shiny)
library(tidyverse)
library(shinydashboard)
library(infer)
library(shinycssloaders)

# ===============================================
#                    UI
# ===============================================

ui <- dashboardPage(
  dashboardHeader(title = "Bootstrap & CLT"),
  
  dashboardSidebar(
    sidebarMenu(
      style = "position:fixed;width:220px;",
      
      # ===============================================
      # Population UI: uses rgamma() to show a graphic of a population.
      #Feature population skew that is tied to 'shape' argument of rgamma()
      #Features population amount N
      #gives the ability to toggle left skew or rights skew
      # ===============================================
      
      menuItem("Population",
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
      
      # ===============================================
      # Empirical data Inputs
      # User chooses sample size and is given a button to make the sample
      # ===============================================
      
      menuItem("Empirical Data",
               numericInput(inputId = "n",
                            label = "sample size",
                            min = 30,
                            value = 100)
      ),
      # ===============================================
      # Bootstrap Model Inputs
      # ===============================================
      
      menuItem("Bootstrap",
               numericInput(inputId = "bs_samps",
                            label = "How many boot-strap repititions would you like",
                            min = 1,
                            value = 500)
      ),
      
      # ===============================================
      # shows the bootstrap model and a CLT x~N(x_bar, s/sqrt(n)) over layed
      # ===============================================
      
      menuItem("Let's Compare!",
               checkboxGroupInput(inputId = "comp_choice",
                                  label = "Choose your overlay",
                                  choices = c("Ideal World",
                                              "Bootstrap",
                                              "Central Limit Theorem"))
      ),
      # ===============================================
      # Fastrak Buttons for pulling a quick BS and CLT and
      # Pulling 100 CIs
      # ===============================================
      menuItem("Fastrak",
               actionButton(inputId = "fastrak_info",
                            label = "",
                            icon = icon(name = "info-circle"),
                            style = "background-color:#FFFFFF;
                                        color:#000000;
                                        border-color:#BEBEBE;
                                        border-style:none;
                                        border-width:1px;
                                        border-radius:100%;
                                        font-size:25px;"),
               actionButton(inputId = "fast",
                            label = "Fastrak",
                            icon = icon(name = "forward")))
    )
  ),
  dashboardBody(
    fluidRow(
      box(title = "Distribution of the Population", collapsible = TRUE,
          plotOutput(outputId = "pop_plot")),
      box(title = "In a World", collapsible = TRUE,
          h5("If you had unlimited resources, funding, time, etc.
              you would be able to draw as many samples as you would like from this population.
              Here is ten thousand sample means."),
          withSpinner(plotOutput(outputId = "godmode")),
          actionButton(inputId = "god",
                       label = "Simulate ideal expirement"))
    ),
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
              });"
      ),
      
      # ===============================================
      # UI
      # ===============================================
      
      
      box(title = 'Empirical Data', collapsible = TRUE,
          withSpinner(plotOutput(outputId = "empir_data")),
          actionButton(inputId = "make_sample",
                       label = "see a sample")),
      tabBox(id = "tabbox1",
             tabPanel("Bootstrap",
                      p("Your bootstrap sample size:", textOutput("bs_size", inline = T)),
                      actionButton(inputId = "sim_bs",
                                   label = "Compute the Bootstrap"),
                      withSpinner(plotOutput(outputId = "bs_plot")),
                      p(uiOutput(outputId = "bs_stats"))),
             tabPanel("Central Limit Theorem",
                      actionButton(inputId = "sim_clt",
                                   label = "Compute the Central Limit Theorem"),
                      br(),
                      p("The CLT follows a normal distribution:" , uiOutput(outputId = "clt_norm")),
                      withSpinner(plotOutput("CLT")),
                      actionButton(inputId = "CLT_info",
                                   label = "",
                                   icon = icon(name = "info-circle"),
                                   style = "background-color:#FFFFFF;
                                        color:#000000;
                                        border-color:#BEBEBE;
                                        border-style:none;
                                        border-width:1px;
                                        border-radius:100%;
                                        font-size:25px;"))
      )
    ),
    fluidRow(
      box(title = "Let's Compare", collapsible = TRUE,
      plotOutput("comp_graph")),
      tabBox(
        id = "tabbox2",
        tabPanel(
          "Confidence Intervals",
          p("Statistics for the population", uiOutput(outputId = "pop_stats")),
          p("95% Confidence Interval for the Ideal World is", uiOutput(outputId = "CI_ideal")),
          p("95% Confidence Interval for the Bootstrap is", uiOutput(outputId = "CI_bs")),
          p("95% Confidence Interval for Central Limit Theorem is", uiOutput(outputId = "CI_clt")),
          actionButton(inputId = "CI_info",
                       label = "",
                       icon = icon(name = "info-circle"),
                       style = "background-color:#FFFFFF;
                                        color:#000000;
                                        border-color:#BEBEBE;
                                        border-style:none;
                                        border-width:1px;
                                        border-radius:100%;
                                        font-size:25px;")
        ),
        tabPanel(
          "CI CLT graph",
          p("Here you will draw 100 different samples from the population, compute an approximate Confidence Interval on each,
            and graph them to see how many capture the truth. "),
          actionButton(inputId = "ci_sim",
                       label = "Plot 100 CIs"),
          withSpinner(plotOutput("CI100"))
        ),
        tabPanel(
          "CI bootstrap graph",
          p("Here you will create 100 different BS from 100 different samples and compute 100 different CIs,
            we'll then graph them and compare them to the true mean."),
          actionButton(inputId = "ci_bs_sim",
                       label = "Plot 100 CIs"),
          withSpinner(plotOutput("CIbs100"))
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
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
        theme_classic(base_size = 18)+
        xlab("")+
        ggtitle("Distribution of the Population")+
        theme(axis.title.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank(),
              axis.line.y = element_blank())
      
    } else {
      
      pop() %>% 
        ggplot(aes(x = right))+
        geom_histogram(color = "white",
                       fill = "blue")+
        theme_classic(base_size = 18)+
        xlab("")+
        ggtitle("Distribution of the Population")+
        theme(axis.title.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank(),
              axis.line.y = element_blank())
      
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
      theme_classic(base_size = 18)+
      theme(axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.line.y = element_blank(),
            axis.title.x = element_blank())+
      ggtitle("Sampling Distribution")+
      xlim(mean(goddf()$s) - 5*sd(goddf()$s), mean(goddf()$s) + 5*sd(goddf()$s))
    
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
    bindEvent(input$make_sample,
              input$fast,
              ignoreInit = TRUE)
  
  output$empir_data <- renderPlot({
    
    samp() %>% 
      ggplot(aes(x = samp))+
      geom_histogram(color = "white",
                     fill = "green3")+
      theme_classic(base_size = 18)+
      xlab("x")+
      ggtitle("A Sample")+
      theme(axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.line.y = element_blank())
    
  })
  
  
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
              input$sim_clt,
              input$fast,
              ignoreInit = TRUE)
  
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
      theme_classic(base_size = 18)+
      xlab(bquote(bar(x)))+
      ylab("")+
      theme(axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.line.y = element_blank())+
      ggtitle("Bootstrap Sampling Distribution")+
      xlim(mean(goddf()$s) - 5*sd(goddf()$s), mean(goddf()$s) + 5*sd(goddf()$s))
    
  }) %>% 
    bindEvent(input$sim_bs,
              input$fast,
              ignoreInit = TRUE)
  
  output$bs_stats <- renderUI({
    
    withMathJax(
      sprintf("$$\\bar{x} = %g \\hspace{1cm} \\sigma = %g$$",
              round(mean(dataframe()$bs), 2),
              round(sd(dataframe()$bs), 2))
    )
    
  }) %>% 
    bindEvent(input$sim_bs,
              input$fast,
              ignoreInit = TRUE)
  
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
      theme_classic(base_size = 18)+
      ylab("")+
      theme(axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.line.y = element_blank(),
            axis.title.x = element_blank())
    
  }) %>% 
    bindEvent(input$sim_clt,
              input$fast,
              ignoreInit = TRUE)
  
  output$clt_norm <- renderUI({
    
    withMathJax(
      sprintf("$$\\mathcal{N}(\\mu = \\bar{x} = %g,\\sigma = \\frac{s}{\\sqrt{n}} = %g)$$",
              round(mean(samp()$samp), 2),
              round(sd(samp()$samp)/sqrt(input$n), 2))
    )
    
  }) %>% 
    bindEvent(input$sim_clt,
              input$fast,
              ignoreInit = TRUE)
  
  observeEvent(input$CLT_info, {
    
    showModal(modalDialog(
      title = "The Central Limit Theorem",
      withMathJax(
        p("The Central Limit Theorem states that the distibution of the sum of (n) independent and identically distributed ((iid))
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
                        "here"), ".")
      ),
      easyClose = TRUE
    ))
    
  })
  
  
  # ===============================================
  # graph magnum opus
  # ===============================================
  
  output$comp_graph <- renderPlot({
    
    plot <- ggplot()
    
    ideal <- "Ideal World" %in% input$comp_choice
    bs <- "Bootstrap" %in% input$comp_choice
    clt <- "Central Limit Theorem" %in% input$comp_choice
    
    if (ideal & bs & clt){
      
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
                       fill = "orangered",
                       alpha = .65)+
        geom_line(data = dataframe(),
                  mapping = aes(x = l,
                                y = m),
                  size = 1,
                  color = "black")
      
    } else if(ideal & bs){
      
      plot <- plot + geom_histogram(data = goddf(),
                                    mapping = aes(x = s,
                                                  y = ..density..),
                                    color = "white",
                                    fill = "purple")+
        geom_histogram(data = dataframe(),
                       mapping = aes(x = bs,
                                     y = ..density..),
                       color = "white",
                       fill = "orangered",
                       alpha = .65)
      
    } else if(ideal & clt){
      
      plot <- plot + 
        geom_histogram(data = goddf(),
                       mapping = aes(x = s,
                                     y = ..density..),
                       color = "white",
                       fill = "purple")+
        geom_line(data = dataframe(),
                  mapping = aes(x = l,
                                y = m),
                  size = 1,
                  color = "black")
      
    }else if(bs & clt){
      
      plot <- plot +
        geom_histogram(data = dataframe(),
                       mapping = aes(x = bs,
                                     y = ..density..),
                       color = "white",
                       fill = "orangered",
                       alpha = .65)+
        geom_line(data = dataframe(),
                  mapping = aes(x = l,
                                y = m),
                  size = 1,
                  color = "black")
      
    }else if(ideal){
      
      plot <- plot +
        geom_histogram(data = goddf(),
                       mapping = aes(x = s,
                                     y = ..density..),
                       color = "white",
                       fill = "purple")
      
    }else if(bs){
      
      plot <- plot +
        geom_histogram(data = dataframe(),
                       mapping = aes(x = bs,
                                     y = ..density..),
                       color = "white",
                       fill = "orangered",
                       alpha = .65)
      
    }else if(clt){
      
      plot <- plot +
        geom_line(data = dataframe(),
                  mapping = aes(x = l,
                                y = m),
                  size = 1,
                  color = "black")
      
    }
    
    plot + 
      theme_classic(base_size = 18)+
      theme(axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.line.y = element_blank())+
      xlim(mean(goddf()$s) - 5*sd(goddf()$s), mean(goddf()$s) + 5*sd(goddf()$s))+
      xlab(bquote(bar(x)))
    
  })
  
  
  # ===============================================
  # List of Confidence intervals
  # ===============================================
  
  observeEvent(input$CI_info, {
    
    showModal(modalDialog(
      title = "Confidence Interval",
      withMathJax(
        div("A confidence interval for the population mean is a random interval calculated from the sample mean that contains
            the population mean within a certain probability. For a 95% confidence interval, we use our previous knowledge from the
            CLT and the empirical rule. The CLT tells us that the sample mean, $\\bar{X}$, follows a normal distribution as $n \\rightarrow \\infty$.
            Part of the empirical rule states that 95% of the area underneath a normal curve is within 1.96 standard deviation of the mean."),
        div("Here is what we have so far:")
        ),
        withMathJax(
          div("$$(\\mu - 1.96\\frac{\\sigma}{\\sqrt{n}}, \\mu + 1.96\\frac{\\sigma}{\\sqrt{n}})$$")
        ),
      withMathJax(
        div("An issue here is that we usually do not know what $\\sigma$ is so we estimate it with $s$, the sample standard deviation:")
      ),
      withMathJax(
        div("$$s = \\sqrt{\\frac{1}{n-1}\\sum_{i=1}^{n} (x_i - \\bar{x})^2}$$")
      ),
      withMathJax(
        div("And because we are using the estimator $s$, our confidence interval now uses the $t$ distribution. It should be noted
        that for large $n$ the difference between using the inverse $t$ CDF and the inverse normal CDF is negligable. Rule of thumb for this
        is $n \\geq 30$."),
        div("Here is our 95% confidence intervals for $\\mu$, with $n \\le 30$:")
      ),
      withMathJax(
        div("$$(\\bar{X} - t_{n-1}^{-1}(.95)\\frac{s}{\\sqrt{n}}, \\bar{X} + t_{n-1}^{-1}(.95)\\frac{s}{\\sqrt{n}})$$")
      ),
      withMathJax(
        div("And $n \\geq 30$")
      ),
      withMathJax(
        div("$$(\\bar{X} - 1.96\\frac{s}{\\sqrt{n}}, \\bar{X} + 1.96\\frac{s}{\\sqrt{n}})$$")
      ),
      withMathJax(
        tags$div("please visit",
                 tags$a(href = "https://en.wikipedia.org/wiki/Student%27s_t-distribution#:~:text=In%20probability%20and%20statistics%2C%20Student's,around%20zero%20and%20bell%2Dshaped.",
                        "here"), "for more information on the $t$ distribution.")
      ),
      easyClose = TRUE
    ))
    
  })
  
  output$pop_stats <- renderUI({
    
    if(input$skew == "left-skew"){
      
      withMathJax(
        sprintf("$$\\mu = %g \\hspace{1cm} \\sigma = %g$$",
                round(mean(pop()$left), 2),
                round(sd(pop()$left), 2))
      )
      
    }else if(input$skew == "right-skew"){
      
      withMathJax(
        sprintf("$$\\mu = %g \\hspace{1cm} \\sigma = %g$$",
                round(mean(pop()$right), 2),
                round(sd(pop()$right), 2))
      )
      
      
    }
    
  })
  
  output$CI_ideal <- renderUI({
    
    withMathJax(
      sprintf(
        "$$(%g, %g)$$",
        round(mean(goddf()$s) - 1.96*sd(goddf()$s)/sqrt(input$n), 2),
        round(mean(goddf()$s) + 1.96*sd(goddf()$s)/sqrt(input$n), 2)
        )
    )
    
  })
  
  output$CI_bs <- renderUI({
    
    ci_boot <- data.frame(lower = mean(samp()$samp) - (quantile(dataframe()$bs, probs = 0.975) - mean(samp()$samp)),
                          upper = mean(samp()$samp) - (quantile(dataframe()$bs, probs = 0.025) - mean(samp()$samp)))
    
    withMathJax(
      sprintf(
        "$$(%g, %g)$$",
        round(ci_boot[1], 2),
        round(ci_boot[2], 2)
      )
    )
    
  })
  
  output$CI_clt <- renderUI({
    
    withMathJax(
      sprintf(
        "$$(%g, %g)$$",
        round(mean(samp()$samp - qt(.975, df = input$n-1)*sd(samp()$samp)/sqrt(input$n)), 2),
        round(mean(samp()$samp + qt(.975, df = input$n-1)*sd(samp()$samp)/sqrt(input$n)), 2)
      )
    )
    
  })
  
  # ===============================================
  # Fastrak Info
  # ===============================================
  
  observeEvent(input$fastrak_info, {
    
    showModal(modalDialog(
      title = "Fastrak Info",
      h4("Fastrak-simulation"),
      p("Fastrak-simulation pulls a new sample from the population, computes the Bootstrap and CLT for you.
         After clicking the Fastrak button just scroll down to 'Let's Compare'. "),
      easyClose = TRUE
    ))
    
  })
  
  
  # ===============================================
  # CI graphs
  # ===============================================
  
  cis100 <- reactive({
    
    if(input$skew == "left-skew"){
      
      samp100_means <- replicate(100, mean(sample(pop()$left, input$n, replace = T)))
      
      lower <- samp100_means - qt(.975, df = input$n-1) * sd(pop()$left)/sqrt(input$n)
      upper <- samp100_means + qt(.975, df = input$n-1) * sd(pop()$left)/sqrt(input$n)
      trials <- 1:100
      cover <- (mean(pop()$left) >= lower) & (mean(pop()$left) <= upper)
      
    }else if(input$skew == "right-skew"){
      
      samp100_means <- replicate(100, mean(sample(pop()$right, input$n, replace = T)))
      
      lower <- samp100_means - qt(.975, df = input$n-1) * sd(pop()$right)/sqrt(input$n)
      upper <- samp100_means + qt(.975, df = input$n-1) * sd(pop()$right)/sqrt(input$n)
      trials <- 1:100
      cover <- (mean(pop()$right) >= lower) & (mean(pop()$right) <= upper)
      
    }
    
    CIs <- data.frame(trials, lower, upper, cover)
    
    CIs
    
  }) %>% 
    bindEvent(input$ci_sim)
  
  
  output$CI100 <- renderPlot({
    
    plot <- cis100() %>% 
        ggplot(aes(y = trials))+
        geom_segment(aes(x = lower, xend = upper, yend = trials, color = cover),
                     show.legend=FALSE)+
        theme_classic()+
        labs(x=expression(bar(x)), y = "Iteration")
    
    if(input$skew == "right-skew"){
      
      plot <- plot+
        annotate("segment", x=mean(pop()$right), xend=mean(pop()$right),
                 y=0, yend=101, color="black")
      
    }else if(input$skew == "left-skew"){
      
      plot <- plot+
        annotate("segment", x=mean(pop()$left), xend=mean(pop()$left),
                 y=0, yend=101, color="black")
      
    }
    
    plot
    
  })
  
  
  bs_cis <- reactive({
    
    if(input$skew == "right-skew"){
      
      X <- replicate(100, sample(pop()$right, 100, replace = T))
      X <- data.frame(X)
      
      X_bar <- colMeans(X)
      
      boot_function <- function(X){
        
        replicate(100, mean(sample(X, 100, replace = TRUE)))
        
      }
      
      boot <- apply(X, 2, boot_function )
      df_boot <- data.frame(boot)
      
      quantiles <- apply(df_boot, 2, quantile, probs = c(0.025, 0.975))
      
      upper <- X_bar - (quantiles[1,] - X_bar)
      lower <- X_bar - (quantiles[2,] - X_bar)
      trials <- 1:100
      cover <- (mean(pop()$right) >= lower) & (mean(pop()$right) <= upper)
      
    }else if(input$skew == "left-skew"){
      
      X <- replicate(100,sample(pop()$left, 100, replace = T))
      X <- data.frame(X) 
      
      X_bar <- colMeans(X)
      
      boot_function <- function(X){
        
        replicate(100, mean(sample(X, 100, replace = TRUE)))
        
      }
      
      boot <- apply(X, 2, boot_function )
      df_boot <- data.frame(boot)
      
      quantiles <- apply(df_boot, 2, quantile, probs = c(0.025, 0.975))
      
      upper <- X_bar - (quantiles[1,] - X_bar)
      lower <- X_bar - (quantiles[2,] - X_bar)
      trials <- 1:100
      cover <- (mean(pop()$left) >= lower) & (mean(pop()$left) <= upper)
      
    }
    
    
    df <- data.frame(upper, lower, trials, cover)
    
    df
    
    
  }) %>% 
    bindEvent(input$ci_bs_sim)
  
  
  output$CIbs100 <- renderPlot({
    
    bsplot <- bs_cis() %>% 
      ggplot(aes(y = trials))+
      geom_segment(aes(x = lower, xend = upper, yend = trials, color = cover),
                   show.legend=FALSE)+
      theme_classic()+
      labs(x=expression(bar(x)), y = "Iteration")
    
    if(input$skew == "right-skew"){
      
      bsplot <- bsplot +
        annotate("segment", x=mean(pop()$right), xend=mean(pop()$right),
                 y=0, yend=101, color="black")
      
    }else if(input$skew == "left-skew"){
      
      bsplot <- bsplot+
        annotate("segment", x=mean(pop()$left), xend=mean(pop()$left),
                 y=0, yend=101, color="black")
      
    }
    
    bsplot
    
    
  }) %>% 
    bindEvent(input$ci_bs_sim)
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
