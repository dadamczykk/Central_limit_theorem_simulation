library(shiny)

toPlot <- c()

ui <- fluidPage( # gui settings
  
  titlePanel("Central limit theorem - simulation"),
  
  sidebarLayout(
    
    sidebarPanel(
      sliderInput(inputId = "sampleSize",
                  label = "Sample size:",
                  min = 1,
                  max = 200,
                  value = 50,
                  step = 1),
      
      sliderInput(inputI = "samples",
                  label = "Number of samples:",
                  min = 1,
                  max = 2000,
                  value = 100,
                  step = 1),
      
      radioButtons(inputId ="distType",
                   label = "Distribution type:",
                   c("Normal" = "normal",
                     "Uniform" = "uniform",
                     "Exponential" = "exponential",
                     "Chi-Square" = "chi",
                     "Beta" = "beta")),
      
      sliderInput("range", "Interval to check the probability:",
                  min = -3,
                  max = 6,
                  value = c(0,1),
                  step = 0.01),
      
      
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel("Density",
                 
                  h5("The program generates variables
                     from a selected distribution."),
                 
                  h5("Every sample set has the same specified size.
                     The program computes its mean."),
                 
                  h5("Those means are presented on the histogram below."),
                 
                  h5("By the central limit theorem those means
                     should form a normal distribution."),
                         
                 plotOutput(outputId = "distPlot"),
                 
                 plotOutput(outputId = "densityPlot"),
                 verbatimTextOutput(outputId = "info")
                 
                 
                 ),
        
        tabPanel("Distributions and probability",
                 h5("Test variables were drawn from a distribution
                    which density function is presented below."),
                 
                 plotOutput(outputId = "initDensityPlot"),
                 
                 h5("Having test samples, we can compute approximate
                    probability, that mean of random sample is inside
                    specified interval."),
                 
                 verbatimTextOutput(outputId = "intervalInfo")
                 
                 )
        )
    )
  )
)

server <- function(input, output) {
  
  
  rchisqWrapped <- function(n){ # wrappers for density functions
    return(rchisq(n, df = 5))
  }
  
  betaWrapped <- function(n){
    return(rbeta(n, shape1 = 5, shape2 = 3))
  }
  
  
  
  distributionType <- reactive({
    
    d <- switch(input$distType,
                 normal = rnorm,
                 uniform = runif,
                 exponential = rexp,
                 chi = rchisqWrapped,
                 beta = betaWrapped,
                 rnorm)
    
    return(d)
  })
  
  
  computeDistribution <- reactive({ # creates vector of means from vector of variables
    toplot <- c()
    d <- distributionType()
      for (i in 1:input$samples){
        x <- d(input$sampleSize)
        toplot <- c(toplot,mean(x))
        
      }
    return(toplot)
  })
  

  output$initDensityPlot <- renderPlot({ # plots density functions of input distributions
    switch(input$distType,
           normal = {x <- seq (-4, 4, length.out = 50)
           plot(x,dnorm(x), type="l",
                main = "Density of the normal distribution")},
           uniform = {x <- seq (-1, 2, length.out = 50)
           plot(x,dunif(x), type="l",
                main = "Density of the uniform distribution")},
           exponential = {x <- seq (-1, 5, length.out = 50)
           plot(x,dexp(x), type="l",
                main ="Density of the exponential distribution")},
           chi = {x <- seq (-1, 10, length.out = 50)
           plot(x,dchisq(x, df=5), type="l",
                main = "Density of the chi-square distribution with 5 degrees of freedom")},
           beta = {x <- seq (-1, 2, length.out = 50)
           plot(x,dbeta(x, shape1=5, shape2=3), type="l",
                main = "Density of the beta distribution with a=5, b=3")},
           {x <- seq (-4, 4, length.out = 50)
           plot(x,xdnorm(x), type="l")})
  })
  
  
  output$distPlot <- renderPlot({ # plots histogram
    d <- computeDistribution()
    hist(d, breaks=30, col = "#007bc2", border = "orange",
         xlab = "Sample mean",
         main = "Histogram of sample means")
  })
  
  
  output$densityPlot <- renderPlot({ # plots density functions based on histogram
    d <- computeDistribution()
    av = mean(d)
    s = sd(d)
    x <- seq(min(d), max(d), length.out = 50)
    y <- dnorm(x, mean = av, sd = s)
    
    plot(x, y,type="l", col="red", lwd=2, main = "Density of sample means"
         )
    lines(density.default(d), lwd=2)
    legend("topright", c("Density from sample values",
                              "Density of the normal distribution"),
                col = c("black", "red"), lwd=c(2,2))
  })

# Miłego dnia (albo wieczoru)! 
  
  output$intervalInfo <- renderPrint({ # gives information on probability
    mima <- input$range
    d <- computeDistribution()
    x <- d[d>mima[1]&d<mima[2]]
    cat("Probability, that mean of random sample from selected distribution is
        between values", mima[1], "and", mima[2], "is approximately", 
        length(x)/length(d) * 100, "%")
  })
  
  
  output$info <- renderPrint({ # gives information on normal distribution
    d <-computeDistribution()
    av = mean(d)
    s = sd(d)

    cat(" Computed mean: ", av, "\n", "Computed standard deviation: ", s)
    
  })
}

shinyApp(ui = ui, server = server)
