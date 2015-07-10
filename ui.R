library(shiny)
library(ggplot2)
library(gridExtra)

# Define UI for random distribution application 
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Simulated Level and Power for One Sample t-test"),
  
  plotOutput("plot"),
  
  wellPanel(
    fluidRow(
      column(6,
             radioButtons(inline=TRUE,"dist", "Parent Dist. (all rescaled with mean 0, sd 1):",
                          c("Normal" = "norm",
                            "Uniform" = "unif",
                            "Log-normal" = "lnorm",
                            "Exponential" = "exp",
                            "Heavy tails" = "heavyyy"))
      ),
      column(4,
             radioButtons(inline=TRUE,"tails","Direction of test:",
                          c("Right tailed" = "right",
                            "Left tailed" = "left",
                            "Two tailed" = "two") )
      ),
      column(2,
             actionButton("resample", label = "Draw New Samples")
      )
    )
  ),
  fluidRow(
    column(4,
           sliderInput("n", 
                       "Sample size:", 
                       step = 10,
                       value = 30,
                       min = 10, 
                       max = 400)
    ),
    column(4,
           sliderInput("alpha",
                       "alpha:",
                       step = 0.01,
                       value = .05,
                       min = .01,
                       max = .1)
    ),
    column(4,
           sliderInput("shift",
                       "shift (number of std. dev.)",
                       step = 0.05,
                       value = 0.5,
                       min = -1.0,
                       max = 1.0) 
    )
  )  
))
