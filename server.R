library(shiny)

t0 <- NULL
t1 <- NULL
x0 <- NULL
x1 <- NULL
plot1 <- NULL
plot2 <- NULL
plot3 <- NULL
plot4 <- NULL

# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  # Reactive expression to update the list of stats when the distribution changes
  # (this seems unecessary)
  doReset <- reactive({
    # in addition to input$dist react to changes in...
    input$dist
    input$resample
    input$n
    input$alpha
    input$shift
    input$tails
     
    t0 <<- NULL
    t1 <<- NULL
    x0 <<- NULL
    x1 <<- NULL
    plot1 <<- NULL
    plot2 <<- NULL
    plot3 <<- NULL
    plot4 <<- NULL
    
  })
  
  # Generate a plot of the data. Also uses the inputs to build
  # the plot label. Note that the dependencies on both the inputs
  # and the data reactive expression are both tracked, and
  # all expressions are called in the sequence implied by the
  # dependency graph
  output$plot <- renderPlot({
    doReset()
    
    # Plot parameters...
    fcol="orange"
    rcol="red"
    pcol="forestgreen"

    n <- input$n
    shift <- input$shift
    parentDist <- input$dist
    reps <- 10000
    nparent <- 100000
    #x<-data()
    doReset()
    mu = switch(parentDist,
                norm = 0,
                unif = .5,
                heavy = 0,
                heavyyy = 0,
                lnorm = exp(.5),
                exp = 1,
                0)
    sigma = switch(parentDist,
                   norm = 1,
                   unif = 1/sqrt(12),
                   heavy = sqrt(1.5),
                   heavyyy = 3,
                   lnorm = sqrt((exp(1)-1)*(exp(1))),
                   exp = 1,
                   1)
    dist <- function(n,type,mu,sigma){
        x <- switch(type,
               norm = rnorm(n),
               unif = runif(n),
               heavy = rt(n,6),
               heavyyy = rt(n,2.25),
               lnorm = rlnorm(n),
               exp = rexp(n),
               rnorm(n))
        return((x - mu)/sigma)
    }
    
    dens <- function(z,type,mu,sigma){
      x <- sigma*z + mu
      sigma * switch(type,
             norm = dnorm(x),
             unif = dunif(x),
             heavy = dt(x,6),
             heavyyy = dt(x,2.25),
             lnorm = dlnorm(x),
             exp = dexp(x),
      )
    }
    
    t0 <<- 1:reps
    t1 <<- 1:reps
    xparent0 <- dist(nparent,parentDist,mu,sigma)
    xparent1 <- dist(nparent,parentDist,mu,sigma)+shift
    parent <- data.frame(xparent0,xparent1)
    for (i in 1:reps) {
      x0 <<- dist(n,parentDist,mu,sigma)
      x1 <<- dist(n,parentDist,mu,sigma)
      se0 <- sd(x0)/sqrt(n)
      se1 <- sd(x1)/sqrt(n)
      t0[i] <<- mean(x0)/se0
      t1[i] <<- (mean(x1)+shift)/se1
    }
    
    if(input$tails == "right"){
      tcrit <- qt(1-input$alpha,df=n-1)
      crit0 <- t0>tcrit
      crit1 <- t1>tcrit
      vlines <- tcrit
    }
    if(input$tails == "left"){
      tcrit <- qt(input$alpha,df=n-1)
      crit0 <- t0<tcrit
      crit1 <- t1<tcrit
      vlines <- tcrit
    }
    if(input$tails == "two"){
      tcrit <- qt(1-input$alpha/2,df=n-1)
      crit0 <- abs(t0)>tcrit
      crit1 <- abs(t1)>tcrit
      vlines <- c(-tcrit,tcrit)
    }
    
    # set plot range
    xmin = switch(input$dist,
                  norm = -3, 
                  unif = -2, 
                  heavy = -3, 
                  heavyyy = -3, 
                  lnorm = -.8, 
                  exp = -1, 
                  -3)
    xmax = switch(input$dist, 
                  norm =  3, 
                  unif = 2, 
                  heavy =  3, 
                  heavyyy =  3, 
                  lnorm = 2.5, 
                  exp = 3,  
                  3)
    
    
    x <- seq(xmin,xmax,length=400)
    y <- dens(x,parentDist,mu,sigma)
    xfill <- c(xmin,x,xmax)
    yfill <- c(0,y,0)
    xshift <- x + shift
    xshiftfill <- xfill+shift
    df <- data.frame(xfill,yfill,xshiftfill)
    xlimits <- c( min(c(xmin,xmin+shift)),max(c(xmax,xmax+shift)))
    
    plot1 <<- qplot(x,y,geom="line") + 
      geom_polygon(data=df,aes(xfill,yfill),fill=fcol) +
      geom_vline( xintercept = 0 ) + 
      ggtitle("Null Parent Distribution") +
      scale_x_continuous(limits = xlimits) +
      labs(x="x",y="density")

    
    plot2 <<- qplot(xshift,y,geom="line") + 
      geom_polygon(data=df,aes(xshiftfill,yfill),fill=fcol) +
      geom_vline( xintercept = shift ) + 
      ggtitle("Alternative Parent Distribution") +
      scale_x_continuous(limits = xlimits) + 
      labs(x="x",y="density")
      
    
    k <- 20
    bw <- abs(tcrit)/k
    left <- min( quantile(t0,.005), quantile(t1,.005))
    right <- max( quantile(t0,.995), quantile(t1,.995))
    n1 <-   floor( left / bw )
    n2 <- ceiling( right / bw )
    
    brks <- (n1:n2)*bw
    lim <- c(n1*bw,n2*bw)
    
    simult <- data.frame(t0,crit0,t1,crit1)
    
    plot3 <<- ggplot(simult,aes(x=t0,fill=crit0)) +
      geom_histogram(breaks=brks) + 
      scale_x_continuous(limits = lim) +
      scale_fill_manual(values=c(fcol,rcol)) + 
      geom_vline( xintercept = vlines )+
      guides(fill=FALSE) + 
      ggtitle( paste("Approximate Level = ",signif(sum(crit0)/reps,3) ) ) +
      labs(x="t",y="count")
    
    # if all the shifted t scores are in the rejection region so that 
    # all of crit1 is TRUE, then only the first color gets used
    # this hack fixes it (alternately just set one FALSE it won't be visible)    
    MyPalette <- c(fcol,pcol)
    if (min(crit1)>0)
    {
      MyPalette <- c(pcol,fcol)
    }
    plot4 <<- ggplot(simult,aes(x=t1,fill=crit1)) +
      geom_histogram(breaks=brks) +
      scale_x_continuous(limits = lim) + 
      scale_fill_manual(values=MyPalette) + 
      geom_vline (xintercept = vlines) +
      guides(fill=FALSE) + 
      ggtitle( paste("Approximate Power = ",signif(sum(crit1)/reps,3) ) ) +
      labs(x="t",y="count")
    
    grid.arrange(plot1,plot3,plot2,plot4,ncol = 2)
    
  })
})

