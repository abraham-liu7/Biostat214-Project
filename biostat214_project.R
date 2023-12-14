library(tidyverse)
library(shiny)
library(invgamma)

# App Framework - could use more polishing in terms of the introduction

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("2024 US Presidential Election Poll Predictions"),
  
  
  # Adding a main panel with a title and plot output
  mainPanel(
    # Output: Tabset w/ plot, summary, and table ----
    tabsetPanel(type = "tabs",
                tabPanel("Introduction", value = 1, br(), textOutput("intro1"), br(),
                         textOutput("intro2")),
                tabPanel("Population Parameters", value = 2, br(), sidebarPanel(
                  br(),
                  checkboxGroupInput(
                    inputId = "PopSelect", "Select the voter population",
                    c("Likely Voters" = "lv",
                      "Adults" = "a",
                      "Registered Voters" = "rv")
                  ),
                  br()),
                  mainPanel(fluidRow(splitLayout(cellWidths = c("50%", "50%"), plotOutput("ybardistPlot"), plotOutput("sigmadistPlot")))), br(), br(), 
                  ),
                id = "conditionedPanels"
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$intro1 <- renderText({
    return("The US Presidential is the most important election that happens
    every four years. The 2024 Election is no different. Incumbent President
    Joe Biden of the Democratic Party is running for re-election. President
    Biden defeated his predecessor Donald Trump in the 2020 election, and Mr.
    Trump is currently campaigning for the Republican presidential nomination.
    Governor Ron DeSantis, former Ambassador Nikki Haley, and former Governor
    Chris Christie are a few of the other candidates who are also campaigning
    for the nomination.")
  })
  output$intro2 <- renderText({
    return("By implementing the finite sampling methods we learned in our class,
           we attempt to analyze the 2024 election poll data to gain a better
           understanding of all the intricacies regarding this event.")
  })
  # Only run once
  Polls <- read.csv("approval_polllist.csv")
  
  posterior_sampler <- function(df = df) {
    set.seed(0)
    y_bar <- df$approve[1:100]/100
    n <- df$sample_size[1:100]
    sample_var <- (y_bar)*(1-y_bar)/n
    
    a0 <- 3
    b0 <- 1
    theta <- 0.40
    n0 <- 10
    N <- 200000000
    f <- n/N
    f0 <- n0/N
    lambda <- n/(1 - f)
    lambda0 <- n0/(1 + f0)
    n_survey <- length(y_bar) # T in overleaf
    y_bar_weighted <- sum(lambda*y_bar)/sum(lambda) # y_bar_dot in overleaf
    
    astar <- sum(n/2) - n_survey + a0
    bstar <- b0 + 1/2*((lambda0*(sum(lambda)))/(lambda0 + sum(lambda))*
                         (y_bar_weighted - theta)^2 + 
                         sum(lambda*(y_bar_weighted - y_bar)^2) + 
                         sum((n-1)*sample_var))
    
    n_mc <- 10000
    sigma <- rinvgamma(n_mc, astar, bstar)
    
    mu_post <- (lambda0*theta + sum(lambda*y_bar))/(lambda0 + sum(lambda))
    sigma_post <- sigma/(lambda0 + sum(lambda))
    Y_bar <- rnorm(n_mc, mean = mu_post, sd = sqrt(sigma_post))
    post_samples <- data.frame(Y_bar, mu_post, sigma_post)
    return(post_samples)
  }
  
  samples <- reactive({
    Polls %>% filter(population %in% input$PopSelect) %>% posterior_sampler()
  })
  
  # You should define all the rendered output in the server
  output$ybardistPlot <- renderPlot({
    return(ggplot(data = as.data.frame(samples()$Y_bar), aes(x = samples()$Y_bar)) 
           + geom_density() + labs(x = "Population Mean") + 
             ggtitle("Posterior Distribution of the Finite Population Mean"))
  })
  
  output$sigmadistPlot <- renderPlot({
    return(ggplot(data = as.data.frame(samples()$sigma_post),
                  aes(x = samples()$sigma_post)) 
           + geom_density() + labs(x = "Population Variance") + 
             ggtitle("Posterior Distribution of the Finite Population Variance"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
