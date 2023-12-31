library(tidyverse)
library(shiny)
library(invgamma)
library(lubridate)

# App Framework - could use more polishing in terms of the introduction

# Define UI for application
ui <- fluidPage(
  
  withMathJax(),
  
  # Application title
  titlePanel("2024 US Presidential Election Poll Predictions"),
  
  
  # Adding a main panel with a title and plot output
  mainPanel(
    # Output: Tabset w/ plot, summary, and table ----
    tabsetPanel(type = "tabs",
                tabPanel("Introduction", value = 1, br(), textOutput("intro1"), br(),
                         textOutput("intro2"), br(), h4("Problem Statement"), br(), 
                         uiOutput("formula1"), br(), h4("Solution"), br(),
                         uiOutput("formula2"), br(), 
                         tags$a(href="https://drive.google.com/file/d/1RWLVoSndeENN-aKNZqbS19h1gXwKSR5E/view?usp=sharing", 
                                "Complete derivation(open in browser before you click)"), br(), br()),
                tabPanel("Posterior Population Parameters", value = 2, br(), sidebarPanel(
                  br(),
                  checkboxGroupInput(
                    inputId = "PopSelect", "Select the voter population",
                    c("Likely Voters" = "lv",
                      "Adults" = "a",
                      "Registered Voters" = "rv"), selected = c("lv", "a", "rv")
                  ),
                  br()),
                  mainPanel(fluidRow(splitLayout(cellWidths = c("50%", "50%"), plotOutput("ybardistPlot"), plotOutput("sigmadistPlot")))),
                  br(), br(), 
                  ),
                tabPanel("Predictions", value = 3, br(), h1("Historic Biden Approval Rating by Month"), textOutput("explanation1"), br(), sidebarPanel(
                  br(),
                  sliderInput("months", "Months into the Biden Administration", 1, 36, 12), br()), mainPanel(plotOutput("predsbymonth")),
                  br(),
                  br(),
                  h1("Recent Biden Approval Ratings by Day"),
                  textOutput("explanation2"), br(), sidebarPanel(
                    br(),
                    sliderInput("days", "Approval Rating Since X Days Ago", 1, 880, 100), br()), mainPanel(plotOutput("lastdays")), br(), br()),
                id = "conditionedPanels"
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$intro1 <- renderText({
    return("The US Presidential election is often considered the most important election. 
    Not only is it the election for the highest office, but it almost always has a massive impact
    on other elections throughout the country, namely, congressional elections.\n
    The 2024 Election is no different. Incumbent President
    Joe Biden of the Democratic Party is currently running for re-election. Joe
    Biden defeated his predecessor Donald Trump in the 2020 election, and
    Trump is currently campaigning for the Republican presidential nomination.\n
    Biden's 2020 victory was arguably by a slim margin, especially if we consider which states 
    won him the election and by how little he won them, i.e., Arizona and Georgia.  Since then, 
    the general consensus is that public opinion of Joe Biden has only gone down, and 
    by a seemingly substantial amount.\n"
           
    )
  })
  output$intro2 <- renderText({
    return("By implementing the finite sampling methods we learned in our class,
           we have attempted to analyze 2024 election poll data to gain a better
           understanding of the current political climate from a purely quantitative
           point of view.  We will be mainly focusing on demonstrating how Biden's
           approval rating's have changed with time since the beggining of his term 
           until now(12/10/2023). Doing so, we hope our work helps to indicate
           just how likely Biden is to win a second term. \n
           The model is specified below:")
  })
  output$title1 <- renderText({return("1: Problem Statement")})
  output$formula1 <- renderUI({
    withMathJax(
      helpText('Suppose you are given the following data from T surveys:'),
      helpText('\\begin{align*}
                  \\{(n_1, \\overline{y}_1, s_1^2), (n_2, \\overline{y}_2, s_2^2), 
                  \\ldots, (n_T, \\overline{y}_T, s_T^2)\\}
                \\end{align*}'),
      helpText('where the surveys are independently collected, 
                and the data points come from the distribution:'),
      helpText('\\begin{align*}
                  y_1, \\ldots, y_n | \\mu, \\sigma^2 & \\sim 
                    N(\\mu, \\sigma^2)\\\\
                  \\mu | \\sigma^2 &\\sim 
                    N\\left(\\theta, \\frac{\\sigma^2}{n_0}\\right)\\\\
                  \\sigma^2 & \\sim IG(a_0, b_0)
                \\end{align*}'),
      helpText('find $$\\begin{align*}
                  P(\\overline{Y}, \\sigma^2 | D_t)
                \\end{align*} \\!$$'),
      helpText('where $$D_t = \\{(n_1, \\overline{y}_1, s_1^2), \\ldots, 
               (n_t, \\overline{y}_t, s_t^2)\\} \\!$$')
      )
  })
  output$title2 <- renderText({return("2: Answer:")})
  output$formula2 <- renderUI({
    withMathJax(
      helpText('\\begin{align*}
                  P(\\overline{Y}, \\sigma^2 | D_t) \\sim  
                  N(\\overline{Y} | M_Tm_T, \\sigma^2 M_T) 
                  \\cdot IG(\\sigma^2 | a_T, b_T)
                \\end{align*}'),
      helpText('\\begin{align*}
                  M_T^{-1} &= \\sum_{i=0}^T \\lambda_i\\\\
                  m_T &= \\sum_{i=0}^T \\lambda_i \\overline{\\mathbf{y}}_i\\\\
                  a_T &= a_0 + \\sum_{i=1}^T\\frac{n_i}{2}\\\\
                  b_T &= b_0 + \\frac{1}{2}\\left[\\left(\\sum_{i=1}^T (n_i-1) s_i^2\\right) 
                    + \\left(\\sum_{i=1}^T \\lambda_i \\overline{\\mathbf{y}}_i^2\\right)  
                    + M_0 m_0^2 -  M_T m_T^2 \\right]
                \\end{align*}'),
      helpText('We will denote: $$
                \\begin{align*}
                  \\lambda_0 &= \\frac{n_0}{1 + f_0}\\\\
                  \\lambda_i &= \\frac{n_i}{1 - f_i} \\hspace{0.5 cm} i = 1,2,\\ldots, T\\\\
                  M_i^{-1} &= \\sum_{j=0}^i \\lambda_j \\hspace{0.5 cm} j = 0,1,2,\\ldots, T\\\\
                  m_i &= \\sum_{j=0}^i \\lambda_j \\overline{\\mathbf{y}}_j \\hspace{0.5 cm} j = 0,1,2,\\ldots, T
                \\end{align*} \\!$$'),
      helpText('where we abuse notation and call $$\\overline{\\mathbf{y}}_0 = \\theta \\!$$')
    )
  })
  
  # Only run once
  Polls <- read.csv("approval_polllist.csv")
  
  posterior_sampler <- function(df = df) {
    set.seed(0)
    y_bar <- df$approve/100
    n <- df$sample_size
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
           + geom_density() + labs(x = "Population Mean", y ="Density") + 
             ggtitle("Posterior Distribution \nof the Finite \nPopulation Mean"))
  })
  
  output$sigmadistPlot <- renderPlot({
    return(ggplot(data = as.data.frame(samples()$sigma_post),
                  aes(x = samples()$sigma_post)) 
           + geom_density() + labs(x = "Population Variance", y ="") + 
             ggtitle("Posterior Distribution \nof the Finite \nPopulation Variance"))
      
  })
  
  output$explanation1 <- renderText({
    return("This plot illustrates the progression of Biden's approval rating on a monthly basis 
           since the beginning of his presidency.  Polling began in January 2021. 
           Between then and now, there have been 36 months of polls conducted. 
           'Time in Months' is the number of months since polling started, and 
           roughly represents the number of months since the beginning of his 
           presidency. These predictions are made cumulatively by month using all of the polling data in the given time frame.
           Use the sliding scale to see how Biden's approval rating fluctuates from the beginning of his 
           administration onward.")
  })
  output$predsbymonth <- renderPlot({
      newpolls <- Polls %>% 
        mutate(bymonth = floor_date(date(end_date), unit="month")) %>% 
        mutate(count = match(bymonth, sort(unique(bymonth), decreasing = FALSE)))
      Y_bars <- c()
      for (i in 1:input$months) {
        sample <- newpolls %>% filter(count %in% 1:i) %>% posterior_sampler()
        Y_bars <- append(Y_bars, mean(sample$Y_bar))
      }
      ybardf <- data.frame(survey_month = c(1:input$months), approval_pred = Y_bars)
      ggplot(ybardf, aes(survey_month, Y_bars)) + geom_line() + xlab("Time in Months")+
        ylab("Approval Rating")
        })
  output$explanation2 <- renderText({
    return("This plot shows the changes in Biden's approval rating on a daily 
    basis for a recent time frame of your choice. The polls were conducted from 
    January 21, 2021 until December 10th, 2023, with a total of 880 polling days. 
    Use the sliding scale to see how Biden's rating fluctuates from any point in
    the polling period until now (12/10/2023). These predictions are made cumulatively based 
    only on the polls within the indicated period in order to eliminate any possible bias
    from polls conducted earlier on in his presidency and to shed light on new insights.")
  })
  output$lastdays <- renderPlot({
    newpolls <- Polls %>% mutate(count = match(end_date, sort(unique(end_date), decreasing = FALSE)))
    Y_bars <- c()
    for (i in (length(unique(newpolls$end_date))-input$days):length(unique(newpolls$end_date))) {
      sample <- newpolls %>% filter(count %in% (length(unique(newpolls$end_date))-input$days):i) %>% posterior_sampler()
      Y_bars <- append(Y_bars, mean(sample$Y_bar))
    }
    ybardf <- data.frame(survey_day = c((length(unique(newpolls$end_date))-input$days):length(unique(newpolls$end_date))), approval_pred = Y_bars)
    ggplot(ybardf, aes(survey_day, Y_bars)) + geom_line() +
      xlab("Time in Days")+
      ylab("Approval Rating")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
