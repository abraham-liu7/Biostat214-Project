library(tidyverse)
library(shiny)

# App Framework - could use more polishing in terms of the introduction

# Define UI for application
ui <- fluidPage(

  # Application title
  titlePanel("2020 Election Poll Predictions"),


  # Adding a main panel with a title and plot output
  mainPanel(
    # Output: Tabset w/ plot, summary, and table ----
    tabsetPanel(type = "tabs",
                tabPanel("Introduction", value = 1, textOutput("intro1"),textOutput("intro2")),
                tabPanel("Plots", value = 2, plotOutput("distPlot"), plotOutput("kmean"),
                         textOutput("lowest_k")),
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
  df <- read_csv("approval_polllist.csv")
  df$study <- as.factor(df$study)
  df$treat <- as.factor(df$treat)

  # You should define all the rendered output in the server
  output$distPlot <- renderPlot({
    return(ggplot(data = df, aes(x = measure , y = age, color = study)) + geom_point())
  })
}

# Run the application
shinyApp(ui = ui, server = server)
