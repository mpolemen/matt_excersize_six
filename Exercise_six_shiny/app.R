# This is the absolute bare minimum of what I need to create a shiny app.
# Beware! ... This alone will be a REALLY boring app. A blank page :(

# You will create an app to compare states' cumulative number of COVID cases over time. 
# The x-axis will be number of days since 20+ cases and the y-axis will be cumulative cases on the 
# log scale (`scale_y_log10()`). We use number of days since 20+ cases on the x-axis so we can make 
# better comparisons of the curve trajectories. You will have an input box where the user can choose 
# which states to compare (`selectInput()`) and have a submit button to click once the user has chosen
# all states they're interested in comparing. The graph should display a different line for each state, 
# with labels either on the graph or in a legend. Color can be used if needed.

library(shiny)
library(tidyverse)


ui <- fluidPage(
  selectInput(inputId = "states", 
              label = "Choose a State",
              choices = unique(covid19$state),
              multiple = TRUE),
  #submitButton(text = "Create Plot!"),
  plotOutput(outputId = "covidtimeplot")
)

server <- function(input, output) {
  output$covidtimeplot <- renderPlot({
    covid19 %>% 
      group_by(state) %>%
      filter(cases >= 20) %>%
      mutate(days_since = as.numeric(difftime(date, lag(date, 1))),
             Between = ifelse(is.na(days_since), 0, days_since),
             days_since20 = cumsum(as.numeric(between))) %>%
      select(~days_since, ~Between) %>%
      filter(state %in% input$state) %>% 
      ggplot(aes(x = days_since20 , y = cases, color = state)) +
      geom_line() +
      scale_y_log10() +
      scale_x_discrete() +
      labs(title = "Days Since 20 Covid Cases by State",
           x = "Days Since 20 Cases",
           y = "Total Cases")
  })
}

shinyApp(ui = ui, server = server)