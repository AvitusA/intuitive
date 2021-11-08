#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)
library(ggsci)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("SAAS Acquisition Initiative"),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput(inputId = "initial_monthly_revenue",
                   label = "Monthly Rev Per New Customer",
                   value = 5),
      numericInput(inputId = "customer_growth",
                   label = "Annual Per-Customer Revenue Growth",
                   value = 0.1),
      numericInput(inputId = "retention",
                   label = "Month-Over-Month Retention",
                   value = 0.8),
      hr(),
      numericInput(inputId = "monthly_acquisition",
                   label = "Monthly Nr of New Customers",
                   value = 100),
      numericInput(inputId = "customer_cac",
                   label = "Cost of Acquisition - Per Customer",
                   value = 1500),
      numericInput(inputId = "fixed_cac",
                   label = "Cost of Acquisition - Monthly",
                   value = 0)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("mau_plot")
    )
  )
)

cohort_mau <- function(cohort_month, calendar_month, initial_size, retention) {
  if_else(cohort_month > calendar_month, as.double(NA), initial_size * retention ** (calendar_month - cohort_month))
}

cohort_revenue <- function(cohort_month, calendar_month, initial_size, retention, initial_revenue, customer_growth) {
  if_else(cohort_month > calendar_month, as.double(NA),
          cohort_mau(cohort_month, calendar_month, initial_size, retention) 
          * initial_revenue 
          * (1 + customer_growth) ** (calendar_month - cohort_month))
}

months_to_simulate <- 48

simulate <- function(months_to_simulate, initial_cohort_size, retention, initial_revenue, customer_growth) {
  cohorts <- crossing(month = seq(1:months_to_simulate),
                      cohort = seq(1:months_to_simulate)) %>%
    mutate(mau = cohort_mau(cohort, month, initial_cohort_size, retention),
           revenue = cohort_revenue(cohort, month, initial_cohort_size, retention, initial_revenue, customer_growth))
  cohorts
}

mau_plot <- function(sim) {
  sim %>% 
    ggplot(aes(x = month, y = mau)) + 
    geom_col(aes(fill=as.factor(cohort)), position = position_stack(reverse=F)) + 
    scale_fill_discrete(name = "Cohort") +
    scale_y_continuous(name = "Monthly Active Users")
}

arr_plot <- function(sim) {
  sim %>% 
    ggplot(aes(x = month, y = revenue * 12)) + 
    geom_col(aes(fill=as.factor(cohort)), position = position_stack(reverse=F)) + 
    scale_fill_discrete(name = "Cohort") +
    scale_y_continuous(name = "Annual Recurring Revenue")
}

cumrev <- function(sim) {
  sim %>% 
    replace_na(list(revenue = 0)) %>% 
    group_by(cohort) %>% 
    arrange(month) %>% 
    mutate(cum_revenue = cumsum(revenue)) %>% 
    ggplot(aes(x = month, y = cum_revenue)) + 
    geom_col(aes(fill=as.factor(cohort))) +
    scale_fill_discrete(name = "Cohort") +
    scale_y_continuous(name = "Cumulative Revenue")
}

# Define server logic required to draw a histogram
server <- function(input, output) {
  sim <- reactive({
    simulate(months = months_to_simulate, 
             initial_cohort_size = input$monthly_acquisition,
             retention = input$retention,
             initial_revenue = input$initial_monthly_revenue,
             customer_growth = input$customer_growth)
  })
  
  output$mau_plot <- renderPlot(
    mau_plot(sim()) %>% ggplotly()
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
