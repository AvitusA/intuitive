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
                   value = 5 * 99),
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
      numericInput(inputId = "cac_per_month",
                   label = "Monthly Fixed Cost of Acquisition",
                   value = 0)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h2("Monthly Active Users"),
      plotlyOutput("mau_plot"),
      h2("Annual Returning Revenue"),
      plotlyOutput("arr_plot"),
      h2("Revenue To Date"),
      plotlyOutput("cumrev_plot"),
      h2("Payback Time on CAC (Revenue only)"),
      plotlyOutput("pbt_plot")
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
          * (1 + customer_growth) ** ((calendar_month - cohort_month)/12))
}

cohort_pbt <- Vectorize(function(cac_per_customer, cac_per_month, cohort_size, retention, initial_revenue, customer_growth) {
  months <- seq(1:48)
  cohort_cac <- cohort_size * cac_per_customer + cac_per_month
  cumrev <- cohort_revenue(1, months, cohort_size, retention, initial_revenue, customer_growth) %>%
    cumsum()
  first(which(cumrev >= cohort_cac))
})

pbt_plot <- function(cac_per_month, cohort_size, retention, initial_revenue, customer_growth) {
  pbt_vs_cac <- tibble(cac_per_customer = seq(0, 10000, 100),
                       cohort_pbt = cohort_pbt(cac_per_customer, cac_per_month, cohort_size, retention, initial_revenue, customer_growth))
  the_plot <- pbt_vs_cac %>%
    # Remove redundant points, keep last point
    arrange(cac_per_customer) %>%
    rev() %>%
    mutate(last = row_number() == 1) %>%
    rev() %>%
    group_by(cohort_pbt) %>%
    filter(row_number() == 1 | last) %>%
    ungroup() %>%
    ggplot(aes(x = cac_per_customer, y = cohort_pbt)) +
    geom_line() +
    geom_point() +
    scale_y_continuous("PBT (single cohort)")
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
    mutate(cohort = as.factor(cohort)) %>%
    ggplot(aes(x = month, y = mau)) + 
    geom_col(aes(fill=cohort), position = position_stack(reverse=F)) + 
    scale_fill_discrete(name = "Cohort") +
    scale_y_continuous(name = "Monthly Active Users")
}

arr_plot <- function(sim) {
  sim %>% 
    mutate(cohort = as.factor(cohort)) %>%
    ggplot(aes(x = month, y = revenue * 12)) + 
    geom_col(aes(fill=cohort), position = position_stack(reverse=F)) + 
    scale_fill_discrete(name = "Cohort") +
    scale_y_continuous(name = "Annual Recurring Revenue")
}

cumrev_plot <- function(sim) {
  sim %>% 
    replace_na(list(revenue = 0)) %>% 
    group_by(cohort) %>% 
    arrange(month) %>% 
    mutate(cum_revenue = cumsum(revenue)) %>% 
    mutate(cohort = as.factor(cohort)) %>%
    ggplot(aes(x = month, y = cum_revenue)) + 
    geom_col(aes(fill=cohort)) +
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
  
  output$mau_plot <- renderPlotly(
    mau_plot(sim()) %>% ggplotly()
  )
  
  output$arr_plot <- renderPlotly(
    arr_plot(sim()) %>% ggplotly()
  )
  
  output$cumrev_plot <- renderPlotly(
    cumrev_plot(sim()) %>% ggplotly()
  )
  
  output$pbt_plot <- renderPlotly(
    pbt_plot(cac_per_month = input$cac_per_month, 
             cohort_size = input$monthly_acquisition,
             retention = input$retention, 
             initial_revenue = input$initial_monthly_revenue, 
             customer_growth = input$customer_growth) %>% ggplotly()
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
