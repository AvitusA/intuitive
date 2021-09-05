library(ablabsetup)

ui <- shiny::fluidPage(
    shiny::tags$head(
        shiny::tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    shiny::withMathJax(),
    shiny::titlePanel("A/B Test - Sample Size Calculator"),
    shiny::sidebarLayout(
        shiny::sidebarPanel(shiny::tabsetPanel(
            shiny::tabPanel(
                "Basic",
                shiny::br(),
                shiny::numericInput(
                    "total_volume",
                    "Total number of exposures per day",
                    min = 1,
                    max = 1000,
                    value = 100
                ),
                shiny::helpText("The number of exposures is the number of opportunities to convert. If you're measuring conversion rate from ad impression to click, the number of exposures is the number of ad impressions."),
                shiny::br(),
                shiny::sliderInput(
                    "baseline",
                    "Conversion Rate (%) of Version A [Control]",
                    min = 0.1,
                    max = 99.9,
                    value = 10
                ),
                shiny::helpText(paste("Current/expected conversion rate of Version A. This will be used as a baseline to measure uplift over.")),
                shiny::br(),
                shiny::sliderInput(
                    "expected_effect",
                    "Assumed uplift (%) of Version B [Treatment]",
                    min = -100,
                    max = 100,
                    value = 25
                ),
                shiny::helpText("How much better conversion rate you expect Version B to have."),
                shiny::helpText("Example: Conversion Rate A: 10%, Assumed uplift: 25%, the expected conversion rate of B is implied to be 25% higher than that of A (i.e: 12.5%)."),
                shiny::br(),
                shiny::sliderInput(
                    "critical_effect",
                    "Experiment Win Margin (%)",
                    min = -100,
                    max = 100,
                    value = 15
                ),
                shiny::helpText(paste("If you require that one version is better with a margin, you may specify it here. If you don't want to use this, you can leave it at 0%")),
                shiny::helpText("Example 1: Experiment Win Criteria = 15% means B needs to have an uplift of at least 15% in order for it to win the experiment"),
                shiny::helpText("Example 2: Experiment Win Criteria = -15% means B wins as long as its conversion rate is not more than 15% worse than A")
                                      
            ),
            shiny::tabPanel(
                "Advanced",
                shiny::br(),
                shiny::helpText("The default parameters on this tab are sane, and are most likely what you want if you do not understand what they do."),
                shiny::br(),
                shiny::sliderInput(
                    "split",
                    "% of Exposures assigned to Version A",
                    min = 0.1,
                    max = 99.9,
                    value = 50
                ),
                shiny::helpText("How large proportion of the total number of exposures to expose to Version A"),
                shiny::helpText("Unless you are using non-default priors, a 50/50-split requires the fewest number of total exposures except in extreme cases."),
                shiny::br(),
                shiny::h4("Prior: Version A"),
                fluidRow(
                    column(4,
                           shiny::selectInput("a_distr",
                                              "Distribution",
                                              c("Beta"),
                                              selected = "Beta")),
                    column(
                        4,
                        shiny::numericInput(
                            "a_prior_alpha",
                            "alpha",
                            min = 1,
                            max = Inf,
                            value = 1
                        )
                    ),
                    column(
                        4,
                        shiny::numericInput(
                            "a_prior_beta",
                            "beta",
                            min = 1,
                            max = Inf,
                            value = 1
                        )
                    ),
                    column(12,
                           shiny::helpText(
                               shiny::textOutput("prior_a_interval"))
                    )
                ),
                shiny::br(),
                shiny::h4("Prior: Version B"),
                fluidRow(
                    column(4,
                           shiny::selectInput("b_distr",
                                              "Distribution",
                                              c("Beta"),
                                              selected = "Beta")),
                    column(
                        4,
                        shiny::numericInput(
                            "b_prior_alpha",
                            "alpha",
                            min = 1,
                            max = Inf,
                            value = 1
                        )
                    ),
                    column(
                        4,
                        shiny::numericInput(
                            "b_prior_beta",
                            "beta",
                            min = 1,
                            max = Inf,
                            value = 1
                        )
                    ),
                    column(12,
                           shiny::helpText(
                               shiny::textOutput("prior_b_interval"))
                    )
                ),
                shiny::br(),
                shiny::selectInput(
                    "samples",
                    "Computation quality",
                    c("fast", "accurate", "very accurate"),
                    "accurate"
                ),
                shiny::helpText(paste("Controls accuracy of simulation. \"accurate\"",
                                      "should be sufficient for except for extremely low baseline conversion rates. Increase this if plots appear jagged")),
                class = "collapse",
                id = "advanced-options"
            )
        )),
        shiny::mainPanel(
            shiny::h3("Experiment Simulation"),
            shiny::withMathJax(shiny::plotOutput("scenario_plot")),
            shiny::h3("Insights"),
            shiny::tableOutput("insights"),
            shiny::uiOutput("no_insights_ui")
        )
    )
)

renderErrors <- function(errors) {
    if (length(errors) > 0) {
        bullets <-
            purrr::map(errors, ~ paste("<li>", .x, "</li>", sep = "")) %>%
            purrr::reduce(stringr::str_c, .init = "")
        paste("<h3>Please correct the following errors</h3><ul>",
              bullets,
              "</ul>",
              sep = "")
    } else {
        ""
    }
}

interpret_beta_prior <- function(alpha, beta) {
    if(alpha == 1 && beta == 1) {
        return ("This prior corresponds to the belief that all conversion rates between 0% and 100% are equally likely.")
    }
    cred_mass <- 0.9
    the_hdi <- HDInterval::hdi(rbeta(1e3, alpha, beta), credMass = cred_mass)
    return(paste("This prior corresponds to the belief that conversion rate is between ", 
                 round(100 * the_hdi[[1]], 1), 
                 "% and ", 
                 round(100 * the_hdi[[2]], 1),
                 "% with ",
                 round(100 * cred_mass), "% probability.",
                 sep = ""))
}

server <- function(input, output) {
    distr_tbl <- shiny::reactive({
        samples <- if (input$samples == "very accurate") {
            1e6
        } else if (input$samples == "accurate") {
            1e5
        } else {
            5e3
        }
        a_prior_alpha <-
            tidyr::replace_na(input$a_prior_alpha, 1)
        a_prior_beta <- tidyr::replace_na(input$a_prior_beta, 1)
        b_prior_alpha <-
            tidyr::replace_na(input$b_prior_alpha, 1)
        b_prior_beta <- tidyr::replace_na(input$b_prior_beta, 1)
        
        days <- c(seq(1, 14), seq(21, 84, 7), seq(91, 365, 28))
        day_tbl <- days_vs_volume(
            days,
            input$total_volume,
            input$split / 100,
            input$baseline / 100,
            input$baseline / 100 * (1 + input$expected_effect /
                                        100)
        )
        distr_tbl <- effect_distributions(
            day_tbl,
            a_prior = list(alpha = a_prior_alpha,
                           beta = a_prior_beta),
            b_prior = list(alpha = b_prior_alpha,
                           beta = b_prior_beta),
            samples = samples,
            crit = input$critical_effect / 100
        )
    })
    
    output$prior_a_interval <- shiny::renderText({
        interpret_beta_prior(input$a_prior_alpha, input$a_prior_beta)
    })

    output$prior_b_interval <- shiny::renderText({
        interpret_beta_prior(input$b_prior_alpha, input$b_prior_beta)
    })
    
    scenario <- shiny::reactive({
        validate(
            need(
                input$baseline / 100 * (1 + input$expected_effect / 100) < 1,
                "Expected conversion rate exceeds 100%. Lower Baseline Conversion Rate or Expected change in Conversion Rate"
            ),
            need(
                input$a_prior_alpha > 0,
                "$\\alpha_A$ must to be greater than 0"
            ),
            need(
                input$a_prior_beta > 0,
                "$\\beta_A$ must to be greater than 0"
            ),
            need(
                input$b_prior_alpha > 0,
                "$\\alpha_B$ must to be greater than 0"
            ),
            need(
                input$b_prior_beta > 0,
                "$\\beta_B$ must to be greater than 0"
            )
        )
        credible_vs_time(
            distr_tbl(),
            volume_per_day = input$total_volume,
            levels = c(0.8, 0.95)
        )

    })
    
    output$scenario_plot <- shiny::renderPlot({
        scenario()$plot
    })
    
    output$insights <- shiny::renderTable({
        the_table <- scenario()$insights
        if(nrow(the_table) > 0) {
            the_table %>%
                dplyr::transmute(
                    `Day` = as.integer(day),
                    Exposures = human_format(total_volume, 2),
                    Insight = desc,
                    Probability = paste(round(level * 100), "%", sep = "")
                )
        } else {
            NULL
        }
    }, 
    width = "100%",
    striped = T, rownames = T)
    
    output$no_insights_ui <- shiny::renderUI({
        if(nrow(scenario()$insights) == 0) {
            list(
                shiny::tags$p(paste("An experiment with these parameters won't generate any insights within the first", 
                                    max(distr_tbl()$day), "days. Here are some things you can try:"), class="no-insights"),
                shiny::tags$ul(
                    shiny::tags$li("Increase the total number of exposures per day."),
                    shiny::tags$li("Increase baseline conversion rate."),
                    shiny::tags$li("Increase OR Decrease assumed uplift."),
                    shiny::tags$li("Increase OR Decrease experiment win margin."),
                    shiny::tags$li("Consider using priors (Advanced tab)")
                )
            )
        } else {
            NULL
        }
    })
}

# Run the application
shinyApp(ui = ui,
         server = server,
         options = list(host = "127.0.0.1"))
