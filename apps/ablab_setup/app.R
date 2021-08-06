library(ablabsetup)

ui <- shiny::fluidPage(
    shiny::withMathJax(),
    # Application title
    shiny::titlePanel("Bayesian A/B Test - Pre-analysis"),
    shiny::sidebarLayout(
        shiny::sidebarPanel(
            shiny::numericInput(
                "total_volume",
                "Total Volume per day",
                min = 1,
                max = 1000,
                value = 100
            ),
            shiny::sliderInput(
                "split",
                "Volume Split (%)",
                min = 0.1,
                max = 99.9,
                value = 50
            ),
            shiny::sliderInput(
                "baseline",
                "Baseline Conversion Rate (%)",
                min = 0.1,
                max = 99.9,
                value = 10
            ),
            shiny::sliderInput(
                "expected_effect",
                "Expected change in Conversion Rate (%)",
                min = -100,
                max = 100,
                value = 25
            ),
            shiny::sliderInput(
                "critical_effect",
                "Experiment Success Criteria (%)",
                min = -100,
                max = 100,
                value = 3
            ),
            shiny::numericInput(
                "a_prior_alpha",
                "$$\\alpha_A$$",
                min = 1,
                max = Inf,
                value = 1
            ),
            shiny::numericInput(
                "a_prior_beta",
                "$$\\beta_A$$",
                min = 1,
                max = Inf,
                value = 1
            ),
            shiny::numericInput(
                "b_prior_alpha",
                "$$\\alpha_B$$",
                min = 1,
                max = Inf,
                value = 1
            ),
            shiny::numericInput(
                "b_prior_beta",
                "$$\\beta_B$$",
                min = 1,
                max = Inf,
                value = 1
            ),
            shiny::selectInput("samples", "Quality", c("fast", "accurate", "very accurate"), "accurate")
        ),
        shiny::mainPanel(shiny::h3("Experiment Simulation"),
                         shiny::withMathJax(
                             shiny::plotOutput("scenario_plot")
                         ),
                         shiny::h3("Insights"),
                         shiny::tableOutput("insights"))
    )
)

renderErrors <- function(errors) {
    if(length(errors) > 0) {
        bullets <- purrr::map(errors, ~ paste("<li>", .x, "</li>", sep="")) %>%
            purrr::reduce(stringr::str_c, .init = "")
        paste("<h3>Please correct the following errors</h3><ul>", bullets, "</ul>", sep = "")
    } else {
        ""
    }
}

server <- function(input, output) {
    distr_tbl <- shiny::reactive({
        samples <- if(input$samples == "very accurate") {
            1e6
        } else if(input$samples == "accurate") {
            1e5
        } else {
            5e3
        }
        a_prior_alpha <- tidyr::replace_na(input$a_prior_alpha, 1)
        a_prior_beta <- tidyr::replace_na(input$a_prior_beta, 1)
        b_prior_alpha <- tidyr::replace_na(input$b_prior_alpha, 1)
        b_prior_beta <- tidyr::replace_na(input$b_prior_beta, 1)
        
        days <- c(seq(1, 14), seq(21, 84, 7), seq(91, 365, 28))
        day_tbl <- days_vs_volume(days,
                                  input$total_volume,
                                  input$split / 100,
                                  input$baseline / 100,
                                  input$baseline / 100 * (1 + input$expected_effect/100))
        distr_tbl <- effect_distributions(day_tbl,
                                          a_prior = list(alpha = a_prior_alpha,
                                                         beta = a_prior_beta),
                                          b_prior = list(alpha = b_prior_alpha,
                                                         beta = b_prior_beta),
                                          samples = samples,
                                          levels = c(0.8, 0.95))
    })
    
    output$scenario_plot <- shiny::renderPlot({
        validate(
            need(input$baseline / 100 * (1 + input$expected_effect / 100) < 1, "Expected conversion rate exceeds 100%. Lower Baseline Conversion Rate or Expected change in Conversion Rate"),
            need(input$a_prior_alpha > 0, "$\\alpha_A$ must to be greater than 0"),
            need(input$a_prior_beta > 0, "$\\beta_A$ must to be greater than 0"),
            need(input$b_prior_alpha > 0, "$\\alpha_B$ must to be greater than 0"),
            need(input$b_prior_beta > 0, "$\\beta_B$ must to be greater than 0")
        )
        
        plot_uplift_credible_vs_time(distr_tbl(), critical_effect = input$critical_effect / 100)
    })
    
    output$insights <- shiny::renderTable({
        req(distr_tbl)
        timeline_insights(distr_tbl(), critical_effect = input$critical_effect / 100)
    })
}

# Run the application 
shinyApp(ui = ui, 
         server = server, 
         options = list(host = "127.0.0.1"))
