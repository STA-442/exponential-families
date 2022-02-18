#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(bslib)
library(MASS)
library(statmod)
library(invgamma)
library(dplyr)
library(plotly)
library(gt)
my_theme <- bs_theme(
    bg = "#202123", 
    fg = "#B8BCC2", 
    primary = "#EA80FC", 
    base_font = font_google("Grandstander"),
    "font-size-base" = "1.1rem"
)


# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = my_theme,

    # Application title
    titlePanel("Simulate Data from an exponential family"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("family",
                        "Select Family:",
                        choices = c("Beta",
                                    "Binomial",
                                    "Exponential",
                                    "Gamma",
                                    "Inverse Gamma",
                                    "Normal",
                                    "Negative Binomial",
                                    "Poisson"),
                        selected = "Normal"),
            sliderInput("sample_size", "Sample Size", 
                        min = 1, 
                        max = 10000,
                        step = 1, value = 1000),
            conditionalPanel(
                condition = "input.family == 'Normal'",
                sliderInput("mean", "Mean", min = -500, 
                            max = 500,
                            step = .1, value = 0),
                sliderInput("sd", "Standard Deviation", min = 0.1, 
                            max = 500,
                            step = .1, value = 2)
            ),
            conditionalPanel(
                condition = "input.family == 'Beta'",
                sliderInput("alpha", "Alpha", min = 0, 
                            max = 100,
                            step = .1, value = 0),
                sliderInput("beta", "Beta", min = 0.1, 
                            max = 100,
                            step = .1, value = 1)
            ),
            conditionalPanel(
                condition = "input.family == 'Binomial'",
                sliderInput("prob", "Probability", min = 0, 
                            max = 1,
                            step = .01, value = .5)
            ),
            conditionalPanel(
                condition = "input.family == 'Gamma'",
                sliderInput("shape", "Shape", min = 0.01, 
                            max = 100,
                            step = .01, value = 1),
                sliderInput("scale", "Scale", min = 0.01, 
                            max = 100,
                            step = .01, value = 1)
            ),
            conditionalPanel(
                condition = "input.family == 'Exponential'",
                sliderInput("rate", "Rate", min = 0.1, 
                            max = 100,
                            step = .01, value = 5)
            ),
            conditionalPanel(
                condition = "input.family == 'Poisson'",
                sliderInput("lambda", "Rate", min = 0.1, 
                            max = 100,
                            step = .01, value = 5)
            ),
            conditionalPanel(
                condition = "input.family == 'Negative Binomial'",
                sliderInput("mu", "mu", min = 0.1, 
                            max = 100,
                            step = .01, value = 5),
                sliderInput("theta", "theta", min = 0.01, 
                            max = 100,
                            step = .01, value = 5)
            ),
            conditionalPanel(
                condition = "input.family == 'Inverse Gamma'",
                sliderInput("sh", "Shape", min = 0.1, 
                            max = 100,
                            step = .01, value = 1),
                sliderInput("sc", "Scale", min = 0.01, 
                            max = 100,
                            step = .01, value = 10)
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput("distribution_plot"),
            gt_output('formula')
        )
   
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    get_data <- reactive({
        dist_type <- input$family
        sample_size <- input$sample_size
        
        if(dist_type == "Normal") {
            mean <- input$mean
            sd <- input$sd
            y <- rnorm(sample_size, mean, sd)
            
            return(data.frame(y = y))
        } else if(dist_type == "Poisson") {
            lambda <- input$lambda
            y <- rpois(sample_size, lambda)
            
            return(data.frame(y = y))
        } else if(dist_type == "Binomial") {
            p <- input$prob
            y <- rbinom(sample_size, 1, p)
            
            return(data.frame(y = y))
        } else if(dist_type == "Gamma") {
            
            shape <- input$shape
            scale <- input$scale
            y <- rgamma(sample_size, shape = shape, scale = scale)
            
            y <- y[is.finite(y)]
            
            return(data.frame(y = y))
        } else if(dist_type == "Beta") {
            alpha <- input$alpha
            beta <- input$beta
            y <- rbeta(sample_size, alpha, beta)
            
            return(data.frame(y = y))
        } else if(dist_type == "Exponential") {
            rate <- input$rate
            y <- rexp(sample_size, rate)
            return(data.frame(y = y))
        } else if(dist_type == "Inverse Gamma") {
            scale <- input$sh
            shape <- input$sc
            y <- rinvgamma(sample_size, shape = shape, scale = scale)
            
            y <- y[is.finite(y)]
            return(data.frame(y = y))
        } else if(dist_type == "Negative Binomial") {
            mu <- input$mu
            theta <- input$theta
            y <- MASS::rnegbin(sample_size, mu = mu, theta = theta)
            
            return(data.frame(y = y))
        }
        
        
    })
    
    output$distribution_plot <- renderPlotly({
        
        data <- get_data()
        
        dist_type <- input$family
        
        if(dist_type == "Binomial") {
            
            hc <- data %>%
                count(y) %>% 
                ggplot(aes(y, n)) +
                geom_bar(stat='identity') 
            
            return(ggplotly(hc))
                       
            
        } else  {
            hc <- data %>% 
                ggplot(aes(y)) +
                geom_density(alpha=.2) +
                geom_histogram(aes(y=..density..), alpha=0.5, 
                               position="identity")
                
            
            return(ggplotly(hc))
        }
        
    })
    
    output$formula <- render_gt({
        
        data <- get_data()
        
        data_summary <- data %>% 
            summarize(min_val = min(y), 
                      max_val = max(y), 
                      mean_val = mean(y),
                      median_val = median(y),
                      sd_val = sd(y),
                      q_25 = quantile(y, .25),
                      q_75 = quantile(y, .75)) %>% 
            tidyr::pivot_longer(min_val:q_75,
                                names_to = "statistic", 
                                values_to = "values")
        
        tab <- data_summary %>% 
            gt() %>% 
            tab_header(title = "Summary stats for simulated data",
                    subtitle = glue::glue("simulated from {input$family}"))
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
