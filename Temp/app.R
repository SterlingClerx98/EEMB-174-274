# A little shiny app to estimate the current prevalence of a disease including false positive rates
# Written by Stephen Proulx sproul@ucsb.edu

library(shiny)

library(tidyverse)
library(rstan)
options(mc.cores = parallel::detectCores())
library(bayesplot)


prevalence_model <- readRDS("model.rds") 



# Define UI
ui <- fluidPage(
    
    # Application title
    titlePanel("Estimate Prevalence"),
    
    
    sidebarLayout(
        sidebarPanel(
            sliderInput("n_pre",
                        "Number of samples in pre-disease test set",
                        min = 0,
                        max = 1000,
                        value = 401),
            sliderInput("pos_pre",
                        "Number of samples that tested positive in pre-disease test set",
                        min = 0,
                        max = 1000,
                        value = 2),
            sliderInput("n_sample",
                        "Number of samples in active epidemic test set",
                        min = 1,
                        max = 10000,
                        value = 3330),
            sliderInput("pos_pos",
                        "Number of samples that tested positive in active epidemic test set",
                        min = 1,
                        max = 10000,
                        value = 50),
            # sliderInput("lbound",
            #             "lower bound on prior for prevalence",
            #             min = 0,
            #             max = 1,
            #             value = 0),
            # sliderInput("ubound",
            #             "upper bound on prior for prevalence",
            #             min = 0,
            #             max = 1,
            #             value = 1),
            actionButton("do", "Run Models")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("results")
        )
    )
)



# Define server logic 
server <- function(input, output) {
    
    observeEvent(input$do, {
        
        
        
        stan_data <-   list( n_pre = input$n_pre , pos_pre = input$pos_pre , 
                             n_sample=input$n_sample , pos_pos = input$pos_pos ,
                             prior_prev = c(0,1))
        
        stanfit_bin_antibody <- sampling(prevalence_model, data = stan_data, chains = 2,
                                     iter = 1000, seed = 213123)
        
        
        post <- posterior_samples(stanfit_bin_antibody)
        
        output$results <-  renderPlot({
            mcmc_areas(post,area_method = "equal height" , pars=c("mu"),prob=.95)+
                scale_x_continuous(limits = c(0,input$pos_pos/input$n_sample) )+
                labs(title = "Bounds of estimate prevelance",
                     x = "Disease Prevalence", y = "Posterior Probability Density") +
                theme_bw()   
        })
        
        
    })
    
    
    
   
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)