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



# In this problem, we assume that there is a probability that you have cold
# symptoms when you don't have covid, for example because you have a cold. 
# There is some fraction of the population who has covid, and given that an 
# individual has covid, they have a chance of having cold symptoms

# fixed parameters for the example
probSgC <- 0.25 # probability of symptoms given covid


# Prepare our dataframes 


params <- tibble(
    probSgNC   = seq(from=0, to=1,by=0.01),
    incidence   = seq(from=0, to=1,by=0.01)
    )

# All possible combinations of false positive and incidence rate.
params <- params %>% expand(probSgNC, incidence) %>%
    mutate(probCgS = probSgC * incidence /(probSgC * incidence + probSgNC * (1-incidence)  ) ,
           info = log(probCgS/incidence))
           


    

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Inferring covid status from cold like symptoms"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("prevalence",
                        "Disease prevalence in the population:",
                        min = 0.01,
                        max = 0.99,
                        value = 0.5)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("probPlot")
        )
    )
)

# Define server logic required to draw the plot
server <- function(input, output) {

    output$probPlot <- renderPlot({ggplot(data=filter(params,incidence==input$prevalence),
                                          aes(x=probSgNC, y = probCgS)) + 
            geom_line()+
            scale_y_continuous( limits=c(0,1))+
            labs( x="Probability of colds among the non-covid population" , y="Probability of covid given symptoms")
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
