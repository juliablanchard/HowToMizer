# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(mizer)
require(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Exploring Time Series"),
   
     fluidRow(
       column(4, wellPanel(
         sliderInput("Rmax", "log10 Maximum Recruitment:", min = round(log10(min(params@species_params$R_max)),0.1), max = round(log10(max(params@species_params$R_max)),0.1), value = 14,
                       step = 0.1),
         sliderInput("erepro", "log10 Reproductive Efficiency:", min = -8, max = 1, value = -2,
                     step = 0.1),
         selectInput("Species", "Species:",
                     c("Sprat","Sandeel","N.pout","Herring","Dab","Whiting", "Sole","Gurnard","Plaice","Haddock","Cod","Saithe"))
       )),
       column(6,
              plotOutput("distPlot", width = 600, height = 600)
       )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$distPlot <- renderPlot({
    
   
    #change erepro of selected species
    params@species_params$erepro[params@species_params$species==input$Species] <- 10^input$erepro
    #change Rmax of selected species
    params@species_params$R_max[params@species_params$species==input$Species] <- 10^input$Rmax
    
    params <- setReproduction(params)
    
    #re-run model
    simt <- project(params, effort=relative_effort, dt = 0.1, t_save = 1,initial_n = sim@n[200,,],initial_n_pp = sim@n_pp[200,])
    
    #re-run model
    simt <- project(params, effort=relative_effort, dt = 0.1, t_save = 1,initial_n = sim@n[200,,],initial_n_pp = sim@n_pp[200,])
    
    # output adjusted modelled yields and reshape for plotting
    y <- getYield(simt)
    y <- reshape2::melt(y)
    
    
    p<-ggplot(y) + geom_line(data=filter(y,sp==input$Species), aes(x = time, y = value, 
                                                             colour = sp)) +
      geom_point(data=filter(obsy,sp==input$Species),aes(x = time, y = value, 
                                                   colour = sp),size=0.6) +
      #facet_wrap(~sp) +
      scale_y_continuous(name = "Yield [g/year]")  +
      scale_colour_manual(values = sim@params@linecolour) +
      xlim(1965, 2011)
    p
    

   })
}

# Run the application 
shinyApp(ui = ui, server = server)

