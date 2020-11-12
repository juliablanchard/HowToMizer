#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(mizer)

# Define server logic required to draw a plot
shinyServer(function(input, output) {
   
  output$distPlot <- renderPlot({
    # set up params using values given, need check and change parameter values so units work in days units 
    params@species_params$erepro <- rep(10^input$erepro,dim(params@species_params)[1])
   # params@species_params$Rmax <- rep(10^input$Rmax,12)
    params <- setParams(params,kappa=10^input$kappa)
    # run without fishing
    sim <- project(params, effort = 1, t_max = 100)
    plot(sim)
     })

})
