#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(ggplot2)
library(shiny)
library(mizer)

#  https://sizespectrum.org/mizer/

# Define UI for application that draws a plot
shinyUI(fluidPage(
  
  # Application title
  titlePanel("North Sea Model Example"),
  
  fluidRow(
    column(4, wellPanel(
       sliderInput("kappa", "log10 Resource Carrying Capacity:", min = 8, max = 12, value = 10.7,
                   step = 0.1),
    #   sliderInput("Rmax", "log10 Maximum Recruitment:", min = 1, max = 12, value = 12,
    #              step = 0.1),
       sliderInput("erepro", "log10 Reproductive Efficiency:", min = -8, max = 1, value = -3,
                   step = 0.1)
          )),
    column(6,
           plotOutput("distPlot", width = 600, height = 600)
    ))
     
  
    
  )
)



