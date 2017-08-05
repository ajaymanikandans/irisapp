#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(markdown)
# UI for application
shinyUI(navbarPage("Iris Species Prediction",
    tabPanel("Application",
        sidebarPanel(
            width = 4,
            h3('User Input'),
            sliderInput("sepalWidth",
                "Sepal Width:",
                min = round(min(iris$Sepal.Width)/2, 1),
                max = round(max(iris$Sepal.Width)*1.25, 1),
                value = round(mean(iris$Sepal.Width), 1)
            ),
            sliderInput("sepalLength",
                "Sepal Length:",
                min = round(min(iris$Sepal.Length)/2, 1),
                max = round(max(iris$Sepal.Length)*1.25, 1),
                value = round(mean(iris$Sepal.Length), 1)
            ),
            sliderInput("petalWidth",
                "Petal Width:",
                min = round(min(iris$Petal.Width)/2, 1),
                max = round(max(iris$Petal.Width)*1.25, 1),
                value = round(mean(iris$Petal.Width), 1)
            ),
            sliderInput("petalLength",
                "Petal Length:",
                min = round(min(iris$Petal.Length)/2, 1),
                max = round(max(iris$Petal.Length)*1.25, 1),
                value = round(mean(iris$Petal.Length), 1)
            ),
            actionButton(
                inputId = "submitBtn",
                label = "Submit"
            )
        ), 
        
        mainPanel(
            h3("Prediction Plot"),
            fluidRow(
                splitLayout(cellWidths = c("50%", "50%"),
                            plotOutput("plotSepalWidth", height = "180px"),
                            plotOutput("plotSepalLength", height = "180px")
                )),
            fluidRow(
                splitLayout(cellWidths = c("50%", "50%"),
                            plotOutput("plotPetalWidth", height = "180px"),
                            plotOutput("plotPetalLength", height = "180px")
                )),
            h3("Prediction Output"),
            tableOutput("prediction")
        )
    ),
    
    tabPanel(p(icon("info"), "About"),
        mainPanel(
            includeMarkdown("about.Rmd")
        )
    )
))
