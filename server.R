#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(lattice)
library(ggplot2)
library(e1071)
library(caret)
library(randomForest)

#Prediction model - Random Forest

RFModel <- function() {
    fitControl <- trainControl(method = "cv", number = 5)
    fitRF <- train(Species ~ ., data = iris,
                   method = "rf",
                   trControl = fitControl)
    print(timestamp())
    return(fitRF)
}

predictIris <- function(trainedModel, inputs) {
    prediction <- predict(trainedModel,
                          newdata = inputs,
                          type = "prob",
                          predict.all = TRUE)
    return(renderTable(prediction))
}

# Define server logic required
shinyServer(function(input, output, session) {
    data("iris")
    
    output$outputSepalWidth <- renderText(input$sepalWidth)
    output$outputSepalLength <- renderText(input$sepalLength)
    output$outputPetalWidth <- renderText(input$petalWidth)
    output$outputPetalLength <- renderText(input$petalLength)
    
    output$outputSepalWidthSD <- renderText(sd(iris$Sepal.Width))
    output$outputSepalLengthSD <- renderText(sd(iris$Sepal.Length))
    output$outputPetalWidthSD <- renderText(sd(iris$Petal.Width))
    output$outputPetalLengthSD <- renderText(sd(iris$Petal.Length))
    
    output$outputSepalWidthMean <- renderText(mean(iris$Sepal.Width))
    output$outputSepalLengthMean <- renderText(mean(iris$Sepal.Length))
    output$outputPetalWidthMean <- renderText(mean(iris$Petal.Width))
    output$outputPetalLengthMean <- renderText(mean(iris$Petal.Length))
    
    output$plotSepalWidth <- renderPlot({
        ggplot(iris, aes(x = Sepal.Width,
                         group = Species,
                         fill = as.factor(Species))) + 
            geom_density(position = "identity", alpha = 0.5) +
            scale_fill_discrete(name = "Species") +
            theme_bw() +
            xlab("Sepal Width") +
            geom_vline(xintercept = input$sepalWidth,
                       color = "black",
                       size = 2) +
            scale_x_continuous(limits = c(round(min(iris$Sepal.Width) / 2, 1),
                                          round(max(iris$Sepal.Width) * 1.25, 1)))
        
    })
    
    output$plotSepalLength <- renderPlot({
        ggplot(iris, aes(x = Sepal.Length,
                         group = Species,
                         fill = as.factor(Species))) + 
            geom_density(position = "identity", alpha = 0.5) +
            scale_fill_discrete(name = "Species") +
            theme_bw() +
            xlab("Sepal Length") +
            geom_vline(xintercept = input$sepalLength,
                       color = "black",
                       size = 2) +
            scale_x_continuous(limits = c(round(min(iris$Sepal.Length) / 2, 1),
                                          round(max(iris$Sepal.Length) * 1.25, 1)))
        
    })
    
    output$plotPetalWidth <- renderPlot({
        ggplot(iris, aes(x = Petal.Width,
                         group = Species,
                         fill = as.factor(Species))) + 
            geom_density(position = "identity", alpha = 0.5) +
            scale_fill_discrete(name = "Species") +
            theme_bw() +
            xlab("Petal Width") +
            geom_vline(xintercept = input$petalWidth,
                       color = "black",
                       size = 2) +
            scale_x_continuous(limits = c(round(min(iris$Petal.Width) / 2, 1),
                                          round(max(iris$Petal.Width) * 1.25, 1)))
        
    })
    
    output$plotPetalLength <- renderPlot({
        ggplot(iris, aes(x = Petal.Length,
                         group = Species,
                         fill = as.factor(Species))) + 
            geom_density(position = "identity", alpha = 0.5) +
            scale_fill_discrete(name = "Species") +
            theme_bw() +
            xlab("Petal Length") +
            geom_vline(xintercept = input$petalLength,
                       color = "black",
                       size = 2) +
            scale_x_continuous(limits = c(round(min(iris$Petal.Length) / 2, 1),
                                          round(max(iris$Petal.Length) * 1.25, 1)))
        
    })
    
    builtModel <- reactive({
        RFModel()
    })
    
    observeEvent(
        eventExpr = input[["submitBtn"]],
        handlerExpr = {
            withProgress(message = 'Please Wait', value = 0, {
                myModel <- builtModel()
            })
            Sepal.Length <- input$sepalLength
            Sepal.Width <- input$sepalWidth
            Petal.Length <- input$petalLength
            Petal.Width <- input$petalWidth
            myEntry <- data.frame(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
            
            output$prediction <- predictIris(myModel, myEntry)
        })
})
