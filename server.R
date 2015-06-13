#
# - Server
#

# - load scripts and data
#   this code outside shinyServer() only gets executed once
source("scripts/flower_toolbox.R")
data(iris)
library(DT)

# - gets called multiple times
shinyServer(function(input, output) {
    
    # - unsupervised learning plots
    output$unsup_learn <- renderPlot({
        unsup_learn(method = input$unsup_method)
    })
    
    # - unsupervised learning summary
    output$unsup_learn_summary <- renderText({
        unsup_learn_summary(method = input$unsup_method)
    })
    
    # - data plot
    output$data_plot <- renderPlot({
        data_plot(method = input$data_plot_method)
    })
    
    # - data table
    output$data_table <- DT::renderDataTable(iris, options = list(pageLength = 10))
})