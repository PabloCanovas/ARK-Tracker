library(shiny)

ui <- basicPage(
  plotOutput("plot1", click = "plot_click"),
  verbatimTextOutput("info")
)

server <- function(input, output) {
  output$plot1 <- renderPlot({
    browser() # Here, the browser stops but when run the below line, cannot see the plot in plots tab
    plot(mtcars$wt, mtcars$mpg) # or ggplot ...both not rendering plot in browser mode
  })
  
  output$info <- renderText({
    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
  })
}

shinyApp(ui, server)

