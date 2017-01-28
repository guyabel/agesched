library(shiny)
library(ggplot2)
ui <- fluidPage(
  column(4, sliderInput(inputId = "tfr", label = "TFR",
                        value = 2.1, step = 0.1,
                        min = 1, max = 9)
  ),
  column(4, sliderInput(inputId = "mean_cb", label = "Mean Age of Child Bearing",
              value = 26, step = 0.1,
              min = 15, max = 40),
  sliderInput(inputId = "mode_cb", label = "Modal Age of Child Bearing",
              value = 25, step = 0.1,
              min = 15, max = 40)
  ),
  column(4,
  sliderInput(inputId = "ages", label = "Child Bearing Ages",
              value = c(15, 15+35), step = 0.1,
              min = 10, max = 60)
  ),
  plotOutput("plot1"),
  textOutput("print1")
)
server <- function(input, output){
  output$plot1 <- renderPlot({
    f0 <- romaniuk(tfr = input$tfr, mean_cb = input$mean_cb, mode_cb = input$mode_cb,
                   start_fertage = input$ages[1], width_fertage = input$ages[2] - input$ages[1])
    df0 <- data.frame(asfr = f0, x = 0:100)
    ggplot(data = df0,
           mapping = aes(y = asfr , x = x)) +
      geom_line() +
      labs(y = "ASFR", x = "Age") +
      theme_bw() +
      xlim(c(10, 60))
  })
  output$print1 <- renderText({
    f0 <- romaniuk(tfr = input$tfr, mean_cb = input$mean_cb, mode_cb = input$mode_cb,
                   start_fertage = input$ages[1], width_fertage = input$ages[2] - input$ages[1])
    print(f0)
    print(input$ages)
  })
}
shinyApp(ui = ui, server =  server)
