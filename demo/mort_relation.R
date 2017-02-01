library(shiny)
library(tidyverse)

ui <- fluidPage(
  titlePanel("Relational Models of Mortility"),
  mainPanel(
    tabsetPanel(
      tabPanel(
        title = "Brass",
        fluidRow(
          column(width = 4, sliderInput(inputId = "alpha1", label = "alpha", value = 0, step = 0.1, min = -5, max = 5)),
          column(width = 4, sliderInput(inputId = "beta1", label = "beta", value = 1, step = 0.1, min = 0.5, max = 2))
        ),
        plotOutput("plot1"),
        verbatimTextOutput("text1")
      )
    )
  )
)

server <- function(input, output){
  output$plot1 <- renderPlot({
    df0 <- subset(austria, Year == 2014)
    df0$lx_f <- df0$Lx_f + df0$Dx_f/2
    f0 <- df0$lx_f

    df1 <- brass_mort(model = f0, x = df0$Age, alpha = input$alpha1, beta = input$beta1)
    ggplot(data = df1,
           mapping = aes(x = x)) +
      geom_line(mapping = aes(y = lx_s)) +
      geom_line(mapping = aes(y = lx, colour = "red")) +
      labs(y = "lx", x = "Age") +
      theme_bw() 
  })
  output$text1 <- renderPrint(
    
    print(
      brass_mort(model = f0, x = df0$Age, alpha = input$alpha1, beta = input$beta1) %>% 
            select(lx_s, lx, ex)
          )
  )
}
shinyApp(ui = ui, server =  server)
