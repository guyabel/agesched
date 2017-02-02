library(shiny)
library(tidyverse)
library(agesched)

ui <- fluidPage(
  titlePanel("Relational Models of Mortality"),
  mainPanel(
    tabsetPanel(
      tabPanel(
        title = "Brass",
        fluidRow(
          column(width = 4, sliderInput(inputId = "alpha1", label = "alpha", value = 0, step = 0.1, min = -5, max = 5)),
          column(width = 4, sliderInput(inputId = "beta1", label = "beta", value = 1, step = 0.1, min = 0.5, max = 2))
        ),
        plotOutput("plot1"),
        verbatimTextOutput("text1"),
        verbatimTextOutput("text2")
      )
    )
  )
)

server <- function(input, output){
  output$plot1 <- renderPlot({
    df0 <- subset(austria, Year == 2014)
    f0 <- df0$Lx_f
    f1 <- brass_mort(model = f0, x = df0$Age, alpha = input$alpha1, beta = input$beta1)
    df1 <- data_frame(Age = df0$Age, Relational = f1, Model = f0) %>%
      gather(key = "Schedule", value = "Lx", -Age)
    ggplot(data = df1,
           mapping = aes(x = Age, y = Lx, colour = Schedule)) +
      geom_line() +
      theme_bw()
  })
  print_f0 <- reactive({
    df0 <- subset(austria, Year == 2014)
    f0 <- df0$Lx_f
    paste0("Model e0: ", round(sum(f0)/100000,3))
  })
  output$text1 <- renderPrint(
    print_f0()
  )
  print_f1 <- reactive({
    df0 <- subset(austria, Year == 2014)
    f0 <- df0$Lx_f
    f1 <- brass_mort(model = f0, x = df0$Age, alpha = input$alpha1, beta = input$beta1)
    paste0("Relational e0: ", round(sum(f1)/100000,3))
  })
  output$text2 <- renderPrint(
    print_f1()
  )
}
shinyApp(ui = ui, server =  server)
