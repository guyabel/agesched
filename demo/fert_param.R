library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Parametric Fertility Age Schedules"),
  fluidRow(
    column(width = 4, sliderInput(inputId = "tfr", label = "TFR", value = 2.1, step = 0.1, min = 1, max = 9)),
    column(width = 4, sliderInput(inputId = "age", label = "Child Bearing Ages", value = c(15, 15+35), step = 1, min = 10, max = 60)),
    column(width = 4, radioButtons(inputId = "age_grp", label = "Age Groups:", choices = c("Single Year" = 1, "Five Years" = 5), selected = 1))
  ),
  mainPanel(
    tabsetPanel(
      tabPanel(
        title = "Romainuk",
        fluidRow(
          column(width = 4, sliderInput(inputId = "mean_cb1", label = "Mean Age of Child Bearing", value = 26, step = 0.1, min = 15, max = 40)),
          column(width = 4, sliderInput(inputId = "mode_cb1", label = "Modal Age of Child Bearing", value = 25, step = 0.1, min = 15, max = 40))
        ),
        plotOutput("plot1"),
        verbatimTextOutput("text1")
      ),
      tabPanel(
        title = "Hadwiger",
        fluidRow(
          column(width = 4, sliderInput(inputId = "H2", label = "H Parameter", value = 3, step = 0.1, min = 0, max = 10)),
          column(width = 4, sliderInput(inputId = "T2", label = "T Parameter", value = 26, step = 0.1, min = 15, max = 40)),
          column(width = 4, sliderInput(inputId = "d2", label = "d Parameter", value = 2, step = 0.1, min = 0, max = 10))
        ),
        plotOutput("plot2"),
        verbatimTextOutput("text2")
      ),
      tabPanel(
        title = "Gamma",
        fluidRow(
          column(width = 4, sliderInput(inputId = "mean_cb3", label = "Mean Age of Child Bearing", value = 26, step = 0.1, min = 15, max = 40)),
          column(width = 4, sliderInput(inputId = "shape3", label = "Shape Parameter", value = 5, step = 0.1, min = 0, max = 10))
        ),
        plotOutput("plot3"),
        verbatimTextOutput("text3")
      ),
      tabPanel(
        title = "Gage",
         plotOutput("plot4"),
         verbatimTextOutput("text4")
      )
    )
  )
)

server <- function(input, output){
  output$plot1 <- renderPlot({
    f0 <- romaniuk(tfr = input$tfr, mean_cb = input$mean_cb1, mode_cb = input$mode_cb1,
                   start_fertage = input$age[1], width_fertage = input$age[2] - input$age[1],
                   x = seq(from = 0, to = 100, by = as.numeric(input$age_grp)))
    df0 <- data.frame(asfr = f0, x = seq(from = 0, to = 100, by = as.numeric(input$age_grp)))
    ggplot(data = df0,
           mapping = aes(y = asfr , x = x)) +
      geom_line() +
      labs(y = "ASFR", x = "Age") +
      theme_bw() +
      scale_x_discrete(lim = c(10, 60))
  })
  output$text1 <- renderPrint(
    print(paste0(
      "romaniuk(tfr = ",input$tfr," x = seq(from = 0, to = 100, by = ",as.numeric(input$age_grp),"), mean_cb = ",input$mean_cb1,", mode_cb = ",input$mode_cb1,", start_fertage = ",input$age[1],", width_fertage = ",input$age[2] - input$age[1],")"
    ))
  )
  output$plot2 <- renderPlot({
    f0 <- hadwiger(tfr = input$tfr, H = input$H2, T = input$T2, d = input$d2,
                   start_fertage = input$age[1], width_fertage = input$age[2] - input$age[1],
                   x = seq(from = 0, to = 100, by = as.numeric(input$age_grp)))
    df0 <- data.frame(asfr = f0, x = seq(from = 0, to = 100, by = as.numeric(input$age_grp)))
    ggplot(data = df0,
           mapping = aes(y = asfr , x = x)) +
      geom_line() +
      labs(y = "ASFR", x = "Age") +
      theme_bw() +
      scale_x_discrete(lim = c(10, 60))
  })
  output$text2 <- renderPrint(
    print(paste0(
      "hadwiger(tfr = ",input$tfr," x = seq(from = 0, to = 100, by = ",as.numeric(input$age_grp),"), H = ",input$H2,", T = ",input$T2,", d = ",input$d2,", start_fertage = ",input$age[1],", width_fertage = ",input$age[2] - input$age[1],")"
    ))
  )
  output$plot3 <- renderPlot({
    f0 <- gamma_fert(tfr = input$tfr, mean_cb = input$mean_cb3, shape = input$shape3,
                     start_fertage = input$age[1], width_fertage = input$age[2] - input$age[1],
                     x = seq(from = 0, to = 100, by = as.numeric(input$age_grp)))
    df0 <- data.frame(asfr = f0, x = seq(from = 0, to = 100, by = as.numeric(input$age_grp)))
    ggplot(data = df0,
           mapping = aes(y = asfr , x = x)) +
      geom_line() +
      labs(y = "ASFR", x = "Age") +
      theme_bw() +
      scale_x_discrete(lim = c(10, 60))
  })
  output$text3 <- renderPrint(
    print(paste0(
      "gamma_fert(tfr = ",input$tfr," x = seq(from = 0, to = 100, by = ",as.numeric(input$age_grp),"), mean_cb = ",input$mean_cb3,", shape = ",input$shape3,", start_fertage = ",input$age[1],", width_fertage = ",input$age[2] - input$age[1],")"
    ))
  )
  output$plot4 <- renderPlot({
    f0 <- gage(tfr = input$tfr,
               start_fertage = input$age[1], width_fertage = input$age[2] - input$age[1],
               x = seq(from = 0, to = 100, by = as.numeric(input$age_grp)))
    df0 <- data.frame(asfr = f0, x = seq(from = 0, to = 100, by = as.numeric(input$age_grp)))
    ggplot(data = df0,
           mapping = aes(y = asfr , x = x)) +
      geom_line() +
      labs(y = "ASFR", x = "Age") +
      theme_bw() +
      scale_x_discrete(lim = c(10, 60))
  })
  output$text4 <- renderPrint(
    print(paste0(
      "gage(tfr = ",input$tfr," x = seq(from = 0, to = 100, by = ",as.numeric(input$age_grp),"), start_fertage = ",input$age[1],", width_fertage = ",input$age[2] - input$age[1],")"

    ))
  )
}
shinyApp(ui = ui, server =  server)
