demoAirDatepicker("years")

library(shiny)
library(shinyWidgets)


ui <- fluidPage(
  airDatepickerInput(
    inputId = "multiple",
    label = "Select multiple dates:",
    placeholder = "You can pick 5 dates",
    multiple = 5, clearButton = TRUE
  ),
  verbatimTextOutput("res")
)

server <- function(input, output, session) {
  output$res <- renderPrint(input$multiple)
}
  shinyApp(ui, server)