library(shiny)
library(tidyverse)
library(sf)

reactlog::reactlog_enable()
options(shiny.maxRequestSize = 100 * 1024 ^ 2)


ui <- fluidPage(
  shinyFeedback::useShinyFeedback(),
  titlePanel("Cálculo do Excesso de Velocidade"),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "file",
        label = NULL,
        accept = ".csv", 
        buttonLabel = "Selecione .csv...", 
        placeholder = "Nenhum arquivo selecionado."),
      actionButton("calc", label = "Calcular"),
      downloadButton("download", "Download dos resultados")
    ),
    mainPanel(
      h3("Prévia dos resultados:"),
      tableOutput("results")
    )
  )
)

server <- function(input, output, session) {
  file_read <- reactive({
    req(input$file)
    
    ext <- tools::file_ext(input$file$name)
    
    shinyFeedback::feedbackWarning(
      "file",
      ext != "csv", 
      "Formato inválido"
    )
    
    csv = vroom::vroom(input$file$datapath, delim = ",")
  })
  
  calc_results <- eventReactive(input$calc, {
    id <- showNotification(
      "Calculando...", 
      duration = NULL, 
      closeButton = FALSE
    )
    on.exit(removeNotification(id), add = TRUE)
    
    data <- file_read() %>% 
      driver_sf() %>% 
      join_cwb() 
    
    axis <- filter_axis(axis, data)
    
    data %>%  
      join_limits() %>% 
      speeding_calc()
  })
  
  output$results <- renderTable(
    head(
      calc_results() %>% 
        select(SPD_LIMIT, EXP, SPD_10, SPD_20, SPD_30, X, Y)
    )
  )
  
  output$download <- downloadHandler(
    filename = function() {
      paste0(input$upload, "resultado.csv")
    },
    content = function(file) {
     write_excel_csv(calc_results(), file)
    }
  )
}

shinyApp(ui, server)
