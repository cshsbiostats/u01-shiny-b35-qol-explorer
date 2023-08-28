library(tidyverse)
library(shiny)
library(bslib)
library(ggsankey)
library(patchwork)

data <- read_csv(here::here('data/processed_full_qol_pro_data.csv'),
                 show_col_types = FALSE)

data <- data |> 
  select(patientid, trt, time_point_numeric, value, description)

trt_options <- data |> pull(trt) |> unique() |> sort()

desc_options <- data |> 
  pull(description) |> 
  unique()

resp_options <- data |> 
  pull(value) |> 
  unique() |> 
  sort()

main <- layout_sidebar(
  fillable = TRUE,
  sidebar = sidebar(
    selectInput(
      inputId = 'select_trt',
      label = '1. Select Treatment',
      choices = trt_options
    ),
    selectInput(
      inputId = 'select_desc',
      label = '2. Select Description',
      choices = desc_options
    ),
    selectInput(
      inputId = 'select_resp',
      label = '3. Select Response',
      choices = resp_options
    ),
    sliderInput(
      inputId = 'select_timeframe',
      label = '4. Select Timeframe',
      min = 0,
      max = 60,
      value = c(0, 12),
      step = 6
    ),
    actionButton(
      'btn_ae_visualize',
      'Visualize',
      icon = icon(name = 'chart-bar', lib = 'font-awesome')
    ),
    downloadButton(
      'report',
      'Download Report',
      icon = icon(name = 'chart-bar', lib = 'font-awesome')
    )
  ),
  card(full_screen = TRUE, card_header("Results"), card_body(class = "p-0", plotOutput('sankey_plot')), card_body(htmlOutput('summary_descr')))
)

ui <- page_fillable(
  main
)

shinyApp(ui, function(input, output) {
  
  results <- eventReactive(input$btn_ae_visualize, {
    
    make_qol_sankey(
      data = data,
      selected_treatment = input$select_trt,
      selected_description = input$select_desc,
      selected_response = input$select_resp,
      selected_timeframe = input$select_timeframe
    )
    
  })
  
  observeEvent(input$btn_ae_visualize, {
    
    req(results())
    
    output$sankey_plot <- renderPlot({
      
      results()$plot
      
    })
    
    output$summary_descr <- renderPrint({
      
      results()$summary_description
    })
    
  })
  
  
  output$report <- downloadHandler(
    filename = \(x) {paste0('pro_ctcae_ae_sankey_', Sys.Date(), '.pdf')},
    content = function(file) {

      rmarkdown::render(
        'explore_qol_template.Rmd',
        output_file = file,
        params = list('results' = results()),
        envir = new.env(parent = globalenv())
      )

    }
  )

})
