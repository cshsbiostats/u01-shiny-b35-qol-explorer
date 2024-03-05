library(tidyverse)
library(shiny)
library(bslib)
library(ggsankey)
library(patchwork)
library(bsicons)

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

sidebar_layout <- sidebar(
  selectInput(
    inputId = 'select_trt',
    label = tooltip(
      span("1. Select Treatment",
           bs_icon("info-circle")),
      "This dropdown selects the treatment of interest at the starting timeframe.",
      placement = "right"
    ),
    choices = trt_options
  ),
  selectInput(
    inputId = 'select_desc',
    label = tooltip(
      span("2. Select Description",
           bs_icon("info-circle")),
      "This dropdown selects the QOL description of the cohort of interest at the initial starting timeframe",
      placement = "right"
    ),
    choices = desc_options
  ),
  selectInput(
    inputId = 'select_resp',
    label = tooltip(
      span("3. Response",
           bs_icon("info-circle")),
      "This dropdown selects the response for the QOL description for the cohort of interest at the initial starting timeframe",
      placement = "right"
    ),
    choices = resp_options
  ),
  sliderInput(
    inputId = 'select_timeframe',
    label = tooltip(
      span("4. Timeframe",
           bs_icon("info-circle")),
      "This slider selects the initial and end timeframe in months for the cohort of interest to visualize.",
      placement = "right"
    ),
    min = 0,
    max = 60,
    value = c(0, 12),
    step = 6
  ),
  actionButton(
    'btn_ae_visualize',
    tooltip(
      span("Visualize",
           bs_icon("info-circle")),
      "Clicking on the following button will generate a Sankey diagram and summary statement based upon the selected patient cohort.",
      placement = "right"
    ),
    icon = icon(name = 'chart-bar', lib = 'font-awesome')
  ),
  downloadButton(
    'report',
    tooltip(
      span("Download Report",
           bs_icon("info-circle")),
      "Clicking on the following button will generate a PDF report containing the Sankey diagrams and summary statement based upon the select patient cohort.",
      placement = "right"
    ),
    icon = icon(name = 'chart-bar', lib = 'font-awesome')
  )
)

main <- layout_sidebar(
  fillable = TRUE,
  sidebar = sidebar_layout,
  card(
    card_header("B35 QOL Cohort Explorer"),
    card_body(
      "The following application allows you to visualize the PRO-CTCAE QOL data in a Sankey diagram format. The Sankey diagram is a useful tool for visualizing the flow of patients between different timepoint of interest. The following application allows you to select the treatment, QOL description, and response of the cohort of interest at a specified timepoint. We can then observe the flow and responses of the patients across the various timepoints."
    ),
    max_height = '250px'
  ),
  card(
    full_screen = TRUE,
    card_header("Results"),
    card_body(class = "p-0", plotOutput('sankey_plot')),
    card_body(htmlOutput('summary_descr'), max_height = '300px'),
    height = '750px'
  )
)

ui <- page_fillable(
  theme = bs_theme(version = 5, preset = "shiny"),
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
