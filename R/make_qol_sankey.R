make_qol_sankey <- \(data, selected_treatment, selected_description, selected_timeframe, selected_response) {
  
  response_order <- c(
    'No response',
    'Not at all',
    'Slightly',
    'Moderately',
    'Quite a bit',
    'Extremely'
  )
  
  data <- local({
    data <- data |>
      select(patientid, trt, time_point_numeric, value, description) |>
      mutate(time_point_numeric = time_point_numeric * 6)
    
    data <- data |>
      filter(time_point_numeric <= 60)
    
    data <- data |>
      filter(trt == selected_treatment) |>
      group_by(patientid) |>
      filter(
        any(
          time_point_numeric == selected_timeframe[1] &
            description == selected_description &
            value == selected_response
        )
      ) |>
      ungroup() |>
      filter(
        time_point_numeric >= selected_timeframe[1] &
          time_point_numeric <= selected_timeframe[2]
      ) |>
      filter(description == selected_description) |>
      mutate(time_point_label = glue::glue('{time_point_numeric} M'))
    
  })
  
  total_patients <- data |>
    pull(patientid) |>
    unique() |>
    length()
  
  plot_data <- local({
    plot_data <- data |>
      select(-time_point_numeric) |>
      pivot_wider(names_from = time_point_label, values_from = value)
    
    plot_data <- plot_data |>
      make_long(4:ncol(plot_data))
  })
  
  plot <- local({
    grade_colors <- c(
      'No response' = "#1F77B4FF",
      'Not at all' = "#2CA02CFF",
      'Slightly' = "#BCBD22FF",
      'Moderately' = "#FF7F0EFF",
      'Quite a bit' = "#D62728FF",
      'Extremely' = "#7F7F7FFF"
    )
    
    plot_data <- plot_data |>
      mutate(node = factor(
        node,
        levels = response_order
      ))
    
    plot <- ggplot(
      plot_data,
      aes(
        x = x,
        next_x = next_x,
        node = node,
        next_node = next_node,
        fill = factor(node)
      )
    ) +
      geom_sankey(flow.alpha = .3) +
      theme_sankey(base_size = 20) +
      theme(
        legend.position = 'bottom',
        legend.title = element_blank(),
        axis.text.x = element_text(face = 'bold'),
        axis.title = element_blank()
      ) +
      scale_color_manual(values = grade_colors) +
      scale_fill_manual(values = grade_colors) +
      labs(
        title = selected_description,
        subtitle = glue::glue('{selected_treatment}, n = {total_patients}')
      )
    
    plot
  })
  
  summary_description <- local({
    res <- data |> 
      filter(time_point_numeric == selected_timeframe[2]) |> 
      count(value) |> 
      mutate(value = factor(value, levels = response_order)) |> 
      arrange(value) |> 
      mutate(
        prop = n / sum(n),
        prop = scales::percent_format()(prop)
      )
    
    summary_res <- glue::glue_data(res, 'A total of {n} ({prop}) of the patients responded "{value}". ') |> 
      glue::glue_collapse(sep = '<br>')
    
    
    summary_description <- glue::glue(
      'Among a total of {total_patients} patients who responded "{selected_response}" for "{selected_description}" at {selected_timeframe[1]} M.',
      'By the {selected_timeframe[2]} M timepoint',
      
      '{summary_res}',
      .sep = '<br><br>'
    )
  })
  
  tibble::lst(
    plot,
    summary_description
  )

}


# make_qol_sankey(
#   data = data,
#   selected_treatment = 'Tamoxifen',
#   selected_description = 'Breast pain',
#   selected_timeframe = c(0, 6),
#   selected_response = 'Not at all'
# )
