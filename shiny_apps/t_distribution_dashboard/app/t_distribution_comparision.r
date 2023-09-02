library(shiny)
library(ggplot2)
library(tidyverse)

ui <- fluidPage(
  titlePanel("T-Distribution vs Normal Distribution"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("sample_size", "Sample Size", min = 5, max = 100, value = 10),
      # Inserted content
      h4("Summary:"),
      p("The \"T-Distribution vs Normal Distribution\" Shiny app allows users to visualize the differences between the t-distribution and the normal distribution, particularly when dealing with smaller sample sizes. By adjusting the sample size, users can observe how these distributions change and gain insights into their behavior."),
      h4("Features:"),
      p("1. Sample Size Variation: Users can adjust the sample size using a slider to see the impact on the distributions."),
      p("2. Main Distribution Plot: The app displays the t-distribution and normal distribution on the same graph, making it easy to compare their shapes."),
      p("3. Lower Tail Zoom-In: A focused view of the lower tail highlights differences between the t-distribution and the normal distribution's tails."),
      h4("Insights:"),
      p("- As sample size increases, the t-distribution approaches the normal distribution, illustrating the central limit theorem."),
      p("- With smaller sample sizes, the t-distribution's tails are fatter, reflecting increased uncertainty due to limited data.")
    ),
    mainPanel(
      plotOutput("distribution_plot"),
      plotOutput("lower_tail_plot")
    )
  )
)

server <- function(input, output) {
  distribution_data <- reactive({
    sample_size <- input$sample_size
    
    value <- seq(-4, 4, length.out = 1000)
    t_dist <- dt(value, df = sample_size - 1)
    normal_dist <- dnorm(value)
    
    data.frame(value = value, t_dist = t_dist, normal_dist = normal_dist) %>%
        gather(key = "distribution", value = "density", -value)
  })
  
  output$distribution_plot <- renderPlot({
    df <- distribution_data()
    p <- ggplot(df, aes(x = value, y = density)) +
      geom_line(aes(col = distribution, lty = distribution)) +
      labs(title = paste("T-Distribution vs Normal Distribution\nSample Size:", input$sample_size),
           x = "Value", y = "Density") +
      theme_minimal() +
      theme(legend.position = "top")
    p
  })
  
  output$lower_tail_plot <- renderPlot({
    df <- distribution_data()
    lower_tail_p <- ggplot(df, aes(x = value, y = density)) +
      geom_line(aes(col = distribution, lty = distribution)) +
      coord_cartesian(ylim = c(0, 0.05), xlim = c(-4, -2)) +
      labs(title = paste("Zoomed-In Lower Tail\nSample Size:", input$sample_size),
           x = "Value", y = "Density") +
      theme_minimal() +
      theme(legend.position = "top")
    lower_tail_p
  })
}

shinyApp(ui, server)
