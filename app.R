library(shiny)
library(readxl)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(shinythemes)
library(shinyFiles)
library(rsconnect)

# Define UI
ui <- fluidPage(
  theme = shinytheme("readable"),
  titlePanel("AKTAprime plus FPLC Gel Filtration Plots"),
  tabsetPanel(
    tabPanel("Time Plot",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file_time", "Please select your excel file:"),
                 numericInput("elution_time", "Enter the elution time:", value = 0),
                 textInput("plot_title_time", "Plot Title:"),
                 tags$hr(),
                 tags$h4("X-axis Settings"),
                 textInput("x_axis_time", "X-axis Label:", "Retention Time (min)"),
                 fluidRow(
                   column(6, numericInput("x_min", "Min:", value = 0)),
                   column(6, numericInput("x_max", "Max:", value = 300))
                 ),
                 numericInput("x_breaks", "Breaks Interval:", value = 10),
                 tags$hr(),
                 tags$h4("Y-axis Settings"),
                 textInput("y_axis_time", "Y-axis Label:", "Absorbance at 280 nm (mAu)"),
                 fluidRow(
                   column(6, numericInput("y_min", "Min:", value = 0)),
                   column(6, numericInput("y_max", "Max:", value = 100))
                 ),
                 numericInput("y_breaks", "Breaks Interval:", value = 10),
                 tags$hr(),
                 numericInput("plot_height_time", "Plot Height (pixels):", value = 600),
                 numericInput("plot_width_time", "Plot Width (pixels):", value = 1000),
                 downloadButton("download_time_plot", "Download Time Plot")
               ),
               mainPanel(
                 plotOutput("plot_time"),
                 uiOutput("time_plot_error")
               )
             )
    ),
    tabPanel("Volume Plot",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file_volume", "Please select your file:"),
                 numericInput("elution_time_volume", "Enter the elution time:", value = 0),
                 numericInput("flow_rate_volume", "Enter the flow rate:", value = 0.8),
                 textInput("plot_title_volume", "Plot Title:"),
                 tags$hr(),
                 tags$h4("X-axis Settings"),
                 textInput("x_axis_volume", "X-axis Label:", "Volume (mL)"),
                 fluidRow(
                   column(6, numericInput("x_min_volume", "Min:", value = 0)),
                   column(6, numericInput("x_max_volume", "Max:", value = 150))
                 ),
                 numericInput("x_breaks_volume", "Breaks Interval:", value = 10),
                 tags$hr(),
                 tags$h4("Y-axis Settings"),
                 textInput("y_axis_volume", "Y-axis Label:", "Absorbance at 280 nm (mAu)"),
                 fluidRow(
                   column(6, numericInput("y_min_volume", "Min:", value = 0)),
                   column(6, numericInput("y_max_volume", "Max:", value = 100))
                 ),
                 numericInput("y_breaks_volume", "Breaks Interval:", value = 10),
                 tags$hr(),
                 numericInput("plot_height_volume", "Plot Height (pixels):", value = 600),
                 numericInput("plot_width_volume", "Plot Width (pixels):", value = 1000),
                 downloadButton("download_volume_plot", "Download Volume Plot")
               ),
               mainPanel(
                 plotOutput("plot_volume"),
                 uiOutput("volume_plot_error")
               )
             )
    )
  ),
  
  
  tags$div(
    tags$p("This shiny application allows to upload FPLC data files of gel filtration purification and generates publication-ready plots."),
    tags$p("The data have been taken from AKTAprime plus FPLC system and app may be inapplicable to other systems."),
    tags$p("AKTAprime plus FPLC Gel Filtration Plots by Ildam, D. (OKE Lab)"),
    style = "margin-bottom: 20px;"
  )
)





# Define server logic
server <- function(input, output) {
  
  # Reactive expression for the time plot
  time_plot <- reactive({
    req(input$file_time)
    
    raw_df <- read_xls(input$file_time$datapath)
    
    elution_time <- input$elution_time
    
    selected_df <- raw_df[, 1:2]
    
    cleaned_df <- selected_df[-(1:2), ] %>%
      setNames(c("min", "mAu")) %>%
      mutate(across(everything(), as.numeric))
    
    result_df <- cleaned_df %>%
      mutate(adjusted_time = min - elution_time) %>%
      filter(adjusted_time >= 0)
    
    ggplot(result_df, aes(x = adjusted_time, y = mAu)) +
      geom_line(size = 1, colour = "black") +
      scale_y_continuous(
        name = input$y_axis_time,
        limits = c(input$y_min, input$y_max),
        breaks = seq(input$y_min, input$y_max, by = input$y_breaks)
      ) +
      scale_x_continuous(
        name = input$x_axis_time,
        limits = c(input$x_min, input$x_max),
        breaks = seq(input$x_min, input$x_max, by = input$x_breaks)
      ) +
      ggtitle(input$plot_title_time) +
      theme_pubr() +
      labs_pubr() +
      theme(axis.title.y = element_text(face = "bold", color = "black", size = 13),
            axis.title.x = element_text(face = "bold", size = 13),
            plot.title = element_text(hjust = 0.5))
  })
  
  # Reactive expression for the volume plot
  volume_plot <- reactive({
    req(input$file_volume)
    
    raw_df2 <- read_xls(input$file_volume$datapath)
    
    elution_time <- input$elution_time
    flow_rate <- input$flow_rate_volume
    
    selected_df2 <- raw_df2[, 1:2]
    
    cleaned_df2 <- selected_df2[-(1:2), ] %>%
      setNames(c("min", "mAu")) %>%
      mutate(across(everything(), as.numeric))
    
    result_df2 <- cleaned_df2 %>%
      mutate(adjusted_min = min - elution_time) %>%
      mutate(adjusted_volume = adjusted_min * flow_rate) %>%
      filter(adjusted_min >= 0)
    
    ggplot(result_df2, aes(x = adjusted_volume, y = mAu)) +
      geom_line(size = 1, colour = "black") +
      scale_y_continuous(
        name = input$y_axis_volume,
        limits = c(input$y_min_volume, input$y_max_volume),
        breaks = seq(input$y_min_volume, input$y_max_volume, by = input$y_breaks_volume)
      ) +
      scale_x_continuous(
        name = input$x_axis_volume,
        limits = c(input$x_min_volume, input$x_max_volume),
        breaks = seq(input$x_min_volume, input$x_max_volume, by = input$x_breaks_volume)
      ) +
      ggtitle(input$plot_title_volume) +
      theme_pubr() +
      labs_pubr() +
      theme(
        axis.title.y = element_text(face = "bold", color = "black", size = 13),
        axis.title.x = element_text(face = "bold", size = 13),
        plot.title = element_text(hjust = 0.5)
      )
  })
  
  # Render the time plot
  output$plot_time <- renderPlot({
    time_plot()
  }, height = function() { input$plot_height_time }, width = function() { input$plot_width_time })
  
  # Render the volume plot
  output$plot_volume <- renderPlot({
    volume_plot()
  }, height = function() { input$plot_height_volume }, width = function() { input$plot_width_volume })
  
  # Download the time plot as PNG
  output$download_time_plot <- downloadHandler(
    filename = function() {
      paste("time_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = time_plot(), device = "png", width = input$plot_width_time / 100, height = input$plot_height_time / 100, units = "in", dpi = 300)
    }
  )
  
  # Download the volume plot as PNG
  output$download_volume_plot <- downloadHandler(
    filename = function() {
      paste("volume_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = volume_plot(), device = "png", width = input$plot_width_volume / 100, height = input$plot_height_volume / 100, units = "in", dpi = 300)
    }
  )
}


# Run the application
shinyApp(ui = ui, server = server)
