library(shiny)
library(shinythemes)
library(readxl)
library(GrowthCurveME)
library(data.table)
library(flextable)
library(ggplot2)
library(readr)
library(viridis)
library(htmltools)
library(DT)
library(officer)
library(ggtree)
library(tinytex)
library(shinycssloaders)
library(shinyjs)
library(magrittr)
library(dplyr)

shinyUI(fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("GrowthCurveME RShiny"),
  sidebarLayout(
    sidebarPanel(
      fileInput("data_file", "Upload Input Data (.xlsx, .csv, .txt):",
                accept = c(".xlsx, .csv, .txt")),
      selectInput("model_function", "Growth Function:",
                  choices = c("exponential", "linear", "logistic", "gompertz"),
                  selected = "exponential"),
      selectInput("model_type", "Regression Type:",
                  choices = c("mixed", "least-squares"),
                  selected = "mixed"),
      selectInput("fixed_rate", "Fixed Rate:",
                  choices = c("True", "False"),
                  selected = "True"),
      
      textInput("time_unit", "Time Unit:", value = "hours"),
      textInput("report_title", "Report Title:"),
      actionButton("run", "Run Analysis"),
      downloadButton("download_report", "Download Results"),
      downloadButton("download_sample", "Download Sample Data")
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "File Input and Instructions Page",
          
          h3("Introduction"),
          p(
            style = "font-size: 14px;",
            HTML(
              "In-vitro growth assays are often prone to multiple sources of variability, 
              including well-to-well variation in seeding densities, variation across experimental replicates, 
              and variation between equipment and facilities, all of which can significantly impact the reliability of growth estimates. 
              These types of variability often stem from clustering, where data points within the same cluster, such as a well or experimental replicate, 
              are more similar (or correlated) to each other than to data points in other clusters. 
              Ignoring this clustering, as with traditional summary metrics or simple regression models, 
              can lead to biased estimates and limit the reproducibility and generalizability of the results. 
              The goal of GrowthCurveME is to address these aspects of variability by allowing users to easily fit linear and non-linear mixed-effects regression models to account for clustering 
              (e.g., at the well-to-well level or experimental replicate level)."
            )
          ),
          
          h3("Instructions"),
          p(
            style = "font-size: 14px;",
            HTML(
              'GrowthCurveME allows users to account for the well-to-well variation by fitting a mixed-effects model with the growth_curve_model_fit() function. 
              In this case, the data appears to follow an exponential curve, so one can set the function_type to “exponential”. 
              GrowthCurveME is also able to fit “linear”, “logistic”, and “gompertz” shaped functions to data. 
              Download the R package version of GrowthCurveME from CRAN and visit our GitHub page to learn more: <a href="https://github.com/cancermodels-org/GrowthCurveME" target="_blank">https://github.com/cancermodels-org/GrowthCurveME</a>.'
            )
          ),
          p(style = "font-size: 16px;", "The step-by-step instructions will guide you through the analysis process effectively."),
          tags$ol(
            tags$li(
              style = "font-size: 14px;",
              "Upload your dataset using the 'Upload Input Data' button. The file must be in *.xlsx, *.csv, or *.txt format and input file should contain at least the following columns: 'cluster', 'time', and 'growth_metric'."
            ),
            tags$li(style = "font-size: 14px;", "Select model settings from the sidebar:"),
            tags$ul(
              tags$li(style = "font-size: 14px;", tags$b("Growth Function:"), " Choose the model to fit your data. Options include 'exponential', 'linear', 'logistic', and 'gompertz'."),
              tags$li(style = "font-size: 14px;", tags$b("Regression Type:"), " Select 'mixed' for mixed-effects models or 'least-squares' for least-squares fitting."),
              tags$li(style = "font-size: 14px;", tags$b("Fixed Rate:"), " Specify whether the rate is fixed ('True') or not ('False')."),
              tags$li(style = "font-size: 14px;", tags$b("Time Unit:"), " Enter the unit of time used in your dataset, such as 'hours' or 'days'.")
            ),
            tags$li(style = "font-size: 14px;", "Click 'Run Analysis' to perform the analysis with the selected settings."),
            tags$li(style = "font-size: 14px;", "Navigate through the tabs to view the results:"),
            tags$ul(
              tags$li(style = "font-size: 14px;", tags$b("Regression Summary Table:"), " View a summary table generated by the growth_model_summary_table() function."),
              tags$li(style = "font-size: 14px;", tags$b("Growth Plots:"), " Visualize scatter plots generated by the growth_vs_time() function."),
              tags$li(style = "font-size: 14px;", tags$b("Residual Diagnostics:"), " Visualize scatter plots generated by the growth_model_residual_plots() function.")
            ),
            tags$li("Download the results using the 'Download Results' button."),
            p(
              style = "font-size: 14px; margin-top: 10px;",
              "Citation: Panigrahy A (2024). GrowthCurveME: Mixed-Effects Modeling for Growth Data. R",
              "package version 0.1.0, ",
              tags$a(href = "https://CRAN.R-project.org/package=GrowthCurveME", "https://CRAN.R-project.org/package=GrowthCurveME.")
            )
          ),
          h3("View Your Data"),
              plotOutput("InstructionsPlot", width = "600px", height = "400px")
        ),
        
        tabPanel("Regression Summary Table",
                 fluidRow(
                   column(6, sliderInput("header_font_size", "Table Header Font Size:", min = 8, max = 32, value = 14, width = '100%')),
                   column(6, sliderInput("body_font_size", "Table Body Font Size:", min = 8, max = 32, value = 12, width = '100%'))
                 ),
                 div(style = "display: flex; justify-content: center;",
                     uiOutput("summary_table")
                 )
        ),

        tabPanel("Growth Plots", 
                 fluidRow(
                   column(3, selectInput("plot_type", "Select Plot Type:",
                                         choices = c("Growth vs Time \nwith Model Predictions",
                                                     "Growth vs Time \nwith Model Predictions by Cluster", 
                                                     "Prediction Intervals with Doubling Time 95%CI Annotation"),
                                         selected = "Growth vs Time \nwith Model Predictions")),
                   column(3, textInput("growth_metric", "Growth Metric Name:", value = "growth_metric")),
                   column(3, textInput("growth_metric_units", "Growth Metric Units:", value = "growth_unit")), #, value = "Growth Unit"
                   column(3, textInput("cluster", "Cluster Name:", value = "cluster"))
                 ),
                 fluidRow(
                   column(4, numericInput("x_min", "Minimum X Value:", value = NA)),
                   column(4, numericInput("x_max", "Maximum X Value:", value = NA)),
                   column(4, sliderInput("n_x_axis_breaks", "Number of X Axis Breaks:", min = 3, max = 16, value = 3)),
                   column(4, numericInput("y_min", "Minimum Y Value:", value = NA)),
                   column(4, numericInput("y_max", "Maximum Y Value:", value = NA)),
                   column(4, sliderInput("n_y_axis_breaks", "Number of Y Axis Breaks:", min = 3, max = 16, value = 4))
                 ),
                 
                 div(style = "display: flex; justify-content: left;",uiOutput("dynamic_plot"))
                 
        ),
        
        tabPanel(
          "Residual Diagnostics",
          fluidRow(
            column(6, selectInput("residual_type", "Residual Type:",choices = c("population", "cluster"))),
            column(6, selectInput("weighted", "Weighted:", choices = c("Yes" = TRUE, "No" = FALSE), selected = TRUE))
          ),
          # Centering the plot 
          div(style = "display: flex; justify-content: center;",plotOutput("diagnostics"))
        ),
      )
    )
  )
)

)
