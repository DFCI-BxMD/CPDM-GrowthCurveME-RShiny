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

shinyServer(function(input, output, session) {
  
  # Reactive expression to handle file upload
  data <- eventReactive(input$data_file, { 
    req(input$data_file) 
    inFile <- input$data_file
    
    if (grepl("\\.csv$", inFile$name)) {
      df <- read.csv(inFile$datapath)
    } else if (grepl("\\.xlsx$", inFile$name)) {
      df <- read_excel(inFile$datapath)
    } else if (grepl("\\.txt$", inFile$name)) {
      df <- read.delim2(inFile$datapath, header = TRUE, sep = "\t")
    } else {
      stop("Unsupported file type")
    }
    if (!all(c("cluster", "time", input$growth_metric) %in% colnames(df))) {
      stop(paste("The file must contain 'cluster', 'time', and the specified '", input$growth_metric, "' column.", sep = ""))
    }
    # Check for >3 data points
    if (nrow(df) <= 3) {
      stop("The file must contain more than 3 data points.")
    }
    return(df)
  })
  
  # Render the data
  output$InstructionsPlot <- renderPlot({
    df <- data() 
    req(df)
    # scatter plot
    # Define sizes for text elements
    plot_title_size <- 10
    x_axis_title_size <- 10
    y_axis_title_size <- 10
    x_axis_text_size <- 8
    y_axis_text_size <- 8
    # scatter plot
    ggplot2::ggplot(df, 
                    ggplot2::aes(x = time, y = growth_metric, color = cluster)) +
      ggplot2::geom_point(size = 2) +
      viridis::scale_color_viridis(discrete = TRUE) +
      ggplot2::ylim(0, NA) +
      ggplot2::ggtitle("Growth vs Time Data") +
      ggplot2::theme_classic() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          hjust = 0.5,
          size = plot_title_size,
          face = "bold"
        ),
        axis.title.x = ggplot2::element_text(
          size = x_axis_title_size,
          color = "black", 
          face = "bold"
        ),
        axis.title.y = ggplot2::element_text(
          size = y_axis_title_size,
          colour = "black", 
          face = "bold"
        ),
        axis.text.x = ggplot2::element_text(
          size = x_axis_text_size,
          color = "black", 
          face = "bold"
        ),
        axis.text.y = ggplot2::element_text(
          size = y_axis_text_size,
          color = "black", 
          face = "bold"
        ),
        legend.title = ggplot2::element_text(hjust = 0.5, face = "bold")
      )
  }, width = 600, height = 400)

## everything after this require 'Run Analysis' is clicked
  observeEvent(input$run, {
    withProgress(message = "Running analysis", value = 0, {
      steps <- 5
      for (i in seq_len(steps)) {
        Sys.sleep(0.5)
        incProgress(1 / steps)
      }
    })
  })

  model_summary <- eventReactive(input$run, {
    req(data()) 
    df <- data() 
      ## growth curve model
      growth_curve_model_fit(
        data_frame = df,
        function_type = input$model_function,
        model_type = input$model_type,
        time_unit = input$time_unit,
        fixed_rate = as.logical(input$fixed_rate)
      )
  })
############
  summary_table <- reactive({
    req(model_summary())
    flextable_obj <- growth_model_summary_table(
      growth_model_summary_list = model_summary()
    )
    flextable_obj <- flextable::fontsize(flextable_obj, size = input$body_font_size, part = "body")
    flextable_obj <- flextable::fontsize(flextable_obj, size = input$header_font_size, part = "header")
    return(flextable_obj)
  })
  output$summary_table <- renderUI({
    req(summary_table())
    flextable_obj <- summary_table()
    html_table <- flextable::htmltools_value(flextable_obj, ft.align = "center")
    htmltools::HTML(as.character(html_table))
  })
  
  output$dynamic_plot <- renderUI({
    plotOutput("selected_plot", width = "600px", height = "400px")
  })
  # All the three growth_vs_time_plot() plots 
  output$selected_plot <- renderPlot({
    req(model_summary())
    
    # cluster name to use in the plot
    cluster <- if (input$cluster == "cluster") { #input$cluster_name
      input$cluster
    } else {
      input$cluster  # Use the custom name directly
    }
    # growth metric name and units for the y-axis
    growth_metric <- input$growth_metric
    growth_metric_units <- input$growth_metric_units
    
    switch(input$plot_type,
           "Growth vs Time \nwith Model Predictions" = {
             growth_vs_time_plot(
               growth_model_summary_list = model_summary(),
               plot_type = 2,
               growth_metric = paste0(growth_metric, " (", growth_metric_units, ")"),
               time_name = paste0("Time (", input$time_unit, ")"),
               #cluster_name = cluster_name,
               cluster = cluster,
               plot_title = "Growth vs Time \nwith Model Predictions",
               x_limits = c(input$x_min, input$x_max),  
               n_x_axis_breaks = input$n_x_axis_breaks,  
               y_limits = c(input$y_min, input$y_max),  
               n_y_axis_breaks = input$n_y_axis_breaks,  
               plot_title_size = 14,
               annotate_value_text_size = 5.5
             )
           },
           "Growth vs Time \nwith Model Predictions by Cluster" = {
             growth_vs_time_plot(
               growth_model_summary_list = model_summary(),
               plot_type = 3,
               growth_metric = paste0(growth_metric, " (", growth_metric_units, ")"),
               time_name = paste0("Time (", input$time_unit, ")"),
               #cluster_name = cluster_name,
               cluster = cluster,
               plot_title = "Growth vs Time \nwith Model Predictions by Cluster",
               x_limits = c(input$x_min, input$x_max),  
               n_x_axis_breaks = input$n_x_axis_breaks,  
               y_limits = c(input$y_min, input$y_max),  
               n_y_axis_breaks = input$n_y_axis_breaks, 
               plot_title_size = 14,
               annotate_value_text_size = 5.5
             )
           },
           "Prediction Intervals with Doubling Time 95%CI Annotation" = {
             growth_vs_time_plot(
               growth_model_summary_list = model_summary(),
               plot_type = 4,
               growth_metric = paste0(growth_metric, " (", growth_metric_units, ")"),
               time_name = paste0("Time (", input$time_unit, ")"),
               #cluster_name = cluster_name,
               cluster = cluster,
               plot_title = "Simulated Prediction Intervals of Model with\nDoubling Time 95%CI Annotation",
               x_limits = c(input$x_min, input$x_max),  
               n_x_axis_breaks = input$n_x_axis_breaks,  
               y_limits = c(input$y_min, input$y_max),  
               n_y_axis_breaks = input$n_y_axis_breaks, 
               plot_title_size = 14,
               annotate_value_text_size = 5.5
             )
           }
    )
  })
  ########
  observe({
    if (input$model_type == "mixed") {
      updateSelectInput(session, "residual_type",
                        choices = c("population", "cluster"),
                        selected = "cluster")
      updateSelectInput(session, "fixed_rate",
                        choices = c("True", "False"),
                        selected = "True")
    } else if (input$model_type == "least-squares") {
      updateSelectInput(session, "residual_type",
                        choices = c("population"),
                        selected = "population")
      updateSelectInput(session, "fixed_rate",
                        choices = c("True", "False"),
                        selected = "True")
    }
  })
  plot5_data <- reactive({
    req(model_summary())
    growth_model_residual_plots(
      growth_model_summary_list = model_summary(),
      residual_type = input$residual_type,
      weighted = as.logical(input$weighted)
    )
  })
  output$diagnostics <- renderPlot({
    plot5_data()
  }, height = 600, width = 700)
  
  ########## download_sample
  output$download_sample <- downloadHandler(
    filename = function() {
      "exp_mixed_data.csv"
    },
    content = function(file) {
      file.copy("exp_mixed_data.csv", file)
    }
  )
  
  ########## download_results (PDF only)
  ######## download report start here 
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("GrowthCurve_Report_", Sys.Date(), ".pdf",".zip")
    },
    content = function(file) {
# loading progress for download
      withProgress(message = "Downloading Result", value = 0, {
        steps <- 10  # Define the number of steps for progress
        incProgress(1 / steps) #, detail = "Setting up directories"
      
      rmd_path <- "growthcurve.Rmd"
      pdf_filename <- "Growth_Curve_Report.pdf"
      # Create a sub directory for PNG files and svg files
      png_dir <- "png"
      dir.create(png_dir, showWarnings = FALSE)
      svg_dir <- "svg"
      dir.create(svg_dir, showWarnings = FALSE)
      
      capitalize_and_replace_underscores <- function(text) {
        # Replace underscores with spaces
        text <- gsub("_", " ", text)
        # Capitalize the first letter of each word
        text <- tools::toTitleCase(text)
        return(text)
      }
      
      rmarkdown::render(
        input = rmd_path,
        output_file = pdf_filename,
        params = list(
          file_name = input$data_file$datapath,
          report_title = input$report_title,
          growth_metric_with_units = paste0(input$growth_metric, " (", input$growth_metric_units, ")"),
          growth_metric = gsub("_", "\\\\_", input$growth_metric), #input$growth_metric,  # Without units
          time = input$time_unit,
          cluster = input$cluster, #"cluster",
          model_type = input$model_type,
          function_type = input$model_function,
          fixed_rate = input$fixed_rate,
          # x and y limit, x and y axis break
          x_limits = c(input$x_min, input$x_max),  
          n_x_axis_breaks = input$n_x_axis_breaks,  
          y_limits = c(input$y_min, input$y_max),  
          n_y_axis_breaks = input$n_y_axis_breaks
        ),
        envir = new.env(parent = globalenv())
      )
   ########
      # Generate and save plots as PNGs in the png directory
      png_filename1 <- file.path(png_dir, "grow_vs_time.png")
      svg_filename1 <- file.path(svg_dir, "grow_vs_time.svg")
      plot_01 <- growth_vs_time_plot(
        growth_model_summary_list = model_summary(),
        plot_type = 2,
        growth_metric = paste0(input$growth_metric, " (", input$growth_metric_units, ")"),
        time = paste0("Time (", stringr::str_to_title(input$time_unit), ")"),
        cluster = input$cluster,
        plot_title = if(input$model_type == "mixed"){
          "Growth vs Time with Cluster\nLevel Predictions"
        } else {
          "Growth vs Time with Population\nLevel Predictions"
        },
        x_axis_text_size = 10,
        x_limits = c(input$x_min, input$x_max),  
        n_x_axis_breaks = input$n_x_axis_breaks,  
        y_limits = c(input$y_min, input$y_max),  
        n_y_axis_breaks = input$n_y_axis_breaks 
      )
      plot_01 <- plot_01 + guides(color = guide_legend(nrow = 15))
      ggsave(png_filename1, plot = plot_01, dpi = 300, width = 7.5, height = 5.5, units = "in")
      svg(svg_filename1,width = 7.5, height = 5.5)
      print(plot_01)
      dev.off()
      
      png_filename2 <- file.path(png_dir, "grow_vs_time_pred.png")
      svg_filename2 <- file.path(svg_dir, "grow_vs_time_pred.svg")
      plot_02 <- growth_vs_time_plot(
        growth_model_summary_list = model_summary(),
        plot_type = 4,
        growth_metric = paste0(input$growth_metric, " (", input$growth_metric_units, ")"),
        time = paste0("Time (", stringr::str_to_title(input$time_unit), ")"),
        cluster = input$cluster,
        plot_title = if (input$model_type == "mixed") {
          "Simulated Prediction Intervals with\nDoubling Time 95% CI Annotation"
        } else {
          "Prediction Intervals with\nDoubling Time 95% CI Annotation"
        },
        x_axis_text_size = 10,
        x_limits = c(input$x_min, input$x_max),  
        n_x_axis_breaks = input$n_x_axis_breaks,  
        y_limits = c(input$y_min, input$y_max),  
        n_y_axis_breaks = input$n_y_axis_breaks
      )
      plot_02 <- plot_02 + guides(color = guide_legend(nrow = 15))
      ggsave(png_filename2, plot = plot_02, dpi = 300, width = 7.5, height = 5.5, units = "in")
      svg(svg_filename2,width = 7.5, height = 5.5)
      print(plot_02)
      dev.off()
      png_filename3 <- file.path(png_dir, "res_pop_raw.png")
      svg_filename3 <- file.path(svg_dir, "res_pop_raw.svg")
      plot_03 <- growth_model_residual_plots(
        growth_model_summary_list = model_summary(),
        residual_type = "population",
        weighted = FALSE
      )
      ggsave(png_filename3, plot = plot_03, dpi = 300, width = 7.5, height = 5.5, units = "in")
      svg(svg_filename3,width = 7.5, height = 5.5)
      print(plot_03)
      dev.off()
      
      type <- if (input$model_type == "mixed") "wt" else "sd"
      png_filename4 <- file.path(png_dir, paste0("res_pop_", type, ".png"))
      svg_filename4 <- file.path(svg_dir, paste0("res_pop_", type, ".svg"))
      plot_04 <- growth_model_residual_plots(
        growth_model_summary_list = model_summary(),
        residual_type = "population",
        weighted = TRUE
      )
      ggsave(png_filename4, plot = plot_04, width = 7.5, height = 5.5, units = "in")
      svg(svg_filename4,width = 7.5, height = 5.5)
      print(plot_04)
      dev.off()
      
      if (input$model_type == "mixed") {
        png_filename5 <- file.path(png_dir, "res_clus_raw.png")
        svg_filename5 <- file.path(svg_dir, "res_clus_raw.svg")
        plot_05 <- growth_model_residual_plots(
          growth_model_summary_list = model_summary(),
          residual_type = "cluster",
          weighted = FALSE
        )
        ggsave(png_filename5, plot = plot_05, width = 7.5, height = 5.5, units = "in")
        svg(svg_filename5,width = 7.5, height = 5.5)
        print(plot_05)
        dev.off()
        
        png_filename6 <- file.path(png_dir, "res_clus_wt.png")
        svg_filename6 <- file.path(svg_dir, "res_clus_wt.svg")
        
        plot_06 <- growth_model_residual_plots(
          growth_model_summary_list = model_summary(),
          residual_type = "cluster",
          weighted = TRUE
        )
        ggsave(png_filename6, plot = plot_06, width = 7.5, height = 5.5, units = "in")
        svg(svg_filename6,width = 7.5, height = 5.5)
        print(plot_06)
        dev.off()
        
      }
      
      excel_filename <- "growth_results.xlsx"
      write_xlsx(model_summary(), excel_filename)
      
      plot_title7 <- if (input$model_type == "mixed") {
        "Growth vs Time by Cluster\nwith Cluster Level Predictions"
      } else {
        "Growth vs Time by Cluster\nwith Population Level Predictions"
      }
      png_filename7 <- file.path(png_dir, "grow_vs_time_by_clus.png")
      svg_filename7 <- file.path(svg_dir, "grow_vs_time_by_clus.svg")     
      
      plot_07 <- growth_vs_time_plot(
        growth_model_summary_list = model_summary(),
        plot_type = 3,
        growth_metric = paste0(input$growth_metric, " (", input$growth_metric_units, ")"),
        time = paste0("Time (", stringr::str_to_title(input$time_unit), ")"),
        cluster = input$cluster,
        plot_title = plot_title7,
        x_axis_text_size = 10,
        x_limits = c(input$x_min, input$x_max),  
        n_x_axis_breaks = input$n_x_axis_breaks,  
        y_limits = c(input$y_min, input$y_max),  
        n_y_axis_breaks = input$n_y_axis_breaks
      )
      ggsave(png_filename7, plot = plot_07, width = 7.5, height = 5.5, units = "in")
      svg(svg_filename7,width = 7.5, height = 5.5)
      print(plot_07)
      dev.off()
      
      png_filename8 <- file.path(png_dir, "grow_vs_time_raw.png")
      svg_filename8 <- file.path(svg_dir, "grow_vs_time_raw.svg")
      plot_08 <- growth_vs_time_plot(
        growth_model_summary_list = model_summary(),
        plot_type = 1,
        growth_metric = paste0(input$growth_metric, " (", input$growth_metric_units, ")"),
        time = paste0("Time (", stringr::str_to_title(input$time_unit), ")"),
        cluster = input$cluster,
        plot_title = "Growth vs Time",
        x_axis_text_size = 10,
        x_limits = c(input$x_min, input$x_max),  
        n_x_axis_breaks = input$n_x_axis_breaks,  
        y_limits = c(input$y_min, input$y_max),  
        n_y_axis_breaks = input$n_y_axis_breaks, 
      )
      ggsave(png_filename8, plot = plot_08, width = 7.5, height = 5.5, units = "in")
      svg(svg_filename8,width = 7.5, height = 5.5)
      print(plot_08)
      dev.off()
      # PNG file only
      png_files <- list.files(png_dir, pattern = "\\.png$", full.names = TRUE)
      
      # SVG file only
      svg_files <- list.files(svg_dir, pattern = "\\.svg$", full.names = TRUE)
      
      # Zip file for all
      files_to_zip <- c(pdf_filename, excel_filename, png_files, svg_files)
      
      zip(file, files_to_zip)
    })
  

})

})

