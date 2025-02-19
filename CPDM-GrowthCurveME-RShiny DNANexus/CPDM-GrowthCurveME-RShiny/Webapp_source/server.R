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
  
#file upload
data <- reactive({
  req(input$data_file) 
  inFile <- input$data_file
  if (grepl("\\.csv$", inFile$name)) {
    df <- read.csv(inFile$datapath)
  } else if (grepl("\\.xlsx$", inFile$name)) {
    df <- readxl::read_excel(inFile$datapath)
  } else if (grepl("\\.txt$", inFile$name)) {
    df <- read.delim2(inFile$datapath, header = TRUE, sep = "\t")
  } else {
    stop("Unsupported file type")
  }
    
#check columns required
if (!all(c("cluster", "time", input$growth_metric) %in% colnames(df))) {
  stop(paste("The file must contain 'cluster', 'time', and the specified '", input$growth_metric, "' column.", sep = ""))
}

# changing columns to numeric
df$time <- as.numeric(df$time)
df[[input$growth_metric]] <- as.numeric(df[[input$growth_metric]])
    
# Check for non-numeric conversion
if (any(is.na(df$time)) || any(is.na(df[[input$growth_metric]]))) {
  stop("Non-numeric values found in 'time' or growth metric columns. Please ensure all values are numeric.")
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
# simile scatter plot on first page
# sizes for text elements
plot_title_size <- 10
x_axis_title_size <- 10
y_axis_title_size <- 10
x_axis_text_size <- 8
y_axis_text_size <- 8
# scatter plot
ggplot2::ggplot(df, ggplot2::aes(x = time, y = growth_metric, color = cluster)) +
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
  
# Function - R^2 deviation
r_squared_deviation <- function(result, model_type) {
  if (is.null(result)) return(0)  # If model failed to cover, return R^2 = 0
# predicted values
if (model_type == "mixed") {
  predicted <- result$model_residual_data$ind_fit_value
} else if (model_type == "least-squares") {
  predicted <- result$model_residual_data$pop_fit_value
} else {
  stop("Invalid model_type. Use 'mixed' or 'least-squares'.")
}
actual <- result$model_residual_data$growth_metric
y_mean <- mean(actual)
ss_total <- sum((actual - y_mean)^2)
ss_residual <- sum((actual - predicted)^2)
return(1 - (ss_residual / ss_total))  # R² formula
}
  
fit_model <- function(df, function_type, model_type, time_unit) {
  tryCatch(
  {result <- growth_curve_model_fit(
          data_frame = df,
          function_type = function_type,
          model_type = model_type,
          time_unit = time_unit)
        
#calculating R^2
r_squared <- r_squared_deviation(result, model_type)
        return(list(result = result, r_squared = r_squared, best_model = function_type, model_type = model_type))
      },
      error = function(e) {
        showNotification(
          paste("Warning:", function_type, "model did not converge with", model_type, ". Consider alternative model types."),
          type = "warning",
          duration = 15)
        return(NULL)
      }
    )
  }
  
#reactive values to store model summary and R^2 value
current_model_summary <- reactiveVal(NULL)
r_squared_value <- reactiveVal(NA)
best_fit_model <- reactiveVal(NULL)
manual_selection <- reactiveVal(FALSE)

#calculate the best model based on selected model type
calculate_best_model <- function(model_type) {
  req(data())  
  df <- data()
  models <- c("exponential", "linear", "logistic", "gompertz")
  results <- list()
    
for (model in models) {
  result <- fit_model(df, model, model_type, input$time_unit)
  if (!is.null(result)) {results <- append(results, list(result))
  }
}
    
  #model with the highest R^2
  best_model <- results[[which.max(sapply(results, function(x) x$r_squared))]]
  r_squared_value(round(best_model$r_squared, 4))  # round the best fit to 4 decimals
  best_fit_model(best_model$best_model)
  return(best_model)
}
  
#calculate R^2 values for all models based on the selected regression type
calculate_r_squared <- function(model_type) {
    req(data())  
    df <- data()
    models <- c("exponential", "linear", "logistic", "gompertz")
    results <- data.frame(Model = character(), Type = character(), R2 = numeric(), stringsAsFactors = FALSE)
    
    withProgress(message = "Calculating R² values", value = 0, {
      for (i in seq_along(models)) {
        model <- models[i]
        result <- fit_model(df, model, model_type, input$time_unit)
        if (!is.null(result)) {
          r2_value <- format(result$r_squared, digits = 4, nsmall = 4)
          results <- rbind(results, data.frame(Model = model, Type = model_type, R2 = r2_value))
        }
        incProgress(1 / length(models))
      }
    })
    return(results)
}



#rendering the R^2values for each models
  output$model_performance_table <- renderTable({
    calculate_r_squared(input$model_type)
  }, rownames = FALSE)
  
# R^2 value for the best-fit model
  output$r_squared_value <- renderText({
    req(r_squared_value(), best_fit_model())
    paste("R² Value for Best-Fit Model ", best_fit_model(), ":", r_squared_value()) #r_squared_value
  })
  
#automatic model-type selection (default = mixed)
  observeEvent(input$data_file, {
    best_model <- calculate_best_model("mixed") # change "least-square" to "mixed"
    current_model_summary(best_model$result)
    
#best model
    updateSelectInput(session, "model_function", selected = best_model$best_model)
    updateSelectInput(session, "model_type", selected = "mixed") #"least-squares"
    runjs("$('.best-fit-btn').css('background-color', '#4CAF50');")  
  })
  
  # Model-type will change if mixed is selected for best fit
  observeEvent(input$model_type, {
    req(input$model_type)
    if (!manual_selection()) {
      best_model <- calculate_best_model(input$model_type)
      current_model_summary(best_model$result)
      # UI fields will be updated with the best model and model type
      updateSelectInput(session, "model_function", selected = best_model$best_model)
      updateSelectInput(session, "model_type", selected = input$model_type)
      runjs("$('.best-fit-btn').css('background-color', '#4CAF50');")  
    }
  })
  
#recalculate when "Best Fit" pressed
  observeEvent(input$best_fit, {
      best_model <- calculate_best_model(input$model_type) 
      current_model_summary(best_model$result)
      
      manual_selection(FALSE)
      # UI update
      updateSelectInput(session, "model_function", selected = best_model$best_model)
      updateSelectInput(session, "model_type", selected = input$model_type)
      runjs("$('.best-fit-btn').css('background-color', '#4CAF50');")  
    })

  
  observeEvent(input$model_function, {
    manual_selection(TRUE)  # manually selection
    runjs("$('.best-fit-btn').css('background-color', '');")
  })
  
#run analysis based on user selection
  observeEvent(input$run, {
    manual_selection(TRUE) 
    tryCatch(
      {
        withProgress(message = "Running analysis", value = 0, {
          steps <- 5
          for (i in seq_len(steps)) {
            Sys.sleep(0.5)
            incProgress(1 / steps)
          }
          df <- data()
          manual_result <- fit_model(df, input$model_function, input$model_type, input$time_unit)$result
          current_model_summary(manual_result)
          # Update R² value and best-fit model
          r_squared_value(round(manual_result$r_squared, 4))
          best_fit_model(input$model_function)
        })
      },
      error = function(e) {
        return(NULL)
        ##showNotification(
         ## paste("Error:", conditionMessage(e)),
          ##type = "error",
          ##duration = 20)
        }
    )
  })
  
#render the regression summary table
  output$summary_table <- renderUI({
    model_summary <- current_model_summary()
    req(model_summary)
    flextable_obj <- growth_model_summary_table(
      growth_model_summary_list = model_summary
    )
    flextable_obj <- flextable::fontsize(flextable_obj, size = input$body_font_size, part = "body")
    flextable_obj <- flextable::fontsize(flextable_obj, size = input$header_font_size, part = "header")
    html_table <- flextable::htmltools_value(flextable_obj, ft.align = "center")
    htmltools::HTML(as.character(html_table))
  })
  
#render the growth plots
  output$dynamic_plot <- renderUI({
    plotOutput("selected_plot", width = "600px", height = "400px")
  })
  
  output$selected_plot <- renderPlot({
    model_summary <- current_model_summary()
    req(model_summary)
    
    cluster <- if (input$cluster == "cluster") {
      input$cluster
    } else {
      input$cluster
    }
    
    growth_metric <- input$growth_metric
    growth_metric_units <- input$growth_metric_units
    
    switch(input$plot_type,
           "Growth vs Time \nwith Model Predictions" = {
             growth_vs_time_plot(
               growth_model_summary_list = model_summary,
               plot_type = 2,
               growth_metric = paste0(growth_metric, " (", growth_metric_units, ")"),
               time_name = paste0("Time (", input$time_unit, ")"),
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
               growth_model_summary_list = model_summary,
               plot_type = 3,
               growth_metric = paste0(growth_metric, " (", growth_metric_units, ")"),
               time_name = paste0("Time (", input$time_unit, ")"),
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
               growth_model_summary_list = model_summary,
               plot_type = 4,
               growth_metric = paste0(growth_metric, " (", growth_metric_units, ")"),
               time_name = paste0("Time (", input$time_unit, ")"),
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
  
#update residual_type choices based on model_type
  observeEvent(input$model_type, {
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
  
#reactive for plot data
  plot5_data <- reactive({
    model_summary <- current_model_summary()
    req(model_summary, input$residual_type)  
    growth_model_residual_plots(
      growth_model_summary_list = model_summary,
      residual_type = input$residual_type,
      weighted = as.logical(input$weighted)
    )
  })
  
#diagnostics using the reactive data
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
  
  # Download report
  output$download_report <- downloadHandler(
    filename = function() {
      report_title <- input$report_title
      paste0(ifelse(report_title != "", paste0(report_title, "_"), "GrowthCurve_Report_"), Sys.Date(), ".pdf", ".zip")
    },
    
    content = function(file) {
      withProgress(message = "Downloading Result", value = 0, {
        steps <- 10
        incProgress(1 / steps)
        
        rmd_path <- "growthcurve.Rmd"
        pdf_filename <- "Growth_Curve_Report.pdf"
        png_dir <- "png"
        dir.create(png_dir, showWarnings = FALSE)
        svg_dir <- "svg"
        dir.create(svg_dir, showWarnings = FALSE)
        
        capitalize_and_replace_underscores <- function(text) {
          text <- gsub("_", " ", text)
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
            growth_metric = gsub("_", "\\\\_", input$growth_metric),
            time = input$time_unit,
            cluster = input$cluster,
            model_type = input$model_type,
            function_type = input$model_function,
            fixed_rate = input$fixed_rate,
            x_limits = c(input$x_min, input$x_max),  
            n_x_axis_breaks = input$n_x_axis_breaks,  
            y_limits = c(input$y_min, input$y_max),  
            n_y_axis_breaks = input$n_y_axis_breaks
          ),
          envir = new.env(parent = globalenv())
        )
        
        png_filename1 <- file.path(png_dir, "grow_vs_time.png")
        svg_filename1 <- file.path(svg_dir, "grow_vs_time.svg")
        model_summary <- current_model_summary()
        req(model_summary)
        
        plot_01 <- growth_vs_time_plot(
          growth_model_summary_list = model_summary,
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
          growth_model_summary_list = model_summary,
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
        
        # Generate population-level residual plots
        png_filename3 <- file.path(png_dir, "res_pop_raw.png")
        svg_filename3 <- file.path(svg_dir, "res_pop_raw.svg")
        plot_03 <- growth_model_residual_plots(
          growth_model_summary_list = model_summary,
          residual_type = "population",
          weighted = FALSE
        )
        ggsave(png_filename3, plot = plot_03, dpi = 300, width = 7.5, height = 5.5, units = "in")
        svg(svg_filename3,width = 7.5, height = 5.5)
        print(plot_03)
        dev.off()
        
        png_filename4 <- file.path(png_dir, "res_pop_sd.png")
        svg_filename4 <- file.path(svg_dir, "res_pop_sd.svg")
        plot_04 <- growth_model_residual_plots(
          growth_model_summary_list = model_summary,
          residual_type = "population",
          weighted = TRUE
        )
        ggsave(png_filename4, plot = plot_04, width = 7.5, height = 5.5, units = "in")
        svg(svg_filename4,width = 7.5, height = 5.5)
        print(plot_04)
        dev.off()
        
        #Only generate cluster-level plots if model_type is "mixed"
        if (input$model_type == "mixed") {
          png_filename5 <- file.path(png_dir, "res_clus_raw.png")
          svg_filename5 <- file.path(svg_dir, "res_clus_raw.svg")
          plot_05 <- growth_model_residual_plots(
            growth_model_summary_list = model_summary,
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
            growth_model_summary_list = model_summary,
            residual_type = "cluster",
            weighted = TRUE
          )
          ggsave(png_filename6, plot = plot_06, width = 7.5, height = 5.5, units = "in")
          svg(svg_filename6,width = 7.5, height = 5.5)
          print(plot_06)
          dev.off()
        }
        
        excel_filename <- "growth_results.xlsx"
        write_xlsx(current_model_summary(), excel_filename)
        
        plot_title7 <- if (input$model_type == "mixed") {
          "Growth vs Time by Cluster\nwith Cluster Level Predictions"
        } else {
          "Growth vs Time by Cluster\nwith Population Level Predictions"
        }
        png_filename7 <- file.path(png_dir, "grow_vs_time_by_clus.png")
        svg_filename7 <- file.path(svg_dir, "grow_vs_time_by_clus.svg")     
        
        plot_07 <- growth_vs_time_plot(
          growth_model_summary_list = model_summary,
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
          growth_model_summary_list = model_summary,
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
        
        png_files <- list.files(png_dir, pattern = "\\.png$", full.names = TRUE)
        svg_files <- list.files(svg_dir, pattern = "\\.svg$", full.names = TRUE)
        files_to_zip <- c(pdf_filename, excel_filename, png_files, svg_files)
        
        zip(file, files_to_zip)
      })
    })
})

