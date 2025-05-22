library(shiny)   # Core framework, needed throughout
library(ggplot2)  # Used extensively for plotting
library(plotly)   # Used for interactive plots

# Use namespace calls for libraries with limited function usage
# This avoids attaching entire namespaces when only a few functions are needed

datadir <- "data"

## LOAD THE data if not done yet
if (!exists("dt.gtex.melt") || 
    !exists("dt.pcawg.melt") || 
    !exists("rna.ra") || 
    !exists("dt_cn_exp") || 
    !exists("memgtex")) {
  
  message("===== Data loading starting =====")
  
  # Custom message function that also logs to a global variable for the UI to display
  log_message <- function(msg) {
    message(msg)
    if (exists("consoleLogBuffer")) {
      consoleLogBuffer <<- c(consoleLogBuffer, msg)
    } else {
      consoleLogBuffer <<- c(msg)
    }
  }
  
  log_message("Data loading starting")
  
  # Load GTEx data
  log_message("Loading GTEx data")
  dt.gtex.melt <- readRDS(file.path(datadir, "gtex_melt.rds"))
  data.table::setkey(dt.gtex.melt, Description)
  
  # Load PCAWG data
  log_message("Loading PCAWG data")
  dt.pcawg.melt <- readRDS(file.path(datadir, "pcawg_data.rds"))
  data.table::setkey(dt.pcawg.melt, GeneID)
  
  # Load RNA data for expression values
  log_message("Loading RNA data")
  rna.ra <- readRDS(file.path(datadir, "rna_data.rds"))
  data.table::setkey(rna.ra, GeneID)
  
  # Load membrane protein database
  log_message("Loading membrane protein database")
  dt.mem <- readRDS(file.path(datadir, "dt_mem.rds"))
  
  ## load copy-number v expression
  log_message("Loading copy number vs expression data")
  dt_cn_exp <- readRDS(file.path(datadir, "dt_cn_expression.rds"))
  
  ## load membrane data combined with SS data 
  log_message("Loading membrane GTEx data")
  memgtex <- readRDS(file.path(datadir, "membrane_gtex.rds"))
  
  log_message("===== Data loading complete =====")
  
  # Return the buffer for initialization
  consoleLogBuffer
}

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #consoleOutput {
        font-family: monospace;
        font-size: 12px;
        background-color: transparent;
        border: none;
        padding: 0;
        overflow-y: auto;
        height: 150px;
        width: 100%;
        white-space: pre-wrap;
      }
    "))
  ),
  titlePanel(""),
  
  
  sidebarLayout(
    # Shared Sidebar for Gene Selection
    sidebarPanel(
      textInput("gnn", "Enter Gene Name:", value = "GAPDH"),
      actionButton("update", "Update Plot"),
      radioButtons("dataset_choice", "Select Expr Data:", 
                   choices = c("Boston Gene 2024" = "bg24", "Boston Gene 2022" = "bg22"),
                   selected = "bg24"),
      
      # Console output area - persistent
      hr(),
      h4("Console Output"),
      tags$div(
        style = "border: 1px solid #ccc; background-color: #f8f9fa; padding: 10px; height: 150px; overflow-y: auto;",
        verbatimTextOutput("consoleOutput", placeholder = TRUE)
      )
    ),
    
    # Main Panel with Tabs
    mainPanel(
      tabsetPanel(
        tabPanel(
          "GTEx",
          plotOutput("gtexPlot", height = "600px")
        ),
        tabPanel(
          "PCAWG",
          plotOutput("pcawgPlot", height = "600px")
        ),
        tabPanel(
          "Copy-Number / Expression",
          plotlyOutput("cnePlot", height = "600px")
        ),
        tabPanel(
          "Membrane GTEx",
          plotlyOutput("membraneGTExPlot", height = "600px")
        ),
        tabPanel(
          "BG2022 vs BG2024",
          plotlyOutput("bgComparisonPlot", height = "600px")
        )
        #tabPanel("WGS Mutations", DTOutput("mut_table"))
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Create a reactive value to store console log messages
  # Using a reactiveVal instead of reactiveValues for simplicity
  log_messages <- reactiveVal(character(0))
  
  # Initialize with a startup message
  log_messages(c(paste(format(Sys.time(), "[%H:%M:%S]"), "App starting...")))
  
  # Add initial startup messages - using observeEvent with a once trigger
  startup_done <- reactiveVal(FALSE)
  
  observeEvent(startup_done(), {
    # Only execute the first time
    if (!startup_done()) {
      if (exists("consoleLogBuffer") && length(consoleLogBuffer) > 0) {
        current_log <- log_messages()
        log_messages(c(current_log, 
                      paste("Found", length(consoleLogBuffer), "startup messages"), 
                      consoleLogBuffer))
      }
      startup_done(TRUE)
    }
  }, priority = 1000)
  
  # Trigger the startup event immediately
  startup_done(FALSE)
  
  # Function to add a log message with timestamp
  # This function will NOT trigger reactivity
  addLog <- function(message) {
    # Create timestamped message
    timestamp <- format(Sys.time(), "[%H:%M:%S]")
    new_message <- paste(timestamp, message)
    
    # Get current messages 
    current_log <- log_messages()
    
    # Keep only the last 20 messages to avoid too much clutter
    if (length(current_log) > 20) {
      current_log <- current_log[-1]
    }
    
    # Update messages
    log_messages(c(current_log, new_message))
    
    # Print to R console too
    message(new_message)
  }
  
  # Render the console output
  output$consoleOutput <- renderText({
    paste(log_messages(), collapse = "\n")
  })
  
  # Key reactive expression that only updates when the button is clicked
  # This prevents infinite reactivity chains
  gene_data <- eventReactive(input$update, {
    gene_name <- input$gnn
    dataset_choice <- input$dataset_choice
    
    # Log only once when button is clicked
    addLog(paste("Update button clicked - processing gene:", gene_name))
    
    # Return list of computed values to prevent recomputation
    result <- list(
      gene_name = gene_name,
      dataset_choice = dataset_choice,
      gtex_data = NULL,
      pcawg_data = NULL,
      expr_level = NULL
    )
    
    # Try to get GTEx data
    tryCatch({
      if (gene_name %in% dt.gtex.melt$Description) {
        gtex_data <- dt.gtex.melt[gene_name]
        addLog(paste0("GTEx data found for gene '", gene_name, "'. Row count:", nrow(gtex_data)))
        result$gtex_data <- gtex_data
      } else {
        addLog(paste0("Gene '", gene_name, "' not found in GTEx dataset."))
      }
    }, error = function(e) {
      addLog(paste("ERROR in GTEx data retrieval:", e$message))
    })
    
    # Try to get PCAWG data
    tryCatch({
      if (gene_name %in% dt.pcawg.melt$GeneID) {
        pcawg_data <- dt.pcawg.melt[gene_name]
        addLog(paste0("PCAWG data found for gene '", gene_name, "'. Row count:", nrow(pcawg_data)))
        result$pcawg_data <- pcawg_data
      } else {
        addLog(paste0("Gene '", gene_name, "' not found in PCAWG dataset."))
      }
    }, error = function(e) {
      addLog(paste("ERROR in PCAWG data retrieval:", e$message))
    })
    
    # Try to get expression level
    tryCatch({
      if (gene_name %in% rna.ra$GeneID) {
        dataset_name <- if(dataset_choice == "bg24") "Boston Gene 2024" else "Boston Gene 2022"
        addLog(paste0("Expression data found in ", dataset_name, " for gene '", gene_name, "'."))
        
        if (dataset_choice == "bg24") {
          result$expr_level <- 2^rna.ra[gene_name]$bostongene_2024.09
        } else {
          result$expr_level <- 2^rna.ra[gene_name]$bostongene_2022.11
        }
      } else {
        addLog(paste0("Gene '", gene_name, "' not found in expression data."))
      }
    }, error = function(e) {
      addLog(paste("ERROR in expression data retrieval:", e$message))
    })
    
    # Genomic data loading removed
    
    # For CN vs. Expression plot
    tryCatch({
      # Check if gene exists in copy number data
      dt_cn_exp[, is_target := FALSE] # Reset flag
      dt_cn_exp[, is_target := GeneID == gene_name]
      
      if (sum(dt_cn_exp$is_target) > 0) {
        addLog(paste0("Copy number data found for gene '", gene_name, "'."))
        result$cn_exp_data_available <- TRUE
      } else {
        addLog(paste0("Gene '", gene_name, "' not found in copy number data."))
        result$cn_exp_data_available <- FALSE
      }
    }, error = function(e) {
      addLog(paste("ERROR in copy number data processing:", e$message))
    })
    
    # For Membrane GTEx plot
    tryCatch({
      highlighted_points <- memgtex[memgtex$GeneID == gene_name, ]
      
      if (nrow(highlighted_points) > 0) {
        addLog(paste("Membrane data found for gene:", gene_name))
        result$membrane_data_available <- TRUE
      } else {
        addLog(paste("Gene", gene_name, "not found in membrane data."))
        result$membrane_data_available <- FALSE
      }
    }, error = function(e) {
      addLog(paste("ERROR in membrane data processing:", e$message))
    })
    
    return(result)
  })
  
  # GTEx Plot
  output$gtexPlot <- renderPlot({
    # Get the data from our central reactive expression
    data <- gene_data()
    
    # Show placeholder if button not clicked yet
    if (is.null(data)) {
      return(ggplot() + 
               annotate("text", x = 0, y = 0, label = "Enter a gene name and click 'Update Plot'") + 
               theme_void())
    }
    
    # Show error message if no GTEx data
    if (is.null(data$gtex_data)) {
      return(ggplot() + 
               annotate("text", x = 0, y = 0, 
                        label = paste("No GTEx data found for gene:", data$gene_name)) + 
               theme_void())
    }
    
    # Plot the data
    ggplot(data$gtex_data, aes(x = value)) + 
      geom_histogram() + 
      theme_bw() + 
      facet_wrap(~SMTS, scales = "free") + 
      xlab("Expression (TPM)") + 
      ylab("GTEx sample count") +
      {
        # Add expression line if available
        if (!is.null(data$expr_level)) {
          geom_vline(xintercept = data$expr_level, color = "#756bb1", 
                    linetype = "dashed", linewidth = 1.5)
        }
      }
  })
  
  # PCAWG Plot
  output$pcawgPlot <- renderPlot({
    # Get the data from our central reactive expression
    data <- gene_data()
    
    # Show placeholder if button not clicked yet
    if (is.null(data)) {
      return(ggplot() + 
               annotate("text", x = 0, y = 0, label = "Enter a gene name and click 'Update Plot'") + 
               theme_void())
    }
    
    # Show error message if no PCAWG data
    if (is.null(data$pcawg_data)) {
      return(ggplot() + 
               annotate("text", x = 0, y = 0, 
                        label = paste("No PCAWG data found for gene:", data$gene_name)) + 
               theme_void())
    }
    
    # Plot the data
    ggplot(data$pcawg_data, aes(x = tpm)) + 
      geom_histogram() + 
      theme_bw() + 
      facet_wrap(~tumor_type, scales = "free") + 
      xlab("Expression (TPM)") + 
      ylab("PCAWG sample count") +
      {
        # Add expression line if available
        if (!is.null(data$expr_level)) {
          geom_vline(xintercept = data$expr_level, color = "#756bb1", 
                    linetype = "dashed", linewidth = 1.5)
        }
      }
  })
  
  # Copy Number Plot removed
  
  # Membrane GTEx Plot
  output$membraneGTExPlot <- renderPlotly({
    # Get the data from our central reactive expression
    data <- gene_data()
    
    # Show placeholder if button not clicked yet
    if (is.null(data)) {
      return(plot_ly() %>% 
              add_annotations(
                text = "Enter a gene name and click 'Update Plot'",
                x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE
              ))
    }
    
    # Plot the data
    tryCatch({
      # Highlight the gene if it exists in the dataset
      highlighted_points <- memgtex[memgtex$GeneID == data$gene_name, ]
      
      # Plot the whole dataset with the target gene highlighted
      p <- plot_ly() %>%
        add_trace(data = memgtex, 
                  x = ~.data[["bostongene_2024.09"]], 
                  y = ~.data[["max_log2"]], 
                  text = ~.data[["GeneID"]], 
                  type = 'scatter', 
                  mode = 'markers',
                  name = "All Genes",
                  marker = list(size = 5, color = "rgba(128, 128, 128, 0.5)"))
      
      # Add highlighted point if it exists
      if (nrow(highlighted_points) > 0) {
        p <- p %>% add_trace(data = highlighted_points,
                            x = ~.data[["bostongene_2024.09"]], 
                            y = ~.data[["max_log2"]], 
                            text = ~.data[["GeneID"]], 
                            type = 'scatter', 
                            mode = 'markers',
                            name = "Selected Gene",
                            marker = list(size = 12, color = "#756bb1"))
      }
      
      p %>% layout(xaxis = list(title = "Tumor 2024"),
                  yaxis = list(title = "GTEx Max"),
                  title = paste("Membrane GTEx Plot", 
                                ifelse(nrow(highlighted_points) == 0, 
                                      paste("(Gene", data$gene_name, "not found)"), "")))
    }, error = function(e) {
      addLog(paste("ERROR in Membrane GTEx plot:", e$message))
      return(plot_ly() %>% 
              add_annotations(
                text = paste("Error:", e$message),
                x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE
              ))
    })
  })
  
  # Copy Number vs. Expression Plot
  output$cnePlot <- renderPlotly({
    # Get the data from our central reactive expression
    data <- gene_data()
    
    # Show placeholder if button not clicked yet
    if (is.null(data)) {
      return(plot_ly() %>% 
              add_annotations(
                text = "Enter a gene name and click 'Update Plot'",
                x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE
              ))
    }
    
    # Plot the data
    tryCatch({
      # Update the target flag in the data (in case the gene has changed)
      dt_cn_exp[, is_target := GeneID == data$gene_name]
      
      # Check if gene exists in the CN vs Expression data
      if (sum(dt_cn_exp$is_target) == 0) {
        return(plot_ly() %>% 
                add_annotations(
                  text = paste("Gene", data$gene_name, "not found in copy number data"),
                  x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE
                ))
      }
      
      plot_ly() %>%
        add_trace(data = dt_cn_exp[dt_cn_exp$membrane != "Membrane", ], 
                  x = ~jittercn, 
                  y = ~bg24, 
                  text = ~GeneID, 
                  type = 'scatter', 
                  mode = 'markers',
                  name = "Non-Membrane",
                  marker = list(size = 4, opacity = 0.5, color="black")) %>%
        add_trace(data = dt_cn_exp[dt_cn_exp$membrane == "Membrane", ], 
                  x = ~jittercn, 
                  y = ~bg24, 
                  text = ~GeneID, 
                  type = 'scatter', 
                  mode = 'markers',
                  name = "Membrane",
                  marker = list(size = 4, opacity = 1, color = "red")) %>%
        add_trace(data = dt_cn_exp[is_target==TRUE, ], 
                  x = ~jittercn, 
                  y = ~bg24, 
                  text = ~GeneID, 
                  type = 'scatter', 
                  mode = 'markers',
                  name = "Selected Gene",
                  marker = list(size = 16, opacity = 1, color = "#addd8e")) %>%
        layout(title = paste("Copy Number vs. Expression -", data$gene_name),
               xaxis = list(title = "Copy Number"),
               yaxis = list(title = "Boston Gene 2024 log2 expression"))
    }, error = function(e) {
      addLog(paste("ERROR in CN vs Expression plot:", e$message))
      return(plot_ly() %>% 
              add_annotations(
                text = paste("Error:", e$message),
                x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE
              ))
    })
  })
  
  # BG2022 vs BG2024 Comparison Plot
  output$bgComparisonPlot <- renderPlotly({
    # Get the data from our central reactive expression
    data <- gene_data()
    
    # Show placeholder if button not clicked yet
    if (is.null(data)) {
      return(plot_ly() %>% 
              add_annotations(
                text = "Enter a gene name and click 'Update Plot'",
                x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE
              ))
    }
    
    # Create a simplified version of the plot to avoid the subscript error
    tryCatch({
      # Prepare the data inside isolate to prevent reactivity issues
      isolate({
        addLog(paste0("Preparing BG2022 vs BG2024 comparison plot for gene '", data$gene_name, "'."))
      })
      
      # Convert rna.ra data to a data frame for easier handling
      # Using as.data.frame to ensure compatibility with plotly
      bg_df <- data.frame(
        GeneID = rna.ra$GeneID,
        bg22 = 2^as.numeric(rna.ra$bostongene_2022.11),
        bg24 = 2^as.numeric(rna.ra$bostongene_2024.09),
        stringsAsFactors = FALSE
      )
      
      # Add a column to identify the target gene
      bg_df$is_target <- bg_df$GeneID == data$gene_name
      
      # Check if gene exists in the data
      if (sum(bg_df$is_target) == 0) {
        isolate(addLog(paste0("Gene '", data$gene_name, "' not found in expression data.")))
        return(plot_ly() %>% 
                add_annotations(
                  text = paste("Gene", data$gene_name, "not found in expression data"),
                  x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE
                ))
      }
      
      # Split the data into background and highlighted points
      bg_points <- bg_df[!bg_df$is_target, ]
      highlight_points <- bg_df[bg_df$is_target, ]
      
      # Create the plot without range settings to avoid "subscript out of bounds" error
      plot_ly() %>%
        # Add the background points
        add_trace(
          data = bg_points,
          x = ~bg22,
          y = ~bg24,
          text = ~GeneID,
          type = 'scatter',
          mode = 'markers',
          name = "All Genes",
          marker = list(size = 4, opacity = 0.5, color = "rgba(128, 128, 128, 0.5)")
        ) %>%
        # Add the highlighted point
        add_trace(
          data = highlight_points,
          x = ~bg22,
          y = ~bg24,
          text = ~GeneID,
          type = 'scatter',
          mode = 'markers',
          name = "Selected Gene",
          marker = list(size = 16, opacity = 1, color = "#addd8e")
        ) %>%
        # Add layout with title and axis labels
        layout(
          title = paste("Boston Gene 2022 vs 2024 Expression -", data$gene_name),
          xaxis = list(
            title = "Boston Gene 2022 Expression (TPM)",
            type = "log"
          ),
          yaxis = list(
            title = "Boston Gene 2024 Expression (TPM)",
            type = "log"
          ),
          # Add a diagonal reference line
          shapes = list(
            list(
              type = "line",
              x0 = 0,
              y0 = 0,
              x1 = 1,
              y1 = 1,
              line = list(color = "rgba(0, 0, 0, 0.5)", width = 1),
              xref = "paper",
              yref = "paper"
            )
          )
        )
    }, error = function(e) {
      # Log the error, but do it in an isolate to prevent reactivity chain
      isolate(addLog(paste("ERROR in BG comparison plot:", e$message)))
      
      return(plot_ly() %>% 
              add_annotations(
                text = paste("Error:", e$message),
                x = 0.5, y = 0.5, xref = "paper", yref = "paper", showarrow = FALSE
              ))
    })
  })
}

# Run the App
shinyApp(ui = ui, server = server)