########################################################################
####                                                        ############
#### Complete Shiny App for marineSABRES T5.3 Network Analyses    ####
####                                                        ############
########################################################################

# Load required libraries
required_packages <- c(
  "shiny", "shinydashboard", "shinyWidgets", "DT", "plotly",
  "visNetwork", "igraph", "data.table", "BoolNet", "randomForest", 
  "ggplot2", "reshape2", "shinycssloaders"
)

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Source utility functions
source('utils.R')

# Helper function for null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x

########################################################################
####                           UI                                  ####
########################################################################

ui <- dashboardPage(
  
  header = dashboardHeader(
    title = "marineSABRES Network Analysis", 
    titleWidth = 300
  ),
  
  sidebar = dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Data Upload", tabName = "upload", icon = icon("upload")),
      menuItem("Network Visualization", tabName = "network", icon = icon("project-diagram")),
      menuItem("Qualitative Analysis", tabName = "qualitative", icon = icon("sitemap")),
      menuItem("Quantitative Analysis", tabName = "quantitative", icon = icon("chart-line")),
      menuItem("Measures & Interventions", tabName = "measures", icon = icon("tools")),
      menuItem("Results & Export", tabName = "results", icon = icon("download"))
    )
  ),
  
  body = dashboardBody(
    
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f9f9f9;
        }
        .box {
          border-radius: 5px;
        }
        .loading-spinner {
          text-align: center;
          padding: 20px;
        }
      "))
    ),
    
    tabItems(
      
      # Data Upload Tab
      tabItem(
        tabName = "upload",
        fluidRow(
          box(
            title = "Data Upload & Preview", status = "primary", solidHeader = TRUE, width = 12,
            
            fluidRow(
              column(6,
                fileInput("file", "Choose CSV File",
                         accept = c(".csv", ".CSV"),
                         placeholder = "No file selected")
              ),
              column(6,
                br(),
                downloadButton("sampleData", "Download Sample Data", 
                             class = "btn-info")
              )
            ),
            
            conditionalPanel(
              condition = "output.fileUploaded",
              
              h4("Data Preview"),
              withSpinner(DT::dataTableOutput("dataPreview")),
              
              br(),
              h4("Data Summary"),
              fluidRow(
                column(4, valueBoxOutput("numConnections", width = NULL)),
                column(4, valueBoxOutput("numNodes", width = NULL)),
                column(4, valueBoxOutput("strengthTypes", width = NULL))
              ),
              
              br(),
              h4("Column Mapping"),
              p("Verify that your data columns match the expected format:"),
              fluidRow(
                column(4,
                  selectInput("fromCol", "From Column:", choices = NULL)
                ),
                column(4,
                  selectInput("toCol", "To Column:", choices = NULL)
                ),
                column(4,
                  selectInput("strengthCol", "Strength Column:", choices = NULL)
                )
              ),
              
              br(),
              actionButton("processData", "Process Data", 
                         class = "btn-success btn-lg")
            )
          )
        )
      ),
      
      # Network Visualization Tab
      tabItem(
        tabName = "network",
        fluidRow(
          box(
            title = "Interactive Network Visualization", 
            status = "primary", solidHeader = TRUE, width = 8,
            
            conditionalPanel(
              condition = "output.dataProcessed",
              withSpinner(visNetworkOutput("networkVis", height = "600px"))
            ),
            conditionalPanel(
              condition = "!output.dataProcessed",
              div(class = "loading-spinner",
                  h4("Please upload and process data first", style = "color: #999;"))
            )
          ),
          
          box(
            title = "Network Controls", status = "info", solidHeader = TRUE, width = 4,
            
            h4("Visualization Options"),
            checkboxInput("showLabels", "Show Node Labels", value = TRUE),
            checkboxInput("showEdgeLabels", "Show Edge Weights", value = FALSE),
            
            sliderInput("nodeSize", "Node Size:", 
                       min = 10, max = 50, value = 25, step = 5),
            
            sliderInput("edgeWidth", "Edge Width Multiplier:", 
                       min = 1, max = 10, value = 5, step = 1),
            
            h4("Layout Options"),
            selectInput("layoutType", "Layout Algorithm:",
                       choices = list(
                         "Force-directed" = "layout_with_fr",
                         "Hierarchical" = "layout_with_sugiyama", 
                         "Circular" = "layout_in_circle",
                         "Grid" = "layout_on_grid"
                       ), selected = "layout_with_fr"),
            
            br(),
            h4("Network Statistics"),
            conditionalPanel(
              condition = "output.dataProcessed",
              tableOutput("networkStats")
            )
          )
        )
      ),
      
      # Qualitative Analysis Tab
      tabItem(
        tabName = "qualitative",
        fluidRow(
          box(
            title = "Boolean Network Analysis", 
            status = "primary", solidHeader = TRUE, width = 6,
            
            h4("Analysis Parameters"),
            p("Boolean network analysis examines system attractors and basin sizes."),
            
            conditionalPanel(
              condition = "output.dataProcessed",
              actionButton("runQualitative", "Run Qualitative Analysis", 
                         class = "btn-primary btn-lg"),
              br(), br()
            ),
            
            conditionalPanel(
              condition = "output.qualitativeComplete",
              h4("Results Summary"),
              tableOutput("qualitativeResults")
            )
          ),
          
          box(
            title = "Laplacian Eigenvalues", 
            status = "info", solidHeader = TRUE, width = 6,
            
            conditionalPanel(
              condition = "output.qualitativeComplete",
              withSpinner(plotlyOutput("laplacianPlot", height = "400px"))
            ),
            conditionalPanel(
              condition = "!output.qualitativeComplete",
              div(class = "loading-spinner",
                  h4("Run qualitative analysis to see results", style = "color: #999;"))
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Attractors Visualization", 
            status = "success", solidHeader = TRUE, width = 12,
            
            conditionalPanel(
              condition = "output.qualitativeComplete",
              h4("Boolean Attractors"),
              p("Each row represents a stable state (attractor) of the system."),
              withSpinner(DT::dataTableOutput("attractorsTable"))
            )
          )
        )
      ),
      
      # Quantitative Analysis Tab
      tabItem(
        tabName = "quantitative",
        fluidRow(
          box(
            title = "Simulation Parameters", 
            status = "primary", solidHeader = TRUE, width = 4,
            
            h4("Simulation Settings"),
            numericInput("iterations", "Number of Iterations:", 
                        value = 500, min = 100, max = 2000, step = 100),
            
            numericInput("greediness", "Greediness (State-shift samples):", 
                        value = 100, min = 50, max = 1000, step = 50),
            
            selectInput("simulationType", "Simulation Type:",
                       choices = list("Uniform" = "uniform", "Ordinal" = "ordinal"),
                       selected = "uniform"),
            
            h4("Target Variables"),
            conditionalPanel(
              condition = "output.dataProcessed",
              checkboxGroupInput("targetVars", "Select target variables:",
                               choices = NULL, selected = NULL)
            ),
            
            br(),
            conditionalPanel(
              condition = "output.dataProcessed",
              actionButton("runQuantitative", "Run Quantitative Analysis", 
                         class = "btn-success btn-lg")
            )
          ),
          
          box(
            title = "System Dynamics", 
            status = "info", solidHeader = TRUE, width = 8,
            
            conditionalPanel(
              condition = "output.quantitativeComplete",
              tabsetPanel(
                tabPanel("Time Series", 
                        withSpinner(plotlyOutput("timeSeriesPlot", height = "400px"))),
                tabPanel("Phase Space (PCA)", 
                        withSpinner(plotlyOutput("pcaPlot", height = "400px"))),
                tabPanel("Participation Ratio", 
                        withSpinner(plotlyOutput("participationPlot", height = "400px")))
              )
            ),
            conditionalPanel(
              condition = "!output.quantitativeComplete",
              div(class = "loading-spinner",
                  h4("Run quantitative analysis to see results", style = "color: #999;"))
            )
          )
        ),
        
        fluidRow(
          box(
            title = "State-shift Analysis", 
            status = "warning", solidHeader = TRUE, width = 12,
            
            conditionalPanel(
              condition = "output.quantitativeComplete",
              h4("Desirable Outcomes Analysis"),
              p("Shows which parameter combinations lead to desired system states."),
              withSpinner(DT::dataTableOutput("stateShiftResults"))
            )
          )
        )
      ),
      
      # Measures & Interventions Tab
      tabItem(
        tabName = "measures",
        fluidRow(
          box(
            title = "Intervention Design", 
            status = "primary", solidHeader = TRUE, width = 6,
            
            h4("Measure Configuration"),
            conditionalPanel(
              condition = "output.dataProcessed",
              
              textInput("measureName", "Intervention Name:", 
                       value = "New Intervention", placeholder = "e.g., Tourism Throttling"),
              
              selectInput("affectedNode", "Affected Node:",
                         choices = NULL, selected = NULL),
              
              checkboxGroupInput("indicatorNodes", "Indicator Nodes:",
                               choices = NULL, selected = NULL),
              
              h4("Effect Ranges"),
              sliderInput("effectRange", "Effect Range:", 
                         min = -1, max = 1, value = c(-1, 0), step = 0.1),
              
              br(),
              actionButton("addMeasure", "Add Intervention", 
                         class = "btn-warning btn-lg")
            )
          ),
          
          box(
            title = "Intervention Effects", 
            status = "info", solidHeader = TRUE, width = 6,
            
            conditionalPanel(
              condition = "output.measureAdded",
              h4("Network with Intervention"),
              withSpinner(visNetworkOutput("measureNetworkVis", height = "400px"))
            ),
            conditionalPanel(
              condition = "!output.measureAdded",
              div(class = "loading-spinner",
                  h4("Design an intervention to see effects", style = "color: #999;"))
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Intervention Analysis", 
            status = "success", solidHeader = TRUE, width = 12,
            
            conditionalPanel(
              condition = "output.measureAdded",
              
              fluidRow(
                column(6,
                  h4("Simulation Parameters"),
                  numericInput("measureIterations", "Iterations:", 
                              value = 500, min = 100, max = 1000),
                  actionButton("runMeasureAnalysis", "Run Intervention Analysis", 
                             class = "btn-success")
                ),
                column(6,
                  conditionalPanel(
                    condition = "output.measureAnalysisComplete",
                    h4("State Differences"),
                    tableOutput("measureEffects")
                  )
                )
              ),
              
              conditionalPanel(
                condition = "output.measureAnalysisComplete",
                br(),
                h4("Comparison Plots"),
                withSpinner(plotlyOutput("measureComparisonPlot", height = "400px"))
              )
            )
          )
        )
      ),
      
      # Results & Export Tab
      tabItem(
        tabName = "results",
        fluidRow(
          box(
            title = "Analysis Summary", 
            status = "primary", solidHeader = TRUE, width = 6,
            
            h4("Completed Analyses"),
            tableOutput("analysisSummary"),
            
            br(),
            h4("Export Options"),
            downloadButton("exportResults", "Download All Results", 
                         class = "btn-primary"),
            br(), br(),
            downloadButton("exportNetwork", "Export Network (GraphML)", 
                         class = "btn-info"),
            br(), br(),
            downloadButton("exportReport", "Generate HTML Report", 
                         class = "btn-success")
          ),
          
          box(
            title = "Session Information", 
            status = "info", solidHeader = TRUE, width = 6,
            
            h4("Analysis Log"),
            verbatimTextOutput("analysisLog")
          )
        )
      )
    )
  )
)

########################################################################
####                         SERVER                               ####
########################################################################

server <- function(input, output, session) {
  
  # Reactive values to store data and results
  values <- reactiveValues(
    original_data = NULL,
    ses_matrix = NULL,
    qualitative_results = NULL,
    quantitative_results = NULL,
    measure_matrix = NULL,
    measure_results = NULL,
    analysis_log = c(),
    files_created = c()
  )
  
  # Helper function to log messages
  log_message <- function(message) {
    timestamp <- format(Sys.time(), "%H:%M:%S")
    values$analysis_log <- c(values$analysis_log, paste(timestamp, "-", message))
  }
  
  # File upload handling
  output$fileUploaded <- reactive({
    return(!is.null(input$file))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden = FALSE)
  
  # Data processing flag
  output$dataProcessed <- reactive({
    return(!is.null(values$ses_matrix))
  })
  outputOptions(output, 'dataProcessed', suspendWhenHidden = FALSE)
  
  # Analysis completion flags
  output$qualitativeComplete <- reactive({
    return(!is.null(values$qualitative_results))
  })
  outputOptions(output, 'qualitativeComplete', suspendWhenHidden = FALSE)
  
  output$quantitativeComplete <- reactive({
    return(!is.null(values$quantitative_results))
  })
  outputOptions(output, 'quantitativeComplete', suspendWhenHidden = FALSE)
  
  output$measureAdded <- reactive({
    return(!is.null(values$measure_matrix))
  })
  outputOptions(output, 'measureAdded', suspendWhenHidden = FALSE)
  
  output$measureAnalysisComplete <- reactive({
    return(!is.null(values$measure_results))
  })
  outputOptions(output, 'measureAnalysisComplete', suspendWhenHidden = FALSE)
  
  # Sample data download
  output$sampleData <- downloadHandler(
    filename = function() {
      "sample_network_data.csv"
    },
    content = function(file) {
      sample_data <- data.frame(
        from = c("Tourism", "Economy", "Environment", "Tourism", "Economy"),
        to = c("Economy", "Environment", "Tourism", "Environment", "Tourism"),
        strength = c("Strong Positive", "Medium Negative", "Strong Negative", "Medium Positive", "Strong Positive")
      )
      write.csv(sample_data, file, row.names = FALSE)
    }
  )
  
  # Data preview
  observeEvent(input$file, {
    req(input$file)
    
    tryCatch({
      values$original_data <- fread(input$file$datapath)
      log_message(paste("Data loaded:", nrow(values$original_data), "rows"))
      
      # Update column choices
      col_names <- names(values$original_data)
      updateSelectInput(session, "fromCol", choices = col_names, 
                       selected = if("from" %in% col_names) "from" else col_names[1])
      updateSelectInput(session, "toCol", choices = col_names,
                       selected = if("to" %in% col_names) "to" else col_names[2])
      updateSelectInput(session, "strengthCol", choices = col_names,
                       selected = if("strength" %in% col_names) "strength" else col_names[3])
      
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
    })
  })
  
  output$dataPreview <- DT::renderDataTable({
    req(values$original_data)
    DT::datatable(values$original_data, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Data summary boxes
  output$numConnections <- renderValueBox({
    req(values$original_data)
    valueBox(
      value = nrow(values$original_data),
      subtitle = "Connections",
      icon = icon("arrows-alt"),
      color = "blue"
    )
  })
  
  output$numNodes <- renderValueBox({
    req(values$original_data)
    from_col <- input$fromCol %||% names(values$original_data)[1]
    to_col <- input$toCol %||% names(values$original_data)[2]
    unique_nodes <- length(unique(c(values$original_data[[from_col]], values$original_data[[to_col]])))
    
    valueBox(
      value = unique_nodes,
      subtitle = "Unique Nodes",
      icon = icon("circle"),
      color = "green"
    )
  })
  
  output$strengthTypes <- renderValueBox({
    req(values$original_data)
    strength_col <- input$strengthCol %||% names(values$original_data)[3]
    unique_strengths <- length(unique(values$original_data[[strength_col]]))
    
    valueBox(
      value = unique_strengths,
      subtitle = "Strength Types",
      icon = icon("weight"),
      color = "yellow"
    )
  })
  
  # Process data
  observeEvent(input$processData, {
    req(values$original_data, input$fromCol, input$toCol, input$strengthCol)
    
    withProgress(message = 'Processing data...', value = 0, {
      
      tryCatch({
        # Prepare data with correct column mapping
        processed_data <- values$original_data
        names(processed_data)[names(processed_data) == input$fromCol] <- "from"
        names(processed_data)[names(processed_data) == input$toCol] <- "to"
        names(processed_data)[names(processed_data) == input$strengthCol] <- "strength"
        
        incProgress(0.3, detail = "Creating network matrix...")
        
        # Create SES matrix
        values$ses_matrix <- data.load(processed_data, folder = tempdir(), 
                                     graph = FALSE)
        
        incProgress(0.7, detail = "Updating interface...")
        
        # Update target variable choices
        node_names <- rownames(values$ses_matrix)
        updateCheckboxGroupInput(session, "targetVars", 
                                choices = node_names, selected = node_names[1:min(3, length(node_names))])
        updateSelectInput(session, "affectedNode", choices = node_names)
        updateCheckboxGroupInput(session, "indicatorNodes", choices = node_names)
        
        log_message("Data processed successfully")
        showNotification("Data processed successfully!", type = "success")
        
      }, error = function(e) {
        showNotification(paste("Error processing data:", e$message), type = "error")
        log_message(paste("Error:", e$message))
      })
      
      incProgress(1)
    })
  })
  
  # Network visualization
  output$networkVis <- renderVisNetwork({
    req(values$ses_matrix)
    
    tryCatch({
      # Create igraph object
      g <- graph_from_adjacency_matrix(values$ses_matrix, mode = "directed", weighted = TRUE)
      
      # Convert to visNetwork format
      nodes <- data.frame(
        id = V(g)$name,
        label = if(input$showLabels) V(g)$name else "",
        size = input$nodeSize,
        color = list(background = "#97C2FC", border = "#2B7CE9")
      )
      
      edges <- data.frame(
        from = get.edgelist(g)[,1],
        to = get.edgelist(g)[,2],
        width = abs(E(g)$weight) * input$edgeWidth,
        color = ifelse(E(g)$weight > 0, "#2B7CE9", "#E74C3C"),
        arrows = "to"
      )
      
      if (input$showEdgeLabels) {
        edges$label <- round(E(g)$weight, 2)
      }
      
      visNetwork(nodes, edges) %>%
        visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
        visInteraction(navigationButtons = TRUE) %>%
        visLayout(randomSeed = 123)
        
    }, error = function(e) {
      showNotification(paste("Error creating network visualization:", e$message), type = "error")
      NULL
    })
  })
  
  # Network statistics
  output$networkStats <- renderTable({
    req(values$ses_matrix)
    
    g <- graph_from_adjacency_matrix(values$ses_matrix, mode = "directed", weighted = TRUE)
    
    stats <- data.frame(
      Metric = c("Nodes", "Edges", "Density", "Average Degree", "Positive Edges", "Negative Edges"),
      Value = c(
        vcount(g),
        ecount(g),
        round(edge_density(g), 3),
        round(mean(degree(g)), 2),
        sum(E(g)$weight > 0),
        sum(E(g)$weight < 0)
      )
    )
    
    stats
  }, bordered = TRUE, striped = TRUE)
  
  # Qualitative Analysis
  observeEvent(input$runQualitative, {
    req(values$ses_matrix)
    
    withProgress(message = 'Running qualitative analysis...', value = 0, {
      
      tryCatch({
        incProgress(0.2, detail = "Computing Laplacian...")
        
        # Run qualitative analysis
        values$qualitative_results <- qualitative.analyses(
          values$ses_matrix, 
          folder = tempdir(),
          filename.boolean.csv = "boolean_network",
          filename.boolean.graph = "boolean_graph"
        )
        
        incProgress(0.8, detail = "Finalizing results...")
        
        log_message("Qualitative analysis completed")
        showNotification("Qualitative analysis completed!", type = "success")
        
      }, error = function(e) {
        showNotification(paste("Error in qualitative analysis:", e$message), type = "error")
        log_message(paste("Qualitative analysis error:", e$message))
      })
      
      incProgress(1)
    })
  })
  
  # Qualitative results
  output$qualitativeResults <- renderTable({
    req(values$qualitative_results)
    
    results <- values$qualitative_results$boolean.results
    
    data.frame(
      Metric = c("Number of States", "Number of Attractors", "Largest Basin Size"),
      Value = c(
        results$states,
        results$N_attractors,
        if(length(results$basins) > 0) max(results$basins) else 0
      )
    )
  }, bordered = TRUE)
  
  # Laplacian plot
  output$laplacianPlot <- renderPlotly({
    req(values$qualitative_results)
    
    eigenvals <- values$qualitative_results$laplacian
    
    p <- ggplot(data.frame(
      Node = names(eigenvals),
      Eigenvalue = Re(eigenvals)
    ), aes(x = reorder(Node, Eigenvalue), y = Eigenvalue)) +
      geom_col(fill = "steelblue", alpha = 0.7) +
      coord_flip() +
      labs(x = "Nodes", y = "Eigenvalue", title = "Laplacian Eigenvalues") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Attractors table
  output$attractorsTable <- DT::renderDataTable({
    req(values$qualitative_results)
    
    attractors <- values$qualitative_results$boolean.results$attractors
    
    if (length(attractors) > 0) {
      attractor_df <- do.call(rbind, lapply(seq_along(attractors), function(i) {
        attr_data <- attractors[[i]]
        attr_data$Attractor <- paste("Attractor", i)
        return(attr_data)
      }))
      
      DT::datatable(attractor_df, options = list(pageLength = 10, scrollX = TRUE))
    } else {
      DT::datatable(data.frame(Message = "No attractors found"))
    }
  })
  
  # Quantitative Analysis
  observeEvent(input$runQuantitative, {
    req(values$ses_matrix, input$targetVars)
    
    withProgress(message = 'Running quantitative analysis...', value = 0, {
      
      tryCatch({
        incProgress(0.1, detail = "Setting up simulation...")
        
        # Run simulation
        sim_results <- SES.simulate(
          SES.mat = values$ses_matrix, 
          iter = input$iterations,
          save.fig = FALSE
        )
        
        incProgress(0.4, detail = "Computing participation ratio...")
        
        # Participation ratio
        pr_results <- participation_ratio(
          values$ses_matrix, 
          folder = tempdir(),
          filename = "participation_ratio",
          title = "Network Analysis"
        )
        
        incProgress(0.7, detail = "Running state-shift analysis...")
        
        # State-shift analysis
        state_shift_results <- state.shift(
          mat = values$ses_matrix,
          greed = input$greediness,
          iter = input$iterations,
          type = input$simulationType,
          folder = tempdir(),
          file = "state_shift_results.Rdata"
        )
        
        incProgress(0.9, detail = "Finalizing results...")
        
        values$quantitative_results <- list(
          simulation = sim_results,
          participation_ratio = pr_results,
          state_shift = state_shift_results,
          targets = input$targetVars
        )
        
        log_message("Quantitative analysis completed")
        showNotification("Quantitative analysis completed!", type = "success")
        
      }, error = function(e) {
        showNotification(paste("Error in quantitative analysis:", e$message), type = "error")
        log_message(paste("Quantitative analysis error:", e$message))
      })
      
      incProgress(1)
    })
  })
  
  # Time series plot
  output$timeSeriesPlot <- renderPlotly({
    req(values$quantitative_results)
    
    sim_data <- values$quantitative_results$simulation
    sim_df <- reshape2::melt(sim_data)
    colnames(sim_df) <- c("Node", "Time", "Value")
    
    # Subsample if too many points
    if (nrow(sim_df) > 5000) {
      sim_df <- sim_df[seq(1, nrow(sim_df), length.out = 2000), ]
    }
    
    p <- ggplot(sim_df, aes(x = Time, y = Value, color = Node)) +
      geom_line(alpha = 0.7) +
      scale_x_log10() +
      labs(x = "Time (log scale)", y = "System State", title = "System Dynamics") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # PCA plot
  output$pcaPlot <- renderPlotly({
    req(values$quantitative_results)
    
    sim_data <- values$quantitative_results$simulation
    pca <- prcomp(t(sim_data), scale = FALSE)
    
    pca_df <- data.frame(
      PC1 = pca$x[, 1],
      PC2 = pca$x[, 2],
      Time = 1:ncol(sim_data)
    )
    
    p <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Time)) +
      geom_point(alpha = 0.6) +
      geom_path(alpha = 0.4) +
      scale_color_viridis_c() +
      labs(title = "Phase Space (PCA)", x = "PC1", y = "PC2") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Participation ratio plot
  output$participationPlot <- renderPlotly({
    req(values$quantitative_results)
    
    pr_data <- values$quantitative_results$participation_ratio
    
    p <- ggplot(pr_data, aes(x = reorder(components, Re(PR)), y = Re(PR))) +
      geom_col(fill = "orange", alpha = 0.7) +
      coord_flip() +
      labs(x = "Components", y = "Participation Ratio", 
           title = "System Component Participation") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # State-shift results
  output$stateShiftResults <- DT::renderDataTable({
    req(values$quantitative_results)
    
    state_shift <- values$quantitative_results$state_shift
    targets <- values$quantitative_results$targets
    
    # Analyze state shifts
    tol <- 0.000001
    state_sim_bin <- state_shift$state.sim
    state_sim_bin[abs(state_sim_bin) < tol] <- 0
    state_sim_bin <- sign(state_sim_bin)
    
    target_indices <- match(targets, rownames(state_sim_bin))
    target_indices <- target_indices[!is.na(target_indices)]
    
    if (length(target_indices) > 0) {
      desirable_outcomes <- which(colSums(state_sim_bin[target_indices, , drop = FALSE]) == length(target_indices))
      success_rate <- length(desirable_outcomes) / ncol(state_sim_bin) * 100
      
      result_summary <- data.frame(
        Metric = c("Total Simulations", "Successful Outcomes", "Success Rate (%)", "Target Variables"),
        Value = c(
          ncol(state_sim_bin),
          length(desirable_outcomes),
          round(success_rate, 2),
          paste(targets, collapse = ", ")
        )
      )
      
      DT::datatable(result_summary, options = list(dom = 't'))
    } else {
      DT::datatable(data.frame(Message = "No valid targets found"))
    }
  })
  
  # Measures
  observeEvent(input$addMeasure, {
    req(values$ses_matrix, input$measureName, input$affectedNode, input$indicatorNodes)
    
    tryCatch({
      values$measure_matrix <- simulate.measure(
        mat = values$ses_matrix,
        measure = input$measureName,
        affected = input$affectedNode,
        indicators = input$indicatorNodes,
        lower = input$effectRange[1],
        upper = input$effectRange[2]
      )
      
      log_message(paste("Intervention added:", input$measureName))
      showNotification("Intervention added successfully!", type = "success")
      
    }, error = function(e) {
      showNotification(paste("Error adding intervention:", e$message), type = "error")
    })
  })
  
  # Measure network visualization
  output$measureNetworkVis <- renderVisNetwork({
    req(values$measure_matrix)
    
    tryCatch({
      g <- graph_from_adjacency_matrix(values$measure_matrix, mode = "directed", weighted = TRUE)
      intervention_node <- input$measureName
      
      nodes <- data.frame(
        id = V(g)$name,
        label = V(g)$name,
        size = ifelse(V(g)$name == intervention_node, 35, 25),
        color = list(
          background = ifelse(V(g)$name == intervention_node, "#E74C3C", "#97C2FC"),
          border = ifelse(V(g)$name == intervention_node, "#C0392B", "#2B7CE9")
        )
      )
      
      edges <- data.frame(
        from = get.edgelist(g)[,1],
        to = get.edgelist(g)[,2],
        width = abs(E(g)$weight) * 5,
        color = ifelse(E(g)$weight > 0, "#2B7CE9", "#E74C3C"),
        arrows = "to"
      )
      
      visNetwork(nodes, edges) %>%
        visOptions(highlightNearest = TRUE) %>%
        visLayout(randomSeed = 123)
        
    }, error = function(e) {
      NULL
    })
  })
  
  # Run measure analysis
  observeEvent(input$runMeasureAnalysis, {
    req(values$measure_matrix, values$quantitative_results)
    
    withProgress(message = 'Analyzing intervention effects...', value = 0, {
      
      tryCatch({
        measure_sim <- SES.simulate(
          SES.mat = values$measure_matrix,
          iter = input$measureIterations,
          save.fig = FALSE
        )
        
        original_sim <- values$quantitative_results$simulation
        n_original <- nrow(original_sim)
        original_final <- original_sim[, ncol(original_sim)]
        measure_final <- measure_sim[1:n_original, ncol(measure_sim)]
        
        state_difference <- measure_final - original_final
        names(state_difference) <- rownames(values$ses_matrix)
        
        values$measure_results <- list(
          simulation = measure_sim,
          differences = state_difference,
          original_final = original_final,
          measure_final = measure_final
        )
        
        log_message("Intervention analysis completed")
        showNotification("Intervention analysis completed!", type = "success")
        
      }, error = function(e) {
        showNotification(paste("Error in intervention analysis:", e$message), type = "error")
      })
      
      incProgress(1)
    })
  })
  
  # Measure effects
  output$measureEffects <- renderTable({
    req(values$measure_results)
    
    differences <- values$measure_results$differences
    
    data.frame(
      Node = names(differences),
      Original = round(values$measure_results$original_final, 4),
      With_Intervention = round(values$measure_results$measure_final, 4),
      Difference = round(differences, 4)
    )
  }, bordered = TRUE)
  
  # Measure comparison plot
  output$measureComparisonPlot <- renderPlotly({
    req(values$measure_results)
    
    comparison_data <- data.frame(
      Node = names(values$measure_results$differences),
      Original = values$measure_results$original_final,
      With_Intervention = values$measure_results$measure_final
    )
    
    comparison_long <- reshape2::melt(comparison_data, id.vars = "Node")
    
    p <- ggplot(comparison_long, aes(x = Node, y = value, fill = variable)) +
      geom_col(position = "dodge", alpha = 0.8) +
      scale_fill_manual(values = c("Original" = "#3498DB", "With_Intervention" = "#E74C3C")) +
      labs(x = "Nodes", y = "Final State Value", 
           title = "Intervention Impact Comparison", fill = "Condition") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Analysis summary
  output$analysisSummary <- renderTable({
    analyses <- c("Data Processing", "Qualitative Analysis", "Quantitative Analysis", "Intervention Analysis")
    status <- c(
      ifelse(!is.null(values$ses_matrix), "✓ Complete", "✗ Pending"),
      ifelse(!is.null(values$qualitative_results), "✓ Complete", "✗ Pending"),
      ifelse(!is.null(values$quantitative_results), "✓ Complete", "✗ Pending"),
      ifelse(!is.null(values$measure_results), "✓ Complete", "✗ Pending")
    )
    
    data.frame(Analysis = analyses, Status = status)
  }, bordered = TRUE)
  
  # Export functions
  output$exportResults <- downloadHandler(
    filename = function() {
      paste("network_analysis_results_", Sys.Date(), ".zip", sep = "")
    },
    content = function(file) {
      temp_dir <- tempdir()
      files_to_zip <- c()
      
      if (!is.null(values$ses_matrix)) {
        ses_file <- file.path(temp_dir, "ses_matrix.csv")
        write.csv(values$ses_matrix, ses_file, row.names = TRUE)
        files_to_zip <- c(files_to_zip, ses_file)
      }
      
      log_file <- file.path(temp_dir, "analysis_log.txt")
      writeLines(values$analysis_log, log_file)
      files_to_zip <- c(files_to_zip, log_file)
      
      zip(file, files_to_zip, flags = "-j")
    }
  )
  
  output$exportNetwork <- downloadHandler(
    filename = function() {
      paste("network_", Sys.Date(), ".graphml", sep = "")
    },
    content = function(file) {
      req(values$ses_matrix)
      g <- graph_from_adjacency_matrix(values$ses_matrix, mode = "directed", weighted = TRUE)
      write_graph(g, file, format = "graphml")
    }
  )
  
  output$exportReport <- downloadHandler(
    filename = function() {
      paste("network_analysis_report_", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      html_content <- paste0(
        "<html><head><title>Network Analysis Report</title></head><body>",
        "<h1>marineSABRES Network Analysis Report</h1>",
        "<p>Generated on: ", Sys.time(), "</p>",
        "<h2>Analysis Log</h2>",
        "<pre>", paste(values$analysis_log, collapse = "\n"), "</pre>",
        "</body></html>"
      )
      writeLines(html_content, file)
    }
  )
  
  # Analysis log
  output$analysisLog <- renderText({
    paste(values$analysis_log, collapse = "\n")
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)