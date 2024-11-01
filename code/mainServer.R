# Define server logic
main_server <- function(input, output, session) {
  
  #----------------------------------------------------------------------------#
  # Canvas preparation
  #----------------------------------------------------------------------------#
  # Reactive values to store dataset and related data
  # current_dataset <- reactiveVal("")
  seurat_object <- reactiveVal(NULL)
  color_object <- reactiveVal(NULL)
  # Reactive value to track loading status
  loading_status <- reactiveVal(FALSE)
  
  # Notify user that data is loaded
  notify_data_loaded = function() {
    showNotification("Data has been loaded successfully", type = "message")
  }
  
  # Create the progress bar output
  output$progress_bar <- renderUI({
    progressBar(id = "dataset_progress",value = 0,status = "info", striped = TRUE)
  })
  
  # Update the progress bar value
  update_progress_bar <- function(value) {
    updateProgressBar(session = session,
                      id = "dataset_progress",
                      value = value)
  }
  
  #----------------------------------------------------------------------------#
  # Load Data and select cell annotation
  #----------------------------------------------------------------------------#
  # Observe the "Load Data" button click event
  observeEvent(input$loadDataBtn, {
    req(input$tab_dataset_option)
    
    # Show modal when loading starts
    showModal(modalDialog(
      title = "Loading Data",
      "Please wait while the data is being loaded.",
      footer = NULL
    ))
    
    # Reset the current dataset and UI elements before loading new data
    updateSelectInput(session, "celltype_tab_celltype_option", choices = "", selected = "")
    updateSelectInput(session, "celltype_tab_subject_option", choices = "", selected = "")
    updateSelectInput(session, "feature_tab_celltype_option", choices = "", selected = "")
    updateSelectInput(session, "feature_tab_subject_option", choices = "", selected = "")
    
    # Set loading status to TRUE (loading starts)
    loading_status(TRUE)
    
    # Reset and show progress bar
    updateProgressBar(session = session, id = "dataset_progress", value = 0)
    
    # Simulate progress for loading (replace with actual loading progress)
    for (i in 1:10) {
      Sys.sleep(0.1)  # Simulate dataset loading process
      update_progress_bar(i * 1)
    }

    # Load dataset based on the selection
    dataset_name <- input$tab_dataset_option
    if (dataset_name == "T cell dataset") {
      load("data/Tcell_dataset.Rdata")
      
      Tcell_h5_filepath = "data/Tcell_dataset.h5" 
      hdf5_expr_matrix = HDF5Array(Tcell_h5_filepath, "data")
      Tcell_dataset$seurat_object@assays$RNA$data = hdf5_expr_matrix
      
      seurat_object(Tcell_dataset$seurat_object)
      color_object(Tcell_dataset$color_object)
    } 
    
    if (dataset_name == "Myeloid cell dataset") {
      load("data/Myeloid_dataset.Rdata")
      
      Mye_h5_filepath = "data/Myeloid_dataset.h5" 
      hdf5_expr_matrix = HDF5Array(Mye_h5_filepath, "data")
      Myeloid_dataset$seurat_object@assays$RNA$data = hdf5_expr_matrix
      
      seurat_object(Myeloid_dataset$seurat_object)
      color_object(Myeloid_dataset$color_object)
    } 
    
    # Update other input selections based on annotation level
    observeEvent(input$tab_annotation_option, {
      req(seurat_object(), input$tab_annotation_option)
      
      # selected_annotation_level <- input$tab_annotation_option
      
      updateSelectInput(session, "celltype_tab_celltype_option",
                        choices = c("All", dataset_info()$cell_types))
      
      updateSelectInput(session, "celltype_tab_subject_option", 
                        choices = c("All", dataset_info()$subjects))
      
      updateSelectInput(session, "feature_tab_celltype_option",
                        choices = c("All", dataset_info()$cell_types))
      
      updateSelectInput(session, "feature_tab_subject_option",
                        choices = c("All", dataset_info()$subjects))
    })
    
    # Notify user that data is loaded
    notify_data_loaded()
    
    # Set loading status to FALSE (loading complete)
    loading_status(FALSE)
    
    # Update progress bar to 100% after data is loaded
    updateProgressBar(session = session, id = "dataset_progress", value = 100)
    
    # Get the annotation data from the loaded dataset
    annotation_levels <- get_annotation()

    # Update selectInput with valid choices and default selection
    updateSelectInput(session, "tab_annotation_option", 
                      choices = annotation_levels$cell_annot_lev,
                      selected = "")
    
    # Remove modal and update progress message when data is loaded
    removeModal()

  })
  
  # Helper function: 
  # Function to select annotation levels based on the loaded dataset
  get_annotation <- function() {
    dataset_name <- input$tab_dataset_option
    cell_annot_lev <- colnames(seurat_object()@meta.data)[grep("CellType", colnames(seurat_object()@meta.data))]
    if (length(cell_annot_lev) == 0) stop("No valid annotation levels found")
    return(list(cell_annot_lev = cell_annot_lev))
  }
  
  #----------------------------------------------------------------------------#
  # Load Cell Type and Subject from current data set
  #----------------------------------------------------------------------------#
  # Monitor the annotation selection and display/hide the notice
  observe({
    if (is.null(input$tab_annotation_option)) {
      shinyjs::show("annotation_notice")  # Show notice if no annotation level is selected
    } else {
      shinyjs::hide("annotation_notice")  # Hide the notice once an annotation level is selected
    }
  })
  
  # # Update other input selections based on annotation level
  # observeEvent(input$tab_annotation_option, {
  #   req(seurat_object(), input$tab_annotation_option)
  #   
  #   # selected_annotation_level <- input$tab_annotation_option
  # 
  #   updateSelectInput(session, "celltype_tab_celltype_option",
  #                     choices = c("All", dataset_info()$cell_types))
  #   
  #   updateSelectInput(session, "celltype_tab_subject_option", 
  #                     choices = c("All", dataset_info()$subjects))
  #   
  #   updateSelectInput(session, "feature_tab_celltype_option",
  #                     choices = c("All", dataset_info()$cell_types))
  #   
  #   updateSelectInput(session, "feature_tab_subject_option",
  #                     choices = c("All", dataset_info()$subjects))
  # })
  
  # Helper Function: retrieve dataset details based on dataset selection
  get_dataset <- function(annotation_level) {
    req(seurat_object(), color_object())  # Ensure both are loaded
    
    dataset <- seurat_object()
    colors <- color_object()
    
    # Check if annotation level exists
    if (!(annotation_level %in% colnames(dataset@meta.data))) {
      stop("Selected annotation level not found in the dataset")
    }
    
    # Return dataset details
    return(list(
      cell_types = levels(dataset@meta.data[[annotation_level]]),
      subjects = levels(dataset$Subject),
      data_object = dataset,
      colors = colors
    ))
  }
  
  dataset_info <- reactive({
    req(input$tab_annotation_option, seurat_object())
    get_dataset(input$tab_annotation_option)
  })
  
  #---------------------------------------------------------------------------#
  # Cluster Discovery Tab
  #---------------------------------------------------------------------------#
  # Reactive values to track the status of each plot
  plotRenderedStatus <- reactiveValues(
    UMAP_all = FALSE,
    UMAP_Prior = FALSE,
    UMAP_Lesion = FALSE,
    UMAP_Post = FALSE,
    dynamicBarplot_CellType_Pert = FALSE, 
    Barplot_CellType_Pert = FALSE,
    dynamicBarplot_CellType_Count = FALSE,
    Barplot_CellType_Count = FALSE,
    dynamicBarplot = FALSE,
    Barplot_Subject_Cluster_Pert = FALSE
  )
  
  # enable click and run function
  processed_cluster_data <- eventReactive(input$run_cluster, {
    # Reset plot statuses when re-running the analysis
    plotRenderedStatus$UMAP_all <- FALSE
    plotRenderedStatus$UMAP_Prior <- FALSE
    plotRenderedStatus$UMAP_Lesion <- FALSE
    plotRenderedStatus$UMAP_Post <- FALSE
    plotRenderedStatus$dynamicBarplot_CellType_Pert <- FALSE
    plotRenderedStatus$Barplot_CellType_Pert <- FALSE
    plotRenderedStatus$dynamicBarplot_CellType_Count <- FALSE
    plotRenderedStatus$Barplot_CellType_Count <- FALSE
    plotRenderedStatus$dynamicBarplot <- FALSE
    plotRenderedStatus$Barplot_Subject_Cluster_Pert <- FALSE
    
    print("Run Analysis Button Clicked")
    
    # Show modal while data is being processed
    showModal(modalDialog(
      title = "Analyzing Data",
      "Please wait while the data is being analyzed.",
      footer = NULL,
      easyClose = FALSE
    ))
    
    # safeguard
    req(seurat_object(), color_object()) 
    
    message("Run button clicked.")
    
    selected_celltype <- if(any(input$celltype_tab_celltype_option == "All")) {
      levels(Idents(seurat_object()))
    } else {
      input$celltype_tab_celltype_option
    }
    num_selected_celltype <- length(selected_celltype)
    
    selected_subject <- if(any(input$celltype_tab_subject_option == "All")) {
      unique(seurat_object()$Subject)
    } else {
      input$celltype_tab_subject_option
    }
    
    selected_annotation_level <- input$tab_annotation_option
    
    num_selected_subject <- length(selected_subject)
    
    # general theme for UMAP
    umap_tab_custom_theme <- function(){
      theme(legend.position = "none",
            axis.title = element_text(size = 0),
            plot.title = element_text(color="black",
                                      size = 16, hjust = 0.5,
                                      face = "bold"))
    }
    
    list(
      # Plot whole UMAP
      UMAP_all = computeUMAP(seurat_object = seurat_object(),
                             selectedCellType = input$celltype_tab_celltype_option,
                             selectedStatus = "All",
                             selectedSubject = input$celltype_tab_subject_option,
                             selectedAnnotation = selected_annotation_level,
                             CellType_color = color_object()),
      # Plot Prior UMAP
      UMAP_Prior = computeUMAP(seurat_object = seurat_object(),
                               selectedCellType = input$celltype_tab_celltype_option,
                               selectedStatus = "Prior",
                               selectedSubject = input$celltype_tab_subject_option,
                               selectedAnnotation = selected_annotation_level,
                               CellType_color = color_object()) +
        umap_tab_custom_theme() + ggtitle("Prior"),
      
      # Plot Lesion UMAP
      UMAP_Lesion = computeUMAP(seurat_object = seurat_object(),
                                selectedCellType = input$celltype_tab_celltype_option,
                                selectedStatus = "Lesion",
                                selectedSubject = input$celltype_tab_subject_option,
                                selectedAnnotation = selected_annotation_level,
                                CellType_color = color_object()) +
        umap_tab_custom_theme() + ggtitle("Lesion"),
      
      # Plot Post UMAP
      UMAP_Post = computeUMAP(seurat_object = seurat_object(),
                              selectedCellType = input$celltype_tab_celltype_option,
                              selectedStatus = "Post",
                              selectedSubject = input$celltype_tab_subject_option,
                              selectedAnnotation = selected_annotation_level,
                              CellType_color = color_object()) +
        umap_tab_custom_theme() + ggtitle("Post"),
      
      # Plot barplot of celltype pert for subject
      Barplot_CellType_Pert = computeBarPlot_CellType_Status(seurat_object = seurat_object(),
                                                             selectedSubject = input$celltype_tab_subject_option,
                                                             selectedAnnotation = selected_annotation_level)$PertBar,
      
      # Plot barplot of celltype count for subject
      Barplot_CellType_Count = computeBarPlot_CellType_Status(seurat_object = seurat_object(),
                                                              selectedSubject = input$celltype_tab_subject_option,
                                                              selectedAnnotation = selected_annotation_level)$CountBar,
      
      # Plot Stacked barplot of celltype pert for subject
      Barplot_Subject_Cluster_Pert = computeStackBarPlot_Subject(seurat_object = seurat_object(),
                                                                 selectedSubject = input$celltype_tab_subject_option,
                                                                 selectedAnnotation = selected_annotation_level,
                                                                 CellType_color = color_object()),
      
      selected_celltype = selected_celltype,
      num_selected_celltype = num_selected_celltype,
      selected_subject = selected_subject,
      num_selected_subject = num_selected_subject, 
      selected_annotation = selected_annotation_level
    )
  })
  
  # main UMAP
  output$UMAP_all <- renderPlot({
    req(processed_cluster_data())  # Ensure analysisResults is computed
    plot = processed_cluster_data()$UMAP_all
    plotRenderedStatus$UMAP_all <- TRUE  # Mark this plot as rendered
    showNotification("UMAP plot updated",type = "message", duration = 10)
    plot
  })
  outputOptions(output, "UMAP_all", suspendWhenHidden = FALSE)  # Ensure UMAP_all is always computed
  
  # Prior Status UMAP
  output$UMAP_Prior <- renderPlot({
    req(processed_cluster_data())
    plotRenderedStatus$UMAP_Prior <- TRUE  # Mark this plot as rendered
    processed_cluster_data()$UMAP_Prior
  })
  outputOptions(output, "UMAP_Prior", suspendWhenHidden = FALSE)  # Ensure UMAP_all is always computed
  
  # Lesion Status UMAP
  output$UMAP_Lesion <- renderPlot({
    req(processed_cluster_data())
    plotRenderedStatus$UMAP_Lesion <- TRUE  # Mark this plot as rendered
    processed_cluster_data()$UMAP_Lesion
  })
  outputOptions(output, "UMAP_Lesion", suspendWhenHidden = FALSE)  # Ensure UMAP_all is always computed
  
  # Post Status UMAP
  output$UMAP_Post <- renderPlot({
    req(processed_cluster_data()) 
    plotRenderedStatus$UMAP_Post <- TRUE  # Mark this plot as rendered
    processed_cluster_data()$UMAP_Post
  })
  outputOptions(output, "UMAP_Post", suspendWhenHidden = FALSE)  # Ensure UMAP_all is always computed
  
  # Barplot, Celltype pert
  output$dynamicBarplot_CellType_Pert <- renderUI({
    req(processed_cluster_data())
    plotRenderedStatus$dynamicBarplot_CellType_Pert <- TRUE
    numCellType <- length(unique(get_dataset(input$tab_annotation_option)$cell_types))
    numfacetRow <- round(numCellType / 6)
    dynamicHeight <- numfacetRow * 150
    plotOutput("Barplot_CellType_Pert",
               height = paste0(dynamicHeight, "px"), width = "800px")
  })
  outputOptions(output, "dynamicBarplot_CellType_Pert", suspendWhenHidden = FALSE)  # Ensure UMAP_all is always computed
  
  output$Barplot_CellType_Pert <- renderPlot({
    req(processed_cluster_data())
    plotRenderedStatus$Barplot_CellType_Pert <- TRUE  # Mark this plot as rendered
    plot <- processed_cluster_data()$Barplot_CellType_Pert
    showNotification("Bar plot (Pert) updated", type = "message", duration = 10)
    plot
  })
  outputOptions(output, "Barplot_CellType_Pert", suspendWhenHidden = FALSE)  # Ensure UMAP_all is always computed
  
  # Barplot, Celltype count
  output$dynamicBarplot_CellType_Count <- renderUI({
    req(processed_cluster_data())
    plotRenderedStatus$dynamicBarplot_CellType_Count <- TRUE
    numCellType <- length(unique(get_dataset(input$tab_annotation_option)$cell_types))
    numfacetRow <- round(numCellType / 6)
    dynamicHeight <- numfacetRow * 150
    plotOutput("Barplot_CellType_Count",
               height = paste0(dynamicHeight, "px"), width = "800px")
  })
  outputOptions(output, "dynamicBarplot_CellType_Count", suspendWhenHidden = FALSE)  # Ensure UMAP_all is always computed
  
  output$Barplot_CellType_Count <- renderPlot({
    req(processed_cluster_data())
    plotRenderedStatus$Barplot_CellType_Count <- TRUE  # Mark this plot as rendered
    plot <- processed_cluster_data()$Barplot_CellType_Count
    showNotification("Bar plot (Count) updated",
                     type = "message", duration = 10)
    plot
  })
  outputOptions(output, "Barplot_CellType_Count", suspendWhenHidden = FALSE)  # Ensure UMAP_all is always computed
  
  # Barplot, by Subject
  output$dynamicBarplot <- renderUI({
    req(processed_cluster_data())
    plotRenderedStatus$dynamicBarplot <- TRUE
    numSubjects <- processed_cluster_data()$num_selected_subject
    dynamicHeight <- max(200, 150 + numSubjects * 35)
    plotOutput("Barplot_Subject_Cluster_Pert",
               height = paste0(dynamicHeight, "px"))
  })
  outputOptions(output, "dynamicBarplot", suspendWhenHidden = FALSE)  # Ensure UMAP_all is always computed
  
  output$Barplot_Subject_Cluster_Pert <- renderPlot({
    req(processed_cluster_data())
    plotRenderedStatus$Barplot_Subject_Cluster_Pert <- TRUE  # Mark this plot as rendered
    plot <- processed_cluster_data()$Barplot_Subject_Cluster_Pert
    plot
  })
  outputOptions(output, "Barplot_Subject_Cluster_Pert", suspendWhenHidden = FALSE)  # Ensure UMAP_all is always computed
  
  # Close modal after all images are rendered
  observe({
    if (all(unlist(reactiveValuesToList(plotRenderedStatus)) == TRUE)) {
      removeModal()  # Remove the modal after all plots are rendered
      showNotification("All plots are rendered and modal closed", type = "message", duration = 5)
    }
  })
  
  #----------------------------------------------------------------------------#
  # Gene Discovery Tab
  #----------------------------------------------------------------------------#
  plotRenderedStatus2 <- reactiveValues(
    UMAP = FALSE,
    dynamicUMAPfeature = FALSE,
    UMAP_feature = FALSE,
    dynamicHeatmap = FALSE,
    heatmap = FALSE,
    dynamicViolin = FALSE,
    vln = FALSE,
    dynamicFeaturePert = FALSE,
    feaPert = FALSE,
    dynamicDotPlot = FALSE,
    customDot = FALSE
  )

  # Use eventReactive to perform data processing when the button is clicked
  processed_gene_data <- eventReactive(input$run_gene, {
    # Reset plot statuses when re-running the analysis
    plotRenderedStatus2$UMAP = FALSE
    plotRenderedStatus2$dynamicUMAPfeature = FALSE
    plotRenderedStatus2$UMAP_feature = FALSE
    plotRenderedStatus2$dynamicHeatmap = FALSE
    plotRenderedStatus2$heatmap = FALSE
    plotRenderedStatus2$dynamicViolin = FALSE
    plotRenderedStatus2$vln = FALSE
    plotRenderedStatus2$dynamicFeaturePert = FALSE
    plotRenderedStatus2$feaPert = FALSE
    plotRenderedStatus2$dynamicDotPlot = FALSE
    plotRenderedStatus2$customDot = FALSE

    print("Run Analysis Button Clicked")
    
    # Show modal while data is being processed
    showModal(modalDialog(
      title = "Analyzing Data",
      "Please wait while the data is being analyzed.",
      footer = NULL,
      easyClose = FALSE
    ))
    
    # safeguard
    req(seurat_object(), color_object()) 
    
    message("Run button clicked.")
    
    # Trigger validate_gene and store the result
    validation_results = validate_gene()

    # Halt execution if there are invalid genes
    validate(
      need(length(validation_results$invalid_genes) == 0,
           "Please correct the invalid gene names before proceeding.")
    )
    
    # Get genes from valid gene list and get cluster from sharedinput
    valid_genes <- validation_results$valid_genes

    # Calculate the number of gene plots to render
    num_valid_genes <- length(valid_genes)
    
    selected_celltype <- if(any(input$feature_tab_celltype_option == "All")) {
      levels(Idents(seurat_object()))
    } else {
      input$feature_tab_celltype_option
    }

    num_selected_celltype = length(selected_celltype)
    selected_annotation_level <- input$tab_annotation_option

    # Perform your data extraction or analysis function
    list(
      UMAP = computeUMAP(seurat_object = seurat_object(),
                         selectedCellType = input$feature_tab_celltype_option, 
                         selectedStatus = "All",
                         selectedSubject = input$feature_tab_subject_option,
                         selectedAnnotation = selected_annotation_level,
                         CellType_color = color_object()),
      
      UMAP_feature = computeFeatureUMAP(seurat_object = seurat_object(), 
                                        selectedCellType = input$feature_tab_celltype_option,
                                        selectedStatus = "All", 
                                        selectedSubject = input$feature_tab_subject_option, 
                                        selectedAnnotation = selected_annotation_level,
                                        selectedFeature = valid_genes),

      heatmap = computeHeatmap(seurat_object = seurat_object(),
                               selectedCellType = input$feature_tab_celltype_option,
                               selectedStatus = "All",
                               selectedSubject = input$feature_tab_subject_option,
                               selectedFeature = valid_genes,
                               selectedAnnotation = selected_annotation_level,
                               CellType_color = color_object()),
   
      vln = computeVln(seurat_object = seurat_object(),
                       selectedCellType = input$feature_tab_celltype_option,
                       selectedStatus = "All",
                       selectedSubject = input$feature_tab_subject_option,
                       selectedFeature = valid_genes,
                       selectedAnnotation = selected_annotation_level,
                       CellType_color = color_object()),

      feaPert = computeFeaturePert(seurat_object = seurat_object(),
                                   selectedCellType = input$feature_tab_celltype_option,
                                   selectedSubject = input$feature_tab_subject_option,
                                   selectedFeature = valid_genes,
                                   selectedAnnotation = selected_annotation_level,
                                   CellType_color = color_object()),

      customDot = computeDotPlot(seurat_object = seurat_object(),
                                 selectedCellType = input$feature_tab_celltype_option,
                                 selectedSubject = input$feature_tab_subject_option,
                                 selectedFeature = valid_genes,
                                 selectedAnnotation = selected_annotation_level,
                                 CellType_color = color_object()),

      num_valid_genes = num_valid_genes,
      valid_genes = valid_genes,
      selected_celltype = selected_celltype,
      num_selected_celltype = num_selected_celltype
    )
  })
  
  # Helper Function: Validate genes based on the selected dataset
  validate_gene <- eventReactive(input$run_gene, {
    input_genes <- strsplit(input$feature_tab_gene_input, ",\\s*")[[1]]
    input_genes <- trimws(input_genes)
    
    selected_data <- get_dataset(input$tab_annotation_option)
    
    valid_genes <- input_genes[input_genes %in%
                                 rownames(selected_data$data_object@assays$RNA)]
    
    invalid_genes <- setdiff(input_genes,
                             rownames(selected_data$data_object@assays$RNA))
    
    if (length(invalid_genes) > 0) {
      showModal(modalDialog(
        title = "Invalid Gene Names Detected",
        paste("The following gene names are not found in the dataset:",
              paste(invalid_genes, collapse = ", ")),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }
    
    list(valid_genes = valid_genes, invalid_genes = invalid_genes)
  })
  
  # Tab: Feature
  output$UMAP <- renderPlot({
    req(processed_gene_data())  # Ensure analysisResults is computed
    plotRenderedStatus2$UMAP <- TRUE
    plot <- processed_gene_data()$UMAP
    showNotification("UMAP plot updated", type = "message", duration = 10)
    plot
  })
  outputOptions(output, "UMAP", suspendWhenHidden = FALSE)
  
  output$dynamicUMAPfeature <- renderUI({
    req(processed_gene_data()) # Ensure data is processed
    plotRenderedStatus2$dynamicUMAPfeature <- TRUE
    numGene <- processed_gene_data()$num_valid_genes
    dynamicHeight <- 300 * ceiling(numGene/4)
    plotOutput("UMAP_feature", height = paste0(dynamicHeight, "px"))
  })
  outputOptions(output, "dynamicUMAPfeature", suspendWhenHidden = FALSE)
  
  output$UMAP_feature <- renderPlot({
    req(processed_gene_data())  # Ensure analysisResults is computed
    plotRenderedStatus2$UMAP_feature <- TRUE
    plot <- processed_gene_data()$UMAP_feature
    showNotification("UMAP Feature plot updated", type = "message", duration = 10)
    plot
  })
  outputOptions(output, "UMAP_feature", suspendWhenHidden = FALSE)
  
  # Tab: Heatmap
  output$dynamicHeatmap <- renderUI({
    req(processed_gene_data()) # Ensure data is processed
    plotRenderedStatus2$dynamicHeatmap <- TRUE
    numGene <- processed_gene_data()$num_valid_genes
    dynamicHeight <- max(200, 200 + numGene * 30)
    plotOutput("heatmap", height = paste0(dynamicHeight, "px"))
  })
  outputOptions(output, "dynamicHeatmap", suspendWhenHidden = FALSE)
  
  output$heatmap <- renderPlot({
    req(processed_gene_data())
    plotRenderedStatus2$heatmap <- TRUE
    plot <- processed_gene_data()$heatmap
    showNotification("Heatmap plot updated", type = "message", duration = 10)
    plot
  })
  outputOptions(output, "heatmap", suspendWhenHidden = FALSE) 
  
  # Tab: Violin
  output$dynamicViolin <- renderUI({
    req(processed_gene_data()) 
    plotRenderedStatus2$dynamicViolin <- TRUE
    numGene <- processed_gene_data()$num_valid_genes
    numCellType <- processed_gene_data()$num_selected_celltype
    dynamicHeight <- max(120, 120 + numGene * 70)
    dynamicWidth <- max(200, 200 + numCellType * 70)
    plotOutput("vln", 
               height = paste0(dynamicHeight, "px"),
               width = paste0(dynamicWidth, "px"))
  })
  outputOptions(output, "dynamicViolin", suspendWhenHidden = FALSE)

  output$vln <- renderPlot({
    req(processed_gene_data())
    plotRenderedStatus2$vln <- TRUE
    plot <- processed_gene_data()$vln
    showNotification("Violin plot updated", type = "message", duration = 10)
    plot
  })
  outputOptions(output, "vln", suspendWhenHidden = FALSE)

  # Tab: FeaturePert
  output$dynamicFeaturePert <- renderUI({
    req(processed_gene_data()) 
    plotRenderedStatus2$dynamicFeaturePert <- TRUE
    numGene <- processed_gene_data()$num_valid_genes
    numCellType <- processed_gene_data()$num_selected_celltype
    dynamicHeight <- max(120, 120 + numGene * 70)
    dynamicWidth <- max(200, 200 + numCellType * 70)
    plotOutput("feaPert", height = paste0(dynamicHeight, "px"), width = paste0(dynamicWidth, "px"))
  })
  outputOptions(output, "dynamicFeaturePert", suspendWhenHidden = FALSE)
  
  output$feaPert <- renderPlot({
    req(processed_gene_data())
    plotRenderedStatus2$feaPert <- TRUE
    plot <- processed_gene_data()$feaPert
    showNotification("Feature Pert plot updated",type = "message", duration = 10)
    plot
  })
  outputOptions(output, "feaPert", suspendWhenHidden = FALSE)
  
  # Tab: custom Dot
  output$dynamicDotPlot <- renderUI({
    req(processed_gene_data())
    plotRenderedStatus2$dynamicDotPlot <- TRUE
    numGene <- processed_gene_data()$num_valid_genes
    numCellType <- processed_gene_data()$num_selected_celltype
    dynamicHeight <- max(120, 120 + numCellType * 20)
    dynamicWidth <- max(200, 200 +  numGene * 50)
    plotOutput("customDot", 
               height = paste0(dynamicHeight, "px"),
               width = paste0(dynamicWidth, "px"))
  })
  outputOptions(output, "dynamicDotPlot", suspendWhenHidden = FALSE)  # Ensure UMAP_all is always computed
  
  output$customDot <- renderPlot({
    req(processed_gene_data())
    plotRenderedStatus2$customDot <- TRUE
    plot <- processed_gene_data()$customDot
    showNotification("Custom Dot plot updated", type = "message", duration = 10)
    plot
  })
  outputOptions(output, "customDot", suspendWhenHidden = FALSE)  # Ensure UMAP_all is always computed
  
  # Close modal after all images are rendered
  observe({
    if (all(unlist(reactiveValuesToList(plotRenderedStatus2)) == TRUE)) {
      removeModal()  # Remove the modal after all plots are rendered
      showNotification("All plots are rendered and modal closed", type = "message", duration = 5)
    }
  })
  
  #----------------------------------------------------------------------------#
  # Exit
  #----------------------------------------------------------------------------#
  observeEvent(input$signOutBtn, {
    print("Button clicked!")  # Check if this prints anything to the console
    showNotification("Sign Out Button was clicked")
    
    # Perform any cleanup or resource freeing logic here
    gc()  # Force garbage collection to free memory
    # Optionally remove any session-specific cached data here
    
    # End the user session (you may already have this in place)
    session$onSessionEnded(function() {
      showNotification("Session ended, resources cleared.")
      gc()  # Run garbage collection when the session ends
    })
  })
  
}