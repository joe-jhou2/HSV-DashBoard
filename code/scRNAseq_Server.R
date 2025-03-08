# Define server logic
scRNAseq_server <- function(input, output, session) {
  #----------------------------------------------------------------------------#
  # Canvas preparation
  #----------------------------------------------------------------------------#
  # Reactive values to store dataset and related data
  seurat_object <- reactiveVal(NULL)
  color_object <- reactiveVal(NULL)
  
  # Reactive value to track loading status
  loading_status <- reactiveVal(FALSE)
  
  # Notify user that data is loaded
  notify_data_loaded <- function() {
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
    
    # Simulate progress for loading
    for (i in 1:10) {
      Sys.sleep(0.1)  # Simulate dataset loading process
      update_progress_bar(i * 1)
    }

    # Load dataset based on the selection
    dataset_name <- input$tab_dataset_option
    
    if (dataset_name == "T cell dataset") {
      # load data
      load("data/Tcell_dataset.Rdata")

      # connect HDF5 array
      Tcell_h5_filepath = "data/Tcell_dataset.h5" 
      hdf5_expr_matrix = HDF5Array(Tcell_h5_filepath, "data")
      Tcell_dataset$seurat_object@assays$RNA$data = hdf5_expr_matrix
      
      # assign data to objects
      seurat_object(Tcell_dataset$seurat_object)
      color_object(Tcell_dataset$color_object)
    } 
    
    if (dataset_name == "Myeloid cell dataset") {
      # load data
      load("data/Myeloid_dataset.Rdata")
      
      # connect to HDF5 array
      Mye_h5_filepath = "data/Myeloid_dataset.h5" 
      hdf5_expr_matrix = HDF5Array(Mye_h5_filepath, "data")
      Myeloid_dataset$seurat_object@assays$RNA$data = hdf5_expr_matrix
      
      # assign data to objects
      seurat_object(Myeloid_dataset$seurat_object)
      color_object(Myeloid_dataset$color_object)
    } 
    
    # Update other input selections based on annotation level
    observeEvent(input$tab_annotation_option, {
      req(seurat_object(), input$tab_annotation_option)

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
    dataset_name = input$tab_dataset_option
    cell_annot_lev = colnames(seurat_object()@meta.data)[grep("CellType", colnames(seurat_object()@meta.data))]
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
    plotRenderedStatus$UMAP_all = FALSE
    plotRenderedStatus$UMAP_Prior = FALSE
    plotRenderedStatus$UMAP_Lesion = FALSE
    plotRenderedStatus$UMAP_Post = FALSE
    plotRenderedStatus$dynamicBarplot_CellType_Pert = FALSE
    plotRenderedStatus$Barplot_CellType_Pert = FALSE
    plotRenderedStatus$dynamicBarplot_CellType_Count = FALSE
    plotRenderedStatus$Barplot_CellType_Count = FALSE
    plotRenderedStatus$dynamicBarplot = FALSE
    plotRenderedStatus$Barplot_Subject_Cluster_Pert = FALSE
    
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
    
    # get cell type if specified 
    selected_celltype <- if(any(input$celltype_tab_celltype_option == "All")) {
      levels(Idents(seurat_object()))
    } else {
      input$celltype_tab_celltype_option
    }
    
    # how many cell types selected
    num_selected_celltype <- length(selected_celltype)
    
    # get subject if specified
    selected_subject <- if(any(input$celltype_tab_subject_option == "All")) {
      unique(seurat_object()$Subject)
    } else {
      input$celltype_tab_subject_option
    }
    
    # how many subject selected
    num_selected_subject <- length(selected_subject)
    
    # get cell type annotation level
    selected_annotation_level <- input$tab_annotation_option
    
    # general theme for UMAP
    umap_tab_custom_theme <- function(){
      theme(legend.position = "none",
            axis.title = element_text(size = 0),
            plot.title = element_text(color = "black",
                                      size = 16, hjust = 0.5,
                                      face = "bold"))
    }
    
    # Computing Each Figure
    list(
      # Plot whole UMAP
      UMAP_all = computeUMAP(seurat_object = seurat_object(),
                             selectedCellType = input$celltype_tab_celltype_option,
                             selectedStatus = "All",
                             selectedSubject = input$celltype_tab_subject_option,
                             selectedAnnotation = selected_annotation_level,
                             CellType_color = color_object(),
                             selectedFeature = NULL, 
                             feature_data = NULL)$plot,
      # Plot Prior UMAP
      UMAP_Prior = computeUMAP(seurat_object = seurat_object(),
                               selectedCellType = input$celltype_tab_celltype_option,
                               selectedStatus = "Prior",
                               selectedSubject = input$celltype_tab_subject_option,
                               selectedAnnotation = selected_annotation_level,
                               CellType_color = color_object(),
                               selectedFeature = NULL, 
                               feature_data = NULL)$plot +
        umap_tab_custom_theme() + ggtitle("Prior"),
      
      # Plot Lesion UMAP
      UMAP_Lesion = computeUMAP(seurat_object = seurat_object(),
                                selectedCellType = input$celltype_tab_celltype_option,
                                selectedStatus = "Lesion",
                                selectedSubject = input$celltype_tab_subject_option,
                                selectedAnnotation = selected_annotation_level,
                                CellType_color = color_object(),
                                selectedFeature = NULL, 
                                feature_data = NULL)$plot +
        umap_tab_custom_theme() + ggtitle("Lesion"),
      
      # Plot Post UMAP
      UMAP_Post = computeUMAP(seurat_object = seurat_object(),
                              selectedCellType = input$celltype_tab_celltype_option,
                              selectedStatus = "Post",
                              selectedSubject = input$celltype_tab_subject_option,
                              selectedAnnotation = selected_annotation_level,
                              CellType_color = color_object(),
                              selectedFeature = NULL, 
                              feature_data = NULL)$plot +
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
    req(processed_cluster_data())
    plotRenderedStatus$UMAP_all <- TRUE
    plot <- processed_cluster_data()$UMAP_all
    showNotification("UMAP plot updated", type = "message", duration = 10)
    plot
  })
  outputOptions(output, "UMAP_all", suspendWhenHidden = FALSE)  # Ensure UMAP_all is always computed
  
  # Prior Status UMAP
  output$UMAP_Prior <- renderPlot({
    req(processed_cluster_data())
    plotRenderedStatus$UMAP_Prior <- TRUE  # Mark this plot as rendered
    processed_cluster_data()$UMAP_Prior
  })
  outputOptions(output, "UMAP_Prior", suspendWhenHidden = FALSE)
  
  # Lesion Status UMAP
  output$UMAP_Lesion <- renderPlot({
    req(processed_cluster_data())
    plotRenderedStatus$UMAP_Lesion <- TRUE
    processed_cluster_data()$UMAP_Lesion
  })
  outputOptions(output, "UMAP_Lesion", suspendWhenHidden = FALSE)
  
  # Post Status UMAP
  output$UMAP_Post <- renderPlot({
    req(processed_cluster_data()) 
    plotRenderedStatus$UMAP_Post <- TRUE
    processed_cluster_data()$UMAP_Post
  })
  outputOptions(output, "UMAP_Post", suspendWhenHidden = FALSE)
  
  # Barplot, Celltype pert
  output$dynamicBarplot_CellType_Pert <- renderUI({
    req(processed_cluster_data())
    plotRenderedStatus$dynamicBarplot_CellType_Pert <- TRUE
    numCellType <- processed_cluster_data()$num_selected_celltype
    numfacetRow <- round(numCellType / 6)
    dynamicHeight <- numfacetRow * 150
    plotOutput("Barplot_CellType_Pert",
               height = paste0(dynamicHeight, "px"), width = "800px")
  })
  outputOptions(output, "dynamicBarplot_CellType_Pert", suspendWhenHidden = FALSE)  # Ensure UMAP_all is always computed
  
  output$Barplot_CellType_Pert <- renderPlot({
    req(processed_cluster_data())
    plotRenderedStatus$Barplot_CellType_Pert <- TRUE
    plot <- processed_cluster_data()$Barplot_CellType_Pert
    showNotification("Bar plot (Pert) updated", type = "message", duration = 10)
    plot
  })
  outputOptions(output, "Barplot_CellType_Pert", suspendWhenHidden = FALSE)
  
  # Barplot, Celltype count
  output$dynamicBarplot_CellType_Count <- renderUI({
    req(processed_cluster_data())
    plotRenderedStatus$dynamicBarplot_CellType_Count <- TRUE
    numCellType <- processed_cluster_data()$num_selected_celltype 
    numfacetRow <- round(numCellType / 6)
    dynamicHeight <- numfacetRow * 150
    plotOutput("Barplot_CellType_Count",
               height = paste0(dynamicHeight, "px"), width = "800px")
  })
  outputOptions(output, "dynamicBarplot_CellType_Count", suspendWhenHidden = FALSE)
  
  output$Barplot_CellType_Count <- renderPlot({
    req(processed_cluster_data())
    plotRenderedStatus$Barplot_CellType_Count <- TRUE
    plot <- processed_cluster_data()$Barplot_CellType_Count
    showNotification("Bar plot (Count) updated",
                     type = "message", duration = 10)
    plot
  })
  outputOptions(output, "Barplot_CellType_Count", suspendWhenHidden = FALSE)
  
  # Barplot, by Subject
  output$dynamicBarplot <- renderUI({
    req(processed_cluster_data())
    plotRenderedStatus$dynamicBarplot <- TRUE
    numSubjects <- processed_cluster_data()$num_selected_subject
    dynamicHeight <- max(200, 150 + numSubjects * 35)
    plotOutput("Barplot_Subject_Cluster_Pert",
               height = paste0(dynamicHeight, "px"))
  })
  outputOptions(output, "dynamicBarplot", suspendWhenHidden = FALSE)
  
  output$Barplot_Subject_Cluster_Pert <- renderPlot({
    req(processed_cluster_data())
    plotRenderedStatus$Barplot_Subject_Cluster_Pert <- TRUE
    plot <- processed_cluster_data()$Barplot_Subject_Cluster_Pert
    plot
  })
  outputOptions(output, "Barplot_Subject_Cluster_Pert", suspendWhenHidden = FALSE)
  
  # Close modal after all images are rendered
  observe({
    if (all(unlist(reactiveValuesToList(plotRenderedStatus)) == TRUE)) {
      removeModal()
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
    
    # Centralized feature extraction result
    feature_extraction_result <- reactive({
      req(seurat_object())
      # Perform the feature extraction once and store the result
      result <- feature_extraction(seurat_object = seurat_object(),
                                   selectedCellType = "All", 
                                   selectedFeature = valid_genes, 
                                   selectedAnnotation = input$tab_annotation_option)
      return(result)
    })
    
    
    # Perform your data extraction or analysis function
    list(
      UMAP = computeUMAP(seurat_object = seurat_object(),
                         selectedCellType = input$feature_tab_celltype_option, 
                         selectedStatus = "All",
                         selectedSubject = input$feature_tab_subject_option,
                         selectedAnnotation = selected_annotation_level,
                         CellType_color = color_object(),
                         selectedFeature = NULL, 
                         feature_data = NULL),
      
      UMAP_feature = computeUMAP(seurat_object = seurat_object(),
                                 selectedCellType = input$feature_tab_celltype_option, 
                                 selectedStatus = "All",
                                 selectedSubject = input$feature_tab_subject_option,
                                 selectedAnnotation = selected_annotation_level,
                                 CellType_color = color_object(),
                                 selectedFeature = valid_genes, 
                                 feature_data = feature_extraction_result()),
      
      heatmap = computeHeatmap(seurat_object = seurat_object(),
                               selectedCellType = input$feature_tab_celltype_option,
                               selectedStatus = "All",
                               selectedSubject = input$feature_tab_subject_option,
                               selectedFeature = valid_genes,
                               selectedAnnotation = selected_annotation_level,
                               CellType_color = color_object(),
                               feature_data = feature_extraction_result()),
   
      vln = computeVln(seurat_object = seurat_object(),
                       selectedCellType = input$feature_tab_celltype_option,
                       selectedStatus = "All",
                       selectedSubject = input$feature_tab_subject_option,
                       selectedFeature = valid_genes,
                       selectedAnnotation = selected_annotation_level,
                       CellType_color = color_object(),
                       feature_data = feature_extraction_result()),

      feaPert = computeFeaturePert(seurat_object = seurat_object(),
                                   selectedCellType = input$feature_tab_celltype_option,
                                   selectedSubject = input$feature_tab_subject_option,
                                   selectedFeature = valid_genes,
                                   selectedAnnotation = selected_annotation_level,
                                   CellType_color = color_object(),
                                   feature_data = feature_extraction_result()),

      customDot = computeDotPlot(seurat_object = seurat_object(),
                                 selectedCellType = input$feature_tab_celltype_option,
                                 selectedSubject = input$feature_tab_subject_option,
                                 selectedFeature = valid_genes,
                                 selectedAnnotation = selected_annotation_level,
                                 CellType_color = color_object(),
                                 feature_data = feature_extraction_result()),

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
    req(processed_gene_data())
    plotRenderedStatus2$UMAP <- TRUE
    plot <- processed_gene_data()$UMAP$plot
    showNotification("UMAP plot updated", type = "message", duration = 10)
    plot
  })
  outputOptions(output, "UMAP", suspendWhenHidden = FALSE)
  
  output$dynamicUMAPfeature <- renderUI({
    req(processed_gene_data())
    plotRenderedStatus2$dynamicUMAPfeature <- TRUE
    numGene <- processed_gene_data()$num_valid_genes
    dynamicHeight <- 300 * ceiling(numGene/4)
    plotOutput("UMAP_feature", height = paste0(dynamicHeight, "px"))
  })
  outputOptions(output, "dynamicUMAPfeature", suspendWhenHidden = FALSE)
  
  output$UMAP_feature <- renderPlot({
    req(processed_gene_data())
    plotRenderedStatus2$UMAP_feature <- TRUE
    plot <- processed_gene_data()$UMAP_feature$plot_feature
    showNotification("UMAP Feature plot updated", type = "message", duration = 10)
    plot
  })
  outputOptions(output, "UMAP_feature", suspendWhenHidden = FALSE)
  
  # Tab: Heatmap
  output$dynamicHeatmap <- renderUI({
    req(processed_gene_data())
    plotRenderedStatus2$dynamicHeatmap <- TRUE
    numGene <- processed_gene_data()$num_valid_genes
    dynamicHeight <- max(300, 300 + numGene * 30)
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
      removeModal()
      showNotification("All plots are rendered and modal closed", type = "message", duration = 5)
    }
  })
  
  #----------------------------------------------------------------------------#
  # Gene Spatial Discovery
  #----------------------------------------------------------------------------#
  output$dynamic_spatial_plot_ui <- renderUI({
    req(validate_gene_spatial())
    
    # Get number of genes to plot
    numGenes <- length(validate_gene_spatial())
    
    # Calculate dynamic height based on number of genes (4 per row)
    dynamicHeight <- 300 * ceiling(numGenes / 4)
    
    plotOutput("spatial_gene_plot", height = paste0(dynamicHeight, "px"))
  })
  
  outputOptions(output, "dynamic_spatial_plot_ui", suspendWhenHidden = FALSE)
  
  # validate spatial plot input gene
  validate_gene_spatial <- eventReactive(input$run_gene_spatial, {
    print("Run Analysis Button Clicked")
    
    # Show loading animation
    shinyjs::show("loading")

    showModal(modalDialog(
      title = "Analyzing Data",
      "Please wait while the data is being analyzed.",
      footer = NULL,
      easyClose = FALSE
    ))
    
    # safeguard
    req(input$gene_name_input) 
    
    message("Run button clicked.")
    
    input_genes <- strsplit(input$gene_name_input, ",\\s*")[[1]]
    input_genes <- trimws(input_genes)
    
    # Load dataset
    load("data/HSV434-Visium-A1_sub_label.Rdata")
    print("Visium Data loaded successfully!")
    
    # Extract gene names from dataset
    all_genes <- rownames(A1_sub@assays$Spatial)
    
    # Check valid and invalid genes
    valid_genes <- input_genes[input_genes %in% all_genes]
    invalid_genes <- setdiff(input_genes, all_genes)
    
    if (length(invalid_genes) > 0) {
      showModal(modalDialog(
        title = "Invalid Gene Names Detected",
        paste("The following gene names are not found in the dataset:",
              paste(invalid_genes, collapse = ", ")),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }
    
    return(valid_genes)
  })
  
  output$spatial_gene_plot <- renderPlot({
    genes_to_plot <- validate_gene_spatial()
    req(length(genes_to_plot) > 0)
    
    load("data/HSV434-Visium-A1_sub_label.Rdata")
    
    spatial_feature_theme <- theme(
      plot.background = element_rect(fill = "white"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      panel.background = element_rect(fill = "white"),
      legend.position = "right",
      legend.margin = margin(t = 0.1),
      plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 8)
    )
    
    # Generate plots
    plots <- lapply(genes_to_plot, function(gene) {
      SpatialFeaturePlot(A1_sub, 
                         features = gene, 
                         pt.size.factor = 4, image.alpha = 0,
                         stroke = NA, alpha = 1, max.cutoff = 'q95') + spatial_feature_theme
    })
    
    # Arrange plots into a grid (4 per row)
    plot_grid <- cowplot::plot_grid(plotlist = plots, ncol = 4, align = "hv")
    
    # Hide loading animation once the plot is ready
    shinyjs::hide("loading")
    removeModal()
    
    return(plot_grid)
  })
  
  #----------------------------------------------------------------------------#
  # Visium De-convolution Plot
  #----------------------------------------------------------------------------#
  output$dynamic_pie_plot_ui <- renderUI({
    observe({
      req(session$clientData$output_dynamic_plot_ui_hidden == FALSE)
    })

    height <- session$clientData$output_scatter_pie_plot_height
    if (is.null(height)) height <- 800
    div(
      style = "width: 100%;",
      plotlyOutput("scatter_pie_plot", height = paste0(height, "px"))
    )
  })
  
  # validate spatial plot input gene
  deconvo_run <- eventReactive(input$run_deconvo, {
    print("Run Analysis Button Clicked")
    
    # Show loading animation
    shinyjs::show("loading")

    showModal(modalDialog(
      title = "Analyzing Data",
      "Please wait while the data is being analyzed.",
      footer = NULL,
      easyClose = FALSE
    ))
    
    # Load dataset
    load("data/Data__Visium_Deconvolution_Interactive.Rdata")
    
    Sys.sleep(3)

    return(list(Res4Plot_A1 = Res4Plot_A1, cell_type_names_A1 = cell_type_names_A1, colors = colors))
  })
  
  output$scatter_pie_plot <- renderPlotly({
    deconvo_result <- deconvo_run()  # Wait for eventReactive result
    req(deconvo_result)
    
    Res4Plot_A1 <- deconvo_result$Res4Plot_A1
    cell_type_names_A1 <- deconvo_result$cell_type_names_A1
    colors <- deconvo_result$colors
    
    # Compute x and y limits
    x_min <- min(Res4Plot_A1$imagecol) - 55
    x_max <- max(Res4Plot_A1$imagecol) + 55
    y_min <- max(Res4Plot_A1$imagerow) + 55
    y_max <- min(Res4Plot_A1$imagerow) - 55
    
    # Get screen width & height
    plot_width <- session$clientData$output_scatter_pie_plot_width
    plot_height <- session$clientData$output_scatter_pie_plot_height
    
    pie_size <- 0.02 
    
    if (plot_width <= 1000) {
      decrement_factor <- max(0.6, 1 - (floor((1000 - plot_width) / 100) * 0.06))
      pie_size <- pie_size * decrement_factor
    } else {
      increment_factor <- 1 + (floor((plot_width - 1000) / 100) * 0.05)
      pie_size <- pie_size * increment_factor
    }

    # Initialize the plotly figure
    fig <- plot_ly()
    
    # Loop through each row to add pie charts
    for (i in 1:nrow(Res4Plot_A1)) {
      row_data <- Res4Plot_A1[i, ]
      
      # Extract cell type proportions
      pie_values <- as.numeric(row_data[cell_type_names_A1])
      pie_labels <- cell_type_names_A1
      pie_colors <- colors[pie_labels]
      
      # Filter out zero values to avoid empty slices
      non_zero_idx <- which(pie_values > 0)
      if (length(non_zero_idx) > 0) {
        fig <- fig %>%
          add_pie(
            labels = pie_labels[non_zero_idx],
            values = pie_values[non_zero_idx],
            domain = list(
              x = c((row_data$imagecol - x_min) / (x_max - x_min) - pie_size,
                    (row_data$imagecol - x_min) / (x_max - x_min) + pie_size),
              y = c((row_data$imagerow - y_min) / (y_max - y_min) - pie_size,
                    (row_data$imagerow - y_min) / (y_max - y_min) + pie_size)
            ),
            marker = list(colors = pie_colors[non_zero_idx]),
            textinfo = "none",
            showlegend = FALSE,
            hoverinfo = "label+percent"
          )
      }
    }
    
    # Apply axis limits and layout
    fig <- fig %>%
      layout(
        title = "",
        xaxis = list(title = "Image Column", range = c(x_min, x_max), zeroline = FALSE, showgrid = FALSE),
        yaxis = list(title = "Image Row", range = c(y_max, y_min), zeroline = FALSE, showgrid = FALSE, autorange = "reversed"),
        showlegend = TRUE
      )
    
    shinyjs::hide("loading")
    removeModal()
    
    return(fig)
  })
  
}