computeFeatureUMAP = function(seurat_object, selectedCellType, selectedStatus, selectedSubject, selectedAnnotation, selectedFeature) {
  # Check if selectedAnnotation exists in the metadata
  if (!(selectedAnnotation %in% colnames(seurat_object@meta.data))) {
    stop(paste("Column", selectedAnnotation, "does not exist in metadata"))
  }
  
  Idents(seurat_object) = selectedAnnotation
  
  # decide scale limit
  scale_limit = round(max(abs(seurat_object@reductions$umap@cell.embeddings)))
  x_limits = c(-scale_limit, scale_limit)
  y_limits = c(-scale_limit, scale_limit)
  
  # Subset the Seurat object based on conditions
  # Use tryCatch to handle errors during subsetting
  seurat_object_sel = tryCatch({
    subset(
      seurat_object,
      subset = (("All" %in% selectedCellType | Idents(seurat_object) %in% selectedCellType) &
                  ("All" %in% selectedStatus | Status %in% selectedStatus) &
                  ("All" %in% selectedSubject | Subject %in% selectedSubject))
    )},
    error = function(e) {
      # Handle the error by returning NULL or creating a specific output
      return(NULL)  # Returning NULL to indicate an error occurred during subsetting
    }
  )
  
  # Check if the subset operation returned NULL (which means an error occurred)
  if (is.null(seurat_object_sel) || length(seurat_object_sel@meta.data) == 0) {
    # Return a message or a blank plot
    return(ggplot() + 
             geom_blank() +
             coord_fixed(ratio = 1) +
             xlim(x_limits) +
             ylim(y_limits) +
             annotate("text", x = 0.25, y = 0.25, 
                      label = "Data\nUnavailable", 
                      size = 6, hjust = 0.5, vjust = 0.5) +
             labs(x = "UMAP1", y = "UMAP2") + 
             theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
                   panel.background = element_rect(fill = NA), 
                   panel.grid = element_blank(),
                   axis.title = element_text(size = 14), 
                   legend.title = element_text(size = 12, face = "bold"),
                   legend.text = element_text(size = 10),
                   legend.spacing.x = unit(0.1, 'cm'),
                   legend.spacing.y = unit(0.1, 'cm'),
                   legend.position = "right",
                   legend.key.size = unit(0.5, 'cm'),
                   legend.box.margin = margin(2, 2, 2, 2)) +
             guides(colour = guide_legend(override.aes = list(size = 3),
                                          title.hjust = 0.5))
    )
  }
  
  # Merge with metadata
  metadata = seurat_object_sel@meta.data %>% 
    rownames_to_column("Barcode") %>%
    select(Barcode, selectedAnnotation, Status, Subject)
  
  suppressWarnings({
    # Plotting
    # Extract UMAP coordinates for plotting
    UMAP = as.data.frame(seurat_object_sel@reductions$umap@cell.embeddings) %>%
      rownames_to_column() %>%
      dplyr::rename(Barcode = rowname) %>%
      {if("UMAP_1" %in% names(.)) {
        dplyr::rename(., UMAP1 = UMAP_1, UMAP2 = UMAP_2)
      } else if("umap_1" %in% names(.)) {
        dplyr::rename(., UMAP1 = umap_1, UMAP2 = umap_2)
      } else {
        stop("UMAP columns not found.")
      }}
    
    # Add meta info
    UMAP_merge = left_join(UMAP, metadata, by = c("Barcode" = "Barcode"))
    
    # extract expression data 
    df_Extraction = feature_extraction(seurat_object, selectedCellType, selectedFeature, selectedAnnotation)
    
    # Filtering based on parameters
    df_Extraction_sel = tryCatch({
      df_Extraction %>%
        filter(
          if ("All" %in% selectedCellType) TRUE else CellType %in% selectedCellType,
          if ("All" %in% selectedStatus) TRUE else Status %in% selectedStatus,
          if ("All" %in% selectedSubject) TRUE else Subject %in% selectedSubject
        )
    },
    error = function(e) {
      # Handle the error by returning NULL or creating a specific output
      return(NULL)  # Returning NULL to indicate an error occurred during subsetting
    }
    )
    
    df_Extraction_sel$Barcode = rownames(df_Extraction_sel)
    final_data = left_join(UMAP_merge, 
                           df_Extraction_sel[,!colnames(df_Extraction_sel) %in% c("orig.ident", "Subject", "Status", "CellType")], 
                           by = c("Barcode" = "Barcode"))
    
    # plotting
    # Create an empty list to store individual plots
    plot_list <- list()
    
    # Loop through selectedFeature to create individual plots
    for(singleFeature in selectedFeature) {
      # Generate a plot for each feature
      p <- ggplot(data = final_data, aes(x = UMAP1, y = UMAP2)) +
        geom_point(aes(color = .data[[singleFeature]]),  # Use .data[[ ]] for dynamic column name
                   size = 0.4, 
                   alpha = 0.8) +
        scale_color_gradient(low = "gray", high = "red", limits = c(0, max(final_data$CD4)), oob = scales::squish) +
        labs(x = "UMAP1", y = "UMAP2", color = singleFeature) + 
        coord_fixed(ratio = 1) +
        xlim(x_limits) +
        ylim(y_limits) +
        theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
              panel.background = element_rect(fill = NA), 
              panel.grid = element_blank(),
              axis.title = element_text(size = 14), 
              legend.title = element_text(size = 12, face = "bold"),
              legend.text = element_text(size = 10),
              legend.spacing.x = unit(0.1, 'cm'),
              legend.spacing.y = unit(0.1, 'cm'),
              legend.position = "right",
              legend.key.size = unit(0.5, 'cm'),
              legend.box.margin = margin(2, 2, 2, 2))
      
      # Add the plot to the list
      plot_list[[singleFeature]] <- p
    }
    return(wrap_plots(plot_list, ncol = 4))
  })
}
