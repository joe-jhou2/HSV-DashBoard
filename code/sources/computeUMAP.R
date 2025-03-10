#' Computes UMAP and Generates a Plot for a Seurat Object
#'
#' This function calculates the UMAP plot for cells in a Seurat object based on 
#' optional filtering criteria (clusters, status, and subjects). It automatically 
#' determines scale limits for the UMAP plot and adjusts the plotting area accordingly. 
#' If no cells meet the criteria, it returns a blank plot with a notification.
#'
#' @param seurat_object A Seurat object containing single-cell RNA-seq data with UMAP 
#'        reductions already calculated.
#' @param selectedCellType (Optional) A character vector specifying the clusters to 
#'        include in the plot. If 'All' is included or NULL is passed, no cluster-based 
#'        filtering is applied.
#' @param selectedStatus (Optional) A character vector specifying the cell statuses 
#'        to include in the plot. Similar to clusters, if 'All' is included or NULL is 
#'        passed, no status-based filtering is applied.
#' @param selectedSubject (Optional) A character vector specifying the subjects 
#'        to include in the plot. If 'All' is included or NULL is passed, no subject-based 
#'        filtering is applied.
#' @param CellType_color 
#'
#' @return A ggplot object containing the UMAP plot with the specified cells highlighted.
#'         If no cells are found after applying the filters, returns a blank plot with 
#'         a message "No cells found".
#'

computeUMAP = function(seurat_object, selectedCellType, selectedStatus, selectedSubject, selectedAnnotation, CellType_color = NULL, selectedFeature = NULL, feature_data = NULL) {
  start_time <- Sys.time()
  
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
  
  # Merge UMAP coordinates with metadata
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
    
    plot = ggplot(data = UMAP_merge, aes(x = UMAP1, y = UMAP2)) +
      geom_point(aes(color = get(selectedAnnotation)),
                 size = 0.4, 
                 alpha = 0.8) +
      scale_color_manual(selectedAnnotation, values = CellType_color) +
      scale_fill_manual(values = CellType_color) +
      labs(x = "UMAP1", y = "UMAP2") + 
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
            legend.box.margin = margin(2, 2, 2, 2)) +
      guides(colour = guide_legend(override.aes = list(size = 3),
                                   title.hjust = 0.5))
             
      # Check if selectedFeature is NULL
      if (is.null(selectedFeature)) {
        end_time <- Sys.time()
        cat("UMAP Computation time: ", end_time - start_time, "\n")
        return(list(plot = plot, plot_feature = NULL))
      }
      
      # Process selected features
      feature_data_filter <- feature_data %>% filter(
        if ("All" %in% selectedCellType) TRUE else CellType %in% selectedCellType,
        if ("All" %in% selectedStatus) TRUE else Status %in% selectedStatus,
        if ("All" %in% selectedSubject) TRUE else Subject %in% selectedSubject
      )
    
      feature_data_filter$Barcode = rownames(feature_data_filter)
      
      final_data = left_join(UMAP_merge, 
                             feature_data_filter[,!colnames(feature_data_filter) %in% c("orig.ident", "Subject", "Status", "CellType")], 
                             by = c("Barcode" = "Barcode"))
      
      # Create plots in parallel (for improved speed)
      plot_list <- furrr::future_map(selectedFeature, function(singleFeature) {
        ggplot(data = final_data, aes(x = UMAP1, y = UMAP2)) +
          geom_density_2d(stat = "density_2d", aes(color = ..level..), size = 0.5, bins = 30) +
          geom_point(data = final_data[final_data[[singleFeature]] > 0,],  # Highlight red dots
                     aes(color = .data[[singleFeature]]), size = 0.4, alpha = 1) +
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
      })
      
      # Combine the plots using wrap_plots
      plot_combined <- patchwork::wrap_plots(plot_list, ncol = 4)
      end_time <- Sys.time()
      cat("UMAP Computation time: ", end_time - start_time, "\n")
      return(list(plot = plot, plot_feature = plot_combined))
  })
}
