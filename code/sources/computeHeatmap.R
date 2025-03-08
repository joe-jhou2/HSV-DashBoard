
#' Compute and Plot a Heatmap of Gene Expression from a Seurat Object
#'
#' This function generates a heatmap visualizing the expression levels of selected features
#' across specified clusters and statuses in a Seurat object.
#'
#' @param seurat_object A Seurat object containing single-cell RNA-seq data.
#' @param selectedCellType Optional; a character vector of cluster identifiers to include
#'        in the analysis. If NULL, all clusters are included.
#' @param selectedStatus Optional; a character vector of statuses to include in the analysis.
#'        If NULL, all statuses are included.
#' @param selectedFeature Optional; a character vector of gene features to plot.
#'        If NULL, no features are plotted.
#' @param CellType_color 
#'
#' @return A ComplexHeatmap object representing the heatmap, which is also drawn to the current
#'         graphics device. The heatmap shows expression levels of selected features, grouped
#'         by cell type and status.
#'
#' @importFrom ComplexHeatmap Heatmap
#' @importFrom ComplexHeatmap HeatmapAnnotation
#' @importFrom circlize colorRamp2
#' @import reshape2
#' 

# Define a function to assign colors based on the number of subjects
assign_colors <- function(subjects) {
  n_subjects <- length(subjects)
  
  # Find the largest palette available in RColorBrewer
  max_colors <- brewer.pal.info[,"maxcolors"]
  available_palettes <- names(max_colors[max_colors >= n_subjects])
  
  # If there are palettes that support the number of subjects, choose the first one
  if (length(available_palettes) > 0) {
    palette_name <- available_palettes[1]  # Choose the first suitable palette
    colors <- brewer.pal(n_subjects, palette_name)
  } else {
    # If no single palette can handle all subjects, create a custom color ramp
    colors <- colorRampPalette(brewer.pal(max(max_colors), "Set3"))(n_subjects)
  }
  # Assign colors to subjects
  Subject_colors <- setNames(colors, subjects)
  return(Subject_colors)
}

computeHeatmap = function(seurat_object, selectedCellType, selectedStatus, selectedSubject, selectedFeature, selectedAnnotation, CellType_color = NULL, feature_data = NULL) {
  start_time <- Sys.time()
  
  # Use the precomputed feature extraction data if provided
  if(is.null(feature_data)){
    feature_data <- feature_extraction_result()  # Get from centralized feature extraction
  }
  
  # Filtering based on parameters
  feature_data_filter = tryCatch({
    feature_data %>%
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
  
  if (nrow(feature_data_filter) == 0) {
    # Return a message or a blank plot
    return(ggplot() + 
             geom_blank() +
             annotate("text", x = 0.25, y = 0.25, 
                      label = "Data\nUnavailable", 
                      size = 6, hjust = 0.5, vjust = 0.5) +
             theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
                   panel.background = element_rect(fill = NA), 
                   panel.grid = element_blank(),
                   axis.text = element_blank(),
                   axis.ticks = element_blank(),
                   axis.title = element_blank(), 
                   legend.title = element_blank())
    )
  }
  
  suppressWarnings({
    
    # Prepare matrix for heatmap
    ht_data = as.matrix(t(feature_data_filter[, !(names(feature_data_filter) %in% c("orig.ident", "Subject", "CellType", "Status"))]))
    
    # Prepare annotation for heatmap
    ht_meta = feature_data_filter[, c("CellType", "Status", "Subject"), drop = FALSE]
    # ht_meta$CellType = factor(ht_meta$CellType, levels = levels(seurat_object))
    
    # re-order columns by celltype --> status
    column_order = order(feature_data_filter$CellType, feature_data_filter$Status, feature_data_filter$Subject)
    
    # Create column annotation for CellType
    anno_celltype_status = HeatmapAnnotation(CellType = ht_meta$CellType,
                                             Status = ht_meta$Status,
                                             Subject = ht_meta$Subject,
                                             annotation_name_gp = gpar(fontsize = 14), 
                                             border = TRUE,
                                             simple_anno_size = unit(0.65, "cm"), 
                                             annotation_legend_param = list(title_gp = gpar(fontsize = 14), 
                                                                            labels_gp = gpar(fontsize = 14),
                                                                            nrow = 3, by_row = FALSE, direction = "horizontal"),
                                             col = list(CellType = CellType_color,
                                                        Status = c("Prior" = "#0F9D58", 
                                                                   "Lesion" = "#DB4437",  
                                                                   "Post" = "#F4B400"),
                                                        Subject = assign_colors(unique(ht_meta$Subject)) ))
    
    # Create a color mapping for the heatmap
    c(min(ht_data), max(ht_data))
    col_fun = colorRamp2(c(0, 0.01, 7), c("gray","blue", "red"))
    
    ht_opt$message = FALSE
    
    # Generate the heatmap with column annotations
    htmp = Heatmap(ht_data, col = col_fun, row_names_side = "right", 
                   cluster_rows = FALSE, cluster_columns = FALSE, 
                   show_column_names = FALSE, column_order = column_order, 
                   top_annotation = anno_celltype_status,
                   row_split = seq(1, nrow(ht_data)),
                   column_title_gp = gpar(fontsize = 0), 
                   column_split = ht_meta$CellType, 
                   column_title_rot = 45, 
                   row_names_gp = gpar(fontsize = 16),
                   row_title = NULL,
                   row_gap = unit(1, "mm"),
                   border = TRUE, 
                   heatmap_legend_param = list(title = "Expression\nLevel", title_position = "topcenter", direction = "horizontal", 
                                               title_gp = gpar(col = "red", fontsize = 14), 
                                               labels_gp = gpar(col = "black", fontsize = 14)), use_raster = TRUE)
    
    plot = draw(htmp,merge_legend = TRUE, heatmap_legend_side = "bottom", annotation_legend_side = "bottom")
    
    # End timing
    end_time <- Sys.time()
    cat("Heatmap Computation time: ", end_time - start_time, "\n")
    
    return(plot)
  })
}
