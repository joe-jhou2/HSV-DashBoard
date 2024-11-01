#' Visualize Gene Percentage and itensity in Cell Types and Time points
#'
#' @param seurat_object A Seurat object containing single-cell RNA sequencing data.
#' @param selectedCellType A vector of cell types (clusters) to include in the analysis.
#' @param selectedFeature A vector of genes (features) to include in the analysis, typically cytokines.
#' @param CellType_color A named vector of colors corresponding to each cell type for plotting.
#'
#' @return A ggplot object visualizing the percentage and intensity of selected genes 
#' within specified cell types across different statuses/time point.
#'
#' @description This function processes a Seurat object to extract expression data for specified genes and cell types,
#'              calculates the average percentage and average intensity of these genes in the cell types across different conditions (Prior, Lesion, Post),
#'              and visualizes the data using a dot plot. The visualization is faceted
#'              by gene and cell type, showing the variability and mean percentage of gene expression within each cell type across
#'              different statuses.

computeDotPlot = function(seurat_object, selectedCellType, selectedSubject, selectedFeature, selectedAnnotation, CellType_color = NULL){
  # Extracte Cytokine/Gene expression data
  GeneExtraction_df = feature_extraction(seurat_object = seurat_object, 
                                         selectedCellType = "All", 
                                         selectedFeature = selectedFeature,
                                         selectedAnnotation = selectedAnnotation)
  
  # Filtering based on parameters
  GeneExtraction_df_sel = tryCatch({
    GeneExtraction_df %>%
      filter(
        if ("All" %in% selectedCellType) TRUE else CellType %in% selectedCellType,
        if ("All" %in% selectedSubject) TRUE else Subject %in% selectedSubject
      )
  },
  error = function(e) {
    # Handle the error by returning NULL or creating a specific output
    return(NULL)  # Returning NULL to indicate an error occurred during subsetting
  }
  )
  
  if (nrow(GeneExtraction_df_sel) == 0) {
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
    # Calculate Cytokine+ Pert within CellType and Status
    GenePert = calculate_feature_pert(feature_df = GeneExtraction_df_sel, 
                                      selectedFeature = selectedFeature, 
                                      selectedCellType = "All", 
                                      selectedStatus = "All")
    GenePert[is.na(GenePert)] = 0
    
    # Extracte Cytokine/Gene expression data
    Pert_Gene = GenePert %>%
      dplyr::select(Subject, CellType, Status, selectedFeature) %>% 
      group_by(CellType, Status) %>%
      dplyr::filter(if (any(selectedCellType == "All")) TRUE
                    else CellType %in% selectedCellType) %>%
      droplevels() %>%
      pivot_longer(cols = selectedFeature, names_to = "Gene", values_to = "percent") %>%
      dplyr::mutate(Gene = factor(Gene, levels = selectedFeature))
    
    # Calculate Gene Intensity
    Exp_Gene = GeneExtraction_df_sel %>%
      dplyr::select(orig.ident, Subject, CellType, Status, selectedFeature) %>% # , 
      group_by(orig.ident, CellType) %>%
      # summary by subject + status (aka. each sample)
      summarise(across(-c(Subject, Status), \(x) mean(x, na.rm = TRUE))) %>%
      separate(orig.ident, into = c("Subject", "Status"), sep = "_") %>%
      mutate(Status = recode(Status, "Entry" = "Prior", "8WPH" = "Post", "Lesion" = "Lesion")) %>%
      dplyr::filter(if (any(selectedCellType == "All")) TRUE
                    else CellType %in% selectedCellType) %>%
      droplevels() %>% 
      pivot_longer(cols = selectedFeature, names_to = "Gene", values_to = "expression") %>%
      dplyr::mutate(Gene = factor(Gene, levels = selectedFeature))
    
    # Prepare data for plotting
    data.plot1 = Pert_Gene %>% 
      group_by(CellType, Status, Gene) %>%
      summarise(Avg_pert = mean(percent), .groups = 'drop') %>%
      dplyr::mutate(idx = paste(CellType, Status, Gene, sep = "_"))
    
    data.plot2 = Exp_Gene %>%
      group_by(CellType, Status, Gene) %>%
      summarise(Avg_exp = mean(expression), .groups = 'drop') %>%
      dplyr::mutate(idx = paste(CellType, Status, Gene, sep = "_"))
    
    plot.data = data.plot1 %>% left_join(data.plot2[,c("idx", "Avg_exp")], by = "idx") %>%
      dplyr::mutate(Gene = factor(Gene, levels = selectedFeature))
    
    # plotting
    g = ggplot(plot.data, aes(x = Status, y = CellType)) + 
      geom_point(aes(fill = Avg_exp, size = Avg_pert), colour = "black",pch = 21) +
      scale_fill_gradient2(mid = "gray", high = "red") +
      facet_grid2(CellType ~ Gene, scales = "free_y") +
      theme_minimal(base_size = 12) + 
      theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            strip.text = element_text(size = 14),
            strip.text.x = element_text(size = 14, colour = "black", angle = 0),
            strip.text.y = element_text(size = 0, colour = "black", angle = 0),
            axis.text.x = element_text(size = 14, angle = 90, hjust = 1, vjust = 0.5, colour = "black"),
            axis.text.y = element_text(size = 14, colour = "black"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            legend.box = "vertical",
            panel.spacing = unit(0.2, "lines"))  +
      # scale_size_continuous(range = c(1, 10), limits = c(0, 100)) + 
      labs(size = "Percent\nExpressed", fill = "Average\nExpression") +
      guides(size = guide_legend(override.aes = list(shape = 21, colour = "black", fill = "gray")))
    return (g)
  })
}

