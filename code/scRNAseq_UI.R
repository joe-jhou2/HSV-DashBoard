scRNAseq_ui <- function(id, image_prefix = "") {
  ns <- NS(id)
  
  fluidPage(
    useShinyjs(),
    # Link to Custom CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    # TabsetPanel to create scRNAseq and Visium tabs
    tabsetPanel(
      id = "tabs",
      tabPanel("scRNAseq",
               fluidPage(
                 # Dataset and Annotation Selection at the top
                 fluidRow(
                   column(3,
                          selectInput("tab_dataset_option", "Select DataSet: (Step 1)",
                                      choices = c("", "T cell dataset", "Myeloid cell dataset"),
                                      selected = "", multiple = FALSE),
                          actionButton(inputId = "loadDataBtn", label = "Load Data (Step 2)"),
                          textOutput("loadMessage")
                   ),
                   column(3,
                          fluidRow(
                            column(8,
                                   div(class = "annotation-container",
                                       selectInput("tab_annotation_option", "Select Annotation Level: (Step 3)",
                                                   choices = NULL, multiple = FALSE),
                                       div(class = "annotation-description", "(Suggest: CellType_Level3)")
                                   )
                            )
                          )
                   ),
                   column(12,
                          uiOutput("progress_bar")  # Progress bar
                   )
                 ),
                 
                 # Sidebar Layout for clustering and gene discovery inputs
                 sidebarLayout(
                   sidebarPanel(width = 12,
                                tabsetPanel(
                                  # Cluster Discovery Tab
                                  tabPanel("Cluster Discovery", value = 1,
                                           br(),
                                           div(style = "display: inline-block; vertical-align:top; width: 30%;",
                                               selectInput("celltype_tab_celltype_option", "Select Cluster: (Step 4)",
                                                           choices = NULL, selected = "All", multiple = TRUE)),
                                           div(style = "display: inline-block; vertical-align:top; width: 30%;",
                                               selectInput("celltype_tab_subject_option", "Select Subject: (Step 5)",
                                                           choices = NULL, selected = "All", multiple = TRUE)),
                                           div(
                                             tags$span(style = "color: red; font-size: small;", 
                                                       "Warning: Annotation Level required prior to cluster and subject selection.")
                                           ),
                                           br(),
                                           actionButton("run_cluster", "Run Analysis (Step 6)", class = "btn-primary"),
                                           tabsetPanel(
                                             # UMAP Plots
                                             tabPanel("UMAP", value = 2,
                                                      br(),
                                                      fluidRow(
                                                        column(6, plotOutput("UMAP_all", height = "400px", width = "800px"))
                                                      ),
                                                      br(),
                                                      fluidRow(
                                                        column(3, plotOutput("UMAP_Prior",  height = "300px", width = "300px")),
                                                        column(3, plotOutput("UMAP_Lesion", height = "300px", width = "300px")),
                                                        column(3, plotOutput("UMAP_Post",   height = "300px", width = "300px"))
                                                      )
                                             ),
                                             # Cell Type Stats
                                             tabPanel("Cell Type Stat", value = 3,
                                                      br(),
                                                      fluidRow(
                                                        column(12, uiOutput("dynamicBarplot_CellType_Pert")),
                                                        column(12, uiOutput("dynamicBarplot_CellType_Count"))
                                                      )
                                             ),
                                             # Cell Type by Subject
                                             tabPanel("Cell Type by Subject", value = 4,
                                                      br(),
                                                      uiOutput("dynamicBarplot"))
                                           )
                                  ),
                                  
                                  # Gene Discovery Tab
                                  tabPanel("Gene Discovery", value = 2,
                                           br(),
                                           div(style = "display: inline-block; vertical-align:top; width: 30%;",
                                               selectInput("feature_tab_celltype_option", "Select Cluster: (Step 4)",
                                                           choices = NULL, selected = "All", multiple = TRUE)),
                                           div(style = "display: inline-block; vertical-align:top; width: 30%;",
                                               selectInput("feature_tab_subject_option", "Select Subject: (Step 5)",
                                                           choices = NULL, selected = "All", multiple = TRUE)),
                                           div(style = "display: inline-block; vertical-align:top; width: 30%;",
                                               textInput("feature_tab_gene_input", "Select Gene (separated by commas): (Step 6)",
                                                         value = "PTPRC, CD3D, CD4, CD8A, CD14, CD68, GZMB, IFNG")),
                                           div(
                                             tags$span(style = "color: red; font-size: small;", 
                                                       "Warning: Annotation Level required prior to cluster and subject selection.")
                                           ),
                                           br(),
                                           actionButton("run_gene", "Run Analysis (Step 7)", class = "btn-primary"),
                                           tabsetPanel(
                                             # Feature Gene Highlight Plots
                                             tabPanel("Feature Gene Highlight", value = 5,
                                                      br(),
                                                      plotOutput("UMAP", height = "400px", width = "800px"),
                                                      uiOutput("dynamicUMAPfeature")
                                             ),
                                             # Heatmap, Violin, and other plots
                                             tabPanel("Heatmap", value = 6, uiOutput("dynamicHeatmap")),
                                             tabPanel("Violin", value = 7, uiOutput("dynamicViolin")),
                                             tabPanel("FeaturePert", value = 8, uiOutput("dynamicFeaturePert")),
                                             tabPanel("DotPlot",value = 9, uiOutput("dynamicDotPlot"))
                                           )
                                  )
                                )
                   ),
                   mainPanel(width = 12)
                 )
               )
      ),
      
      # Visium Tab
      tabPanel("Visium", value = "Visium", 
               fluidPage(
                 # Add fixed image before the tabs
                 fluidRow(
                   column(12, 
                          div(
                            style = "position: relative; top: 10px; bottom: 50px; width: 100%; text-align: center; z-index: 10;",
                            img(src = paste0(image_prefix, "images/visium.png"), height = "250px", width = "auto")
                          )
                   )
                 ),
                 tags$style(HTML("
                  .tabset-panel {
                    margin-top: 550px;
                  }
                ")),

                 # Subtabs for Gene Spatial Discovery and Cell Type Deconvolution
                 tabsetPanel(
                   # Gene Spatial Discovery Subtab
                   tabPanel("Gene Spatial Discovery",
                            fluidPage(
                              # Input for custom gene name
                              textInput("gene_name_input", "Enter Gene Name(s):", value = "PTPRC, CD3D, CD4, CD8A"),
                              actionButton("run_gene_spatial", "Run Analysis", class = "btn-primary"),
                              
                              # Loading message
                              div(id = "loading", style = "display: none; text-align: center;",
                                  tags$img(src = "loading.gif", height = "100px"), 
                                  tags$p("Analyzing Data... Please wait.")
                              ),
                              
                              uiOutput("dynamic_spatial_plot_ui", style = "width: 100%; height: 600px;") 
                            )
                   ),
                   
                   # Cell Type Deconvolution Subtab
                   tabPanel("Cell Type Deconvolution",
                            fluidPage(
                              actionButton("run_deconvo", "Run Analysis", class = "btn-primary"),
                              
                              # Loading message
                              div(id = "loading", style = "display: none; text-align: center;",
                                  tags$img(src = "loading.gif", height = "100px"),
                                  tags$p("Analyzing Data... Please wait.")
                              ),
                              
                              uiOutput("dynamic_pie_plot_ui", height = "800px")
                            )
                   )
                 )
               )
      )
    )
  )
}
