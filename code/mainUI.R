
# Define UI
main_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    
    useShinyjs(),  # Initialize shinyjs for enabling modal
    
  # Link to Custom CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  # Add the sign-out button at the top right corner
  tags$div(style = "position: absolute; top: 10px; right: 10px;",
           actionButton("signOutBtn", "Sign Out")
  ),
  
  # Title Panel
  titlePanel("Interactive HSV+ Skin Biopsy scRNAseq Data Visualization"),
  
  # Dataset and Annotation Selection at the top
  fluidRow(
    column(3,
      selectInput("tab_dataset_option", "Select DataSet:",
                  choices = c("", "T cell dataset", "Myeloid cell dataset"),
                  selected = "", multiple = FALSE),
      # Load Data button directly below Select DataSet
      actionButton(inputId = "loadDataBtn", label = "Load Data"),
      
      # actionButton(inputId = "clearDataBtn", label = "Clear Data"),
      
      textOutput("loadMessage")
    ),
    column(3,
      # Select Annotation Level input and the dynamic notice
      fluidRow(
        column(8,
          div(class = "annotation-container",
            selectInput("tab_annotation_option", "Select Annotation Level:",
                        choices = NULL, multiple = FALSE),
            div(class = "annotation-description", "(Suggest: CellType_Level3)")
          )
        ),
      )
    ),
    # Add progress bar under the dataset selection
    column(12,
      # uiOutput("annotationNotice"),  # Add UI output for annotation notice
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
              selectInput("celltype_tab_celltype_option", "Select Cluster :",
                          choices = NULL, selected = "All", multiple = TRUE)),
          div(style = "display: inline-block; vertical-align:top; width: 30%;",
              selectInput("celltype_tab_subject_option", "Select Subject :",
                          choices = NULL, selected = "All", multiple = TRUE)),
          # Always show the annotation level notice here
          div(
            tags$span(style = "color: red; font-size: small;", 
                      "Warning: Annotation Level required prior to cluster and subject selection.")
          ),
          br(),
          actionButton("run_cluster", "Run Analysis", class = "btn-primary"),
          tabsetPanel(
            # UMAP Plots
            tabPanel("UMAP", value = 2,
              br(),
              fluidRow(
                column(6,
                  plotOutput("UMAP_all", height = "400px", width = "800px" )
                )
              ),
              br(),
              fluidRow(
                column(3,
                  plotOutput("UMAP_Prior",  height = "300px", width = "300px")
                ),
                column(3,
                  plotOutput("UMAP_Lesion", height = "300px", width = "300px")
                ),
                column(3,
                  plotOutput("UMAP_Post",   height = "300px", width = "300px")
                )
              )
            ),
            # Cell Type Stats
            tabPanel("Cell Type Stat", value = 3,
              br(),
              fluidRow(
                column(12,
                  uiOutput("dynamicBarplot_CellType_Pert")
                ),
                column(12,
                  uiOutput("dynamicBarplot_CellType_Count")
                )
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
              selectInput("feature_tab_celltype_option", "Select Cluster:",
                          choices = NULL, selected = "All", multiple = TRUE)),
          div(style = "display: inline-block; vertical-align:top; width: 30%;",
              selectInput("feature_tab_subject_option", "Select Subject:",
                          choices = NULL, selected = "All", multiple = TRUE)),
          div(style = "display: inline-block; vertical-align:top; width: 30%;",
              textInput("feature_tab_gene_input", "Select Gene (separated by commas):",
                        value = "PTPRC, CD3D, CD4, CD8A, CD14, CD68, GZMB, IFNG")),
          # Always show the annotation level notice here
          div(
            tags$span(style = "color: red; font-size: small;", 
                      "Warning: Annotation Level required prior to cluster and subject selection.")
          ),
          br(),
          actionButton("run_gene", "Run Analysis", class = "btn-primary"),
          tabsetPanel(
            # Feature Gene Highlight Plots
            tabPanel("Feature Gene Highlight", value = 5,
                     br(),
                     plotOutput("UMAP", height = "400px", width = "800px"),
                     uiOutput("dynamicUMAPfeature"),
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
    mainPanel(width = 12)  # Add content or plots here if needed
  )
)
}
