intro_ui <- function(id, image_prefix = "") {
  ns <- NS(id)
  
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = paste0(image_prefix, "styles.css"))
    ),
    
    # FHRC Logo fixed at top-left corner
    div(class = "FHCRC-image",
        img(src = paste0(image_prefix, "images/FHRC_logo.png"))
    ),
    
    # Title and Description
    div(style = "text-align: center;",
        tags$h1("Welcome to HSV Genital Skin Biopsy scRNAseq Explorer")
    ),
    
    # Add a description
    fluidRow(
      column(12,
             div(
               style = "text-align: center; margin-bottom: 20px;",
               h2("Introduction"),
               p("This study explores the composition of immune cells in genital skin tissue from HSV-infected patients using scRNA-seq data.", style = "font-size: 20px;"),
               p("Our application focuses on examining the dynamics of cell types (clusters) across different disease statuses and visualizing gene expression patterns within each cluster and disease status.",
                 style = "font-size: 20px;")
             )
      )
    ),
    
    # Proceed button
    fluidRow(
      column(12,
             div(
               style = "text-align: center; margin-bottom: 20px;",
               p("Click the button below to proceed to the interactive tool.", style = "font-size: 18px; color: blue;")
             )
      ),
      
      column(12, 
             div(style = "text-align: center; margin-top: 20px;",
                 actionButton(inputId = ns("goToMain"),
                              label = "Proceed to Explorer",
                              class = "btn btn-primary",
                              style = "font-size: 18px; padding: 10px 20px;")
             )
      )
    ),
    
    div(class = "Virus-image",
        img(src = paste0(image_prefix, "images/HSV.png"))
    ),
    
    fluidRow(
      column(12,
          # Main image container occupying the remaining space
          div(class = "WorkFlow-image",
              img(src = paste0(image_prefix, "images/Work_Flow_ver2.png"), height = "auto", width = "120%")
          )
      )
    )
  )
}
