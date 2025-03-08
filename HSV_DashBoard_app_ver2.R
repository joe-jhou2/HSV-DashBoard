if (Sys.getenv("DOCKER_ENV") == "true") {
  shiny::addResourcePath('www', '/home/shiny/www')
  image_prefix <- "www/"
} else {
  image_prefix <- ""
}

# Source global setup and modules
source("code/global_ver2.R") 

# Source the first page UI and server scripts
source("code/Intro_UI.R")
source("code/Intro_Server.R")

# Source the main page UI and server scripts
source("code/scRNAseq_UI.R")
source("code/scRNAseq_Server.R")

# UI
ui <- fluidPage(
  useShinyjs(),
  uiOutput("page_content")
  )

# Server
server <- function(input, output, session) {
  
  useShinyjs()
  
  page_redirect <- reactiveVal(FALSE)
  
  output$page_content <- renderUI({
    if (!page_redirect()) {
      intro_ui("Intro", image_prefix)
    } else {
      scRNAseq_ui("scRNAseq", image_prefix)
    }
  })
  
  # Call Intro server module
  callModule(intro_server, "Intro", page_redirect = page_redirect)

  # Call main module server when user is authenticated
  observeEvent(page_redirect(), {
    if (page_redirect()) {
      scRNAseq_server(input, output, session)
    }
  })
}

# Create Shiny app
shinyApp(ui = ui, server = server, options = list(host = shiny_host, port = shiny_port))
