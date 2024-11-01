# Define the UI for the email sign-up module
signup_ui <- function(id, image_prefix = "") {
  ns <- NS(id)

  fluidPage(
    # Link to Custom CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = paste0(image_prefix, "styles.css"))
    ),
    
    # Title and Description
    titlePanel("Welcome to HSV Genital Skin Biopsy scRNAseq Explorer"),
    
    # Add a description
    fluidRow(
      column(12,
             div(
               style = "text-align: center; margin-bottom: 20px;",
               h2("Introduction"),
               p("This study explores the composition of immune cells in genital skin tissue from HSV-infected patients using scRNA-seq data.", style = "font-size: 20px;"),
               p("Our application focuses on examining the dynamics of cell types (clusters) across different disease statuses and visualizing gene expression patterns within each cluster and disease status.",
                 style = "font-size: 20px;"),
               p("Sign in or sign up to access and use this interactive tool.",style = "font-size: 18px; color: blue;")
             )
      )
    ),
    
    # Some images
    div(class = "Virus-image",
        img(src = paste0(image_prefix, "images/HSV.png"))
    ),
    
    # Login or Sign-up Form
    fluidRow(
      column(2,
             wellPanel(
               class = "well-panel",
               textInput(ns("email"), "Email"),
               passwordInput(ns("password"), "Password"),
               actionButton(ns("logIn"), "Log In"),
               actionButton(ns("signUp"), "Sign Up"),
               # Space for displaying messages
               uiOutput(ns("message_login"), style = "color: red;"),
               tags$hr(),
               h4("Reset Password"),
               textInput(ns("reset_email"), "Enter your email address"),
               passwordInput(ns("reset_password"), "Enter your new password"),
               actionButton(ns("reset_password_btn"), "Reset Password")
             )
      ),
      column(10,
          # Main image container occupying the remaining space
          div(class = "WorkFlow-image",
              img(src = paste0(image_prefix, "images/Work_Flow.png"), height = "auto", width = "100%")
          )
      )
    ),
    
    tags$div(
      class = "footer",
      tags$div(
        class = "FHCRC-container",
        tags$div(class = "FHCRC-image",
                 img(src = paste0(image_prefix, "images/FHRC_logo.png"))
        ),
        tags$div(
          class = "footer-text",
          h4("Contributor: Joe Hou", style = "color: lightblue;"),
          h4("Owned by Larry Corey Lab", style = "color: lightblue;")
        )
      )
    ),
    
    uiOutput("mainContent")
  )
}
