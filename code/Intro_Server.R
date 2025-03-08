
intro_server <- function(input, output, session, page_redirect) {
  observeEvent(input$goToMain, {
    # print(paste("Button clicked - Current input value:", input$goToMain))
    page_redirect(TRUE)
    # print(paste("page_redirect updated to:", page_redirect()))
  },ignoreInit = TRUE) 
}