invisible(sapply(list.files(
  pattern = '(modules|normalModule|bimod)',
  recursive = TRUE,
  full.names = TRUE),
  function(f)
    sys.source(f, env = attach(NULL))))

condlPanelUI <- function(id) {
  ns <- NS(id)
  
  tagList(conditionalPanel(condition = 'input.tabs == "normal"',
                           normalUI(ns('normalPanel'))),
          conditionalPanel(condition = 'input.tabs == "bimod"',
                           bimodalUI(ns('bimodalPanel'))))
  
}

condlPanelServer <- function(input, output, session) {
  
  output$outputPanel <- renderUI({
    ns <- session$ns
  })
}