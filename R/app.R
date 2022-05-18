# dear god don't do this (below) sourcing all the files
#
if (FALSE) {
  
  invisible(sapply(list.files(
    pattern = '(modules|^body|^sidebar|^condlPanel)',
    recursive = TRUE,
    full.names = TRUE),
    function(f)
      sys.source(f, env = attach(NULL))))

  ui <- dashboardPage(
    skin = 'purple',
    header = dashboardHeader(title = 'Simulations'),
    sidebar = sidebarMenuOutput('tabs'),
    body = body,
    controlbar = dashboardControlbar(),
    footer = dashboardFooter()
  )

  server <- function(input, output) {
    
    observeEvent(input$tabs, {
      if(input$tabs == 'normal'){
        callModule(normalServer, 'normalModule')
      }
      
      if(input$tabs == 'bimod'){
        callModule(bimodalServer, 'bimodalModule')
      }
      
      if(input$tabs == 'multimod'){
        callModule(multimodServer, 'multimodModule')
      }
      
      if(input$tabs == 'unequalMode'){
        callModule(unequalModeServer, 'unequalModeModule')
      }
      
      if(input$tabs == 'unequalVar'){
        callModule(unequalVarServer, 'unequalVarModule')
      }
      
      if(input$tabs == 'samplesize'){
        callModule(sampleSizeServer, 'sampleSizeModule')
      }
    })
    
    output$tabs <- renderMenu({ sidebar })
    
  }

  shinyApp(ui, server)

} 
