invisible(sapply(list.files(
  pattern = '(modules|powerCalcs)',
  recursive = TRUE,
  full.names = TRUE),
  function(f)
    sys.source(f, env = attach(NULL))))

multimodUI <- function(id) {
  
  ns <- NS(id)
  
  tabItem(tabName = 'multimod',
          fluidRow(column(
            width = 3,
            box(
              width = NULL,
              title = 'Parameters',
              sliderInput(ns("n1"), 
                          label = "Number of Samples in Mode 1", min = 3, max = 100, value = 16, step = 1),
            
              sliderInput(ns("n2"), 
                          label = "Number of Samples in Mode 2", min = 3, max = 100, value = 16, step = 1),
              
              numericInput(ns("alpha"), 
                           label="Significance level adjusted for multiple testing", value=0.05, min = 0.00000001, max = 0.1),
              
              sliderInput(ns("nsim"), 
                          label = "Number of simulations", min = 20, max = 1000, value = 20, step = 20)
            )),
            column(width = 9,
                   box(width = NULL,
                       title = 'Multimodality: Power curves for testing if there is more than 1 peak',
                       plotlyOutput(ns('pplt'))
                       ))
          ))
}

multimodServer <- function(input, output, session) {
  
  ss <-
    reactive({
      find_dist(input$n1, input$n2, input$alpha, input$nsim)
    })
  
  output$pplt <- renderPlotly({
    ggplotly(
      ggplot(ss(), aes(x = Distance, y = power)) + 
        geom_hline(yintercept = 0.8, color = "dodgerblue3") + 
        geom_point(size = 1) + geom_line() + theme_classic(14) +
        ylab("Power") + xlab("Standardized distance between modes") + 
        theme(legend.text = element_text(10), legend.title = element_blank())
    )
  })
  
}