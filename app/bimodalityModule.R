invisible(sapply(list.files(
  pattern = '(modules|powerCalcs|texts)',
  recursive = TRUE,
  full.names = TRUE),
  function(f)
    sys.source(f, env = attach(NULL))))

bimodalUI <- function(id) {
  
  ns <- NS(id)
  
  tabItem(tabName = 'bimod',
          fluidRow(column(
            width = 3,
            box(
              width = NULL,
              title = 'Parameters',
              sliderInput(
                ns("n"),
                label = "N per condition",
                min = 10,
                max = 200 ,
                value = c(40, 50),
                step = 5
              ),
              
              sliderInput(
                ns("p_giga1"),
                label = "Frequency Ref in mode 2",
                min = .05,
                max = .95,
                value = .25,
                step = .01
              ),
              
              sliderInput(
                ns("p_giga2"),
                label = "Frequency Exp in mode 2",
                min = .05,
                max = .95,
                value = .5,
                step = .01
              ),
              
              sliderInput(
                ns("ps"),
                label = "Percent shift of mode 2",
                min = 0 ,
                max = 200 ,
                value = 15,
                step = 5
              ),
              
              numericInput(
                ns("CI"),
                label = "CI% on SD of modes",
                min = 0 ,
                max = 99 ,
                value = 0
              ),
              
              selectInput(ns("sel"), 
                          "Reference data:",
                          c("Bodyweight", "Fat NNAT", "Fat Trim"),
                          selected = 'Bodyweight'),
              
              numericInput(
                ns("alpha"),
                label = "Significance level adjusted for multiple testing",
                value = 0.05,
                min = 0.00000001,
                max = 0.1
              ),
              
              sliderInput(
                ns("nsim"),
                label = "Number of simulations",
                min = 20,
                max = 10000,
                value = 20,
                step = 20
              )
            ) # END box
          ), # END column
        
          column(width = 9,
                 box(
                   width = NULL,
                   title = 'Comparing Bimodal Instability',
                   # verbatimTextOutput(ns('description')),
                   plotlyOutput(ns("pplt"))
                 ) # END box
                 ) # END column
          ) # END fluidRow
          ) # END tabItem
}

bimodalServer <- function(input, output, session) {
  
  ss <- reactive({
    pow = pow_cs(
      ns = input$n,
      p1 = input$p_giga1 ,
      p2 = input$p_giga2,
      shift = input$ps,
      sel  = input$sel,
      CI = input$CI,
      alpha = input$alpha,
      nsim = input$nsim
    )

    pps = rmixnorm(20,
                   input$p_giga1,
                   input$p_giga2,
                   input$ps,
                   input$sel,
                   input$CI)
    
    list(pow = pow, main = pps)
  })
  
  output$description <- renderText({ bimodalityText() })
  
  output$pplt <- renderPlotly({
    
    p1 = ggplot(ss()[["pow"]], aes(x = N, y = Power)) + 
      geom_hline(yintercept = 0.8, color = "dodgerblue3") + 
      geom_point(size = 1) + geom_line() + 
      theme_classic(14) +
      ylab("Power") + 
      facet_wrap(~ Test) + 
      xlab("N per genotype or condition") +
      theme(legend.text = element_text(10), legend.title = element_blank())
    fig1 = ggplotly(p1)
    
    p2 = ggplot(ss()[["main"]], aes(x = values, color = Group)) + 
      geom_density() + theme_classic(14) +
      ylab("Measure") + xlab("Group") + 
      theme(legend.text = element_text(10), legend.title = element_blank()) +
      scale_color_manual(values = viridis(3)[1:2])
    fig2 = ggplotly(p2)
    
    subplot(fig2,
            fig1,
            nrows = 2,
            margin = c(0.02, 0.02, .1, .1))
  })
}
  