invisible(sapply(list.files(
  pattern = '(modules|Module|^condlPanel)',
  recursive = TRUE,
  full.names = TRUE),
  function(f)
    sys.source(f, env = attach(NULL))))

body <- dashboardBody(
  
  tags$head(
    tags$link(rel = 'stylesheet', type = 'text/css', href = '~/TR01/style.css')
  ),
  
  tabItems(
    tabItem(tabName = 'normal',
            h2('Simulations under Normal Distribution'),
            normalUI('normalModule')),
    
    tabItem(tabName = 'unif',
            h2('Simulations under Uniform Distribution')),
    
    tabItem(tabName = 'bimod',
            h2('Bimodal Instability'),
            bimodalUI('bimodalModule')),
    
    tabItem(tabName = 'multimod',
            h2('Multimodal: TBD'),
            multimodUI('multimodModule')),
    
    tabItem(tabName = 'unequalMode',
            h2('Power Testing for Unequal Mode Split'),
            unequalModeUI('unequalModeModule')),
    
    tabItem(tabName = 'unequalVar',
            h2('Unequal Variances: TBD'),
            unequalVarUI('unequalVarModule')),
    
    tabItem(tabName = 'samplesize',
            h2('Sample Size Calculations: TBD'),
            sampleSizeUI('sampleSizeModule'))
  )
) # END dashboardBody