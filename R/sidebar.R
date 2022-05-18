invisible(sapply(list.files(
  pattern = 'modules',
  recursive = TRUE,
  full.names = TRUE),
  function(f)
    sys.source(f, env = attach(NULL))))

sidebar <- dashboardSidebar(
  width = 250,
  sidebarMenu(
    id = 'tabs',
    menuItem('Sample Distributions', tabName = 'distrib', icon = icon('paw', 'fa-1x'),
             menuSubItem('Normal', tabName = 'normal'),
             menuSubItem('Uniform', tabName = 'unif')),
    menuItem('Bimodality', tabName = 'bimod', icon = icon('cogs', 'fa-1x')),
    menuItem('Multimodality', tabName = 'multimod', icon = icon('mountain', 'fa-1x')),
    menuItem('Unequal Mode', tabName = 'unequalMode', icon = icon('not-equal', 'fa-1x')),
    menuItem('Unequal Variances', tabName = 'unequalVar', icon = icon('balance-scale-left', 'fa-1x')),
    menuItem('Sample Size Calculations', tabName = 'samplesize', icon = icon('users', 'fa-1x'))
  ) # END sidebarMenu
) # END dashboardSidebar
