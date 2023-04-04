#' bifurcatoR_Analysis
#'
#' @param   data    data frame with 2 columns, a two-level group column and numeric value column
#' @param   alpha   default significance level (0.05)
#' @param   nboot   number of bootstraps or permutations
#' @param   tests   names of tests to run
#'
#' @return          a data frame where each row corresponds to the results of test, p-values, test stats, and confidence intervals where possible
#'
#' @import          mclust
#' @import          diptest
#' @import          mousetrap
#' @import          LaplacesDemon
#' @import          multimode
#' @import          Hmisc
#' @import          twosamples
#'
#' @export


bifurcatoR_Analysis = function(data,tests,nboot,alpha){
  res <- data.frame(Test = character(),
                    nboot = numeric(),
                    p.value = numeric(),
                    Stat = numeric(),
                    CI = numeric()
  )

  if("mclust" %in% tests){
    tmp = mclust::mclustBootstrapLRT(data$value,modelName="E",verbose=F,maxG=1,nboot=nboot)
    res = rbind(res,data.frame(Test = "Mclust", nboot = nboot,p.value = tmp$p.value,Stat = tmp$obs ,CI = paste(round(quantile(tmp$boot,p=c(alpha/2,1-alpha/2)),floor(log10(nboot)) + 1),collapse=", " )))
  }

  if("mt" %in% tests){
    tmp = mousetrap::mt_check_bimodality(as.data.frame(data$value),t,method="BC")$BC

    tmp.boot = unname(unlist(lapply(1:nboot, function(x) mousetrap::mt_check_bimodality(as.data.frame(sample(s,replace=T)),t,method="BC")$BC)))

    res = rbind(res,data.frame(Test = "Bimodality Coeficient", nboot = nboot,p.value =  sum(I(tmp.boot<=0.555))/nboot,Stat = unname(tmp) ,CI = paste(round(quantile(tmp.boot,p=c(alpha/2,1-alpha/2)),floor(log10(nboot)) + 1),collapse=", " )))

  }

  if("SI" %in% tests){
    tmp = multimode::modetest(data$value,mod0 = 1,method = "SI",B=nboot)

    res = rbind(res,data.frame(Test = tmp$method, nboot = nboot,p.value = tmp$p.value,Stat = unname(tmp$statistic) ,CI = "Not yet available"))

  }

  if("dip" %in% tests){
    tmp = multimode::modetest(data$value,mod0 = 1,method = "HH",B=nboot)

    res = rbind(res,data.frame(Test = tmp$method, nboot = nboot,p.value = tmp$p.value,Stat = unname(tmp$statistic) ,CI = "Not yet available"))

  }

  if("HY" %in% tests){
    tmp = multimode::modetest(data$value,mod0 = 1,method = "HY",B=nboot)

    res = rbind(res,data.frame(Test = tmp$method, nboot = nboot,p.value = tmp$p.value,Stat = unname(tmp$statistic) ,CI = "Not yet available"))

  }

  if("CH" %in% tests){
    tmp = multimode::modetest(data$value,mod0 = 1,method = "CH",B=nboot)

    res = rbind(res,data.frame(Test = tmp$method, nboot = nboot,p.value = tmp$p.value,Stat = unname(tmp$statistic) ,CI = "Not yet available"))

  }

  if("ACR" %in% tests){
    tmp = multimode::modetest(data$value,mod0 = 1,method = "ACR",B=nboot)

    res = rbind(res,data.frame(Test = tmp$method, nboot = nboot,p.value = tmp$p.value,Stat = unname(tmp$statistic) ,CI = "Not yet available"))

  }

  if("FM" %in% tests){
    tmp = multimode::modetest(data$value,mod0 = 1,method = "FM",B=nboot)

    res = rbind(res,data.frame(Test = tmp$method, nboot = nboot,p.value = tmp$p.value,Stat = unname(tmp$statistic) ,CI = "Not yet available"))

  }

  if("ks" %in% tests){

    tmp = twosamples::ks_test(data$value[data$group == unique(data$group)[1]] ,data$value[data$value == unique(data$value)[2]],nboots=nboot,keep.boots=T)

    res = rbind(res,data.frame(Test = "Kolmogorov-Smirnov Test", nboot = nboot,p.value = tmp[[2]],Stat = tmp[[1]] ,CI = paste(round(quantile(attributes(tmp)$bootstraps,p=c(alpha/2,1-alpha/2)),floor(log10(nboot)) + 1),collapse=", " )))


  }


  if("cvm" %in% tests){


    tmp = twosamples::cvm_test(data$value[data$group == unique(data$group)[1]] ,data$value[data$value == unique(data$value)[2]],nboots=nboot,keep.boots=T)

    res = rbind(res,data.frame(Test = "Cramer-von Mises Test", nboot = nboot,p.value = tmp[[2]],Stat = tmp[[1]] ,CI = paste(round(quantile(attributes(tmp)$bootstraps,p=c(alpha/2,1-alpha/2)),floor(log10(nboot)) + 1),collapse=", " )))

  }


  if("dts" %in% tests){

    tmp = twosamples::dts_test(data$value[data$group == unique(data$group)[1]] ,data$value[data$value == unique(data$value)[2]],nboots=nboot,keep.boots=T)

    res = rbind(res,data.frame(Test = "DTS Test", nboot = nboot,p.value = tmp[[2]],Stat = tmp[[1]] ,CI = paste(round(quantile(attributes(tmp)$bootstraps,p=c(alpha/2,1-alpha/2)),floor(log10(nboot)) + 1),collapse=", " )))

  }

  if("ad" %in% tests){

    tmp = twosamples::ad_test(data$value[data$group == unique(data$group)[1]] ,data$value[data$value == unique(data$value)[2]],nboots=nboot,keep.boots=T)

    res = rbind(res,data.frame(Test = "Aderson-Darling Test", nboot = nboot,p.value = tmp[[2]],Stat = tmp[[1]] ,CI = paste(round(quantile(attributes(tmp)$bootstraps,p=c(alpha/2,1-alpha/2)),floor(log10(nboot)) + 1),collapse=", " )))

  }

  if("Levene" %in% tests){

    tmp = car::leveneTest(lm(value~as.factor(data$group),data=data))

    res = rbind(res,data.frame(Test = "Levene's Test", nboot = NA ,p.value =tmp$'Pr(>F)',Stat = tmp$'F value' ,CI = "Not yet available"))

  }



  if ("Permutations (Raw)" %in% tests) {

    temp_df = data.frame(
      y = data$value,
      X= as.numeric(as.factor(data$group))-1
    )

    tmp = permutation_tests(temp_df,nboot,"meanDiff",alpha)

    res = rbind(res,data.frame(Test = "Permutations (Raw)", nboot = nboot ,p.value = tmp$p ,Stat = tmp$diff ,CI = paste(round(quantile(tmp$crit,p=c(alpha/2,1-alpha/2)),floor(log10(nboot)) + 1),collapse=", " )))

  }

  if ("Permutations (SD)" %in% tests) {

    temp_df = data.frame(
      y = data$value,
      X= as.numeric(as.factor(data$group))-1
    )

    tmp = permutation_tests(temp_df,nboot,"sdDiff",alpha)

    res = rbind(res,data.frame(Test = "Permutations (SD)", nboot = nboot ,p.value = tmp$p ,Stat = tmp$diff ,CI = paste(round(quantile(tmp$crit,p=c(alpha/2,1-alpha/2)),floor(log10(nboot)) + 1),collapse=", " )))

  }

  if ("Permutations (MAD)" %in% tests) {

    temp_df = data.frame(
      y = data$value,
      X= as.numeric(as.factor(data$group))-1
    )

    tmp = permutation_tests(temp_df,nboot,"madDiff",alpha)

    res = rbind(res,data.frame(Test = "Permutations (MAD)", nboot = nboot ,p.value = tmp$p ,Stat = tmp$diff ,CI = paste(round(quantile(tmp$crit,p=c(alpha/2,1-alpha/2)),floor(log10(nboot)) + 1),collapse=", " )))

  }


  if ("Permutations (GiniMd)" %in% tests) {

    temp_df = data.frame(
      y = data$value,
      X= as.numeric(as.factor(data$group))-1
    )

    tmp = permutation_tests(temp_df,nboot,"giniDiff",alpha)

    res = rbind(res,data.frame(Test = "Permutations (GiniMd)", nboot = nboot ,p.value = tmp$p ,Stat = tmp$diff ,CI = paste(round(quantile(tmp$crit,p=c(alpha/2,1-alpha/2)),floor(log10(nboot)) + 1),collapse=", " )))

  }



  if("ANOVA" %in% tests){

    tmp = lm(value~as.factor(data$group),data=data)

    res = rbind(res,data.frame(Test = "ANOVA", nboot = NA ,p.value = summary(tmp)$coefficients[2,4] ,Stat = tmp$coefficients[2] ,CI = paste(round(confint(tmp)[2,],floor(log10(nboot)) + 1),collapse=", " )))

  }


  if("Non-parametric ANOVA" %in% tests){

    tmp = lm(rank(value)~as.factor(data$group),data=data)

    res = rbind(res,data.frame(Test = "Non-parametric ANOVA", nboot = NA ,p.value = summary(tmp)$coefficients[2,4] ,Stat = tmp$coefficients[2] ,CI = paste(round(confint(tmp)[2,],floor(log10(nboot)) + 1),collapse=", " )))


  }

  return(res)
}



mod_meanvarModule_ui <- function(id) {
  ns <- shiny::NS(id)

  shinydashboard::tabItem(tabName = 'Analysis',
                          shiny::fluidRow(
                            column(
                              width = 3,
                              shinydashboard::box(
                                width = NULL,
                                title = 'Parameters',

                                fileInput("Dataset","Please input a two column csv file (One column 'group' names, second column 'value'"),

                                numericInput("Alpha",
                                             label = "Significance Level (adjusted for multiple testing)",min = 0.0000000001, max = 0.999, value =0.05),


                                shiny::numericInput(
                                  inputId = ns('nboot'),
                                  label = 'Number of Permutation or Bootstrap Resamples',
                                  min = 10,
                                  max = 5000,
                                  value = 100
                                ),

                                shinyWidgets::pickerInput(
                                  inputId = ns('effect'),
                                  label = 'Select Effect:',
                                  choices = c('Mean', 'Variance','Bimodality', 'Mean-Variance'),
                                  selected = 'Mean'
                                ),

                                shiny::conditionalPanel(
                                  condition = sprintf('input["%s"] == "Mean-Variance"', ns("effect")),
                                  shiny::checkboxGroupInput(
                                    inputId = ns('meanvar'),
                                    label = 'Select Test(s):',
                                    choices = c('Anderson-Darling' = "ad",
                                                'Kolmogorov–Smirnov' = "ks",
                                                'Cramer-Von Mises' = "cvm",
                                                'DTS' = "dts"),
                                    selected = 'Kolmogorov–Smirnov'
                                  )
                                ),

                                shiny::conditionalPanel(
                                  condition = sprintf('input["%s"] == "Mean"', ns("effect")),
                                  shiny::checkboxGroupInput(
                                    inputId = ns('meaneff'),
                                    label = 'Mean Methods',
                                    choices = c('ANOVA',
                                                'Non-parametric ANOVA',
                                                'Permutations (Raw)'),
                                    selected = 'ANOVA'
                                  )
                                ),

                                shiny::conditionalPanel(
                                  condition = sprintf('input["%s"] == "Variance"', ns("effect")),
                                  shiny::checkboxGroupInput(
                                    inputId = ns('vareff'),
                                    label = 'Variance Methods',
                                    choices = c('Levene',
                                                'Permutations (MAD)',
                                                'Permutations (Gini Index)',
                                                'Permutations (SD)'),
                                    selected = 'Levene'
                                  )
                                ),

                                shiny::conditionalPanel(
                                  condition = sprintf('input["%s"] == "Bimodality"', ns("effect")),
                                  shiny::checkboxGroupInput(
                                    inputId = ns('bimode'),
                                    label = ("Bimodality Tests"),
                                    choices = list(
                                      "Hartigans dip test" = "dip",
                                      "Mclust" = "mclust",
                                      "Bimodality Coefficient" = "mt",
                                      "Silverman Bandwidth test" = "SI",
                                      "Hall and York Bandwidth test" = "HY",
                                      "Cheng and Hall Excess Mass" = "CH",
                                      "Ameijeiras-Alonso et al. Excess Mass" = "ACR",
                                      "Fisher and Marron Carmer-von Mises" = "FM"),
                                    selected = "dip")
                                )
                              )
                            ),

                            shiny::column(
                              width = 9,
                              shinydashboard::box(width = NULL,
                                                  title = 'Testing Mean or Variance Effects',
                                                  verbatimTextOutput(ns('description')),
                                                  plotly::plotlyOutput(ns("pplt")),
                                                  DT::dataTableOutput(ns("resTable")),
                                                  shiny::textOutput(ns("testPrint"))
                              ) # END box
                            ) # END column
                          ) # END fluidRow
  ) # END tabItem
}


mod_meanvarModule_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {

                 vals <- reactiveValues(upld.file = NULL)

                 observeEvent(input$annotation,{
                   file <- input$annotation
                   upld.file <- read.csv(file$datapath)
                   vals$upld.file <- upld.file

                   output$pplt <- plotly::renderPlotly({
                     p1 = plotly::ggplotly(
                       ggplot2::ggplot() +
                         ggplot2::geom_density(data = upld.file, ggplot2::aes(x = value, color = group)) +
                         ggplot2::theme_classic(14) +
                         ggplot2::ylab("Population density") +
                         ggplot2::theme(legend.title = ggplot2::element_blank()) +
                         ggplot2::scale_color_manual(values =
                                                       c("black", "blue"))
                     )

                     p2 = plotly::ggplotly(
                       ggplot2::ggplot() +
                         ggplot2::geom_boxplot(data = upld.file, ggplot2::aes(x = group, y= value, color = group),outlier.size = -1) +
                         ggplot2::geom_jitter(data = upld.file, ggplot2::aes(x = group, y= value, color = group),height=0,width=.25) +
                         ggplot2::theme_classic(14) +
                         ggplot2::ylab("Population density") +
                         ggplot2::theme(legend.title = ggplot2::element_blank()) +
                         ggplot2::scale_color_manual(values =
                                                       c("black", "blue"))
                     )


                     fig1 = plotly::ggplotly(p1)

                     fig2 = plotly::ggplotly(p2)

                     plotly::subplot(fig1,
                                     fig2,
                                     nrows = 1,
                                     margin = c(0.02, 0.02, .21, .21))


                   })

                   # Initialize table of parameters
                   init_tbl <- data.frame(
                     Test = character(),
                     nboot = numeric(),
                     p.value = numeric(),
                     Stat = numeric()
                   )
                 })

                 observeEvent(input$analysisButton,{

                   ss = Analyze_Data(data = vals$upld.file, c(input$vareff,input$bimode,input$meanvar,input$vareff),input$nboot,input$alpha)
                   rownames(ss) = NULL
                   paramsTable <- shiny::reactive({
                     calcOutput <- ss
                     # calcOutput
                     tbl_row <- nrow(calcOutput)
                     tbl <- ss
                     tbl <- tbl[order(tbl$Test),]
                     rbind(init_tbl, tbl)
                     # })

                   })

                   output$testPrint <- shiny::renderPrint( c(input$meaneff, input$vareff) ) # ss()[["calcs"]]

                   output$paramsTable <- DT::renderDataTable( paramsTable() )

                   output$downloadParams <- shiny::downloadHandler(

                     filename = function() {
                       paste0(input$filename, ".csv")
                     },

                     content = function(file) {
                       write.csv(paramsTable(), file, row.names = FALSE)
                     }
                   )

                 })
               })
}
