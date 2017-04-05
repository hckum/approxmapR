library(shiny)
library(shinydashboard)
# library(devtools)
# install_github("ilangurudev/ApproxMapSeq")
# library(ApproxMapSeq)
library(dplyr)

# source("./Helpers/Helpers.R")
# source("./Helpers/helpers_p.R")
# source("./Helpers/1_Preprocessing.R")
# source("./Helpers/2_calculate_dist_and_cost.R")
# source("./Helpers/3_alignments.R")
# source("./Helpers/4_clustering.R")
# source("./Helpers/5.1_formatting_output.R")
# source("./Helpers/5_ApproxMap.R")


server <- function(input, output, session) {

  output$text1 <- renderText({
    "Start day is predefined (First day of every calender month)"
  })

  output$text2 <- renderText({
    "Start day is predefined (Jan 1, Apr 1, July 1, Oct 1)"
  })

  data_uploadedL = isolate(input$inp_dataL)



  data_uploaded = reactive(
    {
      inp <- input$inp_data
      #if(is.null(inp)) return(read.csv("./data/demo1.csv")) #for demo, otherwise NULL
      if(is.null(inp)) return(NULL)
      else return(read.csv(inp$datapath, stringsAsFactors = F))

    }
  )

  output$contents <- renderTable({
    if(is.null(data_uploaded())) {
      return("Upload data to view here")
    } else {
      return((data_uploaded()))
    }
    })

  output$processedOut <- renderPrint(
    {
      if(!is.null(data_uploaded()))
        return(str(data_uploaded()))
    }
  )

  ord_date_table <- reactive({
    if(is.null(data_uploaded()))
    {
      return("")
    } else {

      if(input$period1 == "1 Week") {
        return(ord_date(data_uploaded(),
                        get.pd(input$period1),
                        get.st.date(input$week_st)))
      } else if(input$period1 == "6 Months") {
       return(ord_date(data_uploaded(),
                        get.pd(input$period1),
                        get.st.date(input$month6_st)))
      } else if(input$period1 == "1 Year") {
        return(ord_date(data_uploaded(),
                        get.pd(input$period1),
                        get.st.date(input$year_st)))
      } else {
        return(ord_date(data_uploaded(),
                        get.pd(input$period1)))
      }

    }
  })

  # output$date_table = renderTable(ord_date_table())


  data_tree = reactive(
    {
      tree <- input$inp_hier
      if(is.null(tree)) return(NULL)
      else return(read.csv(tree$datapath, stringsAsFactors = F))

    }
  )
  output$date_table = renderTable(ord_heir_table())

  no_of_col = reactive({
    if(is.null(data_tree())){
      return(1)
    } else {
        return(ncol(data_tree()))
      }

  })


  observe({
    updateSelectInput(session, inputId = "level", label = "Select hiearchy level",choices =  1:no_of_col(), selected =1)
    })


   ord_heir_table <- reactive({
     if(is.null(data_tree()))
     {
       return(ord_date_table())
     } else {
       return(aggregate_hierarchy(ord_date_table(),data_tree(),as.numeric(input$level)))
     }
   })






  ProcessInpAndGetApproxMap = function() {
    if(is.null(data_uploaded())) {
      return(NULL)
    } else {
      inp = cvt_seq(ord_heir_table())
      results = get_approxMap(inp,input$numKNN, input$cons_cutoff)
      # format_output(results)
      return(results)
    }
  }



  approxmap_obj <- eventReactive(input$but_AppMap,ProcessInpAndGetApproxMap())


  #to move to the consensus pattern tab
  observeEvent(input$but_AppMap, {
    updateTabsetPanel(session, "tabs", "Consensus Patterns")
  }
  )

  #output$approxMapInfo = renderPrint(approxmap_obj())
  rend_cons = function() {
    if(is.null(approxmap_obj())) {
      return("Approxmap not calculated yet")
    } else {
      nTabs = length(approxmap_obj()$clusters)
      #tabs = c("Overview",paste("Cluster", as.character(1:nTabs)))
      tabs = sapply(1:nTabs, function(x) paste0("Cluster",as.character(x)))
      n = unlist(lapply(approxmap_obj()$weighted_seqs, function(wseq) wseq$n))
      newTabs = lapply(1:nTabs, function(x) {
        tabPanel(title = paste0("Cluster ",x," (n=",n[x],")"),
                 tags$h4(paste("   Number of sequences in cluster = ", n[x])),
                 wellPanel(id = "results_panels",
                   tags$h3("Patterns:"), tags$br(),
                   # tags$h4("Consensus Pattern: "),tags$h4(htmlOutput(outputId = paste(tabs[x],"_cons",sep=""))),tags$br(),
                   # tags$h4("Variation Pattern: "),tags$h4(htmlOutput(outputId = paste(tabs[x],"_var",sep=""))),tags$br()
                   tags$h4(htmlOutput(outputId =  paste(tabs[x],"_pat",sep="")))
                   ),
                 wellPanel(id = "results_panels",
                   tags$h3("Frequency Plot:"), tags$br(),
                   plotOutput(outputId = paste(tabs[x],"_plot",sep="")), tags$br()),
                 wellPanel(id = "results_panels",
                   tags$h3("Weighted Sequence:"), tags$br(),
                   tags$h4(htmlOutput(outputId = paste(tabs[x],"_wseq",sep=""))),tags$br())
                 # wellPanel(
                 #   tags$h3("Variation Pattern:"), tags$br(),
                 #   tags$h4(textOutput(outputId = paste(x,"_var",sep=""))),tags$br())
        )
      }
      )
      do.call(tabsetPanel, newTabs)
    }
  }

  output$approxMapInfo = renderUI(rend_cons())

  observe({
  nTabs = length(approxmap_obj()$clusters)
  lapply(1:nTabs,function(x) {
    # wseq = paste0("Cluster",x,"_wseq")
    # plt = paste0("Cluster",x,"_plot")
    # cons = paste0("Cluster",x,"_cons")
    #output[[wseq]] <- renderPrint(cat(approxmap_obj()$formatted_results$weighted_seq[[x]]))
    output[[paste0("Cluster",x,"_wseq")]] <- renderPrint(cat(get_Wseq_Formatted_HTML(approxmap_obj()$weighted_seqs[[x]])))
    output[[paste0("Cluster",x,"_plot")]] <- renderPlot(plot_frequency(approxmap_obj()$weighted_seqs[[x]],input$cons_cutoff, input$noise_cutoff, input$var_cutoff))
    #output[[paste0("Cluster",x,"_cons")]] <- renderPrint(cat(approxmap_obj()$formatted_results$consensus[[x]]))
    #output[[paste0("Cluster",x,"_cons")]] <- renderPrint(cat(get_consensus_formatted_HTML(approxmap_obj()$weighted_seqs[[x]],input$cons_cutoff)))
    #var_pat = get_consensus_formatted(get_consensus_pattern(approxmap_obj()$weighted_seqs[[x]],input$var_cutoff))
    output[[paste0("Cluster",x,"_pat")]] <- renderPrint(cat(get_cons_var_table(approxmap_obj()$weighted_seqs[[x]],input$var_cutoff,input$cons_cutoff)))
  })
  }
  )

  observeEvent(input$cons_cutoff,{
    if(!is.null(approxmap_obj())) {
      nTabs = length(approxmap_obj()$clusters)
      lapply(1:nTabs,function(x) {
        cons = paste0("Cluster",x,"_pat")
        #cons_pat <- get_consensus_pattern(approxmap_obj()$weighted_seqs[[x]],input$cons_cutoff)
        output[[paste0("Cluster",x,"_pat")]] <- renderPrint(cat(get_cons_var_table(approxmap_obj()$weighted_seqs[[x]],input$var_cutoff,input$cons_cutoff)))
      })

    }
  })

  observeEvent(input$var_cutoff,{
    if(!is.null(approxmap_obj())) {
      nTabs = length(approxmap_obj()$clusters)
      lapply(1:nTabs,function(x) {
        output[[paste0("Cluster",x,"_pat")]] <- renderPrint(cat(get_cons_var_table(approxmap_obj()$weighted_seqs[[x]],input$var_cutoff,input$cons_cutoff)))
      })

    }
  })

  observe({
    updateSliderInput(session,inputId = "cons_cutoff", min = input$var_cutoff, max = 1, value = (1 + input$var_cutoff)/2)
  })

  #output$test <- renderText(paste("<font color=\"red\" size=10>This is some text!</font>"))
  output$test <- renderText(paste("<priority1>This is some text!</priority1>"))
  #priority1
}
