library(shiny)
#library(shinythemes)
library(lubridate)

ui <- fluidPage(
  #theme = "bootstrap3.css",
  #shinythemes::themeSelector(),
  tags$head(
      tags$style(
        HTML("
                priority1 {
                  font-size: 75%;
                  color: #7DCEA0;
                }
                priority2 {
                  font-size: 100%;
                  color: #52BE80;
                }
                priority3 {
                  font-size: 125%;
                  color: #27AE60;
                }
                priority4 {
                  font-size: 150%;
                  color: #229954;
                }
                priority5 {
                  font-size: 175%;
                  color: #1E8449;
                  font-weight: bold;
                }

                #results_panels {
                  background-color: white;
                }

            ")
                )
            ),

  tags$h1("ApproxMap"),
  fluidRow(
    column(4, tags$h2("Steps")),
    column(8, tags$h2("Results"))
  ),
  sidebarLayout(
    sidebarPanel(

      wellPanel(
        fileInput(inputId = "inp_data", label = tags$h4("1. File Upload"))
      ),

      wellPanel(
        tags$h4("2. Data Aggregation"), tags$br(),
        #dateInput(inputId = "st.date",label = "Enter Starting Date for Data",format = "mm-dd-yyyy", startview = "month", weekstart = 0),


        radioButtons(inputId = "period",
                     choices =  c("Predefined time periods" ,"Specify manually"),
                     label = "Select Time Period for Data (To be discussed Later)",
                     selected = "Predefined"
                     ),


        selectInput("period1", label = "Predefined Time Periods:",
                    #list(`Days` = c("1-d","5-d","10-d"), `Weeks` = c("1-w","2-w","4-w"), `Month` = c("Calender Month","3 Month"))
                     c("1 Week","Calender Month", "Quarterly", "6 Month", "1 Year")
        ),

    #    numericInput(inputId = "period2", label = "Specify manually (in days) (To be discussed Later):", value=0),

        conditionalPanel(
          condition = "input.period1  == '1 Week' ",
            selectInput(
              inputId = "week_st", label = "Day to start from",
              c("Mon","Tue","Wed","Thur","Fri","Sat","Sun"))
        ),

        conditionalPanel(
          condition = "input.period1  == 'Calender Month' ",
            "Text 1"
       ),

        conditionalPanel(
          condition = "input.period1  == 'Quarterly' ",
            textOutput("text2")
       ),

        conditionalPanel(
          condition = "input.period1  == '6 Month' ",
            selectInput(
              inputId = "month6_st", label = "Month to start from",
              c("Jan","Feb","Mar","Apr","May","Jun","July","Aug","Sep","Oct","Nov","Dec"))
       ),

        conditionalPanel(
          condition = "input.period1  == '1 Year' ",
            selectInput(
              inputId = "year_st", label = "Month to start from",
              c("Jan","Feb","Mar","Apr","May","Jun","July","Aug","Sep","Oct","Nov","Dec"))
    )

      ),

    wellPanel(
      fileInput(inputId = "inp_hier",
                label = tags$h4("3. Hierarchy File Upload")),

    selectInput(
     inputId = "level", label = "Select hiearchy level",
     c(1:4), selected = 1
     )
    ),


    wellPanel(
        tags$h4("4. Clustering (kNN)"), tags$br(),
        numericInput(inputId = "numKNN", label = "Enter number of nearest neighbours(k)", value=2)
      ),

      wellPanel(
        tags$h4("5. Cutoffs"), tags$br(),
        numericInput(inputId = "noise_cutoff", label = "Noise Cutoff", value = 0), tags$br(),
        sliderInput(inputId = "var_cutoff", label = "Variation Cutoff", min = 0, max = 1,value = 0.2),tags$br(),
        sliderInput(inputId = "cons_cutoff", label = "Consensus Cutoff", min = 0, max = 1,value = 0.4),tags$br()
      ),




     wellPanel(
        tags$h4("6. Get ApproxMap"), tags$br(),
        actionButton(inputId = "but_AppMap", label = "Calculate")
      )

    ),

    mainPanel(

      tabsetPanel(id = "tabs",
        tabPanel("Data",
                 column(6,
                        tableOutput("contents"),
                        verbatimTextOutput(outputId = "processedOut")
                        ),
                 column(6,
                        tableOutput("date_table")
                 )

                 ),

        tabPanel("Clusters",
                 htmlOutput(outputId = "test")
                 ),

        tabPanel("Consensus Patterns",
                 uiOutput(outputId = "approxMapInfo")
        )


      )

    )
  )
)
