ui <- fluidPage(
  shinyjs::useShinyjs(),
  theme = shinythemes::shinytheme("lumen"),
  tags$head(
    tags$style(HTML("
      .tab-content {
        padding: 20px;
      }
      h2 {
        margin-top: 0;
      }
      .custom-title {
        font-family: 'Open Sans', sans-serif;
        font-size: 30px;
        font-weight: bold;
        text-align: center;
        color: #158cba;
        text-shadow: 2px 2px 4px #d9edf7;
        margin-bottom: 20px;
        margin-top: 20px;
      }
      .help-button {
        background-color: #158cba;
        color: white;
        border: none;
        border-radius: 50%;
        width: 30px;
        height: 30px;
        font-size: 16px;
        text-align: center;
        line-height: 30px;
        cursor: pointer;
        padding: 0;
        margin-bottom: 5px;
      }
      .help-button:hover {
        background-color: #0056b3;
      }
    "))
  ),

  titlePanel(title = div(class = "custom-title", "RECOMMENDATION SYSTEM BASED ON FORMAL CONCEPT ANALYSIS")),


  tabsetPanel(
    tabPanel("Data import",
             div(style = "text-align: center;",
                 h2("Upload file"),
                 fluidRow(
                   column(
                     width = 12,
                     align = "center",
                     wellPanel(
                       div(style = "margin: 0 auto; display: inline-block;",
                           fluidRow(
                             column(
                               width = 3,
                               align = "center",
                               offset = 0,
                               checkboxInput("header1", "Header", TRUE)
                             ),
                             column(
                               width = 3,
                               align = "center",
                               offset = 0,
                               checkboxInput("col1", "Has row names?", TRUE)
                             ),
                             column(
                               width = 3,
                               align = "center",
                               offset = 0,
                               radioButtons("sep1", "Delimiter",
                                            choices = c(Comma = ",",
                                                        Semicolon = ";",
                                                        Tab = "\t"),
                                            selected = ";")
                             ),
                             column(
                               width = 3,
                               offset = 0,
                               align = "center",
                               radioButtons("quote1", "Quote",
                                            choices = c(Nothing = "",
                                                        Double = '"',
                                                        Single = "'"),
                                            selected = '')
                             )
                           ),
                           fluidRow(
                             column(
                               width = 11,
                               align = "center",
                               offset = 0,
                               div(style = "display: inline-block; width: 100%; max-width: 300px;",
                                   fileInput("file1", "Choose a CSV file (must be binary)",
                                             accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
                               )
                             )
                           ),
                           fluidRow(
                             column(
                               width = 12,
                               align = "center",
                               actionButton("help0", "?", class = "help-button")
                             )
                           )
                       )
                     )
                   )
                 )
             )
    ),
    tabPanel("Recommend by Attributes",
             div(style = "text-align: center;",
                 h2("Recommend by Attributes")
             ),
             fluidRow(
               column(
                 width = 12,
                 align = "center",
                 shinyWidgets::pickerInput(
                   inputId = "selectedAttributes1",
                   label = div("Select initial attributes:", class = "text-center"),
                   choices = NULL,
                   selected = NULL,
                   multiple = TRUE
                 )
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 align = "center",
                 actionButton("help1", "?", class = "help-button")
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 align = "center",
                 actionButton("saveButton1", "Calculate", class = "btn-primary")
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 align = "center",
                 shinyjs::hidden(uiOutput("dropdown1"))
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 DT::dataTableOutput("tabla1")
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 align = "center",
                 uiOutput("downloadButtonUI1")
               )
             )
    ),
    tabPanel("Recommend by Max Cardinality",
             div(style = "text-align: center;",
                 h2("Recommend by Max Cardinality")
             ),
             fluidRow(
               column(
                 width = 6,
                 align = "right",
                 sliderInput("threshold2", "Confidence threshold:",
                             min = 0, max = 1, value = 0.5, step = 0.05)
               ),
               column(
                 width = 6,
                 align = "left",
                 shinyWidgets::pickerInput(
                   inputId = "selectedAttributes2",
                   label = div("Select initial attributes:", class = "text-center"),
                   choices = NULL,
                   selected = NULL,
                   multiple = TRUE
                 )
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 align = "center",
                 actionButton("help2", "?", class = "help-button")
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 align = "center",
                 actionButton("saveButton2", "Calculate", class = "btn-primary")
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 align = "center",
                 uiOutput("dropdown2")
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 DT::dataTableOutput("tabla2")
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 align = "center",
                 uiOutput("downloadButtonUI2")
               )
             )
    ),
    tabPanel("Recommend through iterative conversation",
             div(style = "text-align: center;",
                 h2("Recommend through iterative conversation")
             ),
             fluidRow(
               column(
                 width = 6,
                 align = "right",
                 sliderInput("threshold3", "Confidence threshold:",
                             min = 0, max = 1, value = 0.5, step = 0.05)
               ),
               column(
                 width = 6,
                 align = "left",
                 shinyWidgets::pickerInput(
                   inputId = "selectedAttributes3",
                   label = div("Select initial attributes:", class = "text-center"),
                   choices = NULL,
                   selected = NULL,
                   multiple = TRUE
                 )
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 align = "center",
                 actionButton("help3", "?", class = "help-button")
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 align = "center",
                 actionButton("saveButton3", "Calculate", class = "btn-primary")
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 align = "center",
                 DT::dataTableOutput("tabla3")
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 align = "center",
                 textOutput("selectedIdxText3"),
                 uiOutput("validateButton3")
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 align = "center",
                 uiOutput("historic3")
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 align = "center",
                 uiOutput("downloadButtonUI3")
               )
             )
    ),
    tabPanel("Interactive Graph",
             fluidRow(
               column(
                 width = 4,
                 fluidRow(
                   column(
                     width = 7,
                     sliderInput("threshold4", "Support threshold:",
                                 min = 0, max = 1, value = 0.8, step = 0.1)
                   ),
                   column(
                     width = 1,
                     align = "center",
                     actionButton("help4", "?", class = "help-button")
                   ),
                   column(
                     width = 3,
                     align = "center",
                     offset = 3,
                     actionButton("submitButton4", "Submit")
                   )
                 )
               ),
               column(
                 width = 8,
                 visNetwork::visNetworkOutput("network"),
                 verbatimTextOutput("selected_node_attributes")
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 align = "center",
                 actionButton("saveButton4", "Calculate", style = "display: none;")
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 align = "center",
                 uiOutput("dropdown4")
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 DT::dataTableOutput("tabla4")
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 align = "center",
                 uiOutput("downloadButtonUI4")
               )
             )
    )
  )
)
