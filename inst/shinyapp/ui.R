ui <- fluidPage(
  titlePanel("RECOMMENDATION SYSTEM BASED ON FORMAL CONCEPT ANALYSIS"),

  tabsetPanel(
    tabPanel("Tab 1: Recommend by Attributes",
             h2("Recommend by Attributes"),
             fluidRow(
               column(
                 width = 12,
                 align = "left",
                 fluidRow(
                   column(
                     width = 3,
                     fileInput("file1", "Choose a CSV file (must be binary)",
                               accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
                   ),
                   column(
                     width = 1,
                     checkboxInput("header1", "Header", TRUE)
                   ),
                   column(
                     width = 1,
                     radioButtons("sep1", "Delimiter",
                                  choices = c(Comma = ",",
                                              Semicolon = ";",
                                              Tab = "\t"),
                                  selected = ",")
                   ),
                   column(
                     width = 1,
                     radioButtons("quote1", "Quote",
                                  choices = c(Nothing = "",
                                              Doble = '"',
                                              Simple = "'"),
                                  selected = '"')
                   )
                 )
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 align = "center",
                 pickerInput(
                   inputId = "selectedAttributes1",
                   label = div("Select the attributes:", class = "text-center"),
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
                 actionButton("saveButton1", "Calculate")
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 align = "center",
                 uiOutput("dropdown1")
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 DT::dataTableOutput("tabla1")
               )
             )
    ),
    tabPanel("Tab 2: Recommend by Max Cardinality",
             h2("Recommend by Max Cardinality"),
             fluidRow(
               column(
                 width = 12,
                 align = "left",
                 fluidRow(
                   column(
                     width = 3,
                     fileInput("file2", "Choose a CSV file (must be binary)",
                               accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
                   ),
                   column(
                     width = 1,
                     checkboxInput("header2", "Header", TRUE)
                   ),
                   column(
                     width = 1,
                     radioButtons("sep2", "Delimiter",
                                  choices = c(Comma = ",",
                                              Semicolon = ";",
                                              Tab = "\t"),
                                  selected = ",")
                   ),
                   column(
                     width = 1,
                     radioButtons("quote2", "Quote",
                                  choices = c(Nothing = "",
                                              Double = '"',
                                              Single = "'"),
                                  selected = '"')
                   )
                 )
               )
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
                 pickerInput(
                   inputId = "selectedAttributes2",
                   label = div("Select the attributes:", class = "text-center"),
                   choices = NULL,
                   selected = NULL,
                   multiple = TRUE
                 )
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 align = "center",  # Centrar el contenido
                 actionButton("saveButton2", "Calculate")
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 align = "center",
                 uiOutput("dropdown2")  # Esta salida se actualizará dinámicamente
               )
             ),
             fluidRow(
               column(
                 width = 12,
                 DT::dataTableOutput("tabla2")
               )
             )
    ),
    tabPanel("Tab 3: Recommend through iterative conversation",
             h2("Recommend through iterative conversation"),
             fluidRow(
               column(
                 width = 12,
                 align = "left",
                 fluidRow(
                   column(
                     width = 3,
                     fileInput("file3", "Choose a CSV file (must be binary)",
                               accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
                   ),
                   column(
                     width = 1,
                     checkboxInput("header3", "Header", TRUE)
                   ),
                   column(
                     width = 1,
                     radioButtons("sep3", "Delimiter",
                                  choices = c(Comma = ",",
                                              Semicolon = ";",
                                              Tab = "\t"),
                                  selected = ",")
                   ),
                   column(
                     width = 1,
                     radioButtons("quote3", "Quote",
                                  choices = c(Nothing = "",
                                              Double = '"',
                                              Single = "'"),
                                  selected = '"')
                   )
                 )
               )
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
                 pickerInput(
                   inputId = "selectedAttributes3",
                   label = div("Select the attributes:", class = "text-center"),
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
                 actionButton("saveButton3", "Calculate")
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
             )
    )
  )
)
