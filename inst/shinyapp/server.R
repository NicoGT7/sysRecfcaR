server <- function(input, output, session) {
  file_csv <- reactiveVal(NULL)
  fc <- reactiveVal(NULL)
  concepts <- reactiveVal(NULL)
  attributes <- reactiveVal(NULL)

  ultimaTabla <- reactiveVal(NULL)
  semaforo <- reactiveVal(FALSE)
  historicIdx <- reactiveVal(NULL)
  selectedIdx <- reactiveVal(NULL)

  observeEvent(input$file1, {
    req(input$file1)

    inFile <- input$file1

    data <- read.csv(inFile$datapath,
                     header = input$header1,
                     sep = input$sep1,
                     quote = input$quote1,
                     row.names = NULL,
                     check.names = FALSE)

    data_matrix <- as.matrix(data)

    file_csv(data_matrix)

    data_matrix <- file_csv()

    fc2 <- FormalContext$new(data_matrix)

    fc(fc2)

    fc2$find_concepts()

    concepts2 <- fc2$concepts

    concepts(concepts2)

    attributes2 <- fc2$attributes

    attributes(attributes2)

    updatePickerInput(session, "selectedAttributes1", choices = attributes())

  })

  calcular1 <- eventReactive(input$saveButton1, {
    req(input$saveButton1)
    selected <- input$selectedAttributes1

    fc2 <- fc()

    set_attributes <- Set$new(fc2$attributes)
    set_attributes$assign(attributes = selected, values = rep(1,length(selected)))

    s <- fc2$closure(set_attributes)

    concepts2 <- concepts()

    idxConcept <- getIdx(concepts2, s)

    dfSubconceptos <- as.data.frame(getSupportSub(concepts2,idxConcept))

    return(dfSubconceptos)
  })

  output$tabla1 <- DT::renderDataTable({
    req(input$atributosProb1)

    columna <- input$atributosProb1

    final_result <- data.frame()

    for (i in 1:length(columna)) {

      res <- calcular1() %>% filter(.data[[columna[i]]] == 1)

      result <- res[1,1:2]

      result$atr <- columna[i]

      for (col in 3:ncol(res)) {
        if (any(res[1,col] == 1)) {
          result <- cbind(result, res[1,col, drop = FALSE])
        }
      }

      final_result <- bind_rows(final_result, result)
      final_result[is.na(final_result)] <- 0

    }

    return(final_result)
  })

  output$dropdown1 <- renderUI({
    if (!is.null(calcular1())) {
      pickerInput(
        inputId = "atributosProb1",
        label = div("Select the attributes to calculate probability:", class = "text-center"),
        choices = attributes(),
        selected = NULL,
        multiple = TRUE
      )
    }
  })

  observeEvent(input$file2, {
    req(input$file2)

    inFile <- input$file2

    data <- read.csv(inFile$datapath,
                     header = input$header2,
                     sep = input$sep2,
                     quote = input$quote2,
                     row.names = NULL,
                     check.names = FALSE)

    data_matrix <- as.matrix(data)

    file_csv(data_matrix)

    data_matrix <- file_csv()

    fc2 <- FormalContext$new(data_matrix)

    fc(fc2)

    fc2$find_concepts()

    concepts2 <- fc2$concepts

    concepts(concepts2)

    attributes2 <- fc2$attributes

    attributes(attributes2)

    updatePickerInput(session, "selectedAttributes2", choices = attributes())

  })

  calcular2 <- eventReactive(input$saveButton2, {
    req(input$saveButton2)
    selected <- input$selectedAttributes2

    fc2 <- fc()

    set_attributes <- Set$new(fc2$attributes)
    set_attributes$assign(attributes = selected, values = rep(1,length(selected)))

    s <- fc2$closure(set_attributes)

    concepts2 <- concepts()

    idxConcept <- getIdx(concepts2, s)

    dfSubconceptos <- as.data.frame(getSupportSub(concepts2,idxConcept))

    dfSubconceptos <- dfSubconceptos[dfSubconceptos$confidence > input$threshold2, ]

    result <- dfSubconceptos[,1:2]

    for (col in 3:ncol(dfSubconceptos)) {
      if (any(dfSubconceptos[,col] == 1)) {
        result <- cbind(result, dfSubconceptos[,col, drop = FALSE])
      }
    }

    result$ones <- rowSums(result[, 3:ncol(result)] == 1)

    result <- result[order(-result$ones), ]

    result$ones <- NULL

    return(result)
  })

  output$tabla2 <- DT::renderDataTable({
    calcular2()
  })

  observeEvent(input$file3, {
    req(input$file3)

    inFile <- input$file3

    data <- read.csv(inFile$datapath,
                     header = input$header3,
                     sep = input$sep3,
                     quote = input$quote3,
                     row.names = NULL,
                     check.names = FALSE)

    data_matrix <- as.matrix(data)

    file_csv(data_matrix)

    data_matrix <- file_csv()

    fc2 <- FormalContext$new(data_matrix)

    fc(fc2)

    fc2$find_concepts()

    concepts2 <- fc2$concepts

    concepts(concepts2)

    attributes2 <- fc2$attributes

    attributes(attributes2)

    updatePickerInput(session, "selectedAttributes3", choices = attributes())

  })

  calcular3 <- eventReactive(input$saveButton3, {
    req(input$saveButton3)
    selected <- input$selectedAttributes3

    fc2 <- fc()
    concepts2 <- concepts()
    attributes2 <- attributes()

    set_attributes <- Set$new(attributes2)
    set_attributes$assign(attributes = selected, values = rep(1, length(selected)))

    s <- fc2$closure(set_attributes)

    idxConcept <- getIdx(concepts2, s)

    dfSubconceptos <- as.data.frame(getSupportSub(concepts2, idxConcept))

    dfSubconceptos <- dfSubconceptos[dfSubconceptos$confidence > input$threshold3, ]

    result <- dfSubconceptos[, 1:2]

    for (col in 3:ncol(dfSubconceptos)) {
      if (any(dfSubconceptos[, col] == 1)) {
        result <- cbind(result, dfSubconceptos[, col, drop = FALSE])
      }
    }

    result$ones <- rowSums(result[, 3:ncol(result)] == 1)

    result <- result[order(-result$ones), ]

    result$ones <- NULL

    ultimaTabla(result)

    auxVector <- concepts2$sub(idxConcept)$get_intent()
    atrVector <- as.vector(t(as.matrix(auxVector$get_vector())))

    val <- auxVector$get_attributes()[which(atrVector == 1)]

    historicIdx(c(historicIdx(), list(list(val, "NA"))))

    selectedIdx(NULL)

    return(result)
  })

  recalcular3 <- eventReactive(input$validateButton3, {
    req(selectedIdx())
    idxConcept <- as.numeric(selectedIdx())

    if (is.na(idxConcept)) {
      return(NULL)
    }

    prevDF <- ultimaTabla()
    prevConf <- prevDF[prevDF$idx == idxConcept, "confidence"]

    fc2 <- fc()
    concepts2 <- concepts()
    attributes2 <- attributes()

    dfSubconceptos <- as.data.frame(getSupportSub(concepts2, idxConcept))

    dfSubconceptos <- dfSubconceptos[dfSubconceptos$confidence > input$threshold3, ]

    result <- dfSubconceptos[, 1:2]

    for (col in 3:ncol(dfSubconceptos)) {
      if (any(dfSubconceptos[, col] == 1)) {
        result <- cbind(result, dfSubconceptos[, col, drop = FALSE])
      }
    }

    result$ones <- rowSums(result[, 3:ncol(result)] == 1)

    result <- result[order(-result$ones), ]

    result$ones <- NULL

    ultimaTabla(result)

    auxVector <- concepts2$sub(idxConcept)$get_intent()
    atrVector <- as.vector(t(as.matrix(auxVector$get_vector())))

    val <- auxVector$get_attributes()[which(atrVector == 1)]

    historicIdx(c(historicIdx(), list(list(val, prevConf))))

    return(result)
  })

  output$tabla3 <- DT::renderDataTable({
    DT::datatable(ultimaTabla(), selection = 'single')
  })

  observeEvent(input$saveButton3, {
    output$tabla3 <- DT::renderDataTable({
      DT::datatable(calcular3(), selection = 'single')
    })
  })

  observeEvent(input$tabla3_rows_selected, {
    selectedRow <- input$tabla3_rows_selected
    if (length(selectedRow) == 1) {
      result <- ultimaTabla()
      selectedIdx(result[selectedRow, "idx"])
      output$selectedIdxText3 <- renderText({
        paste("Selected index:", selectedIdx())
      })

      semaforo(TRUE)
    }
  })

  observeEvent(input$validateButton3, {
    output$tabla3 <- DT::renderDataTable({
      recalcular3()
    })
  })

  output$validateButton3 <- renderUI({
    if (semaforo()) {
      actionButton("validateButton3", "Validate selection")
    }
  })

  output$historic3 <- renderUI({
    if (!is.null(historicIdx())) {
      tags$div(
        h4("History (Attributes, Confidence):"),
        HTML(paste0("<ul style='list-style-type: none;'>",
                    sapply(historicIdx(), function(tupla) {
                      paste0("<li>Attributes: ", paste(tupla[[1]], collapse = ", "), ", Confidence: ", tupla[[2]], "</li>")
                    }),
                    "</ul>"))
      )
    }
  })

}
