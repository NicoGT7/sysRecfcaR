server <- function(input, output, session) {
  file_csv <- reactiveVal(NULL)
  fc <- reactiveVal(NULL)
  concepts <- reactiveVal(NULL)
  attributes <- reactiveVal(NULL)

  ultimaTabla <- reactiveVal(NULL)
  semaforo <- reactiveVal(FALSE)
  historicIdx <- reactiveVal(NULL)
  selectedIdx <- reactiveVal(NULL)

  sublattice <- reactiveVal(NULL)
  idxNode <- reactiveVal(NULL)

  exportData1 <- reactiveVal(NULL)
  exportData2 <- reactiveVal(NULL)
  exportData3 <- reactiveVal(NULL)
  exportData4 <- reactiveVal(NULL)

  mostrar2 <- reactiveVal(FALSE)
  colvalues <- reactiveVal(NULL)

  observeEvent(input$reset1, {
    session$reload()
  })

  observeEvent(input$file1, {
    req(input$file1)

    inFile <- input$file1

    data <- read.csv(inFile$datapath,
                     header = input$header1,
                     sep = input$sep1,
                     quote = input$quote1,
                     row.names = NULL,
                     check.names = FALSE)

    error <- FALSE

    if(input$col1){

      tryCatch({
        rownames(data) <- data[,1]
      }, error = function(e) {
        error <<- TRUE
        shinyalert::shinyalert(title = "Error",
                               type = "error",
                               text = "Error. Please check your csv file. Maybe it does not
                               have row names.")
        return()
      })

      data[,1] <- NULL
    }

    if(!error){
      data_matrix <- as.matrix(data)

      file_csv(data_matrix)

      data_matrix <- file_csv()

      fc2 <- tryCatch({
        fcaR::FormalContext$new(data_matrix)
      }, error = function(e) {
        shinyalert::shinyalert(title = "Error",
                               type = "error",
                               text = "Error creating the formal context. Please check your input data.
                               Maybe you have not selected the correct delimiter.")
        return(NULL)
      })

      if (!is.null(fc2)) {
        fc(fc2)

        tryCatch({
          fc2$find_concepts()
          showNotification("File processed successfully.
                           Computing lattice. Please, wait for the computation to finish.
                           Do not close this window.", type = "message")
        }, error = function(e) {
          shinyalert::shinyalert(title = "Error",
                                 type = "error",
                                 text = "Error: File must be binary. Please check your input data.")
          return()
        })

        if (!is.null(fc2$concepts)) {
          concepts2 <- fc2$concepts
          concepts(concepts2)

          attributes2 <- fc2$attributes
          attributes(attributes2)

          shinyWidgets::updatePickerInput(session, "selectedAttributes1", choices = attributes())
          shinyWidgets::updatePickerInput(session, "selectedAttributes2", choices = attributes())
          shinyWidgets::updatePickerInput(session, "selectedAttributes3", choices = attributes())

          showNotification("Computation finished, recommendation strategies are now available", type = "message")

          ultimaTabla(NULL)
          semaforo(FALSE)
          historicIdx(NULL)
          selectedIdx(NULL)

          sublattice(NULL)
          idxNode(NULL)

          mostrar2(FALSE)
          colvalues(NULL)
        }
      }
    }

  })

  observeEvent(input$help0, {
    showModal(modalDialog(
      title = "Help Data Import",
      "In this window, you must upload a CSV file so that various techniques
      can be applied to the dataset, and different strategies
      of the recommender system can be utilized.",
      easyClose = TRUE,
      footer = NULL
    ))
  })

  observeEvent(input$selectedAttributes1, {
    shinyWidgets::updatePickerInput(session, "atributosProb1", choices = attributes(), selected = NULL)
    shinyjs::hide("tabla1")
    shinyjs::hide("dropdown1")
    shinyjs::hide("downloadData1")
  })

  output$tabla1 <- DT::renderDataTable({
    req(input$saveButton1)
    req(input$atributosProb1)

    selected <- input$selectedAttributes1
    columna <- input$atributosProb1

    fc2 <- fc()

    final_result <- tryCatch({
      sysRecfcaR::recommend_by_attributes(fc2,selected,columna)
    }, error = function(e) {
      shinyalert::shinyalert(title = "Error",
                             type = "error",
                             text = "Error.
                             There is no possible recommendation for the selected attributes")
      return(NULL)
    })

    if(!is.null(final_result)){
      exportData1(final_result)

      `%>%` <- magrittr::`%>%`

      DT::datatable(final_result,
                    options = list(
                      pageLength = 10,
                      autoWidth = TRUE,
                      dom = 'ftipr',
                      class = 'stripe compact hover row-border'
                    ), style = 'bootstrap4') %>%
        DT::formatStyle(
          columns = input$selectedAttributes1,
          backgroundColor = '#cce5ff'
        )
    }
  })

  observeEvent(input$help1, {
    showModal(modalDialog(
      title = "Help Recommend by Attributes",
      "In this window, a strategy based on attributes will be implemented.
      Therefore, a direct search within the lattice will be conducted to obtain
      the most reliable subconcept that contains both the selected attributes
      and the desired attributes.
      Each row of the resulting table will be the best possible recommendation
      for the desired attribute; that is, each recommendation is individual",
      easyClose = TRUE,
      footer = NULL
    ))
  })

  observeEvent(input$saveButton1, {
    shinyjs::show("tabla1")
    shinyjs::show("dropdown1")
    shinyjs::show("downloadData1")
  })

  output$dropdown1 <- renderUI({
    shinyWidgets::pickerInput(
      inputId = "atributosProb1",
      label = div("Select the desired attributes to calculate recommendations:", class = "text-center"),
      choices = attributes(),
      selected = NULL,
      multiple = TRUE
    )
  })

  output$downloadButtonUI1 <- renderUI({
    if (!is.null(exportData1())) {
      downloadButton("downloadData1", "Download CSV")
    }
  })

  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste("recommendationAttr.csv", sep = "")
    },
    content = function(file) {
      df <- exportData1()
      readr::write_csv(df, file)
    }
  )

  observeEvent(input$help2, {
    showModal(modalDialog(
      title = "Help Recommend by Max Cardinality",
      "In this window, a strategy based on max cardinality will be implemented.
      A minimum confidence threshold and the attributes that must be included
      in the recommendation should be selected. The result will be a table
      with all the recommendations that meet both criteria, ordered by the
      number of attributes the concept possesses.",
      easyClose = TRUE,
      footer = NULL
    ))
  })

  observeEvent(input$saveButton2, {
    mostrar2(TRUE)
    shinyjs::show("downloadData2")
  })

  observeEvent(input$selectedAttributes2, {
    mostrar2(FALSE)
    shinyjs::hide("downloadData2")
  })

  output$tabla2 <- DT::renderDataTable({
    req(mostrar2())
    `%>%` <- magrittr::`%>%`

    fc2 <- fc()
    selected <- input$selectedAttributes2
    valConf <- input$threshold2


    final_result <- tryCatch({
      sysRecfcaR::recommend_by_max_cardinality(fc2, selected, valConf)
    }, error = function(e) {
      shinyalert::shinyalert(title = "Error",
                             type = "error",
                             text = "Error.
                             There is no possible recommendation for the selected attributes")
      return(NULL)
    })

    if(!is.null(final_result)){
      exportData2(final_result)
      DT::datatable(final_result,
                    options = list(
                      pageLength = 10,
                      autoWidth = TRUE,
                      dom = 'ftipr',
                      class = 'stripe compact hover row-border'
                    ), style = 'bootstrap4') %>%
        DT::formatStyle(
          columns = input$selectedAttributes2,
          backgroundColor = '#cce5ff'
        )
    }
  })

  output$downloadButtonUI2 <- renderUI({
    if (!is.null(exportData2())) {
      downloadButton("downloadData2", "Download CSV")
    }
  })

  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("recommendationMC.csv", sep = "")
    },
    content = function(file) {
      df <- exportData2()
      readr::write_csv(df, file)
    }
  )

  calcular3 <- eventReactive(input$saveButton3, {
    req(input$saveButton3)
    selected <- input$selectedAttributes3

    colvalues(selected)

    fc2 <- fc()
    concepts2 <- concepts()
    attributes2 <- attributes()

    set_attributes <- fcaR::Set$new(attributes2)
    set_attributes$assign(attributes = selected, values = rep(1, length(selected)))

    s <- fc2$closure(set_attributes)

    idxConcept <- sysRecfcaR::getIdx(concepts2, s)

    valConf <- input$threshold3

    result <- tryCatch({
      sysRecfcaR::recommend_by_max_cardinality(fc2, selected, valConf)
    }, error = function(e) {
      shinyalert::shinyalert(title = "Error",
                             type = "error",
                             text = "Error.
                             There is no possible recommendation for the selected attributes")
      return(NULL)
    })

    if(!is.null(result)){
      ultimaTabla(result)
      exportData3(result)

      auxVector <- concepts2$sub(idxConcept)$get_intent()
      atrVector <- as.vector(t(as.matrix(auxVector$get_vector())))

      val <- auxVector$get_attributes()[which(atrVector == 1)]

      historicIdx(c(historicIdx(), list(list(val, 1))))

      selectedIdx(NULL)

      return(result)
    }
  })

  recalcular3 <- eventReactive(input$validateButton3, {
    req(selectedIdx())
    idxConcept <- as.numeric(selectedIdx())

    if (is.na(idxConcept)) {
      return(NULL)
    }

    prevDF <- ultimaTabla()
    prevConf <- prevDF[prevDF$idx == idxConcept, "Confidence"]

    fc2 <- fc()
    concepts2 <- concepts()
    attributes2 <- attributes()

    shinyWidgets::updatePickerInput(session, "selectedAttributes3", choices = attributes(),
                                    selected = sysRecfcaR::getAttributes(concepts2, idxConcept))

    colvalues(sysRecfcaR::getAttributes(concepts2, idxConcept))

    selected <- sysRecfcaR::getAttributes(concepts2, idxConcept)

    valConf <- input$threshold3

    result <- sysRecfcaR::recommend_by_max_cardinality(fc2, selected, valConf)

    ultimaTabla(result)
    exportData3(result)

    auxVector <- concepts2$sub(idxConcept)$get_intent()
    atrVector <- as.vector(t(as.matrix(auxVector$get_vector())))

    val <- auxVector$get_attributes()[which(atrVector == 1)]

    historicIdx(c(historicIdx(), list(list(val, prevConf))))

    return(result)
  })

  observeEvent(input$help3, {
    showModal(modalDialog(
      title = "Help Recommend through iterative conversation",
      "In this window, a strategy based on an iterative conversation with the user
      will be implemented.
      It follows the same idea as the strategy based on maximum cardinality,
      but for each resulting table, the user will be able to choose one of the
      rows and the process will be repeated until the desired recommendation
      is reached.",
      easyClose = TRUE,
      footer = NULL
    ))
  })

  output$tabla3 <- DT::renderDataTable({
    DT::datatable(ultimaTabla(),
                  options = list(
                    pageLength = 10,
                    autoWidth = TRUE,
                    dom = 'ftipr',
                    class = 'stripe compact hover row-border'
                  ), style = 'bootstrap4', selection = 'single')
  })

  observeEvent(input$saveButton3, {
    req(input$saveButton3)

    if(!is.null(calcular3())){
      output$tabla3 <- DT::renderDataTable({
        `%>%` <- magrittr::`%>%`
        DT::datatable(calcular3(),
                      options = list(
                        pageLength = 10,
                        autoWidth = TRUE,
                        dom = 'ftipr',
                        class = 'stripe compact hover row-border'
                      ), style = 'bootstrap4', selection = 'single') %>%
          DT::formatStyle(
            columns = colvalues(),
            backgroundColor = '#cce5ff'
          )
      })
    }
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
      `%>%` <- magrittr::`%>%`
      DT::datatable(recalcular3(),
                    options = list(
                      pageLength = 10,
                      autoWidth = TRUE,
                      dom = 'ftipr',
                      class = 'stripe compact hover row-border'
                    ), style = 'bootstrap4', selection = 'single') %>%
        DT::formatStyle(
          columns = colvalues(),
          backgroundColor = '#cce5ff'
        )
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
        style = 'border: 2px solid #158cba; padding: 10px; border-radius: 5px; background-color: #f9f9f9; max-width: 600px; margin: 0 auto;',
        h4("History (Attributes, Confidence):"),
        HTML(
          paste0(
            "<ul style='list-style-type: none; padding-left: 0;'>",
            paste0(
              sapply(historicIdx(), function(tupla) {
                paste0(
                  "<li style='margin-bottom: 5px;'>",
                  "<span style='font-weight: bold;'>Attributes:</span> ",
                  paste(tupla[[1]], collapse = ", "),
                  ", <span style='font-weight: bold;'>Confidence:</span> ",
                  tupla[[2]],
                  "</li>"
                )
              }),
              collapse = ""
            ),
            "</ul>"
          )
        )
      )


    }
  })

  output$downloadButtonUI3 <- renderUI({
    if (!is.null(exportData3())) {
      downloadButton("downloadData3", "Download CSV")
    }
  })

  output$downloadData3 <- downloadHandler(
    filename = function() {
      paste("recommendationInt.csv", sep = "")
    },
    content = function(file) {
      df <- exportData3()
      readr::write_csv(df, file)
    }
  )

  generate_graph <- function(threshold) {
    req(input$file1)

    concepts2 <- concepts()

    idx <- which(concepts2$support() > threshold)

    sublattice2 <- concepts2$sublattice(idx)

    sublattice(sublattice2)

    graph <- sysRecfcaR::graph_sublattice(sublattice2)

    vis_data <- visNetwork::toVisNetworkData(graph)

    `%>%` <- magrittr::`%>%`

    return(visNetwork::visNetwork(nodes = vis_data$nodes, edges = vis_data$edges, main = "Concept Lattice") %>%
             visNetwork::visIgraphLayout(layout = "layout_with_sugiyama") %>%
             visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
             visNetwork::visInteraction(hover = TRUE) %>%
             visNetwork::visEvents(click = "function(properties) {
               var nodeId = properties.nodes[0];
               Shiny.setInputValue('selected_node_id', nodeId);
             }"))
  }

  observeEvent(input$submitButton4, {
    req(input$submitButton4)
    threshold <- isolate(input$threshold4)
    output$network <- visNetwork::renderVisNetwork({
      generate_graph(threshold)
    })
  })


  observeEvent(input$selected_node_id, {
    selected_node <- input$selected_node_id
    if (!is.null(selected_node)) {
      shinyjs::show("selected_node_attributes")

      shinyjs::hide("tabla4")
      shinyjs::hide("dropdown4")
      shinyjs::hide("downloadData4")

      sublattice2 <- sublattice()
      attributes <- sysRecfcaR::getAttributes(sublattice2, as.numeric(selected_node))

      shinyWidgets::updatePickerInput(session, "atributosProb4", choices = attributes(), selected = NULL)

      output$selected_node_attributes <- renderPrint({
        cat("Attributes of the selected node:\n")
        cat(paste(attributes, collapse = ", "))
      })

      idxNode(selected_node)

      shinyjs::show("saveButton4")
      shinyjs::hide("downloadData4")
    }
  })

  observeEvent(input$help4, {
    showModal(modalDialog(
      title = "Help Interactive Graph",
      "In this window, a strategy based on an interactive graph will be implemented.
      A minimum support threshold should be selected, and the sublattice
      of concepts that meet this restriction will be displayed, allowing the
      user to explore freely. Once a node is selected, the attribute-based
      recommendation can be activated",
      easyClose = TRUE,
      footer = NULL
    ))
  })

  observeEvent(input$threshold4, {
    shinyWidgets::updatePickerInput(session, "atributosProb4", choices = attributes())
    shinyjs::hide("saveButton4")
    shinyjs::hide("tabla4")
    shinyjs::hide("dropdown4")
    shinyjs::hide("selected_node_attributes")
    shinyjs::hide("downloadData4")
  })

  observeEvent(input$saveButton4, {
    shinyjs::show("tabla4")
    shinyjs::show("dropdown4")
    shinyjs::show("downloadData4")
  })

  output$tabla4 <- DT::renderDataTable({
    req(input$atributosProb4)

    sublattice2 <- sublattice()
    idxNode2 <- idxNode()
    selected <- sysRecfcaR::getAttributes(sublattice2, idxNode2)

    columna <- input$atributosProb4

    fc2 <- fc()

    final_result <- sysRecfcaR::recommend_by_attributes(fc2,selected,columna)

    exportData4(final_result)

    sublattice2 <- sublattice()
    idxNode2 <- idxNode()
    selected <- sysRecfcaR::getAttributes(sublattice2, idxNode2)

    `%>%` <- magrittr::`%>%`

    DT::datatable(final_result,
                  options = list(
                    pageLength = 10,
                    autoWidth = TRUE,
                    dom = 'ftipr',
                    class = 'stripe compact hover row-border'
                  ), style = 'bootstrap4') %>%
      DT::formatStyle(
        columns = selected,
        backgroundColor = '#cce5ff'
      )
  })

  output$dropdown4 <- renderUI({
    shinyWidgets::pickerInput(
      inputId = "atributosProb4",
      label = div("Select the desired attributes to calculate recommendations:", class = "text-center"),
      choices = attributes(),
      selected = NULL,
      multiple = TRUE
    )
  })

  output$downloadButtonUI4 <- renderUI({
    if (!is.null(exportData4())) {
      downloadButton("downloadData4", "Download CSV")
    }
  })

  output$downloadData4 <- downloadHandler(
    filename = function() {
      paste("recommendationGraph.csv", sep = "")
    },
    content = function(file) {
      df <- exportData4()
      readr::write_csv(df, file)
    }
  )
}
