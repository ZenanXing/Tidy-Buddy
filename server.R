# Define server logic
function(input, output, session) {

# Input --------------------------------------------------------------------
  rv <- reactiveValues()
  
## Number of tables --------------------------------------------------------
  
  output$tb_no_ui <- renderUI({
    selectInput(inputId = "tb_no", 
                label = "- Table No.", 
                choices = seq_len(as.numeric(input$n_tb)))
  })
  
## Number of variables -----------------------------------------------------

  output$var_no_ui <- renderUI({
    selectInput(inputId = "var_no", 
                label = "- Variable No.", 
                choices = seq_len(as.numeric(input$n_var)))
  })

## Variable Names ----------------------------------------------------------

  output$var_nms_ui <- renderUI({
    ui_list <- lapply(seq_len(as.numeric(input$n_var)), function(i) {
      conditionalPanel(
        condition = sprintf("input.var_no == %d", i),
        textInput(
          inputId = paste0("var_nm_", i),
          label   = "- Name",
          value   = paste0("V", i)
        )
      )
    })
    # Return them as a tagList to display them in UI
    tagList(ui_list)
  })

## Variable Types ----------------------------------------------------------
  
   output$var_tps_ui <- renderUI({
      # We build a list of conditionalPanels, one per possible 'var_no'
      ui_list <- lapply(seq_len(as.numeric(input$n_var)), function(i) {
        conditionalPanel(
          # Only show this text input if var_no == i
          condition = sprintf("input.var_no == %d", i),
          selectInput(
            inputId = paste0("var_tp_", i),
            label = "- Type", 
            choices = c("Numeric" = "num",
                        "Text" = "txt"),
            selected = "num"
          )
        )
      })
      # Return them as a tagList to display them in UI
      tagList(ui_list)
    })
  
## Variable Layouts --------------------------------------------------------
   
   output$var_slt_ui <- renderUI({
     n_var <- as.numeric(input$n_var)
     lab <- vapply(seq_len(n_var), function(i) {
       nm <- input[[paste0("var_nm_", i)]] %||% paste0("V", i)
       paste(i, nm, sep = " - ")
     }, character(1))
     selectInput(
       inputId = "var_slt_no",
       label   = "- Variable",
       choices = setNames(seq_len(n_var), lab)
     )
   })
   
   output$var_layout_ui <- renderUI({
     if (is.null(input$var_slt_no) || input$var_slt_no == "") { return(NULL) }
     ui_list <- lapply(seq_len(as.numeric(input$n_tb)), function(i) {
       lapply(seq_len(as.numeric(input$n_var)), function(j) {
         conditionalPanel(
           condition = sprintf("input.tb_no == %d && input.var_slt_no == %d", i, j),
           div(style = "overflow-x: auto; max-width: 800px; max-height: 800px;",
             rHandsontableOutput(outputId = paste0("df_", i, "_", j))
           )
         )
       })
     })
     tagList(ui_list)
   })
   
   # Render each rhandsontable
   observeEvent({ input$n_tb; input$n_var; input$row; input$col; input$var_slt_no }, {
     nr <- as.numeric(input$row)
     nc <- as.numeric(input$col)
     
     lapply(seq_len(as.numeric(input$n_tb)), function(i) {
       lapply(seq_len(as.numeric(input$n_var)), function(j) {
         key <- paste0("df_", i, "_", j)  # Unique key for each table and variable
         
         output[[key]] <- renderRHandsontable({
           df <- rv[[key]]
           
           # If the data frame for this specific table doesn't exist, create one
           if (is.null(df)) {
             df <- matrix(NA, nrow = nr, ncol = nc,
                          dimnames = list(LETTERS[seq_len(nr)], seq_len(nc))) %>%
               as.data.frame()
           }
           
           # Ensure mode conforms to chosen type
           if ((input[[paste0("var_tp_", j)]] %||% "num") == "num") {
             df[] <- lapply(df, as.numeric)
           } else {
             df[] <- lapply(df, as.character)
           }
           
           rhandsontable(df)  # Render the handsontable
           
         })
       })
     })
   })

   
# Output ----------------------------------------------------------------------
   
   observe({
     req(input$n_tb, input$n_var)
     for (i in seq_len(as.numeric(input$n_tb))) {
       for (j in seq_len(as.numeric(input$n_var))) {
         key <- paste0("df_", i, "_", j)
         if (!is.null(input[[key]])) {
           rv[[key]] <- hot_to_r(input[[key]])
         }
       }
     }
   })
   
  ## Reactive table
  df_tidy <- eventReactive(input$Trsfm_Butn, {
    # required input info.
    n_tb <- isolate({as.numeric(input$n_tb)})
    n_var <- isolate({as.numeric(input$n_var)})
    row <- isolate({as.numeric(input$row)})
    col <- isolate({as.numeric(input$col)})
    # combine all the info.
    df_tidy <- NULL
    for (i in 1:n_tb) {
      df_tidy_temp <- NULL
      for (j in 1:n_var) {
        var_col_name <- input[[paste0("var_nm_", j)]]
        df_temp <- hot_to_r(input[[paste0("df_", i, "_", j)]])
        df_temp <- df_temp %>%
          mutate(rownm = rownames(df_temp)) %>%
          tidyr::pivot_longer(
            cols = 1:col,
            names_to = "colnm",
            values_to = var_col_name
          ) %>%
          mutate(tb = i) %>%
          tidyr::unite(tb, rownm, colnm, col = "pos")

        if (is.null(df_tidy_temp)) {
          df_tidy_temp <- df_temp
        } else {
          df_tidy_temp <- dplyr::left_join(df_tidy_temp, df_temp, by = "pos")
        }
        
      }
      if (is.null(df_tidy)) {
        df_tidy <- df_tidy_temp
      } else {
        df_tidy <- rbind(df_tidy, df_tidy_temp)
      }
    }

    df_tidy <- df_tidy %>% tidyr::separate(pos, into = c("Table", "Row", "Column"))
    
    return(df_tidy)
  })

  output$tidy_tb <- DT::renderDataTable({
    DT::datatable(df_tidy(), 
                  options = list(lengthMenu = c(5, 30, 50), pageLength = 10, scrollX = T))
  })
  
  ## Download
  output$dl <- renderUI({
    req(input$Trsfm_Butn)
    tagList(
      list(
        h6(tags$b("Download:")),
        textInput(inputId = "file_name", label = "Enter a file name: ", value = Sys.time()),
        p("Select file type:"), 
        div(style = "margin-top: -30px"), 
        selectInput(
          inputId = "file_type", 
          label = NULL, 
          choices = list("EXCEL", "CSV", "TSV", "TXT"),
          selected = "EXCEL",
          width = "150px"
        ),
        div(style = "margin-top: -20px"),
        div(style = "width: 150px;",
            downloadButton(outputId = "dl_df", label = "Download"))
      )
    )
  })
  
  output$dl_df <- downloadHandler(
    filename = function() {
      if (input$file_type == "EXCEL") { ext <- ".xlsx" } else { ext <- paste0(".", tolower(input$file_type))}
      paste0(input$file_name, ext)
    },
    
    content = function(file) {
      if (input$file_type == "EXCEL") {
        openxlsx::write.xlsx(df_tidy(), file)
      } else {
         sep <- switch(input$file_type, "TXT" = " ", "CSV" = ",", "TSV" = "\t" )
        write.table(x = df_tidy(), file = file, sep = sep, quote = FALSE, row.names = FALSE)
      }
    }
  )

# IGGYPOP-related ---------------------------------------------------------
  
  iggypop_df_tidy <- eventReactive(input$Gnrt_Butn, {
    if (input$tdy_opt == "tdy_upld") {
      req(input$file)
      inFile <- input$file
      if (grepl("csv", inFile$datapath)){
        data <- read.table(inFile$datapath, sep = ",", header = TRUE)
      } else {
        if (grepl("tsv", inFile$datapath)){
          data <- read.table(inFile$datapath, sep = "\t", header = TRUE)
        } else {
          data <- read.xlsx(inFile$datapath, colNames = TRUE)
        }
      }
    } else {
      data <- df_tidy()
    }
    return(data)
  })
  
  iggypop_ref <- eventReactive(input$Gnrt_Butn, {
    req(input$ref_file)
    inFile <- input$ref_file
    if (grepl("csv", inFile$datapath)){
      data <- read.table(inFile$datapath, sep = ",", header = TRUE)
    } else {
      if (grepl("tsv", inFile$datapath)){
        data <- read.table(inFile$datapath, sep = "\t", header = TRUE)
      } else {
        data <- read.xlsx(inFile$datapath, colNames = TRUE)
      }
    }
    return(data)
  })
  
  ## Reactive table
  df_iggypop_smp <- eventReactive(input$Gnrt_Butn, {
    # primer_info:
    if (input$prr_plt == "pop") {
      df_primer <- read.xlsx("data/pPOP_PrimerPlate_Layout.xlsx", sheet = "TidyFormat")
    } else {
      df_primer <- read.xlsx("data/pPlantPOP_PrimerPlate_Layout.xlsx", sheet = "TidyFormat")
    }
    df_primer <- df_primer %>% tidyr::unite(PrimerPlate, PrimerWell, sep = "_", col = "pos")
    # plate layout:
    df_plate_layout <- iggypop_df_tidy()
    # ref_seq_info:
    if (input$cds_opt == "n") {
      slt_var <- c("primer_index", "n_frags", "Reference", "ReferenceSequence")
    } else {
      slt_var <- c("primer_index", "n_frags", "CDS_length", "Reference", "ReferenceSequence")
    }
    df_ref <- iggypop_ref() %>% dplyr::select(slt_var)
    # combine all the info.
    df_smp <- df_plate_layout %>% 
      mutate(SampleID = paste0(primer_index, "_", Replicate)) %>% 
      tidyr::unite(PrimerPlate, PrimerWell, sep = "_", col = "pos") %>% 
      dplyr::select(SampleID, primer_index, pos) %>% 
      left_join(df_ref, by = "primer_index") %>% 
      left_join(df_primer, by = "pos") %>% 
      dplyr::select(-pos)
    return(df_smp)
  })
  
  output$iggypop_smp_info <- DT::renderDataTable({
    DT::datatable(df_iggypop_smp(), 
                  options = list(lengthMenu = c(5, 30, 50), pageLength = 10, scrollX = T))
  })
  
  ## Download
  output$iggypop_dl <- renderUI({
    req(input$Gnrt_Butn)
    div(style = "width: 150px;",
        downloadButton(outputId = "iggypop_dl_df", label = "Download"))
  })
  
  output$iggypop_dl_df <- downloadHandler(
    filename = function() {
      paste0("SampleInfo.tsv")
    },
    
    content = function(file) {
      write.table(x = df_iggypop_smp(), file = file, sep = "\t", quote = FALSE, row.names = FALSE)
    }
  )
  
  
}
