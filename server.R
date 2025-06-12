# Define server logic
function(input, output, session) {

# Select the navbar automatically -----------------------------------------
  observeEvent(input$input_nav, {
    if (identical(input$input_nav, "input_other")) {
      bslib::nav_select(id = "output_nav", selected = "output_other")
    } else {
      bslib::nav_select(id = "output_nav", selected = "output_iggy")
    }
  })
  observeEvent(input$output_nav, {
    if (identical(input$output_nav, "output_other")) {
      bslib::nav_select(id = "input_nav", selected = "input_other")
    } else {
      bslib::nav_select(id = "input_nav", selected = "input_iggy")
    }
  })
  
# Input --------------------------------------------------------------------
  rv <- reactiveValues()
  observe({
    req(input$n_tb!="", input$n_var!="", input$row!="", input$col!="")
    rv$n_tb <- input$n_tb
    rv$n_var <- input$n_var
    rv$row <- input$row
    rv$col <- input$col
    #iggypop-related
    req(!is_null(iggypop_ref()), input$iggy_rep!="")
    rv$iggy_rep <- as.numeric(input$iggy_rep)
    rv$iggy_n_tb <- ceiling(nrow(iggypop_ref())/(96/rv$iggy_rep))
    
  })
  
## Number of tables --------------------------------------------------------
  
  output$tb_no_ui <- renderUI({
    selectInput(inputId = "tb_no", 
                label = "- Table No.", 
                choices = seq_len(as.numeric(rv$n_tb)))
  })
  
## Number of variables -----------------------------------------------------

  output$var_no_ui <- renderUI({
    selectInput(inputId = "var_no", 
                label = "- Variable No.", 
                choices = seq_len(as.numeric(rv$n_var)))
  })

## Variable Names ----------------------------------------------------------

  output$var_nms_ui <- renderUI({
    ui_list <- lapply(seq_len(as.numeric(rv$n_var)), function(i) {
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
      ui_list <- lapply(seq_len(as.numeric(rv$n_var)), function(i) {
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
     n_var <- as.numeric(rv$n_var)
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
     ui_list <- lapply(seq_len(as.numeric(rv$n_tb)), function(i) {
       lapply(seq_len(as.numeric(rv$n_var)), function(j) {
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
   observeEvent({ rv$n_tb; rv$n_var; rv$row; rv$col; input$var_slt_no }, {
     nr <- as.numeric(rv$row)
     nc <- as.numeric(rv$col)
     
     lapply(seq_len(as.numeric(rv$n_tb)), function(i) {
       lapply(seq_len(as.numeric(rv$n_var)), function(j) {
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
   
   observe({
     req(rv$n_tb, rv$n_var)
     for (i in seq_len(as.numeric(rv$n_tb))) {
       for (j in seq_len(as.numeric(rv$n_var))) {
         key <- paste0("df_", i, "_", j)
         if (!is.null(input[[key]])) {
           rv[[key]] <- hot_to_r(input[[key]])
         }
       }
     }
   })
   
   # Reset the table when rv$row or rv$col changes
   observeEvent({ rv$row; rv$col}, {
     nr <- as.numeric(rv$row)
     nc <- as.numeric(rv$col)
     
     lapply(seq_len(as.numeric(rv$n_tb)), function(i) {
       lapply(seq_len(as.numeric(rv$n_var)), function(j) {
         key <- paste0("df_", i, "_", j)  # Unique key for each table and variable
         rv[[key]] <- matrix(NA, nrow = nr, ncol = nc, 
                             dimnames = list(LETTERS[seq_len(nr)], seq_len(nc))) %>% 
           as.data.frame()
       })
     })
   })
   

   
# Output ----------------------------------------------------------------------
  
  ## Reactive table
  df_tidy <- eventReactive(input$Trsfm_Butn, {
    # required input info.
    n_tb <- isolate({as.numeric(rv$n_tb)})
    n_var <- isolate({as.numeric(rv$n_var)})
    row <- isolate({as.numeric(rv$row)})
    col <- isolate({as.numeric(rv$col)})
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

## Reference Info ----------------------------------------------------------

  iggypop_ref <- eventReactive(input$Ref_upld, {
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
  

## Tidy data ---------------------------------------------------------------

### Recommended -------------------------------------------------------------
  
  # Number of tables
  output$iggy_n_tb_ui <- renderUI({
    req(!is.null(iggypop_ref()))
    tagList(
      h6(tags$b("Recommended Plate Layout")), 
      div(
        style = "display: flex; gap: 20px; align-items: flex-start; margin-bottom: -15px;",
        textInput(inputId = "iggy_n_tb", label = tags$b("Number of plates"), value = rv$iggy_n_tb),
        uiOutput("iggy_tb_no_ui"),
        # Confirm the input
        div(style = "margin-top: 35px", 
            actionButton(inputId = "iggy_Trsfm_Butn",
                         label = "Confirm the layout"))
      ), 
      p("Note: Please select the Plate No. to check the layout for every plate.")
    )
  })
  
  output$iggy_tb_no_ui <- renderUI({
    req(rv$iggy_n_tb != "")
    tagList(
      selectInput(inputId = "iggy_tb_no", 
                  label = tags$b("Plate No."), 
                  choices = seq_len(as.numeric(rv$iggy_n_tb)))
    )
  })
  
  # Render the layout
  output$iggy_var_layout_ui <- renderUI({
    req(rv$iggy_n_tb, rv$iggy_rep)
    if (is.null(input$iggy_n_tb) || input$iggy_n_tb == "") { return(NULL) }
    ui_list <- lapply(seq_len(as.numeric(input$iggy_n_tb)), function(i) {
      conditionalPanel(
        condition = sprintf("input.iggy_tb_no == %d", i),
        
        # title
        h6(tags$b(sprintf("Layout for Plate %d", i))),
        p("The layout for each variable is shown below; feel free to customize it to suit your own experiment."),
        # primer_index
        h6("- primer index"),
        div(style = "overflow-x: auto; max-width: 800px; max-height: 800px;",
            rHandsontableOutput(outputId = paste0("iggy_df_", i, "_", 1))
        ),
        div(style = "margin-top: 15px"), 
        # Replicate
        h6("- Replicate"),
        div(style = "overflow-x: auto; max-width: 800px; max-height: 800px;",
            rHandsontableOutput(outputId = paste0("iggy_df_", i, "_", 2))
        ),
        div(style = "margin-top: 15px"), 
        # PrimerPlate
        h6("- PrimerPlate:"),
        div(style = "overflow-x: auto; max-width: 800px; max-height: 800px;",
            rHandsontableOutput(outputId = paste0("iggy_df_", i, "_", 3))
        ),
        div(style = "margin-top: 15px"), 
        # PrimerWell
        h6("- PrimerWell:"),
        div(style = "overflow-x: auto; max-width: 800px; max-height: 800px;",
            rHandsontableOutput(outputId = paste0("iggy_df_", i, "_", 4))
        )
      )
    })
    tagList(ui_list)
  })
  
  
  observeEvent(
    { input$iggy_n_tb; input$iggy_rep },
    ignoreInit = TRUE,
    {
      
      ## clear old tables & outputs
      isolate({
        old_keys <- grep("^iggy_df_", names(rv), value = TRUE)
        for (k in old_keys) {
          rv[[k]]     <- NULL      # forget cached data-frame
          output[[k]] <- NULL      # deregister old output
        }
      })
      
      ## build the fresh tables
      req(input$iggy_n_tb, input$iggy_rep)
      n_tb <- as.numeric(input$iggy_n_tb)
      rep  <- as.numeric(input$iggy_rep)
      
      lapply(seq_len(n_tb), function(i) {
        lapply(seq_len(4), function(j) {
          
          local({
            ii <- i; jj <- j
            key <- paste0("iggy_df_", ii, "_", jj)
            
            output[[key]] <- renderRHandsontable({
              df <- get_iggy_df(ii, jj, n_tb, rep, iggypop_ref())
              rv[[key]] <- df
              rhandsontable(df)
            })
            
            outputOptions(output, key, suspendWhenHidden = FALSE)
            
          })
          
        })
      })
    }
  )
  
### Customized --------------------------------------------------------------

  # Number of tables
  output$cus_iggy_n_tb_ui <- renderUI({
    req(!is.null(iggypop_ref()))
    tagList(
      h6(tags$b("Customized Plate Layout")),
      div(
        style = "display: flex; gap: 20px; align-items: flex-start; margin-bottom: -15px;",
        textInput(inputId = "cus_iggy_n_tb", label = tags$b("Number of plates"), value = rv$iggy_n_tb),
        uiOutput("cus_iggy_tb_no_ui")
      ),
      p("Note: Please select the Plate No. to check the layout for every plate.")
    )
  })

  output$cus_iggy_tb_no_ui <- renderUI({
    req(input$cus_iggy_n_tb != "")
    tagList(
      selectInput(inputId = "cus_iggy_tb_no",
                  label = tags$b("Plate No."),
                  choices = seq_len(as.numeric(input$cus_iggy_n_tb)))
    )
  })

  # Render the layout
  output$cus_iggy_var_layout_ui <- renderUI({
    if (is.null(input$cus_iggy_n_tb) || input$cus_iggy_n_tb == "") { return(NULL) }
    ui_list <- lapply(seq_len(as.numeric(input$cus_iggy_n_tb)), function(i) {
      conditionalPanel(
        condition = sprintf("input.cus_iggy_tb_no == %d", i),

        # title
        h6(tags$b(sprintf("Layout for Plate %d", i))),
        p("The layout for each variable is provided as an example; please ensure you customize it to fit your own experiment."),
        # primer_index
        h6("- primer index"),
        div(style = "overflow-x: auto; max-width: 800px; max-height: 800px;",
            rHandsontableOutput(outputId = paste0("cus_iggy_df_", i, "_", 1))
        ),
        div(style = "margin-top: 15px"),
        # Replicate
        h6("- Replicate"),
        div(style = "overflow-x: auto; max-width: 800px; max-height: 800px;",
            rHandsontableOutput(outputId = paste0("cus_iggy_df_", i, "_", 2))
        ),
        div(style = "margin-top: 15px"),
        # PrimerPlate
        h6("- PrimerPlate:"),
        div(style = "overflow-x: auto; max-width: 800px; max-height: 800px;",
            rHandsontableOutput(outputId = paste0("cus_iggy_df_", i, "_", 3))
        ),
        div(style = "margin-top: 15px"),
        # PrimerWell
        h6("- PrimerWell:"),
        div(style = "overflow-x: auto; max-width: 800px; max-height: 800px;",
            rHandsontableOutput(outputId = paste0("cus_iggy_df_", i, "_", 4))
        )
      )
    })
    tagList(ui_list)
  })

  # Render each rhandsontable
  observeEvent({ input$cus_iggy_n_tb }, {
    lapply(seq_len(as.numeric(input$cus_iggy_n_tb)), function(i) {
      lapply(seq_len(4), function(j) {
        key <- paste0("cus_iggy_df_", i, "_", j)  # Unique key for each table and variable

        output[[key]] <- renderRHandsontable({
          df <- rv[[key]]

          # If the data frame for this specific table doesn't exist, create one
          if (is.null(df)) {
            if (j == 3) {
              df <- matrix(i, nrow = 8, ncol = 12,
                           dimnames = list(LETTERS[seq_len(8)], seq_len(12))) %>%
                as.data.frame()
            } else {
              df_dft <- read.xlsx("./data/SampleData_IGGYPOP.xlsx", sheet = j)
              df <- df_dft[1:8, 2:13]
              colnames(df) <- seq_len(12)
              rownames(df) <- LETTERS[seq_len(8)]
            }
          }

          # Ensure mode conforms to chosen type
          if (j %in% 2:3) {
            df[] <- lapply(df, as.numeric)
          } else {
            df[] <- lapply(df, as.character)
          }

          rhandsontable(df)  # Render the handsontable

        })
        
        outputOptions(output, key, suspendWhenHidden = FALSE)
      })
    })
  })

  observe({
    req(input$cus_iggy_n_tb)
    for (i in seq_len(as.numeric(input$cus_iggy_n_tb))) {
      for (j in seq_len(4)) {
        key <- paste0("cus_iggy_df_", i, "_", j)
        if (!is.null(input[[key]])) {
          rv[[key]] <- hot_to_r(input[[key]])
        }
      }
    }
  })


### Final data frame --------------------------------------------------------

  ## Reactive table
  df_tidy_iggy <- eventReactive(input$iggy_Trsfm_Butn, {
    # required input info.
    iggy_n_tb <- as.numeric(rv$iggy_n_tb)
    n_var <- 4
    row <- 8
    col <- 12
    # combine all the info.
    df_tidy <- NULL
    for (i in 1:iggy_n_tb) {
      df_tidy_temp <- NULL
      col_names <- c("primer_index", "Replicate", "PrimerPlate", "PrimerWell")
      for (j in 1:4) {
        var_col_name <- col_names[j]
        
        # load the dataframe
        if (input$gnrt_tp == "rec") {
          df_temp <- hot_to_r(input[[paste0("iggy_df_", i, "_", j)]])
        } else {
          df_temp <- hot_to_r(input[[paste0("cus_iggy_df_", i, "_", j)]])
        }
        
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
    
    df_tidy <- df_tidy %>% tidyr::separate(pos, into = c("Plate", "Row", "Column")) %>% drop_na()
    
    return(df_tidy)
  })
  
  output$iggy_tidy_tb <- DT::renderDataTable({
    DT::datatable(df_tidy_iggy(), 
                  options = list(lengthMenu = c(5, 30, 50), pageLength = 5, scrollX = T))
  })

## Output ------------------------------------------------------------------
  
  ## Download
  output$iggy_tidy_dl <- renderUI({
    req(input$iggy_Trsfm_Butn)
    tagList(
      list(
        layout_column_wrap(
          width = NULL, 
          style = css(grid_template_columns = "2fr 1fr 1fr"),
          textInput(inputId = "iggy_tidy_file_name", label = "Download the plate layout: ", 
                    value = Sys.time()),
          selectInput(
            inputId = "iggy_tidy_file_type", 
            label = "File type:", 
            choices = list("EXCEL", "CSV", "TSV", "TXT"),
            selected = "EXCEL",
            width = "150px"
          ),
          # div(style = "margin-top: -40px"),
          div(div(style = "margin-top: 30px"),
              downloadButton(outputId = "iggy_tidy_dl_df", label = "Download"))
        )
      )
    )
  })

  output$iggy_tidy_dl_df <- downloadHandler(
    filename = function() {
      if (input$iggy_tidy_file_type == "EXCEL") { ext <- ".xlsx" } else { ext <- paste0(".", tolower(input$iggy_tidy_file_type))}
      paste0(input$iggy_tidy_file_name, ext)
    },
    
    content = function(file) {
      if (input$iggy_tidy_file_type == "EXCEL") {
        openxlsx::write.xlsx(df_tidy_iggy(), file)
      } else {
        sep <- switch(input$iggy_tidy_file_type, "TXT" = " ", "CSV" = ",", "TSV" = "\t" )
        write.table(x = df_tidy_iggy(), file = file, sep = sep, quote = FALSE, row.names = FALSE)
      }
    }
  )
  
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
      data <- df_tidy_iggy()
    }
    return(data)
  })
  
  iggypop_primer <- eventReactive(input$Gnrt_Butn, {
    if (input$prr_slt == "upld") {
      req(input$prr_file)
      inFile <- input$prr_file
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
      if (input$prr_plt == "pop") {
        data <- read.xlsx("data/pPOP_PrimerPlate_Layout.xlsx", sheet = "TidyFormat")
      } else {
        data <- read.xlsx("data/pPlantPOP_PrimerPlate_Layout.xlsx", sheet = "TidyFormat")
      }
    }
    return(data)
  })
  
  ## Reactive table
  df_iggypop_smp <- eventReactive(input$Gnrt_Butn, {
    # primer_info:
    df_primer <- iggypop_primer() %>% tidyr::unite(PrimerPlate, PrimerWell, sep = "_", col = "pos")
    # plate layout:
    df_plate_layout <- iggypop_df_tidy()
    # ref_seq_info:
    if ("CDS_length" %in% colnames(iggypop_ref())) {
      slt_var <- c("primer_index", "n_frags", "CDS_length", "ReferenceName", "ReferenceSequence")
    } else {
      slt_var <- c("primer_index", "n_frags", "ReferenceName", "ReferenceSequence")
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
