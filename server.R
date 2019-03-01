shinyServer(function(input, output, session){
  
output$botEncues <- renderUI({
  
  dicBE <- dicEncu %>% distinct(clases, .keep_all = TRUE)
  temas <- unique(dicBE$label)
  
  l <- purrr::map(temas, function(z){
    HTML( paste0('<div class = "contMenu">',
                 tags$button(id = z, class = 'butTemas', type = "button", dicBE$labelOf[dicBE$label == z]
                 ),
                 ' <div class = "dropdownMenuFicha">
                 <div class = "dropdownMenuContent">',
                 paste('<a id = "', dicBE$id[dicBE$label == z], '" href = "#" class = "itemEC">', dicBE$clases[dicBE$label == z] ,'</a>', collapse = ''), '</div></div>
                 </div>'
    ))
  })
  l[[1]] <- gsub('butTemas', 'butTemas activEnc', l[[1]])
  HTML(paste0('<div class = "contBotones">', paste(l, collapse = ''), '</div>'))
})



baseFiltroEncuesta <- reactive({
  
  idEnc <- trimws(input$last_enc)
  if (is.null(idEnc) | identical(idEnc, character(0))) idEnc <- 'encuesta_interceptacion_transmilenio_2011'
  
  if (idEnc == 'volumen_vehicular') {
    df <- read_csv('data/volumen_vehicular_data.csv')
  } else {
  db <- src_sqlite("data/db.sqlite3")
  d <- paste0('SELECT * FROM ', idEnc, '_data')
  df <- tbl(db, sql(d))
  }
  df
  
})



dicFiltroEncuesta <- reactive({

  idEnc <- trimws(input$last_enc)
  if (is.null(idEnc) | identical(idEnc, character(0))) idEnc <- 'encuesta_interceptacion_transmilenio_2011'
  db <- src_sqlite("data/db.sqlite3")
  d <- paste0('SELECT * FROM ', idEnc, '_dic_')
  df <- tbl(db, sql(d)) %>% collect()
  
  df$label <- Hmisc::capitalize(tolower(df$label))
  df
})



# 
selecFilEnc <- reactive({
  idEnc <- trimws(input$last_enc)
  if (is.null(idEnc) | identical(idEnc, character(0))) idEnc <- 'encuesta_interceptacion_transmilenio_2011'

  d <- dicEncu %>% filter(id %in% idEnc) %>% select(id = variables)
  d <- d %>% left_join(dicFiltroEncuesta())
  d
})


output$selectorItems <- renderUI({
  idEnc <- trimws(input$last_enc)
  if (is.null(idEnc) | identical(idEnc, character(0))) idEnc <- 'encuesta_interceptacion_transmilenio_2011'

  dd <- unique(dicEncu$dicc[dicEncu$id == idEnc])

  varn <- as.list(setNames(selecFilEnc()$id, selecFilEnc()$label))

  if (dd) {
    selectizeInput('varChoose', '', varn)
  } else {
    return()
  }
})


output$tabla <- renderPrint({
  dicFiltroEncuesta()
})



output$GrafBar <- renderUI({

  ff <- trimws(input$last_enc)
  if (is.null(ff) | identical(ff, character(0))) ff <- 'encuesta_interceptacion_transmilenio_2011'


  if (ff != 'volumen_vehicular') {
    a <- 'img/'
  } else {
    a <- 'img/otros/'
  }

  map(c('bar', 'pie', 'tree'), function(z){
    tags$button(id = z, class = 'barType', type = "button",
                tags$img(src = paste0(a, z, '.png'))
    )
  })

})



filterEncu <- reactive({
  ff <- trimws(input$last_enc)
  if (is.null(ff) | identical(ff, character(0))) ff <- 'encuesta_interceptacion_transmilenio_2011'

  varS <- input$varChoose
  base <- ff
  dd <- unique(dicEncu$dicc[dicEncu$id == base])
  np <- dicFiltroEncuesta()$label[dicFiltroEncuesta()$id == varS]


  #if (dd) {
    if (ff != 'volumen_vehicular') {
      d <- baseFiltroEncuesta() %>% select_(varS)
      d <- d %>% group_by_(varS) %>% summarise(total = n()) %>% collect()
      names(d) <- c(np, 'Total')
    } else {
      filR <- input$viaElj
      d <- baseFiltroEncuesta() %>% select_('hora', 'via', varS) 
      d <- d %>% filter(via %in% filR) 
      d$hora <- as.character(d$hora)
    }
  # } else {
  #   d <- baseFiltroEncuesta() %>% collect()
  # }

  d

})

output$blaasd <- renderTable({
  filterEncu()
})
# 
# 
output$vizEnc <- renderHighchart({

  graf <- input$last_graf
  if (is.null(graf)) graf <- 'bar'

  ff <- trimws(input$last_enc)
  if (is.null(ff) | identical(ff, character(0))) ff <- 'encuesta_interceptacion_transmilenio_2011'


  if (graf == 'bar') {
    if (ff != 'volumen_vehicular') {
      h <- func_hgch_bar_CatNum(data = filterEncu(), orientation = 'hor', sort = 'desc',  marks = c(',', '.'), colors = '#89909F', nDigits = 0)
    } else{
      h <- hgchmagic::hgch_bar_grouped_CatCatNum(filterEncu(), orientation = 'hor', sort = 'desc', verLabel = ' ', theme = caf_theme, colors = '#0b356D')
    }
  }
  if (graf == 'pie') {
    if (ff != 'volumen_vehicular') {
      h <- hgchmagic::hgch_pie_CatNum(filterEncu(), theme = caf_theme)
    } else{
      s <- filterEncu() %>% select(via, everything())
      h <- hgchmagic::hgch_line_CatOcaNum(s,  theme = caf_theme)
    }
  }
  if (graf == 'tree') {
    if (ff != 'volumen_vehicular') {
      h <- func_hgch_treemap_CatNum(filterEncu(), title = ' ',minColor = '#2A7F62', maxColor = '#89909F')
    } else {
      h <- hgch_bar_stacked_100_CatCatNum(filterEncu(),  theme = caf_theme, horLabel = ' ', verLabel = ' ')
    }}
  h
})


output$UIvizEnc <- renderUI({
  highchartOutput('vizEnc', height = "470px", width = "590px")
})


# 
output$radioBut <- renderUI({

  j <- trimws(input$last_enc)
  if (is.null(j)) j <- 'nada'
  j <-  sub("\\).*", "", sub(".*\\(", "", j))
  if (length(j) < 1) return()

  if (j == 'volumen_vehicular') {
    varsNu <- unique(baseFiltroEncuesta() %>% dplyr::select(via) %>% collect() %>% .$via)
    selectizeInput('viaElj', 'Vía de la intersección en estudio',
                   varsNu, multiple = TRUE,
                   selected = sample(varsNu,2),
                   options = list(#maxItems = 1,
                     plugins= list('remove_button'))
    )
  } else {
    return()
  }

})
# 
# 
# 
# 
output$textDescr <- renderUI({
  ff <- trimws(input$last_enc)
  if (is.null(ff) | identical(ff, character(0))) ff <- 'encuesta_interceptacion_transmilenio_2011'

  HTML('<b>Descripción:</b>', dicEncu$labelDic[dicEncu$id == ff][1],
       ' <i>(Año 2011).</i>'
  )
})
# 
# 
# 
output$idDownEnc <- downloadHandler(
  "all_data.zip",
  content = function(file) {
    dir.create(tmp <- tempfile())
    df <- baseFiltroEncuesta()
    dic <- dicFiltroEncuesta()
    write_csv(df, file.path(tmp, "data_all.csv"), na = '')
    write_csv(dic, file.path(tmp, "dic_all.csv"), na = '')
    zip(file, tmp)
  })
})