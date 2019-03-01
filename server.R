library(shiny)
library(shinyjs)
library(tidyverse)
library(leaflet)
library(geojsonio)
library(hgchmagic)
library(zip)
library(RSQLite)
library(datafringe)
library(DT)

hcoptslang <- getOption("highcharter.lang")
hcoptslang$contextButtonTitle <- 'Descargar Imagen'
hcoptslang$printChart <- "Imprimir Gráfico"
hcoptslang$downloadJPEG <- "Descarga en JPEG"
hcoptslang$downloadPNG <- "Descarga en PNG"
hcoptslang$downloadPDF <- "Descarga en PDF"
hcoptslang$downloadSVG <- "Descarga en SVG"
hcoptslang$thousandsSep <- ","
hcoptslang$decimalPoint <- "."
options(highcharter.lang = hcoptslang)


caf_theme <- hc_theme(
  colors = c('#2A7F62','#C3ACCE', '#538083', '#89909F', '#DFD9E2', '#2c6444'),#c('#0b356D', '#3F8909', '#ACA9A9','#CD7031','#1670D2'),
  chart = list(
    backgroundColor = "transparent"
  ),
  title = list(
    style = list(
      color = '#333333',
      fontFamily = "Open Sans",
      textDecoration= 'none'
    )
  ),
  legend = list(
    itemStyle = list(
      fontFamily = '',
      color = 'black'
    ),
    itemHoverStyle = list(
      color = 'gray'
    )
  )
)




dicEncu <- read_csv('data/generalEnc.csv')


func_hgch_treemap_CatNum <- function (data, title = NULL, subtitle = NULL, caption = NULL, 
                                      minColor = "#E63917", maxColor = "#18941E", back_color = "white", 
                                      color_title = "black", reverse = TRUE, export = FALSE, ...) 
{
  f <- fringe(data)
  nms <- getClabels(f)
  title <- title %||% nms[2]
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""
  data <- f$d
  
  count_pl <- function(x) {
    if (is.na(x)) {return(0)}
    if ((x %% 1) != 0) {
      nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
    } else {
      return(0)
    }
  }
  
  d <- data %>% na.omit() %>% dplyr::group_by(a) %>% dplyr::summarise(b = mean(b))
  d$w <- map_chr(d$b, function(x) format(round(x, 2), nsmall = (ifelse(count_pl(x) > 
                                                                         2, 2, 0)), big.mark = ","))
  hc <- hchart(d, "treemap", hcaes(x = a, value = b, color = b)) %>% 
    hc_chart(backgroundColor = back_color) %>% hc_title(text = title, 
                                                        style = list(color = color_title, useHTML = TRUE)) %>% 
    hc_subtitle(text = subtitle) %>% hc_credits(enabled = TRUE, 
                                                text = caption) %>% hc_colorAxis(maxColor = maxColor, 
                                                                                 minColor = minColor) %>% hc_tooltip(pointFormat = "\n                     {point.a}: {point.w}\n                  ")
  if (reverse) 
    hc <- hc %>% hc_colorAxis(maxColor = minColor, minColor = maxColor)
  if (export) 
    hc <- hc %>% hc_exporting(enabled = TRUE)
  hc
}


func_hgch_bar_CatNum <- function (data, sort = 'desc', marks = c(',', '.'), colors = '#89909F', nDigits = NULL, format = c("", ""), ...) 
{
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d
  d <- sortSlice(d, "b", sort, NULL)
  d$color <- colors
  d$a <- as.character(d$a)
  
  data <- list()
  bla <- map(1:nrow(d), function(z) {
    data$data[[z]] <<- list(name = d$a[z], y = d$b[z], color = as.character(d$color[z]))
  })
  formatLabAxis <- paste0("{value:", marks[1], marks[2], "f}")
  
  
  if(is.null(nDigits)) nDigits <- 2
  
  if (!is.null(nDigits)) {
    formatLabAxis <- paste0("{value:", marks[1], marks[2], 
                            nDigits, "f}")
  }
  if (is.null(format)) {
    format[1] = ""
    format[2] = ""
  }
  aggFormAxis <- "function() {return this.value+\"\";}"
  
  aggFormAxis <- paste0("function() { return '", format[1], 
                        "' + Highcharts.numberFormat(this.value, ", nDigits, ", '", 
                        marks[2], "', '", marks[1], "') + '", format[2], "'}")
  
  hc <- highchart() %>% 
    hc_chart(type =  "bar") %>% 
    hc_xAxis(title = list(text = " "),  
             type = "category") %>%
    hc_yAxis(title = list(text = "Total"),
             labels = list(format = formatLabAxis, 
                           formatter = JS(aggFormAxis))) %>%
    hc_series(data) %>% 
    hc_legend(enabled = F)
  hc
}


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

  if (is.null(dd)) return() 
  
  selectizeInput('varChoose', '', varn)
  
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
      h <- func_hgch_bar_CatNum(data = filterEncu(), sort = 'desc',  marks = c(',', '.'), colors = '#89909F', nDigits = 0)
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