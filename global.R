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


func_hgch_bar_CatNum <- function (data,orientation = 'hor', sort = 'desc', marks = c(',', '.'), colors = '#89909F', nDigits = NULL, format = c("", ""), theme = NULL) 
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
          hc_chart(type = ifelse(orientation == "hor", "bar", "column")) %>% 
            hc_xAxis(title = list(text = " "),  
             type = "category") %>%
               hc_yAxis(title = list(text = "Total"),
                        labels = list(format = formatLabAxis, 
                                      formatter = JS(aggFormAxis))) %>%
                  hc_series(data) %>% 
                   hc_legend(enabled = F)
  if (is.null(theme)) hc <- hc %>% hc_add_theme(custom_theme(custom = theme))

  hc
}

