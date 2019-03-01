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

shinyUI(
  fluidPage(
    useShinyjs(),
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$img(src = 'Cargando.gif', class="loadmessage")),
    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="style.css"),
      includeScript("js/iframeSizer.contentWindow.min.js"),
      includeScript("js/caf.js")
    ),
    div( class ="menuEncuesta",
         uiOutput('botEncues'),
         div(class = 'selEncu',
             uiOutput('selectorItems'),
             uiOutput('radioBut')
         )),
    div(class = 'twoCols',
        div(class ="averSel",
            
            div(class = "scrollTab", tableOutput('blaasd')),
            div(style = "margin-top: 5%;margin-bottom:3%;",
                downloadButton('idDownEnc', 'Descarga los datos')),
            uiOutput('textDescr')),
        
        div(class = 'JunGraViz',
            uiOutput('UIvizEnc'),
            uiOutput('GrafBar'))
    )
  )
)