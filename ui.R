library(shiny)
library(RISmed)
library(igraph)
library(shinyWidgets)
library(visNetwork)
library(tidyverse)
library(data.table)
library(shinyjs)
library(shinydashboardPlus)
library(shinydashboard)

shinyUI(
  fluidPage(
    tags$head(
      tags$style("
                  body{
                  }
                  .shiny-notification {
                     position: fixed;
                     left: 25%;
                     bottom: 2%;
                     right: 1%;
                  }
                  #info {
                    background-color: #fff;
                    box-shadow: 0 8px 8px 0 rgba(0, 0, 0, 0.4), 0 12px 20px 0 rgba(0, 0, 0, 0.2);
                    height: 99vh;
                    overflow-y: scroll;
                  }
                  ::-webkit-scrollbar {
                    width: 0px;  /* remove scrollbar space */
                    background: transparent;  /* optional: just make scrollbar invisible */
                  }
                  #settings_button {
                    background-color: #6eb3a4;
                  }
                 

                 ")
      ),
    setBackgroundColor(color = '#eff1f4'),
    useShinyjs(),
    
    fluidRow(
      width = 12,
      column(
        width = 3,
        wellPanel(
          id = "info",
          width = "100%",
            tags$p(
              tags$h2(tags$b("PubMed Author Network")),
              tags$hr(),
              searchInput(
                inputId = "input_QueryText",
                label = NULL, 
                placeholder = "Alzheimer's Disease",
                btnSearch = icon("search"), 
                width = "100%"
              ),
              tags$hr(),
              tags$h3("About"),
              tags$p("Explore the connections of recent authors from PubMed publications. Every point on the graph represents a 'First Author' on a paper and the connections between points shows which other authors are connected through co-authorship."),
              tags$p("Click on nodes and zoom in to see connections and author names."),
              tags$p("Change the graph by searching for your disease, treatments, or other parameters of interest in PubMed!"),
              tags$hr(),
              actionBttn(
                inputId = "settings_button",
                label = NULL,
                icon = icon("gear"), 
                style = "material-flat",
                color = "warning",
                block = T
                
              ),
              hidden(
                div(
                  id = "settings_panel",
                  tags$h3("Query Information"),
                  tags$hr(),
                  tags$h4("Current Query:"),
                  verbatimTextOutput("graph_Query"),
                  tags$hr(),
                  tags$h4("Dates"),
                  tags$p("30 Days is the default for the queried date range. If you would like to see back further, change this with the slider below."),
                  tags$p(tags$b("Warning: "), "Increasing the date range will drastically increase query time."),
                  sliderInput(
                    inputId = 'input_DateRange',
                    label = NULL,
                    value = 30,
                    step = 1,
                    min = 1,
                    max = 365
                    )
                  )
                )
              )
        )
      ),
      column(
        width = 8,
        fluidRow(
          width = 12,
          visNetworkOutput("graph_Network", height = "97vh", width = "74vw")
          )
        )
      )
    )
) 