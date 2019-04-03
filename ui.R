library(shiny)
library(RISmed)
library(igraph)
library(shinyWidgets)
library(visNetwork)
library(tidyverse)
library(data.table)
library(shinyalert)

shinyUI(
  fluidPage(
    tags$head(
      tags$style("
                  .shiny-notification{
                     position: fixed;
                     left: 30%;
                     bottom: 1%;
                     right: 30%;
                   }
                 "),
      tags$script(
        "
    $(window).on('load',function(){
        $('#myModal').modal('show');
        });
        "
        )
      ),
    setBackgroundColor(color = '#eff1f4'),
    

    column(
      width = 6,
      offset = 3,
      searchInput(
        inputId = "input_QueryText",
        label = "", 
        placeholder = "Alzheimer's Disease",
        btnSearch = icon("search"), 
        width = "100%"
      )
    ),
    fluidRow(
      width = 12,
        visNetworkOutput("graph_Network", height = "95vh", width = "100vw")
      ),
    absolutePanel(
      bottom = 15, 
      left = 15,
      dropdownButton(
        up = T,
        circle = T,
        status = "warning",
        size = "default",        
        icon = icon("question"), 
        width = "40vw",
        fluidRow(
          width = 10,
          column(
            width = 10,
            offset = 1,
            tags$p(
              tags$h3("PubMed Author Network"),
              tags$p("This is an Author - Co-Author network generated from recently published clinical trials and can be used to identify currently influential people in your field of interest. Every author appearing in this graph are the first author for at least one publication."),
              tags$p("Queries are limited to the studies published in the past 30 days for performance reasons. The PubMed API is rate limited and querying large datasets can take several minutes."),
              tags$p("To change the graph use the search bar to search for any Diseases, Treatments, or other parameters of interest."),
              tags$hr(),
              tags$p("The following query has been used to generate the current graph:")
            ),
            verbatimTextOutput("graph_Query")
            )
          )
        )
      )
    )
) 