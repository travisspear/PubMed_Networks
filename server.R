library(shiny)
library(RISmed)
library(igraph)
library(shinyWidgets)
library(visNetwork)
library(tidyverse)
library(data.table)

shinyServer(function(input, output, session) {
  
  getFirst <- function(x){
    
    paste(str_extract(x$Initials[1], "[[:alpha:]]{1}"), x$LastName[1])
    
  }
  
  graphData <- function(x){ # Function to get retrieve data from PubMed and convert it into an author network.
    
    query <- EUtilsSummary(x, retmax = 1500, reldate = 30) # Create Query
    dat <- EUtilsGet(query) # Get data
    
    df <- data.table( # Pull Id and Author from dat
      id = PMID(dat),
      authors = Author(dat)
    ) %>%
      mutate(
        firstAuthor = sapply(authors, function(auth) getFirst(auth)) # Identify first author
      ) %>%
      unnest(authors) %>% # Unnest the complete author list alongside the first author column
      filter(order != 1) %>% # Remove First authors from total author column
      mutate(
        otherAuthors = paste(str_extract(Initials, "[[:alpha:]]{1}"), LastName) # Format other author names like the first author
      ) %>%
      select(-Initials, -LastName, -ForeName, -order) %>% # Exclude unnecessary columns
      filter(otherAuthors %in% firstAuthor) # Only display authors who appear as the first author of at least one paper. The majority of authors appear only in isolation and are thus not important for a graph based on connections. 
    

    graph <- df %>%
      select(-id) %>%
      graph_from_data_frame( # Create a graph from the author dataframe using igraph
        directed = T
      ) 

    return(graph)
    
  }
  
  graph <- readRDS("alz_graph.rds") # Base data import for Alzheimer's Disease query
  
  observe({
    # Show a simple modal
    shinyalert(title = "You did it!", type = "success")
  })
  
  output$graph_Query <- renderText({ # Display the query used for the current graph
    
    text<- if(input$input_QueryText == ""){
      
      EUtilsSummary("Alzheimer's Disease", retmax = 1500, reldate = 30)
      
    } else {
      
      EUtilsSummary(input$input_QueryText, retmax = 1500, reldate = 30)
      
    }
    
    print(text)
    
  })
  
  
  output$graph_Network <- renderVisNetwork({
    
    input$input_QueryText # Wait for the seach query input
    withProgress(message = 'Creating Plot', style = 'notification', value = 0, {
      
      incProgress(0.1)
      data <- if(input$input_QueryText == ""){ # Display graph based on base data on start up, else utilize user input. 
        graph
      } else {
        graphData(input$input_QueryText)
      }

      })

    
    visIgraph(data) %>%
      visEdges(
        arrows = list(
          enabled = F 
        ),
        color = list(
          color = "#7b9691",
          highlight = "#c3594d"
        )
      ) %>%
      visOptions(
        highlightNearest = list(
          algorithm = "hierarchical",
          enabled = T, 
          degree = list(from = 1, to = 2)
        ),
        collapse = T 
      ) %>%
      visNodes(
        color = list(
          background = "#6eb3a4",
          border = "#5e7169",
          highlight = "#c3594d"
        )
      ) 
    
  })
  
})
