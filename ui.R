
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    tags$head(tags$style(HTML(
        ".boxsection {
        border-radius: 10px;
             margin: 10px;
             background-color: #eeeeee;
             padding: 10px;
             overflow-wrap: break-word;
        }"
    ))),
    tags$head(tags$script(src = "message-handler.js")),
    
    # Sidebar with a slider input for number of bins
    
    column(2),
    column(6,
    h1(style = "margin: 20px 0px 20px 0px;", "Finding full-texts"),
           
           
           
           fluidRow(class = "boxsection",
                    style = "min-height: 200px",
                    h2(textOutput("reference")),
                    p(textOutput("full_ref"))
           ),
           fluidRow(class = "boxsection",
                    style = "min-height: 315px",
                    uiOutput("resettable_inputs")
                    # textInput("authors", "Authors:"),
                    # textInput("title", "Title:"),
                    # textInput("url", "url:"),
                    # div(style="display: inline-block;vertical-align:top;", radioButtons("type", "Citation type:", choices = c("Journal article", "News report", "Government/official report", "Other:"), inline = TRUE)),
                    # div(style="display: inline-block;vertical-align:top;", textInput("othertype", "", width = "150px"))
           ),
           fluidRow(class = "boxsection",
                    h4("Action:"),
                    div(style="display: inline-block;vertical-align:top;", actionButton("wok_search", "Search Scopus")),
                    div(style="display: inline-block;vertical-align:top;", actionButton("maybe", "Maybe pile")),
                    div(style="display: inline-block;vertical-align:top;", actionButton("asis", "Add above info"))
                    ),
           div(style = "font-size: 1px; color: #FFFFFF", textOutput("searchdone")),
           conditionalPanel(
               condition = "output.searchdone == 'done'",
               fluidRow(class = "boxsection",
                        h4("Search results:"),
                        div(style = "display: inline", "Search terms: ", tags$i(textOutput("search", inline = TRUE))),
                        br(),
                        div(style = "margin: 10px 0px 10px 20px; font-size: 1.25em;", 
                            tags$i("Article found: "),
                            span(style = "color: red;", textOutput("search_error")),
                            textOutput("title"),
                            textOutput("author"), 
                            textOutput("journal"), 
                            textOutput("doi")
                        ),
                        br(),
                        actionButton("write", "Add to records")
               )
           )
           
           
           
           
    ),
    column(4,
           h3("Progress"),
           plotOutput("prog_graph", height = "100px")
           )
)
)
