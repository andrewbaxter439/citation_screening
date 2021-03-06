
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    tags$head(tags$style(HTML(
        'h1 {
        font-family: "nimbus-sans",sans-serif;
        }
        .boxsection {
             border-radius: 10px;
             margin: 10px;
             background-color: #adcfeb;
             padding: 10px;
             overflow-wrap: break-word;
        }
        ::selection {
        background: #FF00CC;
        color: #FFFFFF
        }'
    )),
    tags$script(HTML("
    
            document.onkeydown = getSelectionText;
        
        
        function getSelectionText() {
            var e = e || window.event;  
            var s = String.fromCharCode(e.keyCode);
            
                 if(e.keyCode == 13){
                    document.getElementById('wok_search').click();
                  } else if (window.getSelection().toString() == '') {
                var text = '';
            } else if (s == 'A') {
                var text = document.getElementById('authors');
                Shiny.setInputValue('authors', window.getSelection().toString());
            } else if (s == 'T') {
                var text = document.getElementById('title');
                Shiny.setInputValue('title', window.getSelection().toString());
            } else if (s == 'U') {
                var text = document.getElementById('url');
                Shiny.setInputValue('url', window.getSelection().toString());
            }
            
            text.value = window.getSelection().toString();
            
        }
    "))),
    tags$head(tags$script(src = "message-handler.js")),
    
    # Sidebar with a slider input for number of bins
    
    column(2),
    column(6,
    h1(style = "margin: 10px 0px 10px 0px;", "Finding full-texts"),
           
           
           
           fluidRow(class = "boxsection",
                    style = "min-height: 160px",
                    h2(textOutput("reference")),
                    p(textOutput("full_ref"))
           ),
           fluidRow(class = "boxsection",
                    style = "min-height: 315px",
                    uiOutput("resettable_inputs")
           ),
           fluidRow(class = "boxsection",
                    div(style="display: inline-block;vertical-align:top;", h4("Action:")),
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
                        div(style = "margin: 10px 0px 0px 20px; font-size: 1.25em;", 
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
           plotOutput("prog_graph", height = "100px"),
           DT::dataTableOutput("prog_tab")
           )
)
)
