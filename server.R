library(shiny)
library(readr)
library(dplyr)
library(googlesheets)
library(httr)
library(rscopus)
library(stringr)
library(ggplot2)
library(tidyr)
library(SPHSUgraphs)


shinyServer(function(input, output, session) {


# reactive inputs ---------------------------------------------------------
output$resettable_inputs <- renderUI({
  times <- input$write + input$asis + input$maybe
  
  div(id=letters[(times %% length(letters)) + 1],
  textInput("authors", "Authors (a):"),
  textInput("title", "Title (t):"),
  textInput("url", "url (u):"),
  div(style="display: inline-block;vertical-align:top;", radioButtons("type", "Citation type:", choices = c("Journal article", "News report", "Government/official report", "Other:"), inline = TRUE)),
  div(style="display: inline-block;vertical-align:top;", textInput("othertype", "", width = "150px")))
})
# reading in data ---------------------------------------------------------

full_df <- read_csv("data/Screen_input.csv", col_types = cols("dir" = "c", "nrefs" = "i"))

sheet <- gs_title("Ongoing results screening")

progress <- gs_read(sheet, ws = "Progress")

complete <- gs_read(sheet, ws = "complete", col_types = cols("dir" = "c", "nrefs" = "i", "url" = "c"))

todo <- full_df %>% 
  filter(!(Reference %in% complete$Reference))

# choosing record to display ----------------------------------------------

incr <- reactiveVal(1)
  
rec <- reactive({todo[incr(), ]})

output$reference <- renderText({
  rec()$Reference
})

output$full_ref <- renderText({
  rec()$`Full Reference`
})

name1 <- reactive(input$authors %>%  str_extract("^\\w+ ?") %>% str_trim())

search_text <- eventReactive(input$title, {paste0("TITLE(",
                                                  input$title %>% 
                                                    str_replace_all("[:punct:]", " ") %>% 
                                                    str_trim() %>% 
                                                    str_squish(),
                                                  ") AND AUTHOR-NAME(",
                                                  name1(), ")")})

output$search <- renderText(search_text())

search_done <- reactiveVal("")

search_result <- eventReactive(input$wok_search, {
  results <- scopus_search(search_text(), count = 1) 
  results$entries[[1]]
})

  # search_done <- eventReactive(input$wok_search,{"done"})

observeEvent(input$wok_search, search_done("done"))

output$searchdone <- renderText(search_done())

output$search_error <- renderText(search_result()$error)
output$title <-   renderText(search_result()$`dc:title`)
output$author <-  renderText(search_result()$`dc:creator`)
output$journal <- renderText(search_result()$`prism:publicationName`)
output$doi <-     renderText(search_result()$`prism:doi`)

submit <- reactive({c(input$write, input$maybe, input$asis)})

observeEvent(input$write,
  {
    session$sendCustomMessage(type = 'testmessage', message = 'done')
    
    if (!is.null(search_result()$error)) {
      
      rec() %>% 
        mutate(
          title = input$title,
          au1 = input$authors,
          url = input$url %>% str_remove_all("( |\n|\r)"),
          type = ifelse(input$type == "Other:", input$othertype, input$type),
          maybe = 0
        ) %>% 
        gs_add_row(sheet, ws = "complete", input = .)
      newval <- incr() + 1
      incr(newval)
      search_done("")
      
    } else {
    
      rec() %>% 
        mutate(
          title = search_result()$`dc:title`,
          au1 = search_result()$`dc:creator`,
          doi = search_result()$`prism:doi`,
          scopus_id = search_result()$`dc:identifier`,
          journal = search_result()$`prism:publicationName`,
          url = input$url %>% str_remove_all("( |\n|\r)"),
          type = ifelse(input$type == "Other:", input$othertype, input$type),
          maybe = 0
        ) %>% 
        gs_add_row(sheet, ws = "complete", input = .)
      newval <- incr() + 1
      incr(newval)
      search_done("")
    }
  })

observeEvent(input$asis,
{
      rec() %>% 
        mutate(
          title = input$title,
          au1 = input$authors,
          url = input$url %>% str_remove_all("( |\n|\r)"),
          type = ifelse(input$type == "Other:", input$othertype, input$type),
          maybe = 0
        ) %>% 
        gs_add_row(sheet, ws = "complete", input = .)
      newval <- incr() + 1
      incr(newval)
      search_done("")
  })

observeEvent(input$maybe,
{
      rec() %>% 
        mutate(
          title = input$title,
          au1 = input$authors,
          url = input$url %>% str_remove_all("( |\n|\r)"),
          type = ifelse(input$type == "Other:", input$othertype, input$type),
          maybe = 1
        ) %>% 
        gs_add_row(sheet, ws = "complete", input = .)
      newval <- incr() + 1
      incr(newval)
      search_done("")
  })


# graphs of progress ------------------------------------------------------

prog_df <- reactive({
  
  done <-       min(max(nrow(complete) + incr() -1, nrow(complete)), nrow(full_df))
  rem <-        nrow(full_df) - done
  
  done_rank1 <- min(max(nrow(complete %>% filter(nrefs>2)) + incr() -1, nrow(complete %>% filter(nrefs>2))), nrow(full_df %>% filter(nrefs>2)))
  rem_rank1 <-  nrow(full_df %>% filter(nrefs>2)) - done_rank1
  
  done_rank2 <- min(max(nrow(complete %>% filter(nrefs>1)) + incr() -1, nrow(complete %>% filter(nrefs>1))), nrow(full_df %>% filter(nrefs>1)))
  rem_rank2 <-  nrow(full_df %>% filter(nrefs>1)) - done_rank2
  
  
tibble(
    done = c(done_rank1, done_rank2, done),
    rem = c(rem_rank1, rem_rank2, rem),
    group = factor(3:1, labels = c("All references", "Cited > 1 times", "Cited > 2 times"), ordered = TRUE)
  ) %>% 
    gather("count", "n", -group)
  
})

output$prog_graph <- renderPlot({
  ggplot(prog_df(), aes(x = group, y = n, fill = count)) +
    geom_bar(position = "fill", stat = "identity") +
    coord_flip() +
    scale_y_reverse() +
    theme(rect = element_blank(),
          line = element_blank(),
          axis.title = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = "20"),
          legend.position = "none") +
    scale_fill_sphsu()
})


# temp table --------------------------------------------------------------

output$prog_tab <- DT::renderDataTable({
  prog_df() %>% 
    spread(count, n) %>% 
    select(References = group, Done = done, Remaining = rem) %>% 
    arrange(Remaining)
  }, options = list(searching = FALSE, paging = FALSE, info = FALSE), rownames = FALSE)
})
