library(shiny)
library(reactlog)
library(plotly)
library(vroom)
library(tidyverse)
library(dplyr)
library(DT)

if (!exists("injuries")) {
  load(file = "injuries.rda")
  load(file = "population.rda")
  load(file = "products.rda")
}

injuries_2017 <-injuries %>% filter(trmt_date >= '2017-01-01' & trmt_date <= '2017-12-31')
injuries_2017 <- injuries_2017 %>% mutate_at(4, tolower)
injuries_2017 <- injuries_2017 %>% mutate_at(3, round, 0)
injuries_2017 <- injuries_2017 %>% rename(prod_code=prod1)
population_2017 <- population %>% filter(year == 2017)
population_2017<- population_2017 %>% rename('population' = 'n') 
products_2017 <- products %>% rename('prod_code' = 'code')


# injuries_2017 %>% pull(narrative) %>% sample(2)

# injuries_2017 
# products_2017 %>% filter(prod_code==1842)


Sorted_ProductName <- setNames(products_2017$prod_code, products_2017$title)
Sorted_ProductName <- Sorted_ProductName[order(names(Sorted_ProductName))]

Cnt_inj <- injuries_2017[,c("prod_code","weight")] %>% 
            left_join(products_2017, by = c("prod_code")) %>% 
            group_by(prod_code, title) %>% summarize(Weighted_count= round(sum(weight)), Total_count= n()) %>%
            arrange(desc(Weighted_count)) %>% 
            rename('Title' = 'title')

url <- a("Mastertering Shiny", href="https://mastering-shiny.org/")
#view(Cnt_inj)

#<< ui
ui <- fluidPage(
  titlePanel("Shiny experiment - products causing ER injuries (2017)"),
  h4("Data from the National Electronic Injury Surveillance System (NEISS), collected by the Consumer Product Safety Commission"),

  h5("Source: Chapter-5 from ", url),
  
  hr(),
  sidebarLayout(
    sidebarPanel(h3("Top 15 products"),
                 br(),
      #fluidRow(column(4, DT::dataTableOutput("product") ) )
      DT::dataTableOutput("product")
    ),
  mainPanel(
    h3("Distribution of injury by Age & Sex"),
    br(),
    fluidRow(
        column(8,selectInput("code", "Product", choices =  Sorted_ProductName, width = "100%" ) ),
        column(4, selectInput("y", "Y axis", c("rate", "count")))
        ),
      # hr(),
      fluidRow(
        column(4, tableOutput("diag")),
        column(4, tableOutput("body_part")),
        column(4, tableOutput("location"))
        ),
      hr(),
      fluidRow(
        column(12, plotOutput("age_sex"))
        ),
      hr(),
      fluidRow(
        column(2, actionButton("story", "Tell me a story")),
        column(10, textOutput("narrative"))
        )
      ))
)
#>>

#<< count_top
count_top <- function(df, var, n = 5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}
#>>

#<< server
server <- function(input, output, session) {
  selected <- reactive(injuries_2017 %>% filter(prod_code == input$code))
  
  # output$product <- renderDataTable(Cnt_inj[,c("Title","Weighted_count")])
  
  output$product <- DT::renderDataTable({
    DT::datatable(Cnt_inj[,c("Title","Weighted_count")], options = list(lengthMenu = c(15, 30, 50), pageLength = 15))
  })
  
  output$diag <- renderTable(count_top(selected(), diag), width = "100%")
  output$body_part <- renderTable(count_top(selected(), body_part), width = "100%")
  output$location <- renderTable(count_top(selected(), location), width = "100%")
  
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population_2017, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  output$age_sex <- renderPlot({
    if (input$y == "count") {
      summary() %>%
        ggplot(aes(age, n, colour = sex)) +
        geom_line() +
        labs(y = "Estimated number of injuries")
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people")
    }
  }, res = 96)
  
  output$narrative <- renderText({
    input$story
    selected() %>% pull(narrative) %>% sample(1)
  })
  
}


#>>

shinyApp(ui, server)