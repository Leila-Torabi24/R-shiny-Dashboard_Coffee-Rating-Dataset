
#install.packages("shinydashboard")
#install.packages("shiny")


library(shinydashboard)
library(shiny)
library(tidyverse)
library(kableExtra)
library(broom)



coffee_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

ui <- dashboardPage(
  dashboardHeader(title = "Coffee Dashboard"),
  dashboardSidebar(
    selectInput("v_country", "Country",
                choices = coffee_ratings %>% 
                  select(country_of_origin) %>% 
                  distinct() %>% 
                  arrange(country_of_origin) %>% 
                  drop_na()
    )
  ),
  dashboardBody(
    fluidRow(box(plotOutput("Avg_Flavour_Profile")) , box(plotOutput("Bean_variety"))),
    fluidRow(box(plotOutput("t_test")) , box(tableOutput("Total_cup_points_Table")) )
)
)

server <- function(input, output) { 
  output$Avg_Flavour_Profile <- renderPlot({
    coffee_ratings %>% 
      filter(country_of_origin==input$v_country) %>% 
      select(aroma:cupper_points) %>% 
      gather() %>% 
      group_by( key) %>% 
      mutate(value=mean(value)) %>% 
      ungroup() %>% 
      mutate(key= str_replace(key,"_", " ")) %>% 
      mutate(key= str_to_title(key)) %>% 
      mutate(key=fct_reorder(key,value)) %>% 
      ggplot(aes(x=key , y=value , color=key))+
      geom_point(size=5)+
      geom_segment(aes(x=key, xend=key, y=value, yend=0 ))+
      coord_flip()+
      theme(legend.position = "none")+
      xlab("")+
      ylab("")+
      labs(title="Avg Flavour Profile")
  })
  
  output$Bean_variety<-renderPlot({
    coffee_ratings %>% 
      filter(country_of_origin==input$v_country) %>% 
      select(variety) %>% 
      drop_na() %>% 
      count(variety) %>% 
      mutate(variety=fct_reorder(variety,n)) %>% 
      ggplot(aes(x=variety, y=n, fill=variety))+
      geom_col()+
      ylab("")+
      xlab("")+
      labs(title="Bean Variety")+
      theme(legend.position = "none")+
      coord_flip()
  })
  
  
  output$t_test<-renderPlot({coffee_ratings %>%
      select(country_of_origin, aroma:cupper_points) %>% 
      mutate(highlight=if_else(country_of_origin==input$v_country, "Highlight","Non-Highlight" )) %>% 
      select(-country_of_origin) %>% 
      gather(key="key", value="value", -highlight) %>% 
      group_by( key) %>% 
      do(t_test=t.test(value~highlight, data=.) %>% tidy()) %>% 
      unnest() %>% 
      mutate(key=str_replace(key, "_", " ")) %>% 
      mutate(key=str_to_title(key)) %>% 
      mutate(key=fct_reorder(key, estimate)) %>% 
      mutate(difference=case_when(
        conf.low<0 & conf.high<0 ~ "Different",
        conf.low>0 & conf.high>0 ~ "Different",
        TRUE ~ "Not-different"
      )) %>% 
      ggplot(aes(x=key, y=estimate))+
      geom_point()+
      geom_pointrange(aes(ymin=conf.low, ymax=conf.high, color=difference))+
      xlab("")+
      ylab("")+
      labs(title = "how different are the flavor profiles?")+
      theme(legend.position = "none")+
      coord_flip()+
      geom_hline(yintercept = 0, linetype="dashed") })
  
  
  
  output$Total_cup_points_Table <-function(){
    coffee_ratings %>%
      filter(country_of_origin==input$v_country) %>% 
      select( species, total_cup_points, region) %>% 
      drop_na() %>% 
      mutate (region=str_trunc(region,12, "right")) %>% 
      group_by( species , region) %>% 
      top_n(total_cup_points, n=1) %>% 
      ungroup() %>% 
      arrange(desc(total_cup_points)) %>% 
      kable() %>% 
      kable_styling() %>% 
      scroll_box(width = "400px", height = "400px")
  }
  
  
  }

shinyApp(ui, server)

