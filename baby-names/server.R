#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  plot_data_top10 <- reactive({
    most_pop %>% 
      filter(year == input$year) %>% 
      filter(sex == input$sex)
  })
   
  output$top_10_names <- renderPlot({
    ggplot(plot_data_top10()) +
      geom_col(aes(x = name, y = n, fill = sex))
    
  })
  
  plot_data_decade <- reactive({
    top %>% 
    filter(decade %in% input$decade) %>% 
      mutate(year = as.factor(year)) %>% 
      mutate(year = fct_reorder(year, desc(year)))
  })
  
  output$names_decade_m <- renderPlot({
    # ggplot(plot_data_decade(), aes(decade, n, color = sex, group=name)) +
    #   geom_line(size = 10) +
    #   labs(x="n", y="Decade") +
    #   coord_flip()
    ggplot(plot_data_decade() %>% filter(sex == "M"), aes(x = year, y = prop, fill = sex)) +
      geom_bar(stat = "identity", position = position_dodge(width = 1)) +
      geom_text(aes(x = year, y = prop, label = name),
                size = 6,
                colour = "grey",
                family = "JetBrains Mono") +
      scale_fill_viridis_d()+
      theme_minimal() +
      coord_flip()
    
  })
  
  output$names_decade_f <- renderPlot({
    # ggplot(plot_data_decade(), aes(decade, n, color = sex, group=name)) +
    #   geom_line(size = 10) +
    #   labs(x="n", y="Decade") +
    #   coord_flip()
    ggplot(plot_data_decade() %>% filter(sex == "F"), aes(x = year, y = prop, fill = sex)) +
      geom_bar(stat = "identity", position = position_dodge(width = 1)) +
      geom_text(aes(x = year, y = prop, label = name),
                size = 6,
                colour = "grey",
                family = "JetBrains Mono") +
      scale_fill_viridis_d()+
      theme_minimal() +
      coord_flip()
    
  })
  
  time_f_data <- reactive ({
    
    time_f %>% 
    filter(name %in% input$time_names_f) %>% 
    arrange(year, name) 
  })
  
  output$plot_time_f <- renderPlot({
    
    ggplot(time_f_data() %>% 
             mutate(name_f = as.factor(name)) %>% 
             mutate(name_f = fct_reorder(name, desc(name))), 
           aes(year, name_f, colour = name_f, group = grp)) +
      geom_line(show.legend = FALSE) + 
      geom_point(aes(start_year, name_f, colour = name_f, group = grp), 
                 show.legend = F) +
      geom_point(aes(end_year, name_f, colour = name_f, group = grp), 
                 show.legend = F) +
      labs(x="Year", y=NULL) +
      theme_minimal()

  })
  
  time_m_data <- reactive ({
    
    time_m %>% 
      filter(name %in% input$time_names_m) %>% 
      arrange(year, name) 
  })
  
  output$plot_time_m <- renderPlot({
    
    ggplot(time_m_data() %>% 
             mutate(name_f = as.factor(name)) %>% 
             mutate(name_f = fct_reorder(name, desc(name))), 
           aes(year, name_f, colour = name_f, group = grp)) +
      geom_line(show.legend = FALSE) + 
      geom_point(aes(start_year, name_f, colour = name_f, group = grp), 
                 show.legend = F) +
      geom_point(aes(end_year, name_f, colour = name_f, group = grp), 
                 show.legend = F) +
      labs(x="Year", y=NULL) +
      theme_minimal()
    
  })
  
  unique_plot_data <- reactive ({
    
   unique %>% 
      filter(sex %in% input$sex_unique) 
  })
  
  output$unique_plot <- renderPlot({
    
    ggplot(unique_plot_data(), 
           aes(year, n, colour = sex)) +
      geom_line() + 
      theme_minimal()
  
})
  
 names_by_state <- reactive ({
    
    top_state %>% 
     filter(state_name == input$state) %>% 
     group_by(name, sex) %>% 
     summarise(n = sum(n)) %>% 
     ungroup()
  })

 output$word_cloud <- renderPlot({

   ggplot(names_by_state(),
          aes(label = name, size = n, colour = sex)) +
     geom_text_wordcloud() +
     scale_size_area(max_size = 40) +
     theme_minimal()

 })
 
 names_by_state_map <- reactive ({
   
   state %>% 
     filter(year == input$map_year) %>% 
     filter(sex == input$map_sex) 
   
 })
 
 output$map_plot <- renderPlot({
   
   ggplot(data=names_by_state_map(), aes(x=long, y=lat, fill = region,
                          label = name)) + 
     geom_polygon(color = "white") + 
     guides(fill=FALSE) + 
     theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
           axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
     ggtitle('U.S. Map with States') + 
     coord_fixed(1.3) +
     geom_text(aes(x = centre_long, y = centre_lat, label = name),
               size = 3)

 })
 
 map_table <- reactive({
   map_data %>% 
     filter(year == input$map_year) %>% 
     filter(sex == input$map_sex) %>% 
     group_by(name) %>% 
     summarise(no_babies = sum(tot)) %>% 
     ungroup() %>% 
     arrange(desc(no_babies)) %>% 
     mutate(perc = round_half_up(no_babies/sum(no_babies), 3)*100)
 })
 
 output$map_data <- renderTable({
   map_table()
 })
  
})

