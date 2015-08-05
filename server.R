library(shiny)


shinyServer(function(input, output) {
  
  #reactive function that returns the filter data. To use the resulting dataframe use ()
  # e,g relv_data
  relv_data <- reactive({
    
    # Due to dplyr issue #318, we need temp variables for input values
    in_sex=input$selec_sex
    in_urb=input$selec_urb 
    in_age_low = input$selec_age[1] 
    in_age_high =input$selec_age[2] 
    in_dis = input$selec_dis
   
    
    filter(df_disab_pop, sex==in_sex &
             Urban_Rural==in_urb &
              age_high >= in_age_low &
             age_high <= in_age_high &
             disability %in% in_dis)
 
  })
  #sum disability and population counts for given inputs
  #calculates percentages and uses switch() & chart_var to display percentages 
  #or counts based on user. Remove INDIA data before charting
  chart_data <- reactive({ 
    
    summarise(group_by(filter(relv_data(),State !="INDIA")
                                ,State,iso_code,sex,Urban_Rural),
              disabled_num=sum(disabl_count),pop=sum(pop_count)) %>%
      mutate(disabled_perc=disabled_num/pop, 
             Age_Range=paste(input$selec_age[1],input$selec_age[2],sep="-"),
             chart_var=sum(switch(input$selec_distype,Percentages=disabled_perc,
                                  Counts=disabled_num)))  
  })
  
  #data of disability by city
  city_data <- reactive({
    temp <- filter(df_dis_cit,sex==input$cit_sex & Age_Group==input$cit_age & 
             Disability==input$cit_dis)%>%
      arrange(desc(Disabled_Count))
    temp<- mutate(temp,rank=as.numeric(rownames(temp)),vpos=50/rank)
    temp
    
  })
  
  #Bubble Chart of Top 10 cities
  output$top_city_map <- renderGvis(gvisBubbleChart(head(city_data(),10), 
                                                    idvar='City',
                                                    sizevar='Disabled_Count',
                                                    colorvar='City',
                                                    xvar='rank',
                                                    yvar='Disabled_Count',
                                                    options=list(
                                                      height=400
                                                    )))
  #table with city data
  output$city_table <- renderGvis({
    sub_temp <- select(city_data(),City,sex,Age_Group,Disability,Disabled_Count)
    names(sub_temp)<-c('City','Sex','Age','Disability','Disabled Count')
    
    gvisTable(sub_temp,formats=list('Disabled Count'='#,###'),
              option=list(page='enable',height='automatic',
                          width='automatic'))
    
  })
  #National Totals for given selection
  india_data <- reactive({
    filter(relv_data(),State =="INDIA")%>%
      group_by(State,sex,Urban_Rural)%>%
      summarise(india_Disabled=sum(disabl_count),Pop=sum(pop_count)) %>%
      mutate(india_Perc_Disabled=india_Disabled/(Pop/length(input$selec_dis)),
             Age_Range=paste(input$selec_age[1],input$selec_age[2],sep="-")) %>%
      select(State,sex,Urban_Rural,Age_Range,india_Disabled,india_Perc_Disabled)
   
  })
  
  #Table with total stats for India
  output$india_summary <- renderGvis({
    sum_temp <- india_data()
    names(sum_temp) <- c ('State','Sex' ,'Urban/Rural','Age Range','Disabled Count','% of Pop. Disabled')
    gvisTable(sum_temp,formats=list('Disabled Count'='#,###','% of Pop. Disabled'='#.##%'))    
  })
  
  #Geo chart of disability by state
  output$by_state_map <-renderGvis(gvisGeoChart(chart_data(), 
                                        locationvar="iso_code", 
                                        colorvar='chart_var',
                                        options=list(region="IN",
                                                     colorAxis="{colors:['green','yellow',
                                                     'red']}",
                                                     displayMode="region",resolution="provinces"
                                        )))
  #Table with State Data
  output$bystate_table <- renderGvis({
    state_temp <- ungroup(chart_data()) %>%
      select(State,sex,Urban_Rural,Age_Range,disabled_num,disabled_perc)
    #merge in national totals
    nat_temp <-select(ungroup(india_data()),sex,Urban_Rural,Age_Range,india_Disabled)
   
    state_temp <- merge(state_temp,
                        nat_temp,
                               by=c('sex','Urban_Rural','Age_Range'))
   
    state_temp <- mutate(state_temp,national_perc=disabled_num/india_Disabled) %>%
      select(State,sex,Urban_Rural,Age_Range,disabled_num,disabled_perc,national_perc) %>%
      arrange(desc(disabled_perc))
    
    names(state_temp) <- c ('State','Sex' ,'Urban/Rural','Age Range','Disabled Count',
                            '% of Pop. Disabled','% of India Disabled Total')
    
    
    gvisTable(state_temp,formats=list('Disabled Count'='#,###','% of Pop. Disabled'='#.##%',
                                      '% of India Disabled Total'='#.##%'),
              option=list(page='enable',height='automatic',
                          width='automatic'))
    
  })
  
  #data for age distribution charts
  age_data <- reactive({
    
    filter(df_disab_pop,State==input$age_state & Urban_Rural==input$age_urb &
                     sex==input$age_sex & disability==input$age_dis & Age_Group != "Total") %>%
      arrange(age_high) %>%
      select(Age_Group,disabl_count,pop_count,pop_disabl_perc) %>%
      mutate(dis_dist=disabl_count/sum(disabl_count),pop_dist=pop_count/sum(pop_count))
  })
  #Chart of % of each age disabled
  output$age_dis_chart <- renderGvis({
    temp <- select(age_data(),Age_Group,pop_disabl_perc)
    names(temp)<- c('Age Group', '% of Age Group Disabled')
    gvisColumnChart(temp, 
                    options=list(height=400, vAxis="{format:'#,###.#%'}",
                                 title='Percentage of Age Group Disabled',
                                 legend='bottom',
                                 bar="{groupWidth:'80%'}",
                                 series="[{color:'#D00000'}]"))
  })
  
  #Chart of age distribution
  output$age_comp_chart <- renderGvis({
    temp <- select(age_data(),Age_Group,pop_dist,dis_dist)
    names(temp)<- c('Age Group', 'General Population','Disabled Population')
    gvisColumnChart(temp, 
                    options=list(height=400, vAxis="{format:'#,###.#%'}",
                                 title='Age Distribution of Disabled v. General Populations',
                                 legend='bottom',
                                 bar="{groupWidth:'80%'}"))
  })
  
  
  #Chart of dis v general workers type
  output$work_comp_chart <- renderGvis({
    temp <- filter(df_work_comp,State==input$work_state & Urban_Rural==input$work_urb &
                     sex==input$work_sex & Disablity_Type==input$work_dis) %>%
      select(worker_type,worker_dist,dis_worker_dist) %>% 
      arrange(desc(dis_worker_dist))
    names(temp) <- c('worker_type','General Population','Disabled Population')
    gvisBarChart(temp, options=list(height=400, hAxis="{format:'#,###.#%'}",
                                   title='Disabled v. General: Employment Type',
                                   legend='bottom'))
    
  })
  
  #Chart of dis v general illiteracy
  output$lit_comp_chart <- renderGvis({
    temp <- filter(df_lit_comp,State==input$work_state & Urban_Rural==input$work_urb &
                     sex==input$work_sex & Disablity_Type==input$work_dis) %>%
      select(State,ill_lit,dis_ill_lit) 
    names(temp) <- c('State','General Population','Disabled Population')
    gvisBarChart(temp, options=list(hAxis="{format:'#,###.#%'}",
                                    title='Disabled v. General: Illiteracy',
                                    legend='bottom'))
    
  })
  
  #Chart of dis v general marriage
  output$mar_comp_chart <- renderGvis({
    temp <- filter(df_mar_comp,State==input$work_state & Urban_Rural==input$work_urb &
                     sex==input$work_sex & Disability_Type==input$work_dis) %>%
      select(status,mar_perc,dis_mar_perc)%>%
      arrange(desc(mar_perc))
    names(temp) <- c('Marriage Status','General Population','Disabled Population')
    gvisBarChart(temp, options=list(height=400,hAxis="{format:'#,###.#%'}",
                                    title='Disabled v. General: Marriage Status',
                                    legend='bottom'))
    
  })
  
  #Chart of country estimates of disability
  output$country_chart <- renderGvis({
    gvisBubbleChart(
      df_dis_count,xvar='nrow',yvar='Disability_Perc',idvar="Country_Label",sizevar='Disability_Perc',
      colorvar='colorlab',
      options=list(
        width=1200,
        vAxis="{minValue:-.03, maxValue:.5,format:'#,###%',title:'% of Pop. Disabled'}",
        hAxis="{minValue:0,  maxValue: 11,textPosition: 'none'}",
        bubble="{textStyle:{fontSize: 11}}",
        sizeAxis="{minSize:10}")
    )
    
  })
  
  
  #Chart of WB data
  output$worldbank_chart <- renderGvis({
    div <- c('World','India',input$wb_group)
    temp <- filter(df_wb_est,as.character(Age_Group)== input$wb_age & 
                       as.character(sex)==input$wb_sex & 
                       Disability_Type==input$wb_disab &
                       variable %in% div) 
      gvisColumnChart(temp,xvar='variable',yvar=c('value','value.annotation','value.style'),
                      options=list(vAxis="{format:'#,###.#%', minValue:0,title:'% of Pop. Disabled'}",
                                             legend='none',height=300))
  })
  #Data of Disability Types
  disab_types <- reactive({
    filter(df_disab_pop,State=='INDIA' & Urban_Rural=="Total" &
             sex=="All" & disability!="TotDisab" & Age_Group == "Total")%>%
      select(State,disability,disabl_count)%>%
      mutate(disability=recode(disability,"'TotDisab' ='All Disabilities'; 
                           'Sight' = 'Sight'; 'Hear' = 'Hearing' ;
                           'Speech' = 'Speech';'Move'='Movement';
                           'MenIll'='Mental Illness';
                           'MenRet'='Mental Retardation';
                           'Other'='Other';'Mult'='Mutiple Disabilites'"))%>%
      arrange(State,disabl_count)%>%
      select(disability,disabl_count)
  })
  #Chart of Disability Types
  output$distype_chart <- renderGvis({
    gvisPieChart(data=disab_types(), numvar='disabl_count',labelvar="disability",
                 options=list(width=1000, height=500
                              , pieSliceText = 'value',is3D=FALSE,
                              legend="{position:'labeled'}"))
  })
  
  
})