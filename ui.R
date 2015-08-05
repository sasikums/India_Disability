library(shiny)
source('import_clean_files.R')
shinyUI(navbarPage("Indian Disability Census '11",
                   tabPanel("Overview",
                            fluidPage(
                            img(src='census_disability_question.png'),
                            h1('Disability in India: By the (Official) Numbers'),
                            p("The",
                              a(href='http://www.censusindia.gov.in/2011census/population_enumeration.html',
                                '2011 Census'), "identified",strong("27 million"), " Indians as disabled. This
                              suggests that", strong("2.2%")," of India suffers from a physical or mental disability."),
                            
                            p("This dashboard describes the statistics of disability in India. Each tab of the dashboard 
(see navigation bar at the top) examines disability from a different perspective. They include the",strong("impact
                              of disability, its geographical distribution and a comparison of Indian figures
                              to global estimates.")),
                            
                            p("Each view is presented as a customizable visualization. Users can apply
                              a variety of filters including,",strong(" age, sex, type of disability etc."),
                              "The underlying data is also available for download."),
                            
                            p("The chart below is an example of a customizable chart. It currently shows
the number of disabled people of both sexes in all of India.  Users update the chart using the 'State' and 'Sex' menus
                              to see the results for a particular State/Sex."),
                            fluidRow(
                            column(4,
                                   selectInput('type_state', 'Choose State', unique(df_disab_pop$State),
                                               selected='INDIA'),offset=2),
                            column(4,
                                   selectInput('type_sex', 'Choose Sex',c('All'='All','Males'='M','Females'='F'),
                                               selected='All'),offest=2)
                            ),
                            htmlOutput('distype_chart')
                                          

                            
                   )),#End of Overview Tab
                   tabPanel("World Estimates",
                            fluidPage(
                              column(12,
                                     h1("Are 80% of India's Disabled Invisible?")),#end of heading
                              column(12,
                                     p("In 2011, the",a(href='http://www.who.int/disabilities/world_report/2011/report.pdf',
                                                        'WHO & World Bank estimated that 15% of the world
                                       population'),"have a signficant disability. However the Indian census
                                       counts only 2.2% of the population as disabled.This would suggest that a large majority of the disabled in India
                                       go uncounted and unreconized in official goverment estimates."),
                                     p('This under-counting can cause governments to allocate insufficient 
                                       resources to assist the disabled. To use a crude illustration,
                                       if the census shows only 2 out of a 100 people have a hearing problem
                                       the government only buys 2 hearing aids. But if in reality, 15 people
                                       have hearing difficulties, then 13/15 or over 80% go without
                                       hearing-aids.'),
                                     p('Underestimating the number of disabled means underserving them.'),
                                     
                                     br(),
                                     h4('WHO/WB Estimates of Disability v. Indian Census',align='center')
                              ),
                              
                             
                              fluidRow(
                                column(12,
                                       fluidRow(
                                         column(3,
                                       column(12,selectInput('wb_age', 'Choose Age Group', unique(as.character(df_wb_est$Age_Group)),
                                                            selected='All ages')),
                                       column(12,selectInput('wb_sex', 'Sex', unique(as.character(df_wb_est$sex)),
                                                            selected='All')),
                                       column(12,selectInput('wb_disab', 'Choose Disability Severity*', unique(as.character(df_wb_est$Disability_Type)),
                                                     selected='Moderate & Severe Disability')),
                                       column(12,h6('Indian census does not track severity of disability')),
                                        column(12,selectInput('wb_group', 'Choose Country Group:', unique(as.character(df_wb_est$variable)),
                                                     selected='World'))
                                       ),#end of selectors column
                                       column(9,
                                              htmlOutput('worldbank_chart'))
                                       )#end of chart column
                                       )),#end of wb chart
                              column(12,
                                     p("The chart below also shows that Indian census estimate of disability is much lower than censuses estimates
                                       by other nations. This lends further weight to the idea that India
                                       is undercounting it's disabled population. (It should be noted that cross-country
                                       comparison are tricky due to diiferences in 
                                       how disability is defined)"),
                                     
                                     p("Estimates of disability tend to be the highest in the high income nations
                                       of the western world. Greater awareness about different types of disability
                                       especially mental, assists more complete identification. The greater availability
                                       of support services in these countries also moitvates individuals to
                                       overcome the social stigmna of identifying themselves as disabled.")
                                       ),
                              h4('National Estimates of Disability Prevalence',align='center'),
                              column(12,htmlOutput('country_chart'))
                            )#end of fluidPage
                            
                            
                   ),# end of world tab  
                   
                   tabPanel("Impact (Literacy, Employment & Marriage)",
                            sidebarLayout(
                              sidebarPanel(
                                h4('Use Menu to Explore across Categories'),
                                selectInput('work_dis', 'Choose Disability Type', unique(df_work_comp$Disablity_Type),
                                            selected='Total disabled population'),
                                selectInput('work_state', 'Choose State', unique(df_work_comp$State),
                                            selected='INDIA'),
                                selectInput('work_sex', 'Choose Sex', c('All'='P','Males'='M','Females'='F'),
                                            selected='P'),
                                selectInput('work_urb', 'Choose Urban/Rural', unique(df_work_comp$Urban_Rural),
                                            selected='Total')
                                
                              ),#end of sidebarPanel
                              mainPanel(
                                h4("On average, the disabled are ~10% more likely to be illiterate. Females
                                   are more adversely affected than males."),
                                htmlOutput('lit_comp_chart'),
                                
                                h4("The disabled are more likley to be un-employed. Mental issues are the most
                                   debilitating, with unemployment rates of ~80%"),
                                htmlOutput('work_comp_chart'),
                                
                                h4("The disabled seem just as likely to be married as the general population, but face being
                                   widowed more often"),
                                
                                htmlOutput('mar_comp_chart')
                                )#end of main panel
                              )#end of sidebarLayout
                   ),#end of impact tab
                   tabPanel("Age Distribution",
                            sidebarLayout(
                              sidebarPanel(
                                h4('Use Menu to Explore across Categories'),
                                selectInput("age_dis", "Choose Disability Type", 
                                            c("All Disabilities" = 'TotDisab', 
                                              "Sight" = 'Sight', "Hearing" = 'Hear',
                                              'Speech' = 'Speech','Movement'='Move',
                                              'Mental Illness'='MenIll',
                                              'Mental Retardation'='MenRet',
                                              'Others'='Other','Mutiple Disabilites'='Mult'),
                                            selected = 'TotDisab'),
                                selectInput('age_state', 'Choose State', unique(df_disab_pop$State),
                                            selected='INDIA'),
                                selectInput('age_sex', 'Sex', c('All'='All','Males'='M','Females'='F'),
                                            selected='All'),
                                selectInput('age_urb', 'Choose Urban/Rural', c('Total'='Total',
                                                                        'Urban'='Urban','Rural'='Rural'),
                                            selected='Total')
                                
                                
                              ),#end of sidebar panel  
                              mainPanel(
                                h4("Unsurprisingly, rate of disability rises with age. For people under 30, less than 2%
                                   are disabled. But rate of disability rises rapidly past the age of 50."),
                                htmlOutput('age_dis_chart', align='center'),
                                h4("As a result the disabled population is on average older than the general population.
                                   Over 60% of the Indian population is UNDER 30. In contrast, 60% of the disabled population
                                   is OVER 30"),
                                htmlOutput('age_comp_chart', align='center')
                                )#end of mainPanel
                                )# end of sidebarylayout
                            ),#end of tabPanel age dist
                   
  tabPanel("Statewide Distribution",
    sidebarLayout(
      sidebarPanel(
        h4('Use Menu to Explore across Categories'),
        selectInput('selec_sex', 'Choose Sex', c('All'='All','Males'='M','Females'='F'),
                    selected='All'),
        selectInput('selec_urb', 'Urban/Rural', c('Total'='Total',
                                                             'Urban'='Urban','Rural'='Rural'),
                               selected='Total'),
        
        
        sliderInput('selec_age',"Choose Age Range",min=0,max=100,value=c(0,100),step=10),
        
        radioButtons('selec_distype','Choose Display Type',choices=c("Percentages","Counts"),
                     selected="Percentages",inline=TRUE),
             
              
              checkboxGroupInput("selec_dis", label = "Disability Type", 
                                 choices = list("All Disabilities" = 'TotDisab', 
                                                "Sight" = 'Sight', "Hearing" = 'Hear',
                                                'Speech' = 'Speech','Movement'='Move',
                                                'Mental Illness'='MenIll',
                                                'Mental Retardation'='MenRet',
                                                'Others'='Other','Mutiple Disabilites'='Mult'),
                                 selected = 'TotDisab')
    ),#end of sidebar panel  
    mainPanel(
      htmlOutput('by_state_map', align='center'),
      br(),
      h4("Table of Selected Disabilties", align='center'),
      htmlOutput('india_summary', align='center'),
      h4("By State", align='center'),
      h6("(Click on heading to sort by that column)", align='center'),
      htmlOutput('bystate_table',align='center')
    )#end of mainPanel
  )# end of sidebarylayout
),#end of tabPanel_Statewide
tabPanel("Top Cities",
         sidebarLayout(
           sidebarPanel(
             h4('Use Menu to Explore across Categories'),
             selectInput('cit_dis', 'Choose Disability Type', unique(df_dis_cit$Disability),
                         selected='Total'),
             selectInput('cit_sex', 'Sex', unique(df_dis_cit$sex),
                         selected='All'),
             selectInput('cit_age', 'Age Group', unique(df_dis_cit$Age_Group),
                         selected='Total')
             
           ),#end of sidebarPanel
           mainPanel(
             h4("Cities with the Highest Disabled Population", align='center'),
             htmlOutput('top_city_map'),
             h4("All Cities", align='center'),
             h6("(Click on heading to sort by that column)", align='center'),
             htmlOutput('city_table', align='center')
           )#end of main panel
         )#end of sidebarLayout
)#end of cities tab


))
