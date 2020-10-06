library(shinydashboard)
library(dashboardthemes)
library(dplyr)
library(readr)
library(leaflet)
library(sf)
library(plotly)
library(shinyjs)
library(rintrojs)
library(shinyBS)
library(shinyWidgets)
library(DT)
library(RColorBrewer)
library(stringr)
#library(apputils)
library(shinyalert)
library(readxl)

#code adapted from dashboard community capitals-BII


data_pol <- read_rds("data_pol_ext.rds")
measures <- read.csv("measures.csv")

css_fix <- "div.info.legend.leaflet-control br {clear: both;}"
html_fix <- as.character(htmltools::tags$style(type = "text/css", css_fix))



#
# USER INTERFACE ----------------------------------------------------------------------------------------------------
#

ui <- dashboardPage(title = "Economic Mobility Data Infrastructure",
                    
                    dashboardHeader(
                      titleWidth='100%',
                      title = span(
                        tags$img(src = "header.jpg", width = '100%'), 
                        column(12, class = "title-box", 
                               tags$h1(class = "primary-title", 
                                       style = "font-size: 2.8em; 
                                                font-weight: bold; 
                                                text-shadow: -1px -1px 0 #DCDCDC,
                                                             1px -1px 0 #DCDCDC,
                                                             -1px 1px 0 #DCDCDC,
                                                             1px 1px 0 #DCDCDC;", 
                                       'Economic Mobility Data Infrastructure') 
                        )
                      )
                    ),
                    
                    dashboardSidebar(
                      a(href = "https://datascienceforthepublicgood.org/economic-mobility", 
                        img(src = "logo.png", height = 60, width = 235)
                      ),
                      sidebarMenu(
                        menuItem(text = "Political", tabName = "financial", icon = icon("balance-scale-left")),
                        #menuItem(text = "Index", tabName = "political", icon = icon(" ")),
                        menuItem(text = "Policy Assets", tabName = "policyassets", icon = icon("")),
                        menuItem(text = "Data and Measures", tabName = "datamethods", icon = icon("")),
                        menuItem(text = "About Us", tabName = "contact", icon = icon(""))
                        #menuItem(text = "About Us", tabName = "contact", icon = icon(""))
                      )
                    ),
                    
                    dashboardBody(
                      HTML(html_fix),    
                      tags$style(
                        type = 'text/css', 
                        '.bg-olive {background-color: #FFFFFF!important; }'
                      ),
                      
                      tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
                      
                      # http://jonkatz2.github.io/2018/06/22/Image-In-Shinydashboard-Header
                      tags$style(type="text/css", "
/*    Move everything below the header */
               .content-wrapper {
               margin-top: 50px;
               }
               .content {
               padding-top: 60px;
               }
               /*    Format the title/subtitle text */
               .title-box {
               position: absolute;
               text-align: center;
               top: 50%;
               left: 50%;
               transform:translate(-50%, -50%);
               }
               @media (max-width: 590px) {
               .title-box {
               position: absolute;
               text-align: center;
               top: 10%;
               left: 10%;
               transform:translate(-5%, -5%);
               }
               }
               @media (max-width: 767px) {
               .primary-title {
               font-size: 1.1em;
               }
               .primary-subtitle {
               font-size: 1em;
               }
               }
               /*    Make the image taller */
               .main-header .logo {
               align: center;
               display: block;
               margin:0!important;
               padding:0!important;
               border:0!important;
               height: 125px;
               }
               /*    Override the default media-specific settings */
               @media (max-width: 5000px) {
               .main-header {
               padding: 0 0;
               position: relative;
               }
               .main-header .logo,
               .main-header .navbar {
               width: 100%;
               float: none;
               }
               .main-header .navbar {
               margin: 0;
               }
               .main-header .navbar-custom-menu {
               float: right;
               }
               }
               /*    Move the sidebar down */
               .main-sidebar {
               position: absolute;
               }
               .left-side, .main-sidebar {
               padding-top: 175px;
               }"
                      ), 
                      
                      # https://stackoverflow.com/questions/37169039/direct-link-to-tabitem-with-r-shiny-dashboard/37170333
                      tags$script(HTML("
        var openTab = function(tabName){
          $('a', $('.sidebar')).each(function() {
            if(this.getAttribute('data-value') == tabName) {
              this.click()
            };
          });
        }
      ")),
                      
                      shinyDashboardThemes(
                        theme = "grey_light"
                      ),
                      
                      useShinyjs(),
                      introjsUI(),
                      useShinyalert(),
                      
                      tabItems(
                        
                        # SUMMARY CONTENT -------------------------
                        
                        #
                        # POLITICAL CAPITAL CONTENT -------------------------------------------------
                        #
                        
                        tabItem(tabName = "financial",
                                
                                fluidRow(
                                  box(title = "About Political Capital",
                                      width = 9,
                                      "Political capital refers to the ability of a community to influence and enforce rules, 
                                      regulations, and standards through their organizations, connections, voice, and power as citizens."
                                  ),
                                  box(title = "Select Your State",
                                      width = 3,
                                      selectInput("fin_whichstate", label = NULL,
                                                  choices = list("Iowa",
                                                                 "Oregon",
                                                                 "Virginia"), 
                                                  selected = "Iowa")
                                  )
                                ),
                                
                                fluidRow(
                                  box(
                                      width = 12,
                                      column(11,
                                             h4(strong("Explore the Political Domains"))
                                             # radioGroupButtons(
                                             #   inputId = "finidx_choice", 
                                             #   choices = c("POLITICAL CAPITAL INDEX", "POLICY ASSETS"
                                             #               ),
                                             #   checkIcon = list(yes = icon("angle-double-right")),
                                             #   direction = "horizontal", width = "100%",
                                             #   justified = FALSE, status = "success", individual = TRUE)
                                      ),
                                      column(1,
                                             #infobutton_fin
                                             circleButton(inputId = "pcindex_info", icon = icon("info"), status = "info", size = "sm")
                                      )
                                  )
                                  
                                ),
                                
                                #
                                # POLITICAL INDEX ------------------------------------------
                                #
                                
                                tabPanel("input.finidx_choice == 'POLITICAL CAPITAL INDEX'",
                                                 
                                                 fluidRow(
                                                   
                                                   box(title = "Political Capital Index",
                                                       width = 12,
                                                       #h5(strong("County-Level Map")),
                                                      
                                                       #leafletOutput("mainplot2")
                                                       leafletOutput("plot_political_index")
                                                       #leafletOutput("plot_fin_index_commerce")
                                                       
                                                   )
                                                   
                                                 ),
                                                 fluidRow(
                                                   tabBox(title = "Political Capital Measures",
                                                          id = "tab_indexfin_co",
                                                          width = 12,
                                                          side = "right",
                                                          tabPanel(title = "Contributions",
                                                                   fluidRow(
                                                                     box(
                                                                       width = 12,
                                                                       column(11,
                                                                              h4(strong("Number of Contributors per 1,000 People"), align = "center")
                                                                       ),
                                                                       column(1,
                                                                              circleButton(inputId = "contribution_info", icon = icon("info"), status = "info", size = "sm")
                                                                       )
                                                                       #infobutton_fin
                                                                     ),
                                                                     
                                                                     #h4(strong("Number of Contributors per 1,000 People"), align = "center"),
                                                                     column(
                                                                       width = 6,
                                                                       h5(strong("County-Level Map")),
                                                                       leafletOutput("leaflet_contrib")
                                                                       #leafletOutput("plot_fin_co_bus")
                                                                     ),
                                                                     column(
                                                                       width = 6,
                                                                       h5(strong("Measure Box Plot and Values by Rurality")),
                                                                       plotlyOutput("plotly_contrib")
                                                                     )
                                                                   )
                                                          ),
                                                          tabPanel(title = "Participation",
                                                                   fluidRow(
                                                                     box(
                                                                       width = 12,
                                                                       column(11,
                                                                              h4(strong("Number of Organizations per 1,000 People"), align = "center")
                                                                       ),
                                                                       column(1,
                                                                              circleButton(inputId = "participation_info2", icon = icon("info"), status = "info", size = "sm")
                                                                       )
                                                                       #infobutton_fin
                                                                     ),
                                                                     
                                                                     #h4(strong("Number of Organizations per 1,000 People"), align = "center"),
                                                                     column(
                                                                       width = 6,
                                                                       h5(strong("County-Level Map")),
                                                                       leafletOutput("leaflet_organization")
                                                                     ),
                                                                     column(
                                                                       width = 6,
                                                                       h5(strong("Measure Box Plot and Values by Rurality")),
                                                                       plotlyOutput("plotly_organization")
                                                                     )
                                                                   )
                                                          )
                                                          ,
                                                          tabPanel(title = "Representation",
                                                                   fluidRow(
                                                                     
                                                                     box(
                                                                         width = 12,
                                                                         column(11,
                                                                                h4(strong("Voters divided by Voting-age Population"), align = "center")
                                                                      ),
                                                                         column(1,
                                                                                circleButton(inputId = "representation_info2", icon = icon("info"), status = "info", size = "sm")
                                                                         )
                                                                      #infobutton_fin
                                                                     ),
                                                                     column(
                                                                       width = 6,
                                                                       h5(strong("County-Level Map")),
                                                                       leafletOutput("leaflet_voters")
                                                                     ),
                                                                     column(
                                                                       width = 6,
                                                                       h5(strong("Measure Box Plot and Values by Rurality")),
                                                                       plotlyOutput("plotly_voters")
                                                                     )
                                                                   )
                                                          )
                                                          
                                                   )
                                                 )
                                )
                
                        ),
                        
                        
                        
                        # POLITICAL CAPITAL CONTENT -------------------------
                        tabItem(tabName = "political",
                                fluidRow(
                                  box(title = "About Political Capital",
                                      width = 9,
                                      "Political capital refers to the ability of a community to influence and enforce rules, regulations, 
                    and standards through their organizations, connections, voice, and power as citizens."
                                  ),
                                  box(title = "Select Your State",
                                      width = 3,
                                      selectInput("nat_whichstate", label = NULL,
                                                  choices = list("Iowa",
                                                                 "Oregon",
                                                                 "Virginia"), 
                                                  selected = "Iowa")
                                  )
                                ),
                                
                                fluidRow(
                                  style = "margin: 1px",
                                  width = 12 ,align="center",
                                  h3(strong("Political Capital Index")),
                                  plotlyOutput("plotly_index3d", width = "auto" ,  height = "auto") ),
                                br(),
                                fluidRow(
                                  style = "margin: 6px",
                                  width = 12,
                                  
                                  column(6, wellPanel(strong('Index'), 
                                                      p(),
                                                      #em('Description.'),
                                                      br('The index of political capital is comprised by the three variables above mentioned. 
                                                         The following procedure was used in order to compare these metrics:'),
                                                      br('1. Every variable was first standarized so that, the data is mapped within a range
                                                         between 0 and 1. The lowest values represent low representation, participation and contribution or, viceversa.  The transformation of variables is possible by using the following expression:'),
                                                      
                                                      p(
                                                        withMathJax(
                                                          #"$$y_i = \\frac{\\bar{x}_1 - \\bar{x}_2}{\\sqrt{\\frac{s_1^2}{N_1}+\\frac{s_2^2}{N_2}}}$$"
                                                          "$$y_i = \\frac{x_i - \\min{(x_i)}}{\\max{(x_i)}-\\min{(x_i)}}$$"
                                                          
                                                        )),
                                                      p(),
                                                      
                                                      br('where x represents the variable for political capital with participation (i=1), representation (i=2) and contribution (i=3).'),
                                                      #  withMathJax(),
                                                      # helpText('where x represents the variable for political capital with participation, representation or contribution $\\sqrt{2}$ '),
                                                      
                                                      
                                                      p(),
                                                      br('2. With the standarized variable, we proceed to estimate the distance with respect to the origin. 
                                                         This is just the correspondence to the so called euclidian distance, given by the following expression:'),
                                                      p(
                                                        withMathJax(
                                                          
                                                          "$$index = \\sqrt{ ({y}_1-\\bar{y}_1)^2 + ({y}_2-\\bar{y}_2)^2 +({y}_3-\\bar{y}_3)^2} $$"
                                                          
                                                        )), 
                                                      br('Given that the origin is represented by the point:'),
                                                      
                                                      p(
                                                        withMathJax(
                                                          
                                                          "$$(\\bar{y}_1, \\bar{y}_2, \\bar{y}_3)=(0,0,0) $$"
                                                          
                                                        )
                                                        
                                                      ),
                                                      p('Then, we finally obtain the expression for the index:' ),
                                                      
                                                      p(
                                                        withMathJax(
                                                          
                                                          "$$index = \\sqrt{{y}_1^2 + {y}_2^2 +{y}_3^2} $$"
                                                          
                                                        ))

                                                      )),                         
                                  
                                  column(6, wellPanel(strong('Interpretation of the index'), 
                                                      p(),
                                                      #em('Description.'),
                                                      br('Every county is reprented by one of the three-dimensional points in the above figure, where the three different dimensions are identified in the corresponding axis.
States differ by color. The viewer may click on the top right legend in case they want to visualize a specific state separately.
The index is the length of the graphical distance between the point (county) and the origin.'), 
                                                      br('For example, the furthest point represents Fairfaix city, VA, where 76.9% of population votes, 
there are 2.9 organizations per every 1000 inhabitants and, where approximately 40 individuals contribute to politicall purposes per every 1000 people. The index reveals that Fairfax county
possesses a relatively high political capital, where individuals participate, organize and seek representation. In contrast, in Malheur County, Oregon; only 51.0% of the 
population voted in recent 2016 elections, there exists only 1.2 organization per every 1000 people and approximately 3 individuals (out of 1000) contribute political purposes. One may then conclude that political capital in Marion County, OR is relatively incipient.'),
                                                      br('The index and the variables used are proxies to political participation and representation, and they may be incomplete measures of
                                                             political capital, but they may be a starting point for the study of  relationships of power within communities. One of the key meaurements with interest for communities might be the existence of leadership and 
influence but as the complexity of the identification scalates and the lack of county-level data becomes a limitation, this initial analysis may offer an opportunity to motivate further research.')
                                              
                                                      
                                  )
                                  )                         
 
                                  
                                  )
                                
                                
                        ),  
                        

                        # DATA AND METHODS CONTENT -------------------------
                        tabItem(tabName = "policyassets",
                                fluidRow(style = "margin: 6px",
                                         width = 12, 
                                    
                         
                                         #new content from POLICY ASSETS
                                         #conditionalPanel("input.finidx_choice == 'POLICY ASSETS'",
                                                          
                                                          fluidRow( width=12,
                                                                    br(),
                                                                    box(width = 12,
                                                                        h4(strong("Domains of Policy Assets") ) ),
                                                                    
                                                                    column(title = "Policy Assets",
                                                                           width = 6,
                                                                           
                                                                           
                                                                           # leafletOutput("plot_fin_index_agri")
                                                                           
                                                                           box(width = 12,
                                                                               br(style="text-align: justify;", "Policy questions for each area were constructed to 
                                                                             have a “Yes” or “No” response, where a “Yes” indicates the policy has the potential to have a positive impact based on empirical research. A response of “Yes” was assigned a value of 1. 
                                                                             If the state did not have a particular policy or regulation, it was assigned a value of 0.
                                                                             For example, Student Discipline is one of the sub-domains identified within the Education policy area. Multiple questions regarding Student Discipline were evaluated such as:"),
                                                                               br(tags$li('Is there a ban on corporal punishment?'),
                                                                                  
                                                                                  tags$li('Are there in-school disciplinary approaches other than corporal punishment?')),
                                                                               
                                                                               br(style="text-align: justify;","According to Cuddy and Reeves (2014), students subject to corporal punishment performed worse than their peers in non-punitive environments. 
                                                                             Therefore, if a state banned corporal punishment they received a value of 1, if corporal punishment was not banned or there was no policy or 
                                                                             regulation on corporal punishment that policy question was assigned a 0."),
                                                                               br(style="text-align: justify;","Policy areas have multiple domains. We standardized the scores by summing across a domain and dividing by the number of 
                                                                             questions. The final score for a domain was calculated by taking the mean across the domains. The final score for a policy area was calculated 
                                                                             by taking the mean across the domains. The following table summarizes the areas of policy and their respective domains. The specific questions and their values can be observed by choosing the respective area.") )
                                
                                                                               )
                                                                           
                                                                    ,
                                                                    column(6, align= "center",
                                                                           br(),
                                                                           br(),
                                                                           box(width = 12,
                                                                           img(src = "table_domains.png", height = 510 , width = 400)) )
                                                                           ),
                                                          br(),
                                                          br(),
                                         
                                         #tabItem("global", "global"),
                                                          
                                                          fluidRow(
                                                            tabBox(title = "  ",
                                                                   id = "tab_indexfin_ag",
                                                                   width = 12,
                                                                   side = "left",
                                                                   
                                                                   
                                                                   tabPanel(title = "Education",
                                                                   #  tabItem(tabName= "segundo", title = "Education",
                                                                            fluidRow(
                                                                              
                                                                              column(4,
                                                                                     #----------
                                                                                     br(),
                                                                                     strong("Background"),
                                                                                     br(style="text-align: justify;","Education is a fundamental vehicle enabling economic mobility.   Timothy Bartik (Senior Economist at W.E. Upjohn Institute for Employment Research) states that for every one dollar invested in high quality early childhood programs, a state economy will benefit with a two to three dollar return on investment. "),
                                                                                     br(style="text-align: justify;","Four subdomains were identified: school climate, early childhood education, post-secondary affordability, and workforce development. There are 19 subcategories which are derived from 73 policy questions.  "),
                                                                                     br(style="text-align: justify;","a. As defined by the National School Climate Center", strong("School climate"), "refers to the quality and character of school life. School climate is based on patterns of students', parents' and school personnel's experience of school life and reflects norms, goals, values, interpersonal relationships, teaching and learning practices, and organizational structures. A sustainable, positive school climate fosters youth development and learning necessary for a productive, contributing and satisfying life in a democratic society.” It addresses suspensions, specific infractions and conditions; prevention and non-punitive behavioral interventions; monitoring and accountability; school resources for safety and truant/attendance officers; and state education agency support."),
                                                                                     br(style="text-align: justify;","b.", strong("Early childhood"), "includes those school years from pre-kindergarten to the third grade. Early childhood education policies group them by kindergarten requirements; teacher quality; school readiness and transitions; assessment intervention and retention; family engagement; and social-emotional learning."),
                                                                                     br(style="text-align: justify;","c.", strong("Post-secondary education"),"is the educational level following the completion of secondary education (high school). Post-secondary education includes non-degree credentials such as certifications, licenses, and work experience programs, as well as, college and professional degrees.  Post-secondary affordability policies grouped them by, need and merit based financial aid; financial aid; and free college."),
                                                                                     br(style="text-align: justify;","d.", strong("Workforce development."), "The Federal Workforce Innovation and Opportunity Act (WIOA) encourages state policymakers to seek ways to connect education, job seekers, and employers in their states by developing a one-stop delivery system that provides information on career and training services, access to employer programs and activities, and access to real-time labor market information. Workforce development policies grouped them by, statewide apprenticeships; connecting education to work; and post-secondary career and technical education.")
                                                                                     
                                                                              ),
                                                                              
                                                                              column(8, 
                                                                                     h3(strong( "Asset Map")),
                                                                                     br('The following figure summarizes the extent of every domain for each state.'),
                                                                                     ###graph here
                                                                                     plotOutput("political_dom_edu",  width = "auto", height=800)  
                                                                              )
                                                                              
                                                                            ),
                                                                            hr(),
                                                                            
                                                                            tabPanel("Data Sources & References",
                                                                                     fluidRow(width =12,
                                                                                              column(1),
                                                                                              column(10, h3(strong("Data Sources and References")),
                                                                                                     
                                                                                                     
                                                                                                     
                                                                                                     br(),
                                                                                                     h3("Data Sources"),
                                                                                                     tags$a(href="https://www.ecs.org/research-reports/key-issues/postsecondary-affordability/", "Education Commission of The States: Postsecondary Affordability"),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.ecs.org/research-reports/key-issues/early-childhood-education/", "Education Commission of The States: Early Childhood Education"),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.ecs.org/research-reports/key-issues/workforce-development/", "Education Commission of The States: Workforce Development"),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.ecs.org/research-reports/key-issues/school-climate/", "Education Commission of The States: School Climate"),
                                                                                                     br(),
                                                                                                     tags$a(href="https://safesupportivelearning.ed.gov/sites/default/files/discipline-compendium/Oregon%20School%20Discipline%20Laws%20and%20Regulations.pdf/", "Oregon Compilation of School Discipline Laws and Regulations"),
                                                                                                     br(),
                                                                                                     tags$a(href="https://safesupportivelearning.ed.gov/sites/default/files/discipline-compendium/Virginia%20School%20Discipline%20Laws%20and%20Regulations.pdf", "Virginia Compilation of School Discipline Laws and Regulations"),
                                                                                                     br(),
                                                                                                     tags$a(href="https://safesupportivelearning.ed.gov/sites/default/files/discipline-compendium/Iowa%20School%20Discipline%20Laws%20and%20Regulations.pdf", "Iowa Compilation of School Discipline Laws and Regulations"),
                                                                                                     br(),
                                                                                                     h3("References"),
                                                                                                     
                                                                                                     
                                                                                                     
                                                                                                     tags$a(href="https://www.brookings.edu/research/hitting-kids-american-parenting-and-physical-punishment/", "Brookings Corporal Punishment"),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.pnas.org/content/116/17/8255", "PNAS Corporal Punishment"),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.ecs.org/50-state-comparison-postsecondary-education-funding/", "ECS Early Childhood Programs as Economic Development Tool"),
                                                                                                     br(),
                                                                                                     tags$a(href="https://cew.georgetown.edu/cew-reports/recovery-job-growth-and-education-requirements-through-2020/", "Georgetown Job Growth and Education Requirements through 2020"),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.luminafoundation.org/news-and-views/does-higher-education-really-increase-economic-mobility/", "Lumina Foundation: Does higher education really increase economic mobility?")
                                                                                                     
                                                                                              )
                                                                                     ) #close fluid row
                                                                                     ,
                                                                                     br(),
                                                                                     br()
                                                                            ) # Data Sources & References panel
                                                                            
                                                                            # tabPanel("View the Subcategory Data",
                                                                            #          fluidRow(width =12,
                                                                            #                   column(1),
                                                                            #                   column(10, h3(strong("Education Data Set")),
                                                                            #                          p("These are the subcategories used in all graphs. These subcategories are derived from more detailed questions."),
                                                                            #                          DT::dataTableOutput("edutable")
                                                                            #                          
                                                                            #                          
                                                                            #                   )
                                                                            #          )#close fluidrow
                                                                            # )
                                                                            
                                                                   ), 
                                                                   
                                                                   
                                                                   tabPanel(title = "Employment",
                                                                            fluidRow( 
                                                                              
                                                                              column(4,
                                                                                     #----------
                                                                                     br(),
                                                                                     strong("Background"),
                                                                                     br(),
                                                                                     br(style="text-align: justify;","The majority of research on economic mobility focuses on income, particularly, labor income.  Policies regarding employment are essential to connect the ability to generate income of individuals with the probability to improve social and economic status.  Communities with adequate policies enhancing employment show significant improvements to overcome barriers that commonly maintain low levels of mobility.  Employment is the fastest and probably the most direct mechanism to access services of a strong and “healthy” middle class, such as, housing, childcare, high-performing schools, safe neighborhoods, college education, etc. Zimmerman (2008) suggests that there is evidence to support that increasing minimum wage legislation potentially benefits a considerable proportion of the population and legislation favoring unions increases mobility since union members typically earn higher wages than non-members (Card, 1996. Shea, 1997)."),
                                                                                     br(),
                                                                                     br(style="text-align: justify;","Three aspects serve as a theoretical umbrella to understand the impact of employment related policies on social mobility: wage legislation, organizing capacity and considerations for protections."),
                                                                                     br(style="text-align: justify;", strong("Wage"), "legislation seeks to highlight the existence of local policies regarding minimum wages.", strong("Organizing"), "refers to the presence of union-friendly orientation.  Finally,", strong("Protection"), " covers a wide range of details concerning different aspect of employment protection that go beyond monetary aspects and include paid sick leave, equal pay mandates, pregnancy benefits, family care, etc.  These categories are suggested by the Report on the Work index by Oxfam (Oxfam, 2018).  For instance, Oregon seems to have a high rank of work index since it has the fourth highest minimum wage, part of the top states allowing organization of workers in 2018, etc. On the other hand, Virginia seems to have one of the lowest minimum wages along with other 21 states, and Iowa occupies a middle position among all the states according to the Oxfam ranking.  ")
                                                                                     
                                                                              ),
                                                                              column(8,
                                                                                     
                                                                                     h3(strong( "Asset Map")),
                                                                                     br('The following figure summarizes the extent of every domain for each state.'),
                                                                                     ###graph here
                                                                                     plotOutput("political_dom_emp",  width = "auto", height=800)
                                                                              )
                                                                              
                                                                              
                                                                            ),
                                                                            hr(),
                                                                            tabPanel("Data Sources & References",
                                                                                     
                                                                                     
                                                                                     fluidRow(width =12,
                                                                                              column(1),
                                                                                              column(10, h3(strong("Data Sources and References")),
                                                                                                     br(),
                                                                                                     
                                                                                                     h3("Data Sources"),
                                                                                                     tags$a(href="https://policy-practice.oxfamamerica.org/work/poverty-in-the-us/best-states-to-work/",
                                                                                                            "OXFAM: \"The Best and Worst States to work in America\""),
                                                                                                     br(),
                                                                                                     tags$a(href="https://statusofwomendata.org/state-data/",
                                                                                                            "Status of Women: State Data"),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.osha.gov/stateplans ",
                                                                                                            "OSHA: State Plans"),
                                                                                                     br(),
                                                                                                     
                                                                                                     h3("References"),
                                                                                                     tags$a(href="https://www.urban.org/sites/default/files/publication/31191/1001163-labor-market-institutions-and-economic-mobility.pdf", "Zimmerman, S.: \"Labor market institutions and economic mobility\""),
                                                                                                     br(),
                                                                                                     tags$a(href="https://davidcard.berkeley.edu/papers/union-struct-wage.pdf", "Card, David.: The Effect of Unions on the Structure of Wages: A Longitudinal Analysis."),
                                                                                                     br(),
                                                                                                     tags$a(href="https://s3.amazonaws.com/oxfam-us/www/static/media/files/Best_States_to_Work_Index.pdf", "OMFAM: \"The Best States To Work Index. A Guide To Labor Policy in US States\" "),
                                                                                                     br(),
                                                                                                     tags$a(href="http://papers.nber.org/papers/w6026 ", "Shea, John: \"Does Parents’ Money Matter?\" ")
                                                                                                     
                                                                                              )
                                                                                     ) #close fluid row
                                                                                     ,
                                                                                     br(),
                                                                                     br()
                                                                            ) # Data Sources & References panel
                                                                            
                                                                            # tabPanel("View the Data",
                                                                            #          fluidRow(width =12,
                                                                            #                   column(1),
                                                                            #                   column(10, h3(strong("Employment Set")),
                                                                            #                          DT::dataTableOutput("emptable")
                                                                            #                          
                                                                            #                   )
                                                                            #          )#close fluidrow
                                                                            # ) 
                                                                            
                                                                   ), 
                                                                   
                                                                   
                                                                   
                                                                   
                                                                   tabPanel(title = "Housing",
                                                                            fluidRow(
                                                                              
                                                                              column(4,  
                                                                                     br(),
                                                                                     strong("Background"),
                                                                                     br(style="text-align: justify;", "Housing policies are crucial to evidence how policies may affect economic mobility.  Low income families struggle to obtain low housing prices.  We researched various housing and zoning policies to better understand which legislation may promote or delay mobility."),
                                                                                     br(),
                                                                                     br(style="text-align: justify;","There are three main subdomains within housing and zoning policy: assistance policies, financial policies, and development policies."),
                                                                                     br(),
                                                                                     br(style="text-align: justify;",strong("Assistance"), "policies are programs and discounts which aid in reducing the cost of housing for disadvantaged individuals. Loan assistance programs for disabled members and first-time homeowners are examples."),
                                                                                     br(),
                                                                                     br(style="text-align: justify;","Housing", strong("Financial"), " policy describes policies which aid in covering costs to help provide a fair financial environment when purchasing or renting homes. This includes loan assistance programs, home price discounts and tax exemptions. By understanding housing financial policies and their effects on communities, we can understand which policies cultivate the ideal environment for economic mobility."),
                                                                                     br(),
                                                                                     br(style="text-align: justify;",strong("Development"), " policies are land use and planning regulations that influence the cost and equity of housing. Restricting the development of multi-unit housing, for example, can drive up the cost of housing. ")
                                                                                     
                                                                              ), 
                                                                              
                                                                              column(8, 
                                                                                     h3(strong( "Asset Map")),
                                                                                     br('The following figure summarizes the extent of every domain for each state.'),
                                                                                     ###graph here
                                                                                     plotOutput("political_dom_hou",  width = "auto", height=800)     
                                                                              )
                                                                            ),
                                                                            hr(),
                                                                            
                                                                            tabPanel("Data Sources & References",
                                                                                     
                                                                                     
                                                                                     fluidRow(width =12,
                                                                                              column(1),
                                                                                              column(10, h3(strong("Data Sources and References")),
                                                                                                     br(),
                                                                                                     
                                                                                                     h3("Data Sources"),
                                                                                                     tags$a(href="https://www.fha.com/fha-grants?state=OR#:~:text=First%20Time%20Home%20Buyer%20Loan,within%20the%20City%20of%20Corvallis.", "Federal Housing Administration (FHA): \"States with First Time Home Buyer Programs\""),
                                                                                                     br(),
                                                                                                     tags$a(href="https://smartasset.com/mortgage/first-time-home-buyer-programs-iowa", "Smart Asset: \"First Time Home Buyer Programs in Iowa (2019)\""),
                                                                                                     br(),
                                                                                                     tags$a(href="https://m.vhda.com/loancombo.aspx", "Virginia Housing Development Authority (VHDA): \"Virginia Housing Loan Combo\""),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.legis.iowa.gov/docs/code/16.54.pdf", "Iowa Finance Authority (IFA): \"Home Ownership Assistance Programs in Iowa\""),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.vhda.com/Programs/Pages/MilitaryVeteransPrograms.aspx", "Virginia Housing Development Authority (VHDA): \"Virginia Housing and the US military\""),
                                                                                                     br(),
                                                                                                     tags$a(href= "https://www.iowafinance.com/homeownership/mortgage-programs/military-homeownership-assistance-program/#:~:text=We'd%20like%20to%20help,and%20Homes%20for%20Iowans%20programs", "Iowa Finance Authority (IFA): \"Military Homeownership Assistance Program\""),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.oregon.gov/odva/Benefits/Pages/Home-Loans.aspx#:~:text=ODVA%20Home%20Loan%20Program,than%20334%2C000%20veterans%20since%201945", "Oregon Department of Veterans' Affairs (ODVA): \"Benefits and Programs\""),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.militarytimes.com/home-hq/2018/08/21/not-just-va-7-more-states-with-veteran-friendly-home-loan-programs/", "Military Times: \"States with Veteran-Friendly Home Loan Programs\""),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.vhda.com/Programs/Pages/GrantingFreedom.aspx", "Virginia Housing Development Authority (VHDA): \"Granting Freedom Program\""),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.dvs.virginia.gov/benefits/real-estate-tax-exemption", "Virginia Department of Veterans' Services: \"Real Estate Tax Exemption\""),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.vhda.com/Programs/Pages/Programs.aspx", "Virginia Housing Development Authority (VHDA): \"Virginia Housing Programs\""),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.self.inc/blog/the-complete-guide-to-home-loans-for-people-with-disabilities", "Self: \"The Complete Guide to Home Loans for People with Disabilities\""),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.disabled-world.com/disability/finance/american-home-loans.php", "Disabled World: \"Disability Housing and Home Loans for Disabled Americans\""),
                                                                                                     br(),
                                                                                                     tags$a(href="https://tax.iowa.gov/sites/default/files/2019-08/PTCandRRPForecast.pdf ", "Iowa Department of Revenue: \"Iowa’s Disabled and Senior Citizens Property Tax Credit and Rent Reimbursement Program Expenditure Projections Study\""),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.eldercaredirectory.org/state-resources.htm", "Eldercare Directory: \"State Resources\""),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.hud.gov/states/virginia/homeownership/seniors", "The United States Department of Housing and Urban Development (HUD): \"Housing Resources for Seniors: Virginia\""),
                                                                                                     br(),
                                                                                                     tags$a(href="https://vda.virginia.gov/", "VDA: \"Office of Aging Services\""),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.seniorresource.com/virginia.htm", "Senior Resource: \"Virginia Senior Resources\""),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.hud.gov/states/virginia/renting", "The United States Department of Housing and Urban Development (HUD): \"Virginia Rental Help\""),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.portland.gov/phb/nplte#:~:text=In%201985%2C%20Oregon%20legislature%20authorized,held%20by%20charitable%2C%20nonprofit%20organizations.&text=program%20to%202027.-,The%20tax%20exemption%20is%20intended%20to%20benefit%20low%2Dincome%20renters,that%20provide%20this%20housing%20opportunity", "City of Portland, Oregon: \"Non-Profit Low Income Housing Limited Tax Exemption (NPLTE)\""),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.vhda.com/BusinessPartners/MFDevelopers/LIHTCProgram/Pages/LIHTCProgram.aspx", "Virginia Housing Development Authority (VHDA): \"Low-Income Housing Tax Credit Program\""),
                                                                                                     br(),
                                                                                                     tags$a(href="https://tax.iowa.gov/tax-credits-and-exemptions#:~:text=Iowa%20Low%2DRent%20Housing%20Exemption&text=Eligibility%3A%20Property%20owned%20and%20operated,no%20later%20than%20February%201", "Iowa Department of Revenue: \"Tax Credits and Exemptions\""),
                                                                                                     # br()
                                                                                                     # ,
                                                                                                     # tags$a(href="https://www.veteransunited.com/futurehomeowners/veteran-property-tax-exemptions-by-state/#:~:text=A%20veteran%20in%20Iowa%20may,of%2018%20months%20during%20peacetime.&text=More%20exemptions%20exist%20for%20veterans,Read%20more", "Veterans United: \"Veteran Property Tax Exemption by State\""),
                                                                                                     # br(),
                                                                                                     # tags$a(href="https://www.dvs.virginia.gov/benefits/real-estate-tax-exemption", "Virginia Department of Veterans Services (DVS): \"Real Estate Tax Exemption\""),
                                                                                                     # br(),
                                                                                                     # tags$a(href="https://www.oregon.gov/dor/programs/property/Pages/exemptions.aspx", "Oregon Department of Revenue: \"Property tax exemptions\""),
                                                                                                     # br(),
                                                                                                     # tags$a(href="https://law.lis.virginia.gov/vacode/title58.1/chapter32/section58.1-3219.5/", "Virginia Law Library: \"Exemption from taxes on property for disabled veterans\""),
                                                                                                     # br(),
                                                                                                     # tags$a(href="https://ballotpedia.org/Virginia_Property_Tax_Exemption_for_Elderly_and_Disabled,_Question_1_(2010)", "Ballotpedia: \"irginia Property Tax Exemption for Elderly and Disabled\""),
                                                                                                     # br(),
                                                                                                     # tags$a(href="https://www.nerdwallet.com/article/mortgages/oregon-first-time-home-buyer-programs#:~:text=Oregon%20RateAdvantage%20Home%20Loan%20for,put%20towards%20your%20home%20purchase", "Nerd Wallet: \"Oregon First-Time Home Buyer Programs of 2020\""),
                                                                                                     # br(),
                                                                                                     # tags$a(href="https://olis.leg.state.or.us/liz/2019R1/Measures/Overview/HB2006", "Oregon State Legislature: \"HB2006\""),
                                                                                                     # br(),
                                                                                                     # tags$a(href="https://www.lamberthomeinspections.com/tax-breaks-for-virginia-homeowners/", "Lambert Home Inspections: \"Tax Breaks for Virginia Homeowners\""),
                                                                                                     # br(),
                                                                                                     # tags$a(href="https://support.taxslayer.com/hc/en-us/articles/360015707812-What-type-of-credits-are-available-on-my-Oregon-return-", "Oregon TaxSlayer: \"What type of credits are available on my Oregon return?\""),
                                                                                                     # br(),
                                                                                                     # tags$a(href="https://www.oregon.gov/dor/programs/individuals/pages/credits.aspx", "Oregon Department of Revenue: \"Oregon credits\""),
                                                                                                     # br(),
                                                                                                     # tags$a(href="https://www.tax.virginia.gov/tax-credits", "Virginia Tax: \"Virginia Tax Credits\""),
                                                                                                     # br(),
                                                                                                     # tags$a(href="https://wallethub.com/edu/states-with-the-highest-and-lowest-property-taxes/11585/", "WalletHub: \"Property Taxes by State\""),
                                                                                                     # br(),
                                                                                                     # tags$a(href="https://www.vhda.com/Homebuyers/Pages/homebuyers.aspx", "Virginia Housing Development Authority (VHDA): \"Virginia Homebuyers\""),
                                                                                                     # br(),
                                                                                                     # tags$a(href="https://www.teachernextdoor.us/Virginia", "Teacher Next Door: \"Virginia\""),
                                                                                                     # br(),
                                                                                                     # tags$a(href="https://www.oregonlive.com/business/2019/09/down-payment-program-for-teachers-trying-to-buy-a-house-comes-to-expensive-portland.html", "The Oregonian: \"Down-payment program for teachers trying to buy a house comes to expensive Portland\""),
                                                                                                     # br(),
                                                                                                     # tags$a(href="https://www.dhcd.virginia.gov/cdbg-planning-grants", "Virginia Department of Housing and Community Development (DHCD): \"CDBG Planning Grants\""),
                                                                                                     # br(),
                                                                                                     # tags$a(href="https://www.hud.gov/states/oregon/community/cdbg", "The United States Department of Housing and Urban Development (HUD): \"Oregon CDBG\""),
                                                                                                     # br(),
                                                                                                     # tags$a(href="https://www.hudexchange.info/programs/cdbg-state/", "The United States Department of Housing and Urban Development (HUD): \"State Community Development Block Grant Program\""),
                                                                                                     # br(),
                                                                                                     # tags$a(href="https://www.hud.gov/sites/documents/19565_CDBG.PDF", "The United States Department of Housing and Urban Development (HUD): \"State Community Development Block Grant\""),
                                                                                                     # br(),
                                                                                                     # tags$a(href="https://www.iowagrants.gov/insideLinkOpps.jsp?documentPk=1314908543321#:~:text=Approximately%20%241.5%20million%20in%20federal,of%20Iowa's%20Community%20Facilities%20Fund.&text=Communities%20with%20populations%20greater%20than%2015%2C000%20can%20receive%20up%20to%20%24800%2C000", "Iowa Grants: \"Opportunities\""),
                                                                                                     # br(),
                                                                                                     # tags$a(href="https://www.iowaeconomicdevelopment.com/CDBGHousing", "Iowa Economic Development: \"CDBG Housing Fund\""),
                                                                                                     # br(),
                                                                                                     # tags$a(href="https://www.dss.virginia.gov/geninfo/reports/agency_wide/block_grants.cgi", "Virginia Department of Social Services: \"VDSS Block Grants\""),
                                                                                                     # br(),
                                                                                                     # tags$a(href="https://en.wikipedia.org/wiki/Housing_trust_fund#/media/File:United_States_Housing_Trust_Support_by_State.svg", "United States Housing Trust): \"United States Housing Support by State\""),
                                                                                                     # br(),
                                                                                                     # tags$a(href="https://www.tld-inc.com/news/2019/01/iowa-itemized-deductions-2018-tax-year#:~:text=Qualified%20home%20mortgage%20interest%20deduction,%24100%2C000%20of%20home%20equity%20loans", "Terry Lockridge & Dunn: \"Iowa Itemized Deductions for 2018 Tax Year\""),
                                                                                                     # br(),
                                                                                                     # tags$a(href="https://www.tax.virginia.gov/deductions", "Virginia Tax: \"Deductions\""),
                                                                                                     # br(),
                                                                                                     # tags$a(href="https://www.ocpp.org/2019/03/11/hb-3349-reform-oregons-mortgage-interest-deduction/#:~:text=Oregon's%20mortgage%20interest%20deduction%20is,for%20mortgages%20prior%20to%202018", "Oregon Center for Public Policy: \"HB 3349: Reform Oregon's Mortgage Interest Deduction\""),
                                                                                                     # br(),
                                                                                                     # tags$a(href="https://www.portlandoregon.gov/citycode/28465", "The City of Portland, Oregon: \"Chapter 3.102 Property Tax Exemption for New Construction of Single-Unit Housing in Homebuyer Opportunity Areas\""),
                                                                                                     # br(),
                                                                                                     # tags$a(href="https://www.oregon.gov/lcd/op/pages/goals.aspx", "Oregon Planning): \"Oregon's Statewide Land Use Planning Goals\""),
                                                                                                     # br(),
                                                                                                     # tags$a(href="https://www.iowaeconomicdevelopment.com/LandUsePlanning", "Iowa Economic Development): \"Land Use Planning\""),
                                                                                                     # br(),
                                                                                                     # tags$a(href="file:///Users/tasfiachowdhury/Downloads/APA-VA-Chapter-Toolbox-2016.pdf", "The Virginia Chapter of The American Planning Association: \"Managing Growth and Development in Virginia\""),
                                                                                                     # br(),
                                                                                                     # tags$a(href="http://www.virginiaplaces.org/landuseplan/", "Virginia Places: \"Land Use Planning in Virginia\""),
                                                                                                     # br(),
                                                                                                     # tags$a(href="https://projects.arlingtonva.us/plans-studies/general-land-use-plan/", "The City of Arlington, Virginia: \"General Land Use Plan (GLUP)\""),
                                                                                                     # br(),
                                                                                                     # tags$a(href="https://www.rd.usda.gov/or", "United States Department of Agriculture (USDA): \"Key Programs in Oregon\""),
                                                                                                     # br(),
                                                                                                     # tags$a(href="https://www.vhda.com/about/Planning-Policy/Pages/StrategicPlanningProcess.aspx", "Virginia Housing Development Authority (VHDA): \"VHDA's Strategic Planning Process\""),
                                                                                                     # br(),
                                                                                                     # tags$a(href="https://www.vhda.com/SiteCollectionDocuments/StrategicPlan.pdf", "Virginia Housing Development Authority (VHDA): \"VHDA Strategic Direction\""),
                                                                                                     # 
                                                                                                     # br(),
                                                                                                     # tags$a(href="https://oregonlawhelp.org/resource/reasonable-rules-in-mobile-home-parks-and-flo", "Law Help: \"Oregon Mobile Home Laws\""),
                                                                                                     # 
                                                                                                     # br(),
                                                                                                     # tags$a(href="https://www.oregon.gov/ohcs/development/pages/index.aspx", "Oregon Housing Development: \"Housing Development\""),
                                                                                                     # br(),
                                                                                                     # tags$a(href="https://www.vacommunitycapital.org/news/2019/08/19/more-affordable-housing-in-more-virginia-places/", "Virginia Community Capital: \"More Affordable Housing in Virginia\""),
                                                                                                     # br(),
                                                                                                     # tags$a(href="https://www.vhda.com/Programs/Pages/Low-IncomeHousingTaxCreditProgram.aspx", "Virginia Housing Development Authority (VHDA): \"Low-Income Housing Tax Credit Program\""),
                                                                                                     # br(),
                                                                                                     # tags$a(href="https://www.portland.gov/bps/adap/gentrification-and-displacement-studies", "The City of Portland, Oregon: \"Gentrification and Displacement Studies\""),
                                                                                                     br(),
                                                                                                     tags$a(href="https://bpr.berkeley.edu/2018/06/01/how-portlands-right-to-return-is-indeed-right-to-return-housing-to-the-underrepresented/ ", "Berkeley Political Review: \"Portland's 'Right to Return'\""),
                                                                                                     
                                                                                                     
                                                                                                     h3("References"),
                                                                                                     tags$a(href="https://www.urban.org/sites/default/files/alfresco/publication-pdfs/2000428-Housing-Policy-Levers-to-Promote-Economic-Mobility.pdf", "Urban Institute: \"Housing Policy Levers to Promote Economic Mobility\""),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.cato.org/publications/policy-analysis/zoning-land-use-planning-housing-affordability", "The Cato Institute: \"Zoning, Land‐Use Planning, and Housing Affordability\""),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.dcpolicycenter.org/publications/economic-cost-land-use/", "DC Policy Center: \"The economic costs of land use regulations\""),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.urban.org/sites/default/files/publication/98758/lithc_how_it_works_and_who_it_serves_final_2.pdf", "Urban Institute: \"The Low-Income Housing Tax Credit\"")
                                                                                                     
                                                                                              )
                                                                                     ) #close fluid row
                                                                                     ,
                                                                                     br(),
                                                                                     br()
                                                                            ) # Data Sources & References panel
                                                                            
                                                                            # tabPanel("View the Data",
                                                                            #          fluidRow(width =12,
                                                                            #                   column(1),
                                                                            #                   column(10, h3(strong("Housing & Zoning Data Set")),
                                                                            #                          DT::dataTableOutput("housetable")
                                                                            #                          
                                                                            #                   )
                                                                            #          )#close fluidrow
                                                                            # ) # close Data tab
                                                                            
                                                                   ),
                                                                   
                                                                   tabPanel(title = "Law Enforcement",
                                                                            fluidRow(
                                                                              column(4, 
                                                                                     br(),
                                                                                     strong("Background"),
                                                                                     br(style="text-align: justify;", "Law enforcement policies play an essential role in economic
                                                                                       mobility. Having a criminal record
                                                                                       increases the difficulty to obtain a job. Moreover, the ramifications of a criminal record or an encounter with the law are 
                                                                                       felt most by male citizens,
                                                                                       particularly, Hispanic or Black men. Therefore, law enforcement becomes an increasingly important aspect of
                                                                                       political capital that must be studied to understand economic mobility.  "),
                                                                                     
                                                                                     br(style="text-align: justify;", "Our research on law enforcement practices and policies identified of three main subdomains of interest: arrest and court proceedings, incarceration and community policing practices. The three subdomains are comprised of 20 policy questions which assess the existence or non-existence of a practice.  The entire dataset, both binary and qualitative, can be found by clicking on the “download CSV” button in the All Data tab in the Summary section of Data, Methods and Measures"),
                                                                                     br(style="text-align: justify;", "a.", strong("Arrest and Court Proceeding Policies"), " focused on the process of arresting and trying individuals in court. We analyzed stop and identify, bail, and civil asset forfeiture policies. Practices revealed inequalities across distinct socio-economic groups. For example, paying cash bail or having your assets seized has an effect on and is affected by an individual’s financial standing. In addition, we explored zero tolerance policies related to driving under the influence. "),
                                                                                     br(style="text-align: justify;", "b.", strong("Incarceration Practices"), " covers the policies that impact individuals held in state facilities. We focused on inmates’ rights as well as the equitability and social justness of practices within the facility and upon return to their communities.  Specifically, we assessed the ability to acquire skills and certifications, as well as, access necessary healthcare, youth adjudication and the death penalty. "),
                                                                                     br(style="text-align: justify;", "c.", strong("Community Policing Practices"), "explores the standards that officers must abide by in policing the community with a focus on the equality of standards. For example, custodial sexual misconduct policies are used to assess how states hold officers accountable for allegations of misconduct towards individuals under their custody. We include policies on body camera usage, demographic information collection and domestic violence related polices. Also, the nature of officer training programs, particularly those pertaining to treating individuals with mental health issues.")
                                                                                     ),
                                                                              
                                                                              column(8,
                                                                                     h3(strong( "Asset Map")),
                                                                                     br('The following figure summarizes the extent of every domain for each state.'),
                                                                                     ###graph here
                                                                                     plotOutput("political_dom_law",  width = "auto", height=800)
                                                                              )
                                                                              ),
                                                                            hr(),
                                                                            tabPanel("Data Sources & References",
                                                                                     
                                                                                     
                                                                                     fluidRow(width =12,
                                                                                              column(1),
                                                                                              column(10, h3(strong("Data Sources and References")),
                                                                                                     hr(), h3("Data Sources"),
                                                                                                     downloadButton("downloadData", "Download CSV"),
                                                                                                     
                                                                                                     br(),
                                                                                                     p("Key Data Sources are listed below. The entire list can be found by downloading the entire domain-specific dataset using the button above."),
                                                                                                     
                                                                                                     tags$a(href="https://justiceforwardva.com/bail-reform#:~:text=As%20it%20stands%2C%20Virginia%20employs,whether%20pretrial%20release%20is%20appropriate.&text=If%20a%20person%20cannot%20make,to%20pay%20the%20money%20bail.", "Justice Forward Virginia: Bail"),
                                                                                                     br(),
                                                                                                     tags$a(href="https://ij.org/activism/legislation/civil-forfeiture-legislative-highlights/", "Institute for Justice: Civil Forfeiture Reforms on the State Level"),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.aclu.org/state-standards-pregnancy-related-health-care-and-abortion-women-prison-0#hd4", "ACLU: State Standards For Pregnancy-related Health Care and Abortion for Women in Prison"),
                                                                                                     br(),
                                                                                                     tags$a(href="https://static.prisonpolicy.org/scans/sprcsmstatelaw.pdf", "PrisonPolicy.Org: Custodial Sexual Misconduct Laws: A State-by-State Legislative Review"),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.ncsl.org/research/civil-and-criminal-justice/state-trends-in-law-enforcement-legislation-2014-2017.aspx", "National Conference of State Legislature: State Trends in Law Enforcement"),
                                                                                                     br(),
                                                                                                     tags$a(href="https://statusofwomendata.org/explore-the-data/state-data/oregon/#violence-safety", "Status of Women in the United States"),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.sentencingproject.org/publications/private-prisons-united-states/#:~:text=In%20six%20states%20the%20private,%2C%20and%20Georgia%20(110%25).", "Sentencing Project: Private Prisons in the United States"),
                                                                                                     
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.courts.oregon.gov/programs/inclusion/Documents/juvrights.pdf", "Courts.Oregon.Org: YOUTH FACES THE LAW:A Juvenile Rights Handbook"),
                                                                                                     br(),
                                                                                                     tags$a(href="https://deathpenaltyinfo.org/state-and-federal-info/state-by-state", "Death Penalty Information Center: State by State"),
                                                                                                     br(),
                                                                                                     
                                                                                                     h3("References"),
                                                                                                     
                                                                                                     tags$a(href="https://www.theatlantic.com/politics/archive/2015/12/how-families-pay-the-never-ending-price-of-a-criminal-record/433641/", "The Atlantic: How Families Pay the Never-Ending Price of a Criminal Record"),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.ncjrs.gov/pdffiles1/nij/grants/244756.pdf ", "NCJRS: Criminal Stigma, Race, Gender and Employment")
                                                                                              )
                                                                                     ) #close fluid row
                                                                                     ,
                                                                                     br(),
                                                                                     br()
                                                                            ) # Data Sources & References panel
                                                                            # tabPanel("View the Data",
                                                                            #          
                                                                            #          fluidRow(width =12,
                                                                            #                   column(1),
                                                                            #                   column(10, h3(strong("Law Enforcement Data Set")),
                                                                            #                          DT::dataTableOutput("lawtable")
                                                                            #                          
                                                                            #                   )
                                                                            #          )#close fluidrow
                                                                            #          
                                                                            # ) # close Data tab
                                                                            
                                                                            ),
                                                                   
                                                                   
                                                                   #new tab
                                                                   tabPanel(title = "Taxation",
                                                                            
                                                                            fluidRow( 
                                                                              
                                                                              column(4,
                                                                                     #----------
                                                                                     br(), 
                                                                                     strong("Background"),
                                                                                     br(style="text-align: justify;", "
                                                                                       Taxation may influence economic mobility since it may change the patterns of wealth accumulation and the distribution of resources in society.
                                                                                       Tax revenues are used to fund goods and services that drive mobility, under the principle of equality, such as education and health, and tax deduction and credit policies
                                                                                       lower the cost of mobility enhancing goods. Taxation can also lead to the redistribution of wealth, an important part of combating wealth inequality."),
                                                                                     br(style="text-align: justify;", "Since 1979, income inequality in the United States has increased dramatically. In every state, the average income of the top 5% of households is at least 10 times
                                                                                       that of the poorest 20%.  It is vital to understand how legislation of states may implement more progressive tax policies and re-evaluate regressive
                                                                                       structures to boost economic mobility. Our research identified four main subdomains of tax policy: tax credits, wealth-related taxes, business tax policy, and the Gini index."),
                                                                                     br(style="text-align: justify;", strong("a. Tax credits"), "are negative marginal tax rates, or tax incentives, that reduce tax liability and increase tax refunds, which may improve economic mobility for low-income individuals.
                                                                                       They ease low- to moderate-income family burdens by providing appropriate financial support for expenses like childcare, income tax, and property tax.  "),
                                                                                     br(style="text-align: justify;", strong("b: Taxes on inherited wealth"), "such as the estate and inheritance tax, largely affect the wealthiest individuals. These taxes help redistribute income and wealth and, thus improve economic mobility.
                                                                                       Since wealth concentration has exacerbated in recent decades, wealth-related taxes help upend financial barriers for low-income people.  "),
                                                                                     br(style="text-align: justify;", strong("c: Businesses"), "create opportunities for employment, thus increasing incomes, and provide access to services that increase future earning potentials.
                                                                                       States play a significant role in supporting businesses by nullifying corporate tax avoidance strategies to equalize the playing field between multimillion-dollar corporations and small businesses, as well as, creating a tax climate that fosters entrepreneurial efforts. "),
                                                                                     br(style="text-align: justify;", strong("d.  The Gini coefficient"), "is a measure of  dispersion intended to represent income or wealth inequality in a nation or area. Because the Gini coefficient measures inequality after the effects of taxes, by understanding how Gini indexes change as a result
                                                                                       of tax policies and financial redistribution, we can better understand how tax policy can support economic mobility.  ")
                                                                                     
                                                                                     ),
                                                                              
                                                                              column(8,
                                                                                     h3(strong( "Asset Map")),
                                                                                     br('The following figure summarizes the extent of every domain for each state.'),
                                                                                     ###graph here
                                                                                     plotOutput("political_dom_tax",  width = "auto", height=800))
                                                                              
                                                                                     ), 
                                                                            hr(),
                                                                            tabPanel("Data Sources & References",
                                                                                     
                                                                                     
                                                                                     fluidRow(width =3,
                                                                                              column(1),
                                                                                              column(10, h3(strong("Data Sources and References")),
                                                                                                     br(),
                                                                                                     h3("Data Sources"),
                                                                                                     tags$a(href = "https://www.americanadoptions.com/blog/your-state-adoption-tax-credit-and-how-you-can-protect-it/", "American Adoptions"),
                                                                                                     br(),
                                                                                                     tags$a(href = "https://www.cbpp.org/27-states-plus-dc-require-combined-reporting-for-the-state-corporate-income-tax",
                                                                                                            "CBPP: 27 states plus DC Require Combined Reporting for the State Corporate Income Tax"),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.irs.gov/credits-deductions/individuals/earned-income-tax-credit/states-and-local-governments-with-earned-income-tax-credit",
                                                                                                            "IRS: States and Local Governments with Earned Income Tax Credit "),
                                                                                                     br(),
                                                                                                     tags$a(href = "https://itep.org/property-tax-circuit-breakers-2019/", "ITEP: Property Tax Circuit Breakers in 2019"),
                                                                                                     br(),
                                                                                                     tags$a(href = "https://www.livestories.com/statistics/iowa/des-moines-county-gini-index-income-inequality",
                                                                                                            "Live Stories: Des Moines County Gini Index of Income Inequality"),
                                                                                                     br(),
                                                                                                     tags$a(href = "https://opportunityindex.cfnova.org/indicator/chart?region=&demographic=&indicator=12&date_start=2005&date_end=2017",
                                                                                                            "Opportunity Index of Northern Virginia: Gini Coefficient"),
                                                                                                     br(),
                                                                                                     tags$a(href = "https://www.realized1031.com/capital-gains-tax-rate", "Realized: Capital Gain Tax Rates by State"),
                                                                                                     br(),
                                                                                                     tags$a(href = "https://files.taxfoundation.org/20180925174436/2019-State-Business-Tax-Climate-Index.pdf",
                                                                                                            "Tax Foundation: State Business Tax Climate Index"),
                                                                                                     br(),
                                                                                                     tags$a(href = "https://taxfoundation.org/state-corporate-income-tax-rates-brackets-2020/",
                                                                                                            "Tax Foundation: State Corporate Income Tax Rate Brackets 2020"),
                                                                                                     br(),
                                                                                                     tags$a(href="http://www.taxcreditsforworkersandfamilies.org/state-tax-credits/",
                                                                                                            "TCFW: State Tax Credits"),
                                                                                                     br(),
                                                                                                     tags$a(href = "https://www.thebalance.com/state-estate-tax-and-exemption-chart-3505462", "The Balance: State Estate Tax and Exemption Chart"),
                                                                                                     br(),
                                                                                                     tags$a(href = "https://www.thebalance.com/state-inheritance-tax-chart-3505460", "The Balance: State Inheritance Tax Charts"),
                                                                                                     br(),
                                                                                                     tags$a(href = "https://www.qualityinfo.org/-/wage-inequality-in-oregon-a-wide-gap", "Quality Info: Wage Inequality in Oregon"),
                                                                                                     br(),
                                                                                                     tags$a(href = "https://en.wikipedia.org/wiki/List_of_U.S._states_by_Gini_coefficient",
                                                                                                            "Wikipedia: List of U.S. States by Gini Coefficient"),
                                                                                                     br(),
                                                                                                     tags$a(href = "https://data.worldbank.org/indicator/SI.POV.GINI", "World Bank: Gini Index"),
                                                                                                     br(),
                                                                                                     
                                                                                                     h3("References"),
                                                                                                     tags$a(href="https://www.cbpp.org/research/state-budget-and-tax/how-state-tax-policies-can-stop-increasing-inequality-and-start",
                                                                                                            "CBPP: How State Tax Policies Can Stop Increasing Inequality and Start Reducing it"),
                                                                                                     br(),
                                                                                                     tags$a(href = "https://www.cbpp.org/research/state-budget-and-tax/state-taxes-on-inherited-wealth",
                                                                                                            "CBPP: State Taxes on Inherited Wealth"),
                                                                                                     br(),
                                                                                                     tags$a(href = "https://hbr.org/2015/01/3-ways-businesses-are-addressing-inequality-in-emerging-markets",
                                                                                                            "Harvard Business Review: 3 Ways Businesses are Addressing Inequality in Emerging Markets"),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.fool.com/taxes/2020/02/15/your-2020-guide-to-tax-credits.aspx",
                                                                                                            "Motley Fool: Your 2020 Guide to Tax Credits"),
                                                                                                     br()
                                                                                                     
                                                                                              )
                                                                                     ) #close fluid row
                                                                            ), # Data Sources & References panel
                                                                            br(),
                                                                            br()
                                                                            # tabPanel("View the Data",
                                                                            #          fluidRow(width =3,
                                                                            #                   column(1),
                                                                            #                   column(10, h3(strong("Taxation Data Set")),
                                                                            #                          DT::dataTableOutput("taxtable")
                                                                            #                          
                                                                            #                   )
                                                                            #          )#close fluidrow
                                                                            # )
                                                                            
                                                                            
                                                                            
                                                                              ), 
                                                                   
                                                                   
                                                                   tabPanel(title = "Voting",
                                                                            fluidRow( 
                                                                              
                                                                              column(4,
                                                                                     br(),
                                                                                     strong("Background"),
                                                                                     br(style="text-align: justify;", "Chetty et al. (2014) established a positive correlation between social capital 
                                                                                       and upward mobility. Social capital is a group level phenomena that reflects the cohesiveness of a community,
                                                                                       the connections between people and organizations. Quantifying social capital relies on surrogate measurements 
                                                                                       like the number of non-profits, response rate to the Census, voter turnout, the number of civic and social associations 
                                                                                       (Rupasingha et al., 2006)."),
                                                                                     br(),
                                                                                     br(style="text-align: justify;", "Here we focus on policies that have potential to impact voter turnout such as 
                                                                                       automatic voter registration, online registration, and voter photo ID requirements. Innovations in automatic voter 
                                                                                       registration have streamlined the way Americans register to vote, by providing automatic registration at DMV offices 
                                                                                       and social service agencies. These policies can dramatically increase the number of registered voters. For example, 
                                                                                       since Oregon became the first state in the nation to implement automatic voter registration in 2016, registration rates 
                                                                                       quadruple at DMV offices. In the first six months after automatic voter registration was implemented in Vermont on 
                                                                                       January 1, 2017, registration rates jumped 62 percent when compared to the first half of 2016. In contrast, strict photo 
                                                                                       ID policies block 11 percent of eligible voters that do not have government issued photo IDs and that percentage is even 
                                                                                       higher among seniors, minorities, people with disabilities, low-income voters, and students.")
                                                                                     ),
                                                                              column(8,
                                                                                     h3(strong( "Asset Map")),
                                                                                     br('The following figure summarizes the extent of every domain for each state.'),
                                                                                     ###graph here
                                                                                     plotOutput("political_dom_vot",  width = "auto", height=800)
                                                                              )
                                                                                     ),
                                                                            hr(), 
                                                                            
                                                                            tabPanel("Data Sources & References",
                                                                                     
                                                                                     
                                                                                     fluidRow(width =12,
                                                                                              column(1),
                                                                                              column(10, h3(strong("Data Sources and References")),
                                                                                                     br(),
                                                                                                     
                                                                                                     h3("Data Sources"),
                                                                                                     tags$a(href="https://www.ncsl.org/research/elections-and-campaigns/voter-id.aspx", "National Conference on State Legislatures: \" Voter Identification Requirements | Voter ID Laws\" "),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.ncsl.org/research/elections-and-campaigns/early-voting-in-state-elections.aspx", "National Conference on State Legislatures: \"State Laws Governing Early Voting \" "),
                                                                                                     br(),
                                                                                                     tags$a(href="https://evic.reed.edu/", "EVIC: Early Voting Information Center"),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.vote.org/early-voting-calendar/", "Vote.org: \"Early Voting by State \" "),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.elections.virginia.gov/casting-a-ballot/absentee-voting/index.html", "Virgnia Department of Elections: \" Absentee and Early Voting\""),
                                                                                                     br(),
                                                                                                     tags$a(href="https://ballotpedia.org/Absentee/mail-in_voting", "Ballotopedia: Absentee/mail-in voting"),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.ncsl.org/research/elections-and-campaigns/absentee-and-early-voting.aspx", "National Conference on State Legislatures: \" Voting Outside the Polling Place- Absentee, All-Mail and other Voting at Home Options \" "),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.ncsl.org/research/elections-and-campaigns/felon-voting-rights.aspx", "National Conference on State Legislatures: \"Felon Voting Rights \""),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.ncsl.org/research/elections-and-campaigns/voter-registration.aspx", "National Conference on State Legislatures: \"Voter Registration \""),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.ncsl.org/research/elections-and-campaigns/automatic-voter-registration.aspx", "National Conference on State Legislatures: \"Automatic Voter Registration \""),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.brennancenter.org/our-work/research-reports/history-avr-implementation-dates", "Brennan Center for Justice: \"History of AVR & Implementation Dates \" "),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.fhwa.dot.gov/policyinformation/quickfinddata/qfdrivers.cfm", "Office of Highway Policy Information: \"Drivers and Driver Licensing\" "),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.ncsl.org/research/elections-and-campaigns/electronic-or-online-voter-registration.aspx", "National Conference on State Legislatures: \"Online Voter Registration \""),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.ncsl.org/research/elections-and-campaigns/voter-registration-deadlines.aspx", "National Conference on State Legislatures: \"Voter Registration Deadlines \""),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.vote.org/voter-registration-deadlines/", "Vote.org: \"Voter Registration Deadlines\" "),
                                                                                                     br(),
                                                                                                     tags$a(href="-https://www.ncsl.org/research/elections-and-campaigns/preregistration-for-young-voters.aspx", "National Conference on State Legislatures: \"Preregistration for Young \" "),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.elections.virginia.gov/registration/how-to-register/", "Virgnia Department of Elections: \"How to Register\" "),
                                                                                                     br(),
                                                                                                     
                                                                                                     
                                                                                                     h3("References"),
                                                                                                     tags$a(href="https://www.brennancenter.org/our-work/research-reports/automatic-voter-registration-summary", "Brennan Center for Justice: Automatic Voter Registration a Summary"),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.brennancenter.org/issues/ensure-every-american-can-vote/vote-suppression/voter-id", "Brennan Center for Justice: Voter ID"),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.nber.org/papers/w19843 ", "Chetty, R., Hendren, N., Kline, P., & Saez, E.: Where is the Land of Opportunity? The Geography of Intergenerational Mobility in the United States."),
                                                                                                     br(),
                                                                                                     tags$a(href="https://www.researchgate.net/publication/222822589_The_Production_of_Social_Capital_in_US_Counties", "Rupasingha, A., Goetz, S. J., & Freshwater, D.: The production of social capital in US counties. Journal of Socio-Economics")
                                                                                                     
                                                                                              ) # close column
                                                                                     ) #close fluid row
                                                                                     ,
                                                                                     br(),
                                                                                     br()
                                                                            ) # Data Sources & References panel
                                                                            
                                                                            # tabPanel("View the Data",
                                                                            #          fluidRow(width =12,
                                                                            #                   column(1),
                                                                            #                   column(10, h3(strong("Voting Data Set")),
                                                                            #                          DT::dataTableOutput("votetable")
                                                                            #                          
                                                                            #                   )
                                                                            #          )#close fluidrow
                                                                            # ) 
                                                                            
                                                                            
                                                                              )
                                                                   
                                                                            )
                                                                   )
                                                          #)
                                         
                                         
                                         ############# END POLICY ASSETS
                                         
                                         )
                                
                                                        ),

tabItem(tabName = "datamethods",
        fluidRow(style = "margin: 6px",
                 width = 12, 
                 
                 #new content from POLICY ASSETS
                 #conditionalPanel("input.finidx_choice == 'POLICY ASSETS'",
                 
                 fluidRow( width=12,
                           br(),
                           title = "Policy Assets",
                           width = 12,
                           #h4(strong("Domains of Policy Assets")),
                           # leafletOutput("plot_fin_index_agri")
                           
                          box(width = 12,
                              includeHTML("pcindex_info.html"  ) 
                              )
                             #p(style="text-align: justify;", "Policy qu"),
                             
                             #includeHTML("index_interpretation.html")
                           
                           
                 ),
                 #data tables
                 br(),
                 br(),
                 fluidRow(DT:: dataTableOutput("table_polcap"))
                 
                 
        )
),

tabItem(tabName = "contact",
        fluidRow(
          box(width = 4,
              title = "About Us",
              "We are a partnership of five universities—the University of Virginia, Virginia Tech, Virginia State University, 
              Iowa State University, and Oregon State University—funded by the Bill & Melinda Gates Foundation to pilot 
              an initiative that will use data science to unravel complex, community challenges and advance economic 
              mobility across Virginia, Iowa, and Oregon.", 
              br(""),
              a(href = "https://datascienceforthepublicgood.org/economic-mobility/about", "Learn more about us.")
          ),
          box(width = 4,
              title = "Advancing Economic Mobility",
              "This project is part of our Advancing Economic Mobility: Towards A National Community Learning Network initiative,
              which will amplify the viability of Cooperate Extension professionals to discover opportunities and enable the 
              integration of data-driven governance at local and state levels.", 
              br(""),
              a(href = "https://datascienceforthepublicgood.org/economic-mobility/", "Learn more about the initiative.")
          ),
          box(width = 4,
              title = "Contact",
              "Please direct inquiries to", a(href = "https://biocomplexity.virginia.edu/teja-pristavec", "Teja Pristavec."))
        ),
        
        fluidRow(
          box(width = 12,
              title = "Acknowledgements",
              p("We would like to thank our colleagues for their input and contributions to this project.", align = "left"),
              
              #column(width = 4,
              #h5(strong("Cooperative Extension")),
              tags$a(tags$img(src = "VA_CES_logo.png", width = '35%'), href = "https://ext.vt.edu/"),
              br(), br(),
              tags$ul(
                tags$li("Daniel Goerlich, Ph.D., Associate Director: Economy, Community, and Food; Virginia Cooperative Extension, Virginia Tech"),
                tags$li("Ed Jones, Ph.D., Director, Virginia Cooperative Extension and Associate Dean, College of Agriculture and Life Sciences, Virginia Tech"),
                tags$li("Michael Lambur, Ph.D., Associate Director: Program Development, Virginia Cooperative Extension, Virginia Tech"),
                tags$li("Cathy Sutphin, Ph.D., Associate Director: Youth, Families, and Health; Virginia Cooperative Extension, Virginia Tech"),
                style = "list-style: none; margin-left: 0px; padding-left: 0px"
              ),
              br(),
              
              
              #),
              
              #column(width = 4,
              #h5(strong("Iowa State University")),
              tags$a(tags$img(src = "ISU_logo.png", width = '15%'), href = "https://www.iastate.edu/"),
              br(), br(),
              tags$ul(em("Faculty:"),
                      tags$li("Todd Abraham, Assistant Director of Data and Analytics for the Iowa Integrated Data System"),
                      tags$li("Cass Dorius, Associate Professor of Human Development and Family Studies"), 
                      tags$li("Shawn Dorius, Associate Professor of Sociology"),
                      style = "list-style: none; margin-left: 0px; padding-left: 0px"
              ),
              tags$ul(em("Graduate Fellows:"),
                      tags$li("Atefeh Rajabalizadah"),
                      tags$li("Haley Jeppson"),
                      tags$li("Kishor Sridhar"), 
                      style = "list-style: none; margin-left: 0px; padding-left: 0px"
              ),
              tags$ul(em("Undergraduate Interns:"),
                      tags$li("Jessie Bustin"),
                      tags$li("Grant Durbahn"), 
                      tags$li("Vikram Magal"),
                      tags$li("Katie Thompson"), 
                      tags$li("Joel Von Behren"),
                      tags$li("Matthew Voss"), 
                      style = "list-style: none; margin-left: 0px; padding-left: 0px"
              ),
              br(),
              
              #),
              
              #column(width = 4,
              #h5(strong("University of Virginia")),
              tags$a(tags$img(src = "BII_logo.png", width = '30%'), href = "https://biocomplexity.virginia.edu/"),
              br(), br(),
              tags$ul(em("Social and Decision Analytics Division:"),
                      tags$li("Sallie Keller, Division Director, Distinguished Professor in Biocomplexity, and Professor of Public Health Sciences, School of Medicine"),
                      tags$li("Brandon Kramer, Postdoctoral Research Associate"),
                      tags$li("Vicki Lancaster, Principal Scientist"),
                      tags$li("Kathryn Linehan, Research Scientist"),
                      tags$li("Cesar Montalvo, Postdoctoral Research Associate"),
                      tags$li("Teja Pristavec, Research Assistant Professor"),
                      tags$li("Stephanie Shipp, Deputy Division Director and Research Professor"),
                      style = "list-style: none; margin-left: 0px; padding-left: 0px"
              ),
              
              tags$ul(em("Graduate Fellows:"),
                      tags$li("Lara Haase"),
                      tags$li("Morgan Stockham"),
                      style = "list-style: none; margin-left: 0px; padding-left: 0px"
              ),
              tags$ul(em("Undergraduate Interns:"),
                      tags$li("Riya Berry"),
                      tags$li("Tasfia Chowdhury"),
                      tags$li("Martha Czernuszenko"),
                      tags$li("Saimun Habib"),
                      tags$li("Owen Hart"),
                      tags$li("Sarah McDonald"),
                      tags$li("Vatsala Ramanan"),
                      style = "list-style: none; margin-left: 0px; padding-left: 0px"
              )
              
              #)
          )
          
        )
        
)
                        
                        # CONTACT CONTENT -------------------------
                      )
                    )
)


########################################################################################
########################################################################################
# SERVER ----------------------------------------------------------------------------------------------------
########################################################################################
########################################################################################

server <- function(input, output, session) {
  # Plot colors --------------------------
  cbGreens <- c("#F7F7F7", "#D9F0D3", "#ACD39E", "#5AAE61", "#1B7837", "grey")
  cbGreens2 <- c("#4E5827", "#6E752A", "#959334", "#C3B144", "#F9F1CB", "#EB8E38", "#C96918")
  cbBrowns <- c("#FFF4A2", "#E9DC7A", "#D2C351", "#BCAB29", "#A59200", "grey")
  
  # Info button content ---------------------
  observeEvent(input$pcindex_info, {
    shinyalert(text = includeHTML("pcindex_info.html"), html = TRUE, type = "info", size = "l", animation = FALSE,
               closeOnEsc = TRUE, closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText = "Close")
  })
  
  #-----new
  
  observeEvent(input$representation_info2, {
    shinyalert(text = includeHTML("info_expl_representation.html"), html = TRUE, type = "info", size = "l", animation = FALSE,
               closeOnEsc = TRUE, closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText = "Close")
  })
  
  
  observeEvent(input$participation_info2, {
    shinyalert(text = includeHTML("info_expl_participation.html"), html = TRUE, type = "info", size = "l", animation = FALSE,
               closeOnEsc = TRUE, closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText = "Close")
  })
  
  
  observeEvent(input$contribution_info, {
    shinyalert(text = includeHTML("info_expl_contribution.html"), html = TRUE, type = "info", size = "l", animation = FALSE,
               closeOnEsc = TRUE, closeOnClickOutside = TRUE, showConfirmButton = TRUE, confirmButtonText = "Close")
  })
  
  
  
  
  ###---------------------------------###
  
  

  
  #establish the new function with switch 
  
  create_index <- function(data, myvar, myvarlabel) {
    
    cbGreens <- c("#F7F7F7", "#D9F0D3", "#ACD39E", "#5AAE61", "#1B7837")
    pal <- colorNumeric(cbGreens, domain = myvar)
    
    labels <- lapply(
      paste("<strong>Area: </strong>",
            data$name,
            "<br />",
            "<strong>", myvarlabel, ": </strong>",
            round(myvar, 2)),
      htmltools::HTML
    )
    
    leaflet(data = data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(fillColor = ~pal(myvar), 
                  fillOpacity = 0.7, 
                  stroke = TRUE, smoothFactor = 0.7, weight = 0.5, color = "#202020",
                  label = labels,
                  labelOptions = labelOptions(direction = "bottom",
                                              style = list(
                                                "font-size" = "12px",
                                                "border-color" = "rgba(0,0,0,0.5)",
                                                direction = "auto"
                                              ))) %>%
      addLegend("bottomleft",
                pal = pal,
                values =  ~(myvar),
                title = "Index Value [1-5]",
                opacity = 0.4
                # ,
                # na.label = "Not Available"
      )
  }
  
  
  #####
  

  
  # Switches
  fin_data <- reactive({datafin %>% filter(state == input$fin_whichstate)})
  #pol_data <- reactive({data_pol %>% filter(State ==  input$fin_whichstate )})
  
  #base ahora
  

  
  #Political Capital Index ---------------

  
  output$plot_political_index <- renderLeaflet({
    data <- data_pol
    data <- switch(input$fin_whichstate,
                       "Iowa" = data[data$STATEFP == "19", ],
                       "Oregon" = data[data$STATEFP == "41", ],
                       "Virginia" = data[data$STATEFP == "51", ])
    #create_index( pol_data(), na.omit(pol_data()$quant) , "Political Capital Index")
    #create_index( data_pol %>% filter(state=="OR") , na.omit(data_pol$quant) , "Political index"      )
    create_index( data , na.omit(data$quant) , "Political index")
  })
  

  
 #------- Political capital----------------------------------
  
######### MAIN NEW PLOT

  
  
  output$mainplot2 <- renderLeaflet({
    
      data <- data_pol
      data$quantile <- (data$votepartQuint + data$assn2014Quint  + data$num1000Quint)/3
      data$quintileQuint <- ntile(data$quantile, 5)
      
      data <- switch(input$fin_whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorQuantile("Greens", domain = data$quantile, probs = seq(0, 1, length = 6), right = TRUE)

      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% Political capital index:</strong>",
              round(data$quantile, 3), 
              "<br />",
              "<strong>Quintile:</strong>",
              data$quintileQuint),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$quantile), 
                    fillOpacity = 0.8,
                    stroke = TRUE,
                    weight = 0.9,
                    color = "gray",
                    smoothFactor = 0.7,
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft", pal = pal, values = ~data$quantile,
                  title = "Index<br>(Quintile Group)", opacity = 1,
                  na.label = "Not Available",
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
  
  })
  
  
  
  #############################################END MAIN NEW PLOT
  
  

  ###----- CONTRIBUTIONS
  #plotly
  
  output$plotly_contrib <- renderPlotly({
    
    data <- data_pol
    # Recode rurality  -----------------------------------------------------------------------
    #
    data <- data %>% mutate(irr2010_discretize = case_when(IRR2010 < 0.15 ~ "Most Urban [0.12, 0.15)",
                                                           IRR2010 >= 0.15 & IRR2010 < 0.25 ~ "More Urban [0.15, 0.25)",
                                                           IRR2010 >= 0.25 & IRR2010 < 0.35 ~ "Urban [0.25, 0.35)",
                                                           IRR2010 >= 0.35 & IRR2010 < 0.45 ~ "In-Between [0.35, 0.45)",
                                                           IRR2010 >= 0.45 & IRR2010 < 0.55 ~ "Rural [0.45, 0.55)",
                                                           IRR2010 >= 0.55 & IRR2010 < 0.65 ~ "More Rural [0.55, 0.65)",
                                                           IRR2010 >= 0.65 ~ "Most Rural [0.65, 0.68]"
    ))
    
    data$irr2010_discretize <- factor(data$irr2010_discretize,
                                      levels = c("Most Urban [0.12, 0.15)", "More Urban [0.15, 0.25)", "Urban [0.25, 0.35)",
                                                 "In-Between [0.35, 0.45)", "Rural [0.45, 0.55)", "More Rural [0.55, 0.65)",
                                                 "Most Rural [0.65, 0.68]"))
    
    #data$irr2010_discretize <- as.factor(data$irr2010_discretize)
    
    # 
    #   ----------------------------------------------------------------------------------------
    
    data <- switch(input$fin_whichstate,
                   "Iowa" = data[data$STATEFP == "19", ],
                   "Oregon" = data[data$STATEFP == "41", ],
                   "Virginia" = data[data$STATEFP == "51", ])
    
    cbGreens2 <-c("#4E5827", "#6E752A", "#959334", "#C3B144", "#F9F1CB", "#EB8E38", "#C96918")
    
    group <- as.factor(data$state)
    
    
    data %>%
      plot_ly(colors = cbGreens2) %>%  
      add_trace(x = as.numeric(group),
                type = "box",
                fillcolor = "#BCBBBC",
                line = list(color = "#787878"),
                y = ~num1000,
                showlegend = F,
                marker = list(symbol = "asterisk", color = ~irr2010_discretize),
                hoverinfo = "y",
                name = "") %>%
      add_markers(x = ~jitter(as.numeric(group) , amount = 0.1), 
                  y = ~num1000, 
                  color = ~irr2010_discretize,
                  marker = list(size = 6, line = list(width = 1, color = "#3C3C3C")),
                  hoverinfo = "text",
                  text = ~paste0("Rurality Index: ", round(IRR2010,2),
                                 "<br>County: ", county),
                  showlegend = TRUE 
      ) %>%
      layout(title = "",
             legend = list(title = list(text = "<b>Index of Relative\nRurality</b>")),
             xaxis = list(title = "Contributors per 1000 people ",
                          zeroline = FALSE,
                          showticklabels = FALSE),
             yaxis = list(title = "",
                          zeroline = FALSE,
                          hoverformat = ".2f"))      
  })
  
#--------------------------------------------------------------------------------
  
#leaflet
  
  output$leaflet_contrib <- renderLeaflet({
      
      data <- data_pol
      data <- switch(input$fin_whichstate,
                     "Iowa" = data[data$STATEFP == "19", ],
                     "Oregon" = data[data$STATEFP == "41", ],
                     "Virginia" = data[data$STATEFP == "51", ])
      
      pal <- colorQuantile("Greens", domain = data$num1000, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>County: </strong>",
              data$name,
              "<br />",
              "<strong>% Population enrolled in K-12:</strong>",
              round(data$num1000, 2), 
              "<br />",
              "<strong>Quintile:</strong>",
              data$num1000Quint),
        htmltools::HTML
      )
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(fillColor = ~pal(data$num1000), 
                    fillOpacity = 0.8,
                    stroke = TRUE,
                    weight = 0.9,
                    color = "gray",
                    smoothFactor = 0.7,
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft", pal = pal, values = ~data$num1000,
                  title = "Percent<br>(Quintile Group)", opacity = 1,
                  na.label = "Not Available",
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })

  })
  
  #--------------------
  
  ###----- ORGANIZATIONS
  #plotly
  
  output$plotly_organization <- renderPlotly({
    
    data <- data_pol
    # Recode rurality  -----------------------------------------------------------------------
    #
    data <- data %>% mutate(irr2010_discretize = case_when(IRR2010 < 0.15 ~ "Most Urban [0.12, 0.15)",
                                                           IRR2010 >= 0.15 & IRR2010 < 0.25 ~ "More Urban [0.15, 0.25)",
                                                           IRR2010 >= 0.25 & IRR2010 < 0.35 ~ "Urban [0.25, 0.35)",
                                                           IRR2010 >= 0.35 & IRR2010 < 0.45 ~ "In-Between [0.35, 0.45)",
                                                           IRR2010 >= 0.45 & IRR2010 < 0.55 ~ "Rural [0.45, 0.55)",
                                                           IRR2010 >= 0.55 & IRR2010 < 0.65 ~ "More Rural [0.55, 0.65)",
                                                           IRR2010 >= 0.65 ~ "Most Rural [0.65, 0.68]"
    ))
    data$irr2010_discretize <- factor(data$irr2010_discretize,
                                      levels = c("Most Urban [0.12, 0.15)", "More Urban [0.15, 0.25)", "Urban [0.25, 0.35)",
                                                 "In-Between [0.35, 0.45)", "Rural [0.45, 0.55)", "More Rural [0.55, 0.65)",
                                                 "Most Rural [0.65, 0.68]"))

    #   ----------------------------------------------------------------------------------------
    
    data <- switch(input$fin_whichstate,
                   "Iowa" = data[data$STATEFP == "19", ],
                   "Oregon" = data[data$STATEFP == "41", ],
                   "Virginia" = data[data$STATEFP == "51", ])
    
    cbGreens2 <- c("#4E5827", "#6E752A", "#959334", "#C3B144", "#F9F1CB", "#EB8E38", "#C96918")
    
    group <- as.factor(data$state)
    
    
    data %>%
      plot_ly(colors = cbGreens2) %>%  
      add_trace(x = as.numeric(group),
                type = "box",
                fillcolor = "#BCBBBC",
                line = list(color = "#787878"),
                y = ~assn2014,
                showlegend = F,
                marker = list(symbol = "asterisk", color = ~irr2010_discretize),
                hoverinfo = "y",
                name = "") %>%
      add_markers(x = ~jitter(as.numeric(group) , amount = 0.1), 
                  y = ~assn2014, 
                  color = ~irr2010_discretize,
                  marker = list(size = 6, line = list(width = 1, color = "#3C3C3C")),
                  hoverinfo = "text",
                  text = ~paste0("Rurality Index: ", round(IRR2010,2),
                                 "<br>County: ", county),
                  showlegend = TRUE 
      ) %>%
      layout(title = "",
             legend = list(title = list(text = "<b>Index of Relative\nRurality</b>")),
             xaxis = list(title = "Organizations per 1000 people ",
                          zeroline = FALSE,
                          showticklabels = FALSE),
             yaxis = list(title = "",
                          zeroline = FALSE,
                          hoverformat = ".2f"))      
  })
  
  #--------------------------------------------------------------------------------
  
  #leaflet
  
  output$leaflet_organization <- renderLeaflet({
    
    data <- data_pol
    data <- switch(input$fin_whichstate,
                   "Iowa" = data[data$STATEFP == "19", ],
                   "Oregon" = data[data$STATEFP == "41", ],
                   "Virginia" = data[data$STATEFP == "51", ])
    
    pal <- colorQuantile("Greens", domain = data$assn2014, probs = seq(0, 1, length = 6), right = TRUE)
    
    labels <- lapply(
      paste("<strong>County: </strong>",
            data$name,
            "<br />",
            "<strong>% Organizations per 1000:</strong>",
            round(data$assn2014, 2), 
            "<br />",
            "<strong>Quintile:</strong>",
            data$assn2014Quint),
      htmltools::HTML
    )
    
    leaflet(data) %>%
      addTiles() %>%
      addPolygons(fillColor = ~pal(data$assn2014), 
                  fillOpacity = 0.8,
                  stroke = TRUE,
                  weight = 0.9,
                  color = "gray",
                  smoothFactor = 0.7,
                  label = labels,
                  labelOptions = labelOptions(direction = "bottom",
                                              style = list(
                                                "font-size" = "12px",
                                                "border-color" = "rgba(0,0,0,0.5)",
                                                direction = "auto"
                                              ))) %>%
      addLegend("bottomleft", pal = pal, values = ~data$assn2014,
                title = "Percent<br>(Quintile Group)", opacity = 1,
                na.label = "Not Available",
                labFormat = function(type, cuts, p) {
                  n = length(cuts)
                  paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                })
    
  })
  
  
  
  ###----- REPRESENTATION voters
  #plotly
  
  output$plotly_voters <- renderPlotly({
    
    data <- data_pol
    # Recode rurality  -----------------------------------------------------------------------
    #
    data <- data %>% mutate(irr2010_discretize = case_when(IRR2010 < 0.15 ~ "Most Urban [0.12, 0.15)",
                                                           IRR2010 >= 0.15 & IRR2010 < 0.25 ~ "More Urban [0.15, 0.25)",
                                                           IRR2010 >= 0.25 & IRR2010 < 0.35 ~ "Urban [0.25, 0.35)",
                                                           IRR2010 >= 0.35 & IRR2010 < 0.45 ~ "In-Between [0.35, 0.45)",
                                                           IRR2010 >= 0.45 & IRR2010 < 0.55 ~ "Rural [0.45, 0.55)",
                                                           IRR2010 >= 0.55 & IRR2010 < 0.65 ~ "More Rural [0.55, 0.65)",
                                                           IRR2010 >= 0.65 ~ "Most Rural [0.65, 0.68]"
    ))
    data$irr2010_discretize <- factor(data$irr2010_discretize,
                                      levels = c("Most Urban [0.12, 0.15)", "More Urban [0.15, 0.25)", "Urban [0.25, 0.35)",
                                                 "In-Between [0.35, 0.45)", "Rural [0.45, 0.55)", "More Rural [0.55, 0.65)",
                                                 "Most Rural [0.65, 0.68]"))
    # 
    #   ----------------------------------------------------------------------------------------
    
    data <- switch(input$fin_whichstate,
                   "Iowa" = data[data$STATEFP == "19", ],
                   "Oregon" = data[data$STATEFP == "41", ],
                   "Virginia" = data[data$STATEFP == "51", ])
    
    cbGreens2 <- c("#4E5827", "#6E752A", "#959334", "#C3B144", "#F9F1CB", "#EB8E38", "#C96918")
    
    group <- as.factor(data$state)
    
    
    data %>%
      plot_ly(colors = cbGreens2) %>%  
      add_trace(x = as.numeric(group),
                type = "box",
                fillcolor = "#BCBBBC",
                line = list(color = "#787878"),
                y = ~votepart,
                showlegend = F,
                marker = list(symbol = "asterisk", color = ~irr2010_discretize),
                hoverinfo = "y",
                name = "") %>%
      add_markers(x = ~jitter(as.numeric(group) , amount = 0.1), 
                  y = ~votepart, 
                  color = ~irr2010_discretize,
                  marker = list(size = 6, line = list(width = 1, color = "#3C3C3C")),
                  hoverinfo = "text",
                  text = ~paste0("Rurality Index: ", round(IRR2010,2),
                                 "<br>County: ", county),
                  showlegend = TRUE 
      ) %>%
      layout(title = "",
             legend = list(title = list(text = "<b>Index of Relative\nRurality</b>")),
             xaxis = list(title = "Voters/Voting-age Population",
                          zeroline = FALSE,
                          showticklabels = FALSE),
             yaxis = list(title = "",
                          zeroline = FALSE,
                          hoverformat = ".2f"))      
  })
  
  #--------------------------------------------------------------------------------
  
  #leaflet
  
  output$leaflet_voters <- renderLeaflet({
    
    data <- data_pol
    data <- switch(input$fin_whichstate,
                   "Iowa" = data[data$STATEFP == "19", ],
                   "Oregon" = data[data$STATEFP == "41", ],
                   "Virginia" = data[data$STATEFP == "51", ])
    
    pal <- colorQuantile("Greens", domain = data$votepart, probs = seq(0, 1, length = 6), right = TRUE)
    
    labels <- lapply(
      paste("<strong>County: </strong>",
            data$name,
            "<br />",
            "<strong>% Organizations per 1000:</strong>",
            round(data$votepart, 2), 
            "<br />",
            "<strong>Quintile:</strong>",
            data$votepartQuint),
      htmltools::HTML
    )
    
    leaflet(data) %>%
      addTiles() %>%
      addPolygons(fillColor = ~pal(data$votepart), 
                  fillOpacity = 0.8,
                  stroke = TRUE,
                  weight = 0.9,
                  color = "gray",
                  smoothFactor = 0.7,
                  label = labels,
                  labelOptions = labelOptions(direction = "bottom",
                                              style = list(
                                                "font-size" = "12px",
                                                "border-color" = "rgba(0,0,0,0.5)",
                                                direction = "auto"
                                              ))) %>%
      addLegend("bottomleft", pal = pal, values = ~data$votepart,
                title = "Percent<br>(Quintile Group)", opacity = 1,
                na.label = "Not Available",
                labFormat = function(type, cuts, p) {
                  n = length(cuts)
                  paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                })
    
  }) 
  
  #---------
  
  
  ############## 3D plot for the index
  #library(plot3D)
  
  # 
  # output$plotly_index3d <- renderPlotly({
  #   colors <- c('#9FBE7A', '#60999A', '#31596E')
  #   index3d <- plot_ly(data_pol, x = ~votepart, y = ~assn2014, z = ~num1000,  color = ~state, text= ~name, width= 1000, 
  #                      height = 800, colors=colors )
  #   index3d <- index3d %>% add_markers()
  #   index3d <- index3d  %>% layout(scene = list(xaxis = list(title = 'Voters/voting-age population' ) ,
  #                                               yaxis = list(title = 'Number of organizations <br>per 1000 people'),
  #                                               zaxis = list(title = 'Contributors <br>per 1000 people'), align='center')
  #   )
  #   
  #   index3d
  # })
  #####################################
  
  #----- Circular asset maps
  
  # --------- circ LAW ENFORCEMENT
  output$political_dom_law <- renderPlot({
    #data <- read_csv("/home/cpm9w/EM/gates/src/dashboard/polcap4/datan.csv")
    data <- read_csv("datan.csv") 
    data <- data %>% filter(dom=="LawEnforcement")
    
    
    # Set a number of 'empty bar' to add at the end of each group
    empty_bar <- 3
    to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
    colnames(to_add) <- colnames(data)
    to_add$group <- rep(levels(data$group), each=empty_bar)
    data <- rbind(data, to_add)
    data <- data %>% arrange(group)
    data$id <- seq(1, nrow(data))
    
    # Get the name and the y position of each label
    label_data <- data
    number_of_bar <- nrow(label_data)
    angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
    label_data$hjust <- ifelse( angle < -90, 1, 0)
    label_data$angle <- ifelse(angle < -90, angle+180, angle)
    
    # prepare a data frame for base lines
    base_data <- data %>% 
      group_by(group) %>% 
      summarize(start=min(id), end=max(id) ) %>% 
      rowwise() %>% 
      mutate(title=mean(c(start, end)))
    
    # prepare a data frame for grid (scales)
    grid_data <- base_data
    grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
    grid_data$start <- grid_data$start - 1
    grid_data <- grid_data[-1,]
    
    ##conditional for format of justification of name
    if (length(unique(data$group))==4) {
      arr<- c(1,1,0,0)
    } else if (length(unique(data$group))==3) {
      arr<-c(1,0.3,0)
    } else if (length(unique(data$group))==2) {
      arr<-c(1,0)
    }
    
    # Make the plot
    ggplot(data, aes(x=as.factor(id), y=value, fill=group), width=800, height = 800) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
      
      #add grid lines
      geom_hline(yintercept=0, color = "gray", size=0.2, alpha=1)+
      geom_hline(yintercept=0.20, color = "gray", size=0.2, alpha=1)+
      geom_hline(yintercept=0.40, color = "gray", size=0.2, alpha=1)+
      geom_hline(yintercept=0.60, color = "gray", size=0.2, alpha=1)+
      geom_hline(yintercept=0.80, color = "gray", size=0.2, alpha=1)+
      geom_hline(yintercept=1, color = "gray", size=0.2, alpha=1)+
      
      geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
      scale_fill_manual(values=c("#9FBE7A","#60999A","#31596E","#F9F1CB")) +
      
      # Add text showing the value of each 100/75/50/25 lines
      annotate("text", x = rep(0, 6), y = c(0.0, 0.20, 0.40, 0.60, 0.80, 1.00), label = c("0.0","0.20", "0.40", "0.60", "0.80", "1.00") , color="black", size=3 , angle=0, fontface="bold", hjust=0) +
      
      geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
      ylim(-1.0, 1.2) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4), "cm") 
      ) +
      coord_polar() + 
      geom_text(data=label_data, aes(x=id, y=value+0.10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=4, angle= label_data$angle, inherit.aes = FALSE ) +
      
      
      # Add base line information
      geom_segment(data=base_data, aes(x = start, y = -0.1, xend = end, yend = -0.1), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
      geom_text(data=base_data, aes(x = title, y = -0.2, label=group), hjust=c(arr), colour = "black", alpha=1, size=4, fontface="bold", inherit.aes = FALSE)
    
    
  })
  
  
  
  # --------- circ EDUCATION
  output$political_dom_edu <- renderPlot({
    #data <- read_csv("/home/cpm9w/EM/gates/src/dashboard/polcap4/datan.csv")
    data <- read_csv("datan.csv") 
    data <- data %>% filter(dom=="Education")
    
    
    # Set a number of 'empty bar' to add at the end of each group
    empty_bar <- 3
    to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
    colnames(to_add) <- colnames(data)
    to_add$group <- rep(levels(data$group), each=empty_bar)
    data <- rbind(data, to_add)
    data <- data %>% arrange(group)
    data$id <- seq(1, nrow(data))
    
    # Get the name and the y position of each label
    label_data <- data
    number_of_bar <- nrow(label_data)
    angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
    label_data$hjust <- ifelse( angle < -90, 1, 0)
    label_data$angle <- ifelse(angle < -90, angle+180, angle)
    
    # prepare a data frame for base lines
    base_data <- data %>% 
      group_by(group) %>% 
      summarize(start=min(id), end=max(id) ) %>% 
      rowwise() %>% 
      mutate(title=mean(c(start, end)))
    
    # prepare a data frame for grid (scales)
    grid_data <- base_data
    grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
    grid_data$start <- grid_data$start - 1
    grid_data <- grid_data[-1,]
    
    ##conditional for format of justification of name
    if (length(unique(data$group))==4) {
      arr<- c(1,1,0,0)
    } else if (length(unique(data$group))==3) {
      arr<-c(1,0.3,0)
    } else if (length(unique(data$group))==2) {
      arr<-c(1,0)
    }
    
    # Make the plot
    ggplot(data, aes(x=as.factor(id), y=value, fill=group), width=800, height = 800) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
      
      #add grid lines
      geom_hline(yintercept=0, color = "gray", size=0.2, alpha=1)+
      geom_hline(yintercept=0.20, color = "gray", size=0.2, alpha=1)+
      geom_hline(yintercept=0.40, color = "gray", size=0.2, alpha=1)+
      geom_hline(yintercept=0.60, color = "gray", size=0.2, alpha=1)+
      geom_hline(yintercept=0.80, color = "gray", size=0.2, alpha=1)+
      geom_hline(yintercept=1, color = "gray", size=0.2, alpha=1)+
      
      geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
      scale_fill_manual(values=c("#9FBE7A","#60999A","#31596E","#F9F1CB")) +
      
      # Add text showing the value of each 100/75/50/25 lines
      annotate("text", x = rep(0, 6), y = c(0.0, 0.20, 0.40, 0.60, 0.80, 1.00), label = c("0.0","0.20", "0.40", "0.60", "0.80", "1.00") , color="black", size=3 , angle=0, fontface="bold", hjust=0) +
      
      geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
      ylim(-1.0, 1.2) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4), "cm") 
      ) +
      coord_polar() + 
      geom_text(data=label_data, aes(x=id, y=value+0.10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=4, angle= label_data$angle, inherit.aes = FALSE ) +
      
      
      # Add base line information
      geom_segment(data=base_data, aes(x = start, y = -0.1, xend = end, yend = -0.1), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
      geom_text(data=base_data, aes(x = title, y = -0.15, label=group), hjust=c(arr), colour = "black", alpha=1, size=4, fontface="bold", inherit.aes = FALSE)
    
    
  })  
  
  
  # --------- circ TAXATION
  output$political_dom_tax <- renderPlot({
    #data <- read_csv("/home/cpm9w/EM/gates/src/dashboard/polcap4/datan.csv")
    data <- read_csv("datan.csv") 
    data <- data %>% filter(dom=="Taxation")
    
    
    # Set a number of 'empty bar' to add at the end of each group
    empty_bar <- 3
    to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
    colnames(to_add) <- colnames(data)
    to_add$group <- rep(levels(data$group), each=empty_bar)
    data <- rbind(data, to_add)
    data <- data %>% arrange(group)
    data$id <- seq(1, nrow(data))
    
    # Get the name and the y position of each label
    label_data <- data
    number_of_bar <- nrow(label_data)
    angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
    label_data$hjust <- ifelse( angle < -90, 1, 0)
    label_data$angle <- ifelse(angle < -90, angle+180, angle)
    
    # prepare a data frame for base lines
    base_data <- data %>% 
      group_by(group) %>% 
      summarize(start=min(id), end=max(id) ) %>% 
      rowwise() %>% 
      mutate(title=mean(c(start, end)))
    
    # prepare a data frame for grid (scales)
    grid_data <- base_data
    grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
    grid_data$start <- grid_data$start - 1
    grid_data <- grid_data[-1,]
    
    ##conditional for format of justification of name
    if (length(unique(data$group))==4) {
      arr<- c(1,1,0,0)
    } else if (length(unique(data$group))==3) {
      arr<-c(1,0.3,0)
    } else if (length(unique(data$group))==2) {
      arr<-c(1,0)
    }
    
    # Make the plot
    ggplot(data, aes(x=as.factor(id), y=value, fill=group), width=200, height = 200) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
      
      #add grid lines
      geom_hline(yintercept=0, color = "gray", size=0.2, alpha=1)+
      geom_hline(yintercept=0.20, color = "gray", size=0.2, alpha=1)+
      geom_hline(yintercept=0.40, color = "gray", size=0.2, alpha=1)+
      geom_hline(yintercept=0.60, color = "gray", size=0.2, alpha=1)+
      geom_hline(yintercept=0.80, color = "gray", size=0.2, alpha=1)+
      geom_hline(yintercept=1, color = "gray", size=0.2, alpha=1)+
      
      geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
      scale_fill_manual(values=c("#9FBE7A","#60999A","#31596E","#F9F1CB")) +
      
      # Add text showing the value of each 100/75/50/25 lines
      annotate("text", x = rep(0, 6), y = c(0.0, 0.20, 0.40, 0.60, 0.80, 1.00), label = c("0.0","0.20", "0.40", "0.60", "0.80", "1.00") , color="black", size=3 , angle=0, fontface="bold", hjust=0) +
      
      geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
      ylim(-1.0, 1.2) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4), "cm") 
      ) +
      coord_polar() + 
      geom_text(data=label_data, aes(x=id, y=value+0.10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=4, angle= label_data$angle, inherit.aes = FALSE ) +
      
      
      # Add base line information
      geom_segment(data=base_data, aes(x = start, y = -0.1, xend = end, yend = -0.1), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
      geom_text(data=base_data, aes(x = title, y = -0.2, label=group), hjust=c(arr), colour = "black", alpha=1, size=4, fontface="bold", inherit.aes = FALSE)
    
    
  })  
  
  
  # --------- circ HOUSING
  output$political_dom_hou <- renderPlot({
    #data <- read_csv("/home/cpm9w/EM/gates/src/dashboard/polcap4/datan.csv")
    data <- read_csv("datan.csv") 
    data <- data %>% filter(dom=="Housing")
    
    
    # Set a number of 'empty bar' to add at the end of each group
    empty_bar <- 3
    to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
    colnames(to_add) <- colnames(data)
    to_add$group <- rep(levels(data$group), each=empty_bar)
    data <- rbind(data, to_add)
    data <- data %>% arrange(group)
    data$id <- seq(1, nrow(data))
    
    # Get the name and the y position of each label
    label_data <- data
    number_of_bar <- nrow(label_data)
    angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
    label_data$hjust <- ifelse( angle < -90, 1, 0)
    label_data$angle <- ifelse(angle < -90, angle+180, angle)
    
    # prepare a data frame for base lines
    base_data <- data %>% 
      group_by(group) %>% 
      summarize(start=min(id), end=max(id) ) %>% 
      rowwise() %>% 
      mutate(title=mean(c(start, end)))
    
    # prepare a data frame for grid (scales)
    grid_data <- base_data
    grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
    grid_data$start <- grid_data$start - 1
    grid_data <- grid_data[-1,]
    
    ##conditional for format of justification of name
    if (length(unique(data$group))==4) {
      arr<- c(1,1,0,0)
    } else if (length(unique(data$group))==3) {
      arr<-c(1,0.3,0)
    } else if (length(unique(data$group))==2) {
      arr<-c(1,0)
    }
    
    # Make the plot
    ggplot(data, aes(x=as.factor(id), y=value, fill=group), width=800, height = 800) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
      
      #add grid lines
      geom_hline(yintercept=0, color = "gray", size=0.2, alpha=1)+
      geom_hline(yintercept=0.20, color = "gray", size=0.2, alpha=1)+
      geom_hline(yintercept=0.40, color = "gray", size=0.2, alpha=1)+
      geom_hline(yintercept=0.60, color = "gray", size=0.2, alpha=1)+
      geom_hline(yintercept=0.80, color = "gray", size=0.2, alpha=1)+
      geom_hline(yintercept=1, color = "gray", size=0.2, alpha=1)+
      
      geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
      scale_fill_manual(values=c("#9FBE7A","#60999A","#31596E","#F9F1CB")) +
      
      # Add text showing the value of each 100/75/50/25 lines
      annotate("text", x = rep(0, 6), y = c(0.0, 0.20, 0.40, 0.60, 0.80, 1.00), label = c("0.0","0.20", "0.40", "0.60", "0.80", "1.00") , color="black", size=3 , angle=0, fontface="bold", hjust=0) +
      
      geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
      ylim(-1.0, 1.2) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4), "cm") 
      ) +
      coord_polar() + 
      geom_text(data=label_data, aes(x=id, y=value+0.10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=4, angle= label_data$angle, inherit.aes = FALSE ) +
      
      
      # Add base line information
      geom_segment(data=base_data, aes(x = start, y = -0.1, xend = end, yend = -0.1), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
      geom_text(data=base_data, aes(x = title, y = -0.2, label=group), hjust=c(arr), colour = "black", alpha=1, size=4, fontface="bold", inherit.aes = FALSE)
    
    
  })  
  
  
  # --------- circ EMPLOYMENT
  output$political_dom_emp <- renderPlot({
    #data <- read_csv("/home/cpm9w/EM/gates/src/dashboard/polcap4/datan.csv")
    data <- read_csv("datan.csv") 
    data <- data %>% filter(dom=="Employment")
    
    
    # Set a number of 'empty bar' to add at the end of each group
    empty_bar <- 3
    to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
    colnames(to_add) <- colnames(data)
    to_add$group <- rep(levels(data$group), each=empty_bar)
    data <- rbind(data, to_add)
    data <- data %>% arrange(group)
    data$id <- seq(1, nrow(data))
    
    # Get the name and the y position of each label
    label_data <- data
    number_of_bar <- nrow(label_data)
    angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
    label_data$hjust <- ifelse( angle < -90, 1, 0)
    label_data$angle <- ifelse(angle < -90, angle+180, angle)
    
    # prepare a data frame for base lines
    base_data <- data %>% 
      group_by(group) %>% 
      summarize(start=min(id), end=max(id) ) %>% 
      rowwise() %>% 
      mutate(title=mean(c(start, end)))
    
    # prepare a data frame for grid (scales)
    grid_data <- base_data
    grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
    grid_data$start <- grid_data$start - 1
    grid_data <- grid_data[-1,]
    
    ##conditional for format of justification of name
    if (length(unique(data$group))==4) {
      arr<- c(1,1,0,0)
    } else if (length(unique(data$group))==3) {
      arr<-c(1,0.3,0)
    } else if (length(unique(data$group))==2) {
      arr<-c(1,0)
    }
    
    # Make the plot
    ggplot(data, aes(x=as.factor(id), y=value, fill=group), width=800, height = 800) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
      
      #add grid lines
      geom_hline(yintercept=0, color = "gray", size=0.2, alpha=1)+
      geom_hline(yintercept=0.20, color = "gray", size=0.2, alpha=1)+
      geom_hline(yintercept=0.40, color = "gray", size=0.2, alpha=1)+
      geom_hline(yintercept=0.60, color = "gray", size=0.2, alpha=1)+
      geom_hline(yintercept=0.80, color = "gray", size=0.2, alpha=1)+
      geom_hline(yintercept=1, color = "gray", size=0.2, alpha=1)+
      
      geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
      scale_fill_manual(values=c("#9FBE7A","#60999A","#31596E","#F9F1CB")) +
      
      # Add text showing the value of each 100/75/50/25 lines
      annotate("text", x = rep(0, 6), y = c(0.0, 0.20, 0.40, 0.60, 0.80, 1.00), label = c("0.0","0.20", "0.40", "0.60", "0.80", "1.00") , color="black", size=3 , angle=0, fontface="bold", hjust=0) +
      
      geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
      ylim(-1.0, 1.2) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4), "cm") 
      ) +
      coord_polar() + 
      geom_text(data=label_data, aes(x=id, y=value+0.10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=4, angle= label_data$angle, inherit.aes = FALSE ) +
      
      
      # Add base line information
      geom_segment(data=base_data, aes(x = start, y = -0.1, xend = end, yend = -0.1), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
      geom_text(data=base_data, aes(x = title, y = -0.2, label=group), hjust=c(arr), colour = "black", alpha=1, size=4, fontface="bold", inherit.aes = FALSE)
    
    
  })  
  
  
  # --------- circ VOTING
  output$political_dom_vot <- renderPlot({
    #data <- read_csv("/home/cpm9w/EM/gates/src/dashboard/polcap4/datan.csv")
    data <- read_csv("datan.csv") 
    data <- data %>% filter(dom=="Voting")
    
    
    # Set a number of 'empty bar' to add at the end of each group
    empty_bar <- 3
    to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
    colnames(to_add) <- colnames(data)
    to_add$group <- rep(levels(data$group), each=empty_bar)
    data <- rbind(data, to_add)
    data <- data %>% arrange(group)
    data$id <- seq(1, nrow(data))
    
    # Get the name and the y position of each label
    label_data <- data
    number_of_bar <- nrow(label_data)
    angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
    label_data$hjust <- ifelse( angle < -90, 1, 0)
    label_data$angle <- ifelse(angle < -90, angle+180, angle)
    
    # prepare a data frame for base lines
    base_data <- data %>% 
      group_by(group) %>% 
      summarize(start=min(id), end=max(id) ) %>% 
      rowwise() %>% 
      mutate(title=mean(c(start, end)))
    
    # prepare a data frame for grid (scales)
    grid_data <- base_data
    grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
    grid_data$start <- grid_data$start - 1
    grid_data <- grid_data[-1,]
    
    ##conditional for format of justification of name
    if (length(unique(data$group))==4) {
      arr<- c(1,1,0,0)
    } else if (length(unique(data$group))==3) {
      arr<-c(1,0.3,0)
    } else if (length(unique(data$group))==2) {
      arr<-c(1,0)
    }
    
    # Make the plot
    ggplot(data, aes(x=as.factor(id), y=value, fill=group), width=800, height = 800) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
      
      #add grid lines
      geom_hline(yintercept=0, color = "gray", size=0.2, alpha=1)+
      geom_hline(yintercept=0.20, color = "gray", size=0.2, alpha=1)+
      geom_hline(yintercept=0.40, color = "gray", size=0.2, alpha=1)+
      geom_hline(yintercept=0.60, color = "gray", size=0.2, alpha=1)+
      geom_hline(yintercept=0.80, color = "gray", size=0.2, alpha=1)+
      geom_hline(yintercept=1, color = "gray", size=0.2, alpha=1)+
      
      geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
      scale_fill_manual(values=c("#9FBE7A","#60999A","#31596E","#F9F1CB")) +
      
      # Add text showing the value of each 100/75/50/25 lines
      annotate("text", x = rep(0, 6), y = c(0.0, 0.20, 0.40, 0.60, 0.80, 1.00), label = c("0.0","0.20", "0.40", "0.60", "0.80", "1.00") , color="black", size=3 , angle=0, fontface="bold", hjust=0) +
      
      geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
      ylim(-1.0, 1.2) +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,4), "cm") 
      ) +
      coord_polar() + 
      geom_text(data=label_data, aes(x=id, y=value+0.10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=4, angle= label_data$angle, inherit.aes = FALSE ) +
      
      
      # Add base line information
      geom_segment(data=base_data, aes(x = start, y = -0.1, xend = end, yend = -0.1), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
      geom_text(data=base_data, aes(x = title, y = -0.2, label=group), hjust=c(arr), colour = "black", alpha=1, size=4, fontface="bold", inherit.aes = FALSE)
    
    
  })  
  
 
  #-------------
 
  #---Data Tables
  
  #dataTable data
  #data
  # em_data <- read_csv('Composite Scorecard - Sheet2.csv')
  # 
  # #full data, not composites
  # all_data <- read_excel('em_master_data_final_final.xlsx')
  # 
  # 
  # emp_dt_data<- all_data %>%
  #   filter(domain == 'Employment')
  # 
  # vote_dt_data<- all_data %>%
  #   filter(domain == 'Voting')
  # 
  # edu_dt_data<- all_data %>%
  #   filter(domain == 'Education')
  # 
  # house_dt_data<- all_data %>%
  #   filter(domain == 'Housing')
  # 
  # law_dt_data<- all_data %>%
  #   filter(domain == 'Law Enforcement')
  # 
  # tax_dt_data<- all_data %>%
  #   filter(domain == 'Taxation')
  
  #Education question
  #question_edu_dt_data<- edu_3states  
  ###  
  # output$all_data_table <- DT::renderDataTable({
  #   all_data
  # })
  # 
  # output$lawtable = DT::renderDataTable({
  #   law_dt_data
  #   
  # })
  # output$taxtable = DT::renderDataTable({
  #   tax_dt_data
  #   
  # })
  # output$housetable = DT::renderDataTable({
  #   house_dt_data
  #   
  # })
  # output$edutable = DT::renderDataTable({
  #   edu_dt_data
  #   
  # })
  # output$votetable = DT::renderDataTable({
  #   vote_dt_data
  #   
  # })
  # output$emptable = DT::renderDataTable({
  #   emp_dt_data
  # })
  # output$qedutable = DT::renderDataTable({
  #   question_edu_dt_data
  # })
  
  
  #-------
  
  
  ##table of information 
  
  polcap_table <- read_csv('data_polcap_table.csv')
  output$table_polcap = DT::renderDataTable({polcap_table})
  
  
  
  #--------- Measures table -------------------------
  #
  measures_topic <- reactive({
    input$topic
  })
  
  output$measures_table <- renderDataTable({
    if(measures_topic() == "All"){
      table <- as.data.frame(measures)
      names(table) <- c("Capital", "Index", "Measure", "Data Source", "Year", "Geography")
      datatable(table, rownames = FALSE, options = list(pageLength = 15)) 
    }
    else{
      data <- switch(input$topic,
                     "Financial" = "financial",
                     "Human" = "human",
                     "Social" = "social",
                     "Natural" = "natural", 
                     "Built" = "built",
                     "Political" = "political", 
                     "Cultural" = "cultural")
      
      table <- measures[measures$capital == data, ]
      table <- as.data.frame(table)
      datatable(table, rownames = FALSE, options = list(pageLength = 15)) 
    }
  })
  
  

 }

shinyApp(ui, server)