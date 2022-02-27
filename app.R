
# Dependencies ------------------------------------------------------------


library(magrittr)
library(shiny)
options(encoding = "UTF-8")


# Connect to database -----------------------------------------------


myDB <- DBI::dbConnect(RMySQL::MySQL(), dbname = "sys", user = 'admin', password = 'ArxEd01742!',host = 'arxed-sal.cnlnwcegporn.us-east-1.rds.amazonaws.com', port = 8209)

 check_creds <- function(dbname, host, port, db_user, db_password) {
      function(user, password) {
        
        con <- DBI::dbConnect(RMySQL::MySQL(), 
                              dbname = dbname, 
                              user = db_user, 
                              password = db_password,
                              host = host, 
                              port = port)
        
        on.exit(DBI::dbDisconnect(con))
        
        
        res <- RMySQL::fetch(RMySQL::dbSendQuery(myDB, glue::glue_sql("SELECT * 
                                                            FROM users 
                                                            WHERE user = {user} 
                                                            AND password = {password}
                                                            ", user = user, password = password, .con = con)))
        if (nrow(res) > 0) {
          list(result = TRUE, user_info = list(user = user, something = 123))
        } else {
          list(result = FALSE)
        }
      }
 }
 

# Data --------------------------------------------------------------------

 gt_theme_538 <- function(data,...) {
    data %>%
        gt::opt_all_caps()  %>%
        gt::opt_table_font(
            font = list(gt::default_fonts())
        ) %>%
        gt::tab_style(
            style = gt::cell_borders(
                sides = "bottom", color = "black", weight = gt::px(3)
            ),
            locations = gt::cells_body(
                columns = TRUE,
                # This is a relatively sneaky way of changing the bottom border
                # Regardless of data size
                rows = nrow(data$`_data`)
            )
        )  %>% 
        gt::tab_options(
            column_labels.background.color = "white",
            table.border.top.width = gt::px(3),
            table.border.top.color = "white",
            table.border.bottom.color = "white",
            table.border.bottom.width = gt::px(3),
            table_body.border.bottom.color = "black",
            column_labels.border.top.width = gt::px(3),
            column_labels.border.top.color = "black",
            column_labels.border.lr.color = "grey",
            column_labels.vlines.style = "solid",
            table_body.vlines.style = "solid",
            table_body.vlines.color = "lightgrey",
            column_labels.border.bottom.width = gt::px(3),
            column_labels.border.bottom.color = "black",
            data_row.padding = gt::px(3),
            source_notes.font.size = 12,
            table.font.size = 16,
            heading.align = "left",
            ...
        ) 
}
 
school_info <- readr::read_csv("school_info.csv") %>%
    dplyr::mutate(
        dart_name = dplyr::case_when(
            district_name == "Seven Hills Charter Public (District)" ~ "Learning First Charter Public School (District)",
            district_name == "Massachusetts Virtual Academy at Greenfield Commonwealth Virtual District" ~ "Greater Commonwealth Virtual District",
            district_name == "Hampden Charter School of Science (District)" ~ "Hampden Charter School of Science East (District)",
            district_name == "Tri County Regional Vocational Technical" ~ "Tri-County Regional Vocational Technical",
            district_name == "Brooke Charter School East Boston (District)" ~ "Brooke Charter School (District)",
            district_name == "Greater Commonwealth Virtual District" ~ "Greenfield Commonwealth Virtual District",
            district_name == "Southern Worcester County Regional Vocational School District" ~ "Southern Worcester County Regional Vocational Technical",
            district_name == "Boylston" ~ "Berlin-Boylston",
            district_name == "Berlin" ~ "Berlin-Boylston",
            TRUE ~ district_name),
        district_name = dplyr::case_when(
            district_name == "Seven Hills Charter Public (District)" ~ "Learning First Charter Public School (District)",
            district_name == "Massachusetts Virtual Academy at Greenfield Commonwealth Virtual District" ~ "Greater Commonwealth Virtual District",
            district_name == "Tri County Regional Vocational Technical" ~ "Tri-County Regional Vocational Technical",
            district_name == "Greater Commonwealth Virtual District" ~ "Greenfield Commonwealth Virtual District",
            district_name == "Southern Worcester County Regional Vocational School District" ~ "Southern Worcester County Regional Vocational Technical",
            TRUE ~ district_name))
 
district_options <- school_info %>% dplyr::pull(district_name) %>% unique()
 
student_needs <- read.csv('student_needs.csv') %>% 
    dplyr::mutate(
        dart_name = dplyr::case_when(
            district_name == "Seven Hills Charter Public (District)" ~ "Learning First Charter Public School (District)",
            district_name == "Massachusetts Virtual Academy at Greenfield Commonwealth Virtual District" ~ "Greater Commonwealth Virtual District",
            district_name == "Hampden Charter School of Science (District)" ~ "Hampden Charter School of Science East (District)",
            district_name == "Tri County Regional Vocational Technical" ~ "Tri-County Regional Vocational Technical",
            district_name == "Brooke Charter School East Boston (District)" ~ "Brooke Charter School (District)",
            district_name == "Greater Commonwealth Virtual District" ~ "Greenfield Commonwealth Virtual District",
            district_name == "Southern Worcester County Regional Vocational School District" ~ "Southern Worcester County Regional Vocational Technical",
            district_name == "Boylston" ~ "Berlin-Boylston",
            district_name == "Berlin" ~ "Berlin-Boylston",
            TRUE ~ district_name),
        district_name = dplyr::case_when(
            district_name == "Seven Hills Charter Public (District)" ~ "Learning First Charter Public School (District)",
            district_name == "Massachusetts Virtual Academy at Greenfield Commonwealth Virtual District" ~ "Greater Commonwealth Virtual District",
            district_name == "Tri County Regional Vocational Technical" ~ "Tri-County Regional Vocational Technical",
            district_name == "Greater Commonwealth Virtual District" ~ "Greenfield Commonwealth Virtual District",
            district_name == "Southern Worcester County Regional Vocational School District" ~ "Southern Worcester County Regional Vocational Technical",
            TRUE ~ district_name))

student_diversity <- read.csv('student_diversity.csv') %>% 
    dplyr::mutate(
        dart_name = dplyr::case_when(
            district_name == "Seven Hills Charter Public (District)" ~ "Learning First Charter Public School (District)",
            district_name == "Massachusetts Virtual Academy at Greenfield Commonwealth Virtual District" ~ "Greater Commonwealth Virtual District",
            district_name == "Hampden Charter School of Science (District)" ~ "Hampden Charter School of Science East (District)",
            district_name == "Tri County Regional Vocational Technical" ~ "Tri-County Regional Vocational Technical",
            district_name == "Brooke Charter School East Boston (District)" ~ "Brooke Charter School (District)",
            district_name == "Greater Commonwealth Virtual District" ~ "Greenfield Commonwealth Virtual District",
            district_name == "Southern Worcester County Regional Vocational School District" ~ "Southern Worcester County Regional Vocational Technical",
            district_name == "Boylston" ~ "Berlin-Boylston",
            district_name == "Berlin" ~ "Berlin-Boylston",
            TRUE ~ district_name),
        district_name = dplyr::case_when(
            district_name == "Seven Hills Charter Public (District)" ~ "Learning First Charter Public School (District)",
            district_name == "Massachusetts Virtual Academy at Greenfield Commonwealth Virtual District" ~ "Greater Commonwealth Virtual District",
            district_name == "Tri County Regional Vocational Technical" ~ "Tri-County Regional Vocational Technical",
            district_name == "Greater Commonwealth Virtual District" ~ "Greenfield Commonwealth Virtual District",
            district_name == "Southern Worcester County Regional Vocational School District" ~ "Southern Worcester County Regional Vocational Technical",
            TRUE ~ district_name))

student_mobility <- read.csv('student_mobility.csv') %>% 
    dplyr::mutate(
        dart_name = dplyr::case_when(
            district_name == "Seven Hills Charter Public (District)" ~ "Learning First Charter Public School (District)",
            district_name == "Massachusetts Virtual Academy at Greenfield Commonwealth Virtual District" ~ "Greater Commonwealth Virtual District",
            district_name == "Hampden Charter School of Science (District)" ~ "Hampden Charter School of Science East (District)",
            district_name == "Tri County Regional Vocational Technical" ~ "Tri-County Regional Vocational Technical",
            district_name == "Brooke Charter School East Boston (District)" ~ "Brooke Charter School (District)",
            district_name == "Greater Commonwealth Virtual District" ~ "Greenfield Commonwealth Virtual District",
            district_name == "Southern Worcester County Regional Vocational School District" ~ "Southern Worcester County Regional Vocational Technical",
            district_name == "Boylston" ~ "Berlin-Boylston",
            district_name == "Berlin" ~ "Berlin-Boylston",
            TRUE ~ district_name),
        district_name = dplyr::case_when(
            district_name == "Seven Hills Charter Public (District)" ~ "Learning First Charter Public School (District)",
            district_name == "Massachusetts Virtual Academy at Greenfield Commonwealth Virtual District" ~ "Greater Commonwealth Virtual District",
            district_name == "Tri County Regional Vocational Technical" ~ "Tri-County Regional Vocational Technical",
            district_name == "Greater Commonwealth Virtual District" ~ "Greenfield Commonwealth Virtual District",
            district_name == "Southern Worcester County Regional Vocational School District" ~ "Southern Worcester County Regional Vocational Technical",
            TRUE ~ district_name))

staff_diversity <- read.csv('staff_diversity.csv') %>% 
    dplyr::mutate(
        dart_name = dplyr::case_when(
            district_school_name == "Seven Hills Charter Public (District)" ~ "Learning First Charter Public School (District)",
            district_school_name == "Massachusetts Virtual Academy at Greenfield Commonwealth Virtual District" ~ "Greater Commonwealth Virtual District",
            district_school_name == "Hampden Charter School of Science (District)" ~ "Hampden Charter School of Science East (District)",
            district_school_name == "Tri County Regional Vocational Technical" ~ "Tri-County Regional Vocational Technical",
            district_school_name == "Brooke Charter School East Boston (District)" ~ "Brooke Charter School (District)",
            district_school_name == "Greater Commonwealth Virtual District" ~ "Greenfield Commonwealth Virtual District",
            district_school_name == "Southern Worcester County Regional Vocational School District" ~ "Southern Worcester County Regional Vocational Technical",
            district_school_name == "Boylston" ~ "Berlin-Boylston",
            district_school_name == "Berlin" ~ "Berlin-Boylston",
            TRUE ~ district_school_name),
        district_name = dplyr::case_when(
            district_school_name == "Seven Hills Charter Public (District)" ~ "Learning First Charter Public School (District)",
            district_school_name == "Massachusetts Virtual Academy at Greenfield Commonwealth Virtual District" ~ "Greater Commonwealth Virtual District",
            district_school_name == "Tri County Regional Vocational Technical" ~ "Tri-County Regional Vocational Technical",
            district_school_name == "Greater Commonwealth Virtual District" ~ "Greenfield Commonwealth Virtual District",
            district_school_name == "Southern Worcester County Regional Vocational School District" ~ "Southern Worcester County Regional Vocational Technical",
            TRUE ~ district_school_name))

expenditures <- readr::read_csv("expenditures.csv") %>%
  janitor::clean_names() %>% 
    dplyr::mutate(
        dart_name = dplyr::case_when(
            district == "Seven Hills Charter Public (District)" ~ "Learning First Charter Public School (District)",
            district == "Massachusetts Virtual Academy at Greenfield Commonwealth Virtual District" ~ "Greater Commonwealth Virtual District",
            district == "Hampden Charter School of Science (District)" ~ "Hampden Charter School of Science East (District)",
            district == "Tri County Regional Vocational Technical" ~ "Tri-County Regional Vocational Technical",
            district == "Brooke Charter School East Boston (District)" ~ "Brooke Charter School (District)",
            district == "Greater Commonwealth Virtual District" ~ "Greenfield Commonwealth Virtual District",
            district == "Southern Worcester County Regional Vocational School District" ~ "Southern Worcester County Regional Vocational Technical",
            district == "Boylston" ~ "Berlin-Boylston",
            district == "Berlin" ~ "Berlin-Boylston",
            TRUE ~ district),
        district = dplyr::case_when(
            district == "Seven Hills Charter Public (District)" ~ "Learning First Charter Public School (District)",
            district == "Massachusetts Virtual Academy at Greenfield Commonwealth Virtual District" ~ "Greater Commonwealth Virtual District",
            district == "Tri County Regional Vocational Technical" ~ "Tri-County Regional Vocational Technical",
            district == "Greater Commonwealth Virtual District" ~ "Greenfield Commonwealth Virtual District",
            district == "Southern Worcester County Regional Vocational School District" ~ "Southern Worcester County Regional Vocational Technical",
            TRUE ~ district))
        
dart <- readr::read_csv("all_dart.csv") %>%
    dplyr::select(-c(
        `Berlin-Boylston...36`,
        `Brooke Charter School (District)...58`,
        `Edward M. Kennedy Academy for Health Careers (Horace Mann Charter) (District)...107`,
        `Greenfield Commonwealth Virtual District...136`
    )) %>%
    dplyr::distinct() %>%
    dplyr::rename(district_name = `...1`) %>%
    as.data.frame()
 

# UI ----------------------------------------------------------------------

 
ui <- shinyMobile::f7Page(
    title = paste0("  ArxEd | D³ Data-Driven Decisions"),
    options = list(dark = F),
    tags$script(src = "myscript.js"),
    shinyMobile::f7SingleLayout(
        tags$head(
          tags$link(rel="shortcut icon", href="https://raw.githubusercontent.com/SCasanova/arxed_ddd/main/www/Shield%20Trim.png"),
          tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet_arx2.css"),
          tags$script(src = 'https://kit.fontawesome.com/8c5f048e9f.js',crossorigin="anonymous" )
          ),
        navbar = shinyMobile::f7Navbar( title = tags$div(tags$img(src='https://raw.githubusercontent.com/SCasanova/arxed_ddd/main/www/Shield%20Trim.png',width='45px'),
                                                          tags$span(paste0("  ArxEd | D³ Data Driven Decisions"), style = 'text-align:center;')), 
                                        tags$div(htmlOutput('logo_src'), class = 'top-district-logo')),
        tags$div(shinyMobile::f7Fab(
         inputId = 'pass',
         label = 'Change Password'
         ), class =  'pass-button'),
        tags$div(class = 'password-popup', shinyMobile::f7Popup(
         id = "popup1",
         title = tags$div(class = 'pass-title', h2("Change Password")),
         tags$div(
           shinyMobile::f7Text("newPass", "New Password", ""),
           shinyMobile::f7Text("newPassConf", "Confirm New Password", ""),
           br(),
           shinyMobile::f7Button('submit_pass', 'Submit', size = 'large')
           ),
         
      )),
      tags$div(class = 'select-card',
               shinyMobile::f7Shadow(
                   hover = T,
                   intensity = 16,
                   shinyMobile::f7Card(tags$div(
                       style = 'display:flex',
                       tags$div(
                           tags$div(
                               shinyMobile::f7SmartSelect(
                                   inputId = 'district',
                                   label = tags$span(class = 'input-label', h4('1. Select Your Collaborative:')),
                                   choices = district_options,
                                   selected = c('Concord-Carlisle'),
                                   virtualList = T,
                                   openIn = 'popup'
                               )
                           ),
                           tags$div(
                               shinyMobile::f7Select(
                                   inputId = 'year',
                                   label = tags$span(class = 'input-label', h3('2. Select Year to Examine:')),
                                   choices = c('2015-16', '2016-17', '2017-18', '2018-19', '2019-20', '2020-21', '2021-22'),
                                   selected = '2020-21'
                                   
                               )
                           ),
                           class = 'inputs input-1'
                       ),
                       tags$div(
                           class = 'inputs',
                           shinyMobile::f7CheckboxGroup(
                               inputId = 'comp_cond',
                               label = h5('3. Choose comparison district categories to auto-select:'),
                               choices = c('DART', 'League', 'County', 'Municipality')
                           )
                       ),
                       tags$div(
                           class = 'inputs',
                           shinyMobile::f7CheckboxGroup(
                               inputId = 'comp_filter',
                               label = h5('4. Pare down your results with filters (will exclude any district that fails to meet ANY of the following criteria):'),
                               choices = c('Total Budget', 'Enrollment', 'Teacher FTE', 'Avg Teacher Salary')
                           ),
                           uiOutput('budget_condition'),
                           uiOutput('enrollment_condition'),
                           uiOutput('fte_condition'),
                           uiOutput('salary_condition')
                       )
                   ),
                   tags$div(
                       shinyMobile::f7SmartSelect(
                           inputId = 'district_comps',
                           label = tags$span(class = 'input-label', h4('5. View your comparison districts and add/remove any manually:')),
                           multiple = T,
                           choices = district_options,
                           selected = tableOutput('district_comps_initial'),
                           openIn = 'popup',
                           virtualList = T
                       )
                   ),
                   footer = tagList(
                             tags$div(style= 'display:flex;', tags$div(
                                 shinyMobile::f7Button( #calls for DB replacing 
                                     "save",
                                     'Save "My Schools"',
                                     outline = FALSE,
                                     fill = TRUE,
                                     shadow = FALSE,
                                     rounded = FALSE,
                                     size = NULL
                                 ),
                                 class = 'card-button',
                             ),
                             tags$div(
                                 shinyMobile::f7Button( #resets inputs
                                     "reset",
                                     "Reset",
                                     outline = FALSE,
                                     fill = TRUE,
                                     shadow = FALSE,
                                     rounded = FALSE,
                                     size = NULL
                                 ),
                                 class = 'card-button',
                             ))
                         )
                   )
               )), 
      shinyMobile::f7Tabs(
          animated = T,
            swipeable = F,
            id = "tabset",
            style = 'strong',
          shinyMobile::f7Tab(
               tabName = "Home",
                icon = shinyMobile::f7Icon("house"),
                active = T,
               tags$div(style = 'display:flex;',
                 tags$div(class = 'side-panel',
                          style = 'padding:10px;',
                   gt::gt_output('summary_table')
                 ),
                   htmlOutput('district_at_glance'),
                   tags$div(class = 'float-logo', htmlOutput('main_logo'))
                 ),
               tags$div(style = 'display:flex',
                 tags$div(class = 'side-panel',
                   shinyMobile::f7Shadow(
                     hover = T,
                     intensity = 16,
                     shinyMobile::f7Card(
                       tags$div('Contract Elements', class = 'text1'),
                       tags$div(htmlOutput('comp_2')),
                       br(),
                       plotly::plotlyOutput('cola_plot_all', height = 'auto'),
                        plotly::plotlyOutput('upper_left', height = 'auto'),
                       plotly::plotlyOutput('lower_right', height = 'auto')
                     )
                   )
                 ),
                 tags$div(style = 'width:75%;',
                          shinyMobile::f7Shadow(
                            hover = T,
                            intensity = 16,
                            shinyMobile::f7Card(
                              tags$div(plotly::plotlyOutput('total-budget', height = 'auto')),
                              tags$div(style = 'display:flex;',
                                       
                                
                              )
                              )
                            )
                 )
               )
          ),
          shinyMobile::f7Tab(
               tabName = "Overview",
                icon = shinyMobile::f7Icon("search"),
                active = F
          ),
          shinyMobile::f7Tab(
               tabName = "Students",
                icon = shinyMobile::f7Icon("person_3"),
                active = TRUE,
               shinyMobile::f7Shadow(
                 hover = T,
                 intensity = 16,
                 shinyMobile::f7Card(
                    plotly::plotlyOutput('student_general')
                 )
               ),
               shinyMobile::f7Shadow(
                 hover = T,
                 intensity = 16,
                 shinyMobile::f7Card(
                    plotly::plotlyOutput('student_needs')
                 )
               ),
               shinyMobile::f7Shadow(
                 hover = T,
                 intensity = 16,
                 shinyMobile::f7Card(tags$div(
                   style = 'display:flex',
                   tags$div(plotly::plotlyOutput('student_diversity'),
                            class = 'inputs'),
                   tags$div(plotly::plotlyOutput('student_diversity_pie'),
                            class = 'inputs')
                 ))
               ), 
               shinyMobile::f7Shadow(
                 hover = T,
                 intensity = 16,
                 shinyMobile::f7Card(
                   tags$div(plotly::plotlyOutput('student_mobility'),
                                                 htmlOutput('mobility_info'))
                   
                 )
               )
          ),
          shinyMobile::f7Tab(
               tabName = "Staff",
                icon = shinyMobile::f7Icon("person_2"),
                active = F,
               shinyMobile::f7Shadow(
                 hover = T,
                 intensity = 16,
                 shinyMobile::f7Card(
                   tags$div(style = "display:flex;",
                        tags$div(class = 'inputs',
                          plotly::plotlyOutput('staff_general')),
                        tags$div(class = 'inputs',
                          plotly::plotlyOutput('staff_ratio'))
                        )
                 )
               ),
               shinyMobile::f7Shadow(
                 hover = T,
                 intensity = 16,
                 shinyMobile::f7Card(
                   tags$div(style = "display:flex;",
                        tags$div(class = 'inputs',
                          plotly::plotlyOutput('district_gender')),
                        tags$div(class = 'inputs',
                          plotly::plotlyOutput('other_gender'))
                        )
                 )
               ),
               tags$div(style = 'display:flex',
                 tags$div(
                   class = 'inputs',
                   shinyMobile::f7Shadow(
                     hover = T,
                     intensity = 16,
                     shinyMobile::f7Card(plotly::plotlyOutput('staff_diversity'))
                   )
                 ),
                 tags$div(
                   stlye = 'inputs',
                   shinyMobile::f7Shadow(
                     hover = T,
                     intensity = 16,
                     shinyMobile::f7Card(plotly::plotlyOutput('staff_diversity_pie'))
                   )
               )
               ),
               shinyMobile::f7Shadow(
                     hover = T,
                     intensity = 16,
                     shinyMobile::f7Card(
                       plotly::plotlyOutput('teacher_percents')
                     )
               )
          ),
          shinyMobile::f7Tab(
               tabName = "Finances",
                icon = shinyMobile::f7Icon("money_dollar"),
                active = TRUE
          ),
          shinyMobile::f7Tab(
               tabName = "Academic",
                icon = shinyMobile::f7Icon("book"),
                active = TRUE
          ),
          shinyMobile::f7Tab(
               tabName = "Work Cond.",
                icon = shinyMobile::f7Icon("briefcase"),
                active = TRUE,
               shinyMobile::f7Shadow(
                     hover = T,
                     intensity = 16,
                     shinyMobile::f7Card(
                       plotly::plotlyOutput('days_w_wo')
                     )
               ),
               shinyMobile::f7Shadow(
                     hover = T,
                     intensity = 16,
                     shinyMobile::f7Card(
                         plotly::plotlyOutput('health_percents')
                       
                     )
               ),
               shinyMobile::f7Shadow(
                 hover = T,
                 intensity = 16,
                 shinyMobile::f7Card(
                   tags$div(style = "display:flex;",
                        tags$div(class = 'inputs',
                          plotly::plotlyOutput('sick_bank')),
                        tags$div(class = 'inputs',
                          plotly::plotlyOutput(''))
                        )
                 )
               )
          )
      )
    )
)
 

# Server ------------------------------------------------------------------

 

 server <- function(input, output, session) {
     
     district <- reactive(input$district)
     comp_year <- reactive(input$year)
     
     output$main_logo <- renderUI({
        tags$img(src = "https://raw.githubusercontent.com/SCasanova/arxed_ddd/main/www/D3%20Logo.png",
            width = '180px',
             alt = "")
    })
     
     league <- reactive({
      RMySQL::fetch(RMySQL::dbSendQuery(myDB, paste0("SELECT league  
                                                        FROM district_info 
                                                        WHERE district_name =", "'",  district(),"';"))) %>% 
         unique() %>% 
      as.character()
  })
     county <- reactive({
      RMySQL::fetch(RMySQL::dbSendQuery(myDB, paste0("SELECT county  
                                                        FROM district_info 
                                                        WHERE district_name =", "'",  district(),"';"))) %>% 
        unique() %>% 
         as.character()
  })
     municipality <- reactive({
      RMySQL::fetch(RMySQL::dbSendQuery(myDB, paste0("SELECT municipality  
                                                        FROM district_info 
                                                        WHERE district_name =", "'",  district(),"';"))) %>% 
        unique() %>% 
         as.character()
  })
     
     
     
     district_comps_initial <- reactive({
      req(input$comp_cond)
      if('County' %in% input$comp_cond){
           RMySQL::fetch(RMySQL::dbSendQuery(myDB, paste0("SELECT district_name  
                                                        FROM district_info 
                                                        WHERE county =", "'",  county(),"';"))) %>% 
              dplyr::pull(district_name) %>% 
          unique()
      } 
       else if('League'  %in% input$comp_cond){
           RMySQL::fetch(RMySQL::dbSendQuery(myDB, paste0("SELECT district_name  
                                                        FROM district_info 
                                                        WHERE league =", "'",  league(),"';"))) %>% 
              dplyr::pull(district_name)%>% 
          unique()
       } 
       else if('Municipality'  %in% input$comp_cond){
           RMySQL::fetch(RMySQL::dbSendQuery(myDB, paste0("SELECT district_name  
                                                        FROM district_info
                                                        WHERE municipality =", "'",  municipality(),"';"))) %>% 
              dplyr::pull(district_name)%>% 
          unique()
       } else if('DART'  %in% input$comp_cond){
           # Get data frame with DART names and district names
            dart_names <- school_info %>%
                dplyr::select(district_name, dart_name) %>%
                dplyr::distinct()
            
            # Get individual district's DART name
            dart_district <- dart_names %>%
                dplyr::filter(district_name == district()) %>%
                dplyr::pull(dart_name)
            
            # Get DART districts
            darts <- dart %>%
                # dplyr::select(district_name, as.name(district)) %>%
                dplyr::filter(get(dart_district) == 1, district_name != dart_district) %>%
                dplyr::pull(district_name)
            
            # Convert those DART names back to district names
            dart_districts <- c()
            
            for (i in 1:length(darts)){
                
                new_dart <- dart_names %>%
                    dplyr::filter(dart_name == darts[i]) %>%
                    dplyr::pull(district_name)
                
                dart_districts <- append(dart_districts, new_dart)
            }
       dart_districts     
      } 
        
  })
     

  observeEvent(input$comp_cond, {
      shinyMobile::updateF7SmartSelect(
        inputId = "district_comps",
        openIn = "popup",
        virtualList = T,
        selected = district_comps_initial(),
        choices = district_options,
        multiple = TRUE
      )
    })
  
   comp_districts <- reactive({
     input$district_comps
     })
     
     district_logo <- reactive({ #access district table to get name and logo associated with district
       tempDB <- DBI::dbConnect(RMySQL::MySQL(), dbname = "sys", user = 'admin', password = 'ArxEd01742!',host = 'arxed-sal.cnlnwcegporn.us-east-1.rds.amazonaws.com', port = 8209)
       logo <- RMySQL::fetch(RMySQL::dbSendQuery(myDB, paste0("SELECT logo  
                                                        FROM districts 
                                                        WHERE district_name =", "'",  district(),"';"))) %>% 
         as.character()
       RMySQL::dbDisconnect(tempDB)
       logo
       
  })

  
  output$logo_src <- renderUI(
    tags$div(
      style = 'display:flex',
      tags$div(
        class = 'dis-name',
        district()
      ),
      tags$div(
        tags$img(src = district_logo(), height = '50px')
      )
    )
    
  )
  
  
  output$budget_condition <- renderUI({
    req(input$comp_filter)
    if('Total Budget' %in% input$comp_filter){
      shinyMobile::f7Slider(
            "budget",
            label = "Total Budget (millions):",
            min = 0,
            max = 1600,
            value = c(0,1600))
    } else{
      
    } 
    })

output$enrollment_condition <- renderUI({
  req(input$comp_filter)  
  if('Enrollment' %in% input$comp_filter){
       shinyMobile::f7Slider(
            "enrollment",
            label = "Enrollment (hundreds):",
            min = 0,
            max = 500,
            value = c(0,500))
    } else{
      
    } 
})
  
output$fte_condition <- renderUI({
  req(input$comp_filter)
  if('Teacher FTE' %in% input$comp_filter){
      shinyMobile::f7Slider(
            "fte",
            label = "Teacher FTE:",
            min = 0,
            max = 4500,
            value = c(0,4500))
    } else{
      
    } 
})
  
output$salary_condition <- renderUI({
  req(input$comp_filter)
  if('Avg Teacher Salary' %in% input$comp_filter){
      shinyMobile::f7Slider(
            "salary",
            label = "Avg Teacher Salary (thousands):",
            min = 0,
            max = 125,
            value = c(0,125))
  } else{
      
    }
  })
  
  observeEvent(input$reset, {
      shinyMobile::updateF7SmartSelect(
        inputId = "district_comps",
        openIn = "popup",
        selected = "",
        choices = district_options,
        multiple = TRUE,
        virtualList = T
      )
    })
  
  

# Home --------------------------------------------------------------------

summary_gt <- reactive({
    district_df <- school_info %>%
            dplyr::filter(district_name == district(),
                   year == comp_year()) %>%
            dplyr::mutate(
                total_enrollment = paste("Enrollment:", total_enrollment),
                total_teachers = paste("Teacher FTE:", total_teachers),
                year = paste("Year:", year),
                league = paste('League:', league),
                county =  paste('League:', county),
                city =  paste('Municipality:', city)) %>%
            dplyr::select(year, total_enrollment, total_teachers, league, county, city) %>%
            t() %>%
            as.data.frame()
  
    district_df %>%
            gt::gt() %>%
            gt_theme_538() %>%
            gt::cols_label(V1 = gtExtras::img_header(label = district(),
                                                 img_url = district_logo(),
                                                 height = 90)) %>% 
          gt::cols_width(
                 gt::everything() ~ gt::pct(100)
              ) 
  })
    
  
  output$summary_table <- gt::render_gt(
    expr = summary_gt(),
    width = gt::pct(100)
    )
  
  
  # Title of home panel
    output$district_at_glance <- renderUI({
      
      len_comps <- length(comp_districts())
        
        for (i in 1:len_comps){
            if (i == 1){
                comp_list <- comp_districts()[i]
            }
            else{
                comp_list <- paste0(comp_list, ", ", comp_districts()[i])
            }
        }
      
      tags$div(
        tags$div(class = 'title',
          tags$span(paste0(district()), style = "color:#DB9743"), " at a Glance"),
        tags$div(class = 'subtitle',
          paste0(district(), " vs."), tags$span("Comparison Districts", style = "color:#2a3a6e")),
        tags$div(class = 'comparison',
          "Comparing to: ", tags$span(comp_list, style = "color:#2a3a6e"))
        )

    })
    
    output$comp_2 <- renderUI({
      tags$div(class = 'comparison',
          tags$span(paste0(district()), style = "color:#DB9743"), " vs All Available Districts")
    })
    
    plot_all_data <- reactive({
      school_info %>%
            dplyr::mutate(is_district = ifelse(district_name == district(), district(), "Others")) %>%
            dplyr::group_by(is_district) %>%
            dplyr::summarize(
              dplyr::across(total_enrollment:low_income_pct,
                             ~ mean(.x, na.rm = TRUE)
                            )
            ) %>%
            dplyr::mutate(short_district = stringr::str_trunc(is_district, 20, "right"))
            
      
    })
    
    cola_all_data <- reactive({
      plot_all_data() %>% 
        dplyr::select(cola_2020_21:cola_2022_23) %>% 
        t() %>%
        data.frame() %>% 
        dplyr::rename(district = X1,
                   other = X2)
    })
    
    output$cola_plot_all <- plotly::renderPlotly({
      plotly::plot_ly(
        cola_all_data(),
        x = ~other,
        y = ~ c('2022-23', '2021-22', '2020-21'),
        type = 'bar',
        name = 'A',
        orientation = 'h',
        marker = list(color = '#2a3a6e'),
        hovertemplate = '%{x:.2%}<extra></extra>',
        texttemplate = '%{x:.1%}',
        textposition = 'inside'
      ) %>%
         plotly:: config(displayModeBar = FALSE) %>%
        plotly::add_trace(x = ~district,
                  name = 'Other',
                  marker = list(color = '#DB9743')) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T),
          yaxis = list(title = "", fixedrange = T),
          barmode = 'group',
          title = 'COLA %',
          autosize = T,
          height = 235,
          showlegend =F,
          font = list(size = 12)
        )
  })
    
  upper_left_data <- reactive({
      plot_all_data() %>% 
        dplyr::select(upper_left_2020_21:upper_left_2022_23) %>% 
        t() %>%
        data.frame() %>% 
        dplyr::rename(district = X1,
                   other = X2)
    })
    
    output$upper_left <- plotly::renderPlotly({
      plotly::plot_ly(
        upper_left_data(),
        x = ~other,
        y = ~ c('2022-23', '2021-22', '2020-21'),
        type = 'bar',
        name = 'A',
        orientation = 'h',
        marker = list(color = '#2a3a6e'),
        hovertemplate = '%{x:.3s}<extra></extra>',
        texttemplate = '%{x:.2s}',
        textposition = 'inside'
      ) %>%
         plotly:: config(displayModeBar = FALSE) %>%
        plotly::add_trace(x = ~district,
                  name = 'Other',
                  marker = list(color = '#DB9743')) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T),
          yaxis = list(title = "", fixedrange = T),
          barmode = 'group',
          title = 'Upper-Left Salary',
          autosize = T,
          height = 235,
          showlegend =F,
          font = list(size = 12)
        )
  })
    
  lower_right_data <- reactive({
      plot_all_data() %>% 
        dplyr::select(lower_right_2020_21:lower_right_2022_23) %>% 
        t() %>%
        data.frame() %>% 
        dplyr::rename(district = X1,
                   other = X2)
    })
    
    output$lower_right <- plotly::renderPlotly({
      plotly::plot_ly(
        lower_right_data(),
        x = ~other,
        y = ~ c('2022-23', '2021-22', '2020-21'),
        type = 'bar',
        name = 'A',
        orientation = 'h',
        marker = list(color = '#2a3a6e'),
        hovertemplate = '%{x:.3s}<extra></extra>',
        texttemplate = '%{x:.2s}',
        textposition = 'inside'
      ) %>%
         plotly:: config(displayModeBar = FALSE) %>%
        plotly::add_trace(x = ~district,
                  name = 'Other',
                  marker = list(color = '#DB9743')) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T),
          yaxis = list(title = "", fixedrange = T),
          barmode = 'group',
          title = 'Lower-Right Salary',
          autosize = T,
          height = 235,
          showlegend =F,
          font = list(size = 12)
        )
  })
    
  average_salary_data <- reactive({
      plot_all_data() %>% 
        dplyr::select(salary_avg_2020_21:salary_avg_2022_23) %>% 
        t() %>%
        data.frame() %>% 
        dplyr::rename(district = X1,
                   other = X2)
    })
  
    
    output$lower_right <- plotly::renderPlotly({
      plotly::plot_ly(
        average_salary_data(),
        y = ~other,
        x = ~ c('2020-21', '2021-22', '2022-23'),
        type = 'scatter',
        name = 'A',
        mode = 'lines+markers',
        marker = list(color = '#2a3a6e'),
        hovertemplate = '%{y:.3s}<extra></extra>',
        texttemplate = '%{:.2s}',
        textposition = 'inside'
      ) %>%
         plotly:: config(displayModeBar = FALSE) %>%
        plotly::add_trace(y = ~district,
                  name = 'Other',
                  marker = list(color = '#DB9743')) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T),
          yaxis = list(title = "", fixedrange = T, range  = c(min(average_salary_data())*0.85, max(average_salary_data())*1.15)),
          title = 'Average Salary',
          autosize = T,
          height = 235,
          showlegend =F,
          font = list(size = 12)
        )
  })
    
    
    

# Overview ----------------------------------------------------------------

  
# Plot DF for multiple use ------------------------------------------------

plot_comp_df <- reactive({
        
        school_info %>%
            dplyr::filter(district_name %in% comp_districts() | (district_name == district() &
                   year == comp_year()) |  district_name == 'dummy') %>%
            dplyr::mutate(is_district = ifelse(district_name == district(), district(), "Others")) %>%
            dplyr::group_by(is_district) %>%
            dplyr::summarize(
              dplyr::across(total_enrollment:low_income_pct,
                             ~ mean(.x, na.rm = TRUE)
                            )
            ) %>%
            dplyr::mutate(short_district = stringr::str_trunc(is_district, 20, "right"))
            
        
    })
  
# Students ----------------------------------------------------------------

    student_comp <- reactive({
        school_info %>%
            dplyr::filter(district_name %in% comp_districts(),
                          year == comp_year()) %>%
            dplyr::select(grade_pk:grade_12) %>%
            as.data.frame() %>% 
            dplyr::summarise(
                dplyr::across(
                    grade_pk:grade_12,
                    ~ mean(.x, na.rm = TRUE)
                )
            )
            
    })            
    
    
    student_district <- reactive({
        school_info %>%
            dplyr::filter(district_name == district(),
                          year == comp_year()) %>%
            dplyr::select(grade_pk:grade_12) %>%
            # magrittr::set_colnames(overview_statistics) %>%
            as.data.frame()
    })
    
    general_data <- reactive({
        dplyr::bind_rows(student_comp(), student_district()) %>% 
            t() %>% 
            data.frame() %>% 
        dplyr::rename(other = X1,
               district = X2)
    })
    
    
    
    output$student_general <-  plotly::renderPlotly(
        plotly::plot_ly(
        general_data(),
        x = ~ c('PK', 'K', purrr::map_chr(seq(12), function(x){paste('Grade', x)})), #cleaner name),
        y = ~ district,
        type = 'bar',
        name = district(),
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(district()  , ': %{y}<extra></extra>'),
        texttemplate = '%{y}',
        textposition = 'outside'
      )  %>%
        plotly::layout(
          xaxis = list(title = "", tickangle = -45, fixedrange = T, categoryorder = "array", categoryarray = c('PK', 'K', purrr::map_chr(seq(12), function(x){paste('Grade', x)}))),
          yaxis = list(title = "", fixedrange = T,automargin = T, range = c(0, max(general_data(), na.rm = T)*1.15)),
          barmode = 'group',
          showlegend =F,
          font = list(size = 18),
          images = list(
            source = "https://raw.githubusercontent.com/SCasanova/arxed_ddd/main/www/D3%20Logo.png",
            x = 0.02, y = 1.1,
            sizex = 0.2, sizey = 0.2
        )
        ) %>%
        plotly::add_trace(y = ~ other,
                  name = 'Other',
                   hovertemplate = 'Other: %{y:,.1f}<extra></extra>',
                   texttemplate = '%{y:,.0f}',
                  marker = list(color = '#2a3a6e'))
    )

    needs_comp <- reactive({
        student_needs %>%
            dplyr::filter(district_name %in% comp_districts(),
                          year == comp_year()) %>%
            dplyr::select(economically_disadvantaged_percent,english_language_learner_percent,students_with_disabilities_percent ) %>%
            as.data.frame() %>%
            dplyr::summarise(
                dplyr::across(
                    (economically_disadvantaged_percent:students_with_disabilities_percent),
                    ~ mean(.x, na.rm = TRUE)
                )
            )

    })


    needs_district <- reactive({
        student_needs %>%
            dplyr::filter(district_name == district(),
                          year == comp_year()) %>%
            dplyr::select(economically_disadvantaged_percent,english_language_learner_percent,students_with_disabilities_percent ) %>%
            as.data.frame()
    })

    needs_data <- reactive({
        req(needs_district())
        dplyr::bind_rows(needs_comp(), needs_district()) %>%
            t() %>%
            data.frame() %>% 
            dplyr::rename(other = X1,
                   district = X2)
    })
    


    output$student_needs <-  plotly::renderPlotly(
        plotly::plot_ly(
        needs_data(),
        x = ~c('% Economicaly Disadvantaged', '% English Language Learners', '% Special Education'),
        y = ~ district,
        type = 'bar',
        name = district(),
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(district()  , ': %{y}%<extra></extra>'),
        texttemplate = '%{y}%',
        textposition = 'auto'
      ) %>%
        plotly::add_trace(y = ~ other,
                  name = 'Other',
                   hovertemplate = 'Other: %{y:,.1f}%<extra></extra>',
                   texttemplate = '%{y:,.1f}%',
                  marker = list(color = '#2a3a6e')) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('PK', 'K', purrr::map_chr(seq(12), function(x){paste('Grade', x)}))),
          yaxis = list(title = "", fixedrange = T,automargin = T),
          barmode = 'group',
          showlegend =F,
          font = list(size = 18)
        )
    )
    
    diversity_comp <- reactive({
        student_diversity %>%
            dplyr::filter(district_name %in% comp_districts(),
                          year == comp_year()) %>%
            dplyr::select(african_american_percent:multi_race_non_hispanic_percent) %>%
            as.data.frame() %>%
            dplyr::summarise(
                dplyr::across(
                    (african_american_percent:multi_race_non_hispanic_percent),
                    ~ mean(.x, na.rm = TRUE)
                )
            )

    })


    diversity_district <- reactive({
        student_diversity %>%
            dplyr::filter(district_name == district(),
                          year == comp_year()) %>%
            dplyr::select(african_american_percent:multi_race_non_hispanic_percent) %>%
            as.data.frame()
    })

    diversity_data <- reactive({
        req(diversity_district())
        dplyr::bind_rows(diversity_comp(), diversity_district()) %>%
            t() %>%
            data.frame() %>% 
            dplyr::rename(other = X1,
                   district = X2)
    })
    
    output$student_diversity <-  plotly::renderPlotly(
        plotly::plot_ly(
        diversity_data(),
        x = ~ c('African American', 'Asian', 'Hispanic', 'White', 'Native American', 'Hawaiian/Pacific Islander', 'Multi-Race'),
        y = ~ district,
        type = 'bar',
        name = district(),
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(district()  , ': %{y}%<extra></extra>'),
        texttemplate = '%{y:,.0f}%',
        textposition = 'outside'
      )  %>%
        plotly::layout(
          xaxis = list(title = "", tickangle = -25, fixedrange = T, categoryorder = "array", categoryarray = c('PK', 'K', purrr::map_chr(seq(12), function(x){paste('Grade', x)}))),
          yaxis = list(title = "", fixedrange = T,automargin = T,range = c(0, max(diversity_data(), na.rm = T)*1.15)),
          barmode = 'group',
          showlegend =F,
          font = list(size = 18),
          images = list(
            source = "https://raw.githubusercontent.com/SCasanova/arxed_ddd/main/www/D3%20Logo.png",
            x = 0.02, y = 1.1,
            sizex = 0.2, sizey = 0.2
        )
        )%>%
        plotly::add_trace(y = ~ other,
                  name = 'Other',
                   hovertemplate = 'Other: %{y:,.1f}%<extra></extra>',
                   texttemplate = '%{y:,.0f}%',
                  marker = list(color = '#2a3a6e'))
    )
    
    pie_data <- reactive(diversity_district() %>% t() %>% data.frame() %>% 
                            janitor::clean_names())
    observe(print(pie_data()))

    output$student_diversity_pie <- plotly::renderPlotly(
        plotly::plot_ly(
            pie_data(),
            labels = ~ c('African American', 'Asian', 'Hispanic', 'White', 'Native American', 'Hawaiian/Pacific Islander', 'Multi-Race'),
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            values = ~x,
            hoverinfo = 'label+percent',
            text = ~paste0(x, '%'),
             marker = list(colors = c('#DB9743', '#3F7CAC', '#2E282A', '#2a3a6e', '#F3EFF5', '#C03221', '#7DAA92'),
                      line = list(color = '#FFFFFF', width = 1))
            )%>%
            plotly::add_pie(hole = 0.4)%>%
            plotly::layout(
                title = district(),
                autosize = T,
                showlegend = F,
                xaxis = list(
                    showgrid = FALSE,
                    zeroline = FALSE,
                    showticklabels = FALSE
                ),
                yaxis = list(
                    showgrid = FALSE,
                    zeroline = FALSE,
                    showticklabels = FALSE
                )
            )
    )
    
    mobility_comp <- reactive({
        student_mobility %>%
            dplyr::filter(district_name %in% comp_districts(),
                          year == comp_year()) %>%
            dplyr::select(percent_churn, percent_intake, percent_stability) %>%
            as.data.frame() %>%
            dplyr::summarise(
                dplyr::across(
                    c(percent_churn, percent_intake, percent_stability),
                    ~ mean(.x, na.rm = TRUE)
                )
            )

    })


    mobility_district <- reactive({
        student_mobility %>%
            dplyr::filter(district_name == district(),
                          year == comp_year()) %>%
            dplyr::select(percent_churn, percent_intake, percent_stability) %>%
            as.data.frame()
    })

    mobility_data <- reactive({
        req(mobility_district())
        dplyr::bind_rows(mobility_comp(), mobility_district()) %>%
            t() %>%
            data.frame() %>% 
            dplyr::rename(other = X1,
                   district = X2)
    })
    
    output$student_mobility <-  plotly::renderPlotly(
        plotly::plot_ly(
        mobility_data(),
        x = ~ c('Churn %', 'Intake %', 'Stability %'),
        y = ~ district,
        type = 'bar',
        name = district(),
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(district()  , ': %{y}%<extra></extra>'),
        texttemplate = '%{y}%',
        textposition = 'outside'
      )  %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('PK', 'K', purrr::map_chr(seq(12), function(x){paste('Grade', x)}))),
          yaxis = list(title = "", fixedrange = T,automargin = T, range = c(0,max( mobility_data(), na.rm = T)*1.15)),
          barmode = 'group',
          showlegend =F,
          font = list(size = 18),
          images = list(
            source = "https://raw.githubusercontent.com/SCasanova/arxed_ddd/main/www/D3%20Logo.png",
            x = 0.02, y = 1.1,
            sizex = 0.2, sizey = 0.2
        )
        )%>%
        plotly::add_trace(y = ~ other,
                  name = 'Other',
                   hovertemplate = 'Other: %{y:,.1f}%<extra></extra>',
                   texttemplate = '%{y:,.1f}%',
                  marker = list(color = '#2a3a6e'))
    )
    
    output$mobility_info <- renderUI(
      tags$div(style = "display:flex;",
               tags$div(class = 'info-text', "Churn: The combined intake & attrition (left before school year ends) rate of students."),
               tags$div(class = 'info-text', "Intake: Students coming into a district after the school year starts."),
               tags$div(class = 'info-text', "Stability: Students that start & end the school year in the same district.")
               )
    )

# Staff Tab ---------------------------------------------------------------

    staff_gen_comp <- reactive({
        school_info %>%
            dplyr::filter(district_name %in% comp_districts(),
                          year == comp_year()) %>%
            dplyr::select(total_in_district_fte) %>%
            as.data.frame() %>%
            dplyr::summarise(
                dplyr::across(
                    c(total_in_district_fte),
                    ~ mean(.x, na.rm = TRUE)
                )
            )

    })


    staff_gen_district <- reactive({
        school_info %>%
            dplyr::filter(district_name == district(),
                          year == comp_year()) %>%
            dplyr::select(total_in_district_fte) %>%
            as.data.frame()
    })

    staff_gen_data <- reactive({
        req(staff_gen_district())
        dplyr::bind_rows(staff_gen_comp(), staff_gen_district()) %>%
            t() %>%
            data.frame() %>%
            dplyr::rename(other = X1,
                   district = X2)
    })

    output$staff_general <-  plotly::renderPlotly(
        plotly::plot_ly(
        staff_gen_data(),
        x = ~ c('Total FTE'),
        y = ~ district,
        type = 'bar',
        name = district(),
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(district()  , ': %{y:,.1f}<extra></extra>'),
        texttemplate = '%{y:,.1f}',
        textposition = 'auto'
      ) %>%
        plotly::add_trace(y = ~ other,
                  name = 'Other',
                   hovertemplate = 'Other: %{y:,.1f}<extra></extra>',
                   texttemplate = '%{y:,.1f}',
                  marker = list(color = '#2a3a6e')) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('PK', 'K', purrr::map_chr(seq(12), function(x){paste('Grade', x)}))),
          yaxis = list(title = "", fixedrange = T,automargin = T),
          barmode = 'group',
          showlegend =F,
          font = list(size = 18),
          images = list(
            source = "https://raw.githubusercontent.com/SCasanova/arxed_ddd/main/www/D3%20Logo.png",
            x = 0.02, y = 1.08,
            sizex = 0.2, sizey = 0.2
        )
        )
    )

    staff_ratio_comp <- reactive({
        school_info %>%
            dplyr::filter(district_name %in% comp_districts(),
                          year == comp_year()) %>%
            dplyr::select(student_teacher_ratio) %>%
            as.data.frame() %>%
            dplyr::summarise(
                dplyr::across(
                    c(student_teacher_ratio),
                    ~ round(mean(.x, na.rm = TRUE))
                )
            )

    })


    staff_ratio_district <- reactive({
        school_info %>%
            dplyr::filter(district_name == district(),
                          year == comp_year()) %>%
            dplyr::select(student_teacher_ratio) %>%
            as.data.frame()
    })

    staff_ratio_data <- reactive({
        req(staff_ratio_district())
        dplyr::bind_rows(staff_ratio_comp(), staff_ratio_district()) %>%
            t() %>%
            data.frame() %>%
            dplyr::rename(other = X1,
                   district = X2)
    })

    output$staff_ratio <-  plotly::renderPlotly(
        plotly::plot_ly(
        staff_ratio_data(),
        x = ~ c('Student-Teacher Ratio'),
        y = ~ district,
        type = 'bar',
        name = district(),
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(district()  , ': %{y:,.0f}:1<extra></extra>'),
        texttemplate = '%{y:,.0f}:1',
        textposition = 'auto'
      ) %>%
        plotly::add_trace(y = ~ other,
                  name = 'Other',
                   hovertemplate = 'Other: %{y:,.0f}:1<extra></extra>',
                   texttemplate = '%{y:,.0f}:1',
                  marker = list(color = '#2a3a6e')) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T),
          yaxis = list(title = "", fixedrange = T,automargin = T),
          barmode = 'group',
          showlegend =F,
          font = list(size = 18)
        )
    )

    dist_gender_data <- reactive({
         staff_diversity %>%
            dplyr::filter(district_name == district(),
                          year == comp_year()) %>%
            dplyr::select(males_percent, females_percent) %>%
            t() %>%
            as.data.frame()
    })


    output$district_gender <- plotly::renderPlotly(
        plotly::plot_ly(
            dist_gender_data(),
            labels = ~ c('Male', 'Female'),
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            values = ~V1,
            hoverinfo = 'label+percent',
            text = ~paste0(V1, '%'),
             marker = list(colors = c("gray", "#DB9743"),
                      line = list(color = '#FFFFFF', width = 1))
            )%>%
            plotly::add_pie()%>%
            plotly::layout(
                title = district(),
                showlegend = F,
                xaxis = list(
                    showgrid = FALSE,
                    zeroline = FALSE,
                    showticklabels = FALSE
                ),
                yaxis = list(
                    showgrid = FALSE,
                    zeroline = FALSE,
                    showticklabels = FALSE
                )
            )
    )

    other_gender_data  <- reactive({
         staff_diversity %>%
            dplyr::filter(district_name %in% comp_districts(),
                          year == comp_year()) %>%
            dplyr::select(males_percent, females_percent) %>%
            dplyr::summarise(
                dplyr::across(
                    males_percent:females_percent,
                    ~ mean(.x, na.rm = TRUE)
                )
            ) %>%
            t() %>%
            as.data.frame()
    })


    output$other_gender <- plotly::renderPlotly(
        plotly::plot_ly(
            other_gender_data(),
            labels = ~ c('Male', 'Female'),
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            values = ~V1,
            hoverinfo = 'label+percent',
            text = ~paste0(V1, '%'),
             marker = list(colors = c("gray", "#2a3a6e"),
                      line = list(color = '#FFFFFF', width = 1))
            )%>%
            plotly::add_pie()%>%
            plotly::layout(
                title = "Other Districts",
                showlegend = F,
                xaxis = list(
                    showgrid = FALSE,
                    zeroline = FALSE,
                    showticklabels = FALSE
                ),
                yaxis = list(
                    showgrid = FALSE,
                    zeroline = FALSE,
                    showticklabels = FALSE
                )
            )
    )

    staff_diversity_comp <- reactive({
        staff_diversity %>%
            dplyr::filter(district_name %in% comp_districts(),
                          year == comp_year()) %>%
            dplyr::select(african_american_percent:multi_race_non_hispanic_percent) %>%
            as.data.frame() %>%
            dplyr::summarise(
                dplyr::across(
                    (african_american_percent:multi_race_non_hispanic_percent),
                    ~ mean(.x, na.rm = TRUE)
                )
            )

    })
    
    
    staff_diversity_district <- reactive({
        staff_diversity %>%
            dplyr::filter(district_name == district(),
                          year == comp_year()) %>%
            dplyr::select(african_american_percent:multi_race_non_hispanic_percent) %>%
            as.data.frame()
    })

    staff_diversity_data <- reactive({
        req(staff_diversity_district())
        dplyr::bind_rows(staff_diversity_comp(), staff_diversity_district()) %>%
            t() %>%
            data.frame() %>%
            dplyr::rename(other = X1,
                   district = X2)
    })

    output$staff_diversity <-  plotly::renderPlotly(
        plotly::plot_ly(
        staff_diversity_data(),
        x = ~ c('African American', 'Asian', 'Hispanic', 'White', 'Native American', 'Hawaiian/Pacific Islander', 'Multi-Race'),
        y = ~ district,
        type = 'bar',
        name = district(),
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(district()  , ': %{y}%<extra></extra>'),
        texttemplate = '%{y}%',
        textposition = 'outside'
      )  %>%
        plotly::layout(
          xaxis = list(title = "", tickangle = -25, fixedrange = T, categoryorder = "array", categoryarray = c('African American', 'Asian', 'Hispanic', 'White', 'Native American', 'Hawaiian/Pacific Islander', 'Multi-Race')),
          yaxis = list(title = "", fixedrange = T,automargin = T,range = c(0, max(staff_diversity_data(), na.rm = T)*1.15)),
          barmode = 'group',
          showlegend =F,
          font = list(size = 18),
          images = list(
            source = "https://raw.githubusercontent.com/SCasanova/arxed_ddd/main/www/D3%20Logo.png",
            x = 0.02, y = 1.1,
            sizex = 0.2, sizey = 0.2
        )
        )%>%
        plotly::add_trace(y = ~ other,
                  name = 'Other',
                   hovertemplate = 'Other: %{y:,.1f}%<extra></extra>',
                   texttemplate = '%{y:,.1f}%',
                  marker = list(color = '#2a3a6e'))
    )

    staff_pie_data <- reactive(staff_diversity_district() %>% t() %>% data.frame() %>%
                            janitor::clean_names())


    output$staff_diversity_pie <- plotly::renderPlotly(
        plotly::plot_ly(
            staff_pie_data(),
            labels = ~ c('African American', 'Asian', 'Hispanic', 'White', 'Native American', 'Hawaiian/Pacific Islander', 'Multi-Race'),
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            values = ~x,
            hoverinfo = 'label+percent',
            text = ~paste0(x, '%'),
             marker = list(colors = c('#DB9743', '#3F7CAC', '#2E282A', '#2a3a6e', '#F3EFF5', '#C03221', '#7DAA92'),
                      line = list(color = '#FFFFFF', width = 1))
            )%>%
            plotly::add_pie(hole = 0.4)%>%
            plotly::layout(
                title = district(),
                showlegend = F,
                xaxis = list(
                    showgrid = FALSE,
                    zeroline = FALSE,
                    showticklabels = FALSE
                ),
                yaxis = list(
                    showgrid = FALSE,
                    zeroline = FALSE,
                    showticklabels = FALSE
                )
            )
    ) 
    
    teacher_pct_df <- reactive({
      plot_comp_df() %>% dplyr::select(
        pct_teachers_licensed,
        pct_experienced_teachers,
        pct_teachers_without_waiver,
        pct_teaching_in_field
      ) %>%
        t() %>%
        data.frame() %>% 
        dplyr::rename(district = X1,
                   other = X2)
    })
    
    output$teacher_percents <-  plotly::renderPlotly(
        plotly::plot_ly(
        teacher_pct_df(),
        x = ~ c('% of Teachers Licensed', '% Experienced', '% of Teachers w/o Waiver', '% Teaching In-Field'),
        y = ~ district,
        type = 'bar',
        name = district(),
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(district()  , ': %{y:.1%}<extra></extra>'),
        texttemplate = '%{y:.0%}',
        textposition = 'inside'
      ) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('African American', 'Asian', 'Hispanic', 'White', 'Native American', 'Hawaiian/Pacific Islander', 'Multi-Race')),
          yaxis = list(title = "", fixedrange = T,automargin = T,range = c(0, max(teacher_pct_df(), na.rm = T)*1.15)),
          barmode = 'group',
          showlegend =F,
          font = list(size = 18),
          images = list(
            source = "https://raw.githubusercontent.com/SCasanova/arxed_ddd/main/www/D3%20Logo.png",
            x = 0.02, y = 1.1,
            sizex = 0.2, sizey = 0.2
        )
        ) %>% 
        plotly::add_trace(y = ~ other,
                  name = 'Other',
                   hovertemplate = 'Other: %{y:.1%}<extra></extra>',
                   texttemplate = '%{y:.0%}',
                  marker = list(color = '#2a3a6e')) 
    )
  
    

# Finance Tab -------------------------------------------------------------

    
    finance_data <- reactive({
        req(expenditures)
         expenditures %>%
            dplyr::filter(district == district(),
                          year == comp_year()) %>%
            dplyr::select(instructional_services, administration, pupil_services, operations_and_maintenance, insurance_retirement_programs_and_other) %>%
            t() %>%
            as.data.frame() %>%
        mutate(V1 = round(as.numeric(V1), 1))
    })


    output$finances <- plotly::renderPlotly(
        plotly::plot_ly(
            finance_data(),
            labels = ~ c('Instructional Services', 'Administration', 'Pupil Services', 'Operations and Maintenance', 'Insurance, Retirement and others'),
            textposition = 'inside',
            textinfo = 'label+text',
            insidetextfont = list(color = '#FFFFFF'),
            values = ~V1,
            hoverinfo = 'label+text+percent',
            text = ~ paste0("$", formatC(as.numeric(V1), format="f", digits=2, big.mark=",")),
             marker = list(colors =  c('#DB9743', '#3F7CAC', '#2a3a6e', '#2E282A', '#575a5e'),
                      line = list(color = '#FFFFFF', width = 1))
            )%>%
            add_pie(hole = 0.4)%>%
            layout(
                title = "Per-Pupil Expenditures Breakdown",
                showlegend = F,
                xaxis = list(
                    showgrid = FALSE,
                    zeroline = FALSE,
                    showticklabels = FALSE
                ),
                yaxis = list(
                    showgrid = FALSE,
                    zeroline = FALSE,
                    showticklabels = FALSE
                )
            )
    )
    
    plot_cola_df <- reactive({
        
        school_info %>%
            dplyr::filter(district_name %in% comp_districts() | district_name == district()) %>% 
            dplyr::mutate(is_district = ifelse(district_name == district(), district(), "Others")) %>%
            dplyr::group_by(is_district) %>%
            dplyr::summarize(
                cola_2015_16 = mean(cola_2015_16, na.rm = T),
                cola_2016_17 = mean(cola_2016_17, na.rm = T),
                cola_2017_18 = mean(cola_2017_18, na.rm = T),
                cola_2018_19 = mean(cola_2018_19, na.rm = T),
                cola_2019_20 = mean(cola_2019_20, na.rm = T),
                cola_2020_21 = mean(cola_2020_21, na.rm = T),
                cola_2021_22 = mean(cola_2021_22, na.rm = T),
                cola_2022_23 = mean(cola_2022_23, na.rm = T),
                cola_2023_24 = mean(cola_2023_24, na.rm = T)
            )
        
    })
    
    
     cola_plot_comp <- reactive({
       req(comp_districts())
        school_info %>%
            dplyr::filter(district_name %in% comp_districts(),
                          year == comp_year()) %>%
            dplyr::select(cola_2020_21:cola_2022_23) %>%
            as.data.frame() %>%
            dplyr::summarise(
                dplyr::across(
                    (cola_2020_21:cola_2022_23),
                    ~ mean(.x, na.rm = TRUE)
                )
            )

    })
    
    
    cola_plot_district <- reactive({
      req(district())
        school_info %>%
            dplyr::filter(district_name == district(),
                          year == comp_year()) %>%
            dplyr::select(cola_2020_21:cola_2022_23) %>%
            as.data.frame()
    })

    cola_plot_data <- reactive({
        req(cola_plot_district())
        dplyr::bind_rows(cola_plot_comp(), cola_plot_district()) %>%
            t() %>%
            data.frame() %>%
            dplyr::rename(other = X1,
                   district = X2) %>% 
          dplyr::mutate(district = district*100,
                        other = other*100) %>% 
          dplyr::arrange(desc(rownames(.)))
    })
    

    output$cola_plot_comp <- plotly::renderPlotly({
      plotly::plot_ly(
        cola_plot_data(),
        x = ~other,
        y = ~ c('2022-23', '2021-22', '2020-21'),
        type = 'bar',
        name = 'A',
        orientation = 'h',
        marker = list(color = '#2a3a6e'),
        hovertemplate = '%{x:,.2f}%<extra></extra>',
        texttemplate = '%{x:.1f}%',
        textposition = 'inside'
      ) %>%
         plotly:: config(displayModeBar = FALSE) %>% 
        plotly::add_trace(x = ~district,
                  name = 'Other',
                  marker = list(color = '#DB9743')) %>%
        plotly::layout(
          xaxis = list(title = "COLA %", fixedrange = T),
          yaxis = list(title = "", fixedrange = T),
          margin = list(b = 100),
          barmode = 'group',
          showlegend =F,
          font = list(size = 18),
          images = list(
            source = "https://raw.githubusercontent.com/SCasanova/arxed_ddd/main/www/D3%20Logo.png",
            x = -0.03, y = -0.1,
            sizex = 0.3, sizey = 0.2
        )
        )
  })
    

# Work Cond.  -------------------------------------------------------------


    
  days_wo_df <- reactive({
      plot_comp_df() %>% dplyr::select(
        num_days_with_students:num_days_without_students_nurses
      ) %>%
        t() %>%
        data.frame() %>% 
        dplyr::rename(district = X1,
                   other = X2)
    })
    

  output$days_w_wo <-  plotly::renderPlotly(
        plotly::plot_ly(
        days_wo_df(),
        x = ~ c('Teachers: # of days with students', 'Teachers: # of days w/o students', 'Guidance: # of days w/o students', 'Nurses: # of days w/o students'),
        y = ~ district,
        type = 'bar',
        name = district(),
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(district()  , ': %{y:.0f}<extra></extra>'),
        texttemplate = '%{y:.0f}',
        textposition = 'outside'
      ) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('African American', 'Asian', 'Hispanic', 'White', 'Native American', 'Hawaiian/Pacific Islander', 'Multi-Race')),
          yaxis = list(title = "", fixedrange = T,automargin = T,range = c(0, max(days_wo_df(), na.rm = T)*1.15)),
          barmode = 'group',
          showlegend =F,
          font = list(size = 18),
          images = list(
            source = "https://raw.githubusercontent.com/SCasanova/arxed_ddd/main/www/D3%20Logo.png",
            x = 0.99, y = 1.12,
            sizex = 0.2, sizey = 0.2
        )
        ) %>% 
        plotly::add_trace(y = ~ other,
                  name = 'Other',
                   hovertemplate = 'Other: %{y:.1f}<extra></extra>',
                   texttemplate = '%{y:.0f}',
                  marker = list(color = '#2a3a6e')) 
    )
  
  health_df <- reactive({
      plot_comp_df() %>% dplyr::select(
        pct_health_insurance_paid,
        nurses_unionized,
        sick_days_buy_back,
        sick_leave_bank
      ) %>%
        t() %>%
        data.frame() %>% 
        dplyr::rename(district = X1,
                   other = X2)
    })
    

  output$health_percents <-  plotly::renderPlotly(
        plotly::plot_ly(
        health_df(),
        x = ~ c('% Health Insurance Paid', '% Nurses Unionized', 'Sick Days Buy Back %', 'Sick Leave Bank'),
        y = ~ district,
        type = 'bar',
        name = district(),
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(district()  , ': %{y:.1%}<extra></extra>'),
        texttemplate = '%{y:.0%}',
        textposition = 'inside'
      ) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('African American', 'Asian', 'Hispanic', 'White', 'Native American', 'Hawaiian/Pacific Islander', 'Multi-Race')),
          yaxis = list(title = "", fixedrange = T,automargin = T,range = c(0, max(health_df(), na.rm = T)*1.15)),
          barmode = 'group',
          showlegend =F,
          font = list(size = 18),
          images = list(
            source = "https://raw.githubusercontent.com/SCasanova/arxed_ddd/main/www/D3%20Logo.png",
            x = 0.02, y = 1.1,
            sizex = 0.2, sizey = 0.2
        )
        ) %>% 
        plotly::add_trace(y = ~ other,
                  name = 'Other',
                   hovertemplate = 'Other: %{y:.1%}<extra></extra>',
                   texttemplate = '%{y:.0%}',
                  marker = list(color = '#2a3a6e')) 
    )
  
  sick_bank_df <- reactive({
      plot_comp_df() %>% dplyr::select(
        sick_days,sick_days_buy_back_rate,sick_leave_bank_cap
      ) %>%
        t() %>%
        data.frame() %>% 
        dplyr::rename(district = X1,
                   other = X2)
    })
    

  output$sick_bank <-  plotly::renderPlotly(
        plotly::plot_ly(
        sick_bank_df(),
        x = ~ c('Sick Days', 'Buy Back', 'Leave Bank'),
        y = ~ district,
        type = 'bar',
        name = district(),
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(district()  , ': %{y:.1f}<extra></extra>'),
        texttemplate = '%{y:.0f}',
        textposition = 'auto'
      ) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('African American', 'Asian', 'Hispanic', 'White', 'Native American', 'Hawaiian/Pacific Islander', 'Multi-Race')),
          yaxis = list(title = "", fixedrange = T,automargin = T,range = c(0, max(sick_bank_df(), na.rm = T)*1.15)),
          barmode = 'group',
          showlegend =F,
          font = list(size = 18),
          images = list(
            source = "https://raw.githubusercontent.com/SCasanova/arxed_ddd/main/www/D3%20Logo.png",
            x = 0.02, y = 1.1,
            sizex = 0.2, sizey = 0.2
        )
        ) %>% 
        plotly::add_trace(y = ~ other,
                  name = 'Other',
                   hovertemplate = 'Other: %{y:.1f}<extra></extra>',
                   texttemplate = '%{y:.0f}',
                  marker = list(color = '#2a3a6e')) 
    )
     
     
     
 }
 
 onStop( #close DB connection
  function()
  {
    RMySQL::dbDisconnect(myDB)
  }
)

 

shinyApp(ui = ui, server = server)
 
 
 
 
 
 
 
