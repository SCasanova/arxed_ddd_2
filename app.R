
# Dependencies ------------------------------------------------------------

library(magrittr)
library(shiny)
options(encoding = "UTF-8")


# Connect to database -----------------------------------------------

sqlQuery <- function (query) {
  # creating DB connection object with RMysql package
  DB <- DBI::dbConnect(RMySQL::MySQL(), dbname = "sys", user = 'admin', password = 'ArxEd01742!',host = 'arxed-sal.cnlnwcegporn.us-east-1.rds.amazonaws.com', port = 8209)
  # close db connection after function call exits
  on.exit(DBI::dbDisconnect(DB))
  # send Query to btain result set
  rs <- RMySQL::dbSendQuery(DB, query)
  # get elements from result sets and convert to dataframe
  result <- RMySQL::fetch(rs, -1)
  # return the dataframe
  result
}

# Credential manager ------------------------------------------------------

 check_creds <- function(dbname, host, port, db_user, db_password) {

  function(user, password) {
    
    con <- DBI::dbConnect(RMySQL::MySQL(), #Connect to DB
                          dbname = dbname, 
                          user = db_user, 
                          password = db_password,
                          host = host, 
                          port = port)
    
    on.exit(DBI::dbDisconnect(con))
    
    
    res <- DBI::fetch(DBI::dbSendQuery(con, glue::glue_sql("SELECT * 
                            FROM users 
                            WHERE user = {user} 
                            AND password = {password}
                            ", user = user, password = password, .con = con))) #Get result for inputed user
    
    if (nrow(res) > 0) {
      list(result = TRUE, user_info = list(user = user, something = 123))
    } else {
      list(result = FALSE)
    }
  }
}


# Data --------------------------------------------------------------------
overview_statistics <- c( #Full name for overview table stats
    "Total Enrollment",
    "Total Teacher FTE",
    "% Teachers Licensed",
    "Student:Teacher Ratio",
    "% Teachers Experienced",
    "% Teachers w/o Waiver",
    "% Teachers Teaching in Field",
    "Total Core Academic Classes",
    "% of Classes Taught by Highly Qualified Teachers",
    "Total Teacher Salaries",
    "Average Salary",
    "In-District Expenditure",
    "In-District Pupil FTE",
    "In-District Expenditure per Pupil",
    "Total Expenditure",
    "Total Pupil FTE",
    "Total Expenditure per Pupil"
)

palette <- c("#2a3a6e", "#DB9743")

## Create vector of major stat categories for dropdown menus
stat_categories <- c( 
    "Enrollment",
    "Teachers/Classes",
    "Salaries/Expenditure",
    "10th Grade MCAS",
    "SAT/AP",
    "Basic Contract",
    "Working Conditions",
    "Student Population"
)

statistic_names_full <- c( #Vector for sub-level stats
    "District", # 1
    "District Code",
    "1st Year",
    "Year",
    "2nd Year",
    "PK Enrollment",
    "K Enrollment",
    "G1 Enrollment",
    "G2 Enrollment",
    "G3 Enrollment", # 10
    "G4 Enrollment",
    "G5 Enrollment",
    "G6 Enrollment",
    "G7 Enrollment",
    "G8 Enrollment",
    "G9 Enrollment",
    "G10 Enrollment",
    "G11 Enrollment",
    "G12 Enrollment",
    "SP Enrollment", # 20
    "Total Enrollment",
    "Total Teacher FTE",
    "% Teachers Licensed",
    "Student:Teacher Ratio",
    "% Teachers Experienced",
    "% Teachers w/o Waiver",
    "% Teachers Teaching in Field",
    "Total Core Academic Classes",
    "% of Classes Taught by Highly Qualified Teachers",
    "Total Teacher Salaries", # 30
    "Average Salary",
    "In-District Expenditure",
    "In-District Pupil FTE",
    "In-District Expenditure per Pupil",
    "Total Expenditure",
    "Total Pupil FTE",
    "Total Expenditure per Pupil",
    "Students Advanced/Proficient in Science MCAS",
    "% Students Advanced/Proficient in Science MCAS",
    "Students Advanced in Science MCAS", # 40
    "% Students Advanced in Science MCAS",
    "Students Proficient in Science MCAS",
    "% Students Proficient in Science MCAS",
    "Students Need Imp Science MCAS",
    "% Students Need Imp Science MCAS",
    "Students Warning/Failing Science MCAS",
    "% Students Warning/Failing Science MCAS",
    "Students Taking Science MCAS",
    "Students Advanced/Proficient in ELA MCAS",
    "% Students Advanced/Proficient in ELA MCAS", # 50
    "Students Advanced in ELA MCAS",
    "% Students Advanced in ELA MCAS",
    "Students Proficient in ELA MCAS",
    "% Students Proficient in ELA MCAS",
    "Students Need Imp ELA MCAS",
    "% Students Need Imp ELA MCAS",
    "Students Warning/Failing ELA MCAS",
    "% Students Warning/Failing ELA MCAS",
    "Students Taking ELA MCAS", # 60
    "Students Advanced/Proficient in Math MCAS",
    "% Students Advanced/Proficient in Math MCAS",
    "Students Advanced in Math MCAS",
    "% Students Advanced in Math MCAS",
    "Students Proficient in Math MCAS",
    "% Students Proficient in Math MCAS",
    "Students Need Imp Math MCAS",
    "% Students Need Imp Math MCAS",
    "Students Warning/Failing Math MCAS",
    "% Students Warning/Failing Math MCAS",
    "Students Taking Math MCAS", # 70
    "SAT Tests Taken",
    "SAT Reading/Writing Score",
    "SAT Math Score",
    "AP Tests Taken",
    "AP 1s",
    "AP 2s",
    "AP 3s",
    "AP 4s",
    "AP 5s",
    "AP 1-2 %", # 80
    "AP 3-5 %",
    "Contract Length",
    "Contract Y1",
    "Contract Y2",
    "Contract Y3",
    "2015-16 COLA",
    "2016-17 COLA",
    "2017-18 COLA",
    "2018-19 COLA",
    "2019-20 COLA", # 90
    "2020-21 COLA",
    "2021-22 COLA",
    "2022-23 COLA",
    "2023-24 COLA",
    "2015-16 Upper-Left Salary",
    "2016-17 Upper-Left Salary",
    "2017-18 Upper-Left Salary",
    "2018-19 Upper-Left Salary",
    "2019-20 Upper-Left Salary",
    "2020-21 Upper-Left Salary", # 100
    "2021-22 Upper-Left Salary",
    "2022-23 Upper-Left Salary",
    "2023-24 Upper-Left Salary",
    "2015-16 Lower-Right Salary",
    "2016-17 Lower-Right Salary",
    "2017-18 Lower-Right Salary",
    "2018-19 Lower-Right Salary",
    "2019-20 Lower-Right Salary",
    "2020-21 Lower-Right Salary",
    "2021-22 Lower-Right Salary", # 110
    "2022-23 Lower-Right Salary",
    "2023-24 Lower-Right Salary",
    "2015-16 Average Salary",
    "2016-17 Average Salary",
    "2017-18 Average Salary",
    "2018-19 Average Salary",
    "2019-20 Average Salary",
    "2020-21 Average Salary",
    "2021-22 Average Salary",
    "2022-23 Average Salary", # 120
    "2023-24 Average Salary",
    "Contract Steps",
    "Contract Lanes",
    "Average Step % Increase",
    "Last Step % Increase",
    "Average Lane % Increase",
    "Bachelors Masters % Increase",
    "Masters Doctorate % Increase",
    "Longevity Start Year",
    "Longevity End Year", # 130
    "Longevity Min Pay",
    "Longevity Max Pay",
    "# Days with Students",
    "# Days w/o Students Teachers",
    "# Days w/o Students Guidance",
    "# Days w/o Students Nurses",
    "Nurses Unionized",
    "% Health Insurance Paid",
    "Personal Days Annually",
    "Personal Days Carry Over", # 140
    "Personal Days Carry Over Max",
    "Unused Personal Days Paid for",
    "Unused Personal Days Paid for Rate",
    "Sick Days",
    "Sick Days Buy Back",
    "Sick Days Buy Back Rate",
    "Sick Leave Bank",
    "Sick Leave Bank Cap",
    "Bereavement Days",
    "Religious Days", # 150
    "Other Days",
    "Comp for Attendance",
    "Guidance: Max Ratio (High School)",
    "Elementary: Max Class Size",
    "High School: Max Class Size",
    "Elementary: Special Ed Max Class Size",
    "High School: Special Ed Max Class Size",
    "Teacher’s Children Can Attend",
    "Elementary: Length of Workday",
    "High School: Length of Workday", # 160
    "Early Retirement Incentives",
    "Unit B",
    "Tuition Reimbursement",
    "Students 1st Language not English",
    "% Students 1st Language not English",
    "English Learners",
    "% Students English Learners",
    "Students w/ Disabilities",
    "% Students w/ Disabilities",
    "High-Need Students", # 170
    "% Students High-Need",
    "Low-Income Students",
    "% Students Low-Income",
    "League",
    "City",
    "County",
    "Curn Intake/Enrollment",
    '% Churn',
    "% Intake",
    "Stability Enrollment",
    "% Stability",
    "Total # Of Classes",
    "Average Class Size",
    "# Of Students",
    "% African American Students",
    "% Asian Students",
    "% Hispanic Students",
    "% White Students",
    "% Native American Students",
    "% Hawaiian/Pacific Islander Students",
    "% Multi-Race Non-Hispanic Students",
    "% African American Staff",
    "% Asian Staff",
    "% Hispanic Staff",
    "% White Staff",
    "% Native American Staff",
    "% Hawaiian/Pacific Islander Staff",
    "% Multi-Race Non-Hispanic Staff",
    "% Female Staff",
    "% Male Staff",
    "Total FTE",
    "# First Language Not English",
    "% First Language Not English",
    "# English language Learners",
    "% English language Learners",
    "# Students With Disabilities",
    "% Students With Disabilities",
    "# Low Income",
    "% Low Income",
    "# Students With Free Lunch",
    "% Students With Free Lunch",
    "# Students With Reduced Lunch",
    "% Students With Reduced Lunch",
    "# of High Needs Students",
    "% of High Needs Students",
    "# Economically Disadvantaged Students",
    "% Economically Disadvantaged Students",
    "In-district FTE pupils",
    "Out Of District FTE pupils",
    'Total FTE Pupils',
    "Administration Spending",
    "Instructional Leadership Spending",
    "Teachers Spending",
    "Teaching Services Spending",
    "Proffesional Developement Spending",
    "Materials/Equipment Spending",
    "Guidance and Counceling Spending",
    "Pupil Services Spending",
    "Operations and Maintenance Spending",
    "Insurance and Retirement Programs Spending",
    "Total In-district Spending",
    "Instructional Services Spending",
    "Total Expenditures Spending",
    "DART Name"
)
 
 color_palette <- c("darkred", "white", "darkgreen")
 
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
                columns = gt::everything(),
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
 
school_info <- readr::read_csv("school_info.csv") 

axis_options <- purrr::set_names(colnames(school_info), statistic_names_full)
        
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

district_options <- school_info %>% 
  dplyr::pull(district_name) %>% 
  unique()
 

# UI ----------------------------------------------------------------------

 
ui <- shinyMobile::f7Page(
    title = paste0("ArxEd | D³ Data-Driven Decisions"),
    options = list(dark = F),
    shinyjs::useShinyjs(),
    tags$script(src = "myscript.js"),
    shinyMobile::f7SingleLayout(
        tags$head(
          tags$link(rel="shortcut icon", href="https://raw.githubusercontent.com/SCasanova/arxed_ddd/main/www/Shield%20Trim.png"),
          tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet_arx2.css"),
          tags$script(src = 'https://kit.fontawesome.com/8c5f048e9f.js',crossorigin="anonymous" )#For CSS icons
          ),
        navbar = shinyMobile::f7Navbar( title = tags$div(tags$img(src='https://raw.githubusercontent.com/SCasanova/arxed_ddd/main/www/Shield%20Trim.png',width='45px'),
                                                          tags$span(paste0("  ArxEd | D³ Data Driven Decisions"), style = 'text-align:center;')), 
                                        tags$div(htmlOutput('logo_src'), class = 'top-district-logo')),
        tags$div(shinyMobile::f7Fab( #Adds clickable text to change password
         inputId = 'pass',
         label = 'Change Password'
         ), class =  'pass-button'),
        tags$div(class = 'password-popup', shinyMobile::f7Popup( #displays the popup to change password
         id = "popup1",
         title = tags$div(class = 'pass-title', h2("Change Password")),
         tags$div(
           shinyMobile::f7Text("newPass", "New Password", ""),
           shinyMobile::f7Text("newPassConf", "Confirm New Password", ""),
           br(),
           shinyMobile::f7Button('submit_pass', 'Submit', size = 'large')
           ),
         
      )),
      tags$div(class = 'select-card', #Includes all input modules
               shinyMobile::f7Shadow(
                   hover = T,
                   intensity = 16,
                   shinyMobile::f7Card(
                     tags$div( id = 'menu-card',
                       tags$div(
                       style = 'display:flex',
                       tags$div(
                           tags$div(class = 'select-menu',
                             htmlOutput('district_select')
                           ),
                           tags$div(class = 'select-menu',
                               shinyMobile::f7Select(
                                   inputId = 'year',
                                   label = tags$span(class = 'input-label', h3('2. School Year:')),
                                   choices = c('2020-21', '2021-22', '2019-20', '2018-19', '2017-18', '2016-17', '2015-16'),
                                   selected = "2020-21"
                                   
                               )
                           ),
                           class = 'inputs input-1'
                       ),
                       tags$div(
                           class = 'inputs',
                           shinyMobile::f7CheckboxGroup(
                               inputId = 'comp_cond',
                               label = h5('3. Comparison School Districts:'),
                               choices = c('DART', 'League', 'County', 'Vocational', 'Charter', 'Regional', 'My Schools')
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
                     selectizeInput("district_comps",
                                            label = tags$span(class = 'input-label', h3('5. View your comparison districts and add/remove any manually:')),
                                            choices = district_options, multiple = TRUE, width = '100%')
                   )),
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
                               tags$button(
                                 id=  'reset',
                                 type = 'button',
                                 class = 'button f7-action-button button-fill',
                                 onclick = 'uncheck("comp_cond")',
                                 'Reset Districts'
                               ),class = 'card-button',
                             ),
                             tags$div(
                               tags$button(
                                 id=  'toggle_menu',
                                 type = 'button',
                                 class = 'button f7-action-button button-fill',
                                 'Toggle Menu Card'
                               ),class = 'card-button',
                             )
                             )
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
               tags$div(
                 shinyMobile::f7Button(
                   inputId = 'down_home',
                   label = tags$span(tags$i(class = 'fas fa-camera'), tags$span('Download as Image')) #fas fa classes corresponf to fontawesome
                 ),
           class = "other-button card-button2"),
               tags$div(style = 'display:flex;',
                 tags$div(class = 'side-panel',
                          style = 'padding:10px;
                                  margin-top:10px',
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
                       tags$div(htmlOutput('comp_2'),style = 'margin-bottom:20px;'),
                       br(),
                       plotly::plotlyOutput('cola_plot_all', height = 'auto'),
                        plotly::plotlyOutput('upper_left', height = 'auto'),
                       plotly::plotlyOutput('lower_right', height = 'auto'),
                        plotly::plotlyOutput('average_salary_yearly', height = 'auto')
                     )
                   )
                 ),
                 tags$div(style = 'width:75%;', id = 'home-plots',
                          shinyMobile::f7Shadow(
                            hover = T,
                            intensity = 16,
                            shinyMobile::f7Card(
                              tags$div(plotly::plotlyOutput('total_budget', height = 'auto')),
                              tags$div(style = 'display:flex;',
                                       tags$div(class = 'grid-block',
                                                plotly::plotlyOutput('per_pupil_exp', height = 'auto')
                                       ),
                                       tags$div(class = 'grid-block',
                                                plotly::plotlyOutput('average_salary', height = 'auto')
                                       ),
                                       tags$div(class = 'grid-block',
                                                plotly::plotlyOutput('total_enrollment', height = 'auto')
                                       )
                              ),
                              tags$div(style = 'display:flex;',
                                       tags$div(class = 'grid-block',
                                                plotly::plotlyOutput('sat_performance', height = 'auto')
                                       ),
                                       tags$div(class = 'grid-block',
                                                plotly::plotlyOutput('advanced_placement', height = 'auto')
                                       ),
                                       tags$div(class = 'grid-block',
                                                plotly::plotlyOutput('total_teachers', height = 'auto')
                                       )
                              ),
                              tags$div(style = 'display:flex;',
                                       tags$div(style = 'width:75%',
                                                plotly::plotlyOutput('mcas', height = 'auto')
                                       ),
                                       tags$div(class = 'grid-block',
                                                plotly::plotlyOutput('student_demographics', height = 'auto')
                                       )
                              )
                              )
                            )
                 )
               )
          ),
          shinyMobile::f7Tab(
               tabName = "Overview",
                icon = shinyMobile::f7Icon("search"),
                active = F,
                tags$div(style = 'display:flex',
                 tags$div(style = 'width:20%;',
                   shinyMobile::f7Shadow(
                     hover = T,
                     intensity = 16,
                     shinyMobile::f7Card(
                       tags$div(
                         style = 'display:flex;',
                         tags$div(
                           shinyMobile::f7Button(
                             inputId = 'down_overview',
                             label = tags$span(tags$i(class = 'fas fa-camera'), tags$span('Image')) #fas fa classes corresponf to fontawesome
                           ),
                           class = "side-button"
                         ),
                         tags$div(
                      shinyMobile::f7DownloadButton( #various download functionalities
                         outputId = 'down_overview_csv',
                         label = 'CSV'
                      ), class = "side-button"
                      )
                       ),
                       shinyMobile::f7Radio(
                         'individuals',
                         label = tags$span(style = 'font-size:250%', "Show individual comparison districts?"), 
                         choices = c("No","Yes"),
                         selected = 'No'
                       )
                       
                     )
                   )
                 ),
                 tags$div(style = 'width:80%;', id = 'overview_table',
                          shinyMobile::f7Shadow(
                            hover = T,
                            intensity = 16,
                            shinyMobile::f7Card(
                              gt::gt_output('comparisons_table')
                            )
                          )
                 )
                )
          ),
          shinyMobile::f7Tab(
               tabName = "Students",
                icon = shinyMobile::f7Icon("person_3"),
                active = F,
                tags$div(
                   shinyMobile::f7Button(
                     inputId = 'down_students',
                     label = tags$span(tags$i(class = 'fas fa-camera'), tags$span('Download as Image')) #fas fa classes corresponf to fontawesome
                   ),
                   class = "other-button card-button2"
                 ),
               tags$div(id = 'students',shinyMobile::f7Shadow(
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
                         tags$div(style = 'width:65%',
                                  plotly::plotlyOutput('student_diversity')),
                         tags$div(style = 'width:35%',
                                  plotly::plotlyOutput('student_diversity_pie', width = '100%'))
                       ))
               ), 
               shinyMobile::f7Shadow(
                 hover = T,
                 intensity = 16,
                 shinyMobile::f7Card(
                   tags$div(plotly::plotlyOutput('student_mobility'),
                                                 htmlOutput('mobility_info'))
                   
                 )
               ))
          ),
          shinyMobile::f7Tab(
               tabName = "Staff",
                icon = shinyMobile::f7Icon("person_2"),
                active = F,
               tags$div(
                   shinyMobile::f7Button(
                     inputId = 'down_staff',
                     label = tags$span(tags$i(class = 'fas fa-camera'), tags$span('Download as Image')) #fas fa classes corresponf to fontawesome
                   ),
                   class = "other-button card-button2"
                 ),
               tags$div(id = 'staff',shinyMobile::f7Shadow(
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
               shinyMobile::f7Shadow(hover = T,
                       intensity = 16,
                       shinyMobile::f7Card(tags$div(
                         style = 'display:flex',
                         tags$div(style = 'width:65%',
                                  plotly::plotlyOutput('staff_diversity')),
                         tags$div(style = 'width:35%',
                                  plotly::plotlyOutput('staff_diversity_pie', width = '100%'))
                       ))), 
               shinyMobile::f7Shadow(
                     hover = T,
                     intensity = 16,
                     shinyMobile::f7Card(
                       plotly::plotlyOutput('teacher_percents')
                     )
               ))
          ),
          shinyMobile::f7Tab(
               tabName = "Finances",
                icon = shinyMobile::f7Icon("money_dollar"),
                active = F,
               tags$div(
                   shinyMobile::f7Button(
                     inputId = 'down_finances',
                     label = tags$span(tags$i(class = 'fas fa-camera'), tags$span('Download as Image')) #fas fa classes corresponf to fontawesome
                   ),
                   class = "other-button card-button2"
                 ),
               tags$div(id = 'finances', shinyMobile::f7Shadow(
                 hover = T,
                 intensity = 16,
                 shinyMobile::f7Card(
                   tags$div(style = "display:flex;",
                        tags$div(class = 'inputs',
                          plotly::plotlyOutput('finances_plot')),
                        tags$div(class = 'inputs',
                          plotly::plotlyOutput('cola_plot_comp'))
                        )
                 )
               ))
          ),
          shinyMobile::f7Tab(
               tabName = "Academic",
                icon = shinyMobile::f7Icon("book"),
                active = F,
               tags$div(
                   shinyMobile::f7Button(
                     inputId = 'down_academic',
                     label = tags$span(tags$i(class = 'fas fa-camera'), tags$span('Download as Image')) #fas fa classes corresponf to fontawesome
                   ),
                   class = "other-button card-button2"
                 ),
               tags$div(id = 'academic', shinyMobile::f7Shadow(
                 hover = T,
                 intensity = 16,
                 shinyMobile::f7Card(
                   tags$div(style = "display:flex;",
                        tags$div(class = 'inputs',
                          plotly::plotlyOutput('ap_3_5')),
                        tags$div(class = 'inputs',
                          plotly::plotlyOutput('ap_taken'))
                        )
                 )
               ),
               shinyMobile::f7Shadow(
                 hover = T,
                 intensity = 16,
                 shinyMobile::f7Card(
                   tags$div(style = "display:flex;",
                        tags$div(class = 'inputs',
                          plotly::plotlyOutput('sat_results')),
                        tags$div(class = 'inputs',
                          plotly::plotlyOutput('sat_taken'))
                        )
                 )
               ),
               shinyMobile::f7Shadow(
                 hover = T,
                 intensity = 16,
                 shinyMobile::f7Card(
                   tags$div(style = "display:flex;",
                        tags$div(class = 'inputs',
                          plotly::plotlyOutput('mcas_ap')),
                        tags$div(class = 'inputs',
                          plotly::plotlyOutput('mcas_wf'))
                        )
                 )
               )
               )
          ),
          shinyMobile::f7Tab(
               tabName = "Work Cond.",
                icon = shinyMobile::f7Icon("briefcase"),
                active = F,
               tags$div(
                   shinyMobile::f7Button(
                     inputId = 'down_work',
                     label = tags$span(tags$i(class = 'fas fa-camera'), tags$span('Download as Image')) #fas fa classes corresponf to fontawesome
                   ),
                   class = "other-button card-button2"
                 ),
               tags$div(id = 'work',shinyMobile::f7Shadow(
                     hover = T,
                     intensity = 16,
                     shinyMobile::f7Card(
                       tags$div(style = 'display:flex;',
                                tags$div(style=  'width:33%',
                                  plotly::plotlyOutput('days_with')
                                ),
                                tags$div(style=  'width:66%',
                                   plotly::plotlyOutput('days_w_wo')
                                )
                       )
                     )
               ),
               shinyMobile::f7Shadow(
                     hover = T,
                     intensity = 16,
                     shinyMobile::f7Card(
                       tags$div(
                     style = 'display:flex',
                     tags$div(
                       style = 'width:33%',
                       plotly::plotlyOutput('nurses_union_plot'),
                       htmlOutput('district_union_output')
                     ),
                     tags$div(
                       style = 'width:33%',
                       plotly::plotlyOutput('buy_back_binary')
                     ),
                     tags$div(
                       style = 'width:33%',
                       plotly::plotlyOutput('sick_bank_binary')
                     )
                   )
                     )
               ),
               shinyMobile::f7Shadow(
                 hover = T,
                 intensity = 16,
                 shinyMobile::f7Card(
                    tags$div(
                     style = 'display:flex',
                     tags$div(
                       style = 'width:60%',
                       plotly::plotlyOutput('sick_bank')
                     ),
                     tags$div(
                       style = 'width:40%',
                       plotly::plotlyOutput('health_percent')
                     )
                   
                 )
                 )
               )
               )),
          shinyMobile::f7Tab(
            tabName = "Plots",
            icon = shinyMobile::f7Icon("chart_bar"),
            active = F,
            tags$div(
              shinyMobile::f7Button(
                inputId = 'down_plot',
                label = tags$span(tags$i(class = 'fas fa-camera'), tags$span('Download as Image')) #fas fa classes corresponf to fontawesome
              ),
              class = "other-button card-button2"
            ),
            tags$div(
              style = 'display:flex',
              tags$div(
                style = 'width:20%;',
                shinyMobile::f7Shadow(
                  hover = T,
                  intensity = 16,
                  shinyMobile::f7Card(
                    shinyMobile::f7Select(
                      'plot_type',
                      label = h2("Plot Type"),
                      choices = c("Bar", "Scatter", "Pie")
                    ),
                    uiOutput('y_cat_disp'),
                    uiOutput('y_var_disp'),
                    uiOutput('x_cat_disp'),
                    uiOutput('x_var_disp')
                  )
                )
              ),
              tags$div(
                style = 'width:80%;',
                id = 'plot_card',
                shinyMobile::f7Shadow(
                  hover = T,
                  intensity = 16,
                  shinyMobile::f7Card(plotly::plotlyOutput('userplot', height = 'auto'))
                )
                
              )
            )
          )
      )
    )
)

ui <- shinymanager::secure_app(ui)

# Server ------------------------------------------------------------------

 

 server <- function(input, output, session) {
   
   #Info credential validation 

    res_auth <- shinymanager::secure_server(
    check_credentials = check_creds(
      dbname = "sys",
      host = "arxed-sal.cnlnwcegporn.us-east-1.rds.amazonaws.com",
      port = 8209,
      db_user = "admin",
      db_password = "ArxEd01742!"
    )
    )

  auth_output <- reactive({
    reactiveValuesToList(res_auth)
  })

  active_user <- reactive(stringr::str_remove_all(auth_output()$user, "[\r\n]"))
  
  #get distrtict associated with user from DB
  query_district <- reactive({paste0("SELECT district_name
                   FROM districts
                   WHERE user =",
                 "'",active_user(),"';")})
  
  
  #limit options to user district(s) or all districts for administrators
   personal_district_options <- reactive({
     if(active_user() %in% c('Mike', 'Brian', 'Santiago')){
       district_options
     } else{
        sqlQuery(query_district()) %>% dplyr::pull(district_name)
     }
   })
   
   #render the selector with user districts
   output$district_select <- renderUI({
     shinyMobile::f7Select(
       inputId = 'district',
       label = tags$span(class = 'input-label', h3('1. My School District')),
       choices = personal_district_options()
     )
   })
     
     district <- reactive(input$district)
     comp_year <- reactive(input$year)
     
     output$main_logo <- renderUI({
        tags$img(src = "https://raw.githubusercontent.com/SCasanova/arxed_ddd/main/www/D3%20Logo.png",
            width = '180px',
             alt = "")
    })
     
# Change password logic
  
  observeEvent(input$pass, { #Observe for submit button. If passwords match, update on server. If not, notify user
     shinyMobile::updateF7Popup(id = "popup1")
    })
  
  observeEvent(input$submit_pass, {
    if(input$newPassConf == input$newPass){
      
      query <- glue::glue("UPDATE users
                           SET password = '{password}'
                           WHERE user = '{user}';
                           ", user = active_user(), password = input$newPass)
      sqlQuery(query)
      
      shinyMobile::f7Notif(
            text = "Your password has succesfully changed",
            icon = shinyMobile::f7Icon("checkmark_alt_circle"),
            title = "Password Updated",
            titleRightText = "now",
            closeTimeout = 5500,
            closeButton = F
      )

    }else {
      shinyMobile::f7Notif(
            text = "Passwords don't match. Please try again",
            icon = shinyMobile::f7Icon("xmark_circle"),
            title = "Update Error",
            titleRightText = "now",
            closeTimeout = 5500,
            closeButton = T
      )
    }
     
    })

     
  #Get league / county / logo etc for selected district
     league <- reactive({
       req(district())
      query <- paste0("SELECT league  
                        FROM district_info 
                        WHERE district_name =", "'",  district(),"';")
         
      sqlQuery(query) %>% 
        unique() %>% 
        as.character()
  })
     county <- reactive({
       req(district())
      query <- paste0("SELECT county  
                        FROM district_info 
                        WHERE district_name =", "'",  district(),"';")
      sqlQuery(query) %>% 
        unique() %>% 
        as.character()
  })
     
     district_logo <- reactive({ #access district table to get name and logo associated with district
       req(district())
       query <- paste0("SELECT logo  
                        FROM districts 
                        WHERE district_name =", "'",  district(),"';")
      
       sqlQuery(query) %>% 
         unique() %>% 
            as.character()
       
  })

  
  output$logo_src <- renderUI({
    req(district())
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
    
  })
  
  observeEvent(input$toggle_menu, { #Toggles input card (excluding footer)
        shinyjs::toggle(id = "menu-card")
    })
  
  #Next four modules are conditional panels for filters
  
  output$budget_condition <- renderUI({
    if('Total Budget' %in% input$comp_filter){
      shinyMobile::f7Slider(
            "budget",
            label = "Total Budget (millions):",
            min = 0,
            max = 1600,
            value = c(0,1600),
            scaleSteps = 5,
            scaleSubSteps = 3,
            scale = T
            )
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
            value = c(0,500),
            scaleSteps = 5,
            scaleSubSteps = 3,
            scale = T)
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
            value = c(0,4500),
            scaleSteps = 5,
            scaleSubSteps = 3,
            scale = T)
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
            value = c(0,125),
            scaleSteps = 5,
            scaleSubSteps = 3,
            scale = T)
  } else{
      
    }
  })

#Each reactive module checks if County / League / Vocational / etc. are in the input condition.
# If so add the corresponding schools to a vector. If not return an empty vector. At the end concatenate all 
county_schools <- reactive({
  if ('County' %in% input$comp_cond) {
    query <- paste0("SELECT district_name
                            FROM district_info
                            WHERE county =",
                    "'",
                    county(),
                    "';")
    sqlQuery(query) %>%
      dplyr::filter(district_name != district()) %>%
      dplyr::pull(district_name) %>%
      unique()
  } else{
    c()
  }
})

league_schools <- reactive({
  req(district())
  if ('League'  %in% input$comp_cond) {
    query <- paste0("SELECT district_name
                          FROM district_info
                          WHERE league =",
                    "'",
                    league(),
                    "';")
    
    sqlQuery(query) %>%
      dplyr::filter(district_name != district()) %>%
      dplyr::pull(district_name) %>%
      unique()
  } else{
    c()
  }
})

dart_schoools <- reactive({
  req(district())
  if ('DART'  %in% input$comp_cond) {
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
    
    for (i in 1:length(darts)) {
      new_dart <- dart_names %>%
        dplyr::filter(dart_name == darts[i]) %>%
        dplyr::pull(district_name)
      
      dart_districts <- append(dart_districts, new_dart)
    }
    dart_districts
  } else {
    c()
  }
})

my_schools <- reactive({
  req(district())
  if ('My Schools'  %in% input$comp_cond) {
    query <- paste0("SELECT school
                            FROM custom_schools
                            WHERE user =",
                    "'",active_user(),"';")
    
    sqlQuery(query) %>%
      dplyr::pull(school) %>%
      unique()
  } else{
    c()
  }
})

vocational_schools <- reactive({
  req(district())
  if ('Vocational'  %in% input$comp_cond) {
    school_info %>%
      dplyr:::filter(stringr::str_detect(district_name, 'Vocational') &
                       district_name != district()) %>%
      dplyr::pull(district_name) %>%
      unique()
  } else{
    c()
  }
})

regional_schools <- reactive({
  req(district())
  if ('Regional'  %in% input$comp_cond) {
    school_info %>%
      dplyr:::filter(stringr::str_detect(district_name, 'Regional') &
                       district_name != district()) %>%
      dplyr::pull(district_name) %>%
      unique()
  } else{
    c()
  }
})

charter_schools <- reactive({
  req(district())
  if ('Charter'  %in% input$comp_cond) {
    school_info %>%
      dplyr:::filter(stringr::str_detect(district_name, 'Charter') &
                       district_name != district()) %>%
      dplyr::pull(district_name) %>%
      unique()
  } else{
    c()
  }
})

#Combine (union) all selected modules
district_comps_step_1 <- reactive({ 
  c(county_schools(),league_schools(),my_schools(),dart_schoools(),
    vocational_schools(),charter_schools(),regional_schools()
  ) %>%
    unique()
})


#Filters are set to 0 or a virtual inf for each category if the filter option is
#not active. If it is active its set to the selected amount with units
budget_hi <- reactive({
  if(length(input$budget[2]) ==0){
    10000000000000000
  } else{
    input$budget[2] * 1000000
  }
  })
budget_lo <- reactive({
  if(length(input$budget[1])==0){
    0
  }else{
    input$budget[1] * 1000000
  }
  })

enrollment_hi <- reactive({
  if(length(input$enrollment[2]) == 0){
    100000000
  } else{
    input$enrollment[2] * 100
  }
  })
enrollment_lo <- reactive({
  if(length(input$enrollment[1]) == 0){
    0
  }else{
    input$enrollment[1] * 100
  }
  })
fte_hi <- reactive({
  if(length(input$fte[2]) == 0){
    10000000
  } else{
    input$fte[2]
  }
  })
fte_lo <- reactive({
  if(length(input$fte[1]) == 0){
    0
  } else{
    input$fte[1]
  }
  })
salary_hi <- reactive({
  if(length(input$salary[2]) == 0){
    1000000000
  } else {
    input$salary[2] * 1000
  }
  })
salary_lo <- reactive({
  if(length(input$salary[1]) == 0){
    0
  } else {
    input$salary[1] * 1000
  }
  })

#Filter all schools by the criteria defined above
district_comps_step_2 <- reactive({
  school_info %>%
    dplyr::filter(
      total_exp >= budget_lo() &
        total_exp <= budget_hi() &
        total_enrollment >= enrollment_lo() &
        total_enrollment <= enrollment_hi() &
        total_teachers >= fte_lo() &
        total_teachers <= fte_hi()
    ) %>%
    dplyr::pull(district_name) %>%
    unique()
  
})

#intersect filtered schools with previously selected list to get final options
district_comps_initial <- reactive({
  intersect(district_comps_step_1(),district_comps_step_2())
})

#Observe for any input change to update the selected list
observeEvent(list(input$comp_cond, input$comp_filter, input$salary, input$budget, input$fte, input$enrollment), {
  updateSelectizeInput(
     inputId = "district_comps",
     selected = district_comps_initial()
  )
    })

comp_districts <- reactive({
     req(district())
     input$district_comps
     })
  
#reset button will clear the selected options (along with js defined in the UI)
  observeEvent(list(input$reset, input$district), {
      updateSelectizeInput(
        inputId = "district_comps",
        selected = ""
      )
    })
  

  #save button will first delete all currently saved schools in the DB and then 
  #insert row by row before notifying the user
  observeEvent(input$save, {
    query <- paste0("DELETE FROM custom_schools
                    WHERE user =",
                    "'",active_user(),"'",";")
    sqlQuery(query)

    for (i in 1:length(comp_districts())) {
      query <- paste0(
        "REPLACE INTO custom_schools VALUES ('",
        paste0(active_user(), i),"',",
        "'",active_user(),"','",
        comp_districts()[i],"');"
      )
      sqlQuery(query)
    }
    
    shinyMobile::f7Notif(
            text = paste(i ,"Schools succesfully updated"),
            icon = shinyMobile::f7Icon("floppy_disk"),
            title = '"My Schools Updated"',
            titleRightText = "now",
            closeTimeout = 5500,
            closeButton = T
      )
  })

  
 # Plot DF for multiple use ------------------------------------------------

  #create a df that includes all selected districts, user district, and a dummy 
  #row to act as the "other" when no others are selected.
  # The df is then grouped and summarized to get the average of all the others
  # and other special info required for plots
plot_comp_df <- reactive({
        req(district())
        school_info %>%
            dplyr::filter(((district_name %in% comp_districts() | district_name == district()) &
                   year == comp_year()) |  district_name == 'dummy') %>%
            dplyr::mutate(is_district = ifelse(district_name == district(), district(), "Others")) %>%
            dplyr::group_by(is_district) %>%
            dplyr::summarize(
              dplyr::across(grade_pk:total_expenditures,
                             ~ mean(.x, na.rm = TRUE)
                            ),
              year = dplyr::last(year)
            ) %>%
    dplyr::mutate(short_district = stringr::str_trunc(is_district, 20, "right")) %>%
    dplyr::rename(aaa = is_district) %>%
    dplyr::mutate(aaa  = ifelse(aaa == district(), 'district', aaa),
                  no_buy_back = 1-sick_days_buy_back,
                  no_sick_bank  = 1- sick_leave_bank,
                  no_nurses_union = 1-nurses_unionized) %>%
    t() %>%
    data.frame() %>%
    janitor::row_to_names(1) %>% 
    dplyr::mutate(category = rownames(.))
            
    })
  

# Home --------------------------------------------------------------------

  #creates a summary table to be rendered as a gt table 
summary_gt <- reactive({
  req(district())
    district_df <- school_info %>%
            dplyr::filter(district_name == district(),
                   year == comp_year()) %>%
            dplyr::mutate(
                total_enrollment = paste("Enrollment:", total_enrollment),
                total_teachers = paste("Teacher FTE:", total_teachers),
                year = paste("Year:", year)
                ) %>%
            dplyr::select(year, total_enrollment, total_teachers) %>%
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
    
  #rendered with 100% width to fit the container
  output$summary_table <- gt::render_gt(
    expr = summary_gt(),
    width = gt::pct(100)
    )
  
  
  # Title of home panel
    output$district_at_glance <- renderUI({
      req(district())
      
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
      req(district())
      tags$div(class = 'comparison',
          tags$span(district(), style = "color:#DB9743"), " vs"  ,tags$span("All Available Districts", style = "color:#2a3a6e"))
    })
    
    #similar to plot_comp_df but in this case it gets the average for all available
    # districts, not just the selected ones
    plot_all_data <- reactive({
      req(district())
      school_info %>%
        dplyr::mutate(is_district = ifelse(district_name == district(), district(), "Others")) %>%
        dplyr::group_by(is_district) %>%
        dplyr::summarize(dplyr::across(total_enrollment:low_income_pct,
                                       ~ mean(.x, na.rm = TRUE))) %>%
        dplyr::mutate(short_district = stringr::str_trunc(is_district, 20, "right")) %>%
        dplyr::rename(aaa = is_district) %>%
        dplyr::mutate(aaa  = ifelse(aaa == district(), 'district', aaa)) %>%
        t() %>%
        data.frame() %>%
        janitor::row_to_names(1) %>%
        dplyr::mutate(category = rownames(.))
            
      
    })
    
    #filter for a specific category and later render a plotly plot (recurring MO)
    cola_all_data <- reactive({
      req(district())
      plot_all_data() %>% 
        dplyr::filter(category %in% c('cola_2022_23', 'cola_2021_22', 'cola_2020_21')) %>% 
        dplyr::mutate(dplyr::across(.fns = as.numeric))
      
    })
    
    output$cola_plot_all <- plotly::renderPlotly({
      req(district())
      plotly::plot_ly(
        cola_all_data(),
        x = ~Others,
        y = c('2020-21', '2021-22', '2022-23'),
        type = 'bar',
        name = 'A',
        height = 235,
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
          xaxis = list(title = "", fixedrange = T,showticklabels = F),
          yaxis = list(title = "", fixedrange = T),
          barmode = 'group',
          title = 'COLA %',
          autosize = T,
          showlegend =F,
          font = list(size = 12)
        )
  })
    
  upper_left_data <- reactive({
    req(district())
      plot_all_data() %>% 
        dplyr::filter(category %in% c('upper_left_2020_21', 'upper_left_2021_22', 'upper_left_2022_23')) %>% 
        dplyr::mutate(dplyr::across(.fns = as.numeric))
    })
    
    output$upper_left <- plotly::renderPlotly({
      req(district())
      plotly::plot_ly(
        upper_left_data(),
        x = ~Others,
        y = ~ c('2020-21', '2021-22', '2022-23'),
        type = 'bar',
        name = 'A',
        orientation = 'h',
        height = 235,
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
          showlegend =F,
          font = list(size = 12)
        )
  })
    
  lower_right_data <- reactive({
    req(district())
      plot_all_data() %>% 
         dplyr::filter(category %in% c('lower_right_2020_21', 'lower_right_2021_22', 'lower_right_2022_23')) %>% 
        dplyr::mutate(dplyr::across(.fns = as.numeric))
    })
    
    output$lower_right <- plotly::renderPlotly({
      req(district())
      plotly::plot_ly(
        lower_right_data(),
        x = ~Others,
        y = ~ c('2020-21', '2021-22', '2022-23'),
        type = 'bar',
        name = 'A',
        height = 235,
        orientation = 'h',
        marker = list(color = '#2a3a6e'),
        hovertemplate = '%{x:.4s}<extra></extra>',
        texttemplate = '%{x:.3s}',
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
          showlegend =F,
          font = list(size = 12)
        )
  })
    
  average_salary_data <- reactive({
    req(district())
      plot_all_data() %>% 
        dplyr::filter(category %in% c('salary_avg_2020_21', 'salary_avg_2021_22', 'salary_avg_2022_23')) %>% 
        dplyr::mutate(dplyr::across(.fns = as.numeric))
    })
  
    
    output$average_salary_yearly <- plotly::renderPlotly({
      req(district())
      plotly::plot_ly(
        average_salary_data(),
        y = ~Others,
        x = ~ c('2020-21', '2021-22', '2022-23'),
        type = 'scatter',
        height = 235,
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
          showlegend =F,
          font = list(size = 12)
        )
  })

    #the start all the plots that depend on the comparison districts
    
    palette_names <- reactive({ #This palette always sets orange for the district and blue for the others
      req(district())
      setNames(palette, c('Others', district()))
      })
    
    
    budget_data <- reactive({
      plot_comp_df() %>%
        dplyr::filter(category %in% c('total_exp', 'salary_total')) %>% 
        dplyr::mutate(dplyr::across(.fns = as.numeric))
    })
    
    
  output$total_budget <- plotly::renderPlotly({
      plotly::plot_ly(
        data = budget_data(),
        x = ~ Others,
        y = ~ c('Teacher Salaries',
                'Total Budget'),
        type = 'bar',
        name = 'Others',
        height = 310,
        orientation = 'h',
        marker = list(color = '#2a3a6e'),
        hovertemplate = '%{x:.3s}<extra></extra>',
        texttemplate = '%{x:.2s}',
        textposition = 'inside'
      ) %>%
         plotly:: config(displayModeBar = FALSE) %>%
        plotly::add_trace(x = ~ district,
                  marker = list(color = '#DB9743')) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, range = c(0, max(budget_data(), na.rm = T)*1.15)),
          yaxis = list(title = "", fixedrange = T),
          barmode = 'group',
          title = 'Total Budget',
          autosize = T,
          showlegend =F,
          font = list(size = 12)
         )
  })
  
  per_pupil <- reactive({
      plot_comp_df() %>% 
        dplyr::filter(category %in% c('total_exp_per_pupil')) %>% 
        dplyr::mutate(dplyr::across(.fns = as.numeric))
    })
  

  output$per_pupil_exp <-  plotly::renderPlotly(
        plotly::plot_ly(
        per_pupil(),
        x = ~c(''),
        y = ~ district,
        height = 235,
        type = 'bar',
        name = district(),
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(district()  , ': %{y:.4s}<extra></extra>'),
        texttemplate = '%{y:.3s}',
        textposition = 'auto'
      ) %>%
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::add_trace(y = ~ Others,
                  name = 'Others',
                   hovertemplate = 'Other: %{y:,.4s}<extra></extra>',
                   texttemplate = '%{y:,.3s}',
                  marker = list(color = '#2a3a6e')) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('PK', 'K', purrr::map_chr(seq(12), function(x){paste('Grade', x)}))),
          yaxis = list(title = "", fixedrange = T,automargin = T, range = c(0, max(per_pupil(), na.rm = T)*1.15)),
          barmode = 'group',
          title = 'Per-Pupil Expenditure',
          showlegend =F,
          font = list(size = 13)
        )
    )
  
  avg_salary <- reactive({
      plot_comp_df() %>%
        dplyr::filter(category %in% c('average_salary')) %>% 
        dplyr::mutate(dplyr::across(.fns = as.numeric))
    })
  

  output$average_salary <-  plotly::renderPlotly(
        plotly::plot_ly(
        avg_salary(),
        x = ~c(''),
        y = ~ district,
        type = 'bar',
        height = 235,
        name = district(),
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(district()  , ': %{y:.4s}<extra></extra>'),
        texttemplate = '%{y:.3s}',
        textposition = 'auto'
      ) %>%
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::add_trace(y = ~ Others,
                  name = 'Others',
                   hovertemplate = 'Others: %{y:,.4s}<extra></extra>',
                   texttemplate = '%{y:,.3s}',
                  marker = list(color = '#2a3a6e')) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('PK', 'K', purrr::map_chr(seq(12), function(x){paste('Grade', x)}))),
          yaxis = list(title = "", fixedrange = T,automargin = T, range = c(0, max(avg_salary(), na.rm = T)*1.15)),
          barmode = 'group',
          title = 'Average Teacher Salary',
          showlegend =F,
          font = list(size = 13)
        )
    )
  
  enrollment <- reactive({
      plot_comp_df() %>% 
        dplyr::filter(category %in% c('total_enrollment')) %>% 
        dplyr::mutate(dplyr::across(.fns = as.numeric))
    })
  

  output$total_enrollment <-  plotly::renderPlotly(
        plotly::plot_ly(
        enrollment(),
        x = ~c(''),
        y = ~ district,
        height = 235,
        type = 'bar',
        name = district(),
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(district()  , ': %{y:.3s}<extra></extra>'),
        texttemplate = '%{y:.2s}',
        textposition = 'auto'
      ) %>%
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::add_trace(y = ~ Others,
                  name = 'Others',
                   hovertemplate = 'Others: %{y:,.3s}<extra></extra>',
                   texttemplate = '%{y:,.2s}',
                  marker = list(color = '#2a3a6e')) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('PK', 'K', purrr::map_chr(seq(12), function(x){paste('Grade', x)}))),
          yaxis = list(title = "", fixedrange = T,automargin = T, range = max(enrollment(), na.rm = T)*1.15),
          barmode = 'group',
          title = 'Total Enrollment',
          showlegend =F,
          font = list(size = 13)
        )
    )
  
  placement <- reactive({
      plot_comp_df() %>% 
        dplyr::filter(category %in% c('ap_score_3_5_pct')) %>% 
        dplyr::mutate(dplyr::across(.fns = as.numeric))
    })
  
  

  output$advanced_placement <-  plotly::renderPlotly(
        plotly::plot_ly(
        placement(),
        x = ~c(''),
        y = ~ district,
        type = 'bar',
        name = district(),
        height = 235,
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(district()  , ': %{y:.2f}%<extra></extra>'),
        texttemplate = '%{y:.1f}%',
        textposition = 'auto'
      ) %>%
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::add_trace(y = ~ Others,
                  name = 'Others',
                   hovertemplate = 'Others: %{y:,.2f}%<extra></extra>',
                   texttemplate = '%{y:,.1f}%',
                  marker = list(color = '#2a3a6e')) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('PK', 'K', purrr::map_chr(seq(12), function(x){paste('Grade', x)}))),
          yaxis = list(title = "% of scores 3-5", fixedrange = T,automargin = T, range = c(0, max(placement(), na.rm = T)*1.15)),
          barmode = 'group',
          title = 'Advanced Placement',
          showlegend =F,
          font = list(size = 13)
        )
    )
    
  total_teachers_data <- reactive({
      plot_comp_df() %>% 
        dplyr::filter(category %in% c('total_teachers')) %>% 
        dplyr::mutate(dplyr::across(.fns = as.numeric))
    })
  

  output$total_teachers <-  plotly::renderPlotly(
        plotly::plot_ly(
        total_teachers_data(),
        x = ~c(''),
        y = ~ district,
        type = 'bar',
        name = district(),
        height = 235,
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(district()  , ': %{y:.4s}<extra></extra>'),
        texttemplate = '%{y:.3s}',
        textposition = 'auto'
      ) %>%
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::add_trace(y = ~ Others,
                  name = 'Others',
                   hovertemplate = 'Others: %{y:,.4s}<extra></extra>',
                   texttemplate = '%{y:,.3s}',
                  marker = list(color = '#2a3a6e')) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('PK', 'K', purrr::map_chr(seq(12), function(x){paste('Grade', x)}))),
          yaxis = list(title = "", fixedrange = T,automargin = T, range = c(0, max(total_teachers_data(), na.rm = T)*1.15)),
          barmode = 'group',
          title = 'Total Teachers',
          showlegend =F,
          font = list(size = 13)
        )
    )
  
  sat_data <- reactive({
      plot_comp_df() %>% 
      dplyr::filter(category %in% c('sat_reading_writing', 'sat_math')) %>% 
      dplyr::mutate(dplyr::across(.fns = as.numeric)) %>% 
      t() %>% 
      data.frame() %>% 
      dplyr::mutate(is_district = rownames(.),
                    is_district = ifelse(is_district == 'district', district(),is_district))
    })
  
    
  output$sat_performance <- plotly::renderPlotly({
      plotly::plot_ly(
      sat_data(),
      colors = palette_names(),
      x = ~sat_reading_writing,
      y = ~sat_math,
      type = "scatter",
      mode = "markers",
      color = ~is_district,
      marker = list(size = 25,
                    opacity = .75),
      hovertemplate = paste0( sat_data()$is_district, '<br>Reading/Writing: %{x:,.1f}<br>Math: %{y:,.1f}'),
      height = 225
    ) %>%
      plotly:: config(displayModeBar = FALSE) %>%
      plotly::layout(
        yaxis = list(title = "Math",
                     range = list(0, 800),
                     fixedrange = T),
        xaxis = list(
          title = "Reading/Writing",
          range = list(0, 800),
          fixedrange = T
        ),
        title = "SAT Performance",
        showlegend = F
      )
  })
  
  mcas_data <- reactive({
    plot_comp_df() %>% 
      dplyr::filter(category %in% c('ela_advanced_proficient_pct','mth_advanced_proficient_pct','sci_advanced_proficient_pct')) %>% 
      dplyr::mutate(dplyr::across(.fns = as.numeric))
    })
  


  output$mcas <-  plotly::renderPlotly(
        plotly::plot_ly(
        mcas_data(),
        x = ~c('Science', 'ELA', 'Math'),
        y = ~ district,
        type = 'bar',
        name = district(),
        height = 235,
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(district()  , ': %{y:.0f}%<extra></extra>'),
        texttemplate = '%{y:.0f}%',
        textposition = 'auto'
      ) %>%
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::add_trace(y = ~ Others,
                  name = 'Other',
                   hovertemplate = 'Others: %{y:,.2f}%<extra></extra>',
                   texttemplate = '%{y:,.1f}%',
                  marker = list(color = '#2a3a6e')) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('PK', 'K', purrr::map_chr(seq(12), function(x){paste('Grade', x)}))),
          yaxis = list(title = "% Advanced/Proficient", fixedrange = T,automargin = T, range = c(0, max(mcas_data(), na.rm = T)*1.15)),
          barmode = 'group',
          title = '10th Grade MCAS',
          showlegend =F,
          font = list(size = 13)
        )
    )
  
  demographics_data <- reactive({
    plot_comp_df() %>%
      dplyr::filter(category %in% c('english_learner_pct', 'low_income_pct')) %>%
      dplyr::mutate(dplyr::across(.fns = as.numeric)) %>% 
      t() %>% 
      data.frame() %>% 
      dplyr::mutate(is_district = rownames(.),
                    is_district = ifelse(is_district == 'district', district(),is_district))
  })
  
    
  output$student_demographics <- plotly::renderPlotly({
      plotly::plot_ly(
      demographics_data(),
      colors = palette_names(),
      x = ~english_learner_pct,
      y = ~low_income_pct,
      type = "scatter",
      mode = "markers",
      color = ~is_district,
      marker = list(size = 25,
                    opacity = .75),
      hovertemplate = paste0( sat_data()$is_district, '<br>English-Learners: %{x:,.1f}%<br>Low Income: %{y:,.1f}%'),
      height = 225
    ) %>%
      plotly:: config(displayModeBar = FALSE) %>%
      plotly::layout(
        yaxis = list(title = "Low Income %",
                     range = c(0, max(demographics_data()$low_income_pct, na.rm = T)*1.25),
                     fixedrange = T),
        xaxis = list(title = "English-Learner %",
                     range = c(0, max(demographics_data()$low_income_pct, na.rm = T)*1.25),
                     fixedrange = T
        ),
        title = "Student Demographics",
        showlegend = F
      )
  })
  
  #Screenshot manager that will capture the special selector in each page and generate a PNG
  observeEvent(input$down_home, { #take "screenshot" of home selector
    shinyscreenshot::screenshot(selector = '#home-plots' , filename = paste(comp_year(), district(), 'Home'), scale = 4)
  })
  observeEvent(input$down_overview, { #take "screenshot" of  selector
    shinyscreenshot::screenshot(selector = '#overview_table' , filename =  paste(comp_year(), district(),'Overview Table'), scale = 4)
  })
  observeEvent(input$down_plot, { #take "screenshot" of  selector
    shinyscreenshot::screenshot(selector = '#plot_card' , filename =  paste(comp_year(), district(),'Custom Plot'), scale = 4)
  })
  observeEvent(input$down_students, { #take "screenshot" of  selector
    shinyscreenshot::screenshot(selector = '#students' , filename =  paste(comp_year(), district(),'Student Page'), scale = 4)
  })
  observeEvent(input$down_staff, { #take "screenshot" of  selector
    shinyscreenshot::screenshot(selector = '#staff' , filename =  paste(comp_year(), district(),'Staff Page'), scale = 4)
  })
  observeEvent(input$down_finance, { #take "screenshot" of  selector
    shinyscreenshot::screenshot(selector = '#finances' , filename =  paste(comp_year(), district(),'Finance Page'), scale = 4)
  })
  observeEvent(input$down_academic, { #take "screenshot" of  selector
    shinyscreenshot::screenshot(selector = '#academic' , filename =  paste(comp_year(), district(),'Academic Page'), scale = 4)
  })
  observeEvent(input$down_work, { #take "screenshot" of selector
    shinyscreenshot::screenshot(selector = '#work' , filename =  paste(comp_year(), district(),'Working Conditions Page'), scale = 4)
  })
  
  
  output$down_overview_csv = downloadHandler(
     filename = function() {
       paste(comp_year(), district(), "Overview.csv")
     },
     content = function(file) {
       write.csv(comp_table(), file, row.names = F)
     }
   )

    

# Overview ----------------------------------------------------------------
# Create data frame with comparison of district and comparison districts
    # NOTE: User can choose to see individual comp districts or aggregated
    # comp districts using the input$individuals parameter (used in if statements below)
    comp_df <- reactive({
      req(district())
        
        # If comp districts are grouped...
        if (input$individuals == "No"){
            
            # Create data frame of comparison districts and rename columns
            comp_df <- school_info %>%
                dplyr::filter(district_name %in% comp_districts(),
                       year == comp_year()) %>%
                dplyr::select(total_enrollment:total_exp_per_pupil) %>%
                magrittr::set_colnames(overview_statistics) %>%
                as.data.frame()
            
            # Create data frame of user's district() and rename columns
            district_df <- school_info %>%
                dplyr::filter(district_name == district(),
                       year == comp_year()) %>%
                dplyr::select(total_enrollment:total_exp_per_pupil) %>%
                magrittr::set_colnames(overview_statistics) %>%
                as.data.frame()
            
            # Iterate thru columns and for each, create a row with the average of
            # comp districts, value for the user's district(), and % difference between
            # the two - as I said on our call, there's probably a vectorized way to
            # do this, but I never had time to make more efficient
            for (i in 1:ncol(comp_df)){
                
                col_mean <- mean(comp_df[,i], na.rm = T)
                col_name <- names(comp_df)[i]
                district_value <- district_df[,i][1]
                
                if (i == 1){
                    
                    diff <- ifelse(!is.na(district_value) & !is.na(col_mean),
                                   district_value-col_mean, NA_real_)
                    
                    pct_diff <- ifelse(!is.na(diff), diff/col_mean, NA_real_)
                    
                    differences_df <- data.frame(column = c(col_name),
                                                 value = c(district_value),
                                                 mean = c(col_mean),
                                                 difference = c(diff),
                                                 pct_difference = c(pct_diff))
                    
                    colnames(differences_df) <- c("column",
                                                  "value",
                                                  "mean",
                                                  "diff",
                                                  "pct_diff")
                }
                else{
                    
                    diff <- ifelse(!is.na(district_value) & !is.na(col_mean),
                                   district_value-col_mean, NA_real_)
                    
                    pct_diff <- ifelse(!is.na(diff), diff/col_mean, NA_real_)
                    
                    new_row <- data.frame(column = c(col_name),
                                          value = c(district_value),
                                          mean = c(col_mean),
                                          difference = c(diff),
                                          pct_difference = c(pct_diff))
                    
                    colnames(new_row) <- c("column",
                                           "value",
                                           "mean",
                                           "diff",
                                           "pct_diff")
                    
                    differences_df <- rbind(new_row, differences_df)
                }
            }
        }
        # If comp districts are not grouped...
        else{
            
            # Create data frame with only user's district() and chosen comp districts
            comp_df <- school_info %>%
                dplyr::filter(district_name %in% comp_districts() | district_name == district(),
                       year == comp_year())
            
            district_names <- comp_df$district_name
            new_df_names <- c()
            
            # Create names for new data frame with '{district() name}' and '{district() name} Diff'
            # to record both value and % difference from user's district()
            for (i in 1:nrow(comp_df)){
                new_df_names <- append(new_df_names, comp_df$district_name[i])
                new_df_names <- append(new_df_names, paste0(comp_df$district_name[i], " Diff"))
            }
            
            # Transpose comparison df so columns are the districts and rows are the statistics of interest
            comp_df <- comp_df %>%
                dplyr::select(total_enrollment:total_exp_per_pupil) %>%
                t() %>%
                magrittr::set_colnames(district_names) %>%
                magrittr::set_rownames(overview_statistics) %>%
                as.data.frame()
            
            # Create data frame to store comp district() values and % differences
            value_column <- as.vector(comp_df[district()])
            differences_df <- data.frame(value = c(overview_statistics),
                                         diff = c(value_column))
            colnames(differences_df) <- c(district(), paste0(district(), " Diff"))
            
            # Iterate thru districts and for each, create a column for the district()
            # values and % differences
            for (j in 1:ncol(comp_df)){
                
                current_district <- district_names[j]
                current_df_names <- colnames(differences_df)
                
                if (current_district != district()){
                    
                    value_column <- as.vector(comp_df[current_district])
                    diff_column <- (value_column-comp_df[district()])/comp_df[district()]
                    
                    differences_df <- cbind(differences_df, value_column) %>%
                        cbind(diff_column)
                    
                    colnames(differences_df) <- append(current_df_names, c(current_district, paste0(current_district, " Diff")))
                    
                }
            }
            
        }
        
        ## YOU NOW HAVE A DF WITH A COLUMN FOR EACH district() AND EACH district() % DIFF FROM USER'S district()
        differences_df
    })
    
    # Now use the data frame from above to generate a pretty {gt} table for comparisons
    comp_table <- reactive({
        
        # If comp districts are grouped...
        if (input$individuals == "No"){
            
            len_comps <- length(comp_districts())
            
            # Create list of comp districts for table source note
            # NOTE: There might be a better way to do this
            for (i in 1:len_comps){
                if (i == 1){
                    comp_list <- comp_districts()[i]
                }
                else{
                    comp_list <- paste0(comp_list, ", ", comp_districts()[i])
                }
            }
            
            # Build {gt} table
            comp_df() %>%
                dplyr::mutate(
                    # Replace NaNs with NAs (this was causing problems earlier)
                    value = ifelse(is.nan(value), NA_real_, as.numeric(value)),
                    mean = ifelse(is.nan(mean), NA_real_, as.numeric(mean)),
                    diff = ifelse(is.nan(diff), NA_real_, as.numeric(diff))
                ) %>%
                # Arrange in order of columns so we can do row groupings later
                dplyr::arrange(match(column,
                              c("Total Expenditure",
                                "Total Expenditure per Pupil",
                                "In-District Expenditure",
                                "In-District Expenditure per Pupil",
                                
                                "Total Teacher FTE",
                                "Total Teacher Salaries",
                                "Average Salary",
                                
                                "Total Enrollment",
                                "Total Pupil FTE",
                                "In-District Pupil FTE",
                                "Student:Teacher Ratio",
                                
                                "Total Core Academic Classes",
                                "% Teachers Licensed",
                                "% Teachers Experienced",
                                "% Teachers w/o Waiver",
                                "% Teachers Teaching in Field",
                                "% Core Classes Taught by Exp Teachers")),
                        dplyr::desc(column)) %>%
                gt::gt() %>%
                gt::cols_label(
                    column = "",
                    value = "Your District",
                    mean = "Avg of Comparison Districts",
                    diff = "Difference",
                    pct_diff = "% Difference"
                ) %>%
                gt::cols_align("center", columns = 2:5) %>%
                gt::fmt_number(columns = 2:4, rows = c(8,12), decimals = 0) %>%
                gt::fmt_number(columns = 2:4, rows = c(5,9:11), decimals = 1) %>%
                gt::fmt_currency(columns = 2:4, rows = c(1:4,6:7), decimals = 0) %>%
                gt::fmt_percent(columns = 2:4, rows = c(13:17), decimals = 1) %>%
                gt::fmt_percent(columns = 5, decimals = 1) %>%
                # Color palette is such that anything 1-100% is lighter hue; anything
                # greater in magnitude is dark
                gt::data_color(
                    columns = 5, 
                    colors =
                        scales::col_bin(
                            palette = (
                                palette = color_palette
                            ) %>% as.character(),
                            domain = c(-1000,1000),
                            bins = c(-1000,-1,0,1,1000),
                            na.color = "white"
                        )
                ) %>%
                gt::tab_row_group(
                    label = "Teacher Stats",
                    rows = c(12:17)
                ) %>%
                gt::tab_row_group(
                    label = "Enrollment",
                    rows = c(8:11)
                ) %>%
                gt::tab_row_group(
                    label = "Teacher Salaries",
                    rows = c(5:7)
                ) %>%
                gt::tab_row_group(
                    label = "Expenditure",
                    rows = c(1:4)
                ) %>%
                gt_theme_538() %>%
                gt::cols_width(
                  gt::everything() ~ gt::pct(10)
                ) %>% 
                gt::tab_source_note(
                    source_note = "Note: Only enrollment data available for 2021/22;
                                    Teacher salaries available beginning in 2019/20;
                                    Teacher stats vary in availability by year/district()") %>%
                gt::tab_header(
                    title = paste0(district(), " vs. Comparison Districts"),
                    subtitle = paste0("Stats for ", comp_year(), "; See footnote for data availability details")
                ) %>%
                gt::tab_footnote(footnote="Dark red = ≤ -100%; Dark green = ≥ 100%", locations=gt::cells_column_labels(columns=5)) %>%
                gt::tab_footnote(comp_list, locations=gt::cells_column_labels(columns=3))
        }
        
        # If comp districts aren't grouped...
        else{
            
            # Create list of columns that will store % values
            # Need for table formatting later
            if (ncol(comp_df()) > 2){
                pct_cols <- seq(4, ncol(comp_df()), 2)
            }
            else{
                pct_cols <- c()
            }
            
            # Create list of columns that will store measured statistics
            # Need for table formatting later
            if (ncol(comp_df()) > 2){
                value_cols <- append(2, seq(3, ncol(comp_df()), 2))
            }
            else{
                value_cols <- c(2)
            }
            
            # Get column indexes for renaming purposes later
            # Easier to refer to index with dynamic user input
            col_indexes <- c(seq(ncol(comp_df())))
            
            # Comparison districts WITHOUT the district() itself
            comp_districts_adjusted <- c()
            
            if (ncol(comp_df()) > 2){
                for (i in 1:length(comp_districts())){
                    if (comp_districts()[i] != district() & comp_districts()[i] %in% colnames(comp_df())){
                        comp_districts_adjusted <- append(comp_districts_adjusted, comp_districts()[i])
                    }
                }
            }
            
            # Column labels
            # NOTE: First two columns are the names of the statistics and the 
            # values for the district() itself, so we set those right off the bat
            col_labs <- c("",district())
            
            # Again, we use the convention of '{district() name}' and '{district() name} % Diff'
            if (length(comp_districts_adjusted) > 0){
                for (i in 1:length(comp_districts_adjusted)){
                    col_labs <- append(col_labs, c(comp_districts_adjusted[i], "% Diff"))
                }
            }
            
            # Finally, create named vector where column indexes correspond to names from above
            col_labs <- purrr::set_names(col_labs, col_indexes)
            
            # Build {gt} table
            comp_df() %>%
                # Arrange in order of columns so we can do row groupings later
                dplyr::arrange(match(get(district()),
                              c("Total Expenditure",
                                "Total Expenditure per Pupil",
                                "In-District Expenditure",
                                "In-District Expenditure per Pupil",
                                
                                "Total Teacher FTE",
                                "Total Teacher Salaries",
                                "Average Salary",
                                
                                "Total Enrollment",
                                "Total Pupil FTE",
                                "In-District Pupil FTE",
                                "Student:Teacher Ratio",
                                
                                "Total Core Academic Classes",
                                "% Teachers Licensed",
                                "% Teachers Experienced",
                                "% Teachers w/o Waiver",
                                "% Teachers Teaching in Field",
                                "% Core Classes Taught by Exp Teachers")),
                        dplyr::desc(get(district()))) %>%
                # Set column names to indexes
                magrittr::set_colnames(col_indexes) %>%
                gt::gt() %>%
                # Set column labels
                gt::cols_label(.list = col_labs) %>%
                gt_theme_538() %>%
                gt::tab_row_group(
                    label = "Teacher Stats",
                    rows = c(12:17)
                ) %>%
                gt::tab_row_group(
                    label = "Enrollment",
                    rows = c(8:11)
                ) %>%
                gt::tab_row_group(
                    label = "Teacher Salaries",
                    rows = c(5:7)
                ) %>%
                gt::tab_row_group(
                    label = "Expenditure",
                    rows = c(1:4)
                ) %>%
                gt::fmt_number(columns = value_cols, rows = c(8,12), decimals = 0) %>%
                gt::fmt_number(columns = value_cols, rows = c(5,9:11), decimals = 1) %>%
                gt::fmt_currency(columns = value_cols, rows = c(1:4,6:7), decimals = 0) %>%
                gt::fmt_percent(columns = value_cols, rows = c(13:17), decimals = 1) %>%
                gt::fmt_percent(columns = pct_cols, decimals = 1) %>%
                # Color palette is such that anything 1-100% is lighter hue; anything
                # greater in magnitude is dark
                gt::data_color(
                    columns = pct_cols, 
                    colors =
                        scales::col_bin(
                            palette = (
                                palette = color_palette
                            ) %>% as.character(),
                            domain = c(-1000,1000),
                            bins = c(-1000,-1,0,1,1000),
                            na.color = "white"
                        )
                ) %>%
                gt::cols_align("center", columns = 2:max(col_indexes)) %>%
                gt::tab_style(
                    style = list(
                        gt::cell_borders(
                            sides = c("left"),
                            color = "black",
                            weight = gt::px(2)
                        ),
                        gt::cell_borders(
                            sides = c("left"),
                            color = "black",
                            weight = gt::px(2)
                        )
                    ),
                    locations = list(
                        gt::cells_body(
                            columns = value_cols
                        ),
                        gt::cells_column_labels(
                            columns = value_cols
                        )
                    )
                ) %>%
                gt::tab_source_note(
                    source_note = "Note: Only enrollment data available for 2021/22;
                                    Teacher salaries available beginning in 2019/20;
                                    Teacher stats vary in availability by year/district()") %>%
                gt::tab_footnote(footnote="Dark red = ≤ -100%; Dark green = ≥ 100%", locations=gt::cells_column_labels(columns=pct_cols)) %>%
                gt::tab_header(
                    title = paste0(district(), " vs. Comparison Districts"),
                    subtitle = paste0("Stats for ", comp_year(), "; See footnote for data availability details")
                ) %>%
                # Hacky way to ensure that all '% Diff' columns are the same size no matter how
                # many comp districts the user enters
                gt::cols_width(`1` ~ gt::px(275),
                           gt::ends_with("12") ~ gt::px(75),
                           gt::ends_with("22") ~ gt::px(75),
                           gt::ends_with("32") ~ gt::px(75),
                           gt::ends_with("42") ~ gt::px(75),
                           gt::ends_with("52") ~ gt::px(75),
                           gt::ends_with("62") ~ gt::px(75),
                           gt::ends_with("72") ~ gt::px(75),
                           gt::ends_with("82") ~ gt::px(75),
                           gt::ends_with("92") ~ gt::px(75),
                           gt::ends_with("02") ~ gt::px(75),
                           gt::ends_with("4") ~ gt::px(75),
                           gt::ends_with("6") ~ gt::px(75),
                           gt::ends_with("8") ~ gt::px(75),
                           gt::ends_with("0") ~ gt::px(75),
                           gt::everything() ~ gt::px(150)) %>%
                gt::tab_options(table.font.size = 14)
        }
    })
    
    # Actually render the {gt} table (need {gt} object to save in next function)
    output$comparisons_table <- gt::render_gt({comp_table()})
    
    observe(print(comp_df()))
  

# Plots -------------------------------------------------------------------

    #display major stat categories
output$x_cat_disp <- renderUI({
    req(input$plot_type)
    if(input$plot_type == 'Scatter'){
      shinyMobile::f7Select(
            "x_cat",
            label = h3("X-Axis Category"),
            choices = stat_categories
      )
    } else{
      
    } 
    })
    
    
    #display minor stat options
output$x_var_disp <- renderUI({
    req(input$plot_type)
    if(input$plot_type == 'Scatter'){
      shinyMobile::f7Select(
            "x_cat",
            label = h3("X-Axis Variable"),
            choices = c('')
      )
    } else{
      
    } 
    })
    
output$y_cat_disp <- renderUI({
    req(input$plot_type)
      shinyMobile::f7Select(
         'y_cat',
         label = h3("Y-Axis Category"), 
         choices = stat_categories
       )
    })

output$y_var_disp <- renderUI({
    req(input$plot_type)
      shinyMobile::f7Select(
         'y_var',
         label = h3("Y-Axis Variable"), 
         choices = c('')
       )
    })

#depending on the major stat input, update the minor stat options 
output$x_var_disp <- renderUI({
  req(input$x_cat)
  if(input$plot_type == 'Scatter'){
    if (input$x_cat == "Enrollment") {
    shinyMobile::f7Select("x_var",h3('X-Axis Variable'), choices = axis_options[6:21])
  }
  else if (input$x_cat == "Teachers/Classes") {
    shinyMobile::f7Select("x_var",h3('X-Axis Variable'), choices = axis_options[22:29])
  }
  else if (input$x_cat == "Salaries/Expenditure") {
    shinyMobile::f7Select("x_var",h3('X-Axis Variable'), choices = axis_options[30:37])
  }
  else if (input$x_cat == "10th Grade MCAS") {
    shinyMobile::f7Select("x_var",h3('X-Axis Variable'), choices = axis_options[38:70])
  }
  else if (input$x_cat == "SAT/AP") {
    shinyMobile::f7Select("x_var",h3('X-Axis Variable'), choices = axis_options[71:81])
  }
  else if (input$x_cat == "Basic Contract") {
    shinyMobile::f7Select("x_var",h3('X-Axis Variable'), choices = axis_options[82:132])
  }
  else if (input$x_cat == "Working Conditions") {
    shinyMobile::f7Select("x_var",h3('X-Axis Variable'), choices = axis_options[133:163])
  }
  else if (input$x_cat == "Student Population") {
    shinyMobile::f7Select("x_var",h3('X-Axis Variable'), choices = axis_options[164:173])
  }
  }
})

output$y_var_disp <- renderUI({
  req(input$y_cat)
  if (input$y_cat == "Enrollment") {
    shinyMobile::f7Select("y_var", h3('Y-Axis Variable'), choices = axis_options[6:21])
  }
  else if (input$y_cat == "Teachers/Classes") {
    shinyMobile::f7Select("y_var",h3('Y-Axis Variable'), choices = axis_options[22:29])
  }
  else if (input$y_cat == "Salaries/Expenditure") {
    shinyMobile::f7Select("y_var",h3('Y-Axis Variable'), choices = axis_options[30:37])
  }
  else if (input$y_cat == "10th Grade MCAS") {
    shinyMobile::f7Select("y_var",h3('Y-Axis Variable'), choices = axis_options[38:70])
  }
  else if (input$y_cat == "SAT/AP") {
    shinyMobile::f7Select("y_var",h3('Y-Axis Variable'), choices = axis_options[71:81])
  }
  else if (input$y_cat == "Basic Contract") {
    shinyMobile::f7Select("y_var",h3('Y-Axis Variable'), choices = axis_options[82:132])
  }
  else if (input$y_cat == "Working Conditions") {
    shinyMobile::f7Select("y_var",h3('Y-Axis Variable'), choices = axis_options[133:163])
  }
  else if (input$y_cat == "Student Population") {
    shinyMobile::f7Select("y_var",h3('Y-Axis Variable'), choices = axis_options[164:173])
  }
})

y <- reactive(input$y_var)
x <- reactive(input$x_var)

# Put data in df to build plots
plotdata <- reactive({
  plot_data <- school_info %>%
    dplyr::filter(
      district_name %in% comp_districts() | district_name == district(),
      year == comp_year(),
      !is.na(get(y()))
    ) %>%
    dplyr::mutate(
      is_district = ifelse(district_name == district(), district(), "Others"),
      short_district = stringr::str_trunc(district_name, 20, "right")
    )
})
output$userplot <- plotly::renderPlotly({
        
        
        if (input$plot_type == "Scatter"){
          req(x())
          req(y())
            
            x <- plotdata() %>% dplyr::pull(x())
            y <- plotdata() %>% dplyr::pull(y())

            plotly::plot_ly(
                x = x,
                y = y,
                type = "scatter",
                mode = "markers",
                marker = list(
                    size = 30,
                    opacity = .5
                ),
                color = plotdata()$short_district,
                colors = ggsci::pal_jco()(10),
                hovertemplate = paste((plotdata() %>% dplyr::pull(district_name)),
                                      "<br>",
                                      names(axis_options)[axis_options == x()], ":",
                                      (plotdata() %>% dplyr::pull(x())),
                                      "<br>",
                                      names(axis_options)[axis_options == y()], ":",
                                      (plotdata() %>% dplyr::pull(y())),
                                      "<extra></extra>"),
                height = 630) %>%
                plotly::layout(yaxis = list(title = names(axis_options)[axis_options == y()], fixedrange = T),
                       xaxis = list(title = names(axis_options)[axis_options == x()], fixedrange = T),
                       title = paste0(district(), " vs. Comparison Districts",
                                     "<br><sup>", names(axis_options)[axis_options == y()],
                                     "vs.", names(axis_options)[axis_options == x()], input$comp_year, "</sup>"),
                       images = list(
                           source = base64enc::dataURI(file = "https://raw.githubusercontent.com/SCasanova/arxed_ddd/main/www/D3%20Logo.png"),
                           x = 0, y = 1,
                           sizex = 0.2, sizey = 0.2
                       ))
            
        }
        
        else if (input$plot_type == "Pie"){
          req(y())
            
            y <- plotdata() %>% dplyr::pull(y())
            x <- factor(plotdata() %>% dplyr::pull(district_name), levels = unique(plotdata() %>% dplyr::pull(district_name))[order(plotdata() %>% dplyr::pull(y()))])
            y_mean <- mean(y, na.rm = T)
            
            plotly::plot_ly(
                labels = x,
                values = y,
                marker = list(colors = ggsci::pal_jco()(10)),
                textinfo='label+percent',
                hovertemplate = paste((plotdata() %>% dplyr::pull(district_name)),
                                      "<br>",
                                      names(axis_options)[axis_options == y()], ":",
                                      y, "<extra></extra>"),
                height = 630) %>%
              plotly::add_pie(hole = 0.4) %>% 
                plotly::layout(title = paste0(district(), " vs. Comparison Districts",
                                     "<br><sup>", names(axis_options)[axis_options == y()], input$comp_year, "</sup>"),
                               margin = list(l = 50,r = 50,b = 50,t = 50,pad = 4), 
                       legend = list(font = list(size = 11)),
                       images = list(
                           source = base64enc::dataURI(file = "https://raw.githubusercontent.com/SCasanova/arxed_ddd/main/www/D3%20Logo.png"),
                           x = 0, y = 1,
                           sizex = 0.2, sizey = 0.2
                       ))
            
        }
        
        else{
          req(y())
            
            y <- plotdata() %>% dplyr::pull(y())
            x <- factor(plotdata() %>% dplyr::pull(district_name), levels = unique(plotdata() %>% dplyr::pull(district_name))[order(plotdata() %>% dplyr::pull(y()))])
            district_short <- factor(plotdata() %>% dplyr::pull(short_district), levels = unique(plotdata() %>% dplyr::pull(short_district))[order(plotdata() %>% dplyr::pull(y()))])
            y_mean <- mean(y, na.rm = T)
            
            plotly::plot_ly(
                x = x,
                y = y,
                type = "bar",
                color = district_short,
                colors = ggsci::pal_jco()(10),
                hovertemplate = paste((plotdata() %>% dplyr::pull(district_name)),
                                      "<br>",
                                      names(axis_options)[axis_options == y()], ":",
                                      y, "<extra></extra>"),
                texttemplate = '%{y:.2f}', textposition = 'outside',
                height = 630) %>%
                plotly::layout(xaxis = list(title = "District", showticklabels = F, fixedrange = T),
                       yaxis = list(title = names(axis_options)[axis_options == y()] , fixedrange = T),
                       title = paste0(district(), " vs. Comparison Districts",
                                     "<br><sup>", names(axis_options)[axis_options == y()], input$comp_year, "</sup>"),
                       legend = list(font = list(size = 11)),
                       annotations = list(
                           x = sort(x)[1],
                           y = y_mean,
                           text = paste("Avg:", round(y_mean,1)),
                           xref = "x",
                           yref = "y",
                           showarrow = TRUE,
                           arrowhead = 7,
                           ax = 20,
                           ay = -40
                       ),
                       shapes = list(type='line', x0 = sort(x)[1], x1 = sort(x)[nrow(plotdata())], y0=y_mean, y1=y_mean, line=list(dash='dot', width=1)),
                       images = list(
                           source = base64enc::dataURI(file = "https://raw.githubusercontent.com/SCasanova/arxed_ddd/main/www/D3%20Logo.png"),
                           x = 0, y = 1,
                           sizex = 0.2, sizey = 0.2
                       ))
            
        }
        
    })
        
# Students ----------------------------------------------------------------

    general_data <- reactive({
        plot_comp_df() %>% 
        dplyr::filter(category %in% c('grade_pk', 'grade_k', paste0('grade_', seq(12)))) %>% 
        dplyr::mutate(dplyr::across(.fns = as.numeric))
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
         plotly:: config(displayModeBar = FALSE) %>%
        plotly::layout(
          xaxis = list(title = "", tickangle = -45, fixedrange = T, categoryorder = "array", categoryarray = c('PK', 'K', purrr::map_chr(seq(12), function(x){paste('Grade', x)}))),
          yaxis = list(title = "", fixedrange = T,automargin = T, range = c(0, max(general_data(), na.rm = T)*1.15)),
          barmode = 'group',
          showlegend =F,
          font = list(size = 18),
          images = list(
            source = "https://raw.githubusercontent.com/SCasanova/arxed_ddd/main/www/D3%20Logo.png",
            x = 0, y = 1,
            sizex = 0.2, sizey = 0.2
        )
        ) %>%
        plotly::add_trace(y = ~ Others,
                  name = 'Other',
                   hovertemplate = 'Other: %{y:,.1f}<extra></extra>',
                   texttemplate = '%{y:,.0f}',
                  marker = list(color = '#2a3a6e'))
    )

    needs_data <- reactive({
      req(district())
      plot_comp_df() %>% 
        dplyr::filter(category %in% c('economically_disadvantaged_percent', 'english_language_learner_percent', 'students_with_disabilities_percent')) %>% 
        dplyr::mutate(dplyr::across(.fns = as.numeric))
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
        plotly::add_trace(y = ~ Others,
                  name = 'Other',
                   hovertemplate = 'Other: %{y:,.1f}%<extra></extra>',
                   texttemplate = '%{y:,.1f}%',
                  marker = list(color = '#2a3a6e')) %>%
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('PK', 'K', purrr::map_chr(seq(12), function(x){paste('Grade', x)}))),
          yaxis = list(title = "", fixedrange = T,automargin = T),
          barmode = 'group',
          showlegend =F,
          font = list(size = 18)
        )
    )
    
    diversity_data <- reactive({
      req(district())
      plot_comp_df() %>% 
        dplyr::filter(category %in% c('african_american_percent_students', 'asian_percent_students', 'hispanic_percent_students', 'white_percent_students', 'native_american_percent_students', 'native_hawaiian_pacific_islander_percent_students', 'multi_race_non_hispanic_percent_students')) %>% 
        dplyr::mutate(dplyr::across(.fns = as.numeric))
      
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
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::layout(
          xaxis = list(title = "", tickangle = -25, fixedrange = T, categoryorder = "array", categoryarray = c('PK', 'K', purrr::map_chr(seq(12), function(x){paste('Grade', x)}))),
          yaxis = list(title = "", fixedrange = T,automargin = T,range = c(0, max(diversity_data(), na.rm = T)*1.15)),
          barmode = 'group',
          showlegend =F,
          font = list(size = 18),
          images = list(
            source = "https://raw.githubusercontent.com/SCasanova/arxed_ddd/main/www/D3%20Logo.png",
            x = 0, y = 1,
            sizex = 0.2, sizey = 0.2
        )
        )%>%
        plotly::add_trace(y = ~ Others,
                  name = 'Others',
                   hovertemplate = 'Others: %{y:,.1f}%<extra></extra>',
                   texttemplate = '%{y:,.0f}%',
                  marker = list(color = '#2a3a6e'))
    )
    
    pie_data <- reactive({
      req(district())
      diversity_data() %>% 
        dplyr::select(district)})
    

    output$student_diversity_pie <- plotly::renderPlotly(
        plotly::plot_ly(
            pie_data(),
            labels = ~ c('African American', 'Asian', 'Hispanic', 'White', 'Native American', 'Hawaiian/Pacific Islander', 'Multi-Race'),
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            values = ~district,
            hoverinfo = 'label+percent',
            text = ~paste0(district, '%'),
             marker = list(colors = c('#DB9743', '#3F7CAC', '#2E282A', '#2a3a6e', '#F3EFF5', '#C03221', '#7DAA92'),
                      line = list(color = '#FFFFFF', width = 1))
            )%>%
            plotly::add_pie(hole = 0.4) %>%
          plotly:: config(displayModeBar = FALSE) %>%
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
    
    mobility_data <- reactive({
      req(district())
      plot_comp_df() %>% 
        dplyr::filter(category %in% c('percent_churn', 'percent_intake', 'percent_stability')) %>% 
        dplyr::mutate(dplyr::across(.fns = as.numeric))
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
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('PK', 'K', purrr::map_chr(seq(12), function(x){paste('Grade', x)}))),
          yaxis = list(title = "", fixedrange = T,automargin = T, range = c(0,max( mobility_data(), na.rm = T)*1.15)),
          barmode = 'group',
          showlegend =F,
          font = list(size = 18),
          images = list(
            source = "https://raw.githubusercontent.com/SCasanova/arxed_ddd/main/www/D3%20Logo.png",
            x = 0, y = 1,
            sizex = 0.2, sizey = 0.2
        )
        )%>%
        plotly::add_trace(y = ~ Others,
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

    staff_gen_data <- reactive({
        plot_comp_df() %>%
        dplyr::filter(category %in% c('total_in_district_fte')) %>%
        dplyr::mutate(dplyr::across(.fns = as.numeric))
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
        plotly::add_trace(y = ~ Others,
                  name = 'Other',
                   hovertemplate = 'Other: %{y:,.1f}<extra></extra>',
                   texttemplate = '%{y:,.1f}',
                  marker = list(color = '#2a3a6e')) %>%
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('PK', 'K', purrr::map_chr(seq(12), function(x){paste('Grade', x)}))),
          yaxis = list(title = "", fixedrange = T,automargin = T),
          barmode = 'group',
          showlegend =F,
          font = list(size = 18)
        )
    )

    staff_ratio_data <- reactive({
      plot_comp_df() %>%
        dplyr::filter(category %in% c('student_teacher_ratio')) %>%
        dplyr::mutate(dplyr::across(.fns = as.numeric))
      
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
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::add_trace(y = ~ Others,
                  name = 'Other',
                   hovertemplate = 'Others: %{y:,.0f}:1<extra></extra>',
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

    staff_gender_data <- reactive({
      req(district())
      plot_comp_df() %>% 
        dplyr::filter(category %in% c('males_percent', 'females_percent')) %>% 
        dplyr::mutate(dplyr::across(.fns = as.numeric))
    })


    output$district_gender <- plotly::renderPlotly(
        plotly::plot_ly(
            staff_gender_data(),
            labels = ~ c('Male', 'Female'),
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            values = ~district,
            hoverinfo = 'label+percent',
            text = ~paste0(district, '%'),
             marker = list(colors = c("gray", "#DB9743"),
                      line = list(color = '#FFFFFF', width = 1))
            )%>%
          plotly:: config(displayModeBar = FALSE) %>%
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


    output$other_gender <- plotly::renderPlotly(
        plotly::plot_ly(
            staff_gender_data(),
            labels = ~ c('Male', 'Female'),
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            values = ~Others,
            hoverinfo = 'label+percent',
            text = ~paste0(Others, '%'),
             marker = list(colors = c("gray", "#2a3a6e"),
                      line = list(color = '#FFFFFF', width = 1))
            )%>%
            plotly::add_pie()%>%
          plotly:: config(displayModeBar = FALSE) %>%
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

    staff_diversity_data <- reactive({
      req(district())
      plot_comp_df() %>% 
        dplyr::filter(category %in% c('african_american_percent_staff', 'asian_percent_staff', 'hispanic_percent_staff', 'white_percent_staff', 'native_american_percent_staff', 'native_hawaiian_pacific_islander_percent_staff', 'multi_race_non_hispanic_percent_staff')) %>% 
        dplyr::mutate(dplyr::across(.fns = as.numeric))
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
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::layout(
          xaxis = list(title = "", tickangle = -25, fixedrange = T, categoryorder = "array", categoryarray = c('African American', 'Asian', 'Hispanic', 'White', 'Native American', 'Hawaiian/Pacific Islander', 'Multi-Race')),
          yaxis = list(title = "", fixedrange = T,automargin = T,range = c(0, max(staff_diversity_data(), na.rm = T)*1.15)),
          barmode = 'group',
          showlegend =F,
          font = list(size = 18),
          images = list(
            source = "https://raw.githubusercontent.com/SCasanova/arxed_ddd/main/www/D3%20Logo.png",
            x = 0, y = 1,
            sizex = 0.2, sizey = 0.2
        )
        )%>%
        plotly::add_trace(y = ~ Others,
                  name = 'Other',
                   hovertemplate = 'Other: %{y:,.1f}%<extra></extra>',
                   texttemplate = '%{y:,.1f}%',
                  marker = list(color = '#2a3a6e'))
    )

    staff_pie_data <- reactive({
      req(district())
      staff_diversity_data() %>% 
        dplyr::select(district)})


    output$staff_diversity_pie <- plotly::renderPlotly(
        plotly::plot_ly(
            staff_pie_data(),
            labels = ~ c('African American', 'Asian', 'Hispanic', 'White', 'Native American', 'Hawaiian/Pacific Islander', 'Multi-Race'),
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            values = ~district,
            hoverinfo = 'label+percent',
            text = ~paste0(district, '%'),
             marker = list(colors = c('#DB9743', '#3F7CAC', '#2E282A', '#2a3a6e', '#F3EFF5', '#C03221', '#7DAA92'),
                      line = list(color = '#FFFFFF', width = 1))
            )%>%
            plotly::add_pie(hole = 0.4)%>%
          plotly:: config(displayModeBar = FALSE) %>%
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
      plot_comp_df() %>% 
        dplyr::filter(category %in% c('pct_teachers_licensed', 'pct_experienced_teachers', 'pct_teachers_without_waiver','pct_teaching_in_field')) %>% 
        dplyr::mutate(dplyr::across(.fns = as.numeric))
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
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('African American', 'Asian', 'Hispanic', 'White', 'Native American', 'Hawaiian/Pacific Islander', 'Multi-Race')),
          yaxis = list(title = "", fixedrange = T,automargin = T,range = c(0, max(teacher_pct_df(), na.rm = T)*1.15)),
          barmode = 'group',
          showlegend =F,
          font = list(size = 18),
          images = list(
            source = "https://raw.githubusercontent.com/SCasanova/arxed_ddd/main/www/D3%20Logo.png",
            x = 0, y = 1,
            sizex = 0.2, sizey = 0.2
        )
        ) %>% 
        plotly::add_trace(y = ~ Others,
                  name = 'Other',
                   hovertemplate = 'Others: %{y:.1%}<extra></extra>',
                   texttemplate = '%{y:.0%}',
                  marker = list(color = '#2a3a6e')) 
    )
  
    

# Finance Tab -------------------------------------------------------------

    
    finance_data <- reactive({
        req(district())
      plot_comp_df() %>% 
        dplyr::filter(category %in% c('instructional_services', 'administration', 'pupil_services', 'operations_and_maintenance', 'insurance_retirement_programs_and_other')) %>% 
        # dplyr::mutate(dplyr::across(.fns = as.numeric)) %>% 
        dplyr::select(district)
    })
    


    output$finances_plot <- plotly::renderPlotly(
        plotly::plot_ly(
            finance_data(),
            labels = ~ c('Instructional Services', 'Administration', 'Pupil Services', 'Operations and Maintenance', 'Insurance, Retirement and others'),
            textposition = 'inside',
            textinfo = 'label+text',
            insidetextfont = list(color = '#FFFFFF'),
            values = ~district,
            hoverinfo = 'label+text+percent',
            text = ~ paste0("$", formatC(as.numeric(district), format="f", digits=2, big.mark=",")),
             marker = list(colors =  c('#DB9743', '#3F7CAC', '#2a3a6e', '#2E282A', '#575a5e'),
                      line = list(color = '#FFFFFF', width = 1))
            )%>%
            plotly::add_pie(hole = 0.4)%>%
            plotly:: config(displayModeBar = FALSE) %>%
            plotly::layout(
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
    
  cola_plot_data <- reactive({
    plot_comp_df() %>% 
        dplyr::filter(category %in% c('cola_2020_21', 'cola_2021_22', 'cola_2022_23')) %>% 
        dplyr::mutate(dplyr::across(.fns = as.numeric))
  })
  
    output$cola_plot_comp <- plotly::renderPlotly({
      plotly::plot_ly(
        cola_plot_data(),
        x = ~Others,
        y = ~ c('2020-21', '2021-22', '2022-23'),
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
          xaxis = list(title = "COLA %", fixedrange = T, ticks = ''),
          yaxis = list(title = "", fixedrange = T),
          margin = list(b = 100),
          barmode = 'group',
          showlegend =F,
          font = list(size = 18)
        )
  })
    

# Academic ----------------------------------------------------------------

ap_3_5_data <- reactive({
        plot_comp_df() %>%
        dplyr::filter(category %in% c('ap_score_3_5_pct')) %>%
        dplyr::mutate(dplyr::across(.fns = as.numeric))
    })


    output$ap_3_5 <-  plotly::renderPlotly(
        plotly::plot_ly(
        ap_3_5_data(),
        x = ~ c('% of 3-5 AP Results'),
        y = ~ district,
        type = 'bar',
        name = district(),
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(district()  , ': %{y:,.1f}%<extra></extra>'),
        texttemplate = '%{y:,.1f}%',
        textposition = 'auto'
      ) %>%
        plotly::add_trace(y = ~ Others,
                  name = 'Other',
                   hovertemplate = 'Other: %{y:,.2f}%<extra></extra>',
                   texttemplate = '%{y:,.1f}%',
                  marker = list(color = '#2a3a6e')) %>%
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('PK', 'K', purrr::map_chr(seq(12), function(x){paste('Grade', x)}))),
          yaxis = list(title = "", fixedrange = T,automargin = T),
          barmode = 'group',
          showlegend =F,
          font = list(size = 18)
        )
    )
    
ap_taken_data <- reactive({
        plot_comp_df() %>%
        dplyr::filter(category %in% c('ap_tests_taken')) %>%
        dplyr::mutate(dplyr::across(.fns = as.numeric))
    })


    output$ap_taken <-  plotly::renderPlotly(
        plotly::plot_ly(
        ap_taken_data(),
        x = ~ c('AP Tests Taken'),
        y = ~ district,
        type = 'bar',
        name = district(),
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(district()  , ': %{y:,.0f}<extra></extra>'),
        texttemplate = '%{y:,.0f}',
        textposition = 'auto'
      ) %>%
        plotly::add_trace(y = ~ Others,
                  name = 'Other',
                   hovertemplate = 'Other: %{y:,.1f}<extra></extra>',
                   texttemplate = '%{y:,.0f}',
                  marker = list(color = '#2a3a6e')) %>%
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('PK', 'K', purrr::map_chr(seq(12), function(x){paste('Grade', x)}))),
          yaxis = list(title = "", fixedrange = T,automargin = T),
          barmode = 'group',
          showlegend =F,
          font = list(size = 18)
        )
    )
    
sat_results_data <- reactive({
        plot_comp_df() %>%
        dplyr::filter(category %in% c('sat_reading_writing', 'sat_math')) %>%
        dplyr::mutate(dplyr::across(.fns = as.numeric))
    })

    output$sat_results <-  plotly::renderPlotly(
        plotly::plot_ly(
        sat_results_data(),
        x = ~ c('SAT Reading/Writing','SAT Math'),
        y = ~ district,
        type = 'bar',
        name = district(),
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(district()  , ': %{y:,.0f}<extra></extra>'),
        texttemplate = '%{y:,.0f}',
        textposition = 'auto'
      ) %>%
        plotly::add_trace(y = ~ Others,
                  name = 'Other',
                   hovertemplate = 'Other: %{y:,.2f}<extra></extra>',
                   texttemplate = '%{y:,.1f}',
                  marker = list(color = '#2a3a6e')) %>%
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('PK', 'K', purrr::map_chr(seq(12), function(x){paste('Grade', x)}))),
          yaxis = list(title = "", fixedrange = T,automargin = T),
          barmode = 'group',
          showlegend =F,
          font = list(size = 18)
        )
    )
    
sat_taken_data <- reactive({
        plot_comp_df() %>%
        dplyr::filter(category %in% c('sat_tests_taken')) %>%
        dplyr::mutate(dplyr::across(.fns = as.numeric))
    })


    output$sat_taken <-  plotly::renderPlotly(
        plotly::plot_ly(
        sat_taken_data(),
        x = ~ c('SAT Tests Taken'),
        y = ~ district,
        type = 'bar',
        name = district(),
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(district()  , ': %{y:,.0f}<extra></extra>'),
        texttemplate = '%{y:,.0f}',
        textposition = 'auto'
      ) %>%
        plotly::add_trace(y = ~ Others,
                  name = 'Other',
                   hovertemplate = 'Other: %{y:,.1f}<extra></extra>',
                   texttemplate = '%{y:,.0f}',
                  marker = list(color = '#2a3a6e')) %>%
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('PK', 'K', purrr::map_chr(seq(12), function(x){paste('Grade', x)}))),
          yaxis = list(title = "", fixedrange = T,automargin = T),
          barmode = 'group',
          showlegend =F,
          font = list(size = 18)
        )
    )
    
mcas_ap_data <- reactive({
        plot_comp_df() %>%
        dplyr::filter(category %in% c('sci_advanced_proficient_pct', 'ela_advanced_proficient_pct', 'mth_advanced_proficient_pct')) %>%
        dplyr::mutate(dplyr::across(.fns = as.numeric))
    })


    output$mcas_ap <-  plotly::renderPlotly(
        plotly::plot_ly(
        mcas_ap_data(),
        x = ~ c('Science', 'ELA', 'Math'),
        y = ~ district,
        type = 'bar',
        name = district(),
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(district()  , ': %{y:,.0f}%<extra></extra>'),
        texttemplate = '%{y:,.0f}%',
        textposition = 'auto'
      ) %>%
        plotly::add_trace(y = ~ Others,
                  name = 'Other',
                   hovertemplate = 'Other: %{y:,.2f}%<extra></extra>',
                   texttemplate = '%{y:,.1f}%',
                  marker = list(color = '#2a3a6e')) %>%
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('PK', 'K', purrr::map_chr(seq(12), function(x){paste('Grade', x)}))),
          yaxis = list(title = "% Advanced/Proficient", fixedrange = T,automargin = T),
          barmode = 'group',
          showlegend =F,
          font = list(size = 18)
        )
    )
    
mcas_wf_data <- reactive({
        plot_comp_df() %>%
        dplyr::filter(category %in% c('sci_warning_failing_pct', 'ela_warning_failing_pct', 'mth_warning_failing_pct')) %>%
        dplyr::mutate(dplyr::across(.fns = as.numeric))
    })



    output$mcas_wf <-  plotly::renderPlotly(
        plotly::plot_ly(
        mcas_wf_data(),
        x = ~ c('Science', 'ELA', 'Math'),
        y = ~ district,
        type = 'bar',
        name = district(),
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(district()  , ': %{y:,.0f}%<extra></extra>'),
        texttemplate = '%{y:,.0f}%',
        textposition = 'auto'
      ) %>%
        plotly::add_trace(y = ~ Others,
                  name = 'Other',
                   hovertemplate = 'Other: %{y:,.2f}%<extra></extra>',
                   texttemplate = '%{y:,.1f}%',
                  marker = list(color = '#2a3a6e')) %>%
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('PK', 'K', purrr::map_chr(seq(12), function(x){paste('Grade', x)}))),
          yaxis = list(title = "% Warning/Failing", fixedrange = T,automargin = T),
          barmode = 'group',
          showlegend =F,
          font = list(size = 18)
        )
    )
    

# Work Cond.  -------------------------------------------------------------

days_with_df <- reactive({
  plot_comp_df() %>% 
    dplyr::filter(category %in% c('num_days_with_students'))%>% 
        dplyr::mutate(dplyr::across(.fns = as.numeric))

    })
    
  output$days_with <-  plotly::renderPlotly(
        plotly::plot_ly(
        days_with_df(),
        x = ~ c('Teachers: # of days with students'),
        y = ~ district,
        type = 'bar',
        name = district(),
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(district()  , ': %{y:.0f}<extra></extra>'),
        texttemplate = '%{y:.0f}',
        textposition = 'outside'
      ) %>%
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('African American', 'Asian', 'Hispanic', 'White', 'Native American', 'Hawaiian/Pacific Islander', 'Multi-Race')),
          yaxis = list(title = "", fixedrange = T,automargin = T,range = c(0, max(days_with_df(), na.rm = T)*1.15)),
          barmode = 'group',
          showlegend =F,
          font = list(size = 18)
        ) %>% 
        plotly::add_trace(y = ~ Others,
                  name = 'Other',
                   hovertemplate = 'Others: %{y:.1f}<extra></extra>',
                   texttemplate = '%{y:.0f}',
                  marker = list(color = '#2a3a6e')) 
    )

    
  days_wo_df <- reactive({
      plot_comp_df() %>% 
        dplyr::filter(category %in% c('num_days_without_students_teachers', 'num_days_without_students_guidance', 'num_days_without_students_nurses'))%>% 
        dplyr::mutate(dplyr::across(.fns = as.numeric))
    })
    

  output$days_w_wo <-  plotly::renderPlotly(
        plotly::plot_ly(
        days_wo_df(),
        x = ~ c('Teachers: # of days w/o students', 'Guidance', 'Nurses'),
        y = ~ district,
        type = 'bar',
        name = district(),
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(district()  , ': %{y:.0f}<extra></extra>'),
        texttemplate = '%{y:.0f}',
        textposition = 'outside'
      ) %>%
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('African American', 'Asian', 'Hispanic', 'White', 'Native American', 'Hawaiian/Pacific Islander', 'Multi-Race')),
          yaxis = list(title = "", fixedrange = T,automargin = T,range = c(0, max(days_wo_df(), na.rm = T)*1.15)),
          barmode = 'group',
          showlegend =F,
          font = list(size = 18),
          images = list(
            source = "https://raw.githubusercontent.com/SCasanova/arxed_ddd/main/www/D3%20Logo.png",
            x = 0, y = 1,
            sizex = 0.2, sizey = 0.2
        )
        ) %>% 
        plotly::add_trace(y = ~ Others,
                  name = 'Others',
                   hovertemplate = 'Other: %{y:.1f}<extra></extra>',
                   texttemplate = '%{y:.0f}',
                  marker = list(color = '#2a3a6e')) 
    )
  
  health_df <- reactive({
      plot_comp_df() %>%
        dplyr::filter(category %in% c('pct_health_insurance_paid'))%>% 
        dplyr::mutate(dplyr::across(.fns = as.numeric))
    })
    

  output$health_percent <-  plotly::renderPlotly(
        plotly::plot_ly(
        health_df(),
        x = ~ c('% Health Insurance Paid'),
        y = ~ district,
        type = 'bar',
        name = district(),
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(district()  , ': %{y:.1%}<extra></extra>'),
        texttemplate = '%{y:.0%}',
        textposition = 'inside'
      ) %>%
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('African American', 'Asian', 'Hispanic', 'White', 'Native American', 'Hawaiian/Pacific Islander', 'Multi-Race')),
          yaxis = list(title = "", fixedrange = T,automargin = T,range = c(0, max(health_df(), na.rm = T)*1.15)),
          barmode = 'group',
          showlegend =F,
          font = list(size = 18),
          images = list(
            source = "https://raw.githubusercontent.com/SCasanova/arxed_ddd/main/www/D3%20Logo.png",
            x = 0, y = 1,
            sizex = 0.2, sizey = 0.2
        )
        ) %>% 
        plotly::add_trace(y = ~ Others,
                  name = 'Other',
                   hovertemplate = 'Others: %{y:.1%}<extra></extra>',
                   texttemplate = '%{y:.0%}',
                  marker = list(color = '#2a3a6e')) 
    )
  
  sick_bank_df <- reactive({
      plot_comp_df() %>%
        dplyr::filter(category %in% c('sick_days', 'sick_leave_bank_cap', 'personal_days_annually', 'personal_days_carry_over_max', 'bereavement_days', 'religious_days'))%>% 
        dplyr::mutate(dplyr::across(.fns = as.numeric))
    })
    

  output$sick_bank <-  plotly::renderPlotly(
        plotly::plot_ly(
        sick_bank_df(),
        x = ~ c('Sick Days', 'Sick Bank Cap', 'Personal Days', 'Personal Days Carryover Cap', 'Bereavement Days', 'Religious Days'),
        y = ~ district,
        type = 'bar',
        name = district(),
        marker = list(color = '#DB9743'),
        hovertemplate = paste0(district()  , ': %{y:.1f}<extra></extra>'),
        texttemplate = '%{y:.0f}',
        textposition = 'outside'
      ) %>%
        plotly:: config(displayModeBar = FALSE) %>%
        plotly::layout(
          xaxis = list(title = "", fixedrange = T, categoryorder = "array", categoryarray = c('African American', 'Asian', 'Hispanic', 'White', 'Native American', 'Hawaiian/Pacific Islander', 'Multi-Race')),
          yaxis = list(title = "", fixedrange = T,automargin = T,range = c(0, max(sick_bank_df(), na.rm = T)*1.15)),
          barmode = 'group',
          showlegend =F,
          font = list(size = 18),
          images = list(
            source = "https://raw.githubusercontent.com/SCasanova/arxed_ddd/main/www/D3%20Logo.png",
            x = 0, y = 1,
            sizex = 0.2, sizey = 0.2
        )
        ) %>% 
        plotly::add_trace(y = ~ Others,
                  name = 'Other',
                   hovertemplate = 'Others: %{y:.1f}<extra></extra>',
                   texttemplate = '%{y:.0f}',
                  marker = list(color = '#2a3a6e')) 
    )
  
  nurse_union_data  <- reactive({
    req(input$comp_cond)
        plot_comp_df() %>%
          dplyr::filter(category %in% c('nurses_unionized', 'no_nurses_union'))%>% 
        dplyr::mutate(dplyr::across(.fns = as.numeric))
    })
  
district_union_text <- reactive({
  req(comp_districts)
    if(subset(nurse_union_data(), rownames(nurse_union_data()) == 'nurses_unionized')$district == 1 & !is.na(subset(nurse_union_data(), rownames(nurse_union_data()) == 'nurses_unionized')$district)){
      tags$span('Yes', class = 'answer-text')
    }else{
      tags$span('No', class = 'answer-text')
    }
  })




district_union_output <- renderUI(district_union_text())

    output$nurses_union_plot <- plotly::renderPlotly({
      req(input$comp_cond)
        plotly::plot_ly(
            nurse_union_data(),
            labels = c('Yes', 'No'),
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            values = ~Others,
            hoverinfo = 'label+percent',
            text = ~paste0(Others, '%'),
             marker = list(colors = c("#2a3a6e", "gray"),
                      line = list(color = '#FFFFFF', width = 1))
            )%>%
            plotly::add_pie(hole = 0.5)%>%
          plotly:: config(displayModeBar = FALSE) %>%
            plotly::layout(
                title = "Nurses Unionized",
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
    })
    
    buy_back_data  <- reactive({
    req(input$comp_cond)
      plot_comp_df() %>%
        dplyr::filter(category %in% c('sick_days_buy_back', 'no_buy_back'))%>% 
        dplyr::mutate(dplyr::across(.fns = as.numeric))
    })
    
    output$buy_back_binary <- plotly::renderPlotly({
      req(input$comp_cond)
        plotly::plot_ly(
            buy_back_data(),
            labels = c('Yes', 'No'),
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            values = ~Others,
            hoverinfo = 'label+percent',
            text = ~paste0(Others, '%'),
             marker = list(colors = c("#2a3a6e", "gray"),
                      line = list(color = '#FFFFFF', width = 1))
            )%>%
            plotly::add_pie(hole = 0.5)%>%
          plotly:: config(displayModeBar = FALSE) %>%
            plotly::layout(
                title = "Sick Days Buy Back",
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
    })
    
    sick_bank_data  <- reactive({
    req(input$comp_cond)
        plot_comp_df() %>%
          dplyr::filter(category %in% c('sick_leave_bank', 'no_sick_bank')) %>% 
        dplyr::mutate(dplyr::across(.fns = as.numeric))
    })
    

    output$sick_bank_binary <- plotly::renderPlotly({
      req(input$comp_cond)
        plotly::plot_ly(
            sick_bank_data(),
            labels = c('Yes', 'No'),
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            values = ~Others,
            hoverinfo = 'label+percent',
            text = ~paste0(Others, '%'),
             marker = list(colors = c("#2a3a6e", "gray"),
                      line = list(color = '#FFFFFF', width = 1))
            )%>%
            plotly::add_pie(hole = 0.5)%>%
          plotly:: config(displayModeBar = FALSE) %>%
            plotly::layout(
                title = "Sick Leave Bank",
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
    })
     
     
     
 }
shinyApp(ui = ui, server = server)
 