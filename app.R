## app.R ##

# Load Libraries

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(data.table)
library(plotly)
#library(leaflet)
#library(rgdal)

# Import Data

# I wrangled the 'WAPM Query - Monthly Data.csv' file in Python and created 'All' labels and categories for
# 'Gender' and 'Race' individually and overall, 'Interactive WAPM monthly data wrangle' Python script

#### DAP ####

# Data import for 'Interactive'

data.DAP <-
  read.csv(file = "www/Data/DAP/DAP.csv", fileEncoding = "latin1")

# Convert date column to date class
data.DAP$Date <- as.Date(data.DAP$Date,
                         format = "%d/%m/%Y")

## Corrections
# Total prisoner population & General Purpose Beds
data.prisonPop <-
  read.csv(file = "www/Data/Correctives/WAPM_190106Result.csv", fileEncoding = "latin1")

# Convert date column to date class
data.prisonPop$Date <- as.Date(data.prisonPop$Date,
                               format = "%d/%m/%Y")

# Total prisoner growth rate
data.prisonerGrowth <-
  read.csv(file = "www/Data/Correctives/prisonerGrowth.csv", fileEncoding = "latin1")

# Convert date column to date class
data.prisonerGrowth$Date <- as.Date(data.prisonerGrowth$Date,
                                    format = "%d/%m/%Y")

# Sentenced Unsentenced (Data provided by Andrew Lau)
data.sentencedUnsentenced <-
  read.csv(file = 'www/Data/Correctives/sentencedUnsentenced190801.csv', fileEncoding = "latin1")

# Convert date column to date class
data.sentencedUnsentenced$Date <-
  as.Date(data.sentencedUnsentenced$Date,
          format = "%d/%m/%Y")

# DAPP Monthly Average data provided by Narelle Kinsella
data.DAPMonthly <-
  read.csv(file = 'www/Data/DAP/DAPmonthly.csv', fileEncoding = "latin1")

#The default order will be alphabetized unless specified as below:
data.DAPMonthly$Month <-
  factor(
    data.DAPMonthly$Month,
    levels = c(
      "Jan",
      "Feb",
      "Mar",
      "Apr",
      "May",
      "Jun",
      "Jul",
      "Aug",
      "Sep",
      "Oct",
      "Nov",
      "Dec"
    )
  )

# 2018 WAPM Gender Totals
data.PieGender2018 <-
  read.csv(file = 'www/Data/Correctives/PieGender2018.csv')
#Totals derived from 'WAPM Query - Yearly Data', 2018-19
# DAP_AL_M_A_00 2353.17368421053
# DAP_AL_M_N_00 3795.52631578947
# DAP_AL_F_A_00 340.042105263158
# DAP_AL_F_N_00 397.247368421053
# Total Males 6148.7
# Total Females 737.2895

# 2018 WAPM ATSI Totals
data.PieATSI2018 <-
  read.csv(file = 'www/Data/Correctives/PieATSI2018.csv')
#Totals derived from 'WAPM Query - Yearly Data', 2018-19

# 2018 WAPM Sentence Type Totals
data.PieType2018 <-
  read.csv(file = 'www/Data/Correctives/PieType2018.csv')
#Totals derived from 'WAPM Query - Yearly Data', 2018-19

## Courts
data.Court <- fread('www/Data/Courts/Court.csv')
data.CourtLodgements <- fread('www/Data/Courts/CourtLodgements.csv')

## ROGS
data.CostPerPrisonerPerDay <-
  fread('www/Data/ROGS/CostPerPrisonerPerDay.csv')
data.CommunityOrderCompletions <-
  fread('www/Data/ROGS/CommunityOrderCompletions.csv')
data.AboriginalImprisonmentRate <-
  fread('www/Data/ROGS/AboriginalImprisonmentRate.csv')
data.NonAboriginalImprisonmentRate <-
  fread('www/Data/ROGS/NonAboriginalImprisonmentRate.csv')

## ODPP
data.ODPP <-
  read.csv(file = "www/Data/ODPP/odpp.csv", fileEncoding = "latin1")

## WAPOL
data.WAPOL <-
  read.csv(file = "www/Data/WAPOL/wapol.csv", fileEncoding = "latin1")

## ABS
data.ABS <-
  read.csv(
    file = "www/Data/ABS/abs.csv",
    sep = ",",
    fileEncoding = "latin1",
    header = TRUE
  )

# ABS 45170DO002 2018 Table Crude Imprisonment Rate
data.imprisonRate <-
  read.csv(
    file = "www/Data/ABS/imprisonRate.csv",
    sep = ",",
    fileEncoding = "latin1",
    header = TRUE
  )

data.imprisonRate$State <-
  factor(data.imprisonRate$State,
         levels = c("NSW", "Vic", "Qld", "WA", "SA", "Tas", "Aust"))

# ABS 45170DO002 2018 Table Crude Imprisonment Rate Multiple Years
data.imprisonRateMulti <-
  read.csv(
    file = "www/Data/ABS/imprisonRateMulti.csv",
    sep = ",",
    fileEncoding = "latin1",
    header = TRUE
  )

# Plotting Functions
source('www/DashboardFunctions.R')

ui <- dashboardPagePlus(
  # Dashboard Header ####
  dashboardHeaderPlus(# Creating Title
    title = tagList(
      # Title when sidebar is expanded
      span(class = "logo-lg", "Law and Order Dashboard"),
      # Title when sidebar is collapsed
      fluidRow(
        img(
          src = 'https://www.dpc.wa.gov.au/GuidelinesAndPolicies/CommonBadging/PublishingImages/downloads/coaOriginal/coaOriginalColour_283x207.jpg',
          height = 20,
          width = 26
        )
      )
    )),
  # Dashboard Sidebar ####
  dashboardSidebar(
    sidebarMenu(
      id = 'tabs',
      menuItem(
        'Title Page',
        tabName = 'titlePage',
        icon = icon('home'),
        selected = TRUE
      ),
      menuItem(
        'Corrections',
        tabName = 'corrections',
        icon = icon('bar-chart-o')
      ),
      menuItem(
        'Interactive',
        tabName = 'main',
        icon = icon('file-alt')
      ),
      menuItem(
        'Courts',
        tabName = 'courts',
        icon = icon('balance-scale')
      ),
      menuItem('ROGS', tabName = 'rogs', icon = icon('dashboard')),
      menuItem('ODPP', tabName = 'odpp', icon = icon('cog')),
      menuItem('WAPOL', tabName = 'wapol', icon = icon('table')),
      menuItem('ABS', tabName = 'abs', icon = icon('calendar')),
      menuItem('SIMS', tabName = 'sims', icon = icon('bar-chart-o'))
    )
  ),
  # Dashboard Body ####
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "treasury.css?version=1")
    ),
    tabItems(
      # Title Page ####
      tabItem(tabName = 'titlePage',
              # Content
              box(
                width = 12,
                fluidRow(
                  img(
                    src = 'CoatOfArmsWA.png',
                    height = 225,
                    width = 300,
                    align = 'left',
                    hspace = 20,
                    vspace = 20
                  )
                ),
                fluidRow(
                  h1('Law and Order Chart Pack', align = 'center'),
                  h3(format(Sys.Date(), '%d %B %Y'), align = 'center')
                ),
                fluidRow(
                  img(
                    src = 'JPMGraphic.png',
                    height = 225,
                    width = 300,
                    align = 'right',
                    hspace = 20,
                    vspace = 20
                  )
                )
              )),
      #Corrections
      tabItem(
        tabName = 'corrections',
        # Content
        fluidRow(box(
          width = 4, plotlyOutput('PieGender2018', height = '35vh'),
          footer = h5('Source: ',
                      a(
                        'WAPM - Yearly Data'
                      ))          
        ),
        box(
          width = 4, plotlyOutput('PieATSI2018', height = '35vh'),
          footer = h5('Source: ',
                      a(
                        'WAPM - Yearly Data'
                      ))          
        ),
        box(
          width = 4, plotlyOutput('PieType2018', height = '35vh'),
          footer = h5('Source: ',
                      a(
                        'WAPM - Yearly Data'
                      ))          
        )),        
        fluidRow(
          box(
            width = 6,
            plotlyOutput('prisonerPopulation', height = '35vh'),
            footer = h5('Source: ',
                        a('WAPM 190106 Results',
                          target = '_blank'))
          ),
          box(
            width = 6,
            plotlyOutput('prisonerGrowth', height = '35vh'),
            footer = h5('Source: ',
                        a('WAPM 190106 Results',
                          target = '_blank'))
          )
        ),
        fluidRow(
          box(
            width = 6,
            plotlyOutput('sentencedUnsentenced', height = '35vh'),
            footer = h5('Source: ',
                        a('WAPM'))
          ),
          box(
            width = 6,
            plotlyOutput('imprisonRate', height = '35vh'),
            footer = h5('Source: ',
                        a('ABS 45170DO002_2018'))
          )
          
        ),
        fluidRow(
          box(
            width = 6,
            plotlyOutput('prisonerPopCapacity2', height = '35vh'),
            footer = h5('Source: ',
                        a('WAPM 190106 Results'))
          ),
          box(
            width = 6,
            plotlyOutput('DAPMonthly2', height = '35vh'),
            footer = h5('Source: ',
                        a(
                          'DAPP & Prison Bud - Info for Teasurer Report.xlsx'
                        ))
          )
          
        )

      ),
      # Interactive ####
      tabItem(
        tabName = 'main',
        # Content
        hr(),
        headerPanel("Interactive WAPM"),
        sidebarPanel(
          helpText("Create interactive charts"),
          selectInput(
            inputId = "AAA",
            label = "Measure",
            choices = list(
              "Arrivals" = "ARR",
              "Departures" = "DEP",
              "Daily average population" = "DAP",
              "Census prison population" = "CPP",
              "Length of stay" = "LOS"
            ),
            selected = "DAP"
          ),
          selectInput(
            inputId = "BB",
            label = "Prisoner Type",
            choices = list(
              "All prisoners" = "AL",
              "Sentenced prisoners" = "SN",
              "Unsentenced prisoners" = "UN",
              "Fine default only prisoners" = "FD"
            ),
            selected = "AL"
          ),
          selectInput(
            inputId = "C",
            label = "Gender",
            choices = list(
              "Male" = "M",
              "Female" = "F",
              "All" = "All"
            ),
            selected = "M"
          ),
          selectInput(
            inputId = "D",
            label = "Aboriginality",
            choices = list(
              "Aboriginal" = "A",
              "Non-Aboriginal" = "N",
              "All" = "All"
            ),
            selected = "A"
          ),
          selectInput(
            inputId = "ANZSOC",
            label = "Anzsoc Code",
            choices = list(
              "01 Homicide and related offences" = "01",
              "02 Acts intended to cause injury" = "02",
              "03 Sexual assault and related offences" = "03",
              "04 Dangerous or negligent acts endangering persons" = "04",
              "05 Abduction, harassment and other offences against the person" = "05",
              "06 Robbery, extortion and related offences" = "06",
              "07 Unlawful entry with intent/burglary, break and enter" = "07",
              "08 Theft and related offences" = "08",
              "09 Fraud, deception and related offences" = "09",
              "10 Illicit drug offences" = "10",
              "11 Prohibited and regulated weapons and explosives offences" = "11",
              "12 Property damage and environmental pollution" = "12",
              "13 Public order offences" = "13",
              "14 Traffic and vehicle regulatory offences" = "14",
              "15 Offences against government procedures, government security and government operations" = "15",
              "16 Miscellaneous offences" = "16",
              "17 Remand" = "17",
              "99 Unknown" = "99",
              "All ANZSOC" = "00"
            ),
            
            selected = "04"
          ),
          print('Source: WA Prisoner Model')
          #verbatimTextOutput("AAA"),
          #verbatimTextOutput("BB"),
          #verbatimTextOutput("Combo")
        ),
        mainPanel(
          plotlyOutput('trendPlot', height = "715px", width = "850px"),
          footer = h5('Source: ', 'WA Prisoner Model')
        )
      ),
      # Courts ####
      tabItem(
        tabName = 'courts',
        ## Court Lodgements ####
        fluidRow(
          # Top Left
          box(
            width = 4,
            plotlyOutput('courtLodgementsMCPlot', height = '41vh'),
            footer = h5(
              'Source: ',
              a(
                'Chapter 7 Courts - Table 7A.1, ROGS',
                href = 'https://www.pc.gov.au/research/ongoing/report-on-government-services/2019/justice/courts',
                target = '_blank'
              )
            )
          ),
          # Top Middle
          box(
            width = 4,
            plotlyOutput('courtLodgementsDCPlot', height = '41vh'),
            footer = h5(
              'Source: ',
              a(
                'Chapter 7 Courts - Table 7A.1, ROGS',
                href = 'https://www.pc.gov.au/research/ongoing/report-on-government-services/2019/justice/courts',
                target = '_blank'
              )
            )
          ),
          # Top Right
          box(
            width = 4,
            plotlyOutput('courtLodgementsSCPlot', height = '41vh'),
            footer = h5(
              'Source: ',
              a(
                'Chapter 7 Courts - Table 7A.1, ROGS',
                href = 'https://www.pc.gov.au/research/ongoing/report-on-government-services/2019/justice/courts',
                target = '_blank'
              )
            )
          )
        ),
        ## Time to Trial ####
        fluidRow(
          # Middle Left
          box(
            width = 4,
            plotlyOutput('timeToTrialMCPlot', height = '41vh'),
            footer = h5(
              'Source: ',
              a(
                'Department of Justice Annual Reports',
                href = 'https://department.justice.wa.gov.au/R/reports_publications.aspx',
                target = '_blank'
              )
            )
          ),
          # Middle Middle
          box(
            width = 4,
            plotlyOutput('timeToTrialDCPlot', height = '41vh'),
            footer = h5(
              'Source: ',
              a(
                'Department of Justice Annual Reports',
                href = 'https://department.justice.wa.gov.au/R/reports_publications.aspx',
                target = '_blank'
              )
            )
          ),
          # Middle Right
          box(
            width = 4,
            plotlyOutput('timeToTrialSCPlot', height = '41vh'),
            footer = h5(
              'Source: ',
              a(
                'Department of Justice Annual Reports',
                href = 'https://department.justice.wa.gov.au/R/reports_publications.aspx',
                target = '_blank'
              )
            )
          )
        ),
        ## Cases on Hand ####
        fluidRow(
          # Bottom Left
          box(
            width = 4,
            plotlyOutput('casesOnHandMCPlot', height = '41vh'),
            footer = h5(
              'Source: ',
              a(
                'Chapter 7 Courts - Table 7A.17, ROGS',
                href = 'https://www.pc.gov.au/research/ongoing/report-on-government-services/2019/justice/courts',
                target = '_blank'
              )
            )
          ),
          # Bottom Middle
          box(
            width = 4,
            plotlyOutput('casesOnHandDCPlot', height = '41vh'),
            footer = h5(
              'Source: ',
              a(
                'Chapter 7 Courts - Table 7A.17, ROGS',
                href = 'https://www.pc.gov.au/research/ongoing/report-on-government-services/2019/justice/courts',
                target = '_blank'
              )
            )
          ),
          # Bottom Right
          box(
            width = 4,
            plotlyOutput('casesOnHandSCPlot', height = '41vh'),
            footer = h5(
              'Source: ',
              a(
                'Chapter 7 Courts - Table 7A.17, ROGS',
                href = 'https://www.pc.gov.au/research/ongoing/report-on-government-services/2019/justice/courts',
                target = '_blank'
              )
            )
          )
        )
      ),
      # ROGS ####
      tabItem(tabName = 'rogs',
              fluidRow(
                ## Year Picker ####
                box(
                  width = 12,
                  pickerInput(
                    inputId = 'rogsYearPicker',
                    label = 'Choose a Financial Year:',
                    choices = data.CostPerPrisonerPerDay[, Year],
                    selected = data.CostPerPrisonerPerDay[, tail(Year, 1)],
                    inline = TRUE
                  )
                ),
                ## Cost per Prisoner per Day ####
                # Top Left
                box(
                  width = 6,
                  plotlyOutput('rogsCostPerPrisonerPerDayPlot', height = '32vh'),
                  footer = h5(
                    'Source: ',
                    a(
                      'Chapter 8 Corrective Services - Tables 8A.18, Report on Government Services 2019',
                      href = 'https://www.pc.gov.au/research/ongoing/report-on-government-services/2019/justice/corrective-services',
                      target = '_blank'
                    )
                  )
                ),
                ## Community Corrections Orders Completion Rate ####
                # Top Right
                box(
                  width = 6,
                  plotlyOutput('rogsCommunityCompletionRatePlot', height = '32vh'),
                  footer = h5(
                    'Source: ',
                    a(
                      'Chapter 8 Corrective Services - Table 8A.19, Report on Government Services 2019',
                      href = 'https://www.pc.gov.au/research/ongoing/report-on-government-services/2019/justice/corrective-services',
                      target = '_blank'
                    )
                  )
                )
              ),
              fluidRow(
                ## Aboriginal Imprisonment Rate ####
                # Bottom Left
                box(
                  width = 6,
                  plotlyOutput('rogsATSIImprRatePlot', height = '32vh'),
                  footer = h5(
                    'Source: ',
                    a(
                      'Chapter 8 Corrective Services - Table 8A.6, Report on Government Services 2019',
                      href = 'https://www.pc.gov.au/research/ongoing/report-on-government-services/2019/justice/corrective-services',
                      target = '_blank'
                    )
                  )
                ),
                ## Non-Aboriginal Imprisonment Rate ####
                # Bottom Right
                box(
                  width = 6,
                  plotlyOutput('rogsNonATSIImprRatePlot', height = '32vh'),
                  footer = h5(
                    'Source: ',
                    a(
                      'Chapter 8 Corrective Services - Table 8A.6, Report on Government Services 2019',
                      href = 'https://www.pc.gov.au/research/ongoing/report-on-government-services/2019/justice/corrective-services',
                      target = '_blank'
                    )
                  )
                )
              )),
      #ODPP
      tabItem(
        tabName = 'odpp',
        # Content
        fluidRow(
          # Top Left
          box(
            width = 4,
            plotlyOutput('newCasesPMC', height = '41vh'),
            footer = h5(
              'Source: ',
              a(
                'ODPP-Annual-Report-2017',
                href = 'https://www.dpp.wa.gov.au/_files/annual-reports/ODPP-Annual-Report-2017-2018%20.pdf',
                target = '_blank'
              )
            )
          ),
          # Top Middle
          box(
            width = 4,
            plotlyOutput('casesCompletedPMC', height = '41vh'),
            footer = h5(
              'Source: ',
              a(
                'ODPP-Annual-Report-2017',
                href = 'https://www.dpp.wa.gov.au/_files/annual-reports/ODPP-Annual-Report-2017-2018%20.pdf',
                target = '_blank'
              )
            )
          ),
          # Top Right
          box(
            width = 4,
            plotlyOutput('newCasesSGMC', height = '41vh'),
            footer = h5(
              'Source: ',
              a(
                'ODPP-Annual-Report-2017',
                href = 'https://www.dpp.wa.gov.au/_files/annual-reports/ODPP-Annual-Report-2017-2018%20.pdf',
                target = '_blank'
              )
            )
          )
        ),
        fluidRow(
          # Middle Left
          box(
            width = 4,
            plotlyOutput('casesCompletedSGMC', height = '41vh'),
            footer = h5(
              'Source: ',
              a(
                'ODPP-Annual-Report-2017',
                href = 'https://www.dpp.wa.gov.au/_files/annual-reports/ODPP-Annual-Report-2017-2018%20.pdf',
                target = '_blank'
              )
            )
          ),
          # Middle Middle
          box(
            width = 4,
            plotlyOutput('committalsPMMC', height = '41vh'),
            footer = h5(
              'Source: ',
              a(
                'ODPP-Annual-Report-2017',
                href = 'https://www.dpp.wa.gov.au/_files/annual-reports/ODPP-Annual-Report-2017-2018%20.pdf',
                target = '_blank'
              )
            )
          ),
          # Middle Right
          box(
            width = 4,
            plotlyOutput('committalsSGMC', height = '41vh'),
            footer = h5(
              'Source: ',
              a(
                'ODPP-Annual-Report-2017',
                href = 'https://www.dpp.wa.gov.au/_files/annual-reports/ODPP-Annual-Report-2017-2018%20.pdf',
                target = '_blank'
              )
            )
          )
        ),
        fluidRow(
          # Bottom Left
          box(
            width = 4,
            plotlyOutput('committalsRMC', height = '41vh'),
            footer = h5(
              'Source: ',
              a(
                'ODPP-Annual-Report-2017',
                href = 'https://www.dpp.wa.gov.au/_files/annual-reports/ODPP-Annual-Report-2017-2018%20.pdf',
                target = '_blank'
              )
            )
          ),
          # Bottom Middle
          box(
            width = 4,
            plotlyOutput('committedForSentence', height = '41vh'),
            footer = h5(
              'Source: ',
              a(
                'ODPP-Annual-Report-2017',
                href = 'https://www.dpp.wa.gov.au/_files/annual-reports/ODPP-Annual-Report-2017-2018%20.pdf',
                target = '_blank'
              )
            )
          ),
          # Bottom Right
          box(
            width = 4,
            plotlyOutput('committedForTrial', height = '41vh'),
            footer = h5(
              'Source: ',
              a(
                'ODPP-Annual-Report-2017',
                href = 'https://www.dpp.wa.gov.au/_files/annual-reports/ODPP-Annual-Report-2017-2018%20.pdf',
                target = '_blank'
              )
            )
          )
        )
      ),
      #WAPOL
      tabItem(tabName = 'wapol',
              # Content
              fluidRow(
                box(
                  width = 6,
                  plotlyOutput('totalSelectedOffencesPerMonth', height = '35vh'),
                  footer = h5(
                    'Source: ',
                    a('WAPOL',
                      href = 'https://www.police.wa.gov.au/Crime/CrimeStatistics', target = '_blank')
                  )
                ),
                box(
                  width = 6,
                  plotlyOutput('sexualOffences', height = '35vh'),
                  footer = h5(
                    'Source: ',
                    a('WAPOL',
                      href = 'https://www.police.wa.gov.au/Crime/CrimeStatistics', target = '_blank')
                  )
                )
              ),
              fluidRow(
                box(
                  width = 6,
                  plotlyOutput('totalSelectedOffencesAgainstThePerson', height = '35vh'),
                  footer = h5(
                    'Source: ',
                    a('WAPOL',
                      href = 'https://www.police.wa.gov.au/Crime/CrimeStatistics', target = '_blank')
                  )
                ),
                box(
                  width = 6,
                  plotlyOutput('totalSelectedOffencesAgainstProperty', height = '35vh'),
                  footer = h5(
                    'Source: ',
                    a('WAPOL',
                      href = 'https://www.police.wa.gov.au/Crime/CrimeStatistics', target = '_blank')
                  )
                )
              )),
      #ABS
      tabItem(tabName = 'abs',
              # Content
              fluidRow(
                box(
                  width = 6,
                  plotlyOutput('phyAssVicRateWA', height = '35vh'),
                  footer = h5(
                    'Source: ',
                    a('ABS',
                      href = 'https://www.abs.gov.au/', target = '_blank')
                  )
                ),
                box(
                  width = 6,
                  plotlyOutput('threatAssVicRateWA', height = '35vh'),
                  footer = h5(
                    'Source: ',
                    a('ABS',
                      href = 'https://www.abs.gov.au/', target = '_blank')
                  )
                )
              ),
              fluidRow(
                box(
                  width = 6,
                  plotlyOutput('phyAssRateWA', height = '35vh'),
                  footer = h5(
                    'Source: ',
                    a('ABS',
                      href = 'https://www.abs.gov.au/', target = '_blank')
                  )
                ),
                
                box(
                  width = 6,
                  plotlyOutput('totAssRateWA', height = '35vh'),
                  footer = h5(
                    'Source: ',
                    a('ABS',
                      href = 'https://www.abs.gov.au/', target = '_blank')
                  )
                )
              )),
      #SIMS
      tabItem(tabName = 'sims',
              # Content
              fluidRow(box(
                width = 6,
                footer = h5(
                  'Source: ',
                  a('SIMS',
                    href = 'http://www.treasury.wa.gov.au/sims/', target = '_blank')
                )
              )))
    )
  )
)

# server #
server <- function(input, output) {
  # Summary ####
  data1 <- reactive({
    input$AAA
  })
  data2 <- reactive({
    input$BB
  })
  data3 <- reactive({
    input$C
  })
  data4 <- reactive({
    input$D
  })
  data5 <- reactive({
    input$ANZSOC
  })
  # output$AAA <- renderPrint({
  #   data1()
  # })
  # output$BB <- renderPrint({
  #   data2()
  # })
  dataCombo <- renderText({
    paste(data1(), data2(), data3(), data4(), data5(), sep = "_")
  })
  output$Combo <- renderPrint({
    dataCombo()
  })
  
  output$trendPlot <- renderPlotly({
    p <- plot_ly(data.DAP,
                 x = ~ Date,
                 y = data.DAP[, dataCombo()],
                 #y = data.DAP[,ARR_AL_F_All_00],
                 type = 'bar') %>%
      layout(
        title = list(text = "WA Prisoner Counts"),
        font = list(color = "blue"),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Prisoner Count")
      )
    #%>% config(displayModeBar = F)
  })
  
  #### Corrections ####
  ## prisonerPopulation ####
  
  output$prisonerPopulation <- renderPlotly({
    p <- plot_ly(
      data.prisonPop,
      x = ~ Date,
      y = ~ DAP_Mean,
      type = 'scatter',
      mode = 'lines',
      line = list(color = 'rgb(91,155,213)'),
      fill = 'tozeroy',
      fillcolor = 'rgba(50,117,168,1)'
    ) %>%
      layout(
        title = list(text = "Adult Prisoner Population"),
        xaxis = list(
          title = "",
          dtick = 0,
          range = c(as.Date('2010-01-01'), as.Date('2018-12-01'))
        ),
        yaxis = list(title = "Prisoner Population", range = c(3900, 7600))
      ) %>%
      config(displayModeBar = F)
  })
  
  output$prisonerGrowth <- renderPlotly({
    p <- plot_ly(data.prisonerGrowth) %>%
      # Add growth rate
      add_trace(
        name = 'Prisoner Growth',
        x = ~ Date,
        y = ~ growthRate,
        type = 'bar',
        hoverinfo = "none",
        text = data.prisonerGrowth[, 3],
        textposition = 'outside',
        #marker = list(color = 'rgb(158,202,225)',
        #line = list(color = 'rgb(8,48,107)', width = 1.5))
        marker = list(color = 'rgb(50,117,168)')
        # )%>%
        # add_annotations(
        #   x = data.prisonerGrowth[,3],
        #   y = data.prisonerGrowth[,4],
        #   text = paste0(data.prisonerGrowth[,3]),
        #   #xref = 'x',
        #   #yref = 'y',
        #   showarrow = FALSE,
        #   yanchor = 'bottom'
      ) %>%
      add_trace(
        name = 'Average Growth',
        x = ~ Date,
        y = ~ average,
        type = 'scatter',
        mode = 'lines',
        line = list(color = 'rgb(0,0,0)'),
        # turn off hover info
        hoverinfo = "none"
      ) %>%
      
      layout(
        title = list(text = "Adult Prisoner Population Annual Growth Rate"),
        #font = list(color = "blue"),
        xaxis = list(title = "", dtick = 0),
        yaxis = list(title = "Growth Rate %", range = c(-15, 39))
      ) %>%
      layout(legend = list(x = 0.7, y = 0.9)) %>%
      config(displayModeBar = F)
    
    
  })
  # ABS Table 19 2018 Crude imprisonment rate
  output$imprisonRate <- renderPlotly({
    p <- plot_ly(data.imprisonRate) %>%
      # Add imprisonment rates
      add_trace(
        name = 'State rate',
        x = ~ State,
        y = ~ rate,
        type = 'bar',
        hoverinfo = "none",
        text = data.imprisonRate[, 2],
        marker = list(
          color = c(
            'rgba(50,117,168,1)',
            'rgba(50,117,168,1)',
            'rgba(50,117,168,1)',
            'rgba(50,117,168,1)',
            'rgba(235,134,52,1)',
            'rgba(50,117,168,1)',
            'rgba(50,117,168,1)'
          )
        ),
        textposition = 'outside'
      ) %>%
      add_trace(
        name = 'Australian rate',
        x = ~ State,
        y = ~ Aust,
        type = 'scatter',
        mode = 'lines',
        line = list(color = 'rgb(0,0,0)'),
        hoverinfo = "none"
      ) %>%
      layout(
        title = list(text = "Crude Imprisonment Rate, 2018"),
        #font = list(color = "blue"),
        xaxis = list(title = "",
                     dtick = 0),
        yaxis = list(title = "Rate per 100k", range = c(0, 500))
      ) %>%
      layout(legend = list(x = 0.8, y = 0.9)) %>%
      config(displayModeBar = F)
    
  })
  
  ## prisonerPopulation & Capacity ####
  output$prisonerPopCapacity <- renderPlotly({
    p <- plot_ly(
      data.prisonPop,
      x = ~ Date,
      y = ~ DAP_Mean,
      type = 'bar'
    ) %>%
      layout(
        title = list(text = "WAPM Prisoner Population v Capacity"),
        #font = list(color = "blue"),
        xaxis = list(title = "", dtick = 0),
        yaxis = list(title = "Prisoner Population", range = c(0, 10000))
      ) %>%
      config(displayModeBar = F)
  })
  
  ## prisonerPopulation & Capacity2 ####
  output$prisonerPopCapacity2 <- renderPlotly({
    p <- plot_ly(data.prisonPop) %>%
      # Add Prison Pop
      add_trace(
        name = 'Prison Population',
        x = ~ Date,
        y = ~ DAP_Mean,
        type = 'scatter',
        mode = 'lines',
        line = list(color = 'rgb(91,155,213)'),
        fill = 'tozeroy',
        fillcolor = 'rgba(91,155,213,1)'
      ) %>%
      # Add capacity
      add_trace(
        name = 'Capacity',
        x = ~ Date,
        y = ~ totGenPurBeds,
        type = 'scatter',
        mode = 'lines',
        line = list(color = 'rgb(168,107,50)')
        #fill = 'tozeroy',
        #fillcolor = 'rgba(150,117,168,1)'
      ) %>%
      layout(
        title = list(text = "WAPM Prisoner Population v Capacity"),
        #font = list(color = "blue"),
        xaxis = list(
          title = "",
          dtick = 0,
          range = c(as.Date('2016-01-01'), as.Date('2019-12-01'))
        ),
        yaxis = list(title = "Prisoner Population", range = c(4500, 10000))
      ) %>%
      #layout(showlegend = FALSE)
      layout(legend = list(x = 0.1, y = 0.9)) %>%
      config(displayModeBar = F)
    
  })
  
  ## Sentenced Unsentenced ####
  output$sentencedUnsentenced <- renderPlotly({
    p <- plot_ly(data.sentencedUnsentenced) %>%
      # Unsentenced
      add_trace(
        name = 'Sentenced',
        x = ~ Date,
        y = ~ Sentenced,
        type = 'bar'
      ) %>%
      # Sentenced
      add_trace(
        name = 'Unsentenced',
        x = ~ Date,
        y = ~ Unsentenced,
        type = 'bar'
      ) %>%
      layout(
        title = list(text = "Adult Prison Population by Sentence Status"),
        #font = list(color = "blue"),
        xaxis = list(title = "", dtick = 0),
        yaxis = list(title = "", range = c(0, 6000))
      ) %>%
      #layout(showlegend = FALSE)
      layout(legend = list(x = 0.1, y = 0.9)) %>%
      config(displayModeBar = F)
  })
  
  # Courts ####
  ## Court Lodgements ####
  output$courtLodgementsMCPlot <- renderPlotly({
    plot.line.single(
      data.Court[, c('Year', 'CourtLodgementsMC')],
      '<b>Court Lodgements: MC</b>',
      colorNo = 1,
      xAxisTitle = '',
      xAxisTitleHover = 'Year',
      yAxisTitle = 'Lodgements',
      margin = list(l = 60, t = 40, b = 0)
    ) %>%
      layout(xaxis = list(categoryorder = 'trace', dtick = 2),
             yaxis = list(range = c(
               min(data.Court[, 'CourtLodgementsMC']) * 0.9, max(data.Court[, 'CourtLodgementsMC']) *
                 1.1
             )))
  })
  output$courtLodgementsDCPlot <- renderPlotly({
    plot.line.single(
      data.Court[, c('Year', 'CourtLodgementsDC')],
      '<b>Court Lodgements: DC</b>',
      colorNo = 1,
      xAxisTitle = '',
      xAxisTitleHover = 'Year',
      yAxisTitle = 'Lodgements',
      margin = list(l = 60, t = 40, b = 0)
    ) %>%
      layout(xaxis = list(categoryorder = 'trace', dtick = 2),
             yaxis = list(range = c(
               min(data.Court[, 'CourtLodgementsDC']) * 0.9, max(data.Court[, 'CourtLodgementsDC']) *
                 1.1
             )))
  })
  output$courtLodgementsSCPlot <- renderPlotly({
    plot.line.single(
      data.Court[, c('Year', 'CourtLodgementsSC')],
      '<b>Court Lodgements: SC</b>',
      colorNo = 1,
      xAxisTitle = '',
      xAxisTitleHover = 'Year',
      yAxisTitle = 'Lodgements',
      margin = list(l = 60, t = 40, b = 0)
    ) %>%
      layout(xaxis = list(categoryorder = 'trace', dtick = 2),
             yaxis = list(range = c(
               min(data.Court[, 'CourtLodgementsSC']) * 0.9, max(data.Court[, 'CourtLodgementsSC']) *
                 1.1
             )))
  })
  ## Time to Trial ####
  output$timeToTrialMCPlot <- renderPlotly({
    plot.bar.vert(
      data.Court[, c('Year', 'TimeToTrialMC')],
      '<b>Time to Trial: MC</b>',
      colorNo = 1,
      xAxisTitle = '',
      yAxisTitle = 'Weeks',
      margin = list(l = 60, t = 40, b = 0)
    ) %>%
      add_segments(
        name = 'Target',
        x = data.Court[, head(Year, 1)],
        xend = data.Court[, tail(Year, 1)],
        y = 19,
        yend = 19,
        showlegend = FALSE,
        line = list(color = '#ff7f0e')
      ) %>%
      layout(xaxis = list(categoryorder = 'trace', dtick = 2))
  })
  output$timeToTrialDCPlot <- renderPlotly({
    plot.bar.vert(
      data.Court[, c('Year', 'TimeToTrialDC')],
      '<b>Time to Trial: DC</b>',
      colorNo = 1,
      xAxisTitle = '',
      yAxisTitle = 'Weeks',
      margin = list(l = 60, t = 40, b = 0)
    ) %>%
      add_segments(
        name = 'Target',
        x = data.Court[, head(Year, 1)],
        xend = data.Court[, tail(Year, 1)],
        y = 32,
        yend = 32,
        showlegend = FALSE,
        line = list(color = '#ff7f0e')
      ) %>%
      layout(xaxis = list(categoryorder = 'trace', dtick = 2))
  })
  output$timeToTrialSCPlot <- renderPlotly({
    plot.bar.vert(
      data.Court[, c('Year', 'TimeToTrialSC')],
      '<b>Time to Trial: SC</b>',
      colorNo = 1,
      xAxisTitle = '',
      yAxisTitle = 'Weeks',
      margin = list(l = 60, t = 40, b = 0)
    ) %>%
      add_segments(
        name = 'Target',
        x = data.Court[, head(Year, 1)],
        xend = data.Court[, tail(Year, 1)],
        y = 28,
        yend = 28,
        showlegend = FALSE,
        line = list(color = '#ff7f0e')
      ) %>%
      layout(xaxis = list(categoryorder = 'trace', dtick = 2))
  })
  ## Cases on Hand ####
  output$casesOnHandMCPlot <- renderPlotly({
    plot.line.single(
      data.Court[, c('Year', 'CasesOnHandMC')],
      '<b>Cases on Hand: MC</b>',
      colorNo = 3,
      xAxisTitle = '',
      xAxisTitleHover = 'Year',
      yAxisTitle = 'Cases on Hand',
      margin = list(l = 60, t = 40, b = 0)
    ) %>%
      layout(xaxis = list(categoryorder = 'trace', dtick = 2))
  })
  output$casesOnHandDCPlot <- renderPlotly({
    plot.line.single(
      data.Court[, c('Year', 'CasesOnHandDC')],
      '<b>Cases on Hand: DC</b>',
      colorNo = 3,
      xAxisTitle = '',
      xAxisTitleHover = 'Year',
      yAxisTitle = 'Cases on Hand',
      margin = list(l = 60, t = 40, b = 0)
    ) %>%
      layout(xaxis = list(categoryorder = 'trace', dtick = 2))
  })
  output$casesOnHandSCPlot <- renderPlotly({
    plot.line.single(
      data.Court[, c('Year', 'CasesOnHandSC')],
      '<b>Cases on Hand: SC</b>',
      colorNo = 3,
      xAxisTitle = '',
      xAxisTitleHover = 'Year',
      yAxisTitle = 'Cases on Hand',
      margin = list(l = 60, t = 40, b = 0)
    ) %>%
      layout(xaxis = list(categoryorder = 'trace', dtick = 2))
  })
  
  # ROGS ####
  ## Cost per Prisoner per Day ####
  rogsCostPerPrisonerPerDayPlot <-
    eventReactive(input$rogsYearPicker, {
      plot.bar.vert(
        data = data.frame(
          Category = colnames(data.CostPerPrisonerPerDay)[-1],
          Output = c(t(data.CostPerPrisonerPerDay[Year == input$rogsYearPicker, .(NSW, Vic, Qld, WA, SA, Tas)]))
        ),
        name = paste0(
          '<b>',
          input$rogsYearPicker,
          ' Cost per Prisoner per Day by State</b>'
        ),
        colorNo = c(1, 1, 1, 2, 1, 1),
        xAxisTitle = '',
        yAxisTitle = 'Cost per Prisoner per Day',
        margin = list(l = 60, t = 40, b = 0)
      )
    })
  output$rogsCostPerPrisonerPerDayPlot <- renderPlotly({
    rogsCostPerPrisonerPerDayPlot()
  })
  ## Community Order Completion Rate ####
  rogsCommunityCompletionRatePlot <-
    eventReactive(input$rogsYearPicker, {
      plot.bar.vert(
        data = data.frame(
          Category = colnames(data.CommunityOrderCompletions)[-1],
          Output = c(t(data.CommunityOrderCompletions[Year == input$rogsYearPicker, .(NSW, Vic, Qld, WA, SA, Tas)]))
        ),
        name = paste0(
          '<b>',
          input$rogsYearPicker,
          ' Community Correction Orders Completion Rate</b>'
        ),
        colorNo = c(1, 1, 1, 2, 1, 1),
        xAxisTitle = '',
        yAxisTitle = 'Completion Rate',
        formatType = 'Percent',
        margin = list(l = 60, t = 40, b = 0)
      )
    })
  output$rogsCommunityCompletionRatePlot <- renderPlotly({
    rogsCommunityCompletionRatePlot()
  })
  ## Aboriginal Crude Imprisonment Rate ####
  rogsATSIImprRatePlot <- eventReactive(input$rogsYearPicker, {
    plot.bar.vert(
      data = data.frame(
        Category = colnames(data.AboriginalImprisonmentRate)[-1],
        Output = c(t(data.AboriginalImprisonmentRate[Year == input$rogsYearPicker, .(NSW, Vic, Qld, WA, SA, Tas)]))
      ),
      name = paste0(
        '<b>',
        input$rogsYearPicker,
        ' Aboriginal Crude Imprisonment Rate per 100,000</b>'
      ),
      colorNo = c(1, 1, 1, 2, 1, 1),
      xAxisTitle = '',
      yAxisTitle = 'Rate per 100,000',
      formatType = 'Numeric',
      margin = list(l = 60, t = 40, b = 0)
    )
  })
  output$rogsATSIImprRatePlot <- renderPlotly({
    rogsATSIImprRatePlot()
  })
  ## Non-Aboriginal Crude Imprisonment Rate ####
  rogsNonATSIImprRatePlot <- eventReactive(input$rogsYearPicker, {
    plot.bar.vert(
      data = data.frame(
        Category = colnames(data.NonAboriginalImprisonmentRate)[-1],
        Output = c(t(
          data.NonAboriginalImprisonmentRate[Year == input$rogsYearPicker, .(NSW, Vic, Qld, WA, SA, Tas)]
        ))
      ),
      name = paste0(
        '<b>',
        input$rogsYearPicker,
        ' Non-Aboriginal Crude Imprisonment Rate per 100,000</b>'
      ),
      colorNo = c(1, 1, 1, 2, 1, 1),
      xAxisTitle = '',
      yAxisTitle = 'Rate per 100,000',
      formatType = 'Numeric',
      margin = list(l = 60, t = 40, b = 0)
    )
  })
  output$rogsNonATSIImprRatePlot <- renderPlotly({
    rogsNonATSIImprRatePlot()
  })
  
  # ODPP ####
  ## New Cases Received for Prosecution PMC ####
  output$newCasesPMC <- renderPlotly({
    plot.bar.vert(
      data.ODPP[, c('Year', 'newCasesRecdProsPMC')],
      '<b>New Cases Received for Prosecution: PMC</b>',
      colorNo = 1,
      xAxisTitle = '',
      #xAxisTitleHover = 'Year',
      yAxisTitle = 'Cases',
      margin = list(l = 60, t = 40, b = 0)
    ) %>%
      layout(xaxis = list(categoryorder = 'trace', dtick = 1))
  })
  ## Cases Completed PMC
  output$casesCompletedPMC <- renderPlotly({
    plot.bar.vert(
      data.ODPP[, c('Year', 'casesCompletedPMC')],
      '<b>Cases Completed: PMC</b>',
      colorNo = 1,
      xAxisTitle = '',
      yAxisTitle = 'Cases',
      margin = list(l = 60, t = 40, b = 0)
    ) %>%
      layout(xaxis = list(categoryorder = 'trace', dtick = 1))
  })
  ## New Cases Rec'd (Prosecution) SGMC ####
  output$newCasesSGMC <- renderPlotly({
    plot.bar.vert(
      data.ODPP[, c('Year', 'newCasesRecdProsSGMC')],
      '<b>New Cases (Prosecution): SGMC</b>',
      colorNo = 1,
      xAxisTitle = '',
      #xAxisTitleHover = 'Year',
      yAxisTitle = 'Cases',
      margin = list(l = 60, t = 40, b = 0)
    ) %>%
      layout(xaxis = list(categoryorder = 'trace', dtick = 1))
  })
  ## Cases Completed SGMC
  output$casesCompletedSGMC <- renderPlotly({
    plot.bar.vert(
      data.ODPP[, c('Year', 'casesConcludedSGMC')],
      '<b>Cases Concluded: SGMC</b>',
      colorNo = 1,
      xAxisTitle = '',
      yAxisTitle = 'Cases',
      margin = list(l = 60, t = 40, b = 0)
    ) %>%
      layout(xaxis = list(categoryorder = 'trace', dtick = 1))
  })
  ## Committals PMMC
  output$committalsPMMC <- renderPlotly({
    plot.bar.vert(
      data.ODPP[, c('Year', 'committalsPMMC')],
      '<b>Committals PMMC</b>',
      colorNo = 1,
      xAxisTitle = '',
      yAxisTitle = 'Committals',
      margin = list(l = 60, t = 40, b = 0)
    ) %>%
      layout(xaxis = list(categoryorder = 'trace', dtick = 1))
  })
  ## Committals SGMC
  output$committalsSGMC <- renderPlotly({
    plot.bar.vert(
      data.ODPP[, c('Year', 'committalsSGMC')],
      '<b>Committals SGMC</b>',
      colorNo = 1,
      xAxisTitle = '',
      yAxisTitle = 'Committals',
      margin = list(l = 60, t = 40, b = 0)
    ) %>%
      layout(xaxis = list(categoryorder = 'trace', dtick = 1))
  })
  ## Committals RMC
  output$committalsRMC <- renderPlotly({
    plot.bar.vert(
      data.ODPP[, c('Year', 'committalsRMC')],
      '<b>Committals RMC</b>',
      colorNo = 1,
      xAxisTitle = '',
      yAxisTitle = 'Committals',
      margin = list(l = 60, t = 40, b = 0)
    ) %>%
      layout(xaxis = list(categoryorder = 'trace', dtick = 1))
  })
  ## Committals by Type Magistrates Court
  output$committedForSentence <- renderPlotly({
    plot.bar.vert(
      data.ODPP[, c('Year', 'committedForSentence')],
      '<b>Committed For Sentence</b>',
      colorNo = 1,
      xAxisTitle = '',
      yAxisTitle = 'Committals',
      margin = list(l = 60, t = 40, b = 0)
    ) %>%
      layout(xaxis = list(categoryorder = 'trace', dtick = 1))
  })
  ## Committals by Type Magistrates Court
  output$committedForTrial <- renderPlotly({
    plot.bar.vert(
      data.ODPP[, c('Year', 'committedForTrial')],
      '<b>Committed For Trial</b>',
      colorNo = 1,
      xAxisTitle = '',
      yAxisTitle = 'Committals',
      margin = list(l = 60, t = 40, b = 0)
    ) %>%
      layout(xaxis = list(categoryorder = 'trace', dtick = 1))
  })
  # WAPOL ####
  ## Total Selected Offences Per Month ####
  output$totalSelectedOffencesPerMonth <- renderPlotly({
    plot.line.single(
      data.WAPOL[, c('monthYear', 'totalSelectedOffencesPerMonth')],
      '<b>WA Total Selected Offences Per Month</b>',
      colorNo = 1,
      xAxisTitle = '',
      xAxisTitleHover = 'monthYear',
      yAxisTitle = 'Offences',
      margin = list(l = 60, t = 40, b = 0)
    ) %>%
      layout(
        xaxis = list(categoryorder = 'trace', dtick = 30),
        yaxis = list(range = c(10000, 30000))
      )
  })
  ## Sexual Offences ####
  output$sexualOffences <- renderPlotly({
    plot.line.single(
      data.WAPOL[, c('monthYear', 'sexualOffences')],
      '<b>WA Sexual Offences</b>',
      colorNo = 1,
      xAxisTitle = '',
      xAxisTitleHover = 'monthYear',
      yAxisTitle = 'Offences',
      margin = list(l = 60, t = 40, b = 0)
    ) %>%
      layout(
        xaxis = list(categoryorder = 'trace', dtick = 30),
        yaxis = list(range = c(0, 1000))
      )
  })
  ## Total Selected Offences Against The Person ####
  output$totalSelectedOffencesAgainstThePerson <- renderPlotly({
    plot.line.single(
      data.WAPOL[, c('monthYear', 'totalSelectedOffencesAgainstThePerson')],
      '<b>WA Total selected offences against the Person</b>',
      colorNo = 1,
      xAxisTitle = '',
      xAxisTitleHover = 'monthYear',
      yAxisTitle = 'Offences',
      margin = list(l = 60, t = 40, b = 0)
    ) %>%
      layout(
        xaxis = list(categoryorder = 'trace', dtick = 30),
        yaxis = list(range = c(0, 5000))
      )
  })
  ## Total Selected Offences Against The Property ####
  output$totalSelectedOffencesAgainstProperty <- renderPlotly({
    plot.line.single(
      data.WAPOL[, c('monthYear', 'totalSelectedOffencesAgainstProperty')],
      '<b>WA Total Selected Offences against Property</b>',
      colorNo = 1,
      xAxisTitle = '',
      xAxisTitleHover = 'monthYear',
      yAxisTitle = 'Offences',
      margin = list(l = 60, t = 40, b = 0)
    ) %>%
      layout(
        xaxis = list(categoryorder = 'trace', dtick = 30),
        yaxis = list(range = c(4000, 20000))
      )
  })
  # ABS ####
  ## Physical assault victimisation rate ####
  output$phyAssVicRateWA <- renderPlotly({
    plot.line.single(
      data.ABS[, c('Date', 'phyAssVicRateWA')],
      '<b>WA Physical assault victimisation rate</b>',
      colorNo = 1,
      xAxisTitle = 'Year',
      xAxisTitleHover = 'Date',
      yAxisTitle = 'Rate',
      margin = list(l = 60, t = 40, b = 0)
    ) %>%
      layout(xaxis = list(categoryorder = 'trace', dtick = 2))
  })
  ## Threatened assault victimisation rate ####
  output$threatAssVicRateWA <- renderPlotly({
    plot.line.single(
      data.ABS[, c('Date', 'threatAssVicRateWA')],
      '<b>WA Threatened assault victimisation rate</b>',
      colorNo = 1,
      xAxisTitle = 'Year',
      xAxisTitleHover = 'Date',
      yAxisTitle = 'Rate',
      margin = list(l = 60, t = 40, b = 0)
    ) %>%
      layout(xaxis = list(categoryorder = 'trace', dtick = 2))
  })
  ## Physical assault rate ####
  output$phyAssRateWA <- renderPlotly({
    plot.line.single(
      data.ABS[, c('Date', 'phyAssRateWA')],
      '<b>WA Physical assault reporting rate</b>',
      colorNo = 1,
      xAxisTitle = 'Year',
      xAxisTitleHover = 'Date',
      yAxisTitle = 'Rate',
      margin = list(l = 60, t = 40, b = 0)
    ) %>%
      layout(xaxis = list(categoryorder = 'trace', dtick = 2))
  })
  ## Total assault rate ####
  output$totAssRateWA <- renderPlotly({
    plot.line.single(
      data.ABS[, c('Date', 'totAssRateWA')],
      '<b>WA Total assault victimisaton rate</b>',
      colorNo = 1,
      xAxisTitle = 'Year',
      xAxisTitleHover = '',
      yAxisTitle = 'Rate',
      margin = list(l = 60, t = 40, b = 0)
    ) %>%
      layout(xaxis = list(categoryorder = 'trace', dtick = 2))
  })
  ## DAPMonthly
  output$DAPMonthly <- renderPlotly({
    plot.line.single(
      data.DAPMonthly[, c('Month', 'DAP2015')],
      '<b>WA DAP Monthly</b>',
      xAxisTitle = 'Year',
      #xAxisTitleHover = '',
      yAxisTitle = 'DAP',
      margin = list(l = 60, t = 40, b = 0)
    ) %>%
      layout(xaxis = list(categoryorder = 'trace'))
  })
  
  output$DAPMonthly2 <- renderPlotly({
    p <- plot_ly(data.DAPMonthly) %>%
      #2019
      add_trace(
        name = '2019',
        x = ~ Month,
        y = ~ DAP2019,
        type = 'scatter',
        mode = 'lines'
      ) %>%
      #2018
      add_trace(
        name = '2018',
        x = ~ Month,
        y = ~ DAP2018,
        type = 'scatter',
        mode = 'lines'
      ) %>%
      #2017
      add_trace(
        name = '2017',
        x = ~ Month,
        y = ~ DAP2017,
        type = 'scatter',
        mode = 'lines'
      ) %>%
      #2016
      add_trace(
        name = '2016',
        x = ~ Month,
        y = ~ DAP2016,
        type = 'scatter',
        mode = 'lines'
      ) %>%
      #2015
      add_trace(
        name = '2015',
        x = ~ Month,
        y = ~ DAP2015,
        type = 'scatter',
        mode = 'lines'
      ) %>%
      layout(
        title = list(text = 'Monthly DAP: January 2015 - Present'),
        xaxis = list(title = "", dtick = 0),
        yaxis = list(title = "", range = c(4900, 7500)),
        legend = list(x = 100, y = 0.5)
      ) %>%
      config(displayModeBar = F)
  })
  
  ## Pie chart 2018 Gender
  output$PieGender2018 <- renderPlotly({
    p <- plot_ly(
      data.PieGender2018,
      labels = ~ Category,
      values = ~ Total,
      hoverinfo = "none",
      hole = 0.5,
      type = 'pie'
    ) %>%
      layout(
        title = list(text = "2018-19 WAPM Gender Breakdown"),
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
      )%>%
      config(displayModeBar = F)
    
  })
  
  ## Pie chart 2018 ATSI
  output$PieATSI2018 <- renderPlotly({
    p <- plot_ly(
      data.PieATSI2018,
      labels = ~ Category,
      values = ~ Total,
      hoverinfo = "none",
      hole = 0.5,
      type = 'pie'
    ) %>%
      layout(
        title = list(text = "2018-19 WAPM ATSI Breakdown"),
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
      )%>%
      config(displayModeBar = F)
    
  })  

  ## Pie chart 2018 Sentence Type
  output$PieType2018 <- renderPlotly({
    p <- plot_ly(
      data.PieType2018,
      labels = ~ Category,
      values = ~ Total,
      hoverinfo = "none",
      hole = 0.5,
      type = 'pie'
    ) %>%
      layout(
        title = list(text = "2018-19 WAPM Status Breakdown"),
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
      )%>%
      config(displayModeBar = F)
    
  })      
  
}

shinyApp(ui, server)
