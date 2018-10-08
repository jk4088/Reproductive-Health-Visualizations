library(shiny)
library(tidyverse)
library(ggplot2) # load ggplot
library(shinythemes)
library(plotly)
library(wordcloud)
library(waffle)
library(dplyr)
library(leaflet)
library(DT)
library(reshape2)

# read in RData from Rmd
load("./data/combo.RData")
load("./data/area.RData")

load("./data/pro_life.RData")
load("./data/pro_choice.RData")
load("./data/abortion_waffle.RData")

load("./data/abortion_time.RData")
load("./data/state_laws.RData")
load("./data/us_map.RData")

load("./data/top25_pc.RData")
load("./data/top25_pl.RData")
load("./data/Abortion_Table.RData")
load("./data/counties.RData")
load("./data/clinics.RData")

# for state names 
states <- unique(area$State)
names(states) <- states

# for state names for abortion rate by race graphs 
states_race <- unique(abortion_waffle$State)
names(states_race) <- states_race


#=====UI=====
# Define UI for app
ui <- fluidPage(title = "Women's Reproductive Health", theme = shinytheme("cyborg"), # this makes the background black; "lumen" is the white background
                #This sets the centering for all header types  
                tags$style(type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }",
                           "h5, h6, p {
                           text-align:center;
                           }"),
                tags$style(type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }",
                           "h1 {color: #a821a2; text-align:center; }
                            h4 {color: #db83d7; text-align:center;
                           }"),
                # This aligns the check boxes; found on StackOverflow 
                tags$style(HTML(
                  ".checkbox-inline { 
                    margin-left: 0px;
                    margin-right: 10px;
                  }
                  .checkbox-inline+.checkbox-inline {
                    margin-left: 0px;
                    margin-right: 10px;
                  }"
                  
                )),

                # title 
                titlePanel(h1("Women's Reproductive Health")),
                
                # row of stuff;   
                fluidRow(
                  column(width = 2), #this makes the column more centered, a little hacky?
                  column(width = 8, 
                         h4("By: Sophie Beiers, Joo Kim, Dana Kraushar, Nicole Smith"), br(),
                         h6("This visualization project touches on abortion laws, clinic locations,
                           and demographics in the United States. The data on abortion is vast, so we chose to focus on
                            particular dimensions in an attempt to convey the most important information, and ultimately to paint a picture of
                            who gets abortions in America. We use maps, plots, and text analysis to display this information."),
                         br(), 
                         # input that toggles between abortion rate and birth rate
                         # Plot 1a Abortions over Time
                         h5("Abortions Since Roe v. Wade (1973-2011)"),
                         p("Horizontal lines represent the ratio and rate of abortion in 1973, the year the U.S. Supreme Court ruled
                            that state restrictions on abortion were unconstitutional in the landmark decision Roe v. Wade.
                           (Dashed line represents ratio and dotted line represents rate)."),
                         p("Note that abortion rates rose after Roe, peaking in the early 1980s, before beginning a steady decline
                            that continues to this day. Abortions fell below 1973 levels for the first time in 2014 (not pictured
                            due to discrepancies in the data). A longer trendline would be illuminating, but abortion data was not
                            comprehensively collected prior to 1973."),
                         # radioButtons(inputId = "rateratio", label = "",
                         #                    choices = c("Rate (abortions per 1,000 women ages 15-44)",
                         #                                "Ratio (abortions per 1,000 live births)", "Rate and Ratio"),
                         #                    selected = "Rate and Ratio",
                         #                    inline = TRUE),
                         checkboxGroupInput(inputId = "rateratio", label = "",
                                      choices = c("Rate",
                                                  "Ratio"),
                                      selected = "Rate",
                                      inline = TRUE),
                         plotlyOutput("PlotTime"),
                         br(),
                         br(),
                         

                         # Plot 7 Data Table
                         h5("Abortion Data"),
                         p("Because of the intricacies of abortion data, especially pertaining to restrictions
                           and insurance, we chose to use a data table so that each user could explore the data
                           and discover how abortion policies vary from state to state. In addition to changing
                           the number of states that appear, one can filter the columns by information within them."),
                         br(),
                         DT::dataTableOutput("abortiontable"),
                         br(),
                         
                         # Plot 9 General Map of Abortions - hopefully with state laws
                         h5("Abortion Rate by State, with Legal Restrictions"),                         
                         p("The states with the highest rates of abortion appear to be clustered around the coasts: 
                           the highest abortion rates are found in Washington D.C. (32.7), New York (29.6), and New Jersey (25.8). 
                           Outside of the northeast, the states with the highest abortion rates are Florida, California and Nevada. 
                           The lowest abortion rates are in the heartland: Wyoming’s rate is 1.1, South Dakota’s is 3.5, 
                           and Mississippi is 3.8."),
                         p("It looks like there is some relationship between laws restricting abortion and abortion rate. The states 
                           with the most restrictive policies (limiting abortions prior to 20 weeks) are in the center of the country, 
                           where abortion rates are lower. The states with looser restrictions, like 24 week cutoffs, include higher-abortion 
                           states like New York, Nevada and Florida."),
                         # checkboxGroupInput(inputId = "restrictions", label = h6("Choose restriction: )", 
                         #                    choices = names(unique(us_map$prohibited_after),
                         #                    selected = "(no prohibition)",
                         #                    inline = TRUE),            
                         checkboxInput("legend", "Show legend", FALSE),
                         leafletOutput("abortionmap"),
                         br(),
                         
                         # Plot 8 Map of Driving Times
                         h5("Map of Driving Times to Abortion Clinics"),
                         br(),
                         p("Because states have various restrictions on abortion clinics, a woman may not have
                           a clinic available within the city in which she resides. In order to demonstrate the
                           difficulty that many women face in accessing abortion clinics (although not plotting
                           the restrictions on abortions), this map highlights counties in purple that have access
                           to an abortion clinic within the chosen mileage. Note that as the slider increases in 
                           number of miles, more counties are highlighted in purple. To reiterate, counties 
                           that are highlighted in purple have access to an abortion clinic within the selected
                           driving distance. As the slider input increases, nearly all counties are highlighted 
                           in purple because only a few, extremely remote areas are more than 400 miles from an
                           abortion clinic. To understand the lack of access to abortion clinics, pay attention to
                           the counties that are shaded in gray."),
                         br(),
                         sliderInput("slider_driving", "Miles to Clinic", min = 50, max = 400, value = 50, step = 50),
                         leafletOutput("Map_Driving"),
                         br(),
                         br(),
                         
                         # PLOT 6 waffle chart: abortion rate by race 
                         h5("Abortion Rate by Race"),
                         p("The waffle charts illustrates the breakdown of abortion rate (number of abortions per 1,000 
                           women) by race. Across all states, Black and Hispanic women experience a higher rate of abortion 
                           than White women."),
                         selectInput("state_var", 
                                     label = "Choose a state to see its abortion rates by race",
                                     choices = names(states_race),
                                     selected = "U.S. total"),
                         htmlOutput("text2"),
                         htmlOutput("text3"),
                         plotOutput("abortion_race_plot"),
                         br(),
                         
                         
                         # PLOT 1
                         h3("Teens"),
                         p("The below charts show the differences between teenage abortion rates
                           and birth rates per 1,000 teens (ages 15-19) in all 50 U.S. States for the year 2013. States are ordered by an 
                           encompassing grade of women's reproductive health in that state. Abortion rates are highest in states with better 
                           'grades', while birth rates are higher in states lower on the list."),
                         selectInput("variable1", 
                                     label = "Choose a variable to toggle the charts below",
                                     choices = list("Teen Abortion Rate", 
                                                    "Teen Birth Rate"),
                                     selected = "Teen Abortion Rate"),
                         br(),
                         
                         plotOutput("Plot1"),
                         br(), # this is "enter" to space things out
                         br(),
                         
                         # PLOT 2
                         plotOutput("Plot2"),
                         br(),
                         br(),
                         
                         # PLOT 3
                         h5("Percentage of Teen Pregnancies Ending in Abortion/Birth"),
                         p("The chart below indicates the percentage of teen pregnancies ending in abortions or birth in the US.
                           The area chart is compared to the average percentage of teen pregnancies that end in birth as well as the half-way 
                           mark between abortions and births."),
                         br(),
                         checkboxGroupInput("StateVariable", label = h6("Select a state to learn more about 
                                                                       how many pregnant teens in that state
                                                                       have abortions and 
                                                                       the state's women's health ranking."), 
                                            choices = names(states),
                                            selected = "New York", 
                                            inline = TRUE),
                         htmlOutput("text"),
                         br(),
                         plotOutput("Plot3"),
                         br(),
                         br(),
                         
                         # PLOT 4 word cloud: pro-choice
                         h5("Language Around Abortion"),
                         p("Language around abortion reflects the fierce divisions on the issue and reveals key strategies
                            around framing (for example, 'pro-choice' instead of 'anti-life'). To understand how advocacy organizations 
                            discuss these issues, we performed a text anlaysis of documents published by pro-choice and pro-life organizations. 
                            To do this, we looked at the 'About,' 'Resources,' and 'FAQ' sections of pro-choice and pro-life 
                            websites in an attempt to obtain comparable information from both sides of the debate The word 
                            clouds below show the most common words utilized by its advocates as well as its critics from 14 organizations."),
                         plotOutput("wordcloud_prochoice"),
                         br(),
                         p("On one hand, language most frequently used by pro-choice supporters include words like 'law', 'state', and 'court' that 
                            likely emphasize the legality of abortion access in the country. On the other hand, pro-life supporters tend to 
                           use words that may emotionally appeal to women considering getting an abortion: 'baby', 'human', and 'fetus', 
                           in an attempt to make women reassess their decision."),
                         br(),
                         p("In addition, this pyramid plot shows the 25 most frequently used words by Pro-Choice and
                           Pro-Life organizations. When you select 'Pro-Choice' from the dropdown menu,
                           you will see the most common 25 words for Pro-Choice organizations plotted
                           in comparison to how often Pro-Life organizations use the same words. 
                           In the process of scraping, we noticed that the text from the pro-life organizations was a form of story-telling
                          rather than legal information in the pro-choice organizations. This may help explain the lower frequencies of words
                           in the pro-life organizations."),
                         selectInput("variable_frequency",
                                     label = h6("Select an abortion organization."),
                                     choices = list("Pro-Choice", "Pro-Life"),
                                     selected = "Pro-Choice"),
                         plotOutput("Plot_Frequency"),
                         
                         br(),
                         br(),
                         br(),
                         h3("Conclusion"),
                         p("We obviously did not cover the magnitude of information on abortion in the US. In the future, 
                           we suggest looking into abortions by age, the relationship between contraceptives and abortion, funding,
                           state abortion rates over time and looking at an index of political climate."),
                         br(),
                         br(),
                         
                         h3("Code and Process Book"),
                         p("Visit https://github.com/QMSS-GR5063-2018/GroupA_WomensHealth/blob/master/docs/app.R to view our code."),
                         p("Cleaned datasets available at https://github.com/QMSS-GR5063-2018/GroupA_WomensHealth/blob/master/docs/app.R."),
                         p("Process book available at https://github.com/QMSS-GR5063-2018/GroupA_WomensHealth/blob/master/docs/Process%20Book/Women's%20Health%20Process%20Book.pdf"),
                         
                         h3("References"),
                         h5("[Abortions]"),
                         p("https://www.guttmacher.org/state-policy/explore/overview-abortion-laws"),
                         p("https://www.guttmacher.org/state-policy/explore/restricting-insurance-coverage-abortion"),
                         p("https://data.guttmacher.org/states"),
                         p("https://statusofwomendata.org/explore-the-data/methodology/#rrmethodology"),
                         
                         h5("[Driving Times]"),
                         p("https://www.safeplaceproject.com/list-of-providers"),
                         
                           
                         h5("[Time Series Data]"),
                         p("http://www.jstor.org.ezproxy.cul.columbia.edu/stable/23048833?pq-origsite=summon&seq=3#page_scan_tab_contents"),
                         p("https://www.guttmacher.org/sites/default/files/pdfs/journals/psrh.46e0414.pdf"),
                         
                         h5("[Text Analysis]"),
                         
                         p("https://www.reproductiverights.org/sites/crr.civicactions.net/files/documents/USPS-Year-End-Report-Vs-6.pdf"),
                         p("https://www.reproductiverights.org/sites/crr.civicactions.net/files/documents/Safe%20and%20Legal%20Abortion%20is%20a%20Womans%20Human%20Right.pdf"),
                         p("https://www.reproductiverights.org/sites/crr.civicactions.net/files/documents/20Years_Reform_Report.pdf"), 
                         p("https://prochoice.org/education-and-advocacy/about-abortion/abortion-facts/"),
                         p("https://www.nrlc.org/abortion/"), 
                         p("http://1stwaylifecenter.com/download-literature/"), 
                         p("http://marchforlife.org/education/"), 
                         p("https://www.plannedparenthood.org/learn/abortion/considering-abortion/what-facts-about-abortion-do-i-need-know") 
                         
                         ) # end of column
                      )  
                    ) # this paren matches the fluid ui up above. 
                
#=====SERVER=====
# here are all the ggplots & graphs that are called above. 
server <- function(input, output) {

# Plot 1a
  # output$PlotTime <- renderPlotly({
  #   
  #   time_plotly <- ggplot(data = abortion_time, aes(x=Year)) +
  #     theme_minimal() +
  #     ylim(0, 35) +
  #     # annotate(geom = "text", x = 1981, y = 20,
  #     #                 label = "Ratio at time of Roe v. Wade (1973)", size = 3, color = 'darkgrey') +
  #     # annotate(geom = "text", x = 1977.2, y = 17,
  #     #                 label = "Rate in 1973", size = 3, color = 'darkgrey') +
  #     # geom_text(data=data.frame(x=0,y=h), aes(x, y), label=h, vjust=-1)
  #     #  annotate(geom="text", label="Rate at time of Roe v. Wade", x = 1973, y=roe_rate) +
  #     #  geom_text(aes(0,roe_ratio,label = roe_ratio, vjust = -1)) +
  #     # labs(y = "Number of Abortions") +
  #     ylab("Number of Abortions") +
  #     theme(panel.background = element_rect(fill = "transparent", colour = NA),
  #           plot.background = element_rect(fill = "transparent", colour = NA),
  #           legend.position = c(0.8, 0.3),
  #           legend.title = element_blank(),
  #           # legend.text = element_text(color = 'white'),
  #           # panel.grid.minor = element_blank(),
  #           panel.grid.major = element_blank(),
  #           axis.title.x = element_blank(),
  #           axis.title.y = element_text(vjust = 1),
  #           axis.text = element_text(color = 'white')) +
  #     scale_x_continuous(limits = c(1973, 2011), breaks = c(1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010)) +
  #     scale_color_hue(labels = c("Rate (per 1000 women aged 15 to 44 years)",
  #                                "Ratio (per 1,000 live births"))
  # 
  #   
  #     if (input$rateratio == "Rate (abortions per 1,000 women ages 15-44)") {
  #       time_plotly <- time_plotly + geom_line(aes(y = Rate), color = '#8c6bb1', size=2) +
  #         geom_hline(yintercept = 16.3, linetype="dotted", color = "darkgrey")
  #     } else if (input$rateratio == "Ratio (abortions per 1,000 live births)") {
  #       time_plotly <- time_plotly + geom_line(aes(y = Ratio), color = '#bfd3e6', size=2) +
  #         geom_hline(yintercept = 19.3, linetype="dashed", color = "darkgrey")
  #     } else{
  #       time_plotly <- time_plotly +
  #         geom_line(aes(y = Ratio), color = '#bfd3e6', size=2) +
  #         geom_line(aes(y = Rate), color = '#8c6bb1', size=2) +
  #         geom_hline(yintercept = 19.3, linetype="dashed", color = "darkgrey") +
  #         geom_hline(yintercept = 16.3, linetype="dotted", color = "darkgrey")}
  #   
  # }
  

  
  # Plot 1a
  output$PlotTime <- renderPlotly({
    
    abortion_time <- melt(abortion_time, id.vars = 'Year')
    abortion_time$Abortion <- paste("(",abortion_time$variable,": ", abortion_time$value, ")")
    abortion_time <- abortion_time[abortion_time$variable %in% input$rateratio, ]
    
    time_plotly <- ggplot(data = abortion_time, aes(x = Year, y = value, color = variable, label = Abortion)) +
      geom_line() +
      scale_color_manual(name = "Rate/Ratio", values = c('#8c6bb1','#bfd3e6'), labels = c("Rate (abortions per 1,000 women)", 
                         "Ratio (abortions per 1,000 pregnancies)")) +
      theme_minimal() +
      ylim(0, 35) +
      geom_hline(yintercept = 19.3, linetype="dashed", color = "darkgrey") +
      geom_hline(yintercept = 16.3, linetype="dotted", color = "darkgrey") +
      labs(y = "Number of Abortions") +
      theme(panel.background = element_rect(fill = "transparent", colour = NA),
            plot.background = element_rect(fill = "transparent", colour = NA),
            legend.position = c(0.8, 0.3),
            legend.title = element_blank(),
            legend.text = element_text(color = 'white'),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_text(vjust = 1),
            axis.text = element_text(color = 'white')) +
      scale_x_continuous(limits = c(1973, 2011), breaks = c(1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010))
    
    
    ggplotly(p = time_plotly, tooltip = c("Year", "Abortion"))
    
  })
  
  # Plot: Abortion Table
  
  output$abortiontable = DT::renderDataTable({
    colnames(abortions_table) <- c("State", "Abortion Rate (per 1000 women: 15-44)", 
                                   "Public Spending for Abortions (1000s)", 
                                   "Abortions Prohibited After", "Can Insurance Providers Refuse to Participate?", 
                                   "Is There Mandated Counseling?", "Waiting Period After Counseling (hours)", 
                                   "Is Parental Involvement for Minors Required?")
    datatable(abortions_table, rownames = FALSE, filter = list(position = "top"), 
              options = list(autoWidth = TRUE, lengthMenu = c(10, 25, 50)),
              width = 600, height = 600
              #caption = htmltools::tags$caption(
              # style borrowed from same R Studio Github above
              # style = 'caption-side: bottom; text-align: center;', 
              #  'Table 1: ', htmltools::em('US Abortions by State' ))
    ) %>% 
      
      formatStyle(1:8, color = "white", backgroundColor = "black") %>% 
      formatStyle(columns = c(1:8), fontSize = '80%')
  }, bg = "transparent")
  
  # General abortion map
  
  output$abortionmap <- renderLeaflet({

    pal2 <- colorNumeric(
      palette = c( '#E6EDF2','#e0ecf4','#bfd3e6','#9ebcda','#8c96c6','#8c6bb1','#88419d','#810f7c','#4d004b', '#320044'),
      domain = us_map$abortion_rate)

    m1 <- leaflet(width = "100%", us_map, options = leafletOptions(minZoom = 3)) %>%
      setView(lng = -96, lat = 38, zoom = 4) %>%
      addProviderTiles("Stamen.TonerBackground")

    labels <- sprintf(
      "<strong>State: %s</strong><br/>Abortion rate: %g <br/>Abortions prohibited after %s <br/>Licensed physician required? %s",
      us_map$NAME, us_map$abortion_rate, us_map$prohibited_after, us_map$licensed_physician) %>% lapply(htmltools::HTML)

    m2 <- m1 %>%
      addPolygons(
        fillColor = ~pal2(us_map$abortion_rate), weight = 2,
        opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = '#666',
         dashArray = "",
         fillOpacity = 0.7,
          bringToFront = TRUE, sendToBack = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"))

    if (input$legend == TRUE) {
      m3 <- m2 %>% addLegend(pal = pal2,
                             values = ~us_map$abortion_rate,
                             title = "Abortion Rate in 2014<br>(abortions per 1,000 <br>women aged 15-44)",
                             "bottomright")
      print(m3)
    }
    else {
      print(m2)
    }
    
################### attempted to add layers, but the states got jumbled in the tooltips   ###################
    
    # pal2 <- colorNumeric(
    #   palette = c( '#E6EDF2','#e0ecf4','#bfd3e6','#9ebcda','#8c96c6','#8c6bb1','#88419d','#810f7c','#4d004b', '#320044'),
    #   domain = us_map$abortion_rate)
    # 
    # m1 <- leaflet(width = "100%", us_map, options = leafletOptions(minZoom = 3)) %>%
    #   setView(lng = -96, lat = 38, zoom = 4) %>%
    #   addProviderTiles("Stamen.TonerBackground")
    # 
    # labels <- sprintf(
    #   "<strong>State: %s</strong><br/>Abortion rate: %g <br/>Abortions prohibited after %s <br/>Licensed physician required? %s",
    #   us_map$NAME, us_map$abortion_rate, us_map$prohibited_after, us_map$licensed_physician) %>% lapply(htmltools::HTML)
    # 
    # m2 <- m1 %>%
    #   addPolygons(data = subset(us_map, prohibited_after == "(no prohibition)"),
    #               fillColor = ~pal2(us_map$abortion_rate), weight = 2,
    #               opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7,
    #               highlight = highlightOptions(
    #                 weight = 5,
    #                 color = '#666',
    #                 dashArray = "",
    #                 fillOpacity = 0.7,
    #                 bringToFront = TRUE, sendToBack = TRUE),
    #               label = labels,
    #               labelOptions = labelOptions(
    #                 style = list("font-weight" = "normal", padding = "3px 8px"),
    #                 textsize = "15px",
    #                 direction = "auto"),
    #               group = "no prohibition") %>%
    #   addPolygons(data = subset(us_map, prohibited_after == "viability"),
    #               fillColor = ~pal2(us_map$abortion_rate), weight = 2,
    #               opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7,
    #               highlight = highlightOptions(
    #                 weight = 5,
    #                 color = '#666',
    #                 dashArray = "",
    #                 fillOpacity = 0.7,
    #                 bringToFront = TRUE, sendToBack = TRUE),
    #               label = labels,
    #               labelOptions = labelOptions(
    #                 style = list("font-weight" = "normal", padding = "3px 8px"),
    #                 textsize = "15px",
    #                 direction = "auto"),
    #               group = "viability") %>%
    #   addPolygons(data = subset(us_map, prohibited_after == "20 weeks"),
    #     fillColor = ~pal2(us_map$abortion_rate), weight = 2,
    #     opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7,
    #     highlight = highlightOptions(
    #       weight = 5,
    #       color = '#666',
    #       dashArray = "",
    #       fillOpacity = 0.7,
    #       bringToFront = TRUE, sendToBack = TRUE),
    #     label = labels,
    #     labelOptions = labelOptions(
    #       style = list("font-weight" = "normal", padding = "3px 8px"),
    #       textsize = "15px",
    #       direction = "auto"),
    #     group = "20 weeks") %>%
    #   addPolygons(data = subset(us_map, prohibited_after == "24 weeks"),
    #               fillColor = ~pal2(us_map$abortion_rate), weight = 2,
    #               opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7,
    #               highlight = highlightOptions(
    #                 weight = 5,
    #                 color = '#666',
    #                 dashArray = "",
    #                 fillOpacity = 0.7,
    #                 bringToFront = TRUE, sendToBack = TRUE),
    #               label = labels,
    #               labelOptions = labelOptions(
    #                 style = list("font-weight" = "normal", padding = "3px 8px"),
    #                 textsize = "15px",
    #                 direction = "auto"),
    #               group = "24 weeks") %>%
    #   addPolygons(data = subset(us_map, prohibited_after == "28 weeks"),
    #               fillColor = ~pal2(us_map$abortion_rate), weight = 2,
    #               opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7,
    #               highlight = highlightOptions(
    #                 weight = 5,
    #                 color = '#666',
    #                 dashArray = "",
    #                 fillOpacity = 0.7,
    #                 bringToFront = TRUE, sendToBack = TRUE),
    #               label = labels,
    #               labelOptions = labelOptions(
    #                 style = list("font-weight" = "normal", padding = "3px 8px"),
    #                 textsize = "15px",
    #                 direction = "auto"),
    #               group = "28 weeks") %>%
    #   addLayersControl(
    #     overlayGroups = c("no prohibition", "viability", "20 weeks", "24 weeks", "28 weeks"),
    #     options = layersControlOptions(collapsed = TRUE)
    #   )
    
    
  } )
  
  output$Map_Driving <- renderLeaflet({
    nums <- c(50, 100, 150, 200, 250, 300, 350, 400)
    
    labels_drive <- sprintf(
      "Clinic: %s <br/> City: %s ",
      clinics$Provider, clinics$City) %>% lapply(htmltools::HTML)
    
    if (input$slider_driving %in% nums) {
      pal_driving <- colorBin(palette = c("#810f7c",'#efeded'),
                              domain = counties$dist_miles,
                              bins = c(0, input$slider_driving))
      
      
      leaflet(width = "100%",
              options = leafletOptions(minZoom = 3)) %>%
        setView(lng = -96, lat = 38, zoom = 4) %>%
        addProviderTiles("Stamen.TonerBackground", group = "Simple") %>% 
        addPolygons(data = counties, stroke = FALSE,
                    fillColor = ~pal_driving(counties$dist_miles),
                    fillOpacity = 0.8,
                    weight = 1,
                    group = "Driving Distances") %>% 
        addCircleMarkers(data = clinics, 
                         radius = 1.5, 
                         stroke = FALSE, 
                         fillOpacity = 0.9, 
                         color = "#810f7c", 
                         group = "Clinics",
                         label = labels_drive, 
                         labelOptions = labelOptions(
                           style = list("font-weight" = "normal", padding = "3px 8px"),
                           textsize = "15px",
                           directions = "auto")) %>% 
        addLayersControl(
          baseGroups = c("Simple"),
          overlayGroups = c("Driving Distances", "Clinics"),
          options = layersControlOptions(collapsed = TRUE)
        )
    }
    
    
  })
  
  # Plot waffle chart 
  output$abortion_race_plot <- renderPlot({
    if(input$state_var %in% abortion_waffle$State) {
      abortion_state <- filter(abortion_waffle, abortion_waffle$State == input$state_var)
    }
    
    iron(waffle(abortion_state[1, 3:4], rows = 10, size = 0.3,
                colors = c("#320044", "#b1bdc6"), title = "Black") + 
           theme(axis.title.x = element_text(color = "white", face = "bold"), text = element_text(color = "white", face = "bold")), 
         waffle(abortion_state[2, 3:4], rows = 10, size = 0.3,
                colors = c("#320044", "#b1bdc6"), title = "Hispanic") +
           theme(axis.title.x = element_text(color = "white", face = "bold"), text = element_text(color = "white", face = "bold")), 
         waffle(abortion_state[3, 3:4], rows = 10, size = 0.3,
                colors = c("#320044", "#b1bdc6"), title = "White") +
           theme(axis.title.x = element_text(color = "white", face = "bold"), text = element_text(color = "white", face = "bold")),
         waffle(abortion_state[4, 3:4], rows = 10, size = 0.3,
                colors = c("#320044", "#b1bdc6"), title = "Other") +
           theme(axis.title.x = element_text(color = "white", face = "bold"), text = element_text(color = "white", face = "bold")))
    
    
  }, bg = "transparent")
  
  output$text2 <- renderUI({
    abortion_waffle2 <- abortion_waffle %>% 
      filter(State == input$state_var)
    str1 <- paste("In", input$state_var, ":")
    #str2 <- paste(abortion_waffle2$`Number of Abortions`, "out of", abortion_waffle2$`All Pregnancies`, "pregnant", abortion_waffle2$race, "women have abortions")
    HTML(paste("<font color=\'#a821a2'\"><b>", str1,'<br/>'))
  })
  
# Plot 1  
  output$Plot1 <- renderPlot({
    if (input$variable1 == "Teen Abortion Rate") {
      yaxis <- combo$tot_abortion_rate
      ytitle <- 'Teen Abortion Rate per 1000'
      
    }
    else {
      yaxis <- combo$tot_birth_rate
      ytitle <- 'Teen Birth Rate per 1000'
    }
    
    colors <- c( '#E6EDF2','#e0ecf4','#bfd3e6','#9ebcda','#8c96c6','#8c6bb1','#88419d','#810f7c','#4d004b', '#320044')
    ggplot(data = combo, aes(x = reorder(State, -Rank), y = yaxis, fill = Grade)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = rev(colors)) +
      theme_minimal() +
      theme(axis.text = element_text(color = "white", face = "bold"),
            legend.text = element_text(color = "white", face = "bold"),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()) +
      labs(x = '', y = ytitle) +
      coord_flip()
  }, bg = "transparent") # this makes the background of the plot transparent 

  
# Plot2
    output$Plot2 <- renderPlot({
      
      # the below changes the outlining in the area charts based on whether abortion or birth rate is selected. 
      if (input$variable1 == "Teen Abortion Rate") {
        color_abort <- "yellow"
        color_birth <- "white"
        size_abort <- 0.6
        size_birth <- 0.2
        
      }
      else {
        color_birth <- "yellow"
        color_abort <- "white"
        size_birth <- 0.6
        size_abort <- 0.2
      }
      
      # states 
      states <- combo %>% 
        group_by(State) %>% 
        arrange(Rank) %>% 
        select(State)
      
      states <- states[["State"]]
      
      #ggplot 
      p <- ggplot(data = area, aes(x = Rank, y = Rate, fill = BirthAbortion))
      # can't seem to get the legend without yellow lines
        
      # adding in dots that come in when user selects certain thing 
        if(input$variable1 == "Teen Abortion Rate"){
          p = p + geom_area(data = filter(area, BirthAbortion == 'tot_birth_rate'), 
                    alpha = 0.5, color = color_birth, size = size_birth, show.legend = FALSE) +
          geom_area(data = filter(area, BirthAbortion == 'tot_abortion_rate'),
                    alpha = 0.9, color = color_abort, size = size_abort, show.legend = FALSE) +
          geom_point(data = filter(area, BirthAbortion == 'tot_abortion_rate'), 
                             color = "yellow", show.legend = FALSE) +
            scale_fill_manual(values = c('#810f7c', "#4d004b"), 
                              labels = c("Teen Abortion Rate/1000", "Teen Birth Rate/1000"),
                              name = '') 
        }
        else{
          p = p + geom_area(data = filter(area, BirthAbortion == 'tot_abortion_rate'),
                            alpha = 0.6, color = color_abort, size = size_abort, show.legend = FALSE) +
            geom_area(data = filter(area, BirthAbortion == 'tot_birth_rate'), 
                            alpha = 0.9, color = color_birth, size = size_birth, show.legend = FALSE) +
            geom_point(data = filter(area, BirthAbortion == 'tot_birth_rate'), 
                              color = "yellow", show.legend = FALSE) +
            scale_fill_manual(values = c('#810f7c', "#4d004b"), 
                              labels = c("Teen Abortion Rate/1000", "Teen Birth Rate/1000"),
                              name = '')
        }
      
      p <- p + theme_minimal() +
        scale_x_continuous(breaks = 0:50, labels = states) +
        theme(legend.position = c(0.8, 0.8)) +
        theme(axis.text.x = element_text(color = "white", face = "bold", angle = 90, hjust = 1),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              legend.text = element_text(color = "white", face = "bold")) +
        labs(x = '', y = '')
      
      print(p)

  }, bg = "transparent")
    
    
    

    # Plot3
    output$Plot3 <- renderPlot({
      
      ggplot(data = area, aes(x = Rank, y = Perc, fill = PercentBirthAbortion)) +
        geom_area(alpha = 0.8, color = 'white', size = 0.1) +
        # this takes the input for state and adds a point wherever you tell it to 
        geom_point(data = filter(area, State %in% input$StateVariable & 
                                   PercentBirthAbortion == 'perc_end_birth'), 
                   color = 'yellow',
                   size = 4,
                   show.legend = 'FALSE') +
        geom_text(data = filter(area, State %in% input$StateVariable & 
                                PercentBirthAbortion == 'perc_end_birth'), 
                   aes(label = paste(State, Grade, sep = ": ")),
                   nudge_y = 3,
                   check_overlap = TRUE,
                   color = "white",
                   
                   size = 3) +
        scale_fill_manual(values = c('#810f7c', "#4d004b"), 
                          labels = c("% Ending in Abortion", "% Ending in Births"),
                          name = 'Teenage Pregnancies') +
        geom_hline(yintercept = 50, linetype="dashed", color = "lightgrey") +
        geom_hline(yintercept = 72.37, linetype="dashed", color = "lightgrey") +
        annotate(geom = "text", x = 48, y = 53, 
                 label = "50%", size = 4, color = 'white') +
        annotate(geom = "text", x = 45, y = 75, 
                 label = "Average % Ending in Birth", size = 4, color = 'white') +
        theme_void() +
        theme(legend.position = c(0.8, 0.3), 
              legend.title = element_text(color = 'white'),
              legend.text = element_text(color = 'white'))
      
    }, bg = "transparent")
    
    StateVariable <- reactive({input$checkGroup})
    
  # text output that tells the user facts about each state   
    output$text <- renderUI({
      area2 <- area %>% 
        filter(State %in% input$StateVariable & PercentBirthAbortion == "perc_end_abortion")
      str1 <- paste("In", input$StateVariable, ",", area2$Perc, "% of teen pregnancies end in abortion.")
      HTML(paste("<font color=\'#a821a2'\"><b>", str1, '<br/>'))
      
    })  
    
    # Plot: wordclouds 
    
    output$wordcloud_prochoice <- renderPlot({
      layout(matrix(c(1, 2, 3, 4), 2, 2, byrow = FALSE), 
             heights = c(1, 5))
      par(mar = rep(0, 4))
      plot.new()
      text(x = 0.5, y = 0.4, "Pro-Choice", col = "white", cex = 2)
      wordcloud(pro_choice$word, pro_choice$n, max.words = 30, 
                colors = "#8c6bb1", scale = c(6, 2)) 
      plot.new()
      text(x = 0.5, y = 0.4, "Pro-Life", col = "white", cex = 2)
      wordcloud(pro_life$word, pro_life$n, max.words = 30, 
                colors = "#E6EDF2", scale = c(6, 2))
      
    }, bg = "transparent")
    
    # Plot: word frequency
    
    output$Plot_Frequency <- renderPlot({
      if(input$variable_frequency == "Pro-Choice"){
        top25 <- top25_pc
      }
      else {
        top25 <- top25_pl
      }
      
      ggplot(top25, aes(x = reorder(word, n), y = n, fill = doc_id)) +
        geom_bar(data = filter(top25, doc_id == "Pro-choice"), stat = "identity") +
        geom_bar(data = filter(top25, doc_id == "Pro-life"), stat = "identity") +
        scale_fill_brewer(direction = -1) + 
        scale_fill_manual(values = c("#8c96c6", "#810f7c")) +
        labs(x = "", y = "", title = "Frequency of Top 25 Words", subtitle = "Comparison of Pro-Life and Pro-Choice Organizations") +
        coord_flip() +
        theme_void() +d
        theme(axis.ticks = element_blank(),
              legend.title = element_blank(),
              legend.text = element_text(color = 'white'),
              axis.text = element_text(color = 'white'))
    }, bg = "transparent")
    
 
    
    output$text3 <- renderUI({
      abortion_waffle2 <- abortion_waffle %>% 
        filter(State == input$state_var)
      str2 <- paste(abortion_waffle2$`Number of Abortions`, "out of", "1000 pregnant", abortion_waffle2$race, "women have abortions",
                    "(", (abortion_waffle2$`Number of Abortions`/1000) * 100,"%)")
      HTML(paste("<font color=\'#a821a2'\">", str2, '<br/>'))
      
    })  
    
    

    

    
}

# Run the application 
shinyApp(ui = ui, server = server)


