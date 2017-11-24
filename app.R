# Create newsboard from GDELT API
# 2017, Robin Edwards, @geotheory
# Shared under GNU General Public License v3.0

# setwd('~/Documents/shinyapps.io/news')

require(shiny)
require(shinyBS)
require(shinyjs)
require(shinythemes)
# require(V8)
require(dplyr)
require(stringr)
require(lubridate)

# function to translate logical inputs into a search string
format_search_str = function(x, mode){
  mode = paste0(mode, ':')
  x1 = x %>% str_replace_all(., ' ', '') %>% str_replace(., '^', mode) %>%
    str_replace(., paste0(mode, '-'), paste0('-', mode)) %>%  # move minus to prefix position
    str_replace(., paste0(mode, '%22-'), paste0('-', mode, '%22')) # for image tags which also have %22
  ifelse(any(str_detect(x1, '-')),
         paste(x1, collapse='%20'),
         ifelse(length(x1) > 1, paste0('(', paste(x1, collapse='%20OR%20'), ')'),
                paste(x1, collapse='%20OR%20'))
  )
}

# some time calculation functions
now = as.Date(Sys.Date())
add_days = function(d, n){
  d <- ymd(d)
  d %m+% days(n)
}

# https://blog.gdeltproject.org/gdelt-doc-2-0-api-debuts/
root = 'https://api.gdeltproject.org/api/v2/doc/doc?query='

# Options selections
sort_options = c('Relevance' = 'Relevance', 'Date: newest first' = 'DateDesc', 'Date: oldest first' = 'DateAsc', 'Tone: most positive first' = 'ToneDesc', 'Tone: most negative first' = 'ToneAsc')
country_codes = read.csv('http://data.gdeltproject.org/api/v2/guides/LOOKUP-COUNTRIES.TXT', sep='\t', stringsAsFactors = F, header = F)
country_codes = c('', setNames(as.character(country_codes$V1), country_codes$V2))
lang_codes = read.csv('http://data.gdeltproject.org/api/v2/guides/LOOKUP-LANGUAGES.TXT', sep = '\t', stringsAsFactors = F, header = F)
lang_codes = c('', English = 'eng', setNames(as.character(lang_codes$V1), lang_codes$V2))
image_tags = read.csv('http://data.gdeltproject.org/api/v2/guides/LOOKUP-IMAGETAGS.TXT', sep='\t', stringsAsFactors = F, header = F) %>% .[['V1']]
themes = read.csv('http://data.gdeltproject.org/api/v2/guides/LOOKUP-GKGTHEMES.TXT', sep = '\t', stringsAsFactors = F, header = F) %>%
  arrange(V1) %>% mutate(V1 = str_replace_all(V1, '_', ' ')) %>% .[['V1']] # underscores removed for aesthetic. Need to re-add to URL calls


# SERVER --------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  tt_delay = list(delay = list(show=1500, hide=0))
  addTooltip(session, id = 'search_terms', title = 'Supports multiple terms separated by spaces, phrases in double quotes e.g. "cats and dogs", and OR if nested in parentheses, e.g. (cats OR dogs)', placement = "top", trigger = "hover", options = tt_delay)
  addTooltip(session, id = 'image_tags', title = 'Every image processed by GDELT is assigned one or more topical tags from a universe of more than 10,000 objects and activities recognized by Google algorithms', placement = "top", trigger = "hover", options = tt_delay)
  addTooltip(session, id = 'themes', title = 'Searches for any of the GDELT Global Knowledge Graph (GKG) Themes. GKG Themes offer a powerful way of searching for complex topics, since there can be numerous different phrases or names under a single heading. Key in likely relevant themes to find matching options. Words on the left denote the semantic hierarchy (NB. "TAX" seems to refer to taxonomy not taxation)', placement = "top", trigger = "hover", options = tt_delay)
  addTooltip(session, id = 'search_country', title = 'Country of media origin', placement = "top", trigger = "hover", options = tt_delay)
  addTooltip(session, id = 'search_domain', title = 'Internet domain of origin. Accepts multiple domains seperated by commas.', placement = "top", trigger = "hover", options = tt_delay)
  addTooltip(session, id = 'source_lang', title = 'Language of content. You can specify e.g. French but use search terms in English. GDELT handles the interpretation', placement = "top", trigger = "hover", options = tt_delay)
  addTooltip(session, id = 'timespan', title = 'Specify recent period, e.g: "24h", "3w", "2m" (hours/weeks/months), or without code letter e.g. "30" for minutes', placement = "bottom", trigger = "hover", options = tt_delay)
  addTooltip(session, id = 'daterange', title = '(Functions when "Recent" is blank.) By default GDELT reports the most recent ~3 months, but you can specify any date range within this window', placement = "bottom", trigger = "hover", options = tt_delay)
  addTooltip(session, id = 'smooth', title = 'Line smooth option, using rolling average method', placement = "bottom", trigger = "hover", options = tt_delay)
  addTooltip(session, id = 'max_records', title = 'GDELT will return 75 by default, but this can be increased to 250', placement = "right", trigger = "hover", options = tt_delay)
  addTooltip(session, id = 'data_sort', title = 'By default results are sorted by date You can also sort by date or article tone instead', placement = "top", trigger = "hover", options = tt_delay)
  addTooltip(session, id = 'translate', title = 'Include Google Translate options in newsboard.', placement = "left", trigger = "hover", options = tt_delay)
  addTooltip(session, id = 'plot_title', title = 'You can specify a title for the newsboard.', placement = "bottom", trigger = "hover", options = tt_delay)
  addTooltip(session, id = 'title_keywords', title = 'You can specify additional headline keywords to filter results by. Multiple words (seperated with commas) interpretted as x OR y.', placement = "bottom", trigger = "hover", options = tt_delay)
  addTooltip(session, id = 'dedup_ims', title = 'Results are by default filtered to remove articles with duplicate image filenames or headlines. Uncheck to stop these filters and return all content.', placement = "bottom", trigger = "hover", options = tt_delay)
  addTooltip(session, id = 'dedup_hls', title = 'Results are by default filtered to remove articles with duplicate image filenames or headlines. Uncheck to stop these filters and return all content.', placement = "bottom", trigger = "hover", options = tt_delay)
  
  # feed large selections from server (instead of pre-loading)
  updateSelectizeInput(session = session, inputId = 'image_tags', choices = image_tags, server = TRUE)
  updateSelectizeInput(session = session, inputId = 'themes', choices = themes, server = TRUE)

  observe({
    # logically build up the API search string. This forms part of the final URL call. Seperate items (AND logic) are seperated by space '%20'
    search = ''
    if(input$search_terms != '') search = input$search_terms
    search = str_replace_all(search, ' ', '%20')
    if(length(input$image_tags) > 0){
      img_tags = format_search_str(paste0('%22', input$image_tags, '%22'), 'imagewebtag')
      search = ifelse(search == '', paste0(search, img_tags), paste0(search, '%20', img_tags))
    }
    if(length(input$themes) > 0) for(i in input$themes) search = paste0(search, '%20theme:', str_replace_all(i,' ','_'))
    if(input$search_domain != ''){
      domains = str_split(input$search_domain, ',')[[1]]
      search = paste0(search, '%20', format_search_str(domains, 'domainis'))
    }
    if(length(input$search_country) > 0) search = paste0(search, '%20', format_search_str(input$search_country, 'sourcecountry'))
    if(length(input$source_lang) > 0) search = paste0(search, '%20', format_search_str(input$source_lang, 'sourcelang'))

    # if(input$search_lang != '') search = paste0(search, '%20searchlang:', input$search_lang)

    # date params
    timespan = ''
    if(input$timespan == ''){       # period defined by date range
      end_date = ifelse(input$daterange[2] == as.character(today()),
                        format(Sys.time(), '%Y%m%d%H%M%S'),
                        format(as.Date(input$daterange[2]), paste0('%Y%m%d', '235959')))
      start_date = format(as.Date(input$daterange[1]), paste0('%Y%m%d', '000000'))
    } else{                          # period defined in past hours/days/weeks/months
      timespan = paste0('&timespan=', input$timespan)
    }

    # build URL
    url = paste0(root, search)
    url = paste0(url, '&mode=ArtGallery')
    if(input$data_sort != 'Relevance')  url = paste0(url, '&sort=', input$data_sort) # sort argument
    url = paste0(url, '&maxrecords=', input$max_records)
    if(timespan != ''){
      url = paste0(url, timespan)
    } else{
      if(!all(is.na(input$daterange))){
        url = paste0(url, '&startdatetime=', start_date, '&enddatetime=', end_date)
      }
    }

    final_url <<- url %>% paste0(ifelse(input$translate, '&trans=googtrans', ''))

    # render to interface for reference and external use
    output$gdelt_url = renderUI({ HTML(paste0("<a id='gdelt_url' href='", final_url), "' target='_blank'>",
                                       str_replace(final_url, '&times', '&amp;times'), "</a>")})

    # Manage widget fuctionality
    if(input$timespan == '') shinyjs::enable("daterange") else shinyjs::disable("daterange")

    # iframe container for API response
    # responsiveness. ~2/3 screen width for desktops; full-width for mobile devices
    w = ifelse(input$dimension[1] > 767, -15 + 2 * input$dimension[1]/3, input$dimension[1])
    h = ifelse(input$dimension[1] > 767, input$dimension[2]-50, input$dimension[2])

    output$frame <- renderUI({ tags$iframe(src = final_url, width = w, height = h) })

  })

  # Help dialogue
  observeEvent(input$help, {
    showModal(modalDialog(
      h1("GDELT search options"),
      hr(),
      p("GDELT is a web search tool for media, official and typically non-commercial content that offers some powerful alternative ways to find what you want (", a('GDELT home', href = 'https://www.gdeltproject.org/', target="_blank"), '/', a('reference', href = 'https://blog.gdeltproject.org/gdelt-doc-2-0-api-debuts/', target="_blank"), ').' ," GDELT monitors global news media in multiple languages and processes it with advanced machine learning and deep learning algorithms to offer more intelligent search. Some usage examples include:"),
      tags$ul(
        tags$li('searching globally in English for content in local languages on a particular topic;'),
        tags$li('searching for all content posted in the past hour from Japan, or in Japanese, or from a particular website;'),
        tags$li('finding content based on the features and/or text found in its accompanying imagery;'),
        tags$li('finding content based on the themes it relates to; or'),
        tags$li('searching with a combination of these methods')
      ),
      p("The panel on the left offers a range of options to target your content search. You can search for content in 3 ways (or in combination):"),
      tags$ul(
        tags$li(strong('Search term'), " - any work or phrase. Phrases should be nested in double quotes - e.g. ", code('"the Ides of March"'),". Unquoted words seperated by spaces are interpretted as 'x and y' You can search for 'x or y' by enclosing terms in brackets and seperating with 'OR', e.g.", code('(cats OR dogs)'), ". Terms prefixed by the minus/hyphen symbol are interpretted as NOT, e.g.", code('-dogs')),
        tags$li(strong('Image tags'), " - images within content are processed using deep learning algorithms to identify features and text they contain. Search for available tags in the dialogue box, or ", a(href = 'http://data.gdeltproject.org/api/v2/guides/LOOKUP-IMAGETAGS.TXT', 'see the full list', target="_blank"), '.'),
        tags$li(strong('Themes'), "- these offer a powerful way of searching for complex topics, since they can include hundreds or even thousands of different phrases or names under a single heading. Themes are based on GDELT's Global Knowledge Graph (GKG). Search for relevant themes in the dialogue box, or ", a(href = 'http://data.gdeltproject.org/api/v2/guides/LOOKUP-GKGTHEMES.TXT', 'see the full list', target="_blank"), '.')
      ),
      p(strong('NOT criteria:'), "For most fields, inputs prefixed by the minus/hyphen symbol are interpretted as NOT, e.g. search term:", code('-trump'), ', image tag: ', code('-person'), ', country: ', code('-United Kingdom'), ', search languge: ', code('-eng'), ', domainis: ', code('-bbc.co.uk'), '.'),
      h4('Other criteria'),
      p("You can further narrow down your query in a number of ways:"),
      tags$ul(
        tags$li(strong('Search language'), " - defines the language of the search terms if not English. (This feature seems to have some bugs.)"),
        tags$li(strong('Country'), " - defines the country where the media outlets are located. (", a('country list', href = 'http://data.gdeltproject.org/api/v2/guides/LOOKUP-COUNTRIES.TXT', target="_blank"), ')'),
        tags$li(strong('Domain'), "- define web domain of content - e.g. website", code('bbc.co.uk'), ' or top-level domain ', code('.gov'), ' (US government content).'),
        tags$li(strong('Date range'), " - define any window for content dated in the past 3 months. The 'Hours' field must be empty to enable this."),
        tags$li(strong('Hours'), " - set to return most recent content in terms of hours."),
        tags$li(strong('Source language'), " - defines the country where the media outlets are located. (", a('supported languages', href = 'http://data.gdeltproject.org/api/v2/guides/LOOKUP-LANGUAGES.TXT', target="_blank"), ')')
      ),
      p(strong("Translation:"), "If you check the 'GT' option box your newsboard will include a translation option (Google Translate)"),
      easyClose = TRUE
      , size = 'l'))
  })

  # generate
  observeEvent(input$generate, {
    report_title = ifelse(input$plot_title != '', paste0('title=', input$plot_title, '&'), '')
    title_keywords = ifelse(input$title_keywords != '', paste0('title_keywords=', input$title_keywords, '&'), '')
    if(!input$dedup_ims | !input$dedup_hls){
      filter_opts = ifelse(!input$dedup_ims, 'allimages=true&', '') %>% paste0(ifelse(!input$dedup_hls, 'allheadlines=true&', ''))
    } else filter_opts = ''
    report_url = final_url %>% str_replace('ArtGallery', 'ArtList') %>% paste0('&format=json') %>%
      str_replace_all('[\'"]', '%22')

    output$refer = renderUI({
      HTML(paste0('<script type="text/javascript">window.open("http://diplodata.github.io/news/?',
                  report_title, title_keywords, filter_opts, report_url,'", "_BLANK");;</script>'))
    })
  })
}

# UI --------------------------------------------------------------------------------


pad = 'padding:0px 5px 0px 5px;'

ui <- fluidPage(
  theme = shinytheme("flatly"),
  # select existing textInput contents when clicking on it
  tags$script('$(document).ready(function(){ $("input").focus(function() { $(this).select(); } ); });'),
  # return screen dimensions (inc. responsively) to server to manage iframe dims
  tags$head(tags$script('
                        var dimension = 0;
                        $(document).on("shiny:connected", function(e) {
                        dimension = [window.innerWidth, window.innerHeight];
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension = [window.innerWidth, window.innerHeight];
                        Shiny.onInputChange("dimension", dimension);
                        });
                        ')
  ),
  tags$head(
    # custom css
    tags$style(HTML("
                    #sidebar { height: 100vh; overflow-y: auto; }
                    body { overflow:hidden; }

                    /* compress widgets a bit to fit on page */
                    hr {border-top: 1px solid #000; margin-top: 8px; margin-bottom: 12px; }
                    .form-control {height: 25px; margin-bottom: 0px;}
                    .selectize-input, .input-sm, .form-group, .shiny-input-container, .form-control
                    { padding:0px 5px 0px 5px; margin-bottom: 0px; min-height: 25px; }
                    .selectize-control { padding:0px 0px 0px 0px; }
                    .item { font-size: 11px; }

                    /* remove numericInput increment buttons */
                    input[type=number]::-webkit-inner-spin-button,
                    input[type=number]::-webkit-outer-spin-button {
                    -webkit-appearance: none;
                    margin: 0;
                    }
                    #gdelt_url { color:#999; font-size: 90%; word-wrap: break-word; line-height: 100%; }
                    #plot_title, #title_keywords { font-size: 20px; margin:5px 0px 5px 0px; }
                    .btn-default:hover {background-color: black; border-color: black; text-color: white; }
                    .btn-default, .btn-default:focus {background-color: #18BC9C; border-color: #18BC9C; text-color: black; }
                    "))
    ),
  # Custom tooltips for widgets
  shinyjs::useShinyjs(),
  shinyBS:::shinyBSDep,
  
  
  # UI - layout --------------------------------------------------------------------------------

  sidebarLayout(
    sidebarPanel(width = 4, id = 'sidebar',
                 fluidRow( column(12, h3("Newsboard designer"))),
                 fluidRow( column(12, p('Use this page to build a newsboard webpage using the', a('GDELT', href = 'https://www.gdeltproject.org/', target="_blank"), " web search tool, which offers intelligent ways to find web content.", a(href = '#', 'How to use it.', onclick = "$('#help').trigger('click');"), a('GDELT documentation.', href = 'https://blog.gdeltproject.org/gdelt-doc-2-0-api-debuts/', target="_blank")))),

                 # INPUTS
                 hr(),
                 tabsetPanel(id = 'input_tab',
                             tabPanel("SEARCH TERMS",
                                      fluidRow(
                                        column(12, textInput(inputId = 'search_terms', label = 'search terms', value = 'Brexit'), style=pad)
                                      )
                             ),
                             tabPanel("IMAGE TAGS",
                                      fluidRow(
                                        column(12, selectizeInput(inputId = 'image_tags', label = 'Image tags', choices = NULL, selected = 1, multiple=T, options=list(create = T)), style=pad)
                                      )
                             ),
                             tabPanel("THEMES",
                                      fluidRow(
                                        column(12, selectizeInput(inputId = 'themes', label = 'Themes', choices = NULL, selected = 1, multiple = T), style=pad) #  (from GDELT Global Knowledge Graph)
                                      )
                             )
                 ),
                 fluidRow(
                   column(6, selectizeInput(inputId = 'search_country', label = 'Country', choices = country_codes, multiple = T, options=list(create = T)), style=pad),
                   column(6, textInput(inputId = 'search_domain', label = 'Domain', value = '', placeholder = 'e.g. "bbc.co.uk"'), style=pad)
                 ),
                 fluidRow(
                   column(6, dateRangeInput(inputId = "daterange", label = "Date range",
                                            start = NULL, end = NULL,
                                            min = add_days(now, -80), max = now), style=pad),
                   column(2, textInput(inputId = 'timespan', label='Recent', value='24h'), style=pad),
                   column(4, selectizeInput(inputId = 'source_lang', label = 'SourceLang', choices = lang_codes, multiple = T, options=list(create = T)), style=pad)
                 ),

                 # OUTPUTS
                 hr(),
                  fluidRow(
                   column(3, sliderInput('max_records', 'Records', min=75, max=250, step=5, value=75), style=pad),
                   column(6, selectInput(inputId = 'data_sort', label = 'Sort', choices = sort_options, selected = 'DateDesc'), style=pad),
                   column(2, checkboxInput(inputId = 'translate', label = 'GT', value = F))
                 ),
                 hr(),
                 fluidRow(
                   column(9, uiOutput('gdelt_url')),
                   column(3, actionButton("generate", "Make it!", width = '100%', style=pad))
                 ),
                 br(),
                 p('2017 Open Source Unit. This webpage has has no affiliation to GDELT.', id='footer', style="color:#999;font-size: 80%;")
    ),
    mainPanel(width = 8,
              fluidRow(
                column(5, textInput(inputId = 'plot_title', label = NULL, value = '', placeholder = '[ newsboard title ]', width = '100%'), style=pad),
                column(4, textInput(inputId = 'title_keywords', label = NULL, value = '', placeholder = '[ title keywords ]', width = '100%'), style=pad),
                column(1, checkboxInput(inputId = 'dedup_ims', label = 'Images', value = T), style=pad),
                column(1, checkboxInput(inputId = 'dedup_hls', label = 'H/L\'s', value = T), style=pad),
                column(1, actionButton("help", "Help", style='padding:0px 5px 0px 5px; margin:5px 0px 5px 0px;'))
              ),
              fluidRow(htmlOutput("frame")),
              uiOutput('refer')
    )
  )
)

shinyApp(ui, server)
