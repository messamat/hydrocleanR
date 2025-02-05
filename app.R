library(shiny)
library(shinyBS)
#Make sure to keep library(DT) after Shiny as it overrides it
library(DT)
library(data.table)
library(fst)
library(ggplot2)
library(qs)
#library(reactlog) #for debugging: https://shiny.posit.co/r/getstarted/build-an-app/reactivity-essentials/using-reactives.html
library(tools)

#source("helpers.R")
options(shiny.maxRequestSize=2000*1024^2)

server <- function(input, output, session) {
  dt <- reactiveValues(data = NULL,
                       flags = NULL)
  
  inFile <- reactive({
    if (is.null(input$file)){
      return(NULL)
    } else {
      input$file
    }
  })
  
  myData <- reactive({
    if (is.null(inFile())){
      return(NULL)
    } else {
      #Don't forget () after a variable set as reactive
      if (tools::file_ext(inFile()$datapath)=='csv') {
        fread(inFile()$datapath)
      } else if (tools::file_ext(inFile()$datapath)=='fst') { 
        as.data.table(read_fst(inFile()$datapath))
      } else if (tools::file_ext(inFile()$datapath)=='qs') {
        as.data.table(qread(inFile()$datapath))
      }
    }
  })
  
  #An observer is like a reactive expression in that it can read reactive values and call reactive expressions, 
  # and will automatically re-execute when those dependencies change. 
  #Unlike reactive expressions, it doesn't yield a result and can't be used as an input to other reactive expressions.
  #Reactive expressions use lazy evaluation; that is, when their dependencies change, they don't re-execute right away but rather wait until they are called by someone else.
  #Observers use eager evaluation; as soon as their dependencies change, they schedule themselves to re-execute.
  observe({
    if (identical(myData(), '') || identical(myData(), data.table()))
      return(NULL)
    
    #   The input updater functions send a message to the client, telling it to change the settings of an input object. 
    #   The messages are collected and sent after all the observers (including outputs) have finished running.
    updateSelectInput(
      session,
      inputId = "groupvar",
      choices = colnames(myData()),
      selected = colnames(myData())[[1]]
    )
    
    #Create selection of variables based on input table
    updateSelectInput(
      session,
      inputId = "xvar",
      choices= colnames(myData()),
      selected = colnames(myData())[[min(c(ncol(myData()), 2))]]
    )
    
    updateSelectInput(
      session,
      inputId = "yvar",
      choices= colnames(myData()),
      selected = colnames(myData())[[min(c(ncol(myData()), 4))]]
    )
    
    updateSelectInput(
      session,
      inputId = "colorvar",
      choices= c('flags', colnames(myData()), 'none'))
  })
  
  #Create data
  observe({
    if (is.null(dt$data)) {
      dt$data <- myData()
    }
    if (input$checkbox_date & (input$xvar %in% colnames(dt$data))) {
      dt$data[, (eval(input$xvar)) := as.Date(get(input$xvar))]
    }
  })
  
  #Create flags
  observe({
    if (!is.null(dt$data)) {
      dt$flags <- melt(dt$data,
                       id.vars=grep('tag_([2-9]|10)', names(dt$data),
                                    value=T, invert=T)
      ) %>%
        .[!is.na(value),]
    }
  })
  
  #Pass date object to UI
  dd=reactiveValues(select=1)
  
  # output$grouptable <- DT::renderDataTable(
  #   table(), 
  #   selection = list(mode='single', selected = dd$select)
  # )
  
  #Get data to plot
  # site_dat <- reactive({
  #   req(dt$data)
  #   #dd$select <- input$grouptable_rows_selected
  #   site <- table()[dd$select, eval(input$groupvar), with=F][[1]]
  #   sub <- dt$data[get(input$groupvar) == site,] #subset(dt$data, get(input$groupvar) == site) #
  #   sub
  # })
  
  flag_dat <- reactive({
    req(input$xvar, dt$flags, dt$data)
    dt$flags <- dt$flags[eval(input$xvar) %in% dt$data[[input$xvar]],]
  })
  
  #output$sitedat <- DT::renderDataTable(site_dat())
  
  main_plot <- reactive({
    req(dt$data)
    pc <- ggplot(dt$data, 
                 aes_string(x = input$xvar,
                            y = (input$yvar),
                            group=input$groupvar)) 
    
    # if (input$colorvar == 'flags') {
    #   color_dat <- dt$flags
    #   colorvar = 'value'
    # } else if (input$colorvar == 'none') {
    #   color_dat <- NA
    #   colorvar <- NA
    # } else {
    #   color_dat <- dt$data
    #   colorvar <- input$colorvar
    # }
    #
    if (input$checkbox_scale == 2) {
      pc <- pc +
        geom_line(alpha=1/2) +
        geom_point(data=dt$flags, aes_string(color = value)) +
        scale_y_sqrt()

    } else if (input$checkbox_scale == 3) {
      scalar <- 0.01
      pc <- pc +
        geom_line(aes(y =!!rlang::sym(input$yvar) + scalar),
                  alpha=1/2) +
        geom_point(data=dt$flags,
                   aes(colour = value,
                       y =!!rlang::sym(input$yvar) + scalar)) +
        scale_y_log10(breaks=c(0.01, 0.02, 0.1, 1, 10, 100, 1000, 10000, 100000),
                      labels=c(0, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                      expand=c(0,0))
    } else {
      pc <- pc + geom_line(alpha=1/2) +
        geom_point(data=dt$flags,
                   aes_string(color = 'value'))
    }

    if (is.character(input$colorvar)) {
      if (class(dt$data[[eval(input$colorvar)]]) %in% c('integer', 'numeric')) {
        pc <- pc + scale_color_distiller(palette='Spectral')
      }
    }
    pc + theme_bw()
  })
  
  output$mainplot <- renderPlot({
    main_plot()
  })
  
  # Modify zoomedplot to use the stored plot object
  output$zoomedplot <- renderPlot({
    req(input$mainplot_brush)
    brush_dat <- input$mainplot_brush
    
    if (input$checkbox_date) {
      brush_dat$xmin <- as.Date(brush_dat$xmin, origin="1970-01-01")
      brush_dat$xmax <- as.Date(brush_dat$xmax, origin="1970-01-01")
    }
    
    main_plot() +
      coord_cartesian(
        xlim=c(brush_dat$xmin, brush_dat$xmax),
        expand=FALSE,
        clip='off') +
      theme(legend.position = 'none')
  })
  
  #Grab and transform zoomed plot selection
  zoomedplots_brush_trans <- reactive({
    req(input$zoomedplot_brush)
    zoomed_sel <- input$zoomedplot_brush
    if (input$checkbox_scale == 2) {
      zoomed_sel$ymin <- (zoomed_sel$ymin)^2
      zoomed_sel$ymax <- (zoomed_sel$ymax)^2
    } else if (input$checkbox_scale == 3) {
      zoomed_sel$mapping$y <- input$yvar
      zoomed_sel$ymin <- (zoomed_sel$ymin)-0.01
      zoomed_sel$ymax <- (zoomed_sel$ymax)-0.01
    }
    zoomed_sel
  })
  
  output$brushrange <-  DT::renderDataTable({
    req(zoomedplots_brush_trans)
    brushedPoints(dt$data, zoomedplots_brush_trans())
  })
  
  output$hovertxt <- renderText({
    req(input$plot_hover)
    hover_format <- input$plot_hover
    hover_format$mapping$y <- input$yvar
    nearpoint <- nearPoints(dt$data, hover_format, 
                            threshold = 10, maxpoints = 1)
    if (!is.null(nearpoint)) {
      paste("X:", nearpoint[[input$xvar]],
            "Y:", nearpoint[[input$yvar]])
    }
  })
  
  observeEvent(input$del, {
    req(zoomedplots_brush_trans)
    dt$data <- brushedPoints(dt$data, 
                             zoomedplots_brush_trans(),
                             allRows=T)[selected_==FALSE]
  })
  
  output$save <- downloadHandler(
    filename <- reactive({ 
      paste(file_path_sans_ext(inFile()$name), '_edit.csv', sep = '') 
    }),
    content = function(file) {
      fwrite(dt$data, file, row.names = F)
    }
  )
}


ui <- function(request){
  fluidPage(
    titlePanel("Time series cleaning app"),
    fluidRow(
      column(2, 
             #bookmarkButton(),
             fileInput("file", label = h3("Data table (.csv, .qs, .fst)"),
                       accept = c(
                         "text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv",
                         ".qs")
             ),
             selectInput("groupvar", 
                         label = h5("Group variable"),
                         ""),
             selectInput("xvar", 
                         label = h5("X variable"),
                         ""),
             checkboxInput("checkbox_date",
                           label = "Convert X variable to dates?", 
                           value = T),
             # print("Warning: once checked, unchecking will cancel
             #       temporary changes to data"),
             
             selectInput("yvar", 
                         label = h5("Y variable"),
                         ""),
             radioButtons("checkbox_scale",
                          label = "Scale Y variable?", 
                          choices = list("No scaling" = 1,
                                         "Square root" = 2,
                                         "Log" = 3),
                          selected = 1),
             #print("Warning: once chosen, changing will cancel temporary changes to data"),
             
             selectInput("colorvar",
                         label = h5("Color-coding variable"),
                         choices = "flags",
                         selected = 1)
             
      ),
      column(5,
             plotOutput('mainplot',
                        height='600px',
                        brush = brushOpts(id = "mainplot_brush", 
                                          clip = TRUE, 
                                          resetOnNew = TRUE,
                                          direction = "x"))
      ),
      column(5,
             plotOutput('zoomedplot',
                        height='600px',
                        brush = brushOpts(id = "zoomedplot_brush", 
                                          clip = TRUE, 
                                          resetOnNew = TRUE),
                        hover = hoverOpts(id="plot_hover",
                                          delay = 10,
                                          clip=F
                        )
             )
      )
    ),
    
    fluidRow(
      # column(4,
      #        DT::dataTableOutput('grouptable')),
      column(8,
             fluidRow(
               column(3,
                      actionButton('del', "Delete")),
               # column(3,
               #        actionButton('res', 'Group reset')),
               column(3,
                      downloadButton('save', 'Save'))),
             verbatimTextOutput("hovertxt"),
             #verbatimTextOutput("brushrangetxt"),
             DT::dataTableOutput('brushrange')
      )
    )
  )
}


shinyApp(ui = ui, server= server, enableBookmarking = "url")