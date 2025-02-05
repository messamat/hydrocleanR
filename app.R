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

############################## SERVER ##########################################
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
  
  flag_dat <- reactive({
    req(input$xvar, dt$flags, dt$data)
    dt$flags <- dt$flags[eval(input$xvar) %in% dt$data[[input$xvar]],]
  })
  
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
        clip='off'
      ) +
      theme(legend.position = 'none')
  })
  
  #----------------------- Deal with zoomed plot brush -------------------------
  brush_trans <- reactiveVal(NULL)
  
  observeEvent(input$zoomedplot_brush, {
    brush_trans(input$zoomedplot_brush)
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

  #Display table of zoomed plot selection data
  output$brushrange <-  DT::renderDataTable({
    req(brush_trans())
    #req(zoomedplots_brush_trans)
    brushed_df <- brushedPoints(dt$data, brush_trans())
    #brushedPoints(dt$data, zoomedplots_brush_trans())
    
    if (nrow(brushed_df) == 0) return(NULL)  
    
    if (input$checkbox_scale > 1) {
      zoomed_sel <- brushed_df 
      if (input$checkbox_scale == 2) {
        zoomed_sel$ymin <- (zoomed_sel$ymin)^2
        zoomed_sel$ymax <- (zoomed_sel$ymax)^2
      } else if (input$checkbox_scale == 3) {
        zoomed_sel$mapping$y <- input$yvar
        zoomed_sel$ymin <- (zoomed_sel$ymin)-0.01
        zoomed_sel$ymax <- (zoomed_sel$ymax)-0.01
      }
      zoomed_sel
    } else {
      brushed_df 
    }
  })
  
  #-------------------- Deal with hover tooltip --------------------------------
  hover_store <- reactiveVal(NULL)
  
  observeEvent(input$plot_hover, {
    hover_store(input$plot_hover)
  })
  
  #Display a tooltip of X-Y data on hover in the zoomed plot
  #https://gitlab.com/-/snippets/16220
  output$hover_info <- renderUI({
    # req(input$plot_hover)
    # hover <- input$plot_hover
    req(hover_store())
    hover <- hover_store()
    hover$mapping$y <- input$yvar
    nearpoint <- nearPoints(dt$data, hover, 
                            threshold = 10, maxpoints = 1)
    if (nrow(nearpoint) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_px <- hover$coords_css$x
    top_px <- hover$coords_css$y
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style_hover <- paste0(
      "position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style_hover,
      p(HTML(paste0("<b>", input$xvar, ": </b>", 
                    nearpoint[[input$xvar]], "<br/>",
                    "<b>", input$yvar, ": </b>", 
                    nearpoint[[input$yvar]], "<br/>")))
    )
  })
  
  # observeEvent(input$del, {
  #   req(zoomedplots_brush_trans)
  #   dt$data <- brushedPoints(dt$data, 
  #                            zoomedplots_brush_trans(),
  #                            allRows=T)[selected_==FALSE]
  # })

  output$save <- downloadHandler(
    filename <- reactive({ 
      paste(file_path_sans_ext(inFile()$name), '_edit.csv', sep = '') 
    }),
    content = function(file) {
      fwrite(dt$data, file, row.names = F)
    }
  )
}


############################## UI ##############################################
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
                         ".qs",
                         '.fst')
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
             ),
             uiOutput("hover_info", style = "pointer-events: none"),
             fluidRow(
               column(3,
                      actionButton('del', "Delete")),
               column(3,
                      downloadButton('save', 'Save'))
             )
      )
    )
    ,

    fluidRow(
      # column(4,
      #        DT::dataTableOutput('grouptable')),
      DT::dataTableOutput('brushrange')
    )
  )
}


shinyApp(ui = ui, server= server, enableBookmarking = "url")