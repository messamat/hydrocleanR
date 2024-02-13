library(shiny)
library(shinyjs)
#Make sure to keep library(DT) after Shiny as it overrides it
library(DT)
library(data.table)
#library(quantmod)
#library(plyr)
library(ggplot2)
library(qs)
library(tools)

#source("helpers.R")
options(shiny.maxRequestSize=2000*1024^2)


server <- function(input, output, session) {
  dt <- reactiveValues(data = NULL)
  
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
      choices= colnames(myData()))
  })
  
  #Create data
  observe({
    if (is.null(dt$data)) {
      dt$data <- myData()
    }
    if (input$checkbox_date) {
      dt$data[, (eval(input$xvar)) := as.Date(get(input$xvar))]
    }
  })
  
  #Create table of sites
  table <- reactive({
    sites <- unique(dt$data[, eval(input$groupvar), with=F])
    sites_dt <- data.table(Groups = sites, Marked = rep(NA, length(sites)))
    colnames(sites_dt)[1] <- eval(input$groupvar)
    sites_dt
  })
  
  #Pass date object to UI
  dd=reactiveValues(select=1)
  
  output$grouptable <- DT::renderDataTable(
    table(), 
    selection = list(mode='single', selected = dd$select)
  )
  
  #Get data to plot
  site_dat <- reactive({
    req(dt$data)
    dd$select <- input$grouptable_rows_selected
    site <- table()[dd$select, eval(input$groupvar), with=F][[1]]
    sub <- dt$data[get(input$groupvar) == site,] #subset(dt$data, get(input$groupvar) == site) #
    sub
  })
  
  #output$sitedat <- DT::renderDataTable(site_dat())
  
  main_plot <- reactive({
    req(site_dat())
    pc <- ggplot(site_dat(), 
                 aes_string(x = input$xvar, y = input$yvar, 
                            colour = input$colorvar)) +
      geom_point()
    
    if (input$checkbox_scale == 2) {
      pc <- pc + scale_y_sqrt()
    } else if (input$checkbox_scale == 3) {
      pc <- pc + scale_y_continuous(trans=scales::pseudo_log_trans(base = 10))
    }
    
    if (is.character(input$colorvar)) {
      if (class(site_dat()[[eval(input$colorvar)]]) %in% c('integer', 'numeric')) {
        pc <- pc + scale_color_distiller(palette='Spectral')
      }
    } 
    pc
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
    main_plot() + coord_cartesian(xlim=c(brush_dat$xmin, brush_dat$xmax))
  })
  
  output$brushrange <-  DT::renderDataTable({
    brushedPoints(site_dat(), input$zoomedplot_brush)
  })
  
  
  output$brushrangetxt <- renderText({
    if(!is.null(input$zoomedplot_brush$xmin)) {
      range <- data.table(
        xmin = fifelse(input$checkbox_date,
                       as.Date(input$zoomedplot_brush$xmin, origin="1970-01-01"),
                       input$zoomedplot_brush$xmin),
        xmax = fifelse(input$checkbox_date,
                       as.Date(input$zoomedplot_brush$xmax, origin="1970-01-01"),
                       input$zoomedplot_brush$xmax),
        ymin = input$zoomedplot_brush$ymin,
        ymax = input$zoomedplot_brush$ymax
      )
      paste0("xmin=", range$xmin, "\nxmax=", range$xmax,
             "\nymin=", range$ymin, "  ymax=", range$ymax)
    } else {
      print("Selected range")
    }
  })
  
  observeEvent(input$del, {
    # brush_datzoom <- input$zoomedplot_brush
    # if (input$checkbox_date) {
    #   brush_datzoom$xmin <- as.Date(brush_datzoom$xmin, origin = "1970-01-01")
    #   brush_datzoom$xmax <- as.Date(brush_datzoom$xmax, origin = "1970-01-01")
    # }
    # 
    # dd$select <- input$grouptable_rows_selected
    # site <- table()[dd$select, get(input$groupvar)]
    # 
    # dt$data <- dt$data[
    #   !(get(input$groupvar) == site &
    #       ((get(input$xvar) > brush_datzoom$xmin
    #         & get(input$xvar) < brush_datzoom$xmax) &
    #          (get(input$yvar) > brush_datzoom$ymin
    #           & get(input$yvar) < brush_datzoom$ymax))),]
    
    dt$data <- brushedPoints(site_dat(), 
                             input$zoomedplot_brush,
                             allRows=T)[selected_ == FALSE,]
  })
  
  # observeEvent(input$res, {
  #   dd$select <- input$grouptable_rows_selected
  #   site <- table()[dd$select, eval(input$groupvar), with=F]
  #   dt$data <- dt$data[!(get(input$groupvar) == site),]
  #   dt$data <- rbind(dt$data,
  #                    myData()[get(input$groupvar) == site,]
  #   )
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


ui <- fluidPage(
  titlePanel("Time series cleaning app"),
  
  #Create a logfile (See Observer demo in Shiny gallery)
  
  fluidRow(
    column(2, 
           fileInput("file", label = h3("Data table (.csv, .qs)"),
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
                         value = F),
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
                       "")
           
    ),
    column(5,
           plotOutput('mainplot',
                      brush = brushOpts(id = "mainplot_brush", 
                                        clip = TRUE, 
                                        resetOnNew = TRUE,
                                        direction = "x"))
           
    ),
    column(5,
           plotOutput('zoomedplot',
                      brush = brushOpts(id = "zoomedplot_brush", 
                                        clip = TRUE, 
                                        resetOnNew = TRUE))
           
    )
  ),
  
  fluidRow(
    column(4,
           DT::dataTableOutput('grouptable')),
    column(8,
           fluidRow(
             column(3,
                    actionButton('del', "Delete")),
             # column(3,
             #        actionButton('res', 'Group reset')),
             column(3,
                    downloadButton('save', 'Save'))),
           #verbatimTextOutput("brushrangetxt"),
           DT::dataTableOutput('brushrange')
    )
  )
)


shinyApp(ui = ui, server= server)