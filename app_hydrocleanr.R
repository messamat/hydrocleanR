library(shiny)
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
  
  #Create 
  observe({
    if (input$checkbox_date) {
      dt$data[, (eval(input$xvar)) := as.Date(get(input$xvar))]
    } else {
      dt$data <- myData()
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
  
  #Create table of sites
  table <- reactive({
    sites <- unique(dt$data[, eval(input$groupvar), with=F])
    sites_dt <- data.table(Groups = sites, Marked = rep(NA, length(sites)))
    colnames(sites_dt)[1] <- eval(input$groupvar)
    sites_dt
  })
  #Pass date object to UI
  output$grouptable <- DT::renderDataTable(table(),selection = 'single')
  
  #Get data to plot
  site_dat <- reactive({
    grp <- input$grouptable_rows_selected
    site <- table()[grp, eval(input$groupvar), with=F][[1]]
    sub <- dt$data[get(input$groupvar) == site,] #subset(dt$data, get(input$groupvar) == site) #
    sub
  })
  
  #output$sitedat <- DT::renderDataTable(site_dat())
  
  output$siteplot <- renderPlot({
    #ggplot(dat, aes_string(x = "SampleDateTime_format_dayonly", y = "dailymean")) + geom_point()
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
  
  output$brushrange <- renderText({
    if(!is.null(input$plot_brush$xmin)) {
      range <- data.table(
        xmin = as.Date(input$plot_brush$xmin, origin="1970-01-01"),
        xmax = as.Date(input$plot_brush$xmax, origin="1970-01-01"),
        ymin = input$plot_brush$ymin,
        ymax = input$plot_brush$ymax
      )
      if (input$checkbox_date) {
        paste0("xmin=", range$xmin, "  xmax=", range$xmax,
               "\nymin=", range$ymin, "  ymax=", range$ymax)
      }else {
        paste0("xmin=", range$xmin, "\nxmax=", range$xmax,
               "\nymin=", range$ymin, "  ymax=", range$ymax)
      }
    } else {
      print("Selected range")
    }
  })
  
  observeEvent(input$del, {
    brush_dat <- input$plot_brush
    if (input$checkbox_date) {
      brush_dat$xmin <- as.Date(brush_dat$xmin, origin="1970-01-01")
      brush_dat$xmax <- as.Date(brush_dat$xmax, origin="1970-01-01")
    }
    
    grp <- input$grouptable_rows_selected
    site <- table()[grp, get(input$groupvar)]
    #temp <- subset(dt$data, get(input$yvar) < 2)
    #dt$data <- temp
    
    dt$data <- dt$data[
      !(get(input$groupvar) == site 
        & ((get(input$xvar) > brush_dat$xmin 
            & get(input$xvar) < brush_dat$xmax)
           & (get(input$yvar) > brush_dat$ymin 
              & get(input$yvar) < brush_dat$ymax))),]
    
  })
  
  observeEvent(input$res, {
    grp <- input$grouptable_rows_selected
    site <- table()[grp, eval(input$groupvar), with=F]
    dt$data <- dt$data[!(get(input$groupvar) == site),]
    dt$data <- rbind(dt$data,
                     myData()[get(input$groupvar) == site,]
    )
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
    column(10,
           plotOutput('siteplot',
                      brush = brushOpts(id = "plot_brush", 
                                        clip = TRUE, resetOnNew = TRUE))
           
    )
  ),
  
  fluidRow(
    column(4,
           DT::dataTableOutput('grouptable')),
    column(8,
           verbatimTextOutput("brushrange"),
           fluidRow(
             column(4,
                    actionButton('del', "Delete")),
             column(4,
                    actionButton('res', 'Group reset'))),
           
           fluidRow(
             column(8,
                    downloadButton('save', 'Save')))
    )
  )
)


shinyApp(ui = ui, server= server)