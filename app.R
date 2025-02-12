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
  
  zoom_history <- reactiveVal(list())
  
  myData <- reactive({
    req(input$file)
    if (is.null(input$file)){
      return(NULL)
    } else {
      #Don't forget () after a variable set as reactive
      if (tools::file_ext(input$file$datapath)=='csv') {
        fread(input$file$datapath)
      } else if (tools::file_ext(input$file$datapath)=='fst') { 
        as.data.table(read_fst(input$file$datapath))
      } else if (tools::file_ext(input$file$datapath)=='qs') {
        as.data.table(qread(input$file$datapath))
      }
    }
  })
  
  output$download_link <- renderUI({
    req(input$file)  # Ensure the path is selected before generating the link
    
    # Create a clickable link using tags$a()
    site_url <- sprintf('https://hydro.eaufrance.fr/sitehydro/%s/fiche', 
                      regmatches(input$file$name, 
                                 regexec('(?<=site_)[A-Z0-9]+', 
                                         input$file$name, 
                                         perl=T)
                      )
    )

    tags$a(href = site_url,
           "Hydroportail",
           target = "_blank")
  })
  
  #An observer is like a reactive expression in that it can read reactive values and call reactive expressions, 
  # and will automatically re-execute when those dependencies change. 
  #Unlike reactive expressions, it doesn't yield a result and can't be used as an input to other reactive expressions.
  #Reactive expressions use lazy evaluation; that is, when their dependencies change, they don't re-execute right away but rather wait until they are called by someone else.
  #Observers use eager evaluation; as soon as their dependencies change, they schedule themselves to re-execute.
  observe({
    req(myData())
    if (identical(myData(), '') || identical(myData(), data.table()))
      return(NULL)
    
    #   The input updater functions send a message to the client, telling it to change the settings of an input object. 
    #   The messages are collected and sent after all the observers (including outputs) have finished running.
    # updateSelectInput(
    #   session,
    #   inputId = "groupvar",
    #   choices = colnames(myData()),
    #   selected = colnames(myData())[[1]]
    # )
    
    # input$xvar <- 'date'
    # input$yvar <- 'flow'

    #Create selection of variables based on input table
    # updateSelectInput(
    #   session,
    #   inputId = "xvar",
    #   choices= colnames(myData()),
    #   selected = 'date'  #colnames(myData())[[min(c(ncol(myData()), 2))]]
    # )

    # updateSelectInput(
    #   session,
    #   inputId = "yvar",
    #   choices= colnames(myData()),
    #   selected = 'flow' #colnames(myData())[[min(c(ncol(myData()), 4))]]
    # )
    
    updateSelectInput(
      session,
      inputId = "colorvar",
      choices= c('flags', colnames(myData()), 'none'))
  })
  
  #Create data
  observe({
    #req(input$xvar)
    
    if (is.null(dt$data)) {
      dt$data <- myData()
    }
    # if (input$checkbox_date & (input$xvar %in% colnames(dt$data))) {
    #   dt$data[, (input$xvar) := as.Date(get(input$xvar))]
    # }
    if ('flow' %in% colnames(dt$data)) {
      dt$data[, flow_backup := flow]
    }
    
    if ('date' %in% colnames(dt$data)) { #(input$checkbox_date & 
      dt$data[, date := as.Date(date)]
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
  flag_dat <- reactive({
    req(dt$flags, dt$data) #input$xvar, 
    # dt$flags <- dt$flags[eval(input$xvar) %in% dt$data[[input$xvar]],]
    dt$flags <- dt$flags[date %in% dt$data[['date']],]
  })
  
  #Produce main plot
  main_plot <- reactive({
    req(dt$data)
    # pc <- ggplot(dt$data, 
    #              aes_string(x = input$xvar,
    #                         y = (input$yvar),
    #                         group=1)) 
    pc <- ggplot(dt$data, aes(x = date, y = flow, group=1)) 
    
    if (input$colorvar == 'flags') {
      color_dat <- dt$flags
      colorvar = 'value'
    } else if (input$colorvar == 'none') {
      color_dat <- NA
      colorvar <- NA
    } else {
      color_dat <- dt$data
      colorvar <- input$colorvar
    }

    if (input$checkbox_scale == 2) {
      pc <- pc +
        geom_line(alpha=1/2, color='#3A76C0') +
        geom_point(data=color_dat, aes(color = !!rlang::sym(colorvar))) +
        scale_y_sqrt()

    } else if (input$checkbox_scale == 3) {
      scalar <- 0.01
      pc <- pc +
        # geom_line(aes(y =!!rlang::sym(input$yvar) + scalar),
        #           alpha=1/2) +
        geom_line(aes(y = flow + scalar), alpha=1/2, color='#3A76C0') +
        geom_point(data=color_dat,
                   aes(colour = !!rlang::sym(colorvar),
                       y = flow + scalar)) +
                       # y =!!rlang::sym(input$yvar) + scalar)) +
        scale_y_log10(breaks=c(0.01, 0.02, 0.1, 1, 10, 100, 1000, 10000, 100000),
                      labels=c(0, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000),
                      expand=c(0,0))
    } else {
      pc <- pc + geom_line(alpha=1/2) +
        geom_point(data=color_dat,
                   aes_string(color = colorvar))
    }

    if (is.character(input$colorvar)) {
      if (class(dt$data[[eval(input$colorvar)]]) %in% c('integer', 'numeric')) {
        pc <- pc + scale_color_distiller(palette='Spectral')
      }
    }
    pc + theme_bw() +
      theme(text = element_text(size = 16))
  })
  
  output$mainplot <- renderPlot({
    main_plot()
  })
  
  # Create a reactive Value to store the brush x-range.
  savedBrush <- reactiveVal(NULL)
  
  # Modify zoomedplot to use the stored plot object ----------------------------
  output$zoomedplot <- renderPlot({
    # Use the saved brush range.
    brush_dat <- savedBrush()
    req(brush_dat)  # Make sure it exists.
    
    # If the checkbox for date conversion is on, convert the saved values.
    #if (input$checkbox_date) {
    brush_dat$xmin <- as.Date(brush_dat$xmin, origin="1970-01-01")
    brush_dat$xmax <- as.Date(brush_dat$xmax, origin="1970-01-01")
    #}
    
    main_plot() +
      #geom_point(color = 'black', shape=1, alpha=1/4) +
      coord_cartesian(
        xlim=c(brush_dat$xmin, brush_dat$xmax),
        expand=FALSE,
        clip='on') +
      theme(legend.position = 'none')
  })
  
  # When the "Last 6 months" button is clicked ---------------------------------
  observeEvent(input$last_6months, {
    req(savedBrush())  # Make sure there's a stored brush range
    brush_dat <- savedBrush()
    
    # Shift by 6 months
    brush_dat$xmin <- brush_dat$xmin - 183
    brush_dat$xmax <- brush_dat$xmax - 183
    
    # Save the current zoom state before updating
    history <- zoom_history()
    zoom_history(append(history, list(brush_dat)))
    
    # Save the new brush range
    savedBrush(brush_dat)
  })
  
  # When the "Next 6 months" button is clicked ---------------------------------
  observeEvent(input$next_6months, {
    req(savedBrush())  # Make sure there's a stored brush range
    brush_dat <- savedBrush()
    
    # Save the current zoom state before updating
    history <- zoom_history()
    zoom_history(append(history, list(brush_dat)))
    
    # Shift by 1 month (30 days)
    brush_dat$xmin <- brush_dat$xmin + 183
    brush_dat$xmax <- brush_dat$xmax + 183
    
    # Save the new brush range
    savedBrush(brush_dat)
  })
  
  # When the "Last month" button is clicked ---------------------------------
  observeEvent(input$last_month, {
    req(savedBrush())  # Make sure there's a stored brush range
    brush_dat <- savedBrush()
    
    # Save the current zoom state before updating
    history <- zoom_history()
    zoom_history(append(history, list(brush_dat)))
    
    # Shift by 1 month (30 days)
    brush_dat$xmin <- brush_dat$xmin - 30
    brush_dat$xmax <- brush_dat$xmax - 30
    
    # Save the new brush range
    savedBrush(brush_dat)
  })
  
  # When the "Next month" button is clicked ---------------------------------
  observeEvent(input$next_month, {
    req(savedBrush())  # Make sure there's a stored brush range
    brush_dat <- savedBrush()
    
    # Save the current zoom state before updating
    history <- zoom_history()
    zoom_history(append(history, list(brush_dat)))
    
    # Shift by 1 month (30 days)
    brush_dat$xmin <- brush_dat$xmin + 30
    brush_dat$xmax <- brush_dat$xmax + 30
    
    # Save the new brush range
    savedBrush(brush_dat)
  })
  
  #Save brush history -----------------------------------------------------------
  observe({
    req(input$mainplot_brush)
    # Save the brush's x-range as a list.
    savedBrush(list(
      xmin = input$mainplot_brush$xmin,
      xmax = input$mainplot_brush$xmax
    ))
  })
  
  #Grab and transform y-values of zoomed plot selection ------------------------
  zoomedplots_brush_trans <- reactive({
    req(input$zoomedplot_brush)
    zoomed_sel <- input$zoomedplot_brush
    if (input$checkbox_scale == 2) {
      zoomed_sel$ymin <- (zoomed_sel$ymin)^2
      zoomed_sel$ymax <- (zoomed_sel$ymax)^2
    } else if (input$checkbox_scale == 3) {
      # zoomed_sel$mapping$y <- input$yvar
      zoomed_sel$mapping$y <- 'flow'
      zoomed_sel$ymin <- (zoomed_sel$ymin)-0.01
      zoomed_sel$ymax <- (zoomed_sel$ymax)-0.01
    }
    zoomed_sel
  })

  #Display table of zoomed plot selection data----------------------------------
  output$brushrange <-  DT::renderDataTable(
    expr = {
      req(zoomedplots_brush_trans)
      brushed_df <- brushedPoints(dt$data, zoomedplots_brush_trans())
      
      if (nrow(brushed_df) == 0) {
        return(NULL)  # Prevent errors on empty selection
      } else {
        return(brushed_df[, !'flow_backup', with=F])
      }
    }, 
    options = list(iDisplayLength = 50))
  
  #Allow zooming to zoomed plot selection data----------------------------------
  observeEvent(input$zoomplot_to_selection, {
    req(input$zoomedplot_brush)
    
    #Get the current zoome range
    brush_dat <- savedBrush()
    
    # Save the current zoom state before updating
    history <- zoom_history()
    zoom_history(append(history, list(brush_dat)))
    
    # Update zoom range based on selection
    brush_dat$xmin <- input$zoomedplot_brush$xmin
    brush_dat$xmax <- input$zoomedplot_brush$xmax
    
    # Save the new brush range
    savedBrush(brush_dat)
  })
  
  #Go back zoom observer -------------------------------------------------------
  observeEvent(input$go_back_zoom, {
    history <- zoom_history()
    req(length(history) > 0)  # Ensure there's history to go back to
    
    # Pop the last zoom state
    last_zoom <- tail(history, 1)[[1]]
    zoom_history(head(history, -1))  # Remove last state from history
    
    # Restore the previous zoom
    savedBrush(last_zoom)
  })
  
  #Display a tooltip of X-Y data on hover in the zoomed plot--------------------
  #https://gitlab.com/-/snippets/16220
  output$hover_info <- renderUI({
    req(input$plot_hover)
    hover_dat <- isolate(input$plot_hover)
    # hover_dat$mapping$y <- input$yvar
    hover_dat$mapping$y <- 'flow'
    nearpoint <- nearPoints(dt$data, hover_dat, threshold = 10, maxpoints = 1)
    if (nrow(nearpoint) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_px <- hover_dat$coords_css$x
    top_px <- hover_dat$coords_css$y
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style_hover <- paste0(
      "position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style_hover,
      # p(HTML(paste0("<b>", input$xvar, ": </b>", 
      #               nearpoint[[input$xvar]], "<br/>",
      #               "<b>", input$yvar, ": </b>", 
      #               nearpoint[[input$yvar]], "<br/>")))
      p(HTML(paste0("<b>Date: </b>", 
                    nearpoint$date, "<br/>",
                    "<b>Flow: </b>", 
                    nearpoint$flow, "<br/>")))
    )
  })
  
  #Delete points ---------------------------------------------------------------
  observeEvent(input$del, {
    req(zoomedplots_brush_trans)
    # Get the brush selection and mark the selected points as deleted
    selected_data <- brushedPoints(dt$data, 
                                   zoomedplots_brush_trans(),
                                   allRows = TRUE)
    
    # Set yvar to NA for the rows that will be deleted (selected_ == TRUE)
    dt$data[selected_data$selected_ == TRUE, 'flow'] <- NA
    # dt$data[selected_data$selected_ == TRUE, input$yvar] <- NA
  })

  #Download data  --------------------------------------------------------------
  output$save <- downloadHandler(
    filename <- reactive({ 
      paste(file_path_sans_ext(input$file$name), '_edit.csv', sep = '') 
    }),
    content = function(file) {
      fwrite(dt$data, file, row.names = F)
    }
  )
}


############################## UI ##############################################
ui <- function(request){
  fluidPage(
    #titlePanel("Time series cleaning app"),
    fluidRow(
      column(1, 
             #bookmarkButton(),
             fileInput("file", label = h5("Data table"),
                       accept = c(
                         "text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv",
                         ".qs",
                         '.fst')
             ),
             # selectInput("groupvar", 
             #             label = h5("Group variable"),
             #             ""),
             # selectizeInput("xvar",
             #                label = h5("X variable"),
             #                choices = "",
             #                options = list(closeAfterSelect=TRUE)
             #                ),
             # checkboxInput("checkbox_date",
             #               label = "Convert X variable to dates?", 
             #               value = T),
             # print("Warning: once checked, unchecking will cancel
             #       temporary changes to data"),
             
             # selectInput("yvar",
             #             label = h5("Y variable"),
             #             ""),
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
                         selected = 1),
             
             uiOutput("download_link") 
             
      ),
      column(4,
             plotOutput('mainplot',
                        height='600px',
                        brush = brushOpts(id = "mainplot_brush", 
                                          clip = TRUE, 
                                          resetOnNew = TRUE,
                                          direction = "x"))
      ),
      column(6,
             plotOutput('zoomedplot',
                        height='600px',
                        brush = brushOpts(id = "zoomedplot_brush",
                                          clip = TRUE,
                                          resetOnNew = TRUE)
                        ,
                        hover = hoverOpts(id="plot_hover",
                                          delay = 10,
                                          delayType = "debounce",
                                          clip=T
                        )
             ),
             uiOutput("hover_info", style = "pointer-events: none"),
             fluidRow(
               actionButton('last_6months', "Last 6 months"),
               actionButton('next_6months', "Next 6 months"),
               actionButton('zoomplot_to_selection', 'Zoom to selection'),
               actionButton("go_back_zoom", "Undo zoom"),
               actionButton('last_month', "Last month"),
               actionButton('next_month', "Next month"),
             ),
             fluidRow(
               actionButton('del', "Delete", 
                                      style="color: #fff; background-color: #dd7055; border-color: darkred"),
               downloadButton('save', 'Save', 
                                        style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
             ),
      )
    )
    ,
    # Place the table in its own fluidRow and wrap it in a fixed-height, scrollable div.
    fluidRow(
      div(id = "table_container", 
          style = "height:300px; overflow-y:auto; margin-top: 0px;",
          DT::dataTableOutput('brushrange')
      )
    )
  )
}


shinyApp(ui = ui, server= server, enableBookmarking = "url")