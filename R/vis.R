##' Visualize the master files interactively.  Creates a shiny app to
##' choose the master files to look at with datatables.
##'
##' @param update Check for updates to master files (default FALSE).
##' @import data.table
##' @import shiny
##' @export
idata <- function(update=FALSE) {
    sync.afs::check_afs()
    filename <- rname <- NULL
    dkey <- if (update) sync.afs::update_key() else data.table::copy(sync.afs::data_key)

    ## App
    dtypes <- c('sas7bdat', 'csv', 'rda')  # file types to include for data choices
    path <- sync.afs::get_afs()            # prepend to sync.afs::data_key[,afs_path]
    rfiles <- dkey[filetype %in% dtypes, rname]
    mfiles <- dkey[filetype %in% dtypes, filename]
    
    ui <- shinyUI(
        fluidPage(
            inputPanel(
                selectInput('data', 'Data', choices=rfiles, selected='pp_raw'),
                checkboxInput('useMasterNames', 'Use master file names', value=FALSE),
                actionButton('update', 'Update')
            ),
            fluidRow(
                column(6, DT::dataTableOutput('tbl')),
                column(6, verbatimTextOutput('columnSummary'))
            )
        )
    )

    server <- shinyServer(function(input, output, session) {
        vals <- reactiveValues(dat=data.frame())
        
        output$tbl <- DT::renderDataTable(
            vals$dat,
            filter='top',
            selection=list(mode='single', target='column'),
            options=list(pageLength=20)
        )

        output$columnSummary <- renderPrint({
            if (nrow(vals$dat) && !is.null((cols <- input$tbl_columns_selected))) {
                summary(vals$dat[, cols, with=FALSE])
            } else NULL
        })
        
        ## Get data
        observeEvent(input$update, {
            vals$dat <- if (input$useMasterNames) {
                            sync.afs::get_data(sync.afs::data_key[filename == input$data, rname],
                                               dkey=dkey)
                        } else sync.afs::get_data(input$data,
                                                  dkey=dkey)
            print(head(vals$dat))
        })

        ## Change b/w R/master file names
        observeEvent(input$useMasterNames, {
            if (input$useMasterNames) {
                choices <- dkey[filetype %in% dtypes, filename]
                current <- dkey[rname == input$data, filename]
            } else {
                choices <- dkey[filetype %in% dtypes, rname]
                current <- dkey[filename == input$data, rname]
            }
            updateSelectInput(session, inputId='data', choices=choices, selected=current)
        })
    })

    runApp(list(ui=ui, server=server))
}

