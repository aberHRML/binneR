#' Shiny app to view 
#' @name viewSpectrum
#' @description A shiny app to view converted MS data files.
#' @param file File path of data file to open
#' @param polarityOrder specified only when using mzML files. A numeric vector of the length of the number of polarities present in the file giving the polarity order. 
#' @author Jasen Finch
#' @examples 
#' \dontrun{
#' # for an mzXML file
#' viewSpectrum(file=list.files(system.file('mzXML',package='binneR'),full.names = T)[1])
#' 
#' # for an mzML file containing both polarities with negative scanning before positive
#' viewSpectrum(polarityOrder=c(0,1))
#' }
#' @importFrom shiny shinyApp fluidPage titlePanel sidebarLayout fileInput textInput
#' @importFrom shiny tags radioButtons uiOutput numericInput mainPanel plotOutput
#' @importFrom shiny reactive sidebarPanel renderUI sliderInput fluidRow renderPlot
#' @importFrom mzR header runInfo
#' @importFrom ggplot2 ggplot geom_line aes_string theme_bw ggtitle geom_vline xlab 
#' @importFrom ggplot2 ylab geom_segment geom_text

viewSpectrum <- function(file = NULL, polarityOrder = NULL){
  options(shiny.maxRequestSize = 500*1024^2)
  shinyApp(
    ui = fluidPage(
      
      titlePanel("viewSpectrum"),
      sidebarLayout(
        sidebarPanel(if (is.null(file)) {
          fileInput('file1', 'Upload Data File:',
                    accept = c(
                      '.mzXML','.mzML'
                    )
          )} else {
          	textInput('file1', 'Data File Path:',value = file)
          },
          tags$hr(),
          tags$b("Chromatogram"),
          radioButtons('chromY', '',
                       c(`Base Peak` = 'basePeak',
                         `Total Ion Count` = 'totIonCurrent')),
          radioButtons('chromX', '',
                       c(`Scan Number` = 'scanNum',
                         `Retention Time` = 'retentionTime')
                         ),
          uiOutput("selectChromRange"),
          tags$hr(),
          tags$b("Spectrum"),
          numericInput("dp","Decimal Places:",0),
          uiOutput("selectScan"),
          uiOutput("selectSpecRange")
        ),
        mainPanel(
          plotOutput('chromatogram.p'),
          plotOutput('spectrum.p'),
          plotOutput('chromatogram.n'),
          plotOutput('spectrum.n')
        )
      )
    ),
    
    server = function(input, output) {
      loadData <- reactive({
        if (is.null(input$file1)) {
          return(NULL)
        }
      	if (class(input$file1) == 'data.frame') {
        	aa <- openMSfile(input$file1$datapath)
      	} else {
      		aa <- openMSfile(input$file1)
      	}
        headers <- header(aa)
        if (grepl('.mzML',input$file1)) {
        	if (!is.null(polarityOrder)) {
        		headers$polarity <- rep(polarityOrder,(1 * 1/length(polarityOrder)) * length(headers$polarity))
        	} else {
        		cat('\nmzML file used, polarityOrder needs to be specified\n')
        	}
        }
        headers$retentionTime <- round(headers$retentionTime/60,2)
        head <- lapply(0:1,function(x,ms){return(ms[which(ms$polarity == x),])},ms = headers)
        names(head) <- c('neg','pos')
        head <- lapply(head,function(x){
          if (nrow(x) > 0) {
            x$seqNum <- seq(1,nrow(x))
          }
          return(x)
          })
        if (!(0 %in% sapply(head,nrow))) {
          if (length(head$pos$retentionTime) > length(head$neg$retentionTime)) {
            head$neg$retentionTime <- head$pos$retentionTime[1:length(head$neg$retentionTime)]
          } else {
            head$pos$retentionTime <- head$neg$retentionTime[1:length(head$pos$retentionTime)]
          }
        }
        pl <- peaks(aa)
        pl <- lapply(pl,function(x){x[,1] <- round(x[,1],input$dp);x <- aggregate(x[,2],by = list(x[,1]),sum);return(x)})
        pl <- lapply(pl,function(x){x <- data.frame(x); names(x) <- c("mz","intensity");return(x)})
        pl <- lapply(0:1,function(x,head,pl){return(pl[which(head$polarity == x)])},head = headers,pl = pl)
        names(pl) <- c('neg','pos')
        ms <- list(headerTable = head,peakList = pl,info = runInfo(aa))
        ms
      })
      chromRange <- reactive({
        aa <- loadData()
        modes <- as.numeric(which(sapply(aa$headerTable,nrow) > 0))
        if (is.null(input$file1)) {
          return(NULL)
        }
        if (input$chromX == "scanNum") {
          range <- list(min = 1,max = nrow(aa$headerTable[[modes[1]]]))
          range
        } else {
          range <- list(min = min(aa$headerTable[[modes[1]]]$retentionTime),max = max(aa$headerTable[[modes[1]]]$retentionTime))
          range
        }
      })
      specRange <- reactive({
        aa <- loadData()
        modes <- as.numeric(which(sapply(aa$headerTable,nrow) > 0))
        if (is.null(input$file1)) {
          return(NULL)
        }
        if (length(modes) > 1) {
          low <- floor(min(c(sapply(aa$peakList$pos,function(x){min(x[,1])})),sapply(aa$peakList$neg,function(x){min(x[,1])})))
          high <- ceiling(max(c(sapply(aa$peakList$pos,function(x){max(x[,1])})),sapply(aa$peakList$neg,function(x){max(x[,1])})))
        } else {
          low <- floor(min(sapply(aa$peakList[[modes[1]]],function(x){min(x[,1])})))
          high <- ceiling(max(sapply(aa$peakList[[modes[1]]],function(x){max(x[,1])})))
        }
        range <- list(min = low,max = high)
        range
      })
      output$selectChromRange <- renderUI({
        range <- chromRange()
        if (is.null(range)) {
          return(NULL)
        }
        sliderInput("rangeChrom", "Range:",
                    min = range$min, max = range$max, value = c(range$min,range$max))
      })
      output$selectScan <- renderUI({
        if (is.null(input$rangeChrom)) {
          return(NULL)
        }
        if (input$chromX == "retentionTime") {
          Xchrom <- "Retention Time Range:"
        } else {
          Xchrom <- "Scan Range:"
        }
        sliderInput("rangeScan",Xchrom,
                    min = input$rangeChrom[1], max = input$rangeChrom[2], value = c(input$rangeChrom[1],input$rangeChrom[2]))
      })
      output$selectSpecRange <- renderUI({
        range <- specRange()
        if (is.null(range)) {
          return(NULL)
        }
        fluidRow(
          tags$b("m/z Range"),
          numericInput("rangeSpecLow","From:",range$min),
          numericInput("rangeSpecHigh","To:",range$max)
        )
      })
      output$chromatogram.n <- renderPlot({
        aa <- loadData()
        if (is.null(aa)) {
          return(NULL)
        }
        if (nrow(aa$headerTable$neg) == 0) {
          return(NULL)
        }
        if (input$chromX == "retentionTime") {
          head.neg <- aa$headerTable$neg[which(aa$headerTable$neg$retentionTime >= input$rangeChrom[1] & aa$headerTable$neg$retentionTime <= input$rangeChrom[2]),]
        } else {
          head.neg <- aa$headerTable$neg[which(aa$headerTable$neg$seqNum >= input$rangeChrom[1] & aa$headerTable$neg$seqNum <= input$rangeChrom[2]),]
        }
        # Plot
        chrom.plot <- ggplot(head.neg) + geom_line() + theme_bw() + ggtitle("Negative Mode")
        if (input$chromX == "retentionTime") {
          chrom.plot <- chrom.plot + 
            aes_string(x = 'retentionTime') + 
            xlab("Retention Time (minutes)") + 
            geom_vline(xintercept = input$rangeScan[1],colour = "Red") +
            geom_vline(xintercept = input$rangeScan[2],colour = "Red")
        } else {
          chrom.plot <- chrom.plot + 
            aes_string(x = 'seqNum') + xlab("Scan Number") + 
            geom_vline(xintercept = input$rangeScan[1],colour = "Red") +
            geom_vline(xintercept = input$rangeScan[2],colour = "Red")
        }
        if (input$chromY == "basePeak") {
          chrom.plot <- chrom.plot + aes_string(y = 'basePeakIntensity') + ylab("Intensity")
        } else {
          chrom.plot <- chrom.plot + aes_string(y = 'totIonCurrent') + ylab("Intensity")
        }
        print(chrom.plot)
      })
      output$chromatogram.p <- renderPlot({
        aa <- loadData()
        if (is.null(aa)) {
          return(NULL)
        }
        if (nrow(aa$headerTable$pos) == 0) {
          return(NULL)
        }
        if (input$chromX == "retentionTime") {
          head.pos <- aa$headerTable$pos[which(aa$headerTable$pos$retentionTime >= input$rangeChrom[1] & aa$headerTable$pos$retentionTime <= input$rangeChrom[2]),]
        } else {
          head.pos <- aa$headerTable$pos[which(aa$headerTable$pos$seqNum >= input$rangeChrom[1] & aa$headerTable$pos$seqNum <= input$rangeChrom[2]),]
        }
        # Plot
        chrom.plot <- ggplot(head.pos) + geom_line() + theme_bw() + ggtitle("Positive Mode")
        if (input$chromX == "retentionTime") {
          chrom.plot <- chrom.plot + 
            aes_string(x = 'retentionTime') + 
            xlab("Retention Time (minutes)") + 
            geom_vline(xintercept = input$rangeScan[1],colour = "Red") +
            geom_vline(xintercept = input$rangeScan[2],colour = "Red")
        } else {
          chrom.plot <- chrom.plot + 
            aes_string(x = 'seqNum') + xlab("Scan Number") + 
            geom_vline(xintercept = input$rangeScan[1],colour = "Red") +
            geom_vline(xintercept = input$rangeScan[2],colour = "Red")
        }
        if (input$chromY == "basePeak") {
          chrom.plot <- chrom.plot + aes_string(y = 'basePeakIntensity') + ylab("Intensity")
        } else {
          chrom.plot <- chrom.plot + aes_string(y = 'totIonCurrent') + ylab("Intensity")
        }
        chrom.plot
      })
      output$spectrum.n <- renderPlot({
        aa <- loadData()
        if (is.null(aa)) {
          return(NULL)
        }
        if (nrow(aa$headerTable$neg) == 0) {
          return(NULL)
        }
        if (input$chromX == "retentionTime") {
          scan <- aa$peakList$neg[which(aa$headerTable$neg$retentionTime >= input$rangeScan[1] & aa$headerTable$neg$retentionTime >= input$rangeScan[2])]
        } else {
          scan <- aa$peakList$neg[which(aa$headerTable$neg$seqNum >= input$rangeScan[1] & aa$headerTable$neg$seqNum <= input$rangeScan[2])]
        }
        scan <- addMasses(scan)
        scan <- massMat(scan)
        scan <- apply(scan,2,mean)
        scan <- data.frame(mz = as.numeric(as.character(names(scan))),intensity = scan,row.names = NULL)
        scan <- scan[which(scan[,1] >= input$rangeSpecLow & scan[,1] <= input$rangeSpecHigh),]
        if (nrow(scan) < 1) {
          return(NULL)
        }
        labels <- data.frame(scan[which(scan[,2] == max(scan[,2])),])
        # Plot
        ggplot(scan,aes_string(x = 'mz',y = 0,xend = 'mz',yend = 'intensity')) + 
          geom_segment() + 
          geom_text(data = labels,aes_string(x = 'mz',y = 'intensity',label = 'mz'),hjust = 0) +
          theme_bw() + 
          xlab("m/z") + 
          ylab("Intensity")
      })
      output$spectrum.p <- renderPlot({
        aa <- loadData()
        if (is.null(aa)) {
          return(NULL)
        }
        if (nrow(aa$headerTable$pos) == 0) {
          return(NULL)
        }
        if (input$chromX == "retentionTime") {
          scan <- aa$peakList$pos[which(aa$headerTable$pos$retentionTime >= input$rangeScan[1] & aa$headerTable$pos$retentionTime >= input$rangeScan[2])]
        } else {
          scan <- aa$peakList$pos[which(aa$headerTable$pos$seqNum >= input$rangeScan[1] & aa$headerTable$pos$seqNum <= input$rangeScan[2])]
        }
        scan <- addMasses(scan)
        scan <- massMat(scan)
        scan <- apply(scan,2,mean)
        scan <- data.frame(mz = as.numeric(as.character(names(scan))),intensity = scan,row.names = NULL)
        scan <- scan[which(scan[,1] >= input$rangeSpecLow & scan[,1] <= input$rangeSpecHigh),]
        if (nrow(scan) < 1) {
          return(NULL)
        }
        labels <- data.frame(scan[which(scan[,2] == max(scan[,2])),])
        # Plot
        ggplot(scan,aes_string(x = 'mz',y = 0,xend = 'mz',yend = 'intensity')) + 
          geom_segment() + 
          geom_text(data = labels,aes_string(x = 'mz',y = 'intensity',label = 'mz'),hjust = 0) +
          theme_bw() + 
          xlab("m/z") + 
          ylab("Intensity")
      })
      
    }
  )
}