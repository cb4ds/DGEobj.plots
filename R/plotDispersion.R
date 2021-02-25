#' Plot edgeR dispersion from a DGEobj
#'
#' Creates an edgeR dispersion plot for RNA-Seq QC purposes.  Takes a counts matrix
#' or DGEList for input.  Dispersion is plotted against AveLogCPM.  Optionally,
#' the plot can instead be Biological Coefficient of Variation (BCV is the square root of
#' dispersion) against AveLogCPM.
#'
#' @param DGEdata Counts matrix or DGEList (required)
#' @param designMatrix A design matrix created by stats::model.matrix (required)
#' @param plotType Plot type must be canvasXpress or ggplot (default = canvasXpress).
#' @param plotCategory One of "dispersion" or "BCV" (default = "dispersion")
#' @param symbolSize (default = 3)
#' @param symbolShape (default = "circle") @seealso \url{http://www.cookbook-r.com/Graphs/Shapes_and_line_types/}
#' @param symbolColor Fill color (default = "darkblue")
#' @param symbolFill  This attribute is specific to ggplot. If the symbolShape which has a fill is specified, symbolColor becomes the outline and
#'  symbolFill becomes the till color  (default = "darkblue")
#' @param symbolAlpha Transparency for the points. Value ranges from 0 to 1. Smaller
#'   indicate more transparency (default = 0.3)
#' @param linefitColor (default = "yellow")
#' @param lineFit (default = NULL) Any type supported by geom_smooth(if plotType is ggplot). Loess is
#'   recommended.
#' @param lineType Any ggplot2 lineType supported by geom_smooth (default = solid)
#' @param ... Extra parameters to pass to edgeR::estimateDisp
#'
#' @return canvasxpress or ggplot object based on plotType selection
#'
#' @examples
#' \dontrun{
#'    # canvasxpress plot
#'    myCxplot <- plotDispersion(myDGElist)
#'    myCxplot <- plotDispersion(myDGEobj)
#'
#'    # ggplot
#'    myGgplot <- plotDispersion(myDGElist, plotType = "ggplot")
#'    myGgplot <- plotDispersion(myDGEobj, plotType = "ggplot")
#' }
#'
#' @importFrom assertthat assert_that
#' @importFrom edgeR calcNormFactors estimateDisp DGEList
#' @importFrom canvasXpress canvasXpress
#'
#' @export
plotDispersion <- function(DGEdata,
                           designMatrix,
                           plotType = "canvasXpress",
                           plotCategory = "dispersion",
                           symbolSize = 3,
                           symbolShape = "circle",
                           symbolColor = "darkblue",
                           symbolFill = "darkblue",
                           symbolAlpha = 0.3,
                           linefitColor = "yellow",
                           lineFit = NULL,
                           lineType = "solid",
                           ...) {

    assertthat::assert_that(!missing(DGEdata),
                            !missing(designMatrix),
                            msg = "Both DGEdata and designMatrix must be specified.")

    assertthat::assert_that(class(DGEdata)[[1]] == "matrix" | class(DGEdata)[[1]] == "DGEList",
                            "matrix" %in% class(designMatrix),
                            msg = "DGEdata must be of class 'matrix' or 'DGEList' and designMatrix must be of class 'matrix'.")

    assertthat::assert_that(tolower(plotType) %in% c("canvasxpress", "ggplot"),
                            msg = "Plot type must either be canvasXpress or ggplot.")

    assertthat::assert_that(tolower(plotCategory) %in% c("dispersion", "bcv"),
                            msg = "Plot Category must  either be dispersion or BCV.")

    if (tolower(plotType) == "canvasxpress") {
        if (!is.character(symbolShape) | !length(symbolShape) == 1 ) {
            message("symbolShape must be of class 'character'. Assigning default value = 'circle'")
            symbolShape = "circle"
        }
    }

    if (tolower(plotType) == "ggplot") {
        if (!(class(symbolShape) %in% c("character", "numeric")) | !length(symbolShape) == 1) {
            message("symbolShape must be a singular numeric value between 0 and 25 or must
                    be of class 'character'. Refer help section for the list of shapes supported. Assigning default value 'circle'.")
            symbolShape = "circle"
        }
    }

    if (!is.numeric(symbolSize) | !length(symbolSize) == 1) {
        message("symbolSize must be a singular numeric value. Assigning a default value of 3.")
        symbolSize = 3
    }

    if (!is.character(symbolColor) | !length(symbolColor) == 1) {
        message("symbolColor must be of class character and must specify the name of the
                                color or the rgb value. Assigning default value 'deepblue'.")
        symbolColor = "deepblue"
    }

    if (!is.character(symbolFill) | !length(symbolFill) == 1) {
        message("symbolFill must be of class character and must specify the name of the
                color or the rgb value. Assigning default value 'deepblue'.")
        symbolFill = "deepblue"
    }

    if (!is.numeric(symbolAlpha) | (symbolAlpha < 0) | (symbolAlpha > 1) | !length(symbolAlpha) == 1) {
        message("symbolAlpha must be a singular numeric value and must be between 0 and 1. Assigning default value 0.3.")
        symbolAlpha = 0.3
    }


    if (!is.null(lineFit)) {

        if (!is.character(linefitColor) | !length(linefitColor) == 1) {
            message("linefitColor must be of class character and must specify the name of the color or the rgb value. Assigning default value 'yellow'.")
            linefitColor = "yellow"
        }

        if (tolower(plotType) == "ggplot") {
            ggplot_lineType = !(class(lineType) %in% c("character", "numeric")) | !length(lineType)
            if (ggplot_lineType) {
                message("lineType must be a singular numeric value between 0 and 6 or must
                    be of class 'character'. Refer help section for the list of line types supported. Assigning default value 'solid'.")
                lineType = "solid"
            }
        }

        if (tolower(plotType) == "canvasxpress") {
            cxplot_lineType = !is.character(lineType) | !length(lineType) == 1
            if (cxplot_lineType) {
                message("lineType must be a must be of class 'character'. Refer help section for the list of line types supported. Assigning default value 'solid'.")
                lineType = "solid"
            }
        }
    }


    if (class(DGEdata)[[1]] == "DGEList") {
        dgelist <- DGEdata %>%
            edgeR::calcNormFactors() %>%
            edgeR::estimateDisp(design = designMatrix, robust = TRUE, ...)
    } else {
        dgelist <- DGEdata %>%  # Process a counts matrix
            as.matrix %>%
            edgeR::DGEList() %>%
            edgeR::calcNormFactors() %>%
            edgeR::estimateDisp(design = designMatrix, robust = TRUE, ...)
    }

    if (tolower(plotCategory) == "dispersion") {
        plotdata <- data.frame(AveLogCPM = dgelist$AveLogCPM, Dispersion = dgelist$tagwise.dispersion)
        title <- "EdgeR Dispersion Plot"
        ylab  <- "Dispersion"
    } else {
        plotdata <- data.frame(AveLogCPM = dgelist$AveLogCPM, Dispersion = sqrt(dgelist$tagwise.dispersion))
        title <- "EdgeR BCV Plot"
        ylab  <- "BCV"
    }
    rownames(plotdata) <- rownames(dgelist$counts)

    if (plotType == "canvasXpress") {
        showLoessFit <- FALSE
        afterRender <- NULL
        if (!is.null(lineFit)) {
            lineFit <- cxSupportedLineFit(lineFit)
            if (lineFit == "lm") {
                afterRender <- list(list("addRegressionLine"))
            } else if (lineFit == "loess") {
                showLoessFit <- TRUE
            }
        }

        if (tolower(plotCategory) == "bcv") {
            colnames(plotdata) <- c("AveLogCPM", "BCV")
        }

        MyDispPlot <- canvasXpress::canvasXpress(data                    = plotdata,
                                                 graphType               = "Scatter2D",
                                                 colors                  = symbolFill,
                                                 dataPointSize           = symbolSize,
                                                 shapes                  = symbolShape,
                                                 title                   = title,
                                                 yAxisTitle              = ylab,
                                                 showLoessFit            = showLoessFit,
                                                 fitLineColor            = linefitColor,
                                                 fitLineStyle            = lineType,
                                                 afterRender             = afterRender)
    } else {
        MyDispPlot <- ggplot(plotdata, aes(x = AveLogCPM, y = Dispersion)) +
            geom_point(size = symbolSize,
                       shape = symbolShape,
                       fill = symbolFill,
                       color = symbolColor,
                       alpha = symbolAlpha)

        if (!is.null(lineFit)) {
            MyDispPlot <- MyDispPlot +
                geom_smooth(formula = y ~ x,
                            method = lineFit,
                            color = linefitColor,
                            linetype = lineType)
        }

        MyDispPlot <- MyDispPlot +
            ylab(ylab) +
            ggtitle(title) +
            expand_limits(x = 0, y = 0)
    }

    return(MyDispPlot)
}
