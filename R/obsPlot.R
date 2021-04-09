#' Function obsPlot
#'
#' Provides a summary plot (box/violin/points) for each observation (gene), showing data for each
#' experiment group. The plot can optionally include one or more of the
#' following layers: boxplot, violin plot, individual points, and/or mean of all
#' points.  The layers are built up in the order listed with user settable
#' transparency, colors etc.  By default, the boxplot, point, and mean layers are
#' active. Also, by default, the plots are faceted.  Faceting the plot can be turned
#' off to return a list of individual ggplot graphics for each gene.
#'
#' Input is a tidy dataframe of intensity data.
#'
#' @param data A tidy dataframe of intensity data with row and colnames (required)
#' @param plotByCol Define the column name to separate plots (typically a gene ID column) (required)
#' @param groupCol Define the column name to group boxplots by (typically a replicate group column) (required)
#' @param valueCol Define the column of values for plotting (typically a log intensity measure) (required)
#' @param groupOrder Define the order for the groups in each plot.  Should
#'   contain values in unique(data$group) listed in the order that you want the
#'   groups to appear in the plot. (optional; default = unique(data[groupCol]))
#' @param boxLayer Adds a boxplot layer (Default = TRUE)
#' @param violinLayer Adds a violin layer (Default = FALSE)
#' @param pointLayer Adds a point layer (Default = TRUE)
#' @param meanLayer Adds a mean layer (Default = TRUE)
#' @param xlab X axis label (defaults to groupCol)
#' @param ylab Y axis label (defaults to valueCol)
#' @param title Plot title (optional)
#' @param boxColor Color for the boxplot layer (Default = "deepskyblue3")
#' @param boxFill Fill Color for the boxplot layer (Default = "deepskyblue3")
#' @param boxTransparency Transparency for the box layer (Default = 0.5)
#' @param violinColor Color for the violin layer (Default = "yellow")
#' @param violinFill Fill Color for the violin (Default = "yellow")
#' @param violinTransparency Transparency for the violin layer (Default = 0.5)
#' @param pointColor Color for the point layer (Default = "dodgerblue4")
#' @param pointFill Fill color for the point layer (Default = "dodgerblue4")
#' @param pointShape Shape for the point layer (Default = 21; fillable circle)
#' @param pointTransparency Transparency for the box layer (Default = 1)
#' @param boxNotch Turn on/off confidence interval notches on boxplots (Default = FALSE)
#' @param boxNotchWidth Set the width of box notches (0-1) (Default = 0.8)
#' @param pointSize Size of the points (Default = 4)
#' @param pointJitter Amount to jitter the points (Default = 0) Try 0.2 if you
#'   have a lot of points.
#' @param meanColor Color for the mean layer (Default = "goldenrod1")
#' @param meanOutlineColor Fill color for the mean layer (Default = "red2")
#' @param meanShape Shape for the mean layer (Default = 21; fillable circle)
#' @param meanTransparency Transparency for the mean layer (Default = 0.7)
#' @param meanSize Size of the mean points (Default = 3)
#' @param facet Specifies whether to facet (TRUE) or print individual plots
#'   (FALSE)  (Default = TRUE)
#' @param facetRow Explicitly set the number of rows for the facet plot.
#'   Default behavior will automatically set the columns. (Default = NULL)
#' @param xAngle Angle to set the sample labels on the X axis. (Default =  30; Range = 0-90)
#' @param scales Specify same scales or independent scales for each subplot (Default = "free_y";
#'   Allowed values: "fixed", "free_x", "free_y", "free")
#'
#' @return Plot of type canvasXpress or ggplot. If Facet = TRUE (default) returns a faceted object. If
#'   facet = FALSE, returns a list of objects indexed
#'   by observation (gene) names.
#'
#' @examples
#' \dontrun{
#'   # Simulate some data with row and colnames
#'   groups <- paste("group", factor(rep(1:4, each = 100)), sep = "")
#'   x <- matrix(rnorm(2400, mean = 10), ncol = length(groups))
#'   colnames(x) <- paste("sample", 1:ncol(x), sep = "")
#'   rownames(x) <- paste("gene", 1:nrow(x), sep="")
#'   # Reformat into tidy dataframe
#'   tidyInt <- tidyIntensity(x)
#'
#'  # Or get data from a DGEobj with RNA-Seq data
#'  log2cpm <- convertCounts(dgeObj$counts, unit = "cpm", log = TRUE, normalize = "tmm")
#'  tidyInt <- tidyIntensity(log2cpm,
#'                           rowidColname = "GeneID",
#'                           keyColname = "Sample",
#'                           valueColname = "Log2CPM",
#'                           group = dgeObj$design$ReplicateGroup)
#'
#'   # Faceted boxplot
#'   obsPlot(tidyInt,
#'            plotByCol = "GeneID",
#'            groupCol = "group",
#'            valueCol ="Log2CPM",
#'            pointJitter = 0.1,
#'            facetRow = 2)
#'
#'   # Faceted violin plot
#'   obsPlot(tidyInt,
#'            plotByCol = "GeneID",
#'            violinLayer = TRUE,
#'            boxLayer = FALSE,
#'            groupCol="group",
#'            valueCol = "Log2CPM",
#'            pointJitter = 0.1,
#'            facetRow = 2)
#'
#'   # Return a list of ggplots for each individual gene
#'   myplots <- obsPlot(tidyInt,
#'                       plotByCol="GeneID",
#'                       groupCol = "group",
#'                       valueCol ="Log2CPM",
#'                       pointJitter = 0.1,
#'                       facet = FALSE)
#'   # Plot one from the list
#'   myplots[[2]]
#' }
#'
#' @import ggplot2 magrittr
#' @importFrom dplyr left_join filter select
#' @importFrom assertthat assert_that
#' @importFrom canvasXpress canvasXpress
#'
#' @export
obsPlot <- function(data,
                    plotType = "canvasXpress",
                     plotByCol,
                     groupCol,
                     valueCol,
                     groupOrder = unique(as.character(data[,groupCol, drop = TRUE])),
                     boxLayer = TRUE,
                     violinLayer = FALSE,
                     pointLayer = TRUE,
                     meanLayer = TRUE,
                     xlab = groupCol,
                     ylab = valueCol,
                     title,
                     boxColor = "deepskyblue3",
                     boxTransparency = 0.5,
                     boxNotch = FALSE,
                     boxNotchWidth = 0.8,
                     violinColor = "goldenrod1",
                     violinTransparency = 0.5,
                     pointColor = "dodgerblue4",
                     pointShape = 21,
                     pointTransparency = 1,
                     pointSize = 2,
                     pointJitter = 0,
                     meanColor = "goldenrod1",
                     meanOutlineColor = "red2",
                     meanShape = 22,
                     meanTransparency = 0.7,
                     meanSize = 3,
                     facet = TRUE,
                     facetRow,
                     xAngle = 30,
                     scales = "free_y")
{
    #assert statements
    assertthat::assert_that(!missing(data),
                            "data.frame" %in% class(data),
                            msg = "data must be specified and should be of class 'data.frame'.")
    assertthat::assert_that(!is.null(plotType),
                            tolower(plotType) %in% c("canvasxpress", "ggplot"),
                            msg = "Plot type must be either canvasXpress or ggplot.")
    assertthat::assert_that(!is.null(plotByCol),
                            plotByCol %in% colnames(data),
                            msg = 'The plotByCol must be included in the colnames of the specified data.')
    assertthat::assert_that(!is.null(groupCol),
                            groupCol %in% colnames(data),
                            msg = "The groupCol must be included in the colnames of the specified data.")
    assertthat::assert_that(!is.null(valueCol),
                            valueCol %in% colnames(data),
                            msg = "The valueCol must be included in the colnames of the specified data.")
    #assertthat::assert_that(all(groupOrder %in% as.character(data[, ,groupCol, drop = TRUE])))

    #input validations
    plotType = tolower(plotType)

    if (any(is.null(facet), !is.logical(facet), length(facet) != 1)) {
        warning("facet must be a singular logical value. Assigning default value TRUE.")
        facet <- TRUE
    }


    if (facet) {

      if  (missing(facetRow)) {
          numcol <- data[plotByCol] %>% unique %>% length %>% sqrt %>% ceiling
      } else if (is.null(facetRow) || length(facetRow) != 1 || (!is.numeric(facetRow))) {
          numcol <- data[plotByCol] %>% unique %>% length %>% sqrt %>% ceiling
          warning(paste("facetRow needs a singular numeric value. Assigning default value",numcol))
      } else {
          numcol <- facetRow
    }
    }

    if (missing(title)) {
        title <- NULL
    } else if (!is.character(title) || length(title) != 1) {
        warning("Invalid title specificed. Title must be singular value of class character.")
        title <- NULL
    }

    if (!is.character(xlab) || length(xlab) != 1) {
        warning("xlab value specified is not valid. Assigning groupCol name as the default value.")
        xlab <- groupCol
    }

    if (!is.character(ylab) || length(ylab) != 1) {
        warning("ylab value specified is not valid. Assigning valueCol name as the default value.")
        ylab <- valueCol
    }

    #boxplot Validations

    if (any(is.null(boxLayer),length(boxLayer) != 1, !is.logical(boxLayer))) {
        warning("boxLayer must be a single logical value. Assigning default value 'TRUE'.")
        boxLayer = TRUE
    }

    if (boxLayer) {
        if (any(is.null(boxColor), length(boxColor) != 1, !is.character(boxColor))) {
            warning("boxColor must be of class character and must specify the name of the color or the rgb value. Assigning default value 'deepskyblue3'.")
            boxColor <- "deepskyblue3"
        } else if (.rgbaConversion(boxColor) == "invalid value") {
            warning("boxColor specified is not valid. Assigning default value 'deepskyblue3'.")
            boxColor <- "deepskyblue3"
        }

        if (any(is.null(boxTransparency),!is.numeric(boxTransparency), length(boxTransparency) != 1, boxTransparency <= 0, boxTransparency > 1)) {
            warning("boxTransparency must be a singular numeric value and must be between 0 and 1. Assigning default value 0.5.")
            boxTransparency <- 0.5
        }

        if (any(is.null(boxNotch), length(boxNotch) != 1, !is.logical(boxNotch))) {
            warning("boxNotch must be a single logical value. Assigning default value 'FALSE'.")
            boxNotch <- FALSE
        }
    }

    #Violin Validations
    if (any(is.null(violinLayer),length(violinLayer) != 1, !is.logical(violinLayer))) {
        warning("violinLayer must be a singular logical value. Assigning default value FALSE.")
        violinLayer <- FALSE
    }

    if (violinLayer) {
        if (any(is.null(violinColor), length(violinColor) != 1, !is.character(violinColor))) {
            warning("violinColor must be of class character and must specify the name of the color or the rgb value. Assigning default value 'goldenrod1'.")
            violinColor <- "goldenrod1"
        } else if (.rgbaConversion(violinColor) == "invalid value") {
            warning("violinColor specified is not valid. Assigning default value 'goldenrod1'.")
            violinColor <- "goldenrod1"
        }

        if (any(is.null(violinTransparency),!is.numeric(violinTransparency), length(violinTransparency) != 1, violinTransparency <= 0, violinTransparency > 1)) {
            warning("violinTransparency must be a singular numeric value and must be between 0 and 1. Assigning default value 0.5.")
            violinTransparency <- 0.5
        }
    }

    #Mean layer validations
    if (any(is.null(meanLayer),length(meanLayer) != 1, !is.logical(meanLayer))) {
        warning("meanLayer must be a singular logical value. Assigning default value TRUE.")
        meanLayer <- TRUE
    }

    if (meanLayer) {
        if (any(is.null(meanColor), length(meanColor) != 1, !is.character(meanColor))) {
            warning("meanColor must be of class character and must specify the name of the color or the rgb value. Assigning default value 'goldenrod1'.")
            meanColor <- "goldenrod1"
        } else if (.rgbaConversion(meanColor) == "invalid value") {
            warning("meanColor specified is not valid. Assigning default value 'goldenrod1'.")
            meanColor <- "goldenrod1"
        }

        if (any(is.null(meanOutlineColor), length(meanOutlineColor) != 1, !is.character(meanOutlineColor))) {
            warning("meanOutlineColor must be of class character and must specify the name of the color or the rgb value. Assigning default value 'goldenrod1'.")
            meanOutlineColor <- "red2"
        } else if (.rgbaConversion(meanOutlineColor) == "invalid value") {
            warning("meanOutlineColor specified is not valid. Assigning default value 'goldenrod1'.")
            meanOutlineColor <- "red2"
        }

        if (any(is.null(meanTransparency),!is.numeric(meanTransparency), length(meanTransparency) != 1, meanTransparency <= 0, meanTransparency > 1)) {
            warning("meanTransparency must be a singular numeric value and must be between 0 and 1. Assigning default value 0.7.")
            meanTransparency <- 0.7
        }

        if (plotType == "canvasxpress" && any(is.null(meanShape), length(meanShape) != 1, !.is_valid_symbolShapes_cxplot(meanShape))) {
            warning("meanShape specified is not valid. Assigning default value 'square'.")
            meanShape <- "square"
        }

        if (plotType == "ggplot" && any(is.null(meanShape), length(meanShape) != 1, !.is_valid_symbolShapes_ggplot(meanShape))) {
            warning("meanShape specified is not valid. Assigning default value '22'.")
            meanShape <- 22
        }

        if (any(is.null(meanSize), length(meanSize) != 1, !is.numeric(meanSize))) {
            warning("meanSize must be a singular numeric value. Assigning default value 3.")
            meanSize <- 3
        }
    }

    #point validations
    if (any(is.null(pointLayer),length(pointLayer) != 1, !is.logical(pointLayer))) {
        warning("pointLayer must be a singular logical value")
    }

    if (pointLayer) {
        if (any(is.null(pointColor), length(pointColor) != 1, !is.character(pointColor))) {
            warning("pointColor must be of class character and must specify the name of the color or the rgb value. Assigning default value 'dodgerblue4'.")
            pointColor <- "dodgerblue4"
        } else if (.rgbaConversion(pointColor) == "invalid value") {
            warning("pointColor specified is not valid. Assigning default value 'dodgerblue4'.")
            pointColor <- "dodgerblue4"
        }

        if (any(is.null(pointTransparency),!is.numeric(pointTransparency), length(pointTransparency) != 1, pointTransparency <= 0, pointTransparency > 1)) {
            warning("pointTransparency must be a singular numeric value and must be between 0 and 1. Assigning default value 1.")
            pointTransparency <- 1
        }

        if (plotType == "canvasXpress" && any(is.null(pointShape), length(pointShape) != 1, !.is_valid_symbolShapes_cxplot(pointShape))) {
            warning("pointShape specified is not valid. Assigning default value 'sphere'.")
            pointShape <- "sphere"
        }

        if (plotType == "ggplot" && any(is.null(pointShape), length(pointShape) != 1, !.is_valid_symbolShapes_ggplot(pointShape))) {
            warning("pointShape specified is not valid. Assigning default value '21'.")
            pointShape <- 21
        }

        if (any(is.null(pointSize), length(pointSize) != 1, !is.numeric(pointSize))) {
            warning("pointSize must be a singular numeric value. Assigning default value 2.")
            pointSize <- 2
        }
    }

    obsPlot <- NULL
    if (plotType == "canvasxpress") {

        if (facet == TRUE) {
            cx_data <- data %>% select(Log2CPM) %>% t() %>% as.data.frame()
            smp_data <- data %>% select(-Log2CPM)
            rownames(smp_data) <- colnames(cx_data)

            obsPlot <- canvasXpress(data = cx_data,
                                    smpAnnot = smp_data,
                                    graphOrientation = "vertical",
                                    graphType = "Boxplot",
                                    groupingFactors = groupCol,
                                    boxplotColor = boxColor,
                                    boxplotDataPointTransparency = boxTransparency,
                                    boxplotMean = meanLayer,
                                    boxplotWhiskersType = "single",
                                    showViolinBoxplot = violinLayer,
                                    showBoxplotIfViolin = boxLayer,
                                    violinColor = violinColor,
                                    violinTransparency = violinTransparency,
                                    smpLabelRotate = xAngle,
                                    smpLabelScaleFontFactor = 1,
                                    smpTitle = xlab,
                                    smpTitleFontStyle = "bold",
                                    smpTitleScaleFontFactor = 1,
                                    layoutAdjust = TRUE,
                                    showBoxplotOriginalData = pointLayer,
                                    theme = "CanvasXpress",
                                    xAxisTitle = "Log2CPM",
                                    xAxis2Show = ylab,
                                    showLegend = FALSE,
                                    segregateSamplesBy = plotByCol)

        } else {
            plotlist <- list()
            plotby_vec <- unique(data[[plotByCol]])
            obsPlot <- lapply(plotby_vec, function(x){
                data_subset <- data %>% filter(!!rlang::sym(plotByCol) == x)
                cx_data <- data_subset %>%
                    select(Log2CPM) %>%
                    t() %>%
                    as.data.frame()
                smp_data <- data_subset %>% select(-Log2CPM)
                rownames(smp_data) <- colnames(cx_data)

                canvasXpress(data = cx_data,
                             smpAnnot = smp_data,
                             graphOrientation = "vertical",
                             graphType = "Boxplot",
                             groupingFactors = groupCol,
                             boxplotColor = boxColor,
                             boxplotDataPointTransparency = boxTransparency,
                             boxplotMean = meanLayer,
                             boxplotWhiskersType = "single",
                             showViolinBoxplot = violinLayer,
                             showBoxplotIfViolin = boxLayer,
                             violinColor = violinColor,
                             violinTransparency = violinTransparency,
                             smpLabelRotate = xAngle,
                             smpLabelScaleFontFactor = 1,
                             smpTitle = xlab,
                             smpTitleFontStyle = "bold",
                             smpTitleScaleFontFactor = 1,
                             layoutAdjust = TRUE,
                             showBoxplotOriginalData = pointLayer,
                             theme = "CanvasXpress",
                             xAxisTitle = "Log2CPM",
                             xAxis2Show = ylab,
                             showLegend = FALSE)
            })

        }
    } else {

        .addGeoms <- function(obsPlot) {
            if (boxLayer == TRUE) {
                obsPlot <- obsPlot + geom_boxplot(alpha = boxTransparency,
                                                  color = boxColor,
                                                  fill = boxColor,
                                                  notch = boxNotch,
                                                  notchwidth = boxNotchWidth,
                                                  outlier.shape = outlier.shape,
                                                  outlier.size = outlier.size)
            }

            if (violinLayer == TRUE) {
                obsPlot <- obsPlot + geom_violin(alpha = violinTransparency,
                                                 color = violinColor,
                                                 fill = violinColor)
            }

            if (pointLayer == TRUE) {
                if (pointJitter > 0) {
                    obsPlot <- obsPlot + geom_point(position = position_jitter(width = pointJitter),
                                                    alpha = pointTransparency,
                                                    color = pointColor,
                                                    fill = pointColor,
                                                    size = pointSize,
                                                    shape = pointShape)
                } else {
                    obsPlot <- obsPlot + geom_point(alpha = pointTransparency,
                                                    color = pointColor,
                                                    fill = pointColor,
                                                    size = pointSize,
                                                    shape = pointShape)
                }
            }

            if (meanLayer == TRUE) {
                obsPlot <- obsPlot +
                    stat_summary(fun.y = mean,
                                 geom = "point",
                                 shape = meanShape,
                                 size = meanSize,
                                 color = meanOutlineColor,
                                 fill = meanColor,
                                 alpha = meanTransparency)
            }

            return(obsPlot)
        }


        # Reduce box outliers to a dot if geom_points turned on
        outlier.size <- 1.5
        outlier.shape <- 19
        if (pointLayer) {
            outlier.size <- 1
            outlier.shape <- "."
        }

        # Plot code here
        if (facet == TRUE) {
            # Set facet columns to sqrt of unique observations (rounded up)
            if (missing(facetRow)) {
                numcol <- data[plotByCol] %>% unique %>% length %>% sqrt %>% ceiling
            } else {
                numcol <- facetRow
            }

            obsPlot <- ggplot2::ggplot(data, aes_string(x = groupCol, y = valueCol))
            obsPlot <- .addGeoms(obsPlot)
            facetFormula <- stringr::str_c("~", plotByCol, sep = " ")
            obsPlot <- obsPlot + ggplot2::facet_wrap(facetFormula, nrow = numcol, scales = scales)

            obsPlot <- obsPlot + ggplot2::xlab(xlab)
            obsPlot <- obsPlot + ggplot2::ylab(ylab)
            if (!missing(title)) {
                obsPlot <- obsPlot + ggplot2::ggtitle(title)
            }

            # Rotate xaxis group labels
            if (xAngle > 0) {
                obsPlot <- obsPlot + theme(axis.text.x = element_text(angle = xAngle, hjust = 1))
            }

        } else {
            plotlist <- list()

            for (obs in unique(data[[plotByCol]])) {
                dat <- data[data[[plotByCol]] == obs, ]
                aplot <- ggplot(dat, aes_string(x = groupCol, y = valueCol)) + # Samples vs Log2CPM
                    xlab(xlab) +
                    ylab(ylab) +
                    ggtitle(obs) +
                    theme_grey(baseFontSize)
                aplot <- .addGeoms(aplot)

                # Rotate xaxis group labels
                if (xAngle > 0) {
                    aplot <- aplot + theme(axis.text.x = element_text(angle = xAngle, hjust = 1))
                }
                plotlist[[obs]] <- aplot
            }

            obsPlot <- plotlist
        }



    }

    return(obsPlot)
}


