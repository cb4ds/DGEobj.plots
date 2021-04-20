#' Create deluxe CDF Plots
#'
#' CDF plots are a good complement to p-value histograms as a way to evaluate
#' model performance and examine support for differential expression. Results
#' are ranked by p-value on the x-axis and the p-value plotted on the y-axis.
#' Since p-value distributions should be flat, this type of plot should produce a
#' straight line.  Any observations that fail to meet the null hypothesis will
#' appear as a break in the line at the low end of the curve.
#'
#' This function is designed to take topTable dataframes and display the
#' corresponding CDF plot. Data for the p-values below 0.1 (configurable via
#' pvalMax argument) are show in a full size plot. An inset figure shows the
#' whole p-value scale and highlights the portion shown in the full plot. Points
#' below 0.01 are a different color by default (threshold set by pThreshold
#' argument; shape/color attributes customizable through other arguments).
#'
#' The output can be controlled using printPlot = TRUE, which outputs the compound plot
#' to the console/knitr.
#'
#' \strong{Data Structure for the input dataframe:}
#'
#' The defaults are set for dataframes produced by topTable.  The column
#' "P.Value" is used by default to accommodate the column names used in topTable
#' dataframes.  Any other dataframe can be used with by explicitly defining the
#' p-value column name with the appropriate argument.
#'
#' Sensible defaults are chosen for symbols (Size, Shape, Color, and Fill).
#' There are optional arguments that allow these to be adjusted. A length of 2
#' is required for these arguments which applies the attributes in
#' this order: Significant, Not Significant.
#'
#' @param contrastDF A dataframe with LogRatio and LogIntensity columns and optionally a p-value or FDR column.
#' @param plotType Plot type must be canvasXpress or ggplot (Default to canvasXpress).
#' @param pvalCol Name of the p-value or FDR column (Default = "P.Value")
#' @param pvalMax Limit the range of the main plot (Default = 0.10)
#' @param pThreshold Used to color points (default = 0.01)
#' @param xlab X axis label (default is "LogIntensity column name "Rank")
#' @param ylab Y axis label (default is p-value column name)
#' @param title Plot title (Optional)
#' @param insetTitle Title for the inset plot (Optional)
#' @param symbolSize Size of symbols for up, no change, and down. Default = c(4, 1, 4);
#'        Note: All three cannot be the same size. Decimal values are acceptable to help offset that
#'        (e.g. 4, 4.1, 4.2).
#' @param symbolShape Shape of the symbols for up, no change, and down; Default = c(21, 20, 21)
#'        (20 = filled circle, 21 = fillable open circle); Note: The same symbol shape cannot
#'        be selected for all three symbols.
#'        See \url{http://www.cookbook-r.com/Graphs/Shapes_and_line_types}
#' @param symbolColor c(Up, NoChange, Down); default = c("black", "grey25", "grey0")
#'        See \url{http://research.stowers-institute.org/efg/R/Color/Chart}
#'        Note: Colors cannot be duplicated.
#' @param symbolFill Set the fill color for the symbols. Note only symbols 21-25 are fillable.
#'        This will have no effect on other symbols.
#'        Default = c("red3", "grey25", "deepskyblue4")
#'        Note: Colors cannot be duplicated.
#' @param transparency Controls the transparency of the plotted points (0-1; Default =
#'   0.5)
#' @param referenceLine Color for an horizontal line drawn at the p-threshold
#'   (Default = NULL; NULL disables, set to desired color to enable)
#' @param refLineThickness Set thickness of the reference line (Default = 1)
#' @param legendPosition (Default = "right")
#' @param viewportX (Default = 0.15)
#' @param viewportY (Default = 0.85)
#' @param viewportWidth (Default = 0.35)
#' @param printPlot Specify printing the combined plot to the console/knitr
#'   (Default = TRUE)
#' @param footnote Optional string placed right justified at bottom of plot.
#' @param footnoteSize Applies to footnote. (Default = 3)
#' @param footnoteColor Applies to footnote. (Default = "black")
#' @param footnoteJust Value 0 - 1. 0 is left justified, 1 is right justified, 0.5 is centered. (Default = 1)
#'
#' @return A list containing main plot, inset plot for both plotType. For plotType ='ggplot' list contains viewport, The plot can be
#'    reconstructed with print(cdfPlot$main); print(inset, vp = cdfPlot$viewport)
#'
#' @examples
#' \dontrun{
#'    # Plot to console (contrastDF is a topTable dataframe)
#'    cdfPlot(contrastDF, title = "My CDF Plot")
#' }
#' @import ggplot2 magrittr
#' @importFrom grid viewport
#' @importFrom dplyr arrange left_join
#' @importFrom assertthat assert_that
#' @importFrom canvasXpress canvasXpress
#'
#' @export
cdfPlot <- function(contrastDF,
                    plotType = "canvasXpress",
                    pvalCol = "P.Value",
                    pThreshold = 0.01,
                    xlab,
                    ylab,
                    title = NULL,
                    insetTitle = NULL,
                    symbolSize = c(2, 1),
                    symbolShape = c("circle", "circle"),
                    symbolColor = c("red3", "deepskyblue4"),
                    transparency = 0.7,
                    referenceLine = NULL,
                    refLineThickness = 3,
                    legendPosition = "right",
                    viewportX = 0.15,
                    viewportY = 0.85,
                    viewportWidth = 0.35,
                    pvalMax = 0.10,
                    printPlot = TRUE,
                    footnote,
                    footnoteSize = 3,
                    footnoteColor = "black",
                    footnoteJust = 1) {

    assertthat::assert_that(!missing(contrastDF),
                            !is.null(contrastDF),
                            "data.frame" %in% class(contrastDF),
                            nrow(contrastDF) > 0,
                            msg = "contrastDF must be specified as dataframe with a p-value column.")

    assertthat::assert_that(!is.null(plotType),
                            is.character(plotType),
                            length(plotType) == 1,
                            tolower(plotType) %in% c("canvasxpress", "ggplot"),
                            msg = "plotType must be either canvasXpress or ggplot.")

    assertthat::assert_that(!is.null(pvalCol),
                            pvalCol %in% colnames(contrastDF),
                            msg = "pvalCol column not found in contrastDF.")

    plotType <- tolower(plotType)
    if (any(is.null(pThreshold),
            !is.numeric(pThreshold),
            length(pThreshold) != 1)) {
        warning("pthreshold must be a singular numeric value. Assigning default value 0.01.")
        pThreshold <- 0.01
    }

    if (!is.null(title) &&
        !all(is.character(title), length(title) == 1)) {
        warning("title must be a singular value of class character. Assigning default value 'NULL'.")
        title <- NULL
    }

    if (!is.null(insetTitle) &&
        !all(is.character(insetTitle), length(insetTitle) == 1)) {
        warning("insetTitle must be a singular value of class character. Assigning default value 'NULL'.")
        insetTitle <- NULL
    }

    if (missing(xlab)) {
        xlab <- 'Rank'
    } else {
        if (!is.null(xlab) &&
             !all(is.character(xlab), length(xlab) == 1)) {
            warning("xlab must be a singular value of class character. Assigning default value 'Rank' as the label.")
            xlab <- 'Rank'
        }
    }

    if (missing(ylab)) {
        ylab <- pvalCol
    } else {
        if (!is.null(ylab) &&
            !all(is.character(ylab), length(ylab) == 1)) {
            warning("ylab must be a singular value of class character. Assigning default value pvalCol as the lavel")
            ylab <- pvalCol
        }
    }

    if (any(is.null(symbolSize),
            !is.numeric(symbolSize),
            length(symbolSize)  != 2,
            !all(symbolSize >= 0))) {
        warning("symbolSize must be a vector of 2 integer values. Assigning default values 2,1.")
        symbolSize  <-  c(2,1)

    }

    if (any(is.null(symbolShape),
            !is.character(symbolShape),
            length(symbolShape)  != 2,
            plotType == "canvasxpress" && !is.null(symbolShape) && length(.validate_cx_shapes(symbolShape)) != 2,
            plotType == "ggplot" && !is.null(symbolShape) && length(.validate_gg_shapes(symbolShape)) != 2)) {
        warning("symbolShape must be a vector of 2 charcter values. Assigning default values 'circle'.")
        symbolShape  <- c("circle", "circle")

    }

    if (any(is.null(symbolColor),
            !is.character(symbolColor),
            length(symbolColor)  != 2,
            length(.validate_colors(symbolColor)) != 2)) {
        warning("symbolColor must be a vector of 2 character values. Assigning default values 'red3', 'deepskyblue4'.")
        symbolColor <- c("red3", "deepskyblue4")
    }

    if (any(is.null(transparency),
            !is.numeric(transparency),
            length(transparency) != 1,
            transparency <= 0,
            transparency > 1)) {
        warning("transparency must be a singular value of class numeric and must be between 0 and 1. Assigning default value '0.7'.")
        transparency <- 0.7
    }

    if (!is.null(referenceLine) &&
        !all(is.character(referenceLine), length(referenceLine) == 1)) {
        warning("referenceLine must be a singular value of class character or 'NULL' to disable. Assigning default value NULL.")
        referenceLine <- NULL
    } else if (.rgbaConversion(referenceLine) == "invalid value") {
        warning("Color specified is not valid. Assigning default value NULL.")
        referenceLine <- NULL
    }

    if (any(is.null(refLineThickness),
            !is.numeric(refLineThickness),
            length(refLineThickness) != 1,
            refLineThickness < 0)) {
        warning("refLineThickness must be a singular value of class numeric Assigning default value '1'.")
        refLineThickness <- 1
    }

    if (!is.null(legendPosition) &&
        !all(is.character(legendPosition),
             length(legendPosition) == 1,
             legendPosition %in% c("top", "bottom", "left", "right", "ne", "se", "nw", "sw"))) {
        warning("legendPosition must be one value from 'top', 'bottom', 'left', 'right', 'ne', 'se', 'nw', 'sw' or 'NULL' to disable. Assigning default value 'right'.")
        legendPosition <- "right"
    }

    if (!is.null(viewportX) &&
        any(!is.numeric(viewportX),
            length(viewportX) != 1)) {
        warning("viewportX must be a singular value of class numeric and must be greater than 0. Assigning default value '0.15'.")
        viewportX <- 0.15
    }

    if (!is.null(viewportY) &&
       any(!is.numeric(viewportY),
           length(viewportY) != 1)) {
        warning("viewportY must be a singular value of class numeric and must be greater than 0. Assigning default value '0.15'.")
        viewportY <- 3
    }

    if (!is.null(viewportWidth) &&
       any(!is.numeric(viewportWidth),
           length(viewportWidth) != 1,
           viewportWidth < 0)) {
        warning("viewportWidth must be a singular value of class numeric. Assigning default value '0.15'.")
        viewportWidth <- 3
    }

    if (any(is.null(pvalMax),
            !is.numeric(pvalMax),
            length(pvalMax) != 1)) {
        warning("pvalMax must be a singular numeric value. Assigning default value 0.1")
        pvalMax <- 0.1
    }

    if (any(is.null(printPlot),
            !is.logical(printPlot),
            length(printPlot) != 1)) {
        warning("printPlot must be a singular logical value. Assigning default value TRUE")
        printPlot <- TRUE
    }


    if (missing(footnote)) {
        footnote <- NULL
    } else if (!is.null(footnote) &&
               !all(is.character(footnote), length(footnote) == 1)) {
        warning("footnote must be a singular value of class character or 'NULL' to disable. Assigning default value 'NULL'.")
        footnote <- NULL
    }

    if (!is.null(footnote) &&
        !is.null(footnoteSize) &&
        any(!is.numeric(footnoteSize),
            length(footnoteSize) != 1,
            footnoteSize < 0)) {
        warning("footnoteSize must be a singular value of class numeric. Assigning default value '3'.")
        footnoteSize <- 3
    }

    if (!is.null(footnote) &&
        !is.null(footnoteColor) &&
        any(!is.character(footnoteColor),
            length(footnoteColor) != 1)) {
        warning("footnoteColor must be a singular value of class character. Assigning default value 'black'.")
        footnoteColor <- "black"
    } else if (.rgbaConversion(footnoteColor) == "invalid value") {
        warning("Color specified is not valid. Assigning default value 'black'.")
        footnoteColor <- "black"
    }


    groupNames <- c("Significant", "Not Significant")
    # Storing column names in x and y variable
    x <- "Rank"
    y <- pvalCol

    # if (is.null(xlab)) {
    #     xlab <- x
    # }
    #
    # if (is.null(ylab)) {
    #     ylab <- y
    # }

    if (is.null(title)) {
        title = ""
    }

    if (is.null(insetTitle)) {
        insetTitle = ""
    }

    # Combo PLOT: full data inset, most significant data in main plot
    # Rank by p-value
    contrastDF <- contrastDF %>%
        dplyr::arrange(!!sym(pvalCol))
    contrastDF$Rank <- c(1:nrow(contrastDF))

    # # Let's plot the p-value subsets
    #contrastDF_1 <- contrastDF
    # Sig <- contrastDF_1[[pvalCol]] <= pThreshold
    # NotSig <- contrastDF_1[[pvalCol]] > pThreshold
    #
    # # Create group factor column in contrastDF
    # contrastDF_1$group <- NA
    # contrastD_1F$group[Sig] <- "Significant"
    # contrastDF_1$group[NotSig] <- "Not Significant"

    contrastDF$group <- NA
    contrastDF$order <- NA
    contrastDF <- contrastDF %>%
        dplyr::mutate(group = dplyr::case_when(!!rlang::sym(pvalCol) <= pThreshold ~ "Significant",
                                               !!rlang::sym(pvalCol) > pThreshold ~ "Not Significant"),
                      group = factor(group,
                                     levels = c("Significant", "Not Significant")),
                      order = dplyr::case_when(group == "Not Significant" ~ 1,
                                               group == "Significant" ~ 2))

    # contrastDF$group <- contrastDF$group %>%
    #     factor(levels = groupNames)
    #
    # contrastDF <-
    #
    # # Set an order field to force plotting of 'not significant' first
    # contrastDF$order <- NA
    # contrastDF$order[NotSig] <- 1
    # contrastDF$order[Sig] <- 2

    # Rows to include in the zoomed in plot
    # subsetRows <- sum(contrastDF[[y]] <= pvalMax)
    # contrastDFsubset <- contrastDF[1:subsetRows,]

    contrastDF_subset <- contrastDF %>% filter(!!rlang::sym(pvalCol) <= pvalMax)
    cdfMain <- NULL
    cdfInset <- NULL

    if (plotType == "canvasxpress") {
        ## Create the canvasXpress cx.data and var.annot
        # Main plot
        # cx.data <- data.frame(a = contrastDF[colnames(contrastDF) == x],
        #                       b = contrastDF[colnames(contrastDF) == y])

        cx.data <- contrastDF %>% dplyr::select(!!x,!!y)
        colnames(cx.data) <- c(x, y)
        #var.annot <- data.frame(Group = contrastDF$group)
        var.annot <- contrastDF %>% dplyr::select(group)

        #rownames(var.annot) <- rownames(cx.data)

        # Inset plot
        # cx.data.subset <- data.frame(a = contrastDFsubset[colnames(contrastDFsubset) == x],
        #                              b = contrastDFsubset[colnames(contrastDFsubset) == y])
        cx.data.subset <- contrastDF_subset %>% dplyr::select(!!x,!!y)
        colnames(cx.data.subset) <- c(x, y)
        #var.annot.subset <- data.frame(Group = contrastDFsubset$group)
        #rownames(var.annot.subset) <- rownames(cx.data.subset)

        var.annot.subset <- contrastDF_subset %>% dplyr::select(group)

        decorations <- list()
        if (!is.null(referenceLine)) {
            referenceLine <- .rgbaConversion(referenceLine, alpha = transparency)
            decorations <- .getCxPlotDecorations(decorations = decorations,
                                                color = referenceLine,
                                                width = refLineThickness,
                                                y     = pThreshold)
        }

        # Footnote
        # if (missing(footnote)) {
        #     footnote <- NULL
        # }
        max.value <- max(pThreshold,max(contrastDF_subset[[y]]))
        maxY <- max.value + max.value*0.1

        cdfMain <- canvasXpress::canvasXpress(data              = cx.data.subset,
                                              varAnnot          = var.annot.subset,
                                              decorations       = decorations,
                                              graphType         = "Scatter2D",
                                              colorBy           = "group",
                                              colors            = symbolColor,
                                              title             = title,
                                              xAxisTitle        = xlab,
                                              yAxisTitle        = ylab,
                                              citation          = footnote,
                                              citationFontSize  = footnoteSize,
                                              citationColor     = footnoteColor,
                                              setMaxY           = maxY,
                                              lazyLoad = TRUE)

        cdfInset <- canvasXpress::canvasXpress(data             = cx.data,
                                               varAnnot         = var.annot,
                                               graphType        = "Scatter2D",
                                               colorBy          = "group",
                                               colors           = symbolColor,
                                               title            = insetTitle,
                                               xAxisTitle       = xlab,
                                               yAxisTitle       = ylab,
                                               setMaxY          = max(contrastDF[[y]]),
                                               lazyLoad = TRUE)
        cdfPlot <- list("main" = cdfMain, "inset" = cdfInset)
    } else {
        names(symbolShape) <- groupNames
        names(symbolSize)  <- groupNames
        names(symbolColor) <- groupNames

        ssc <- data.frame(group = factor(groupNames, levels = groupNames),
                          symbolShape = symbolShape,
                          symbolSize = symbolSize,
                          symbolColor = symbolColor,
                          stringsAsFactors = FALSE)

        contrastDF <- contrastDF %>%
            dplyr::left_join(ssc)

        # Plot subset percent of the data for the main plot
        cdfMain <- ggplot(contrastDF_subset, aes_string(x = x, y = y)) +
            aes(shape = group, size = group, color = group, fill = group) +
            # Scale lines tell it to use the actual values, not treat them as factors
            scale_shape_manual(name = "Group", guide = "legend", labels = ssc$group, values = ssc$symbolShape) +
            scale_size_manual(name = "Group", guide = "legend", labels = ssc$group, values = ssc$symbolSize) +
            scale_color_manual(name = "Group", guide = "legend", labels = ssc$group, values = ssc$symbolColor) +
            scale_fill_manual(name = "Group", guide = "legend", labels = ssc$group, values = ssc$symbolColor) +
            geom_point(alpha = transparency)

        # Optional Decorations
        if (!is.null(referenceLine)) {
            cdfMain <- cdfMain +
                geom_hline(yintercept = pThreshold, color = referenceLine,
                           size = refLineThickness, alpha = 0.5)
        }

        # Add Labels
        cdfMain <- cdfMain +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(title)

        cdfMain <- setLegendPosition(cdfMain, legendPosition)

        if (!missing(footnote)) {
            cdfMain <- addFootnote(cdfMain,
                                   footnoteText = footnote,
                                   footnoteSize = footnoteSize,
                                   footnoteColor = footnoteColor,
                                   footnoteJust = footnoteJust)
        }

        # Set up the inset plot with All Data
        cdfInset <- ggplot(contrastDF, aes_string(x = x, y = y)) +
            aes(shape = group, size = group,
                color = group, fill = group) +
            # Scale lines tell it to use the actual values, not treat them as factors
            scale_shape_manual(name = "Group", guide = "none", labels = ssc$group,
                               values = ssc$symbolShape) +
            scale_size_manual(name = "Group", guide = "none", labels = ssc$group,
                              values = ssc$symbolSize) +
            scale_color_manual(name = "Group", guide = "none", labels = ssc$group,
                               values = ssc$symbolColor) +
            scale_fill_manual(name = "Group", guide = "none", labels = ssc$group,
                              values = ssc$symbolColor) +
            geom_rect(xmin = 0, xmax = nrow(contrastDF),
                      ymin = 0, ymax = max(contrastDF[[y]]), color = "lightblue",
                      fill = "lightblue", alpha = 0.2) +
            geom_point(alpha = transparency)

        # Add Labels and title
        cdfInset <- cdfInset +
            xlab(xlab) +
            ylab(ylab) +
            ggtitle(insetTitle)
    }

        # A viewport taking up a fraction of the plot area (upper left)
        vp <- grid::viewport(width  = viewportWidth,
                             height = viewportWidth,
                             x      = viewportX,
                             y      = viewportY,
                             just   = c("left", "top"))

        if (printPlot) {
            print(cdfMain)
            print(cdfInset, vp = vp)
        }

        cdfPlot <- list(main = cdfMain, inset = cdfInset, viewport = vp)

    return(cdfPlot)
}
