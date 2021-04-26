#' Function obsPlot
#'
#' Provides a summary plot (box/violin/points) for each observation (gene), showing data for each
#' experiment group. The plot can optionally include either one or both of the boxplot and violinplot layer
#' can additionally choose to display points and/or mean of all the points on the plot.  The boxLayer and the
#' violin layer can be customized to have the desired transparency, colors etc. By default, the violinLayer is
#' not displayed and only the boxplot, points and the mean on the plots can be see.
#'
#' Also, by default, the plots are faceted.  Faceting the plot can be turned off to return a list of individual
#' plots for each gene.
#'
#' Input is a dataframe or matrix of intensity values with geneID row names and sample name column names,
#' e.g. from log2cpm. Group must also be specified as the plots are grouped by this attribute.
#'
#' @param data A  DGEObject. The countsMatrix in the DGEObject is extracted to plot the data. (required)
#' @param group Define the column name to group boxplots by (typically a replicate group column) (required)
#' @param boxLayer Adds a boxplot layer (default = TRUE)
#' @param violinLayer Adds a violin layer (default = FALSE)
#' @param showPoints Shows the datapoints on the plot only if boxLayer or violinLayer is enabled. (default = TRUE)
#' @param showMean Show the mean of the points on the plot only if boxLayer is enabled. (default = TRUE)
#' @param xlab X axis label (defaults to group column name if not specified)
#' @param ylab Y axis label (defaults to value column name if not specified)
#' @param title Plot title (optional)
#' @param boxColor Color for the boxplot layer (default = 'deepskyblue3')
#' @param boxTransparency Transparency for the box layer (default = 0.5)
#' @param violinColor Color for the violin layer (default = 'yellow')
#' @param violinTransparency Transparency for the violin layer (default = 0.5)
#' @param facet Specifies whether to facet (TRUE) or print individual plots
#'   (FALSE)  (default = TRUE)
#' @param facetRow Explicitly set the number of rows for the facet plot.
#'   Default behavior will automatically set the columns.
#' @param labelAngle Angle to set the sample labels on the X axis. (default =  30; range = 0-90)
#' @param axisFree Specify same scale or independent scales for each subplot (default = TRUE;
#'   Allowed values: TRUE and FALSE)
#'
#' @return Plot of type canvasXpress or ggplot. If Facet = TRUE (default) returns a faceted object. If
#'   facet = FALSE, returns a list of objects indexed
#'   by observation (gene) names.
#'
#' @examples
#' \dontrun{
#'
#'   # Faceted boxplot
#'   obsPlot(DGEobj,
#'           group = 'replicategroup')
#'
#'   # Faceted violin plot
#'   obsPlot(DGEobj,
#'            violinLayer = TRUE,
#'            boxLayer = FALSE,
#'            group = 'replicategroup')
#'
#'   # Return a list of plot for each individual gene
#'   myplots <- obsPlot(DGEobj,
#'                      group = 'replicategroup'
#'                      facet = FALSE)
#'   # Plot one from the list
#'   myplots[[2]]
#' }
#'
#' @import ggplot2 magrittr
#' @importFrom tidyr gather
#' @importFrom dplyr left_join filter select
#' @importFrom assertthat assert_that
#' @importFrom canvasXpress canvasXpress
#'
#' @export
obsPlot <- function(DGEdata,
                    plotType    = 'canvasXpress',
                    group = 'replicategroup',
                    boxLayer    = TRUE,
                    violinLayer = FALSE,
                    showPoints  = TRUE,
                    showMean   = TRUE,
                    xlab,
                    ylab,
                    title,
                    boxColor    = 'deepskyblue3',
                    boxTransparency = 0.5,
                    violinColor = 'goldenrod1',
                    violinTransparency = 0.5,
                    facet       = TRUE,
                    facetRow,
                    labelAngle      = 30,
                    axisFree      = TRUE) {
    assertthat::assert_that(!missing(DGEdata),
                            !is.null(DGEdata),
                            'DGEobj' %in% class(DGEdata),
                            msg = 'DGEdata must be specified and should be of class DGEobj')

    assertthat::assert_that('counts' %in% names(DGEdata),
                            msg = 'counts matrix must be available in DGEdata to plot the data.')
    data   <- convertCounts(DGEdata$counts, unit = 'cpm') %>%
        as.data.frame()

    assertthat::assert_that('design' %in% names(DGEdata),
                             msg = 'DGEdata needs to have a design item.')

    design            <- DGEobj::getItem(DGEdata, 'design')
    colnames(design)  <- tolower(colnames(design))
    group_default     <- NULL
    if (tolower('ReplicateGroup') %in% colnames(design)) {
        group_default <- 'replicategroup'
    }

    assertthat::assert_that(!is.null(plotType),
                            tolower(plotType) %in% c('canvasxpress', 'ggplot'),
                            msg = 'Plot type must be either canvasXpress or ggplot.')

    if (any(is.null(group),
            !(tolower(group) %in% colnames(design)))) {
        warning('group must be specified and should be one of the columns in the design object in DGEdata.')
        if (!is.null(group_default)) {
            warning(paste('Assigning',
                          group_default,
                          'as the default value.'))
            group <- 'replicategroup'
        } else {
            stop('Unable to find default values in the design item.')
        }
    }

    # Create a rownames column
    data$GeneID = rownames(data);rownames(data) <- NULL
    data <- tidyr::gather(data, key = 'Sample', value = 'value',-GeneID)

    group      <- tolower(group)
    group.data <- design %>% dplyr::select(!!group)
    colnames(group.data) <- 'group'
    group.data$Sample    <- rownames(group.data);rownames(group.data) <- NULL

    data <- merge(x     = data,
                  y     = group.data,
                  by    = 'Sample',
                  all.x = TRUE,
                  sort  = FALSE)
    data <- data[, c('GeneID', 'Sample', 'value', 'group')]

    plotByCol <-  'GeneID'
    valueCol  <- 'value'
    groupCol  <-  'group'


    #input validations
    plotType <- tolower(plotType)
    facet_chart_limit <- 40

    if (any(is.null(facet),
            !is.logical(facet),
            length(facet) != 1)) {
        warning('facet must be a singular logical value. Assigning default value TRUE.')
        facet <- TRUE
    }

    unique_rownames <- rownames(data) %>% unique
    if (facet) {
        #Number of charts to plot
        if (nrow(unique(data[plotByCol])) > facet_chart_limit) {
            warning(paste('A large number of charts/facets has/have been requested and may take significant time to generate.  It is suggested that less than',
                          facet_chart_limit,
                          'charts/facets are requested at a time'))
        }

        #facetRow
        if (missing(facetRow)) {
            numrow <- data[plotByCol] %>%
                unique %>%
                length %>%
                sqrt %>%
                ceiling
        } else if (is.null(facetRow) ||
                   length(facetRow) != 1 ||
                   (!is.numeric(facetRow)) ||
                   facetRow > length(unique_rownames)) {
            numrow <- data[plotByCol] %>%
                unique %>%
                length %>%
                sqrt %>%
                ceiling
            warning(
                paste('facetRow needs a singular numeric value lesser than the total number of unique column by which the plot is segregated.',
                      'Assigning default value.'
                )
            )
        } else {
            numrow <- facetRow
        }
    }

    if (missing(title)) {
        title <- NULL
    } else if ((!is.null(title)) &&
               (!is.character(title) || length(title) != 1)) {
        warning('Invalid title specificed. Title must be singular value of class character.')
        title <- NULL
    }

    if ((!missing(xlab)) &&
        (!is.null(xlab)) &&
        ((!is.character(xlab)) || length(xlab) != 1)) {
        warning('xlab value specified is not valid. Assigning groupCol name as the default value.')
        xlab <- groupCol
    } else if (missing(xlab)) {
        xlab <- groupCol
    }

    if (!missing(ylab) &&
        !is.null(ylab) &&
        (!is.character(ylab) || length(ylab) != 1)) {
        warning('ylab value specified is not valid. Assigning valueCol name as the default value.')
        ylab <- valueCol
    } else if (missing(ylab)) {
        ylab <- valueCol
    }

    if (!missing(labelAngle) &&
        any(!is.numeric(labelAngle),
            length(labelAngle) != 1,
            labelAngle <= 0)) {
        warning('labelAngle must be a single numeric value greater than 0. Assigning default value 30.')
        labelAngle <- 30
    }

    #boxplot Validations
    if (any(is.null(boxLayer),
            length(boxLayer) != 1,
            !is.logical(boxLayer))) {
        warning('boxLayer must be a single logical value. Assigning default value TRUE.')
        boxLayer = TRUE
    }

    if (boxLayer) {
        if (any(is.null(boxColor),
                length(boxColor) != 1,
                !is.character(boxColor))) {
            warning("boxColor must be of class character and must specify the name of the color or the rgb value. Assigning default value 'deepskyblue3'.")
            boxColor <- 'deepskyblue3'
        } else if (.rgbaConversion(boxColor) == 'invalid value') {
            warning("boxColor specified is not valid. Assigning default value 'deepskyblue3'.")
            boxColor <- 'deepskyblue3'
        }

        if (any(is.null(boxTransparency),
                !is.numeric(boxTransparency),
                length(boxTransparency) != 1,
                boxTransparency <= 0,
                boxTransparency > 1)) {
            warning('boxTransparency must be a singular numeric value and must be between 0 and 1. Assigning default value 0.5.')
            boxTransparency <- 0.5
        }
    }

    # ViolinLayer Validations
    if (any(is.null(violinLayer),
            length(violinLayer) != 1,
            !is.logical(violinLayer))) {
        warning('violinLayer must be a singular logical value. Assigning default value FALSE.')
        violinLayer <- FALSE
    }

    if (violinLayer) {
        if (any(is.null(violinColor),
                length(violinColor) != 1,
                !is.character(violinColor))) {
            warning("violinColor must be of class character and must specify the name of the color or the rgb value. Assigning default value 'goldenrod1'.")
            violinColor <- 'goldenrod1'
        } else if (.rgbaConversion(violinColor) == 'invalid value') {
            warning("violinColor specified is not valid. Assigning default value 'goldenrod1'.")
            violinColor <- 'goldenrod1'
        }

        if (any(is.null(violinTransparency),
                !is.numeric(violinTransparency),
                length(violinTransparency) != 1,
                violinTransparency <= 0,
                violinTransparency > 1)) {
            warning('violinTransparency must be a singular numeric value and must be between 0 and 1. Assigning default value 0.5.')
            violinTransparency <- 0.5
        }
    }

    #Mean layer validations
    if (boxLayer &&
        any(is.null(showMean),
            length(showMean) != 1,
            !is.logical(showMean))) {
        warning('showMean must be a singular logical value. Assigning default value TRUE.')
        showMean <- TRUE
    }

    #point validations
    if ((boxLayer || violinLayer) &&
        any(is.null(showPoints),
            length(showPoints) != 1,
            !is.logical(showPoints))) {
        warning('showPoints must be a singular logical value. Assigning default value TRUE.')
        showPoints <- TRUE
    }

    #axisFree
    if (any(is.null(axisFree),
            length(axisFree) != 1,
            !is.logical(axisFree))) {
        warning('axisFree must be a singular logical value. Assigning default value TRUE.')
        axisFree <- TRUE
    }

    obsPlot <- NULL
    if (plotType == 'canvasxpress') {
        if (facet == TRUE) {
            numcol   <- ((data[[plotByCol]] %>% unique %>% length) / numrow) %>% ceiling
            cx_data  <- data %>% select(!!rlang::sym(valueCol)) %>% t() %>% as.data.frame()
            smp_data <- data %>% select(-!!rlang::sym(valueCol))
            rownames(smp_data) <- colnames(cx_data)
            obsPlot  <- canvasXpress(data               = cx_data,
                                    smpAnnot            = smp_data,
                                    graphOrientation    = 'vertical',
                                    graphType           = 'Boxplot',
                                    groupingFactors     = groupCol,
                                    boxplotColor        = boxColor,
                                    boxplotDataPointTransparency = boxTransparency,
                                    boxplotMean         = showMean,
                                    boxplotWhiskersType = 'single',
                                    showViolinBoxplot   = violinLayer,
                                    showBoxplotIfViolin = boxLayer,
                                    violinColor         = violinColor,
                                    violinTransparency  = violinTransparency,
                                    smpLabelRotate      = labelAngle,
                                    smpLabelScaleFontFactor = 1,
                                    smpTitle            = xlab,
                                    smpTitleFontStyle   = 'bold',
                                    smpTitleScaleFontFactor = 1,
                                    layoutAdjust        = axisFree,
                                    showBoxplotOriginalData = showPoints,
                                    theme               = 'CanvasXpress',
                                    xAxisTitle          = ylab,
                                    xAxis2Show          = TRUE,
                                    title               = title,
                                    showLegend          = FALSE,
                                    layoutTopology      = paste0(numrow, 'X', numcol),
                                    segregateSamplesBy  = plotByCol)
        } else {
            plotlist   <- list()
            plotby_vec <- unique(data[[plotByCol]])
            obsPlot    <- lapply(plotby_vec, function(x) {
                data_subset <- data %>%
                    filter(!!rlang::sym(plotByCol) == x)
                cx_data <- data_subset %>%
                    select(!!rlang::sym(valueCol)) %>%
                    t() %>%
                    as.data.frame()
                smp_data <- data_subset %>%
                    select(-!!rlang::sym(valueCol))
                rownames(smp_data) <- colnames(cx_data)
                title <- x

                canvasXpress(data                = cx_data,
                             smpAnnot            = smp_data,
                             graphOrientation    = 'vertical',
                             graphType           = 'Boxplot',
                             groupingFactors     = groupCol,
                             boxplotColor        = boxColor,
                             boxplotDataPointTransparency = boxTransparency,
                             boxplotMean         = showMean,
                             boxplotWhiskersType = 'single',
                             showViolinBoxplot   = violinLayer,
                             showBoxplotIfViolin = boxLayer,
                             violinColor         = violinColor,
                             violinTransparency  = violinTransparency,
                             smpLabelRotate      = labelAngle,
                             smpLabelScaleFontFactor = 1,
                             smpTitle            = xlab,
                             smpTitleFontStyle   = 'bold',
                             smpTitleScaleFontFactor = 1,
                             layoutAdjust        = axisFree,
                             showBoxplotOriginalData = showPoints,
                             theme               = 'CanvasXpress',
                             xAxisTitle          = ylab,
                             title               = title,
                             xAxis2Show          = TRUE,
                             showLegend          = FALSE)
            })
        }
    } else {
        .addGeoms <- function(obsPlot) {
            if (boxLayer == TRUE) {
                obsPlot <- obsPlot + geom_boxplot(
                    alpha = boxTransparency,
                    color = boxColor,
                    fill  = boxColor,
                    outlier.shape = outlier.shape,
                    outlier.size  = outlier.size
                )
            }

            if (violinLayer == TRUE) {
                obsPlot <- obsPlot + geom_violin(alpha = violinTransparency,
                                                 color = violinColor,
                                                 fill  = violinColor)
            }

            if (showPoints == TRUE) {
                    obsPlot <-
                        obsPlot + geom_point(position = position_jitter(width = 0.1),
                                             alpha    = 0.5,
                                             color    = 'grey30',
                                             fill     = 'dodgerblue4',
                                             size     = 2,
                                             shape    = 'circle')
            }

            if (showMean == TRUE) {
                obsPlot <- obsPlot + stat_summary(fun   = mean,
                                                  geom  = 'point',
                                                  shape = 'square',
                                                  size  = 3,
                                                  color = 'goldenrod1',
                                                  fill  = 'red2',
                                                  alpha = 0.7)
            }

            return(obsPlot)
        }

        # Reduce box outliers to a dot if geom_points turned on
        outlier.size  <- 1.5
        outlier.shape <- 19
        if (showPoints) {
            outlier.size  <- 1
            outlier.shape <- '.'
        }

        if (axisFree) {
            axisFree <- 'free'
        } else {
            axisFree <- 'fixed'
        }

        # Plot code here
        if (facet == TRUE) {
            obsPlot <- ggplot2::ggplot(data, aes_string(x = groupCol, y = valueCol))
            obsPlot <- .addGeoms(obsPlot)
            facetFormula <- stringr::str_c('~', plotByCol, sep = ' ')
            obsPlot <- obsPlot + ggplot2::facet_wrap(facetFormula, nrow = numrow, scales = axisFree)
            obsPlot <- obsPlot + ggplot2::xlab(xlab)
            obsPlot <- obsPlot + ggplot2::ylab(ylab)
            if (!missing(title)) {
                obsPlot <- obsPlot + ggplot2::ggtitle(title)
            }

            # Rotate xaxis group labels
            if (labelAngle > 0) {
                obsPlot <- obsPlot + theme(axis.text.x = element_text(angle = labelAngle, hjust = 1))
            }
        } else {
            plotlist <- list()
            for (obs in unique(data[[plotByCol]])) {
                dat   <- data[data[[plotByCol]] == obs, ]
                aplot <- ggplot(dat, aes_string(x = groupCol, y = valueCol)) +
                    xlab(xlab) +
                    ylab(ylab) +
                    ggtitle(obs)
                aplot <- .addGeoms(aplot)

                # Rotate xaxis group labels
                if (labelAngle > 0) {
                    aplot <- aplot + theme(axis.text.x = element_text(angle = labelAngle, hjust = 1))
                }
                plotlist[[obs]] <- aplot
            }
            obsPlot <- plotlist
        }
    }
    return(obsPlot)
}
