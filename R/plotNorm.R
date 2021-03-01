#' Plot log2CPM before and after normalization
#'
#' Takes a DGEobj containing counts or a counts matrix as input. Returns a canvasXpress or ggplot
#' object containing a faceted plot of log2CPM before and after normalization.
#'
#' Either a box plot or density plot category can be chosen.
#'
#' Normalization is performed by edgeR::calcNormFactors. Note TMM is specifically tailored to count-based
#' data.  Thus this function is only appropriate for count-based data.
#'
#' @param DGEdata  A DGEobj or counts matrix.
#' @param plotType Plot type must be canvasXpress or ggplot (Default to canvasXpress).
#' @param plotCategory  One of "box" or "density." (Default = "box")
#' @param normalize Default = "TMM" and invokes TMM normalization. Other allowed
#'   values are: "RLE", "upperquartile", or "none". Invokes edgeR::calcNormFactors for
#'   normalization.
#'
#' @return A faceted canvasXpress/ggplot plot showing before/after log2CPM normalization.
#'
#' @examples
#' \dontrun{
#'    myNormPlotBox <- plotNorm(myDGEobj, plotCategory = "box")
#'    myNormPlotDensity <- plotNorm(counts, plotCategory = "density")
#' }
#'
#' @import magrittr ggplot2
#' @importFrom stringr str_c
#' @importFrom DGEobj getItem
#' @importFrom assertthat assert_that
#' @importFrom canvasXpress canvasXpress
#' @importFrom htmlwidgets JS
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr gather spread
#' @importFrom dplyr mutate bind_rows select
#'
#' @export
plotNorm <- function(DGEdata,
                     plotType = "canvasXpress",
                     plotCategory = "box",
                     normalize = "tmm") {
    plotType     <- tolower(plotType)
    plotCategory <- tolower(plotCategory)
    normalize    <- tolower(normalize)

    assertthat::assert_that(any(c("matrix", "DGEobj") %in% class(DGEdata)),
                            msg = "DGEdata must be of either class 'matrix' or 'DGEobj'.")
    assertthat::assert_that(plotType %in% c("canvasxpress", "ggplot"),
                            msg = "plotType must be either canvasXpress or ggplot.")
    assertthat::assert_that(plotCategory %in% c("box", "density"),
                            msg = "plotCategory must be one of 'box' or 'density'.")
    assertthat::assert_that(normalize %in% c("tmm", "rle", "upperquartile", "none"),
                            msg = "normalize must be one of 'TMM', 'RLE', 'upperquartile', or 'none'.")

    if ("matrix" %in% class(DGEdata)) {
        counts <- DGEdata
    } else {
        counts <- DGEobj::getItem(DGEdata, "counts")
    }

    tall <- build_normalized_data(counts) %>%
        bind_rows(build_normalized_data(counts, toupper(normalize)))

    title <- stringr::str_c("Log2CPM before/after", normalize, "normalization", sep = " ")

    resultPlot <- NULL
    if (plotType == "canvasxpress") {
        if (plotCategory == "box") {
            resultPlot <- build_cx_box_plot(tall, title)
        } else {
            resultPlot <- build_cx_density_plot(tall, title)
        }
    } else {
        if (plotCategory == "density") {
            resultPlot <- build_gg_density_plot(tall, title)
        } else {
            resultPlot <- build_gg_box_plot(tall, title)
        }
    }
}

build_cx_data <- function(data) {
    cx.data <- data.frame()
    for (sample in levels(as.factor(data$SampleID))) {
        if (nrow(cx.data) == 0) {
            cx.data <- select_sample(data, sample, TRUE)
        } else {
            cx.data <- cbind(cx.data, select_sample(data, sample))
        }

    }
    return(cx.data)
}

select_sample <- function(data, sampleID, select_all = FALSE) {
    if (select_all) {
        sample <- subset(data, SampleID == sampleID,
                         select = c("GeneID", "Normalization", "Log2CPM"))
        colnames(sample)[3] = sampleID
    } else {
        sample <- subset(data, SampleID == sampleID, select = c("Log2CPM"))
        colnames(sample)[1] = sampleID
    }

    return(sample)
}

build_cx_density_plot <- function(data, title) {
    plot.data <- data %>%
        spread(SampleID, Log2CPM)
    cx.data <- plot.data %>%
        select(!c(GeneID, Normalization))
    var.annot <- plot.data %>%
        select(c("GeneID", "Normalization"))

    xlab <- "Log2CPM"
    ylab <- "density"
    events <- JS(
        "{'mousemove' : function(o, e, t) {
                            if (o != null && o != false) {
                                if (o.objectType == 'Density' &&
                                    o.display != null &&
                                    o.display.indexOf('-') > -1) {
                                    sampleID = o.display.split('-')[2];
                                    t.showInfoSpan(e, '<b>Sample ID</b>: ' + sampleID);
                                } else {
                                    t.showInfoSpan(e, o.display);
                                };
                            };
                        }
        }"
    )
    resultPlot <- canvasXpress(
        data                    = cx.data,
        varAnnot                = var.annot,
        histogramData           = TRUE,
        graphType               = "Scatter2D",
        xAxisTitle              = xlab,
        yAxisTitle              = ylab,
        hideHistogram           = TRUE,
        showHistogramDensity    = TRUE,
        segregateVariablesBy    = list("Normalization"),
        title                   = title,
        showLegend              = FALSE,
        events                  = events
    )
}

build_cx_box_plot <- function(data, title) {
    smp.data <- build_cx_data(data)
    xlab     <- "Log2CPM"
    ylab     <- "SampleID"
    cx.data  <- as.data.frame(t(subset(smp.data, select = -c(GeneID, Normalization))))
    resultPlot <- canvasXpress(
        data                    = cx.data,
        smpAnnot                = smp.data[, c("GeneID", "Normalization")],
        graphType               = "Boxplot",
        title                   = title,
        showLegend              = FALSE,
        xAxisTitle              = xlab,
        yAxisTitle              = ylab,
        segregateSamplesBy      = list("Normalization"),
        groupingFactors         = list("Normalization"),
        graphOrientation        = "vertical",
        boxplotOutliersRatio    = "2",
        colorScheme             = "ColorScpectrum"
    )
    return(resultPlot)
}


build_gg_density_plot <- function(data, title) {
    data %>%
        ggplot(aes(x = Log2CPM, color = SampleID)) +
        geom_density() +
        facet_grid(~Normalization) +
        ggtitle(title)  +
        theme_gray() +
        theme(legend.position = "none")
}

build_gg_box_plot <- function(data, title) {
    data %>%
        ggplot(aes(x = SampleID, y = Log2CPM, color = SampleID)) +
        geom_boxplot(alpha = 0.5) +
        facet_grid( ~ Normalization) +
        ggtitle(title)  +
        theme_gray() +
        theme(axis.text.x = element_blank(),
              legend.position = "none")
}

build_normalized_data <- function(counts, normalize = "none") {
    DGEobj.utils::convertCounts(counts, unit = "cpm", log = TRUE, normalize = normalize) %>%
        as.data.frame() %>%
        rownames_to_column(var = "GeneID") %>%
        gather(key = "SampleID", val = "Log2CPM", -GeneID) %>%
        mutate(Normalization = normalize)
}
