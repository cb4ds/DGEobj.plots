#' Plot histogram analysis of p-value distributions
#'
#' Generate a facet plot (or optionally individual plots) from a dataframe of
#' numbers. Intended to perform histogram analysis of p-value distributions,
#' but should be useful for any dataframe of numeric columns.
#'
#' @param P.Val A matrix or dataframe of numeric data; col = samples
#' @param plotType Plot type must be canvasXpress or ggplot (Default to canvasXpress).
#' @param facet Set to FALSE to print individual plots instead of a faceted plot. (Default = TRUE)
#' @param binWidth Range is always 0-1 for p-values. (Default = 0.02)
#' @param alpha Set the transparency. (Default = 0.6)
#' @param color Color for the histogram outline. (Default = "dodgerblue3")
#' @param fill Fill color for the histogram. (Default = "dodgerblue3")
#'
#' @return A ggplot2 object if facet = TRUE or a list of plots if facet = FALSE. (Default = TRUE)
#'
#' @examples
#' \dontrun{
#'    # Print to console using all defaults
#'    MyPvalMatrix <- extractCol(getType(myDGEobj, "topTable"), "P.Value")
#'    plotPvalHist(MyPvalMatrix)
#'
#'    # Use some custom arguments
#'    myplot <- plotPvalHist(MyPValMatrix)
#' }
#'
#' @import ggplot2
#' @importFrom dplyr filter
#' @importFrom canvasXpress canvasXpress
#'
#' @export
plotPvalHist <- function(P.Val,
                         plotType = "canvasXpress",
                         facet = TRUE,
                         binWidth = 0.02,
                         alpha = 0.6,
                         color = "dodgerblue3",
                         fill = "dodgerblue3") {

    assertthat::assert_that(!missing(P.Val),
                            class(P.Val)[[1]] == "matrix" | class(P.Val)[[1]] == "data.frame",
                            msg = "P.Val must be specified and must be of class matrix or dataframe.")

    assertthat::assert_that(plotType %in% c("canvasXpress", "ggplot"),
                            msg = "Plot type must be either canvasXpress or ggplot.")

    if (!assertthat::see_if(is.character(color),
                            length(color) == 1)) {
        warning("color must be a singular value of class character and must specify the name of the color or the rgb value. Assigning default value 'dodgerblue3'.")
        color <- "dodgerblue3"
    }

    if (!assertthat::see_if(is.character(fill))) {
        warning("fill must be a singular value of class character and must specify the name of the color or the rgb value. Assigning default value 'dodgerblue3'.")
        fill <- "dodgerblue3"
    }

    if (!assertthat::see_if(is.logical(facet),
                            length(facet) == 1)) {
        warning("facet must be a singular logical value. Assigning default value TRUE")
        facet <- TRUE
    }

    if (!assertthat::see_if(is.numeric(binWidth),
                            length(binWidth) == 1,
                            binWidth > 0 & binWidth <= 1)) {
        warning("binWidth must be a singular numeric value between 0 & 1. Assigning default value 0.02")
        binWidth <- 0.02
    }

    if (!assertthat::see_if(is.numeric(alpha),
                            alpha > 0 & alpha <= 1)) {
        warning("symbolAlpha must be a singular numeric value and must be between 0 and 1. Assigning default value 0.3.")
        alpha <- 0.3
    }

    if (is.matrix(P.Val)) {
        P.Val <- P.Val %>%
            as.data.frame
    }

    samples_num <- ncol(P.Val)
    samples_name <- colnames(P.Val)

    # Set up Tall format

    P.Val$GeneID = rownames(P.Val)
    P.Val <-  stats::reshape(data          = P.Val,
                             idvar         = "GeneID",
                             varying       = samples_name,
                             v.names       = "pval",
                             direction     = "long",
                             timevar       = "levels",
                             times         = samples_name,
                             new.row.names = sequence(prod(length(samples_name), nrow(P.Val))))

    title <- "P-value Histograms"
    plotlist <- list()
    if (plotType == "canvasXpress") {
        if (facet) {
            cx.data <- subset(P.Val, select = pval)
            var.annot <- subset(P.Val, select = -c(pval))
            plotlist <- canvasXpress::canvasXpress(data                    = cx.data,
                                                   varAnnot                = var.annot,
                                                   histogramData           = TRUE,
                                                   graphType               = "Scatter2D",
                                                   colors                  = fill,
                                                   title                   = title,
                                                   xAxisTitle              = "P-value",
                                                   yAxisTitle              = "Count",
                                                   hideHistogram           = FALSE,
                                                   showHistogramDensity    = FALSE,
                                                   showLegend              = FALSE,
                                                   segregateVariablesBy    = list("levels"))
        } else {
            for (i in 1:samples_num) {
                s <- samples_name[i]
                pval_subset <- dplyr::filter(P.Val, grepl(s, levels))
                cx.data <- subset(pval_subset, select = pval)
                var.annot <- subset(pval_subset, select = -c(pval))
                hist_pval <- canvasXpress::canvasXpress(data                    = cx.data,
                                                        varAnnot                = var.annot,
                                                        histogramData           = TRUE,
                                                        graphType               = "Scatter2D",
                                                        colors                  = color,
                                                        title                   = paste(title, "\n", s),
                                                        xAxisTitle              = "P-value",
                                                        yAxisTitle              = "Count",
                                                        hideHistogram           = FALSE,
                                                        showHistogramDensity    = FALSE,
                                                        showLegend              = FALSE)

                plotlist[[i]] = hist_pval
            }
        }
    } else {
        if (facet) {
            numcol <- 3
            numrow <- (samples_num / numcol) %>% ceiling

            plotlist <- ggplot2::ggplot(data = P.Val, aes(x = pval)) +
                ggplot2::geom_histogram(alpha = alpha,
                                        fill = fill,
                                        color = color,
                                        binwidth = binWidth) +
                ggplot2::xlab("P-value") +
                ggplot2::ylab("Count") +
                ggtitle(title) +
                ggplot2::scale_fill_brewer(palette = "Set1") +
                ggplot2::facet_wrap(~levels, nrow = numrow, scales = "free")
        } else {
            for (i in 1:samples_num) {
                s <- samples_name[i]
                pval_subset <- dplyr::filter(P.Val, grepl(s, levels))

                hist_pval <- ggplot2::ggplot(data = pval_subset, aes(x = pval)) +
                    ggplot2::geom_histogram(alpha = alpha,
                                            fill = fill,
                                            color = color,
                                            binwidth = binWidth) +
                    ggplot2::xlab("P-value") +
                    ggplot2::ylab("Count") +
                    ggplot2::ggtitle(paste(title, "\n", s))

                plotlist[[i]] = hist_pval
            }
        }
    }
    return(plotlist)
}
