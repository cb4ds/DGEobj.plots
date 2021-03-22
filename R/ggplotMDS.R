#' Create limma MDS plot
#'
#' This is a wrapper around the plotMDS function that generates the plot with
#' canvasXpress or ggplot2 instead of base graphics.
#'
#' colorBy, shapeBy, and sizeBy are grouping variables that encode group info by
#' color, shape, or size.  These are vectors that must be the same length as
#' ncol(DGEdata). colorBy and sizeBy will plot as continuous color or size
#' changes if a numeric vector is used. Convert the vector to a factor to
#' treat as groups instead of continuous.
#'
#' The underlying limma::plotMDS() function uses a default of top = 500 to use the top 500
#' highest fold change genes for the analysis. Based on observed speed tests,
#' top = Inf has been utilized as the default for this function, as it was shown to quickly produce
#' a more stable result. However, this is configurable using the top argument, which
#' allows for selection of a number close the number of differential genes in the
#' input data.
#'
#' @param DGEdata A DGEList object taken after normalization
#'   OR a DGEobj that contains a DGEList OR a log2cpm matrix. (Required)
#' @param plotType Plot type must be canvasXpress or ggplot (Default to canvasXpress).
#' @param colorBy A grouping vector to color by (e.g. ReplicateGroup) (Required)
#' @param shapeBy A grouping vector to map to shape (Optional)
#' @param sizeBy A numeric vector to define point size (Optional)
#' @param top Number of most variant genes to include (Default = Inf)
#' @param labels Text labels for the samples. These should be short
#'   abbreviations of the sample identifiers.
#'   Default = ReplicateGroup or rownames of DGEdata. Set to NULL to disable
#'   text labels.
#' @param labelSize Control size for the text labels in the plot,
#' @param title A title for the plot. (Optional)
#' @param vlineIntercept X intercept of vertical line (Optional)
#' @param hlineIntercept Y intercept of horizontal line (Optional)
#' @param reflineColor Color for the horizontal and vertical reference lines
#'   (Default = "darkgoldenrod1")
#' @param reflineSize Thickness of the reference lines (Default = 0.5)
#' @param symShape Set the default shape of the symbols if not mapped to a column (Default = 19, solid circle)
#' @param symSize Set the default size of the symbols if not mapped to a column
#'   (Default = 5)
#' @param transparency Set transparency (Default = 0.7)
#' @param shapes A vector of shapes to override the default 8 shapes used in shapeBy (optional)
#' @param colors A color pallet to substitute for the default 8 color pallet used by colorBy (optional)
#' @param dim.plot Define which dimension to plot (Default = c(1,2))
#'
#' @return A list with two elements, the ggplot object and the MDS object returned
#'    by the plotMDS() function.
#'
#' @examples
#' \dontrun{
#'      # Plot the first two dimensions using all genes
#'      myMDS <- ggplotMDS(MyDGEList)
#'
#'      # Plot the 2nd and 3rd dimensions using the top 1000 genes
#'      myMDS <- ggplotMDS(myDGEList, dim.plot = c(2, 3) ndim = 3)
#'      myMDS[[1]]
#' }
#'
#' @import ggplot2 magrittr ggrepel
#' @importFrom canvasXpress canvasXpress
#' @importFrom htmlwidgets JS
#' @importFrom assertthat assert_that
#' @importFrom limma plotMDS
#' @importFrom stats as.dist
#'
#' @export
ggplotMDS <- function(DGEdata,
                      plotType = "canvasXpress",
                      colorBy,
                      shapeBy,
                      sizeBy,
                      top = Inf,
                      labels,
                      labelSize = 3,
                      title,
                      hlineIntercept,
                      vlineIntercept,
                      reflineColor = "red",
                      reflineSize = 0.5,
                      symShape = "circle",
                      symSize = 5,
                      transparency = 0.7,
                      shapes,
                      colors,
                      dim.plot = c(1, 2)) {

    plotType = tolower(plotType)

    assertthat::assert_that(class(DGEdata) %in% c("DGEobj", "DGEList", "matrix"),
                            msg = "DGEdata must be of class 'DGEList', 'DGEobj', or 'matrix'.")
    assertthat::assert_that(plotType %in% c("canvasxpress", "ggplot"),
                            msg = "Plot type must be either canvasXpress or ggplot.")

    assertthat::assert_that(!missing(colorBy),
                            length(colorBy) == ncol(DGEdata),
                            msg = "colorBy must be specified and should be the length of the number of columns in DGEdata.")

    if (!missing(shapeBy)) {
        assertthat::assert_that(length(shapeBy) == ncol(DGEdata),
                                msg = "shapeBy should be the length of the number of columns in DGEdata.")
    }
    if (!missing(sizeBy)) {
        assertthat::assert_that(length(sizeBy) == ncol(DGEdata),
                                msg = "sizeBy should be the length of the number of columns in DGEdata.")
    }

    if (!assertthat::see_if(length(top) == 1, is.numeric(top) | top == Inf)) {
        warning("top should be a numeric value or Inf. Assigning default value 'Inf'.")
        top <- Inf
    }

    if (!assertthat::see_if(is.numeric(transparency),
                            length(transparency) == 1,
                            all(transparency > 0 & transparency <= 1))) {
        warning("transparency must be a singular numeric value and must be between 0 and 1. Assigning default value 0.7.")
        symbolTransparency <- 0.7
    }







    # Validate labels
    addLabels <- TRUE
    addDefaultLabel <- FALSE

    if (!missing(labels)) {
        if (is.null(labels)) {
            addLabels <- FALSE
        } else if (!length(labels) == ncol(DGEdata)) {
            warning("Number of labels does not match the number of columns in DGEdata. Assigning default values.")
            addDefaultLabel <- TRUE
        }
    } else {
        addDefaultLabel <- TRUE
    }

    if (addLabels & addDefaultLabel) {
        labels <- colnames(DGEdata)
        # Get labels from ReplicateGroup if present
        if ("DGEobj" %in% class(DGEdata)) {
            design <- DGEobj::getItem(DGEdata, "design")
            if (exists("design")) {
                if (with(design, exists("ReplicateGroup"))) {
                    labels <- design$ReplicateGroup
                }
            }
        }
    }

    if (plotType == "canvasxpress") {


        cx_valid_shapes <- get_valid_symbolShapes_cxplot()
        cx_default_shapes <- cx_valid_shapes[1:8]
        if (!missing(shapes)) {
            invalid_shapes <- shapes[!shapes %in% cx_valid_shapes]
            if (length(invalid_shapes) > 0) {
                shapes <- shapes[shapes %in% cx_valid_shapes]
                warning(paste("Removing invalid shapes: ",invalid_shapes))
                if (length(shapes) == 0) {
                    shapes <- cx_valid_shapes
                }
            }
        } else {
            shapes <- cx_valid_shapes
        }

        intercept_flag <- FALSE

        if (!missing(hlineIntercept)) {
            if (!is.null(hlineIntercept)) {
                intercept_flag <- TRUE
                if (!(is.numeric(hlineIntercept) & length(hlineIntercept) == 1)) {
                    warning("hlineIntercept must be a singular numeric value. Removing hlineIntercept.")
                    hlineIntercept <- NULL
                }
            }
        }

        if (!missing(vlineIntercept)) {
            if (!is.null(vlineIntercept)) {
                intercept_flag <- TRUE
                if (!(is.numeric(vlineIntercept) & length(vlineIntercept) == 1)) {
                    warning("vlineIntercept must be a singular numeric value. Removing vlineIntercept.")
                    vlineIntercept <- NULL
                }
            }
        }

        if (intercept_flag) {

            if (!missing(reflineColor)) {
                if (!assertthat::see_if(!is.null(reflineColor), is.character(reflineColor), length(reflineColor) == 1)) {
                    warning("reflineColor must be a singular value of class character and must specify the name of the color or the rgb value. Assigning default value 'red'.")
                    reflineColor <- "red"
                } else if (rgbaConversion(reflineColor) == "invalid value") {
                    warning("Color specified is not valid. Assigning default value 'red'.")
                    reflineColor <- "red"
                }
            }

            if (!missing(reflineSize)) {
                if (!assertthat::see_if(!is.null(reflineSize), is.numeric(reflineSize), length(reflineSize) == 1, reflineSize > 0)) {
                    warning("reflineSize must be a singular numeric value greater than 0. Assigning default value '0.5'.")
                    reflineSize <- 0.5
                }
            }
        }
    }

    if (plotType == "ggplot") {

        if (!missing(labels)) {
            if (!assertthat::see_if(is.numeric(labelSize), length(labelSize) == 1, labelSize > 0)) {
                warning("labelSize should be singular numeric value and greater than zero. Assigning default value 3.")
                labelSize <- 3
            }
        }

        intercept_flag <- FALSE
        if (!missing(hlineIntercept)) {
            if (!is.null(hlineIntercept)) {
                intercept_flag <- TRUE
                if (!is.numeric(hlineIntercept)) {
                    warning("hlineIntercept must be numeric. Removing hlineIntercept.")
                    hlineIntercept <- NULL
                }
            }
        }

        if (!missing(vlineIntercept)) {
            if (!is.null(vlineIntercept)) {
                intercept_flag <- TRUE
                if (!is.numeric(vlineIntercept)) {
                    warning("vlineIntercept must be numeric. Removing vlineIntercept.")
                    vlineIntercept <- NULL
                }
            }
        }


        if (intercept_flag) {
            if (!missing(reflineColor)) {
                if (!assertthat::see_if(!is.null(reflineColor), is.character(reflineColor))) {
                    warning("reflineColor must be a of class character and must specify the name of the color or the rgb value. Assigning default value 'red'.")
                    reflineColor <- "red"
                }


            }

            if (!missing(reflineSize)) {
                if (!assertthat::see_if(!is.null(reflineSize), is.numeric(reflineSize), all(reflineSize > 0))) {
                    warning("reflineSize must be a numeric value greater than 0. Assigning default value '0.5'.")
                    reflineSize <- 0.5
                }
            }
        }

        #add valid shapes
        ggplot_valid_shapes <- get_valid_symbolShapes_ggplot()
        ggplot_default_shapes <- ggplot_valid_shapes[1:8]
        if (!missing(shapes)) {
            invalid_shapes <- shapes[!shapes %in% ggplot_valid_shapes]
            if (length(invalid_shapes) > 0) {
                shapes <- shapes[shapes %in% ggplot_valid_shapes]
                warning(paste("Removing invalid shapes: ",invalid_shapes))
                if (length(shapes) == 0) {
                    shapes <- ggplot_valid_shapes
                }
            }
        } else {
            shapes <- ggplot_valid_shapes
        }
    }

    # ColorBlind palette:
    # http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
    cbbPalette <- c("#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "#E69F00",  "#F0E442", "#000000")
    if (missing(colors)) {
        colors <- cbbPalette
    } else {
        if (is.null(colors)) {
            colors <- cbbPalette
            warning("colors is NULL. Assigning default values.")
        } else {
            valid_colors <- validate_colors(colors)
            if (length(valid_colors) == 0) {
                warning("No valid colors present. Assigning default values")
                colors <- cbbPalette
            } else if (!(length(valid_colors) == length(colors))) {
                invalid_colors <- colors[!colors %in% valid_colors]
                warning(paste("Invalid colors present. Eliminating invalid colors:",invalid_colors))
            }

        }
    }

    if (missing(title)) {
        title <- "MDS Plot"
    }

    if ("DGEobj" %in% class(DGEdata)) {
        DGEdata <- DGEobj::getItem(DGEdata, "DGEList")
    }

    mds.data <- limma::plotMDS(DGEdata,
                          top = top,
                          dim.plot = dim.plot,
                          plot = FALSE)

    # Pull the plotting data together
    plot_data <- data.frame(x = mds.data$x, y = mds.data$y, ColorCode = colorBy)

    if (addLabels) {
        plot_data$Labels <- labels
    }

    byShape <- FALSE
    bySize  <- FALSE
    if (!missing(shapeBy)) {
        plot_data$Shape <- shapeBy
        byShape <- TRUE
    } else {
        if (plotType == "ggplot" & !missing(symShape) & !assertthat::see_if(!is.null(symShape), length(symShape) == 1, is_valid_symbolShapes_ggplot(symShape))) {
            warning("symShape must be a singular value of class 'character' or numeric value. Refer help documentation for valid values. Assigning default value 'circle'.")
            symShape <- "circle"
        } else if (plotType == "canvasxpress" & !missing(symShape) & !assertthat::see_if(length(symShape) == 1, is_valid_symbolShapes_cxplot(symShape))) {
            warning("symShape must be a singular value of class 'character'. Assigning default value 'circle'.")
            symShape <- "circle"
        }
    }
    if (!missing(sizeBy)) {
        plot_data$Size <- sizeBy
        bySize <- TRUE
    } else {
        if (!missing(symSize) & !assertthat::see_if(!is.null(symSize), length(symSize) == 1, is.numeric(symSize))) {
            warning("symSize must be a singular numeric value. Assigning default value 5")
            symSize <- 5
        }
    }

    xylab <- list(paste(mds.data$axislabel, mds.data$dim.plot[[1]], sep = " "),
                  paste(mds.data$axislabel, mds.data$dim.plot[[2]], sep = " "))
    citation <- paste("top ", mds.data$top, " genes : gene.selection = ",
                    mds.data$gene.selection, sep = "")

    # PlotType
    if (plotType == "canvasxpress") {
        colors <- unlist(lapply(colors, function(col){
            rgbaConversion(col)
            }))

        colorCol <- "ColorCode"
        shapeCol <- FALSE
        sizeCol  <- FALSE

        if (byShape == TRUE) {
            shapeCol <- "Shape"
        }

        if (bySize == TRUE) {
            sizeCol = "Size"
        }

        reflineColor <- rgbaConversion(reflineColor)
        decorations  <- list()
        if (!missing(hlineIntercept)) {
            decorations <- list(
                line = list(list(color = reflineColor,
                                 width = reflineSize,
                                 y     = hlineIntercept)))
        }

        if (!missing(vlineIntercept)) {
            decorations <- list(
                line = append(decorations$line,
                              list(list(color = reflineColor,
                                        width = reflineSize,
                                        x     = vlineIntercept))))
        }

        cx.data  <- subset(plot_data, select = c(x, y))
        var.data <- subset(plot_data, select = -c(x, y), drop = FALSE)
        events <- htmlwidgets::JS("{ 'mousemove' : function(o, e, t) {
                                                if (o != null && o != false) {
                                                    if (o.objectType == null && o.z.Labels != null) {
                                                        t.showInfoSpan(e, '<b>' +  o.z.Labels[0] + '</b> <br/>' +
                                                        '<b>' + 'SampleID' + ': ' + o.y.vars+ '</b> <br/>' +
                                                        '<b>' + o.y.smps[0]  + '</b>' + ': ' + o.y.data[0][0] + '<br/>' +
                                                        '<b>' + o.y.smps[1]  + '</b>' + ': ' + o.y.data[0][1]);
                                                    } else {
                                                        t.showInfoSpan(e, o.display);
                                                    };
                                                }; }}")

        mdsplot <- canvasXpress::canvasXpress(data                    = cx.data,
                                              varAnnot                = var.data,
                                              decorations             = decorations,
                                              graphType               = "Scatter2D",
                                              colors                  = colors,
                                              colorBy                 = colorCol,
                                              shapeBy                 = shapeCol,
                                              sizeBy                  = sizeCol,
                                              showDecorations         = TRUE,
                                              shapes                  = shapes,
                                              title                   = title,
                                              xAxisTitle              = xylab[[1]],
                                              yAxisTitle              = xylab[[2]],
                                              citation                = citation,
                                              citationScaleFontFactor = 0.8,
                                              events                  = events)

    } else {

        if (byShape == FALSE & bySize == FALSE) {
            mdsplot <- ggplot(plot_data, aes(x = x, y = y, color = ColorCode)) +
                geom_point(shape = symShape, size = symSize, alpha = transparency)
        } else if (byShape == TRUE & bySize == FALSE) {
            mdsplot <- ggplot(plot_data, aes(x = x, y = y, color = ColorCode, shape = Shape)) +
                geom_point(size = symSize, alpha = transparency) +
                scale_shape_manual(values = shapes)
        } else if (byShape == FALSE & bySize == TRUE) {
            mdsplot <- ggplot(plot_data, aes(x = x, y = y, color = ColorCode, size = Size)) +
                geom_point(shape = symShape, alpha = transparency)
        } else if (byShape == TRUE & bySize == TRUE) {
            mdsplot <- ggplot(plot_data, aes(x = x, y = y, color = ColorCode, shape = Shape, size = Size)) +
                geom_point(alpha = transparency) +
                scale_shape_manual(values = shapes)
        }

        if (!is.null(labels)) {
            if (missing(labelSize)) {
                mdsplot <- mdsplot +
                    ggrepel::geom_text_repel(aes(label = Labels))
            } else {
                mdsplot <- mdsplot +
                    ggrepel::geom_text_repel(aes(label = Labels), size = labelSize)
            }
        }

        # For discrete color values
        if (length(unique(colorBy)) <= length(colors)) {
            mdsplot <- mdsplot +
                scale_fill_manual(values = colors) +
                scale_colour_manual(values = colors)
        }

        # Add some other common elements
        mdsplot <- mdsplot +
            coord_fixed() +
            xlab(xylab[[1]]) +
            ylab(xylab[[2]]) +
            ggtitle(title)

        # Place an annotation on the bottom left of the plot
        xrange <- xrange(mdsplot)
        yrange <- yrange(mdsplot)
        # Put the annotation 10% from xmin
        xpos <- xrange[1] + ((xrange[2] - xrange[1]) * 0.1 )
        mdsplot <- mdsplot + annotate("text", x = xpos, y = yrange[1],
                                      label = citation, hjust = 0,
                                      size = rel(2.5), color = "grey30")

        if (!missing(hlineIntercept)) {
            mdsplot <- mdsplot + geom_hline(yintercept = hlineIntercept,
                                            color = reflineColor,
                                            size = reflineSize)
        }
        if (!missing(vlineIntercept)) {
            mdsplot <- mdsplot + geom_vline(xintercept = vlineIntercept,
                                            color = reflineColor,
                                            size = reflineSize)
        }
    }

    return(list(plot = mdsplot, mdsobj = mds.data))
}


#' Explain variance of MDS object or log2 matrix
#'
#' Takes a class MDS object from limma::plotMDS() and generates two plots: 1)
#' fraction of variance for each dimension, 2) cumulative variance. By default,
#' it plots the first 10 dimensions or the first N dimensions totaling 90%.
#'
#' @param mds A class MDS object from limma::plotMDS() or a data matrix to analyze
#'   (typically log2) (required)
#' @param plotType Plot type must be canvasXpress or ggplot (Default to canvasXpress).
#' @param topN The number of dimensions to plot (Default = 10)
#' @param cumVarLimit The maximum cumulative variance to plot. Range 0-1. (Default = 0.9)
#' @param barColor Default = "dodgerblue4"
#' @param barFill Default = "dodgerblue3"
#' @param barWidth Range 0-1. (Default = 0.65)
#' @param barSize Thickness of the fill border (Default = 0.1)
#'
#' @return A list with two ggplots and the variance explained data.frame.
#'
#' @examples
#' \dontrun{
#'      # Plot the first two dimensions
#'      MyMDS <- ggplotMDS(MyDGEList)
#'      MyMDS[[1]]  #the MDS plot
#'
#'      # Then apply MDS_var_explained (the MDS object is MyMDS[[2]])
#'      varResults <- MDS_var_explained(MyMDS[[2]])
#'      varResults[[1]] # The Variance per dimension plot
#'      varResults[[2]] # The cumulative variance plot
#'      var_explained <- varResults[[3]]  # Data used for plotting (unfiltered)
#' }
#'
#' @import ggplot2 magrittr
#' @importFrom assertthat assert_that
#' @importFrom limma plotMDS
#' @importFrom stats cmdscale var
#' @importFrom canvasXpress canvasXpress
#'
#' @export
MDS_var_explained <- function(mds,
                              plotType = "canvasXpress",
                              topN = 10,
                              cumVarLimit = 0.9,
                              barColor="dodgerblue4",
                              barFill = "dodgerblue3",
                              barWidth = 0.65,
                              barSize = 0.1) {

    assertthat::assert_that(!missing(mds),
                            msg = "mds is required and must be specified.")
    assertthat::assert_that(plotType %in% c("canvasXpress", "ggplot"),
                            msg = "Plot type must be either canvasXpress or ggplot.")

    if (!("MDS" %in% class(mds))) {
        mds <- limma::plotMDS(mds, plot = FALSE)
    }

    mds.distances <- mds %$% distance.matrix %>% as.dist

    mdsvals <- mds.distances %>%
        {suppressWarnings(cmdscale(., k = ncol(mds$distance.matrix) - 1))} %>%
        magrittr::set_colnames(stringr::str_c("Dim", seq_len(ncol(.)))) %>%
        as.data.frame

    var_vec <- unname(apply((mdsvals %>% as.matrix), 2, stats::var)) %>%
        magrittr::divide_by(sum(.))
    var_explained <- data.frame(var    = var_vec,
                                cumvar = cumsum(var_vec),
                                dim    = seq_along(var_vec))

    idx <- var_explained$cumvar < cumVarLimit
    if (sum(idx) < topN) {
        topN <- sum(idx)
    }
    plotdat <- var_explained[idx,][1:topN,]

    setBreaks <- function(limits){
        # Return integer breaks
        low <- floor(limits[1])
        high <- ceiling(limits[2])
        seq(from = low, to = high, by = 1)
    }

    resultList   <- list()
    varexp_title <- "Variance Explained by MDS Dimensions"
    cumvar_title <- "Cumulative Variance Explained by MDS Dimensions"

    xlab    <- "MDS dimension"
    ylab_ve <- "Variance Explained"
    ylab_cv <- "Cumulative Variance Explained"

    if (plotType == "canvasXpress") {
        rownames(plotdat) <- rownames(mdsvals[idx,][1:topN,])
        cx.data <- as.data.frame(t(plotdat))

        resultList$varexp <- canvasXpress::canvasXpress(data             = cx.data["var", ],
                                                        graphOrientation = "vertical",
                                                        graphType        = "Bar",
                                                        showLegend       = FALSE,
                                                        colors           = barColor,
                                                        title            = varexp_title,
                                                        xAxisTitle       = xlab,
                                                        smpTitle         = ylab_ve)

        resultList$cumvar <- canvasXpress::canvasXpress(data             = cx.data["cumvar", ],
                                                        graphOrientation = "vertical",
                                                        graphType        = "Bar",
                                                        showLegend       = FALSE,
                                                        colors           = barColor,
                                                        title            = cumvar_title,
                                                        xAxisTitle       = xlab,
                                                        smpTitle         = ylab_cv)
    } else {
        # Fraction variance for each dimension
        resultList$varexp <- ggplot(plotdat) +
            aes(x = dim, y = var) +
            geom_col(color = barColor,
                     fill = barFill,
                     size = barSize,
                     width = barWidth) +
            labs(title = varexp_title,
                 x = xlab,
                 y = ylab_ve) +
            scale_x_continuous(breaks = setBreaks)

        # Cumulative variance plot (change the y dimension and relabel)
        resultList$cumvar <- resultList$varexp + aes(y = cumvar) +
            labs(title = cumvar_title,
                 y = ylab_cv) +
            ylim(0,1)
    }
    # Return the full data table too
    resultList$var_explained <- var_explained

    return(resultList)
}
