#' Plot network of DGEobj relationships
#'
#' Reads a DGEobj and produces a node pair file defining parent/child relationships between
#' data items in the DGEobj.
#'
#' @param dgeObj DGEobj to find the parent/child relationships between data items.
#' @param plotType Plot type must be canvasXpress or ggplot (default = canvasXpress).
#' @param directed Passed to igraph::graph_from_data_frame. Indicates if the graph should
#'     be directed or not. (default = TRUE)
#'
#' @return A class igraph network object for plotType ggplot and canvasxpress network plot for plotType canvasxpress.
#'
#' @examples
#' \dontrun{
#'   library(igraph)
#'   library(RColorBrewer)
#'
#'   # Prepare canvasxpress network plot
#'   mynet <- mapDGEobj(dgeObj)
#'
#'   # Prepare an iGraph object for plotting
#'   mynet <- mapDGEobj(dgeObj, plotType = "ggplot")
#'
#' @import magrittr
#' @importFrom assertthat assert_that
#' @importFrom igraph graph_from_data_frame
#' @importFrom canvasXpress canvasXpress
#' @importFrom htmlwidgets JS
#' @importFrom dplyr filter rename
#' @importFron tibble rownames_to_column
#'
#' @export
mapDGEobj <- function(dgeObj,
                      plotType = "canvasXpress",
                      directed = TRUE) {

    assertthat::assert_that(!missing(dgeObj),
                            !is.null(dgeObj),
                            "DGEobj" %in% class(dgeObj),
                            msg = "dgeObj must be specified and must be of class 'DGEobj'.")

    plotType <- tolower(plotType)
    if (any(is.null(plotType),
            !is.character(plotType),
            length(plotType) != 1,
            !tolower(plotType) %in% c("canvasxpress", "ggplot"))) {
        warning("plotType must be either canvasXpress or ggplot. Assigning default value 'CanvasXpress'.")
        plotType <- "canvasxpress"
    }

    if (any(is.null(directed),
            !is.logical(directed),
            length(directed) != 1)) {
        warning("directed must be a singular logical value. Assigning default value TRUE.")
        directed <- TRUE
    }

    child <- names(dgeObj) %>%
        as.data.frame()
    colnames(child) <- "child"

    parent <- data.frame(attr(dgeObj, "parent")) %>%
        t() %>%
        as.data.frame() %>%
        dplyr::rename(parent = V1) %>%
        dplyr::filter(nchar(parent) > 0) %>%
        tibble::rownames_to_column("child")

    assertthat::assert_that(all(parent$parent %in% child$child),
                            all(parent$child %in% child$child),
                            length(unique(parent$child)) == nrow(parent),
                            msg = "A valid DGEobj needs to be specified. Node can have a maximum of one parent only.")

    type <- attr(dgeObj, "type") %>%
        as.data.frame() %>%
        t() %>%
        as.data.frame() %>%
        dplyr::rename(Type = V1) %>%
        tibble::rownames_to_column("child")

    basetype <- attr(dgeObj, "basetype") %>%
        as.data.frame() %>%
        t() %>%
        as.data.frame() %>%
        dplyr::rename(BaseType = V1) %>%
        tibble::rownames_to_column("child")

    nodes <- left_join(type, basetype, by = "child")
    edges <- parent

    if (plotType == "canvasxpress") {
        colnames(nodes) <- c("id", "Type", "BaseType")
        colnames(edges) <- c("id2", "id1")

        events <- htmlwidgets::JS("{ 'mousemove' : function(o, e, t) {
                                                if (o != null && o != false) {
                                                    if (o.objectType == null) {
                                                        if (o.nodes != null) {
                                                            t.showInfoSpan(e, '<b>' + 'Node' + ': ' + o.nodes[0].id + '</b> <br/>' +
                                                             '<b>' + 'Type'  + '</b>' + ': ' + o.nodes[0].Type + '<br/>' +
                                                             '<b>' + 'Base type'  + '</b>' + ': ' + o.nodes[0].BaseType + '<br/>');

                                                        } else if (o.edges != null) {
                                                            t.showInfoSpan(e, '<b>' + o.edges[0].id1 + '&#10230;' + o.edges[0].id2 + '</b>');
                                                        }
                                                    } else {
                                                        t.showInfoSpan(e, o.display);
                                                    };
                                                }; }}")

        mapDGEplot <- canvasXpress::canvasXpress(data              = list(nodeData = nodes, edgeData = edges),
                                                 colorNodeBy       = "Type",
                                                 labelNodePosition = "left",
                                                 edgeWidth         = 2,
                                                 graphType         = "Network",
                                                 nodeSize          = 30,
                                                 networkLayoutType = "forceDirected",
                                                 events            = events)

    } else {
        mapDGEplot <- igraph::graph_from_data_frame(d = edges, vertices = nodes, directed = directed)
    }

    return(mapDGEplot)
}
