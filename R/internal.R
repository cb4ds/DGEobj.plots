.tsmsg <- function(...) {
    # works like message() but prepends a timestamp
    message(date(), ": ", ...)
}

# A function that returns a rgb specification if a valid color is provided as input.
.rgbaConversion <- function(color, alpha = 0.5){
    rgbastr <- NULL
    if (!is.character(color)) {
        rgbastr <- "invalid value"
    }
    tryCatch({
        rgbaVal <- paste(c(col2rgb(color), alpha), collapse = ",")
        rgbastr <- paste0("rgba(", rgbaVal, ")")
    }, error = function(e) {
        warning(paste("Ïnvalid color specified",color))
    })
    if (is.null(rgbastr)) {
        rgbastr <- "invalid value"
    }
    rgbastr
}


.getCxPlotDecorations <- function(decorations, color, width, x, y) {
    line <- list(color = color,
                 width = width)
    if (!missing(x) && !missing(y)) {
        line <- append(line, list(x  = x,
                                  x2 = y,
                                  y  = x,
                                  y2 = y))
    } else if (!missing(x)) {
        line <- append(line, list(x = x))
    } else if (!missing(y)) {
        line <- append(line, list(y = y))
    }

    decorations <- list(line = append(decorations$line, list(line)))
}

.is_valid_symbolShapes_cxplot <- function(shape) {
    valid_shapes <- .get_valid_symbolShapes_cxplot()
    is_valid_shape <- ifelse(shape %in% valid_shapes, TRUE, FALSE)
}

.get_valid_symbolShapes_cxplot <- function() {
    valid_shapes <- c("sphere", "square", "rhombus", "triangle", "plus", "star", "octagon", "oval",
                      "minus", "pacman", "pacman2", "mdavid", "rect2", "pentagon",
                      "rect3", "arc", "rectangle", "image")
}

.get_valid_symbolShapes_ggplot <- function() {
    shape_names <- c(
        "circle", "square", "diamond", "triangle", "plus", "asterisk", "cross", "bullet",
        paste("circle", c("open", "filled", "cross", "plus", "small")),
        paste("square", c("open", "filled", "cross", "plus", "triangle")),
        paste("diamond", c("open", "filled", "plus")),
        paste("triangle", c("open", "filled", "square")),
        paste("triangle down", c("open", "filled"))
    )
}

.is_valid_symbolShapes_ggplot <- function(shape) {
    valid_shapes <- .get_valid_symbolShapes_ggplot()
    is_valid_shape <- ifelse(shape %in% valid_shapes, TRUE, FALSE)
    if (!is_valid_shape && (shape %in% c(1:25))) {
        is_valid_shape <- TRUE
    }
    is_valid_shape
}
