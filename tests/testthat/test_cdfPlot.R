context("DGEobj.plots - tests for cdfPlot.R functions")


test_that("cdfPlot.R: cdfPlot()", {
    skip_if(!("ReplicateGroupDesign_fit" %in% names(t_obj1)))

    top_table <- topTable(t_obj1$ReplicateGroupDesign_fit, number = 100)

    # testing plot with default values.
    plot <- cdfPlot(top_table, referenceLine = "blue")
    expect_type(plot, "list")
    expect_s3_class(plot$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(plot$inset, c("canvasXpress", "htmlwidget"))

    plot <- cdfPlot(top_table, plotType = "ggplot", referenceLine = "blue")
    expect_s3_class(plot$main, c("gg", "ggplot"))
    expect_s3_class(plot$inset, c("gg", "ggplot"))
    expect_s3_class(plot$viewport, "viewport")

    # testing plot with customized aesthetics.
    plot_with_aes <- cdfPlot(top_table,
                             insetTitle    = "Sub plot title",
                             xlab          = "xaxis-title",
                             ylab          = "yaxis-title",
                             title         = "MyPlot",
                             referenceLine = "blue",
                             footnote      = "this is footnote of the plot")

    expect_type(plot_with_aes, "list")
    expect_s3_class(plot_with_aes$main, c("canvasXpress", "htmlwidget"))
    expect_s3_class(plot_with_aes$inset, c("canvasXpress", "htmlwidget"))

    #check aesthetic parameters for cxplot

    plot_with_aes <- cdfPlot(top_table,
                             plotType      = "ggplot",
                             insetTitle    = "Sub plot title",
                             xlab          = "xaxis-title",
                             ylab          = "yaxis-title",
                             title         = "MyPlot",
                             referenceLine = "blue",
                             footnote      = "this is footnote of the plot")

    expect_s3_class(plot_with_aes$main, c("gg", "ggplot"))
    expect_s3_class(plot_with_aes$inset, c("gg", "ggplot"))
    expect_s3_class(plot_with_aes$viewport, "viewport")

    expect_setequal(unlist(plot_with_aes$main$labels[c("title", "y", "x")]), c("MyPlot", "yaxis-title", "xaxis-title"))
    expect_setequal(plot_with_aes$inset$labels$title, "Sub plot title")
    expect_equal(plot_with_aes$main$layers[[2]]$geom_params$colour, "blue")
    expect_equal(plot_with_aes$main$layers[[3]]$geom_params$label, "this is footnote of the plot")

    expect_error({cdfPlot(top_table, pvalCol = "p.value")},
                 regexp = "Specified pvalCol not found in the supplied dataframe (contrastDF).",
                 fixed = TRUE)
    expect_error({cdfPlot(top_table, symbolShape = 20)},
                 regexp = "All specified symbol arguments must be of length 2, including symbolSize, symbolShape, symbolColor, and symbolFill.")
    expect_error({cdfPlot(top_table, plotType = "cx")},
                 regexp = "Plot type must be either canvasXpress or ggplot.")

    #contrastDF
    msg <- 'contrastDF must be specified as dataframe with a p-value column.'
    expect_error({cdfPlot(contrastDF = NULL)},
                 regexp = msg)
    expect_error({cdfPlot(contrastDF = t_obj1)},
                 regexp = msg)
    expect_error({cdfPlot()},
                 regexp = msg)
    expect_error({cdfPlot(data.frame())},
                 regexp = msg)

    #plotType
    msg <- 'plotType must be either canvasXpress or ggplot.'
    expect_error({cdfPlot(top_table, plotType = "cx")},
                 regexp = msg)
    expect_error({cdfPlot(top_table, plotType = NULL)},
                 regexp = msg)
    expect_error({cdfPlot(top_table, plotType = 1)},
                 regexp = msg)
    expect_error({cdfPlot(top_table, plotType = c('canvasxpress','ggplot'))},
                 regexp = msg)

    #pvalCol
    msg <- 'pvalCol column not found in contrastDF.'
    expect_error({cdfPlot(top_table, pvalCol = "notacolumn")},
                 regexp = msg)
    expect_error({cdfPlot(top_table, pvalCol = NULL)},
                 regexp = msg)

    #pThreshold
    msg <- 'pthreshold must be a singular numeric value. Assigning default value 0.01.'
    expect_warning({cdfPlot(top_table, pThreshold = "notavalue")},
                 regexp = msg)
    expect_warning({cdfPlot(top_table, pThreshold = NULL)},
                 regexp = msg)
    expect_warning({cdfPlot(top_table, pThreshold = c(1,2))},
                 regexp = msg)
    expect_warning({cdfPlot(top_table, pThreshold = "notavalue")},
                 regexp = msg)

    #title
    msg <- "title must be a singular value of class character. Assigning default value 'NULL'."
    expect_warning({cdfPlot(top_table, title = c('title','title'))},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, title = 1)},
                   regexp = msg)

    #insetTitle
    msg <- "insetTitle must be a singular value of class character. Assigning default value 'NULL'."
    expect_warning({cdfPlot(top_table, insetTitle = c('title','title'))},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, insetTitle = 1)},
                   regexp = msg)

    #xlab
    msg <- "xlab must be a singular value of class character. Assigning default value 'Rank' as the label."
    expect_warning({cdfPlot(top_table, xlab = c('xlab','xlab'))},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, xlab = 1)},
                   regexp = msg)

    #ylab
    msg <- "ylab must be a singular value of class character. Assigning default value 'Rank' as the label."
    expect_warning({cdfPlot(top_table, xlab = c('ylab','ylab'))},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, xlab = 1)},
                   regexp = msg)

    #symbolSize
    msg <- 'symbolSize must be a vector of 3 integer values, at least 2 of them are different. Assigning default values 10, 4, 10.'
    expect_warning({cdfPlot(top_table, symbolSize = 'notavalidvalue')},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, symbolSize = 1)},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, symbolSize = c(1,2,3))},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, symbolSize = NULL)},
                   regexp = msg)

    #symbolShape
    msg <- "symbolShape must be a vector of 2 charcter values. Assigning default values 'circle'."
    expect_warning({cdfPlot(top_table, symbolShape = c('notavalidvalue','symbolColor'))},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, symbolShape = 1)},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, symbolShape = c('circle','circle','circle'))},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, symbolShape = NULL)},
                   regexp = msg)

    #symbolColor
    msg <- "symbolColor must be a vector of 2 character values. Assigning default values 'red3', 'deepskyblue4'."
    expect_warning({cdfPlot(top_table, symbolColor = c('notavalidvalue','symbolColor'))},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, symbolColor = 1)},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, symbolColor = c('red','red','red'))},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, symbolColor = NULL)},
                   regexp = msg)

    #transparency
    msg <- "transparency must be a singular value of class numeric and must be between 0 and 1. Assigning default value '0.7'."
    expect_warning({cdfPlot(top_table, transparency = NULL)},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, transparency = c(1,2))},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, transparency = -1)},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, transparency = "notavalidvalue")},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, transparency = 2)},
                   regexp = msg)

    #referenceLine
    msg <- "referenceLine must be a singular value of class character or 'NULL' to disable. Assigning default value NULL."
    expect_warning({cdfPlot(top_table, referenceLine = c("blue", "blue"))},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, referenceLine = 1)},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, referenceLine = "notavalidvalue")},
                   regexp = "Color specified is not valid. Assigning default value NULL.")

    #refLineThickness
    msg <- "refLineThickness must be a singular value of class numeric Assigning default value '1'."
    expect_warning({cdfPlot(top_table, refLineThickness = c(1,2))},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, refLineThickness = -1)},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, refLineThickness = NULL)},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, refLineThickness = "notavalidvalue")},
                   regexp = msg)

    #legendPosition
    msg <- "legendPosition must be one value from 'top', 'bottom', 'left', 'right', 'ne', 'se', 'nw', 'sw' or 'NULL' to disable. Assigning default value 'right'."
    expect_warning({cdfPlot(top_table, legendPosition = c('left','top'))},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, legendPosition = 1)},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, legendPosition = "notavalidvalue")},
                   regexp = msg)

    #viewportX
    msg <- "viewportX must be a singular value of class numeric and must be greater than 0. Assigning default value '0.15'."
    expect_warning({cdfPlot(top_table, viewportX = c(1,2))},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, viewportX = "notavalidvalue")},
                   regexp = msg)
    #expect_warning({cdfPlot(top_table, viewportX = NULL)},
                   #regexp = msg)

    #






















})
