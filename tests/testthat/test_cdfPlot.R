context('DGEobj.plots - tests for cdfPlot.R functions')


test_that('cdfPlot.R: cdfPlot()', {
    skip_if(!('ReplicateGroupDesign_fit' %in% names(t_obj1)))

    top_table <- topTable(t_obj1$ReplicateGroupDesign_fit, number = 100)

    # testing plot with default values.
    plot <- cdfPlot(top_table, referenceLine = 'blue')
    expect_type(plot, 'list')
    expect_s3_class(plot$main, c('canvasXpress', 'htmlwidget'))
    expect_s3_class(plot$inset, c('canvasXpress', 'htmlwidget'))

    plot <- cdfPlot(top_table, plotType = 'ggplot', referenceLine = 'blue')
    expect_s3_class(plot$main, c('gg', 'ggplot'))
    expect_s3_class(plot$inset, c('gg', 'ggplot'))
    expect_s3_class(plot$combined, c('gg', 'ggplot'))


    #testing plot with optional parameters
    plot <- cdfPlot(top_table,
                    referenceLine = "blue",
                    pThreshold = 4.114957e-97,
                    pvalMax = 5.77e-96,
                    symbolSize     = c(30, 20),
                    symbolShape    = c('circle', 'square'),
                    symbolColor    = c('green', 'deepskyblue4'),
                    transparency   = 1.0,
                    refLineThickness = 2)
    expect_type(plot, 'list')
    expect_s3_class(plot$main, c('canvasXpress', 'htmlwidget'))
    expect_s3_class(plot$inset, c('canvasXpress', 'htmlwidget'))


    plot <- cdfPlot(top_table,
                    referenceLine = "blue",
                    pThreshold = 4.114957e-97,
                    pvalMax = 5.77e-96,
                    symbolSize     = c(3, 2),
                    symbolShape    = c('circle', 'square'),
                    symbolColor    = c('green', 'deepskyblue4'),
                    transparency   = 1.0,
                    refLineThickness = 2,
                    plotType = "ggplot")
    expect_type(plot, 'list')
    expect_s3_class(plot$main, c('gg', 'ggplot'))
    expect_s3_class(plot$inset, c('gg', 'ggplot'))
    expect_s3_class(plot$combined, c('gg', 'ggplot'))

    # testing plot with customized aesthetics.
    plot_with_aes <- cdfPlot(top_table,
                             insetTitle    = 'Sub plot title',
                             xlab          = 'xaxis-title',
                             ylab          = 'yaxis-title',
                             title         = 'MyPlot',
                             referenceLine = 'blue',
                             footnote      = 'this is footnote of the plot')

    expect_type(plot_with_aes, 'list')
    expect_s3_class(plot_with_aes$main, c('canvasXpress', 'htmlwidget'))
    expect_s3_class(plot_with_aes$inset, c('canvasXpress', 'htmlwidget'))

    plot_with_aes <- cdfPlot(top_table,
                             plotType      = 'ggplot',
                             insetTitle    = 'Sub plot title',
                             xlab          = 'xaxis-title',
                             ylab          = 'yaxis-title',
                             title         = 'MyPlot',
                             referenceLine = 'blue',
                             footnote      = 'this is footnote of the plot')
    expect_type(plot_with_aes, 'list')
    expect_s3_class(plot_with_aes$main, c('gg', 'ggplot'))
    expect_s3_class(plot_with_aes$inset, c('gg', 'ggplot'))
    expect_s3_class(plot_with_aes$combined, c('gg', 'ggplot'))

    expect_setequal(unlist(plot_with_aes$main$labels[c('title', 'y', 'x')]), c('MyPlot', 'yaxis-title', 'xaxis-title'))
    expect_setequal(plot_with_aes$inset$labels$title, 'Sub plot title')
    expect_equal(plot_with_aes$main$layers[[2]]$geom_params$colour, 'blue')
    expect_equal(plot_with_aes$main$layers[[3]]$geom_params$label, 'this is footnote of the plot')

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
    expect_error({cdfPlot(top_table, plotType = 'cx')},
                 regexp = msg)
    expect_error({cdfPlot(top_table, plotType = NULL)},
                 regexp = msg)
    expect_error({cdfPlot(top_table, plotType = 1)},
                 regexp = msg)
    expect_error({cdfPlot(top_table, plotType = c('canvasxpress','ggplot'))},
                 regexp = msg)

    #pvalCol
    msg <- 'pvalCol column not found in contrastDF.'
    expect_error({cdfPlot(top_table, pvalCol = 'notacolumn')},
                 regexp = msg)
    expect_error({cdfPlot(top_table, pvalCol = NULL)},
                 regexp = msg)

    #pThreshold
    msg <- 'pthreshold must be a singular numeric value. Assigning default value 0.01.'
    expect_warning({cdfPlot(top_table, pThreshold = 'notavalue')},
                 regexp = msg)
    expect_warning({cdfPlot(top_table, pThreshold = NULL)},
                 regexp = msg)
    expect_warning({cdfPlot(top_table, pThreshold = c(1,2))},
                 regexp = msg)
    expect_warning({cdfPlot(top_table, pThreshold = 'notavalue')},
                 regexp = msg)

    #title
    msg <- 'title must be a singular value of class character. Assigning default value NULL.'
    expect_warning({cdfPlot(top_table, title = c('title','title'))},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, title = 1)},
                   regexp = msg)

    #insetTitle
    msg <- 'insetTitle must be a singular value of class character. Assigning default value NULL.'
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
    msg <- "ylab must be a singular value of class character. Assigning default value 'pvalCol' as the label."
    expect_warning({cdfPlot(top_table, ylab = c('ylab','ylab'))},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, ylab = 1)},
                   regexp = msg)

    #symbolSize
    msg <- 'symbolSize must be a vector of 2 integer values. Assigning default values 2,1.'
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
    msg <- 'transparency must be a singular value of class numeric and must be between 0 and 1. Assigning default value 0.7.'
    expect_warning({cdfPlot(top_table, transparency = NULL)},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, transparency = c(1,2))},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, transparency = -1)},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, transparency = 'notavalidvalue')},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, transparency = 2)},
                   regexp = msg)

    #referenceLine
    msg <- 'referenceLine must be a singular value of class character or NULL to disable. Assigning default value NULL.'
    expect_warning({cdfPlot(top_table, referenceLine = c('blue', 'blue'))},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, referenceLine = 1)},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, referenceLine = 'notavalidvalue')},
                   regexp = 'Color specified is not valid. Assigning default value NULL.')

    #refLineThickness
    msg <- 'refLineThickness must be a singular value of class numeric Assigning default value 1.'
    expect_warning({cdfPlot(top_table, refLineThickness = c(1,2))},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, refLineThickness = -1)},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, refLineThickness = NULL)},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, refLineThickness = 'notavalidvalue')},
                   regexp = msg)

    #viewportX
    msg <- 'viewportX must be a singular value of class numeric and must be greater than 0. Assigning default value 0.15.'
    expect_warning({cdfPlot(top_table, viewportX = c(1,2))},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, viewportX = 'notavalidvalue')},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, viewportX = NULL)},
                    regexp = msg)

    #viewportY
    msg <- 'viewportY must be a singular value of class numeric and must be greater than 0. Assigning default value 0.85.'
    expect_warning({cdfPlot(top_table, viewportY = c(1,2))},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, viewportY = 'notavalidvalue')},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, viewportY = NULL)},
                   regexp = msg)

    #viewportWidth
    msg <- 'viewportWidth must be a singular value of class numeric. Assigning default value 0.35.'
    expect_warning({cdfPlot(top_table, viewportWidth = c(1,2))},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, viewportWidth = 'notavalidvalue')},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, viewportWidth = NULL)},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, viewportWidth = -2)},
                   regexp = msg)

    #pvalMax
    msg <- 'pvalMax must be a singular numeric value. Assigning default value 0.1.'
    expect_warning({cdfPlot(top_table, pvalMax = c(1,2))},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, pvalMax = NULL)},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, pvalMax = 'notavalidvalue')},
                   regexp = msg)

    #footnote
    msg <- 'footnote must be a singular value of class character or NULL to disable. Assigning default value NULL.'
    expect_warning({cdfPlot(top_table, footnote = 1)},
                   regexp = msg)
    expect_warning({cdfPlot(top_table, footnote = c('footnote','footnote'))},
                   regexp = msg)
})
