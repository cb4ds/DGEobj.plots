context("DGEobj.plots - tests for plotPValHist.R functions")


test_that("plotPValHist.R: plotPvalHist()", {
    skip_if(is.null(t_obj1$DGEList))

    # testing plotPvalHist with facet = TRUE
    pvalMatrix <- extractCol(getType(t_obj1, "topTable"), colName = "P.Value", robust = FALSE)
    pval_plot <- plotPvalHist(pvalMatrix)
    expect_s3_class(pval_plot, c("canvasXpress", "htmlwidget"))

    pval_plot <- plotPvalHist(pvalMatrix, plotType = "ggplot")
    expect_s3_class(pval_plot, c("gg","ggplot"))

    # testing plotPvalHist with facet = FALSE
    pval_plot <- plotPvalHist(as.matrix(pvalMatrix),
                              facet     = FALSE)
    expect_length(pval_plot, 4)
    expect_s3_class(pval_plot[[1]], c("canvasXpress", "htmlwidget"))

    pval_plot <- plotPvalHist(as.matrix(pvalMatrix),
                              plotType = "ggplot",
                              facet    = FALSE)
    expect_length(pval_plot, 4)
    expect_s3_class(pval_plot[[1]], c("gg","ggplot"))

    expect_error(plotPvalHist(pvalMatrix, plotType = "cx"),
                 regexp = "Plot type must be either canvasXpress or ggplot.")

    #testing optional parameter
    pval_plot <- plotPvalHist(pvalMatrix, binWidth = 0.2, color = "red", transparency = 0.2)
    expect_s3_class(pval_plot, c("canvasXpress", "htmlwidget"))

    pval_plot <- plotPvalHist(pvalMatrix, plotType = "ggplot", binWidth = 0.2, color = "red", transparency = 0.2)
    expect_s3_class(pval_plot, c("gg","ggplot"))

    #testing invalid optional parameters

    #binWidth
    expect_warning(pval_plot <- plotPvalHist(pvalMatrix, binWidth = 1.2),
                   regexp = "binWidth must be a singular numeric value between 0 & 1. Assigning default value 0.02.")
    expect_s3_class(pval_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(pval_plot <- plotPvalHist(pvalMatrix, binWidth = c(1,2)),
                   regexp = "binWidth must be a singular numeric value between 0 & 1. Assigning default value 0.02.")
    expect_s3_class(pval_plot, c("canvasXpress", "htmlwidget"))

    #color
    expect_warning(pval_plot <- plotPvalHist(pvalMatrix, color = c(1,2)),
                   regexp = "color must be a singular value of class character and must specify the name of the color or the rgb value. Assigning default value 'dodgerblue3'.")
    expect_s3_class(pval_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(pval_plot <- plotPvalHist(pvalMatrix, color = 1),
                   regexp = "color must be a singular value of class character and must specify the name of the color or the rgb value. Assigning default value 'dodgerblue3'.")
    expect_s3_class(pval_plot, c("canvasXpress", "htmlwidget"))

    #facet
    expect_warning(pval_plot <- plotPvalHist(pvalMatrix, facet = "true"),
                   regexp = "facet must be a singular logical value. Assigning default value TRUE.")
    expect_s3_class(pval_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(pval_plot <- plotPvalHist(pvalMatrix, facet = c(TRUE,TRUE)),
                    regexp = "facet must be a singular logical value. Assigning default value TRUE.")
    expect_s3_class(pval_plot, c("canvasXpress", "htmlwidget"))

    #transparency
    expect_warning(pval_plot <- plotPvalHist(pvalMatrix, transparency = 1.2),
                   regexp = "Transparency must be a singular numeric value and must be between 0 and 1. Assigning default value 0.6.")
    expect_s3_class(pval_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(pval_plot <- plotPvalHist(pvalMatrix, transparency = c(1,2)),
                   regexp = "Transparency must be a singular numeric value and must be between 0 and 1. Assigning default value 0.6.")
    expect_s3_class(pval_plot, c("canvasXpress", "htmlwidget"))
})
