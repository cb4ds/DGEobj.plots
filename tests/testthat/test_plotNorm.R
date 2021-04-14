context("DGEobj.plots - tests for plotNorm.R functions")


test_that("plotNorm.R: plotNorm()", {
    ######### box test####################
    # testing with DGEobj count matrix and plotType cx - plotCategory box
    norm_plot <- plotNorm(t_obj1$counts)
    expect_s3_class(norm_plot, c("canvasXpress", "htmlwidget"))

    # testing with DGEobj count matrix and plotType ggplot - plotCategory box
    norm_plot <- plotNorm(t_obj1$counts, plotType = "ggplot")
    expect_s3_class(norm_plot, c("gg", "ggplot"))

    # testing with DGEobj object and plotType cx - plotCategory box
    norm_plot <- plotNorm(t_obj1)
    expect_s3_class(norm_plot, c("canvasXpress", "htmlwidget"))

    # testing with DGEobj object and plotType ggplot - plotCategory box
    norm_plot <- plotNorm(t_obj1, plotType = "ggplot")
    expect_s3_class(norm_plot, c("gg", "ggplot"))

    # testing with different normalization methods
    norm_plot <- plotNorm(t_obj1$counts, normalize = "RLE")
    expect_s3_class(norm_plot, c("canvasXpress", "htmlwidget"))

    norm_plot <- plotNorm(t_obj1, plotType = "ggplot", normalize = "RLE")
    expect_s3_class(norm_plot, c("gg", "ggplot"))

    norm_plot <- plotNorm(t_obj1$counts, normalize = "upperquartile")
    expect_s3_class(norm_plot, c("canvasXpress", "htmlwidget"))

    norm_plot <- plotNorm(t_obj1, plotType = "ggplot", normalize = "upperquartile")
    expect_s3_class(norm_plot, c("gg", "ggplot"))

    norm_plot <- plotNorm(t_obj1$counts, normalize = "none")
    expect_s3_class(norm_plot, c("canvasXpress", "htmlwidget"))

    norm_plot <- plotNorm(t_obj1, plotType = "ggplot", normalize = "none")
    expect_s3_class(norm_plot, c("gg", "ggplot"))

    ######### density test####################
    # testing with DGEobj count matrix and plotType cx - plotCategory density
    norm_plot <- plotNorm(t_obj1$counts,  plotCategory = "density")
    expect_s3_class(norm_plot, c("canvasXpress", "htmlwidget"))

    # testing with DGEobj count matrix and plotType ggplot - plotCategory density
    norm_plot <- plotNorm(t_obj1$counts, plotType = "ggplot", plotCategory = "density")
    expect_s3_class(norm_plot, c("gg", "ggplot"))

    # testing with DGEobj object and plotType cx - plotCategory density
    norm_plot <- plotNorm(t_obj1,  plotCategory = "density")
    expect_s3_class(norm_plot, c("canvasXpress", "htmlwidget"))

    # testing with DGEobj object and plotType ggplot - plotCategory density
    norm_plot <- plotNorm(t_obj1, plotType = "ggplot",  plotCategory = "density")
    expect_s3_class(norm_plot, c("gg", "ggplot"))

    # testing with different normalization methods
    norm_plot <- plotNorm(t_obj1$counts,  plotCategory = "density", normalize = "RLE")
    expect_s3_class(norm_plot, c("canvasXpress", "htmlwidget"))

    norm_plot <- plotNorm(t_obj1$counts, plotType = "ggplot", plotCategory = "density",
                          normalize = "RLE")
    expect_s3_class(norm_plot, c("gg", "ggplot"))

    norm_plot <- plotNorm(t_obj1$counts,  plotCategory = "density", normalize = "upperquartile")
    expect_s3_class(norm_plot, c("canvasXpress", "htmlwidget"))

    norm_plot <- plotNorm(t_obj1$counts, plotType = "ggplot", plotCategory = "density",
                          normalize = "upperquartile")
    expect_s3_class(norm_plot, c("gg", "ggplot"))

    norm_plot <- plotNorm(t_obj1$counts,  plotCategory = "density", normalize = "none")
    expect_s3_class(norm_plot, c("canvasXpress", "htmlwidget"))

    norm_plot <- plotNorm(t_obj1$counts, plotType = "ggplot", plotCategory = "density",
                          normalize = "none")
    expect_s3_class(norm_plot, c("gg", "ggplot"))

    ######### testing assert statements ####################
    ## DGEdata
    expect_error(plotNorm(),
                 regexp = "DGEdata must be of either class 'matrix' or 'DGEobj'.")
    expect_error(plotNorm(NULL),
                 regexp = "DGEdata must be of either class 'matrix' or 'DGEobj'.")
    expect_error(plotNorm(t_obj1$BDL_vs_Sham),
                 regexp = "DGEdata must be of either class 'matrix' or 'DGEobj'.")
    ## plotCategory
    msg <- "plotCategory must be one of 'box' or 'density'. Setting default value 'box'"
    expect_warning(plotNorm(t_obj1, plotCategory = NULL),
                   regexp = msg)
    expect_warning(plotNorm(t_obj1, plotCategory = "xyz"),
                   regexp = msg)
    expect_warning(plotNorm(t_obj1, plotCategory = c("box", "box")),
                   regexp = msg)
    ## normalize
    msg <- "normalize must be one of 'TMM', 'RLE', 'upperquartile', or 'none'. Setting default value 'tmm'"
    expect_warning(plotNorm(t_obj1, normalize = NULL),
                 regexp = msg)
    expect_warning(plotNorm(t_obj1, normalize = "xyz"),
                 regexp = msg)
    expect_warning(plotNorm(t_obj1, normalize = c("TMM", "RLE")),
                 regexp = msg)
    ## plotType
    msg <- "plotType must be either canvasXpress or ggplot."
    expect_error(plotNorm(t_obj1, plotType = "myplot"),
                 regexp = msg)
    expect_error(plotNorm(t_obj1, plotType = NULL),
                 regexp = msg)
    expect_error(plotNorm(t_obj1, plotType = c("canvasXpress", "ggplot")),
                 regexp = msg)
})
