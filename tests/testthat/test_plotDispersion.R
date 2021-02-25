context("DGEobj.plots - tests for plotDispersion.R functions")


test_that("plotDispersion.R: plotDispersion()", {
    skip_if(is.null(t_obj1$DGEList))

    # creating designMatrix and dgelist
    dgelist <- t_obj1$DGEList
    designMatrix <- stats::model.matrix(~ 0 + ReplicateGroup, getItem(t_obj1, "design"))

    # Testing dispersion plots with input DGEList
    plot_disp <- plotDispersion(DGEdata = dgelist,
                                designMatrix = designMatrix)
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    plot_disp <- plotDispersion(DGEdata = dgelist,
                                designMatrix = designMatrix,
                                plotType = "canvasXpress")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    plot_disp <- plotDispersion(DGEdata = dgelist,
                                designMatrix = designMatrix,
                                plotType = "ggplot")
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    # Testing dispersion plots with input countMatrix
    plot_disp <- plotDispersion(DGEdata       = t_obj1$counts,
                                designMatrix  = designMatrix)
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    plot_disp <- plotDispersion(DGEdata       = t_obj1$counts,
                                plotType      = "ggplot",
                                designMatrix  = designMatrix)
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    #Testing BCV plots with input DGEList
    plot_disp <- plotDispersion(DGEdata = dgelist,
                                designMatrix = designMatrix,
                                plotCategory  = "BCV")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    plot_disp <- plotDispersion(DGEdata = dgelist,
                                designMatrix = designMatrix,
                                plotCategory  = "BCV",
                                plotType = "ggplot")
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    #Testing BCV plots with input countMatrix
    plot_disp <- plotDispersion(DGEdata       = t_obj1$counts,
                                designMatrix  = designMatrix,
                                plotCategory  = "BCV")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    plot_disp <- plotDispersion(DGEdata       = t_obj1$counts,
                                plotType      = "ggplot",
                                designMatrix  = designMatrix,
                                plotCategory  = "BCV")
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    #Testing parameter - LineFit
    plot_disp <- plotDispersion(DGEdata = dgelist,
                                designMatrix = designMatrix,
                                lineFit = "loess")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    plot_disp <- plotDispersion(DGEdata = dgelist,
                                designMatrix = designMatrix,
                                lineFit = "loess",
                                plotType = "canvasXpress")
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))


    #Testing messages




    #Testing symbol parameters - canvasXpress
    plot_disp <- plotDispersion(DGEdata       = t_obj1$counts,
                                designMatrix  = designMatrix,
                                symbolSize    = 10,
                                symbolShape   = "triangle",
                                symbolcolor   = red)
    expect_s3_class(plot_disp, c("canvasXpress", "htmlwidget"))

    expect_message(plotDispersion(DGEdata = dgelist, designMatrix = designMatrix, symbolSize = "a"),
                   regexp = "symbolSize must be a singular numeric value. Assigning a default value of 3.")
    expect_message(plotDispersion(DGEdata = dgelist, designMatrix = designMatrix, symbolSize = c(1,2)),
                   regexp = "symbolSize must be a singular numeric value. Assigning a default value of 3.")
    expect_message(plotDispersion(DGEdata = dgelist, designMatrix = designMatrix, symbolShape = 1),
                   regexp = "symbolShape must be of class 'character'. Assigning default value = 'circle'")
    expect_message(plotDispersion(DGEdata = dgelist, designMatrix = designMatrix, symbolShape = c(1,2)),
                   regexp = "symbolShape must be of class 'character'. Assigning default value = 'circle'")
    expect_message(plotDispersion(DGEdata = dgelist, designMatrix = designMatrix, symbolColor = c(1,2)),
                   regexp = "symbolColor must be of class character and must specify the name of the
                                color or the rgb value. Assigning default value 'deepblue'.")
    expect_message(plotDispersion(DGEdata = dgelist, designMatrix = designMatrix, symbolColor = 1),
                   regexp = "symbolColor must be of class character and must specify the name of the
                                color or the rgb value. Assigning default value 'deepblue'.")



    #Testing symbol parameters = ggplot
    plot_disp <- plotDispersion(DGEdata       = t_obj1$counts,
                                plotType      = "ggplot",
                                designMatrix  = designMatrix,
                                plotCategory  = "BCV",
                                symbolSize    = 3,
                                symbolShape   = "square",
                                symbolColour  = "red" )
    expect_s3_class(plot_disp, c("gg", "ggplot"))

    expect_message(plotDispersion(DGEdata = dgelist, designMatrix = designMatrix, symbolShape = c(1,2), plotType = "ggplot"),
                   regexp = "symbolShape must be a singular numeric value between 0 and 25 or must
                    be of class 'character'. Refer help section for the list of shapes supported. Assigning default value 'circle'.")
    expect_message(plotDispersion(DGEdata = dgelist, designMatrix = designMatrix, symbolShape = 27, plotType = "ggplot"),
                   regexp = "symbolShape must be a singular numeric value between 0 and 25 or must
                    be of class 'character'. Refer help section for the list of shapes supported. Assigning default value 'circle'.")
    expect_message(plotDispersion(DGEdata = dgelist, designMatrix = designMatrix, symbolShape = c(1,2), plotType = "ggplot"),
                   regexp = "symbolShape must be a singular numeric value between 0 and 25 or must
                    be of class 'character'. Refer help section for the list of shapes supported. Assigning default value 'circle'.")
    expect_message(plotDispersion(DGEdata = dgelist, designMatrix = designMatrix, symbolShape = 'a', plotType = "ggplot"),
                   regexp = "symbolShape must be a singular numeric value between 0 and 25 or must
                    be of class 'character'. Refer help section for the list of shapes supported. Assigning default value 'circle'.")
    expect_message(plotDispersion(DGEdata = dgelist, designMatrix = designMatrix, symbolSize = 'a', plotType = "ggplot"),
                   regex = "symbolSize must be a singular numeric value. Assigning a default value of 3.")




    # Testing messages - linefit related parameters

    # testing assert statements
    expect_error(plotDispersion(),
                 regexp = "DGEdata must be specified.")
    expect_error(plotDispersion(DGEdata = t_obj1$counts),
                 regexp = "designMatrix must be specified and should be of class 'matrix'.")
    expect_error(plotDispersion(DGEdata = dgelist, designMatrix = designMatrix, plotCategory = "cx"),
                 regexp = "Plot value must be either dispersion or BCV.")
    expect_error(plotDispersion(DGEdata = dgelist, designMatrix = designMatrix, plotType = "cx"),
                 regexp = "Plot type must be either canvasXpress or ggplot.")
})
