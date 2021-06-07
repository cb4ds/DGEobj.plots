context("DGEobj.plots - tests for ggplotMDS.R functions")


test_that("obsPlot.R: obsPlot()", {
    obs_plot <- obsPlot(t_obj1,
                        countsMatrix = "counts",
                        convertCounts = NULL,
                        designTable   = "design",
                        group    = "replicategroup",
                        facetRow = 2)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #facet = FALSE
    obs_plot <- obsPlot(t_obj1,
                        designTable   = "design",
                        group = "replicategroup",
                        facet = FALSE)
    expect_length(obs_plot, length(unique(rownames(t_obj1))))
    expect_s3_class(obs_plot[[1]], c("canvasXpress", "htmlwidget"))

    obs_plot <- obsPlot(t_obj1,
                        designTable   = "design",
                        group    = "replicategroup",
                        plotType = "ggplot")
    expect_s3_class(obs_plot, c("gg", "ggplot"))

    obs_plot <- obsPlot(t_obj1,
                        designTable   = "design",
                        group = "replicategroup",
                        facet = FALSE,
                        plotType = "ggplot")
    expect_length(obs_plot, length(unique(rownames(t_obj1))))
    expect_s3_class(obs_plot[[1]], c("gg", "ggplot"))

    obs_plot <- obsPlot(t_obj1,
                        designTable = "design",
                        group       = "replicategroup",
                        boxLayer    = TRUE,
                        violinLayer = TRUE,
                        showPoints  = TRUE,
                        showMean    = TRUE,
                        xlab        = "value",
                        ylab        = "replicate_group",
                        title       = "obsPlot",
                        boxColor    = "deepskyblue3",
                        boxTransparency = 0.5,
                        violinColor = "goldenrod1",
                        violinTransparency = 0.5,
                        facet       = TRUE,
                        facetRow    = 3,
                        axisFree    = TRUE)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    obs_plot <- obsPlot(t_obj1,
                        designTable = "design",
                        group       = "replicategroup",
                        boxLayer    = TRUE,
                        violinLayer = TRUE,
                        showPoints  = FALSE,
                        showMean    = FALSE)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    obs_plot <- obsPlot(t_obj1,
                        designTable = "design",
                        group       = "replicategroup",
                        boxLayer    = FALSE,
                        violinLayer = TRUE)

    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    obs_plot <- obsPlot(t_obj1,
                        plotType    = "ggplot",
                        designTable = "design",
                        group       = "replicategroup",
                        boxLayer    = TRUE,
                        violinLayer = TRUE,
                        showPoints  = FALSE,
                        showMean    = FALSE)
    expect_s3_class(obs_plot, c("gg", "ggplot"))

    obs_plot <- obsPlot(t_obj1,
                        plotType    = "ggplot",
                        group       = "replicategroup",
                        designTable = "design",
                        boxLayer    = FALSE,
                        violinLayer = TRUE)
    expect_s3_class(obs_plot, c("gg", "ggplot"))

    obs_plot <- obsPlot(t_obj1,
                        plotType    = "ggplot",
                        designTable = "design",
                        group       = "replicategroup",
                        boxLayer    = TRUE,
                        violinLayer = TRUE,
                        showPoints  = FALSE,
                        showMean    = FALSE)
    expect_s3_class(obs_plot, c("gg", "ggplot"))

    obs_plot <- obsPlot(t_obj1,
                        designTable = "design",
                        plotType    = "ggplot",
                        group       = "replicategroup",
                        boxLayer    = TRUE,
                        violinLayer = TRUE,
                        showPoints  = TRUE,
                        showMean    = TRUE,
                        xlab        = "value",
                        ylab        = "replicate_group",
                        title       = "obsPlot",
                        boxColor    = "deepskyblue3",
                        boxTransparency = 0.5,
                        violinColor = "goldenrod1",
                        violinTransparency = 0.5,
                        facet       = TRUE,
                        facetRow    = 3,
                        labelAngle  = 30,
                        axisFree    = FALSE)
    expect_s3_class(obs_plot, c("gg", "ggplot"))

    obs_plot <- obsPlot(t_obj1,
                        plotType    = "ggplot",
                        designTable = "design",
                        group       = "replicategroup",
                        boxLayer    = FALSE,
                        violinLayer = FALSE,
                        showPoints  = TRUE,
                        showMean    = TRUE)
    expect_s3_class(obs_plot, c("gg", "ggplot"))

    # testing assert statements
    msg <- "DGEdata must be specified and should be of class DGEobj"
    expect_error(obsPlot(DGEdata = 1:10),
                 regexp = msg)
    expect_error(obsPlot(),
                 regexp = msg)

    obj_no_counts <- t_obj1
    obj_no_counts$counts <- NULL
    expect_error(obsPlot(obj_no_counts,
                         designTable = "design",
                         group       = "replicategroup"),
                 msg = "counts matrix must be available in DGEdata to plot the data.")

    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group = "notavalidname"),
                 msg = "group must be specified and should be one of the columns in the design object in DGEdata. Assigning replicategroup as the default value.")
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    obj_no_rep_group <- t_obj1
    obj_no_rep_group$design$ReplicateGroup <- NULL
    expect_error(obsPlot(obj_no_rep_group,
                         designTable = "design",
                         group = "notavalidgroup"),
                 msg = "group must be specified and should be one of the columns in the designTable in DGEdata.")

    obj_no_design        <- t_obj1
    obj_no_design$design <- NULL
    expect_error(obsPlot(obj_no_design,
                         designTable = "design",
                         group = "replicategroup"),
                 msg = "designTable specified is not present in DGEobj.")

    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "notavalidname",
                                       group = "replicategroup"),
                   msg = "designTable specified is not present in DGEobj. Assigning default value 'design'.")
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    obj_no_rep_group <- t_obj1
    obj_no_rep_group$design$ReplicateGroup <- NULL
    expect_error(obsPlot(obj_no_rep_group,
                         designTable = "design",
                         group = "replicateGroup"),
                 msg = "group must be specified and should be one of the columns in the designTable in DGEdata.")

    msg <- "plotType must be either canvasXpress or ggplot."
    expect_error(obsPlot(t_obj1,
                         designTable = "design",
                         group    = "replicategroup",
                         plotType = "cx"),
                 regexp = msg)
    expect_error(obsPlot(t_obj1,
                         designTable = "design",
                         group    = "replicategroup",
                         plotType = NULL),
                 regexp = msg)

    #Testing optional parameters
    #facet
    msg <- "facet must be a singular logical value. Assigning default value TRUE."
    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group = "replicateGroup",
                                       facet = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group = "replicateGroup",
                                       facet = c(1,2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group = "replicateGroup",
                                       facet = "abc"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #facet row
    msg <-  paste("facetRow needs a singular numeric value lesser than the total number of unique column by which the plot is segregated.",
                  "Assigning default value.")
    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group    = "replicateGroup",
                                       facetRow = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group    = "replicateGroup",
                                       facetRow = c(1,2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group    = "replicateGroup",
                                       facetRow = "abc"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group    = "replicateGroup",
                                       facetRow = nrow(unique(rownames(t_obj1))) + 2),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #title
    msg <- "Invalid title specificed. Title must be singular value of class character."
    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group = "replicateGroup",
                                       title = 1),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group = "replicateGroup",
                                       title = c("title1", "title2")),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    obs_plot <- obsPlot(t_obj1,
                        designTable = "design",
                        group = "replicateGroup",
                        title = NULL)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    obs_plot <- obsPlot(t_obj1,
                        plotType = "ggplot",
                        designTable = "design",
                        group    = "replicateGroup",
                        title    = NULL)
    expect_s3_class(obs_plot, c("gg", "ggplot"))


    #xlab
    msg <- "xlab value specified is not valid. Assigning groupCol name as the default value."
    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group = "replicateGroup",
                                       xlab  = 1),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group = "replicateGroup",
                                       xlab  = c("xlab1", "xlab2")),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    obs_plot <- obsPlot(t_obj1,
                        designTable = "design",
                        group = "replicateGroup",
                        xlab  = NULL)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    obs_plot <- obsPlot(t_obj1,
                        designTable = "design",
                        plotType = "ggplot",
                        group    = "replicateGroup",
                        xlab     = NULL)
    expect_s3_class(obs_plot, c("gg", "ggplot"))

    #ylab
    msg <- "ylab value specified is not valid. Assigning valueCol name as the default value."
    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group = "replicateGroup",
                                       ylab  = 1),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group = "replicateGroup",
                                       ylab  = c("ylab1", "ylab2")),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    obs_plot <- obsPlot(t_obj1,
                        designTable = "design",
                        group = "replicateGroup",
                        ylab  = NULL)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    obs_plot <- obsPlot(t_obj1,
                        plotType = "ggplot",
                        designTable = "design",
                        group    = "replicateGroup",
                        ylab     = NULL)
    expect_s3_class(obs_plot, c("gg", "ggplot"))

    #labelAngle
    msg <- "labelAngle must be a single numeric value greater than 0. Assigning default value 30."
    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       group      = "replicateGroup",
                                       designTable = "design",
                                       labelAngle = "abc"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group      = "replicateGroup",
                                       labelAngle = c(1, 2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group       = "replicateGroup",
                                       boxLayer    = FALSE,
                                       violinLayer = FALSE),
                   regexp = "Either one of boxLayer and violinLayer must be TRUE. Assigning boxLayer as TRUE.")
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #boxlayer
    msg <- "boxLayer must be a single logical value. Assigning default value TRUE."
    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group    = "replicateGroup",
                                       boxLayer = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group    = "replicateGroup",
                                       boxLayer = c(1,2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group    = "replicateGroup",
                                       boxLayer = "abc"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #boxTransparency
    msg <- "boxTransparency must be a singular numeric value and must be between 0 and 1. Assigning default value 0.5."
    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group = "replicateGroup",
                                       boxTransparency = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group = "replicateGroup",
                                       boxTransparency = "abc"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group = "replicateGroup",
                                       boxTransparency = c(1,2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group = "replicateGroup",
                                       boxTransparency = -2),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group = "replicateGroup",
                                       boxTransparency = 2),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #boxColor
    msg <- "boxColor must be of class character and must specify the name of the color or the rgb value. Assigning default value 'deepskyblue3'."
    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group    = "replicateGroup",
                                       boxColor = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group    = "replicateGroup",
                                       boxColor = c("red","blue")),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group    = "replicateGroup",
                                       boxColor = 1),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group    = "replicateGroup",
                                       boxColor = "notavalidcolor"),
                   regexp = "boxColor specified is not valid. Assigning default value 'deepskyblue3'.")
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))


    #Violinplot Validations

    #violinLayer
    msg <- "violinLayer must be a singular logical value. Assigning default value FALSE."
    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group       = "replicateGroup",
                                       violinLayer = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group       = "replicateGroup",
                                       violinLayer = c(1,2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group       = "replicateGroup",
                                       violinLayer = "abc"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #Violin Transparency
    msg <- "violinTransparency must be a singular numeric value and must be between 0 and 1. Assigning default value 0.5."
    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group       = "replicateGroup",
                                       violinLayer = TRUE,
                                       violinTransparency = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group       = "replicateGroup",
                                       violinLayer = TRUE,
                                       violinTransparency = "abc"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group       = "replicateGroup",
                                       violinLayer = TRUE,
                                       violinTransparency = c(1,2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group       = "replicateGroup",
                                       violinLayer = TRUE,
                                       violinTransparency = -2),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group       = "replicateGroup",
                                       violinLayer = TRUE,
                                       violinTransparency = 2),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #violinColor
    msg <- "violinColor must be of class character and must specify the name of the color or the rgb value. Assigning default value 'goldenrod1'."
    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group       = "replicateGroup",
                                       violinLayer = TRUE,
                                       violinColor = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group       = "replicateGroup",
                                       violinLayer = TRUE,
                                       violinColor = c("red","blue")),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                      group        = "replicateGroup",
                                       violinLayer = TRUE,
                                       violinColor = 1),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group       = "replicateGroup",
                                       violinLayer = TRUE,
                                       violinColor = "notavalidcolor"),
                   regexp = "violinColor specified is not valid. Assigning default value 'goldenrod1'.")
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #showMean
    msg <- "showMean must be a singular logical value. Assigning default value TRUE."
    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group     = "replicateGroup",
                                       showMean  = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group     = "replicateGroup",
                                       showMean  = c(1,2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group     = "replicateGroup",
                                       showMean  = "abc"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #showPoints
    msg <- "showPoints must be a singular logical value. Assigning default value TRUE."
    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group      = "replicateGroup",
                                       showPoints = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group      = "replicateGroup",
                                       showPoints = c(1,2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group      = "replicateGroup",
                                       showPoints = "abc"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #axisFree
    msg <- "axisFree must be a singular logical value. Assigning default value TRUE."
    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group      = "replicateGroup",
                                       axisFree   = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group      = "replicateGroup",
                                       axisFree   = c(1,2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(t_obj1,
                                       designTable = "design",
                                       group      = "replicateGroup",
                                       axisFree   = "abc"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

})
