context("DGEobj.plots - tests for ggplotMDS.R functions")


test_that("obsPlot.R: obsPlot()", {
    data   <- convertCounts(t_obj1$counts, unit = "cpm", log = TRUE, normalize = "tmm")
    design <- getItem(t_obj1,"design")
    replicategroup <- design[,"ReplicateGroup",drop = FALSE]

    obs_plot <- obsPlot(data,
                        group = replicategroup)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #facet = FALSE
    obs_plot <- obsPlot(data,
                        group = replicategroup,
                        facet = FALSE)
    expect_length(obs_plot, length(unique(rownames(data))))
    expect_s3_class(obs_plot[[1]], c("canvasXpress", "htmlwidget"))

    obs_plot <- obsPlot(data,
                        group = replicategroup,
                        plotType = "ggplot")
    expect_s3_class(obs_plot, c("gg", "ggplot"))

    obs_plot <- obsPlot(data,
                        group = replicategroup,
                        facet = FALSE,
                        plotType = "ggplot")
    expect_length(obs_plot, length(unique(rownames(data))))
    expect_s3_class(obs_plot[[1]], c("gg", "ggplot"))

    obs_plot <- obsPlot(data,
                        group       = replicategroup,
                        boxLayer    = TRUE,
                        violinLayer = TRUE,
                        pointLayer  = TRUE,
                        meanLayer   = TRUE,
                        xlab     = "value",
                        ylab     = "replicate_group",
                        title    = "obsPlot",
                        boxColor = "deepskyblue3",
                        boxTransparency = 0.5,
                        boxNotch    = FALSE,
                        violinColor = "goldenrod1",
                        violinTransparency = 0.5,
                        pointColor  = "dodgerblue4",
                        pointShape  = "circle",
                        pointTransparency = 1,
                        pointSize   = 2,
                        pointJitter = 0.1,
                        meanColor   = "goldenrod",
                        meanShape   = "square",
                        meanTransparency = 0.7,
                        meanSize = 3,
                        facet    = TRUE,
                        facetRow = 3)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))



obs_plot <- obsPlot(data,
                    plotType    = "ggplot",
                    group       = replicategroup,
                    #groupOrder = unique(as.character(data[,groupCol, drop = TRUE])),
                    boxLayer    = TRUE,
                    violinLayer = TRUE,
                    pointLayer  = TRUE,
                    meanLayer   = TRUE,
                    xlab     = "value",
                    ylab     = "replicate_group",
                    title    = "obsPlot",
                    boxColor = "deepskyblue3",
                    boxTransparency = 0.5,
                    boxNotch = FALSE,
                    boxNotchWidth = 0.8,
                    violinColor   = "goldenrod1",
                    violinTransparency = 0.5,
                    pointColor = "dodgerblue4",
                    pointShape = "circle",
                    pointTransparency = 1,
                    pointSize   = 1,
                    pointJitter = 0.1,
                    meanColor   = "goldenrod",
                    #meanOutlineColor = "red",
                    meanShape = "square",
                    meanTransparency = 0.7,
                    meanSize  = 3,
                    facet     = TRUE,
                    facetRow  = 3,
                    xAngle    = 30,
                    scales    = "free")


    # testing assert statements
    msg <- "data must be specified and should be of class 'data.frame'."
    expect_error(obsPlot(data = 1:10),
                 regexp = msg)
    expect_error(obsPlot(),
                 regexp = msg)

    msg <- "Plot type must be either canvasXpress or ggplot."
    expect_error(obsPlot(data,
                         group    = replicategroup,
                         plotType = "cx"),
                 regexp = msg)
    expect_error(obsPlot(data,
                         group    = replicategroup,
                         plotType = NULL),
                 regexp = msg)

   msg <- "group must be specified and the rownames of the group must be the same as column names of intensityObj"
   expect_error(obsPlot(data,
                        group = 1:10),
                regexp = msg)
   expect_error(obsPlot(data,
                        group = NULL),
                regexp = msg)
   expect_error(obsPlot(data,
                        group = design$ReplicateGroup),
                regexp = msg)
   expect_error(obsPlot(data),
                regexp = msg)
   expect_error(obsPlot(data,
                        group = replicategroup[1:20,]),
                regexp = msg)
   expect_error(obsPlot(data,
                        group = replicategroup[1:20,]),
                regexp = msg)

    #Testing optional parameters

    #facet
    msg <- "facet must be a singular logical value. Assigning default value TRUE."
    expect_warning(obs_plot <- obsPlot(data,
                                       group = replicategroup,
                                       facet = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group = replicategroup,
                                       facet = c(1,2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group = replicategroup,
                                       facet = "abc"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #facet row
    msg <- paste("facetRow needs a singular numeric value lesser  than the total number of unique column by which the plot is segregated.",
                 "Assigning default value.")
    expect_warning(obs_plot <- obsPlot(data,
                                       group    = replicategroup,
                                       facetRow = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group    = replicategroup,
                                       facetRow = c(1,2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group    = replicategroup,
                                       facetRow = "abc"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group    = replicategroup,
                                       facetRow = length(unique(rownames(data))) + 2),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #title
    msg <- 'Invalid title specificed. Title must be singular value of class character.'
    expect_warning(obs_plot <- obsPlot(data,
                                       group = replicategroup,
                                       title = 1),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group = replicategroup,
                                       title = c("title1", "title2")),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    obs_plot <- obsPlot(data,
                        group = replicategroup,
                        title = NULL)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    obs_plot <- obsPlot(data,
                        plotType = "ggplot",
                        group = replicategroup,
                        title = NULL)
    expect_s3_class(obs_plot, c("gg", "ggplot"))


    #xlab
    msg <- 'xlab value specified is not valid. Assigning groupCol name as the default value.'
    expect_warning(obs_plot <- obsPlot(data,
                                       group = replicategroup,
                                       xlab  = 1),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group = replicategroup,
                                       xlab  = c("xlab1", "xlab2")),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    obs_plot <- obsPlot(data,
                        group = replicategroup,
                        xlab  = NULL)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    obs_plot <- obsPlot(data,
                        plotType = "ggplot",
                        group    = replicategroup,
                        xlab     = NULL)
    expect_s3_class(obs_plot, c("gg", "ggplot"))



    #ylab
    msg <- 'ylab value specified is not valid. Assigning valueCol name as the default value.'
    expect_warning(obs_plot <- obsPlot(data,
                                       group = replicategroup,
                                       ylab  = 1),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group = replicategroup,
                                       ylab  = c("ylab1", "ylab2")),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    obs_plot <- obsPlot(data,
                        group = replicategroup,
                        ylab  = NULL)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    obs_plot <- obsPlot(data,
                        plotType = "ggplot",
                        group    = replicategroup,
                        ylab     = NULL)
    expect_s3_class(obs_plot, c("gg", "ggplot"))

    #xAngle
    msg <- "xAngle must be a single numeric value greater than 0. Assigning default value 30."
    expect_warning(obs_plot <- obsPlot(data,
                                       group  = replicategroup,
                                       xAngle = "abc"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group  = replicategroup,
                                       xAngle = c(1, 2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))


    #boxplot validations

    #boxlayer
    msg <- "boxLayer must be a single logical value. Assigning default value 'TRUE'."
    expect_warning(obs_plot <- obsPlot(data,
                                       group    = replicategroup,
                                       boxLayer = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group    = replicategroup,
                                       boxLayer = c(1,2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group    = replicategroup,
                                       boxLayer = "abc"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #boxTransparency
    msg <- "boxTransparency must be a singular numeric value and must be between 0 and 1. Assigning default value 0.5."
    expect_warning(obs_plot <- obsPlot(data,
                                       group = replicategroup,
                                       boxTransparency = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group = replicategroup,
                                       boxTransparency = "abc"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group = replicategroup,
                                       boxTransparency = c(1,2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group = replicategroup,
                                       boxTransparency = -2),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group = replicategroup,
                                       boxTransparency = 2),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #boxColor
    msg <- "boxColor must be of class character and must specify the name of the color or the rgb value. Assigning default value 'deepskyblue3'."
    expect_warning(obs_plot <- obsPlot(data,
                                       group    = replicategroup,
                                       boxColor = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group    = replicategroup,
                                       boxColor = c("red","blue")),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group    = replicategroup,
                                       boxColor = 1),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group    = replicategroup,
                                       boxColor = "notavalidcolor"),
                   regexp = "boxColor specified is not valid. Assigning default value 'deepskyblue3'.")
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #boxNotch
    msg <- "boxNotch must be a single logical value. Assigning default value 'FALSE'."
    expect_warning(obs_plot <- obsPlot(data,
                                       group    = replicategroup,
                                       boxNotch = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group    = replicategroup,
                                       boxNotch = c(1,2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group    = replicategroup,
                                       boxNotch = "abc"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))


    #Violinplot Validations

    #violinLayer
    msg <- "violinLayer must be a singular logical value. Assigning default value FALSE."
    expect_warning(obs_plot <- obsPlot(data,
                                       group       = replicategroup,
                                       violinLayer = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group       = replicategroup,
                                       violinLayer = c(1,2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group       = replicategroup,
                                       violinLayer = "abc"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #Violin Transparency
    msg <- "violinTransparency must be a singular numeric value and must be between 0 and 1. Assigning default value 0.5."
    expect_warning(obs_plot <- obsPlot(data,
                                       group       = replicategroup,
                                       violinLayer = TRUE,
                                       violinTransparency = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group       = replicategroup,
                                       violinLayer = TRUE,
                                       violinTransparency = "abc"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group       = replicategroup,
                                       violinLayer = TRUE,
                                       violinTransparency = c(1,2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group       = replicategroup,
                                       violinLayer = TRUE,
                                       violinTransparency = -2),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group       = replicategroup,
                                       violinLayer = TRUE,
                                       violinTransparency = 2),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #violinColor
    msg <- "violinColor must be of class character and must specify the name of the color or the rgb value. Assigning default value 'goldenrod1'."
    expect_warning(obs_plot <- obsPlot(data,
                                       group       = replicategroup,
                                       violinLayer = TRUE,
                                       violinColor = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group       = replicategroup,
                                       violinLayer = TRUE,
                                       violinColor = c("red","blue")),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                      group        = replicategroup,
                                       violinLayer = TRUE,
                                       violinColor = 1),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group       = replicategroup,
                                       violinLayer = TRUE,
                                       violinColor = "notavalidcolor"),
                   regexp = "violinColor specified is not valid. Assigning default value 'goldenrod1'.")
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))


    #MeanLayer Validations

    #meanLayer
    msg <- "meanLayer must be a singular logical value. Assigning default value TRUE."
    expect_warning(obs_plot <- obsPlot(data,
                                       group     = replicategroup,
                                       meanLayer = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group     = replicategroup,
                                       meanLayer = c(1,2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group     = replicategroup,
                                       meanLayer = "abc"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #meanColor
    msg <- "meanColor must be of class character and must specify the name of the color or the rgb value. Assigning default value 'goldenrod1'."
    expect_warning(obs_plot <- obsPlot(data,
                                       group     = replicategroup,
                                       meanColor = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group     = replicategroup,
                                       meanColor = c("red","blue")),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group = replicategroup,
                                       meanColor = 1),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group     = replicategroup,
                                       meanColor = "notavalidcolor"),
                   regexp = "meanColor specified is not valid. Assigning default value 'goldenrod1'.")
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #meanTransparency
    msg <- "meanTransparency must be a singular numeric value and must be between 0 and 1. Assigning default value 0.7."
    expect_warning(obs_plot <- obsPlot(data,
                                       group = replicategroup,
                                       meanTransparency = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group = replicategroup,
                                       meanTransparency = "abc"),
                   regexp = msg)

    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group = replicategroup,
                                       meanTransparency = c(1,2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group = replicategroup,
                                       meanTransparency = -2),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group = replicategroup,
                                       meanTransparency = 2),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #meanShape - cxplot
    msg <- "meanShape specified is not valid. Assigning default value 'square'."
    expect_warning(obs_plot <- obsPlot(data,
                                       group = replicategroup,
                                       meanShape = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group = replicategroup,
                                       meanShape = c(2,4)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group = replicategroup,
                                       meanShape = "notavalidshape"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #meanshape - ggplot
    msg <- "meanShape specified is not valid. Assigning default value '22'."
    expect_warning(obs_plot <- obsPlot(data,
                                       plotType = "ggplot",
                                       group    = replicategroup,
                                       meanShape = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("gg", "ggplot"))

    expect_warning(obs_plot <- obsPlot(data,
                                       plotType  = "ggplot",
                                       group     = replicategroup,
                                       meanShape = c(2,4)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("gg", "ggplot"))

    expect_warning(obs_plot <- obsPlot(data,
                                       plotType  = "ggplot",
                                       group     = replicategroup,
                                       meanShape = "notavalidshape"),
                   regexp = msg)
   expect_s3_class(obs_plot, c("gg", "ggplot"))

   #meansize
   msg <- "meanSize must be a singular numeric value. Assigning default value 3."
   expect_warning(obs_plot <- obsPlot(data,
                                      group     = replicategroup,
                                      meanSize = NULL),
                  regexp = msg)
   expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

   expect_warning(obs_plot <- obsPlot(data,
                                      group    = replicategroup,
                                      meanSize = c(1,2)),
                  regexp = msg)
   expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

   expect_warning(obs_plot <- obsPlot(data,
                                      group    = replicategroup,
                                      meanSize = "notasize"),
                  regexp = msg)
   expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))


    #PointLayer Validations

    #pointLayer
    msg <- "pointLayer must be a singular logical value. Assigning default value TRUE."
    expect_warning(obs_plot <- obsPlot(data,
                                       group      = replicategroup,
                                       pointLayer = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group      = replicategroup,
                                       pointLayer = c(1,2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group      = replicategroup,
                                       pointLayer = "abc"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #pointTransparency
    msg <- "pointTransparency must be a singular numeric value and must be between 0 and 1. Assigning default value 1."
    expect_warning(obs_plot <- obsPlot(data,
                                       group = replicategroup,
                                       pointTransparency = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group = replicategroup,
                                       pointTransparency = "abc"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group = replicategroup,
                                       pointTransparency = c(1,2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group = replicategroup,
                                       pointTransparency = -2),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group = replicategroup,
                                       pointTransparency = 2),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #pointColor
    msg <- "pointColor must be of class character and must specify the name of the color or the rgb value. Assigning default value 'dodgerblue4'."
    expect_warning(obs_plot <- obsPlot(data,
                                       group = replicategroup,
                                       pointColor = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group = replicategroup,
                                       pointColor = c("red","blue")),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group = replicategroup,
                                       pointColor = 1),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group = replicategroup,
                                       pointColor = "notavalidcolor"),
                   regexp = "pointColor specified is not valid. Assigning default value 'dodgerblue4'.")
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #pointShape - cxplot
    msg <- "pointShape specified is not valid. Assigning default value 'circle'."
    expect_warning(obs_plot <- obsPlot(data,
                                       group = replicategroup,
                                       pointShape = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group      = replicategroup,
                                       pointShape = c(2,4)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group      = replicategroup,
                                       pointShape = "notavalidshape"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #pointshape - ggplot
    msg <- "pointShape specified is not valid. Assigning default value 'circle'."
    expect_warning(obs_plot <- obsPlot(data,
                                       plotType   = "ggplot",
                                       group      = replicategroup,
                                       pointShape = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("gg", "ggplot"))

    expect_warning(obs_plot <- obsPlot(data,
                                       plotType   = "ggplot",
                                       group      = replicategroup,
                                       pointShape = c(2,4)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("gg", "ggplot"))

    expect_warning(obs_plot <- obsPlot(data,
                                       plotType   = "ggplot",
                                       group      = replicategroup,
                                       pointShape = "notavalidshape"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("gg", "ggplot"))

    #pointSize
    msg <- "pointSize must be a singular numeric value. Assigning default value 2."
    expect_warning(obs_plot <- obsPlot(data,
                                       group     = replicategroup,
                                       pointSize = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group     = replicategroup,
                                       pointSize = c(1,2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group     = replicategroup,
                                       pointSize = "notasize"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group = replicategroup,
                                       pointSize = -1),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #pointJitter
    msg <- "pointJitter must be a singular numeric value. Assigning default value 0."
    expect_warning(obs_plot <- obsPlot(data,
                                       group       = replicategroup,
                                       pointJitter = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group       = replicategroup,
                                       pointJitter = c(1,2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group       = replicategroup,
                                       pointJitter = "notasize"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(data,
                                       group       = replicategroup,
                                       pointJitter = -1),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))
})
