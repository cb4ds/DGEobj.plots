context("DGEobj.plots - tests for ggplotMDS.R functions")


test_that("obsPlot.R: obsPlot()", {

    # testing assert statements
    msg <- "data must be specified and should be of class 'data.frame'."
    expect_error(obsPlot(data = 1:10),
                 regexp = msg)
    expect_error(obsPlot(),
                 regexp = msg)

    msg <- "Plot type must be either canvasXpress or ggplot."
    expect_error(obsPlot(tidyInt,
                         plotByCol = "GeneID",
                         groupCol = "group",
                         valueCol = "Log2CPM",
                         plotType = "cx"),
                 regexp = msg)
    expect_error(obsPlot(tidyInt,
                         plotByCol = "GeneID",
                         groupCol = "group",
                         valueCol = "Log2CPM",
                         plotType = NULL),
                 regexp = msg)

    msg <- 'The plotByCol must be included in the colnames of the specified data.'
    expect_error(obsPlot(tidyInt,
                         plotByCol = "sampleID",
                         groupCol = "group",
                         valueCol = "Log2CPM"),
                 regexp = msg)
    expect_error(obsPlot(tidyInt,
                         plotByCol = NULL,
                         groupCol = "group",
                         valueCol = "Log2CPM"),
                 regexp = msg)

    msg <- 'The groupCol must be included in the colnames of the specified data.'
    expect_error(obsPlot(tidyInt,
                         plotByCol = "GeneID",
                         groupCol = "sampleID",
                         valueCol = "Log2CPM"),
                 regexp = msg)
    expect_error(obsPlot(tidyInt,
                         plotByCol = "GeneID",
                         groupCol = NULL,
                         valueCol = "Log2CPM"),
                 regexp = msg)

    msg <- 'The valueCol must be included in the colnames of the specified data.'
    expect_error(obsPlot(tidyInt,
                         plotByCol = "GeneID",
                         groupCol = "group",
                         valueCol = "sampleID"),
                 regexp = msg)
    expect_error(obsPlot(tidyInt,
                         plotByCol = "GeneID",
                         groupCol = "group",
                         valueCol = NULL),
                 regexp = msg)

    #Testing optional parameters

    #template
    expect_warning(obs_plot <- obsPlot(tidyInt,
                                      plotByCol = "GeneID",
                                      groupCol  = "group",
                                      valueCol  = "Log2CPM"),
                   regexp = "")
    expect_s3_class(obs_plot, c("gg", "ggplot"))
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #facet
    msg <- "facet must be a singular logical value. Assigning default value TRUE."
    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       facet = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       facet = c(1,2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       facet = "abc"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #facet row
    msg <- "facetRow needs a singular numeric value. Assigning default value"
    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       facetRow = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       facetRow = c(1,2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       facetRow = "abc"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #title
    msg <- 'Invalid title specificed. Title must be singular value of class character.'
    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       title = 1),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       title = c("title1", "title2")),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #xlab
    msg <- 'xlab value specified is not valid. Assigning groupCol name as the default value.'
    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       xlab = 1),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       xlab = c("xlab1", "xlab2")),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #ylab
    msg <- 'ylab value specified is not valid. Assigning valueCol name as the default value.'
    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       ylab = 1),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       ylab = c("ylab1", "ylab2")),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #boxplot validations

    #boxlayer
    msg <- "boxLayer must be a single logical value. Assigning default value 'TRUE'."
    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       boxLayer = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       boxLayer = c(1,2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       boxLayer = "abc"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #boxTransparency
    msg <- "boxTransparency must be a singular numeric value and must be between 0 and 1. Assigning default value 0.5."
    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       boxTransparency = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       boxTransparency = "abc"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       boxTransparency = c(1,2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       boxTransparency = -2),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       boxTransparency = 2),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #boxColor
    msg <- "boxColor must be of class character and must specify the name of the color or the rgb value. Assigning default value 'deepskyblue3'."
    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       boxColor = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       boxColor = c("red","blue")),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       boxColor = 1),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       boxColor = "notavalidcolor"),
                   regexp = "boxColor specified is not valid. Assigning default value 'deepskyblue3'.")
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #boxNotch
    msg <- "boxNotch must be a single logical value. Assigning default value 'FALSE'."
    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       boxNotch = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       boxNotch = c(1,2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       boxNotch = "abc"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))


    #Violinplot Validations

    #violinLayer
    msg <- "violinLayer must be a singular logical value. Assigning default value FALSE."
    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       violinLayer = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       violinLayer = c(1,2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       violinLayer = "abc"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #Violin Transparency
    msg <- "violinTransparency must be a singular numeric value and must be between 0 and 1. Assigning default value 0.5."
    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       violinLayer = TRUE,
                                       violinTransparency = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       violinLayer = TRUE,
                                       violinTransparency = "abc"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       violinLayer = TRUE,
                                       violinTransparency = c(1,2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       violinLayer = TRUE,
                                       violinTransparency = -2),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       violinLayer = TRUE,
                                       violinTransparency = 2),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #violinColor
    msg <- "violinColor must be of class character and must specify the name of the color or the rgb value. Assigning default value 'goldenrod1'."
    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       violinLayer = TRUE,
                                       violinColor = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       violinLayer = TRUE,
                                       violinColor = c("red","blue")),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       violinLayer = TRUE,
                                       violinColor = 1),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       violinLayer = TRUE,
                                       violinColor = "notavalidcolor"),
                   regexp = "violinColor specified is not valid. Assigning default value 'goldenrod1'.")
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))


    #MeanLayer Validations

    #meanLayer
    msg <- "meanLayer must be a singular logical value. Assigning default value TRUE."
    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       meanLayer = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       meanLayer = c(1,2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       meanLayer = "abc"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #meanColor
    msg <- "meanColor must be of class character and must specify the name of the color or the rgb value. Assigning default value 'goldenrod1'."
    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       meanColor = NULL),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       meanColor = c("red","blue")),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       meanColor = 1),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       meanColor = "notavalidcolor"),
                   regexp = "meanColor specified is not valid. Assigning default value 'goldenrod1'.")
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    #meanOutlineColor
    msg <- "meanOutlineColor must be of class character and must specify the name of the color or the rgb value. Assigning default value 'goldenrod1'."
    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       meanOutlineColor = NULL),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       meanOutlineColor = c("red","blue")),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       meanOutlineColor = 1),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       meanOutlineColor = "notavalidcolor"),
                   regexp = "meanOutlineColor specified is not valid. Assigning default value 'goldenrod1'.")
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    #meanTransparency
    msg <- "meanTransparency must be a singular numeric value and must be between 0 and 1. Assigning default value 0.7."
    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       meanTransparency = NULL),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       meanTransparency = "abc"),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       meanTransparency = c(1,2)),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       meanTransparency = -2),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       meanTransparency = 2),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    #meanShape - cxplot
    msg <- "meanShape specified is not valid. Assigning default value 'square'."
    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       meanShape = NULL),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       meanShape = c(2,4)),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       meanShape = "notavalidshape"),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    #meanshape - ggplot
    msg <- "meanShape specified is not valid. Assigning default value '22'."
    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotType = "ggplot",
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       meanShape = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("gg", "ggplot"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotType = "ggplot",
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       meanShape = c(2,4)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("gg", "ggplot"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotType = "ggplot",
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       meanShape = "notavalidshape"),
                   regexp = msg)
   expect_s3_class(obs_plot, c("gg", "ggplot"))

   #meansize
   msg <- "meanSize must be a singular numeric value. Assigning default value 3."
   expect_warning(obs_plot <- obsPlot(tidyInt,
                                      plotByCol = "GeneID",
                                      groupCol  = "group",
                                      valueCol  = "Log2CPM",
                                      meanSize = NULL),
                  regexp = msg)
   expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

   expect_warning(obs_plot <- obsPlot(tidyInt,
                                      plotByCol = "GeneID",
                                      groupCol  = "group",
                                      valueCol  = "Log2CPM",
                                      meanSize = c(1,2)),
                  regexp = msg)
   expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

   expect_warning(obs_plot <- obsPlot(tidyInt,
                                      plotByCol = "GeneID",
                                      groupCol  = "group",
                                      valueCol  = "Log2CPM",
                                      meanSize = "notasize"),
                  regexp = msg)
   expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))


    #PointLayer Validations

    #pointLayer
    msg <- "pointLayer must be a single logical value. Assigning default value 'TRUE'."
    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       pointLayer = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       pointLayer = c(1,2)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       pointLayer = "abc"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("canvasXpress", "htmlwidget"))

    #pointTransparency
    msg <- "pointTransparency must be a singular numeric value and must be between 0 and 1. Assigning default value 1."
    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       pointTransparency = NULL),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       pointTransparency = "abc"),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       pointTransparency = c(1,2)),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       pointTransparency = -2),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       pointTransparency = 2),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    #pointColor
    msg <- "pointColor must be of class character and must specify the name of the color or the rgb value. Assigning default value 'dodgerblue4'."
    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       pointColor = NULL),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       pointColor = c("red","blue")),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       pointColor = 1),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       pointColor = "notavalidcolor"),
                   regexp = "pointColor specified is not valid. Assigning default value 'dodgerblue4'.")
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    #pointShape - cxplot
    msg <- "pointShape specified is not valid. Assigning default value 'sphere'."
    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       pointShape = NULL),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       pointShape = c(2,4)),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       pointShape = "notavalidshape"),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    #pointshape - ggplot
    msg <- "pointShape specified is not valid. Assigning default value '21'."
    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotType = "ggplot",
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       pointShape = NULL),
                   regexp = msg)
    expect_s3_class(obs_plot, c("gg", "ggplot"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotType = "ggplot",
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       pointShape = c(2,4)),
                   regexp = msg)
    expect_s3_class(obs_plot, c("gg", "ggplot"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotType = "ggplot",
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       pointShape = "notavalidshape"),
                   regexp = msg)
    expect_s3_class(obs_plot, c("gg", "ggplot"))

    #pointSize
    msg <- "pointSize must be a singular numeric value. Assigning default value 2."
    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       pointSize = NULL),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       pointSize = c(1,2)),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(obs_plot <- obsPlot(tidyInt,
                                       plotByCol = "GeneID",
                                       groupCol  = "group",
                                       valueCol  = "Log2CPM",
                                       pointSize = "notasize"),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))




})
