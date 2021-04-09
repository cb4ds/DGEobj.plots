context("DGEobj.plots - tests for ggplotMDS.R functions")


test_that("ggplotMDS.R: ggplotMDS()", {
    skip_if(is.null(t_obj1$DGEList))
    skip_if(is.null(t_obj1$design$ReplicateGroup))

    mds_plot <- ggplotMDS(DGEdata = t_obj1,
                          colorBy = t_obj1$design$ReplicateGroup)

    dgelist <- DGEdata <- DGEobj::getItem(t_obj1, "DGEList")
    mds_plot <- ggplotMDS(DGEdata = dgelist,
                          colorBy = t_obj1$design$ReplicateGroup)

    dge_matrix <- DGEdata <- DGEobj::getItem(t_obj1, "DGEList") %>% as.matrix()
    mds_plot <- ggplotMDS(DGEdata = dge_matrix,
                          colorBy = t_obj1$design$ReplicateGroup)

    expect_length(mds_plot, 2)
    expect_named(mds_plot, c("plot", "mdsobj"))
    expect_type(mds_plot, "list")
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    mds_plot <- ggplotMDS(DGEdata  = t_obj1,
                          plotType = "ggplot",
                          colorBy  = t_obj1$design$ReplicateGroup)
    expect_length(mds_plot, 2)
    expect_named(mds_plot, c("plot", "mdsobj"))
    expect_type(mds_plot, "list")
    expect_s3_class(mds_plot$plot, c("gg", "ggplot"))

    #Testing optional parameters
    mds_plot <- ggplotMDS(DGEdata        = t_obj1,
                          colorBy        = t_obj1$design$ReplicateGroup,
                          shapeBy        = t_obj1$design$ReplicateGroup,
                          sizeBy         = rep(1:4,12),
                          hlineIntercept = c(1,0.25),
                          vlineIntercept = c(1,0.25),
                          top            = 10,
                          reflineColor   = c("blue", "red"),
                          reflineSize    = c(3,6),
                          title          = "MDS",
                          transparency   = 0.7,
                          dim.plot       = c(1,2)
                          )
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    mds_plot <- ggplotMDS(DGEdata  = t_obj1,
                          colorBy  = t_obj1$design$ReplicateGroup,
                          symShape = "square",
                          symSize  = 30
    )
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    mds_plot <- ggplotMDS(DGEdata        = t_obj1,
                          colorBy        = t_obj1$design$ReplicateGroup,
                          shapeBy        = t_obj1$design$ReplicateGroup,
                          plotType       = "ggplot",
                          sizeBy         = rep(1:4,12),
                          hlineIntercept = 0.25,
                          vlineIntercept = 0.25,
                          top            = 2,
                          reflineColor   = "blue",
                          reflineSize    = 0.5,
                          labels         = NULL,
                          title          = "MDS",
                          transparency   = 0.7,
                          dim.plot       = c(1,2))
    expect_s3_class(mds_plot$plot, c("gg", "ggplot"))

    mds_plot <- ggplotMDS(DGEdata   = t_obj1,
                          colorBy   = t_obj1$design$ReplicateGroup,
                          plotType  = "ggplot",
                          labelSize = 2,
                          symShape  = "square",
                          symSize   = 3)
    expect_s3_class(mds_plot$plot, c("gg", "ggplot"))

    mds_plot <- ggplotMDS(DGEdata   = t_obj1,
                          colorBy   = t_obj1$design$ReplicateGroup,
                          shapeBy   = t_obj1$design$ReplicateGroup,
                          plotType  = "ggplot",
                          labelSize = 2,
                          symShape  = "square",
                          symSize   = 3)
    expect_s3_class(mds_plot$plot, c("gg", "ggplot"))

    mds_plot <- ggplotMDS(DGEdata   = t_obj1,
                          colorBy   = t_obj1$design$ReplicateGroup,
                          sizeBy    = rep(1:4,12),
                          plotType  = "ggplot",
                          labelSize = 2,
                          symShape  = "square",
                          symSize   = 3)
    expect_s3_class(mds_plot$plot, c("gg", "ggplot"))

    mds_plot <- ggplotMDS(DGEdata   = t_obj1,
                          colorBy   = t_obj1$design$ReplicateGroup,
                          plotType  = "ggplot",
                          labelSize = 2,
                          symShape  = "square",
                          symSize   = 3)
    expect_s3_class(mds_plot$plot, c("gg", "ggplot"))

    # testing assert statements
    expect_error(ggplotMDS(DGEdata = 1:10),
                 regexp = "DGEdata must be specified and must be of class 'DGEList', 'DGEobj', 'matrix' or 'Dataframe'.")
    expect_error(ggplotMDS(DGEdata = NULL),
                 regexp = "DGEdata must be specified and must be of class 'DGEList', 'DGEobj', 'matrix' or 'Dataframe'.")
    expect_error(ggplotMDS(),
                 regexp = "DGEdata must be specified and must be of class 'DGEList', 'DGEobj', 'matrix' or 'Dataframe'.")
    expect_error(ggplotMDS(DGEdata = t_obj1),
                 regexp = "colorBy must be specified and should be the length of the number of columns in DGEdata.")
    expect_error(ggplotMDS(DGEdata = t_obj1,
                           colorBy = t_obj1$design$ReplicateGroup,
                           shapeBy = 1:10),
                 regexp = "shapeBy should be the length of the number of columns in DGEdata.")
    expect_error(ggplotMDS(DGEdata = t_obj1,
                           colorBy = t_obj1$design$ReplicateGroup,
                           sizeBy  = 1:10),
                 regexp = "sizeBy should be the length of the number of columns in DGEdata.")
    expect_error(ggplotMDS(DGEdata  = t_obj1,
                           plotType = "cx",
                           colorBy  = t_obj1$design$ReplicateGroup),
                 regexp = "Plot type must be either canvasXpress or ggplot.")
    expect_error(ggplotMDS(DGEdata  = t_obj1,
                           plotType = NULL,
                           colorBy  = t_obj1$design$ReplicateGroup),
                 regexp = "Plot type must be either canvasXpress or ggplot.")

    #Testing optional parameters
    msg <- "top should be a numeric value or Inf. Assigning default value 'Inf'."
    expect_warning(mds_plot <- ggplotMDS(DGEdata = t_obj1,
                                         colorBy = t_obj1$design$ReplicateGroup,
                                         top     = "abc"),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))


    expect_warning(mds_plot <- ggplotMDS(DGEdata = t_obj1,
                                         colorBy = t_obj1$design$ReplicateGroup,
                                         top     = c(1,2)),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    #transparency
    msg <- "transparency must be a singular numeric value and must be between 0 and 1. Assigning default value 0.7."
    expect_warning(mds_plot <- ggplotMDS(DGEdata = t_obj1,
                                         colorBy = t_obj1$design$ReplicateGroup,
                                         transparency = "abc"),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(mds_plot <- ggplotMDS(DGEdata = t_obj1,
                                         colorBy = t_obj1$design$ReplicateGroup,
                                         transparency = c(1,2)),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(mds_plot <- ggplotMDS(DGEdata = t_obj1,
                                         colorBy = t_obj1$design$ReplicateGroup,
                                         labels  = "abc"),
                   regexp = "Number of labels does not match the number of columns in DGEdata. Assigning default values.")
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    #ggplot specific validations
    msg <- "labelSize should be singular numeric value and greater than zero. Assigning default value 3."
    expect_warning(mds_plot <- ggplotMDS(DGEdata   = t_obj1,
                                         colorBy   = t_obj1$design$ReplicateGroup,
                                         plotType  = "ggplot",
                                         labels    = t_obj1$design$ReplicateGroup,
                                         labelSize = "a"),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("gg", "ggplot"))

    expect_warning(mds_plot <- ggplotMDS(DGEdata   = t_obj1,
                                         colorBy   = t_obj1$design$ReplicateGroup,
                                         plotType  = "ggplot",
                                         labels    = t_obj1$design$ReplicateGroup,
                                         labelSize = -2),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("gg", "ggplot"))

    expect_warning(mds_plot <- ggplotMDS(DGEdata   = t_obj1,
                                         colorBy   = t_obj1$design$ReplicateGroup,
                                         plotType  = "ggplot",
                                         labels    = t_obj1$design$ReplicateGroup,
                                         labelSize = c(1,2)),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("gg", "ggplot"))

    expect_warning(mds_plot <- ggplotMDS(DGEdata = t_obj1,
                          colorBy   = t_obj1$design$ReplicateGroup,
                          plotType  = "ggplot",
                          labelSize = c(1,2)),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("gg", "ggplot"))

    msg <- "hlineIntercept must be numeric. Ignoring hlineIntercept."
    expect_warning(mds_plot <- ggplotMDS(DGEdata = t_obj1,
                                         colorBy = t_obj1$design$ReplicateGroup,
                                         plotType = "ggplot",
                                         hlineIntercept = "abc"),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("gg", "ggplot"))


    expect_warning(mds_plot <- ggplotMDS(DGEdata = t_obj1,
                                         colorBy = t_obj1$design$ReplicateGroup,
                                         plotType = "ggplot",
                                         hlineIntercept = c("a","b")),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("gg", "ggplot"))

    msg <- "vlineIntercept must be numeric. Ignoring vlineIntercept."
    expect_warning(mds_plot <- ggplotMDS(DGEdata = t_obj1,
                                         colorBy = t_obj1$design$ReplicateGroup,
                                         plotType = "ggplot",
                                         vlineIntercept = "abc"),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("gg", "ggplot"))

    msg <- "reflineSize must be a numeric value greater than 0. Assigning default value '0.5'."
    expect_warning(mds_plot <- ggplotMDS(DGEdata = t_obj1,
                                         colorBy = t_obj1$design$ReplicateGroup,
                                         plotType = "ggplot",
                                         hlineIntercept = 1,
                                         reflineSize = "a"),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("gg", "ggplot"))

    expect_warning(mds_plot <- ggplotMDS(DGEdata  = t_obj1,
                                         colorBy  = t_obj1$design$ReplicateGroup,
                                         plotType = "ggplot",
                                         vlineIntercept = 1,
                                         reflineSize = -1),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("gg", "ggplot"))

    expect_warning(mds_plot <- ggplotMDS(DGEdata  = t_obj1,
                                         colorBy  = t_obj1$design$ReplicateGroup,
                                         plotType = "ggplot",
                                         vlineIntercept = 1,
                                         reflineSize = c(1,2)),
                   regexp = "reflineSize must be either length 1 or the same as the intercept. Assigning default value '0.5'.")
    expect_s3_class(mds_plot$plot, c("gg", "ggplot"))

    expect_warning(mds_plot <- ggplotMDS(DGEdata  = t_obj1,
                                         colorBy  = t_obj1$design$ReplicateGroup,
                                         plotType = "ggplot",
                                         hlineIntercept = 1,
                                         reflineColor = 1),
                   regexp = "reflineColor must be a of class character and must specify the name of the color or the rgb value. Assigning default value 'red'.")
    expect_s3_class(mds_plot$plot, c("gg", "ggplot"))

    expect_warning(mds_plot <- ggplotMDS(DGEdata  = t_obj1,
                                         colorBy  = t_obj1$design$ReplicateGroup,
                                         plotType = "ggplot",
                                         hlineIntercept = 1,
                                         reflineColor = "abc"),
                   regexp = "Color specified is not valid. Assigning default value 'red'.")
    expect_s3_class(mds_plot$plot, c("gg", "ggplot"))

    expect_warning(mds_plot <- ggplotMDS(DGEdata  = t_obj1,
                                         colorBy  = t_obj1$design$ReplicateGroup,
                                         plotType = "ggplot",
                                         hlineIntercept = 1,
                                         reflineColor = c("red", "blue")),
                   regexp = "reflineColor must be either length 1 or the same as the intercept. Assigning default value 'red'.")
    expect_s3_class(mds_plot$plot, c("gg", "ggplot"))

    expect_warning(mds_plot <- ggplotMDS(DGEdata = t_obj1,
                                         colorBy = t_obj1$design$ReplicateGroup,
                                         plotType = "ggplot",
                                         hlineIntercept = 1,
                                         vlineIntercept = 1,
                                         reflineColor = c("red", "blue")),
                   regexp = "reflineColor must be either length 1 or the same as the intercept. Assigning default value 'red'.")
    expect_s3_class(mds_plot$plot, c("gg", "ggplot"))

    expect_warning(mds_plot <- ggplotMDS(DGEdata  = t_obj1,
                                         colorBy  = t_obj1$design$ReplicateGroup,
                                         plotType = "ggplot",
                                         hlineIntercept = 1,
                                         vlineIntercept = 1,
                                         reflineSize = c(1, 2)),
                   regexp = "reflineSize must be either length 1 or the same as the intercept. Assigning default value '0.5'.")
    expect_s3_class(mds_plot$plot, c("gg", "ggplot"))

    msg <- "symShape must be a singular value of class 'character' or numeric value. Refer help documentation for valid values. Assigning default value 'circle'."
    expect_warning(mds_plot <- ggplotMDS(DGEdata  = t_obj1,
                                         colorBy  = t_obj1$design$ReplicateGroup,
                                         plotType = "ggplot",
                                         symShape = 32),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("gg", "ggplot"))

    expect_warning(mds_plot <- ggplotMDS(DGEdata  = t_obj1,
                                         colorBy  = t_obj1$design$ReplicateGroup,
                                         plotType = "ggplot",
                                         symShape = c(1,32)),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("gg", "ggplot"))

    expect_warning(mds_plot <- ggplotMDS(DGEdata  = t_obj1,
                                         colorBy  = t_obj1$design$ReplicateGroup,
                                         symShape = c(1,32)),
                   regexp = "symShape must be a singular value of class 'character'. Assigning default value 'circle'.")
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    msg <- "symShape must be a singular value of class 'character'. Assigning default value 'circle'."
    expect_warning(mds_plot <- ggplotMDS(DGEdata  = t_obj1,
                                         colorBy  = t_obj1$design$ReplicateGroup,
                                         symShape = 32),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    msg <- "symSize must be a singular numeric value. Assigning default value 10."
    expect_warning(mds_plot <- ggplotMDS(DGEdata  = t_obj1,
                                         colorBy  = t_obj1$design$ReplicateGroup,
                                         plotType = "ggplot",
                                         symSize  = "abc"),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("gg", "ggplot"))

    expect_warning(mds_plot <- ggplotMDS(DGEdata = t_obj1,
                                         colorBy = t_obj1$design$ReplicateGroup,
                                         symSize = "abc"),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    msg <- "dim.plot should a numeric vector of length 2 and should be lesser than the number of columns in DGEobj."
    expect_warning(mds_plot <- ggplotMDS(DGEdata = t_obj1,
                                         colorBy = t_obj1$design$ReplicateGroup,
                                         dim.plot = "abc"),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(mds_plot <- ggplotMDS(DGEdata = t_obj1,
                                         colorBy = t_obj1$design$ReplicateGroup,
                                         dim.plot = c(1)),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

    expect_warning(mds_plot <- ggplotMDS(DGEdata = t_obj1,
                                         colorBy = t_obj1$design$ReplicateGroup,
                                         dim.plot = c(1,49)),
                   regexp = msg)
    expect_s3_class(mds_plot$plot, c("canvasXpress", "htmlwidget"))

})


test_that("ggplotMDS.R: MDS_var_explained()", {
    skip_if(is.null(t_obj1$DGEList))
    skip_if(is.null(t_obj1$design$organism))

    mds_plot <- ggplotMDS(DGEdata = t_obj1,
                          colorBy = t_obj1$design$organism)
    var_result <- MDS_var_explained(mds_plot$mdsobj)
    expect_length(var_result, 3)
    expect_named(var_result, c("varexp", "cumvar", "var_explained"))
    expect_type(var_result, "list")
    expect_s3_class(var_result$varexp, c("canvasXpress", "htmlwidget"))
    expect_s3_class(var_result$cumvar, c("canvasXpress", "htmlwidget"))

    var_result <- MDS_var_explained(mds_plot$mdsobj,
                                    plotType = "ggplot")
    expect_length(var_result, 3)
    expect_named(var_result, c("varexp", "cumvar", "var_explained"))
    expect_type(var_result, "list")
    expect_s3_class(var_result$varexp, c("gg", "ggplot"))
    expect_s3_class(var_result$cumvar, c("gg", "ggplot"))

    # testing mds matrix
    var_result <- MDS_var_explained(t_obj1$counts)
    expect_length(var_result, 3)
    expect_named(var_result, c("varexp", "cumvar", "var_explained"))
    expect_type(var_result, "list")
    expect_s3_class(var_result$varexp, c("canvasXpress", "htmlwidget"))
    expect_s3_class(var_result$cumvar, c("canvasXpress", "htmlwidget"))

    var_result <- MDS_var_explained(t_obj1$counts,
                                    plotType = "ggplot")
    expect_length(var_result, 3)
    expect_named(var_result, c("varexp", "cumvar", "var_explained"))
    expect_type(var_result, "list")
    expect_s3_class(var_result$varexp, c("gg", "ggplot"))
    expect_s3_class(var_result$cumvar, c("gg", "ggplot"))

    # testing optional parameters
    var_result <- MDS_var_explained(mds_plot$mdsobj,
                                    barWidth = 0.8,
                                    cumVarLimit = 1.0,
                                    barColor = "red",
                                    topN = Inf)
    expect_length(var_result, 3)
    expect_named(var_result, c("varexp", "cumvar", "var_explained"))
    expect_type(var_result, "list")
    expect_s3_class(var_result$varexp, c("canvasXpress", "htmlwidget"))
    expect_s3_class(var_result$cumvar, c("canvasXpress", "htmlwidget"))

    var_result <- MDS_var_explained(mds_plot$mdsobj,
                                    plotType = "ggplot",
                                    barWidth = 0.8,
                                    cumVarLimit = 1.0,
                                    barColor = "red")
    expect_length(var_result, 3)
    expect_named(var_result, c("varexp", "cumvar", "var_explained"))
    expect_type(var_result, "list")
    expect_s3_class(var_result$varexp, c("gg", "ggplot"))
    expect_s3_class(var_result$cumvar, c("gg", "ggplot"))

    msg <- "barWidth should be a single numeric value between 0 and 1. Assigning default value 0.65."
    expect_warning(var_result <- MDS_var_explained(mds_plot$mdsobj,
                                                   barWidth = 1.5),
                   regexp = msg)
    expect_s3_class(var_result$varexp, c("canvasXpress", "htmlwidget"))

    expect_warning(var_result <- MDS_var_explained(mds_plot$mdsobj,
                                    plotType = "ggplot",
                                    barWidth = 1.5),
                   regexp = msg)
    expect_s3_class(var_result$varexp, c("gg", "ggplot"))

    expect_warning(var_result <- MDS_var_explained(mds_plot$mdsobj,
                                                   plotType = "ggplot",
                                                   barWidth = c('a','b')),
                   regexp = msg)
    expect_s3_class(var_result$varexp, c("gg", "ggplot"))

    expect_warning(var_result <- MDS_var_explained(mds_plot$mdsobj,
                                                   plotType = "ggplot",
                                                   barWidth = -1),
                   regexp = msg)
    expect_s3_class(var_result$varexp, c("gg", "ggplot"))

    msg <- "cumVarLimit should be a single numeric value between 0 and 1. Assigning default value 0.9."
    expect_warning(var_result <- MDS_var_explained(mds_plot$mdsobj,
                                                   cumVarLimit = 1.5),
                   regexp = msg)
    expect_s3_class(var_result$varexp, c("canvasXpress", "htmlwidget"))

    expect_warning(var_result <- MDS_var_explained(mds_plot$mdsobj,
                                                   cumVarLimit = c('a','b')),
                   regexp = msg)
    expect_warning(var_result <- MDS_var_explained(mds_plot$mdsobj,
                                                   cumVarLimit = -1),
                   regexp = msg)
    expect_s3_class(var_result$varexp, c("canvasXpress", "htmlwidget"))

    msg <- "barColor specified is not valid. Assigning default value 'dodgerblue4'."
    expect_warning(var_result <- MDS_var_explained(mds_plot$mdsobj,
                                                   barColor = "abc"),
                   regexp = msg)
    expect_s3_class(var_result$varexp, c("canvasXpress", "htmlwidget"))

    expect_warning(var_result <- MDS_var_explained(mds_plot$mdsobj,
                                                   barColor = 1),
                   regexp = msg)
    expect_s3_class(var_result$varexp, c("canvasXpress", "htmlwidget"))

    expect_warning(var_result <- MDS_var_explained(mds_plot$mdsobj,
                                                   barColor = c("red", "blue")),
                   regexp = msg)
    expect_s3_class(var_result$varexp, c("canvasXpress", "htmlwidget"))

    msg <- "topN should be a numeric value. Assigning default value 10."
    expect_warning(var_result <- MDS_var_explained(mds_plot$mdsobj,
                                                   topN = "abc"),
                   regexp = msg)
    expect_s3_class(var_result$varexp, c("canvasXpress", "htmlwidget"))

    expect_warning(var_result <- MDS_var_explained(mds_plot$mdsobj,
                                                   topN = c(1,2)),
                   regexp = msg)
    expect_s3_class(var_result$varexp, c("canvasXpress", "htmlwidget"))

    expect_warning(var_result <- MDS_var_explained(mds_plot$mdsobj,
                                                   topN = -1),
                   regexp = msg)
    expect_s3_class(var_result$varexp, c("canvasXpress", "htmlwidget"))

    # testing assert statements
    expect_error(MDS_var_explained(), regexp = "mds is required and must be specified.")
    expect_error(MDS_var_explained(t_obj1$counts, plotType = "cx"),
                 regexp = "Plot type must be either canvasXpress or ggplot.")

})
