#
# -  Set of Functions For Analysing the Iris Dataset
#


data_plot <- function(method)
{
    #require(dplyr)          # - data manipulation
    #require(tidyr)          # - tidy up data
    require(ggplot2)        # - graphing
    require(gridExtra)      # - arrange plots together
    
#     # - transform data to long format
#     dl <- iris %>% dplyr::mutate(id=rownames(iris)) %>%
#         dplyr::select(id,Species,1:4) %>%
#         tidyr::gather(x_name, x_value, Sepal.Length:Petal.Width)
#     ggplot(data=dl) + geom_boxplot(aes(y=x_value,x=Species, colour=Species)) +
#             facet_wrap(~x_name) + 
#             theme(legend.position='none', axis.title = element_blank())
    
    # - pairs plot
    if ( method == "Pairs" )
    {
        # - pairs scatter plots with correlations above diag
        plot <- pairs(iris[,1:4], col=iris$Species, upper.panel = panel.cor)
    }
    else if ( method == "Box" )
    {
        # - create plots for each variable
        p1 <- ggplot(data=iris, aes(x=Species, y=Sepal.Length, colour=Species)) + 
            geom_boxplot() + stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
            theme(axis.title.x = element_blank(), legend.position='none')
        p2 <- ggplot(data=iris, aes(x=Species, y=Sepal.Width, colour=Species)) + 
            geom_boxplot() + stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
            theme(axis.title.x = element_blank(), legend.position='none')
        p3 <- ggplot(data=iris, aes(x=Species, y=Petal.Length, colour=Species)) + 
            geom_boxplot() + stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
            theme(axis.title.x = element_blank(), legend.position='none')
        p4 <- ggplot(data=iris, aes(x=Species, y=Petal.Width, colour=Species)) + 
            geom_boxplot() + stat_summary(fun.y=mean, geom="point", shape=5, size=4) +
            theme(axis.title.x = element_blank(), legend.position='none')
        # - 2 by 2 grid
        plot <- gridExtra::grid.arrange(p1, p2, p3, p4, nrow=2)

    }
    else if ( method == "Density" )
    {
        # - create individual plots
        p1 <- ggplot(data=iris, aes(x=Sepal.Length, fill=Species)) + geom_density(alpha = 0.4) + 
            theme(axis.title.y = element_blank())
        p2 <- ggplot(data=iris, aes(x=Sepal.Width, fill=Species)) + geom_density(alpha = 0.4) + 
            theme(axis.title.y = element_blank())
        p3 <- ggplot(data=iris, aes(x=Petal.Length, fill=Species)) + geom_density(alpha = 0.4) + 
            theme(axis.title.y = element_blank())
        p4 <- ggplot(data=iris, aes(x=Petal.Width, fill=Species)) + geom_density(alpha = 0.4) + 
            theme(axis.title.y = element_blank())
        # - 2 by 2 grid
        plot <- grid_arrange_shared_legend(p1, p2, p3, p4)
    }
    else
    {
        # - should never be the case since list is a defined drop down
        plot <- get_empty_ggplot("Invalid Plot Method!")
    }
    
    # - return the plot
    return(plot)
}

unsup_learn <- function(method)
{
    require(ggplot2)        # - graphing
    require(cluster)        # - silhouette plot for k-means
    require(sparcl)         # - h-clusting graphics
    
    if ( method == "Principal Components" )
    {
        # - run pca
        pr <- prcomp(x=iris[,1:4], center=TRUE, scale=TRUE)
        
        # - put pca score vectors and species type in a data frame
        df_pr <- data.frame(pr$x, Species=iris$Species)
        
        # - x and y labels
        pc1_exp <- pr$sdev[1]^2/(sum(pr$sdev^2))
        pc2_exp <- pr$sdev[2]^2/(sum(pr$sdev^2))
        x_lab <- paste0("PC1 (", round(100*pc1_exp,0), "% variance explained)")
        y_lab <- paste0("PC2 (", round(100*pc2_exp,0), "% variance explained)")
        
        # - create circle
        #radius <- 0.7 * min(diff(range(df_pr$PC1)), diff(range(df_pr$PC2)))/2
        #theta <- seq(0, 2*pi, length.out = nrow(iris))
        #df_circle <- data.frame(PC1=radius*cos(theta), PC2=radius*sin(theta), Species=iris$Species)
        
        plot <- ggplot(data = df_pr, aes(x=PC1, y=PC2, colour=Species)) + 
            geom_point() +
            # - circle looks like an ellipse b/c of axis, but dont want to force
            #   plot to be square or it wont resize nicely in browser
            #geom_path(data = df_circle, color = 'black', size = 1/2, alpha = 1/3) + 
            labs(x=x_lab, y=y_lab) + 
            theme(legend.title=element_blank(), legend.position='bottom')
            
#         ggbiplot(pr, obs.scale = 1, var.scale = 1, 
#             groups = iris$Species, ellipse = TRUE, circle = TRUE) + 
#             scale_color_discrete(name = '') + 
#             theme(legend.direction = 'horizontal', legend.position = 'top')
    }
    else if ( method == "K-Means Clustering" ) 
    {
        # - run k means with 3 clusters
        km <- kmeans(iris[,1:4], 3, nstart=50)
        
        # - silhouette plot
        plot <- plot(silhouette(km$cluster, dist(iris[,1:4])))
    }
    else if ( method == "Hierarchical Clustering" ) 
    {
        # - create partition (to make it easier to add labels)
        #library(caret)
        #train_index <- caret::createDataPartition(y=iris$Species, p=0.33, list=FALSE)
        #iris_train <- iris[train_index,]
        
        # - run h-clustering
        hc <- hclust(dist(iris[,1:4]), method="complete")
        
        #library(ggdendro)
        #ggdendrogram(hc, rotate = FALSE, size = 2)
        
        # - lets have two dendrographs and stack them on top of eachother
        #   1) colored based on a cut of 3
        #   2) colored base on original classification
        
#         library(dendextend)
#         dend <- as.dendrogram(hc)
#         dend1 <- color_branches(dend, k = 3)
#         dend2 <- color_labels(dend, k = 3)
#         
#         plot(dend1, main = "Colored branches", xlab="", ylab="")
#         plot(dend2, main = "Colored labels")
#         
#         A2Rplot(hc, k = 3, boxes = FALSE, col.up = "gray50",
#             col.down = c("#FF6B6B", "#4ECDC4", "#556270"))
        
        plot <- sparcl::ColorDendrogram(hc, y=as.numeric(iris$Species), branchlength=2)
    }
    else
    {
        # - should never be the case since list is a defined drop down
        plot <- get_empty_ggplot("Invalid Unsupervised Learning Method!")
    }
    
    return(plot)
}

unsup_learn_summary <- function(method)
{
    if ( method == "Principal Components" )
    {
        unsup_sum <- paste0("The first two Principal Component explain 96% of the total ",
            "vairiation. Petal length and with are highly correlated (0.96) and ",
            "their variability across the species is accounted for mainly by PC1. ",
            "PC1 also explains a large part of sepal length (0.87 and 0.82 correlation ",
            "to petal length and petal width respectively).")
    }
    else if ( method == "K-Means Clustering" ) 
    {
        # - good resource: http://scikit-learn.org/stable/auto_examples/cluster/plot_kmeans_silhouette_analysis.html
        unsup_sum <- paste0("For three clusters, the silhouette scores are all ",
            "above average indicating the sample is far from the neighboring cluster. ",
            "Further, the silhouette thickness are consistent across clusters ",
            "indicating each cluster is roughly equal in size.")
    }
    else if ( method == "Hierarchical Clustering" ) 
    {
        unsup_sum <- paste0("The three species are mostly clustered together ",
            "in the sense that they fuse with eachother at the bottom of the tree, ",
            "however using a cut of three is some overlap between blue (virginica) ",
            "and green (versicolor). Cluster analysis was perfromed with complete ",
            "linkage (single and average linkages produce poor groupings).")
    }
    else
    {
        # - should never be the case since list is a defined drop down
        unsup_sum <- ""
    }
    
    return(unsup_sum)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# - Private Methods
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

grid_arrange_shared_legend <- function(...) 
{
    plots <- list(...)
    g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    grid.arrange(
        do.call(arrangeGrob, lapply(plots, function(x)
            x + theme(legend.position="none"))),
        legend,
        ncol = 1,
        heights = unit.c(unit(1, "npc") - lheight, lheight))
}

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}

panel.hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

get_empty_ggplot <- function(message)
{
    ggplot() + 
        annotate("text", x = 4, y = 25, size=8, label = message) + 
        theme_bw() + 
        theme(axis.title = element_blank(), axis.ticks = element_blank(), 
            axis.text = element_blank())
}
