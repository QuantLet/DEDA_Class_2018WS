################################## Replace Time Variables #####################################################3 This script
################################## provides function to plot a variable against the house price
libraries = c("data.table", "ggplot2")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

price_per_factor_box &lt;- function(factor, factor_name) {
    sold_per_x &lt;- data.frame(factor, train$SalePrice)
    colnames(sold_per_x) &lt;- c(factor_name, "SalePrice")
    # create boxplot
    p &lt;- ggplot(sold_per_x) + geom_boxplot(aes(x = factor, y = SalePrice, group = factor, fill = factor))
    # add title
    p &lt;- p + ggtitle(paste(factor_name, "SalePrice", sep = " vs. "))
    # add colours
    p &lt;- p + theme(legend.position = "none") + xlab(factor_name)
    return(p)
}

price_per_factor_plot &lt;- function(factor, factor_name) {
    sold_per_x &lt;- data.frame(factor, train$SalePrice)
    colnames(sold_per_x) &lt;- c(factor_name, "SalePrice")
    # create scatterplot
    p &lt;- ggplot(sold_per_x) + geom_point(aes(x = factor, y = SalePrice))
    # add mean and confidence intervall
    p &lt;- p + geom_smooth(aes(factor))
    # add title
    p &lt;- p + ggtitle(paste(factor_name, "SalePrice", sep = " vs. ")) + xlab(factor_name)
    return(p)
}

# function to do a boxplot for a parameter
box_hyperparameter &lt;- function(results, parameter, parameter_name) {
    grid &lt;- sort(unique(parameter))
    # create boxplot
    p &lt;- ggplot(results) + geom_boxplot(aes(x = parameter, y = rmse, group = parameter, fill = parameter))
    # add title
    p &lt;- p + ggtitle(paste(parameter_name, "RMSE of log y", sep = " vs. "))
    # add colours
    p &lt;- p + theme(legend.position = "none") + xlab(parameter_name) + scale_x_discrete(limits = c(grid))
    return(p)
}

# function to plot two different parameters in a headmap according to their rmse results
hyperparameter_heatmap &lt;- function(results, parameter1, parameter2, name1, name2) {
    grid1 &lt;- sort(unique(parameter1))
    grid2 &lt;- sort(unique(parameter2))
    p &lt;- ggplot(results, aes(parameter1, parameter2)) + geom_raster(aes(fill = rmse), interpolate = F)
    p &lt;- p + xlab(name1) + ylab(name2)
    p &lt;- p + scale_x_discrete(limits = c(grid1)) + scale_y_discrete(limits = c(grid2))
    return(p)
}

# TODO:correlation plot of all variables
cor_plot &lt;- function(x, title) {
    corrgram(x, order = NULL, lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt, 
        main = title)
}
