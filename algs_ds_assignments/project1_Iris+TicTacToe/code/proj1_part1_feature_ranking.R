rank_feat<- function(dataFrame, class){
    #' @param dataFrame should have all numeric columns of features
    #' @param class should be a list containing the numeric range for each class
    #' @return a vector of the FDR values in ranked order, with colnames (or numeric indeces) attached
    #' these can be interpreted with largest value as rank 1 and smallest value as lowest rank
    rank = rep(0, dim(dataFrame)[2])
    for (feature in seq(1, length(rank))) {
        fdr = 0
        for (i in seq(1, length(class))) {
            tot = 0
            muI = mean(dataFrame[class[[i]],feature])
            varI = var(dataFrame[class[[i]],feature])
            for (j in seq(i+1,length(class))) {
                if(i!=j && j<= length(class)){
                    muJ = mean(dataFrame[class[[j]],feature])
                    varJ = var(dataFrame[class[[j]],feature])
                    tot = tot + ((muI-muJ)^2/(varI+varJ))
                }
            } # end class j
            # add to overall for feature
            fdr = fdr + tot
        } # end class j
        rank[feature] = fdr
    } # end feature
    if(!is.null(colnames(dataFrame))){
        # give name to rank
        names(rank) <- colnames(dataFrame)
    }else{
        names(rank) <- seq(1, length(rank))
    }
    rank <- (rank[order(rank, decreasing = TRUE)])
    return(rank)
}


# run the feature ranking function on iris without outliers
str(no_out_iris)
lens <- dim(no_out_iris[,5] %>% subset(Species == "setosa"))[1]
lenv <- dim(no_out_iris[,5] %>% subset(Species == "versicolor"))[1]
lenvir <- dim(no_out_iris[,5] %>% subset(Species == "virginica"))[1]
class <- list(c(1:lens), c((lens+1):(lens+lenv)), c((lens+lenv+1):(lens+lenv+lenvir)))
class
rm(list= c("lens", "lenv", "lenvir"))
no_out_iris <- as.data.frame(no_out_iris)
rank <- rank_feat(no_out_iris[-5], class)
# the highest rank is petal_length, graph by class
library(lattice)
densityplot(data = no_out_iris, Petal_Length, groups = Species, auto.key = TRUE, plot.points = FALSE)
densityplot(data = no_out_iris, Petal_Width, groups = Species, auto.key = TRUE, plot.points = FALSE)
densityplot(data = no_out_iris, Sepal_Length, groups = Species, auto.key = TRUE, plot.points = FALSE)
densityplot(data = no_out_iris, Sepal_Width, groups = Species, auto.key = TRUE, plot.points = FALSE)



