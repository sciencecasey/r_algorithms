source("../functions/normalize_minmax.R")
source("../functions/mahal_remove.R")
source("../functions/bayesClassify.R")
source("../functions/parzenWindow.R")
source("../functions/prob_neur_net.R")
source("../functions/rbf_neur_net.R")
source("../functions/cross_validate_separations.R")
source("../functions/confusion_matrix.R")
# turned in version
# source("normalize_minmax.R")
# source("bayesClassify.R")
# source("parzenWindow.R")
# source("mahal_remove.R")
# source("prob_neur_net.R")
# source("rbf_neur_net.R")
# source("cross_validate_separations.R")
# source("confusion_matrix.R")
# Data Cleaning
# Min-Max normalization
# cleaned_pca_less <- normalize_by_obs(pca_less)
# by feature
pca_less <- readRDS("out_data/select_pca_all.RDS")
inds <- readRDS("out_data/indx_for_pca_less_select_pa.RDS")
cleaned_pca_less <- normalize_by_feature(pca_less)
min(cleaned_pca_less)
max(cleaned_pca_less)

# Mahalanobis outlier romoval
# make a routine of above for each subsequent df
# change the indeces!
dim(cleaned_pca_less)
class <- rep(-1, 10000)
#indeces <- list(indx0, indx1, indx2, indx3, indx4, indx5, indx6, indx7, indx8, indx9)
indeces <- inds
for(i in 1:10){
    selected <- which(indeces[[i]])
    class[selected] <- (i - 1)
}
# append the classes to the data
cleaned_pca_less <- as.data.frame(cleaned_pca_less)
cleaned_pca_less$class <- class

# outlier removal by class
temp0 <- mahal_remove(cleaned_pca_less[indx0,-11], i=1:10, alpha = .01)
temp0[11] <- 0
temp1 <- mahal_remove(cleaned_pca_less[indx1,-11], i=1:10, alpha = .01)
temp1[11] <- 1
temp0 <- rbind(temp0[,1:11], temp1[,1:11])
temp1 <- mahal_remove(cleaned_pca_less[indx2,-11], i=1:10, alpha = .01)
temp1[11] <- 2
temp0 <- rbind(temp0, temp1[,1:11])
temp1 <- mahal_remove(cleaned_pca_less[indx3,-11], i=1:10, alpha = .01)
temp1[11] <-3
temp0 <- rbind(temp0, temp1[,1:11])
temp1 <- mahal_remove(cleaned_pca_less[indx4,-11], i=1:10, alpha = .01)
temp1[11] <- 4
temp0 <- rbind(temp0, temp1[,1:11])
temp1 <- mahal_remove(cleaned_pca_less[indx5,-11], i=1:10, alpha = .01)
temp1[11] <- 5
temp0 <- rbind(temp0, temp1[,1:11])
temp1 <- mahal_remove(cleaned_pca_less[indx6,-11], i=1:10, alpha = .01)
temp1[11] <- 6
temp0 <- rbind(temp0, temp1[,1:11])
temp1 <- mahal_remove(cleaned_pca_less[indx7,-11], i=1:10, alpha = .01)
temp1[11] <- 7
temp0 <- rbind(temp0, temp1[,1:11])
temp1 <- mahal_remove(cleaned_pca_less[indx8,-11], i=1:10, alpha = .01)
temp1[11] <- 8
temp0 <- rbind(temp0, temp1[,1:11])
temp1 <- mahal_remove(cleaned_pca_less[indx9,-11], i=1:10, alpha = .01)
temp1[11] <- 9
temp0 <- rbind(temp0, temp1[,1:11])
cleaned_pca_less <- temp0
rm(temp0)
rm(temp1)
dim(cleaned_pca_less) # 9558



# remove the stats columns (flags outside distance and )
# cleaned_pca_less <- cleaned_pca_less[,-c(12, 13)]

# udpdate the indeces!
# grab indeces
indx0 <- cleaned_pca_less[,11] == 0
indx1 <- cleaned_pca_less[,11] == 1
indx2 <- cleaned_pca_less[,11] == 2
indx3 <- cleaned_pca_less[,11] == 3
indx4 <- cleaned_pca_less[,11] == 4
indx5 <- cleaned_pca_less[,11] == 5
indx6 <- cleaned_pca_less[,11] == 6
indx7 <- cleaned_pca_less[,11] == 7
indx8 <- cleaned_pca_less[,11] == 8
indx9 <- cleaned_pca_less[,11] == 9

# recheck the separation without outliers
plot(cleaned_pca_less[indx1,2]~cleaned_pca_less[indx1,1], col = cl[10], lwd = 3,
     xlim = c(0, 1), ylim = c(0, 1),
     main = "MinMaxNorm & Mahalanobis",
     xlab = "PC 2", ylab = "PC 1")
points(cleaned_pca_less[indx3,2]~cleaned_pca_less[indx3,1], col = cl[20], lwd = 3)
points(cleaned_pca_less[indx9,2]~cleaned_pca_less[indx9,1], col = cl[30], lwd = 3)
points(cleaned_pca_less[indx0,2]~cleaned_pca_less[indx0,1], col = cl[50], lwd = 3)
points(cleaned_pca_less[indx8,2]~cleaned_pca_less[indx8,1], col = cl[200], lwd = 3)

# export the normalized and cleaned data
# saveRDS(cleaned_pca_less, "out_data/cleaned_pca_less_no_outliers.RDS")

# Use Bayes Classifier 
classified <- bayes_classifier(cleaned_pca_less[,-11], 
                            classList = list(which(indx0), which(indx1), 
                                             which(indx2), which(indx3), 
                                             which(indx4), which(indx5), 
                                             which(indx6), which(indx7),
                                             which(indx8), which(indx9)), 
                            percentTrain = .8)
bayes_classifier.accuracy(classified, test_list = classified$test_classList)
# 91 % ! not bad :) 

# now apply parzen window
classList = list(which(indx0), which(indx1), 
                 which(indx2), which(indx3), 
                 which(indx4), which(indx5), 
                 which(indx6), which(indx7),
                 which(indx8), which(indx9))
windowed <- parz_window(test_data = cleaned_pca_less[,-11], 
            classList =  classList,
            #classList = list(which(indx0)),
            spread = .2, train_amt = .8)
dim(windowed)
parzen_accuracy(windowed, 10)
# 83.6 % accuracy

# try the lda built in MASS library
# requires that we use a factor for classes
unique(cleaned_pca_less[,11])
train <- sample(1:dim(cleaned_pca_less)[1], dim(cleaned_pca_less)*.8)
library(MASS)
linear_disc <- lda(flag_ ~ ., cleaned_pca_less, subset = train)
predicted <- predict(linear_disc, cleaned_pca_less[-train,])
true_class <- cleaned_pca_less[,11]
sum(predicted$class == true_class[-train])/length(true_class[-train])
# 80% accuracty


# implement the PNN
prob_net <- pnn(cleaned_pca_less[,-11], classList, normalize = FALSE)
prob_net$accuracyprob_net
# 92% !
# realized this was normalized by feature, try by observation
# Normalize by observation
# grab original indeces
indx0o <- coefs[,1] == 0
indx1o <- coefs[,1] == 1
indx2o <- coefs[,1] == 2
indx3o <- coefs[,1] == 3
indx4o <- coefs[,1] == 4
indx5o <- coefs[,1] == 5
indx6o <- coefs[,1] == 6
indx7o <- coefs[,1] == 7
indx8o <- coefs[,1] == 8
indx9o <- coefs[,1] == 9

# Note I named this incorrectly - fixed below!!
cleaned_pca_less_byfeature <- normalize_by_obs(pca_less)
# outlier removal by class
temp0 <- mahal_remove(cleaned_pca_less_byfeature[indx0o,-11], i=1:10, alpha = .01)
temp0[11] <- 0
temp1 <- mahal_remove(cleaned_pca_less_byfeature[indx1o,-11], i=1:10, alpha = .01)
temp1[11] <- 1
temp0 <- rbind(temp0[,1:11], temp1[,1:11])
temp1 <- mahal_remove(cleaned_pca_less_byfeature[indx2o,-11], i=1:10, alpha = .01)
temp1[11] <- 2
temp0 <- rbind(temp0, temp1[,1:11])
temp1 <- mahal_remove(cleaned_pca_less_byfeature[indx3o,-11], i=1:10, alpha = .01)
temp1[11] <-3
temp0 <- rbind(temp0, temp1[,1:11])
temp1 <- mahal_remove(cleaned_pca_less_byfeature[indx4o,-11], i=1:10, alpha = .01)
temp1[11] <- 4
temp0 <- rbind(temp0, temp1[,1:11])
temp1 <- mahal_remove(cleaned_pca_less_byfeature[indx5o,-11], i=1:10, alpha = .01)
temp1[11] <- 5
temp0 <- rbind(temp0, temp1[,1:11])
temp1 <- mahal_remove(cleaned_pca_less_byfeature[indx6o,-11], i=1:10, alpha = .01)
temp1[11] <- 6
temp0 <- rbind(temp0, temp1[,1:11])
temp1 <- mahal_remove(cleaned_pca_less_byfeature[indx7o,-11], i=1:10, alpha = .01)
temp1[11] <- 7
temp0 <- rbind(temp0, temp1[,1:11])
temp1 <- mahal_remove(cleaned_pca_less_byfeature[indx8o,-11], i=1:10, alpha = .01)
temp1[11] <- 8
temp0 <- rbind(temp0, temp1[,1:11])
temp1 <- mahal_remove(cleaned_pca_less_byfeature[indx9o,-11], i=1:10, alpha = .01)
temp1[11] <- 9
temp0 <- rbind(temp0, temp1[,1:11])
cleaned_pca_less_byfeature <- temp0
rm(temp0)
rm(temp1)
dim(cleaned_pca_less_byfeature)

# udpdate the indeces!
# grab indeces
indx0o <- cleaned_pca_less_byfeature[,11] == 0
indx1o <- cleaned_pca_less_byfeature[,11] == 1
indx2o <- cleaned_pca_less_byfeature[,11] == 2
indx3o <- cleaned_pca_less_byfeature[,11] == 3
indx4o <- cleaned_pca_less_byfeature[,11] == 4
indx5o <- cleaned_pca_less_byfeature[,11] == 5
indx6o <- cleaned_pca_less_byfeature[,11] == 6
indx7o <- cleaned_pca_less_byfeature[,11] == 7
indx8o <- cleaned_pca_less_byfeature[,11] == 8
indx9o <- cleaned_pca_less_byfeature[,11] == 9

# export RDS
saveRDS(cleaned_pca_less_byfeature, "out_data/cleaned_pca_byobs.RDS")

# Rerun prob net
classListo = list(which(indx0o), which(indx1o), 
                 which(indx2o), which(indx3o), 
                 which(indx4o), which(indx5o), 
                 which(indx6o), which(indx7o),
                 which(indx8o), which(indx9o))
prob_net <- pnn(cleaned_pca_less_byfeature[,-11], classListo, normalize = FALSE)
# rerun accuracy

# implement RBF without bias
rbf_net <- rbf(cleaned_pca_less[,-11], classList, normalize = FALSE)
rbf_accuracy(rbf_net)
# 92 % ! :D 

# implement SVM
library(e1071)
train <- sample(1:dim(cleaned_pca_less)[1], dim(cleaned_pca_less)*.8)
supportive <- svm(x = cleaned_pca_less[train,-11], 
                  y = as.factor(cleaned_pca_less[train,11]), 
                  # scale = FALSE,
                  kernel = "radial", 
                  type = "C-classification")
print(supportive)
summary(supportive)
predicted <- fitted(supportive)
sum(predicted == as.factor(cleaned_pca_less[train, 11]))/length(predicted)
# the model is 94% accurate

# test the unfit data
predicted <- predict(supportive, cleaned_pca_less[-train, -11])
sum(predicted == as.factor(cleaned_pca_less[-train, 11]))/length(predicted)
# also 92% accurate!
