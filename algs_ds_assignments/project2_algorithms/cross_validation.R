source("../functions/cross_validate_separations.R")
source("../functions/confusion_matrix.R")
source("../functions/bayesClassify.R")
library(e1071)
library(MASS)

# get the indeces for the cross validation
cleaned_pca_less <- readRDS("out_data/cleaned_pca_less_no_outliers.RDS")

# this classList was created from cleaned_pca_less[,11] class column
classList = list(which(indx0), which(indx1), 
                 which(indx2), which(indx3), 
                 which(indx4), which(indx5), 
                 which(indx6), which(indx7),
                 which(indx8), which(indx9))
validations <- cross_validate(classList, 5)
validations$number_remaining_perclass
# save the validation split used
# saveRDS(validations, "out_data/validation_split_used_time2.RDS")
validations <- readRDS("out_data/validation_split_used_time2.RDS")

# run through the cross validation
# note I'm only uing 3,500 total observations due to computational cost
# (70*5*10 = 3,500)
validations$cross_val <- lapply(validations$cross_val, function(x) x[1:70,])
results <- list(bayes = list(), 
                avg_bayes_acc = 0, 
                svm = list(), 
                avg_svm_acc = 0, 
                lda = list(),
                avg_lda_acc = 0)
for(validation_num in 1:5){
    test_class_list <- list()
    train_class_list <- list()
    test <- rep(0, 11) # nuber of cols in cleaned_pca_less (our overall data)
    train <- rep(0, 11)
    #for(matrix_num in length(validations$cross_val)){
    for(matrix_num in 1:10){
        # save the class lists to pass to functions
        matrix_ <- validations$cross_val[[matrix_num]][1:70,]
        test_class_list[[matrix_num]] <- matrix_[,validation_num]
        temp <- matrix_[1:70,-validation_num]  # grab all other indeces
        train_class_list[[matrix_num]] <- matrix(temp, nrow = 1)
        test <- rbind(test, 
                      cleaned_pca_less[test_class_list[[matrix_num]], ]) # grab all test rows
        train <- rbind(train, 
                       cleaned_pca_less[train_class_list[[matrix_num]], ]) # grab train rows
    }
    # remove the first row of zeros
    test <- test[-1,]
    train <- train[-1,]
    test_class_list <- list(which(test[,11] == 0),
                          which(test[,11] == 1),
                          which(test[,11] == 2),
                          which(test[,11] == 3),
                          which(test[,11] == 4),
                          which(test[,11] == 5),
                          which(test[,11] == 6),
                          which(test[,11] == 7),
                          which(test[,11] == 8),
                          which(test[,11] == 9))
    train_class_list <- list(which(train[,11] == 0),
                             which(train[,11] == 1),
                             which(train[,11] == 2),
                             which(train[,11] == 3),
                             which(train[,11] == 4),
                             which(train[,11] == 5),
                             which(train[,11] == 6),
                             which(train[,11] == 7),
                             which(train[,11] == 8),
                             which(train[,11] == 9))
    # bayes
    bayes <- bayes_classifier.separated(test = test[,-11],
                                        training = train[,-11],
                                        testClassList = test_class_list,
                                        trainClassList = train_class_list)
    # accuracy
    acc <- bayes_classifier.accuracy(bayes, test_list = test_class_list)
    results$avg_bayes_acc <- results$avg_bayes_acc + acc
    # confusion matrix
    bayes <- confusion_mat(true_class = test[,11], predicted_class = bayes$assigned)
    results$bayes[[validation_num]] <- bayes
    # svm
    svm_mod <- svm(x = train[,-11], 
               y = as.factor(train[,11]), 
               # scale = FALSE, 
               type = "C-classification",
               kernel = 'radial')
    sv <- predict(svm_mod, test[,-11])
    # accuracy
    results$avg_svm_acc <- results$avg_svm_acc + sum(sv == as.factor(test[,11]))/length(test[,11])
    sv <- confusion_mat(true_class = test[,11], predicted_class = sv)
    results$svm[[validation_num]] <- sv
    # lda
    lda_mod <- lda(x = train[,-11], 
                   grouping = as.factor(train[,11]))
    ld <- predict(lda_mod, test[,-11])
    # accuracy
    results$avg_lda_acc <- results$avg_lda_acc + sum(ld$class == as.factor(test[,11]))/length(test[,11])
    # confusion matrix
    ld <- confusion_mat(true_class = as.factor(test[,11]), predicted_class = ld$class)
    results$lda[[validation_num]] = ld
    if(validation_num == 5){
        # take average
        results$avg_bayes_acc <- results$avg_bayes_acc/5
        results$avg_lda_acc <- results$avg_lda_acc/5
        results$avg_svm_acc <- results$avg_svm_acc/5
    }
}
results$avg_svm_acc # 92
results$avg_lda_acc # 79
results$avg_bayes_acc # 90.7
lapply(results$bayes, function(x) x$confusion_stastics)
bayes_cmat <- 0
bayes_stat <- 0
lda_cmat <- 0
lda_stat <- 0
svm_cmat <- 0
svm_stat <- 0
for(i in 1:5){
    bayes_cmat <- bayes_cmat + results$bayes[[i]]$confusion_matrx
    bayes_stat <- bayes_stat + results$bayes[[i]]$confusion_statistics
    lda_cmat <- lda_cmat + results$lda[[i]]$confusion_matrx
    lda_stat <- lda_stat + results$lda[[i]]$confusion_statistics
    svm_cmat <- svm_cmat + results$svm[[i]]$confusion_matrx
    svm_stat <- svm_stat + results$svm[[i]]$confusion_statistics
    if(i == 5){
        bayes_stat <- bayes_stat/5
        lda_stat <- lda_stat/5
        svm_stat <- svm_stat/5
    }
}
bayes_cmat
bayes_stat
lda_stat
svm_stat
all <- rbind(bayes_stat, lda_stat, svm_stat)
all[1,2] <- results$avg_bayes_acc
all

rowSums(bayes_cmat)
rowSums(lda_cmat)
rowSums(svm_cmat)
