#!/usr/bin/env Rscript
library(optparse)
library(plyr)
library(mvtnorm)
library(data.table)
library(caret)
library(e1071)
source('0.init.R')
option_list <- list(make_option(c("-d", "--dataset"), type="character", default='skin',
                                 help="dataset file name"),
                    make_option(c("-f", "--datafolder"), type="character", default='./data',
                                help="dataset file name"),
                    make_option(c("-l", "--lognorm"), type="numeric", default=5,
                                help="log2 of maximal norm of model coefficients"),
                    make_option(c("-b", "--basemodel"), type="numeric", default=500,
                                help="number of base nmodels (unit norm)"),
                    make_option(c("-a", "--alg"), type="character", default='mulactive',
                                help="type of algorithm"),
                    make_option(c("-r", "--out_directory"), type="character", default=".",
                                help="whether to save output files")
                    )

opt_parser <- OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

FLAGS <- opt
n_warmup <- 50    # warmup rounds, keep requesting label

#--------------------
# Load data
#--------------------
datafile <- paste0(FLAGS$datafolder, '/', FLAGS$dataset, '.RData')
load(datafile)
nT <- nrow(X)

for (rep in c(1:20)){
    opt2 <- as.list(FLAGS) ;opt2$datafolder <- NULL ;opt2$otb <- NULL ;opt2$help <- NULL ;opt2$out_directory <- NULL
    basefilename <- paste0(paste0(names(opt2),'_',opt2), collapse = '_')
    filename <- paste0(FLAGS$out_directory,'/',basefilename, '_otb_rep',rep,'.csv')
    if(file.exists(filename)){next}

    set.seed(rep); shuffle <- sample(nrow(X),nrow(X),replace = FALSE)
    ntrain <- floor(nT * 0.8); ntest <- nT - ntrain
    trainX_tmp <- X[shuffle[1:ntrain],]; testX_tmp <- X[shuffle[-c(1:ntrain)],]
    col_to_keep <- apply(trainX_tmp, 2, var, na.rm=TRUE)  != 0
    trainX_tmp  <- trainX_tmp[,col_to_keep]; testX_tmp  <- testX_tmp[,col_to_keep];
    trainy <- y[shuffle[1:ntrain]];  testy <- y[shuffle[-c(1:ntrain)]]
    #traink <- k[shuffle[1:ntrain]];  testk <- k[shuffle[-c(1:ntrain)]]

    # rescale X and testX
    preprop <- preProcess(trainX_tmp,method=c('center','scale','pca'),pcaComp=10)
    trainX <- predict(preprop, trainX_tmp)
    testX <- predict(preprop, testX_tmp) 
    trainX <- cbind(1,as.matrix(trainX)); testX <- cbind(1,as.matrix(testX))

    # take first 50 and train svm
#    model_lr <- glm.fit(trainX[1:n_warmup,],0.5+0.5*trainy[1:n_warmup],family=binomial(link='logit'))
#    h0 <- model_lr$coefficients; h0 <- h0/sqrt(sum(h0^2))
#    model_svm <- svm(trainX[1:n_warmup,-1], trainy[1:n_warmup], kernel='linear', scale = FALSE)
#    w0 <- t(model_svm$coefs) %*% model_svm$SV;
#    h0 <- c(-model_svm$rho, w0)
#    h0 <- h0/sqrt(sum(h0^2))

    scales <- 2^seq(0,FLAGS$lognorm,1)
    all_h <- gen_all_h(num_dim=ncol(trainX), num_base_models=FLAGS$basemodel, scales)#, h0=h0)
    nh <- nrow(all_h)

    # When models' norm scales, scales thre as well.
    X_norm <- apply(trainX,1,function(x){sqrt(sum(x^2))})
    max_x_norm <- quantile(X_norm,0.95)
    if (max_x_norm * max(scales) > 50){ M <- max_x_norm * max(scales) } else{
        M <- log(1+exp(max_x_norm*max(scales)))}# upper bound of logistic loss

    cum_loss_misclass <- 0 # cumulative 0/1 error of the prediction for this round
    cum_loss_logistic <- 0 # cumulative logistics error of prediction for this round
    cum_loss_al <- 0 # cumulative active learning loss: req_cost if request, loss if not request
    cum_label <- 0 # cumulative number of label requests

    checkpoint <- min(100,floor(nT / (100*10)) * 100)
    if (checkpoint==0){checkpoint <- 25}

    # rebuild k
    set.seed(40)
    r_per_h <- 10; Xcol <- ncol(trainX)
    rand_planes <- matrix(rnorm(Xcol*r_per_h), ncol=Xcol)
    rand_planes <- rand_planes / sqrt(rowSums(rand_planes^2))
    train_dist <- abs(as.matrix(trainX) %*% t(rand_planes))
    traink <- apply(train_dist,1,which.min)
    test_dist <- abs(as.matrix(testX) %*% t(rand_planes))
    testk <- apply(test_dist,1,which.min)

    cum_loss <- matrix(0,nrow=nh,ncol=r_per_h)
    Ht <- matrix(TRUE,nrow=nh,ncol=r_per_h)
    cum_samples <- rep(0,r_per_h)
    cum_obs <- rep(0, r_per_h)

    RET_iwal <- matrix(c(0),ncol = 7) # book keeping
    OTB_iwal <- matrix(c(0),ncol = 4) # book keeping
    last_i <- 0
    last_cum_label <- 0

    for (i in seq_len(ntrain)){
        x_t <- trainX[i,]; y_t <- trainy[i]; k_t <- traink[i]
        pred_t <- all_h %*% x_t
        cum_samples[k_t] <- cum_samples[k_t] +1
        if (i <= n_warmup){
            p_t <- 1
        } else{
            pred_max <- max(pred_t[Ht[,k_t]])
            pred_min <- min(pred_t[Ht[,k_t]])
            p_t <- max(log(1+exp(-pred_min)) - log(1+exp(-pred_max)),
                       log(1+exp(pred_max)) - log(1+exp(pred_min))) / M
        }
        Q_t <- as.numeric(runif(1) < p_t)
        if (Q_t > 0){
            cum_obs[k_t] <- cum_obs[k_t]+1; cum_label <- cum_label + 1 
            cum_loss[,k_t] <- cum_loss[,k_t] + loss_func(pred_t,y_t,'logistic')/(M*p_t) # importance weighted cum_loss
            min_err <- min((cum_loss[,k_t])[Ht[,k_t]])
            T_t <- cum_samples[k_t]
            slack_t <- sqrt(T_t*log(T_t+1)) # a more aggresive slack term than IWAL paper
            Ht[,k_t] <- (Ht[,k_t]  & cum_loss[,k_t] <= min_err + slack_t)
        } 

        It <- (seq_len(nh)[Ht[,k_t]])[which.min((cum_loss[,k_t])[Ht[,k_t]])]
        cum_loss_misclass <- cum_loss_misclass + loss_func(pred_t[It],y_t,'misclass')
        cum_loss_logistic <- cum_loss_logistic + loss_func(pred_t[It],y_t)


        if (i!= last_i & cum_label != last_cum_label & (i %% checkpoint ==0 || cum_label %% checkpoint == 0)){
            last_i <-  i
            last_cum_label <- cum_label
            cat('num of rounds:',i, ', num of labels:',cum_label, '\n')

            opt_Its <- rep(0,r_per_h)
            for(r in c(1:r_per_h)){ opt_Its[r] <- (seq_len(nh)[Ht[,r]])[which.min((cum_loss[,r])[Ht[,r]])] }
            curr_otb <- mul_otb(testX, testy, all_h, testk, opt_Its)
            OTB_iwal <- rbind(OTB_iwal, c(i, cum_label, curr_otb))
        }
    }

    opt_Its <- rep(0,r_per_h)
    for(r in c(1:r_per_h)){ opt_Its[r] <- (seq_len(nh)[Ht[,r]])[which.min((cum_loss[,r])[Ht[,r]])] }
    curr_otb <- mul_otb(testX, testy, all_h, testk, opt_Its)
    OTB_iwal <- rbind(OTB_iwal, c(i, cum_label, curr_otb))
    colnames(OTB_iwal) <- c('round','labels','loss_misclass','loss_logistic')

    # save to file
    opt2 <- as.list(FLAGS) ;opt2$datafolder <- NULL ;opt2$otb <- NULL ;opt2$help <- NULL ;opt2$out_directory <- NULL
    basefilename <- paste0(paste0(names(opt2),'_',opt2), collapse = '_')
    filename <- paste0(FLAGS$out_directory,'/',basefilename, '_otb_rep',rep,'.csv')
    write.table(OTB_iwal,filename, sep = ',', row.names = FALSE)
}
