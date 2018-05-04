#!/usr/bin/env Rscript
library(optparse)
library(plyr)
library(mvtnorm)
library(data.table)
library(caret)
source('0.init.R')
option_list <- list(make_option(c("-d", "--dataset"), type="character", default='skin',
                                 help="dataset file name"),
                    make_option(c("-f", "--datafolder"), type="character", default='./data',
                                help="dataset file name"),
                    make_option(c("-l", "--lognorm"), type="numeric", default=5,
                                help="log2 of maximal norm of model coefficients"),
                    make_option(c("-b", "--basemodel"), type="numeric", default=500,
                                help="number of base nmodels (unit norm)"),
                    make_option(c("-a", "--alg"), type="character", default='mulmaster',
                                help="type of algorithm"),
                    make_option(c("-r", "--out_directory"), type="character", default=".",
                                help="whether to save output files"),
                    make_option(c("-c", "--cost"), type="numeric", default=1,
                                help="label request cost")
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
ntrain <- floor(nT * 0.8); ntest <- nT - ntrain

# -- policy set and learning rate
num_policy <- 100
policy_set <- seq(0, FLAGS$cost/ntrain, length.out=num_policy)
gamma <- sqrt(log(num_policy)/(ntrain*(FLAGS$cost)^2))

for (rep in c(1:20)){

    set.seed(rep); shuffle <- sample(nrow(X),nrow(X),replace = FALSE)
    trainX_tmp <- X[shuffle[1:ntrain],]; testX_tmp <- X[shuffle[-c(1:ntrain)],]
    trainy <- y[shuffle[1:ntrain]];  testy <- y[shuffle[-c(1:ntrain)]]
    traink <- k[shuffle[1:ntrain]];  testk <- k[shuffle[-c(1:ntrain)]]

    # -- rescale X and testX
    preprop <- preProcess(trainX_tmp,method=c('center','scale'))
    trainX <- predict(preprop, trainX_tmp)
    testX <- predict(preprop, testX_tmp) 
    trainX <- as.matrix(trainX); testX <- as.matrix(testX)

    # -- take first 50 and get h0
#    model_lr <- glm.fit(trainX[1:n_warmup,],0.5+0.5*trainy[1:n_warmup],family=binomial(link='logit'))
#    h0 <- model_lr$coefficients; 
#    h0[is.na(h0)] <- 0; h0 <- h0/sqrt(sum(h0^2))
    model_svm <- svm(trainX[1:n_warmup,], trainy[1:n_warmup], kernel='linear', scale = FALSE)
    h0 <- t(model_svm$coefs) %*% model_svm$SV

    # -- sample hypotheses not too far away from h0
    scales <- 2^seq(0,FLAGS$lognorm,1)
    all_h <- gen_all_h(num_dim=ncol(X), num_base_models=FLAGS$basemodel, scales, h0=h0)
    nh <- nrow(all_h)

    # --  When models' norm scales, scales thre as well.
    max_x_norm <- quantile(apply(trainX,1,function(x){sqrt(sum(x^2))}),0.95)
    if (max_x_norm * max(scales) > 50){ M <- max_x_norm * max(scales) } else{
        M <- log(1+exp(max_x_norm*max(scales)))}# upper bound of logistic loss

    # -- book keeping
    r_per_h <- 10
    cum_loss <- matrix(0,nrow=nh,ncol=r_per_h)
    Ht <- matrix(TRUE,nrow=nh,ncol=r_per_h)
    cum_samples <- cum_accepts  <- rep(0.5, r_per_h) # incoming unlabeled; passed on to slave; 
    cum_labels <- rep(0, r_per_h)
    exp4_w <- rep(1, num_policy)

    cum_loss_misclass <- cum_loss_logistic <- cum_loss_al <- It <- 0 ## meaningless stuff

    checkpoint <- min(100,floor(nT / (100*10)) * 100)
    if (checkpoint==0){checkpoint <- 25}

    OTB_iwal <- matrix(c(0),ncol = 4) # book keeping
    last_i <- 0
    last_cum_label <- 0

    for (i in seq_len(ntrain)){
        x_t <- trainX[i,]; y_t <- trainy[i]; k_t <- traink[i]; pred_t <- all_h %*% x_t
        cum_samples[k_t] <- cum_samples[k_t] +1
        if (i <= n_warmup){
            cum_labels[k_t] <- cum_labels[k_t]+1
            cum_accepts[k_t] <- cum_accepts[k_t]+1
            cum_loss[,k_t] <- cum_loss[,k_t] + loss_func(pred_t,y_t,'logistic')/(M) # importance weighted cum_loss

            p_tmp <- cum_samples/sum(cum_samples)
            reg_tmp <- sqrt(log(1+cum_accepts)/(cum_accepts+1)) 
            obj_tmp <- p_tmp * reg_tmp + (FLAGS$cost/ntrain) * cum_labels
            objs <- obj_tmp
        } else{
            # Expert advice
            p_tmp <- cum_samples/sum(cum_samples)
            reg_diff_tmp <- sqrt(log(cum_accepts+1)/(cum_accepts+1)) - sqrt(log(cum_accepts+2)/(cum_accepts+2)) 
            req_prop_tmp <- cum_labels/cum_accepts
            advice_t <- as.numeric((p_tmp*reg_diff_tmp)[k_t] - FLAGS$cost/ntrain * (req_prop_tmp)[k_t] > policy_set)

            # Vote
            pass_prob <- (1-gamma)*sum(exp4_w * advice_t)/sum(exp4_w) + gamma/2
            action_t <- runif(1) < pass_prob

            if (action_t){
                cum_accepts[k_t] <- cum_accepts[k_t]+1

                pred_max <- max(pred_t[Ht[,k_t]])
                pred_min <- min(pred_t[Ht[,k_t]])
                p_t <- max(log(1+exp(-pred_min)) - log(1+exp(-pred_max)),
                           log(1+exp(pred_max)) - log(1+exp(pred_min))) / M
                Q_t <- as.numeric(runif(1) < p_t)

                if (Q_t > 0){
                    cum_labels[k_t] <- cum_labels[k_t]+1; 
                    cum_loss[,k_t] <- cum_loss[,k_t] + loss_func(pred_t,y_t,'logistic')/(M*p_t) # importance weighted cum_loss
                    min_err <- min((cum_loss[,k_t])[Ht[,k_t]])
                    T_t <- cum_accepts[k_t]
                    slack_t <- sqrt(T_t*log(T_t+1)) # a more aggresive slack term than IWAL paper
                    Ht[,k_t] <- (Ht[,k_t]  & cum_loss[,k_t] <= min_err + slack_t)
                } 
            }

            # Loss
            p_tmp <- cum_samples/sum(cum_samples)
            reg_tmp <- sqrt(log(1+cum_accepts)/(cum_accepts+1)) 
            obj_tmp <- p_tmp * reg_tmp + (FLAGS$cost/ntrain) * cum_labels

            # Make EXP4 updates
            loss_t <- i * sum(obj_tmp) - (i-1)*sum(objs)
            loss_t_policy <- rep(loss_t/pass_prob, num_policy); 
            loss_t_policy[advice_t != as.numeric(action_t)] <- 0
            exp4_w <- exp4_w * exp(-gamma*loss_t_policy/2); exp4_w <- exp4_w / sum(exp4_w)

            objs <- obj_tmp
        }

        CUM_LABELS <- sum(cum_labels)
        if (i!= last_i & CUM_LABELS != last_cum_label & (i %% checkpoint ==0 || CUM_LABELS %% checkpoint == 0)){
            last_i <-  i
            last_cum_label <- CUM_LABELS
            cat('num of rounds:',i, ', num of labels:',CUM_LABELS, '\n')

            opt_Its <- rep(0,r_per_h)
            for(r in c(1:r_per_h)){ opt_Its[r] <- (seq_len(nh)[Ht[,r]])[which.min((cum_loss[,r])[Ht[,r]])] }
            curr_otb <- mul_otb(testX, testy, all_h, testk, opt_Its)
            OTB_iwal <- rbind(OTB_iwal, c(i, CUM_LABELS, curr_otb))
        }
    }

    opt_Its <- rep(0,r_per_h)
    for(r in c(1:r_per_h)){ opt_Its[r] <- (seq_len(nh)[Ht[,r]])[which.min((cum_loss[,r])[Ht[,r]])] }
    curr_otb <- mul_otb(testX, testy, all_h, testk, opt_Its)
    OTB_iwal <- rbind(OTB_iwal, c(i, CUM_LABELS, curr_otb))

    # save to file
    opt2 <- as.list(FLAGS)
    opt2$datafolder <- NULL
    opt2$otb <- NULL
    opt2$help <- NULL
    opt2$out_directory <- NULL
    basefilename <- paste0(paste0(names(opt2),'_',opt2), collapse = '_')

    filename <- paste0(FLAGS$out_directory,'/',basefilename, '_otb_rep',rep,'.csv')
    colnames(OTB_iwal) <- c('round','labels','loss_misclass','loss_logistic')
    write.table(OTB_iwal,filename, sep = ',', row.names = FALSE)
}
