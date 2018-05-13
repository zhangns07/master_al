#!/usr/bin/env Rscript
library(optparse)
library(plyr)
library(mvtnorm)
library(data.table)
library(caret)
library(e1071)
source('0.init.R')

exp_of <- function(x){if (x>=100){return (x) }else{ return (log(1+exp(x))) } } # with overflow control
get_req_prob <- function(h, X, M){
    ret <- apply(X,1,function(x){
                     pred_t <- h %*% x
                     pred_max <- max(pred_t); pred_min <- min(pred_t)
                     p_t <- max(exp_of(-pred_max) - exp_of(-pred_max),# log(1+exp(-pred_min)) - log(1+exp(-pred_max)),
                                exp_of(pred_max) - exp_of(pred_min) )/M#log(1+exp(pred_max)) - log(1+exp(pred_min))) / M 
                     min(p_t,1) })
    mean(ret)
}


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
                                help="label request cost"),
                    make_option(c("-m", "--master"), type="numeric", default=1,
                                help="types of master algorithm. ")
                    )
# Master
# 1.EXP4, policy: threshold, 
# 2 [NOT GOOD].EXP4, policy: threshold 1 + optimal control via Langrange
# 3.Deterministic with expeccted probability of requesting.
# 4.Randomized version of 3, use same policy set of 1
# 5.Claudio's idea: each policy is a point in simplex, that corresponds to probability of passing samples on
# 6.Ranking: top [k] experts with largest p_k * reg_k
# 7.Ranking: top [k] experts with largest difference between reg / obs
# 8.Ranking: top [k] experts with largest decrease in reg since last obs
# 9.Ranking: top [k] experts with largest expected decrease in loss

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
r_per_h <- 10

# -- policy set and learning rate
num_policy <- 100
if (FLAGS$master==1){
    policy_set <- seq(0, FLAGS$cost/ntrain, length.out=num_policy)
} else if (FLAGS$master==4){
    policy_set <- seq(-FLAGS$cost/ntrain, FLAGS$cost/ntrain, length.out=num_policy)
} else if (FLAGS$master %in% c(6:9)){
    num_policy <- r_per_h+1
}
gamma <- sqrt(log(num_policy)/(ntrain*(FLAGS$cost)^2))

for (rep in c(1:20)){
    opt2 <- as.list(FLAGS) ;opt2$datafolder <- NULL ;opt2$otb <- NULL ;opt2$help <- NULL ;opt2$out_directory <- NULL
    basefilename <- paste0(paste0(names(opt2),'_',opt2), collapse = '_')
    filename <- paste0(FLAGS$out_directory,'/',basefilename, '_otb_rep',rep,'.csv')
    if (file.exists(filename)){next}

    set.seed(rep); shuffle <- sample(nrow(X),nrow(X),replace = FALSE)
    trainX_tmp <- X[shuffle[1:ntrain],]; testX_tmp <- X[shuffle[-c(1:ntrain)],]
    col_to_keep <- apply(trainX_tmp, 2, var, na.rm=TRUE)  != 0
    trainX_tmp  <- trainX_tmp[,col_to_keep]; testX_tmp  <- testX_tmp[,col_to_keep];
    trainy <- y[shuffle[1:ntrain]];  testy <- y[shuffle[-c(1:ntrain)]]
    #traink <- k[shuffle[1:ntrain]];  testk <- k[shuffle[-c(1:ntrain)]]

    # -- rescale X and testX
    preprop <- preProcess(trainX_tmp,method=c('center','scale','pca'),pcaComp=10)
    trainX <- predict(preprop, trainX_tmp)
    testX <- predict(preprop, testX_tmp) 
    trainX <- cbind(1,as.matrix(trainX)); testX <- cbind(1,as.matrix(testX))

    # -- take first 50 and get h0
#    model_lr <- glm.fit(trainX[1:n_warmup,],0.5+0.5*trainy[1:n_warmup],family=binomial(link='logit'))
#    h0 <- model_lr$coefficients; h0[is.na(h0)] <- 0; h0 <- h0/sqrt(sum(h0^2))
#    model_svm <- svm(trainX[1:n_warmup,-1], trainy[1:n_warmup], kernel='linear', scale = FALSE)
#    w0 <- t(model_svm$coefs) %*% model_svm$SV;
#    h0 <- c(-model_svm$rho, w0)
#    h0 <- h0/sqrt(sum(h0^2))

    # -- sample hypotheses not too far away from h0
    scales <- 2^seq(0,FLAGS$lognorm,1)
    all_h <- gen_all_h(num_dim=ncol(trainX), num_base_models=FLAGS$basemodel, scales)#, h0=h0)
    nh <- nrow(all_h)

    # -- generate uniform sample from simplex as policies
    if (FLAGS$master==5){
        tmp_set <- matrix(-log(runif(num_policy*r_per_h)),ncol=r_per_h)
        policy_set <- t(apply(tmp_set, 1, function(x){x/sum(x)}))
    }

    # --  When models' norm scales, scales thre as well.
    X_norm <- apply(trainX,1,function(x){sqrt(sum(x^2))})
    max_x_norm <- quantile(X_norm,0.95)
    if (max_x_norm * max(scales) > 50){ M <- max_x_norm * max(scales) } else{
        M <- log(1+exp(max_x_norm*max(scales)))}# upper bound of logistic loss

    # -- rebuild k
    set.seed(40)
    Xcol <- ncol(trainX)
    rand_planes <- matrix(rnorm(Xcol*r_per_h), ncol=Xcol)
    rand_planes <- rand_planes / sqrt(rowSums(rand_planes^2))
    train_dist <- abs(as.matrix(trainX) %*% t(rand_planes))
    traink <- apply(train_dist,1,which.min)
    test_dist <- abs(as.matrix(testX) %*% t(rand_planes))
    testk <- apply(test_dist,1,which.min)

    # --  prepare a  holdout unlabeled set
    req_prob_X <- testX[1:min(10000,ntest),]; req_prob_k <- testk[1:min(10000,ntest)]
    req_prob <- rep(1,r_per_h)
    cum_req_prob <- rep(0,r_per_h)
    reg_diff_obs <- rep(0,r_per_h)

    # -- book keeping
    cum_loss <- matrix(0,nrow=nh,ncol=r_per_h)
    Ht <- matrix(TRUE,nrow=nh,ncol=r_per_h); Ht_sum_old <- rep(nh, each=r_per_h)
    cum_samples <- cum_accepts  <- rep(0.5, r_per_h) # incoming unlabeled; passed on to slave; 
    last_cum_accepts  <- rep(0.5, r_per_h) # incoming unlabeled; passed on to slave; 
    cum_labels <- rep(0, r_per_h)
    exp_w <- rep(1, num_policy); exp_w <- exp_w / sum(exp_w)
    cum_loss_misclass <- cum_loss_logistic <- cum_loss_al <- It <- 0 ## meaningless stuff

    checkpoint <- min(100,floor(nT / (100*10)) * 100)
    if (checkpoint==0){checkpoint <- 25}

    RET_iwal <- matrix(c(0),ncol = 7) # book keeping
    OTB_iwal <- matrix(c(0),ncol = 4) # book keeping
    last_i <- 0
    last_cum_label <- 0

    for (i in seq_len(ntrain)){
        x_t <- trainX[i,]; y_t <- trainy[i]; k_t <- traink[i]; pred_t <- all_h %*% x_t
        cum_samples[k_t] <- cum_samples[k_t] +1
        if (i <= n_warmup || cum_accepts[k_t] <= n_warmup/r_per_h){
            cum_labels[k_t] <- cum_labels[k_t]+1
            cum_accepts[k_t] <- cum_accepts[k_t]+1
            cum_loss[,k_t] <- cum_loss[,k_t] + loss_func(pred_t,y_t,'logistic')/(M) # importance weighted cum_loss
            cum_req_prob[k_t] <- cum_req_prob[k_t] + req_prob[k_t]

            p_tmp <- cum_samples/sum(cum_samples)
            reg_tmp <- sqrt(log(1+cum_accepts)/(cum_accepts+1)) 
            if (FLAGS$master==1){ 
                objs <- p_tmp * reg_tmp + (FLAGS$cost/ntrain) * cum_labels
            } else if (FLAGS$master %in% c(4:9)){
                objs <- p_tmp * reg_tmp + (FLAGS$cost/ntrain) * cum_req_prob
            }

            if(FLAGS$master==8){
                currreg <- reg_tmp[k_t]
                pastreg <- (p_tmp * sqrt(log(1+last_cum_accepts)/(last_cum_accepts+1)))[k_t]
                reg_diff_obs[k_t] <- pastreg-currreg
                last_cum_accepts[k_t] <- cum_accepts[k_t]
            }

        } else{
            p_tmp <- cum_samples/sum(cum_samples)
            reg_diff_tmp <- sqrt(log(cum_accepts+1)/(cum_accepts+1)) - sqrt(log(cum_accepts+2)/(cum_accepts+2)) 
            req_prop_tmp <- cum_labels/cum_accepts
            reg_tmp <- p_tmp * sqrt(log(cum_accepts+1)/(cum_accepts+1))

            if(FLAGS$master %in% c(3:9) & req_prob[k_t] == 1){
                avail_h <- all_h[Ht[,k_t],]; 
                req_prob_Xk <- req_prob_X[req_prob_k==k_t,]
                req_prob[k_t]  <- get_req_prob(avail_h,req_prob_Xk , M)
            } 

            if (FLAGS$master==1){
                # Expert advice
                advice_t <- as.numeric((p_tmp*reg_diff_tmp)[k_t] - FLAGS$cost/ntrain * (req_prop_tmp)[k_t] > policy_set)
                pass_prob <- (1-gamma)*sum(exp_w * advice_t)/sum(exp_w) + gamma/2
                action_t <- runif(1) < pass_prob
            } else if (FLAGS$master==3){
                ex_reward <- (p_tmp*reg_diff_tmp)[k_t] - FLAGS$cost/ntrain * req_prob[k_t]
                action_t <- ex_reward > 0
            } else {
                if (FLAGS$master==4){
                    advice_t <- as.numeric((p_tmp*reg_diff_tmp)[k_t] - FLAGS$cost/ntrain * (req_prob)[k_t] > policy_set)
                } else if (FLAGS$master==5){
                    advice_t <- as.numeric(runif(1) < policy_set[,k_t])
                } else if (FLAGS$master==6){
                    curr_rank <- which(order(-reg_tmp)==k_t); advice_t <- as.numeric(c(0:r_per_h) >= curr_rank)
                } else if (FLAGS$master==7){
                    tmpreg <- reg_tmp /sum(reg_tmp); tmpobs <- cum_labels / sum(cum_labels) 
                    curr_rank <- which(order(-tmpreg+tmpobs)==k_t); advice_t <- as.numeric(c(0:r_per_h) >= curr_rank)
                } else if (FLAGS$master==8){
                    curr_rank <- which(order(-reg_diff_obs)==k_t); advice_t <- as.numeric(c(0:r_per_h) >= curr_rank)
                } else if (FLAGS$master==9){
                    ex_reward <- (p_tmp*reg_diff_tmp) - FLAGS$cost/ntrain * req_prob;
                    curr_rank <- which(order(-ex_reward)==k_t); advice_t <- as.numeric(c(0:r_per_h) >= curr_rank)
                }
                It <- which(runif(1) < cumsum(exp_w))[1]
                action_t <- advice_t[It]
            }

            if (action_t){
                cum_accepts[k_t] <- cum_accepts[k_t]+1

                pred_max <- max(pred_t[Ht[,k_t]])
                pred_min <- min(pred_t[Ht[,k_t]])
                p_t <- max(log(1+exp(-pred_min)) - log(1+exp(-pred_max)),
                           log(1+exp(pred_max)) - log(1+exp(pred_min))) / M
                Q_t <- as.numeric(runif(1) < p_t)
                cum_req_prob[k_t] <- cum_req_prob[k_t] + req_prob[k_t]

                if (Q_t > 0){
                    cum_labels[k_t] <- cum_labels[k_t]+1; 
                    cum_loss[,k_t] <- cum_loss[,k_t] + loss_func(pred_t,y_t,'logistic')/(M*p_t) # importance weighted cum_loss
                    min_err <- min((cum_loss[,k_t])[Ht[,k_t]])
                    T_t <- cum_accepts[k_t]
                    slack_t <- sqrt(T_t*log(T_t+1)) # a more aggresive slack term than IWAL paper
                    Ht[,k_t] <- (Ht[,k_t]  & cum_loss[,k_t] <= min_err + slack_t)

                    if(FLAGS$master==8){
                        p_tmp <- cum_samples/sum(cum_samples)
                        currreg <- (p_tmp * sqrt(log(1+cum_accepts)/(cum_accepts+1)))[k_t]
                        pastreg <- (p_tmp * sqrt(log(1+last_cum_accepts)/(last_cum_accepts+1)))[k_t]
                        reg_diff_obs[k_t] <- pastreg-currreg
                        last_cum_accepts[k_t] <- cum_accepts[k_t]
                    }
                } 
            }

            # Loss
            p_tmp <- cum_samples/sum(cum_samples)

            if(FLAGS$master==1){
                reg_tmp <- sqrt(log(1+cum_accepts)/(cum_accepts+1)) 
                obj_tmp <- p_tmp * reg_tmp + (FLAGS$cost/ntrain) * cum_labels
                # Make EXP4 updates
                loss_t <- i * sum(obj_tmp) - (i-1)*sum(objs)
                loss_t_policy <- rep(loss_t/pass_prob, num_policy); 
                loss_t_policy[advice_t != as.numeric(action_t)] <- 0
                exp_w <- exp_w * exp(-gamma*loss_t_policy/2); exp_w <- exp_w / sum(exp_w)
                objs <- obj_tmp
            } else if (FLAGS$master %in% c(4:9)){
                if(action_t){
                    reg_tmp1 <- sqrt(log(1+cum_accepts)/(cum_accepts+1)) 
                    obj_tmp1 <- p_tmp * reg_tmp1 + (FLAGS$cost/ntrain) * cum_req_prob

                    cum_accepts_tmp <- cum_accepts; cum_accepts_tmp[k_t] <- cum_accepts_tmp[k_t] -1
                    cum_req_prob_tmp <- cum_req_prob; cum_req_prob_tmp[k_t] <- cum_req_prob_tmp[k_t]-req_prob[k_t]
                    reg_tmp0 <- sqrt(log(cum_accepts_tmp+1)/(cum_accepts_tmp+1)) 
                    obj_tmp0 <- p_tmp * reg_tmp0 + (FLAGS$cost/ntrain) * cum_req_prob_tmp
                } else {
                    cum_accepts_tmp <- cum_accepts; cum_accepts_tmp[k_t] <- cum_accepts_tmp[k_t]+1
                    cum_req_prob_tmp <- cum_req_prob; cum_req_prob_tmp[k_t] <- cum_req_prob_tmp[k_t]+req_prob[k_t]
                    reg_tmp1 <- sqrt(log(1+cum_accepts_tmp)/(cum_accepts_tmp+1)) 
                    obj_tmp1 <- p_tmp * reg_tmp1 + (FLAGS$cost/ntrain) * cum_req_prob_tmp

                    reg_tmp0 <- sqrt(log(1+cum_accepts)/(cum_accepts+1)) 
                    obj_tmp0 <- p_tmp * reg_tmp0 + (FLAGS$cost/ntrain) * cum_req_prob
                }

                loss_1 <-  i*sum(obj_tmp1) - (i-1)*sum(objs)
                loss_0 <-  i*sum(obj_tmp0) - (i-1)*sum(objs)
                loss_t <- rep(0, num_policy); loss_t[advice_t==0] <- loss_0; loss_t[advice_t==1] <- loss_1
                exp_w <- exp_w * exp(-gamma*loss_t); exp_w <- exp_w / sum(exp_w)
                if(action_t){objs <- obj_tmp1} else {objs <- obj_tmp0}
            }

            Ht_sum_new <- sum(Ht[,k_t])
            if(FLAGS$master %in% c(3:9) & Ht_sum_new < Ht_sum_old[k_t]){
                avail_h <- all_h[Ht[,k_t],]; 
                req_prob_Xk <- req_prob_X[req_prob_k==k_t,]
                req_prob[k_t]  <- get_req_prob(avail_h,req_prob_Xk , M)
                Ht_sum_old[k_t] <- Ht_sum_new
            }
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
    colnames(OTB_iwal) <- c('round','labels','loss_misclass','loss_logistic')

    # save to file
    opt2 <- as.list(FLAGS) ;opt2$datafolder <- NULL ;opt2$otb <- NULL ;opt2$help <- NULL ;opt2$out_directory <- NULL
    basefilename <- paste0(paste0(names(opt2),'_',opt2), collapse = '_')
    filename <- paste0(FLAGS$out_directory,'/',basefilename, '_otb_rep',rep,'.csv')
    write.table(OTB_iwal,filename, sep = ',', row.names = FALSE)
}
