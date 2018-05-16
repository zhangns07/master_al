#!/usr/bin/env Rscript
# 1. This version uses objective 
# \sum p_k * err_k + c * \tau_k
# As opposed to the _reg version that minimize
# \sum p_k * (err_k - \mu_k^*) + c * \tau_k
# 2. Therefore, this is back to bandit setting. In fact, feedback graph.
# 3. Experts are all ranking based. Many ideas of the score. 
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
                                help="types of master algorithm. "),
                    make_option(c("-v", "--timevar"), type="logical", default=FALSE,
                                help="whether time varying learning rate")
                    )
# Master
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
num_policy <- r_per_h+1

for (rep in c(1:20)){
    opt2 <- as.list(FLAGS) ;opt2$datafolder <- NULL ;opt2$otb <- NULL ;opt2$help <- NULL ;opt2$out_directory <- NULL
    if(!opt2$timevar){opt2$timevar <- NULL}
    basefilename <- paste0(paste0(names(opt2),'_',opt2), collapse = '_')
    filename <- paste0(FLAGS$out_directory,'/',basefilename, '_obj_empreg_otb_rep',rep,'.csv')
    if (file.exists(filename)){next}

    set.seed(rep); shuffle <- sample(nrow(X),nrow(X),replace = FALSE)
    trainX_tmp <- X[shuffle[1:ntrain],]; testX_tmp <- X[shuffle[-c(1:ntrain)],]
    col_to_keep <- apply(trainX_tmp, 2, var, na.rm=TRUE)  != 0
    trainX_tmp  <- trainX_tmp[,col_to_keep]; testX_tmp  <- testX_tmp[,col_to_keep];
    trainy <- y[shuffle[1:ntrain]];  testy <- y[shuffle[-c(1:ntrain)]]

    # -- rescale X and testX
    preprop <- preProcess(trainX_tmp,method=c('center','scale','pca'),pcaComp=10)
    trainX <- predict(preprop, trainX_tmp)
    testX <- predict(preprop, testX_tmp) 
    trainX <- cbind(1,as.matrix(trainX)); testX <- cbind(1,as.matrix(testX))

    # -- sample hypotheses not too far away from h0
    scales <- 2^seq(0,FLAGS$lognorm,1)
    all_h <- gen_all_h(num_dim=ncol(trainX), num_base_models=FLAGS$basemodel, scales)#, h0=h0)
    nh <- nrow(all_h)

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

    # -- book keeping
    req_prob <- rep(1,r_per_h); cum_train_loss_min <- cum_test_loss <- rep(0,r_per_h)
    last_reg <- rep(1,r_per_h); dec_reg <- rep(0,r_per_h)
    It <- rep(1,r_per_h)

    cum_loss <- matrix(0,nrow=nh,ncol=r_per_h)
    Ht <- matrix(TRUE,nrow=nh,ncol=r_per_h); Ht_sum_old <- rep(nh, each=r_per_h)
    cum_samples <- cum_accepts  <- rep(0.5, r_per_h) # incoming unlabeled; passed on to slave; 
    cum_labels <- rep(0, r_per_h)
    exp_w <- rep(1, num_policy); exp_w <- exp_w / sum(exp_w)
    #gamma <- sqrt(log(num_policy)/(ntrain*(FLAGS$cost)^2))

    checkpoint <- min(100,floor(nT / (100*10)) * 100)
    if (checkpoint==0){checkpoint <- 25}

    OTB_iwal <- matrix(c(0),ncol = 4) # book keeping
    last_i <- 0; last_cum_label <- 0

    for (i in seq_len(ntrain)){
        x_t <- trainX[i,]; y_t <- trainy[i]; k_t <- traink[i]; pred_t <- all_h %*% x_t
        cum_samples[k_t] <- cum_samples[k_t] +1
        if (i <= n_warmup || cum_accepts[k_t] <= n_warmup/r_per_h){
            cum_labels[k_t] <- cum_labels[k_t]+1
            cum_accepts[k_t] <- cum_accepts[k_t]+1
            cum_loss[,k_t] <- cum_loss[,k_t] + loss_func(pred_t,y_t,'logistic')/(M) # importance weighted cum_loss
            cum_test_loss[k_t] <- cum_test_loss[k_t] + loss_func(pred_t[It[k_t]],y_t,'logistic')/(M)

            It[k_t] <- which.min(cum_loss[,k_t])
            cum_train_loss_min[k_t] <- cum_loss[It[k_t],k_t]

            curr_reg <- (cum_test_loss[k_t] - cum_train_loss_min[k_t]) / (2*cum_accepts[k_t])
            dec_reg[k_t] <- last_reg[k_t] - curr_reg
            last_reg[k_t] <- curr_reg

            p_tmp <- cum_samples/sum(cum_samples)
            objs <- p_tmp * (cum_test_loss-cum_train_loss_min)/(2*cum_accepts) + (FLAGS$cost/ntrain) * cum_labels

        } else{
            p_tmp <- cum_samples/sum(cum_samples)
            reg_diff_tmp <- sqrt(log(cum_accepts+1)/(cum_accepts+1)) - sqrt(log(cum_accepts+2)/(cum_accepts+2)) 

            if(req_prob[k_t] == 1){
                avail_h <- all_h[Ht[,k_t],]; 
                req_prob_Xk <- req_prob_X[req_prob_k==k_t,]
                req_prob[k_t]  <- get_req_prob(avail_h,req_prob_Xk , M)
            } 

            if (FLAGS$master==1){
                # (test_error - train_error)/2, as an estimate of regret 
                score <-  (cum_test_loss/cum_accepts - cum_train_loss_min / cum_accepts)*p_tmp
            } else if (FLAGS$master==2){
                # decrease in (test_err - train_err) between last two labels
                score <- dec_reg*p_tmp
            } else if (FLAGS$master==3){
                # expected decrease in regret using 1/(tk)^1.5) - c * prob_req 
                score <- (p_tmp*reg_diff_tmp) - FLAGS$cost/ntrain * req_prob;
            } else if (FLAGS$master==4){
                # decrease in regret per label, using (1/sqrt(tk)^1.5 / req_prob_k)
                score <- (p_tmp*reg_diff_tmp) / req_prob;
            } 

            curr_rank <- which(order(-score)==k_t); 
            advice_t <- as.numeric(c(1:num_policy)-1 >= curr_rank)
            Jt <- which(runif(1) < cumsum(exp_w))[1]
            action_t <- advice_t[Jt]

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
                    cum_test_loss[k_t] <- cum_test_loss[k_t] + loss_func(pred_t[It[k_t]],y_t,'logistic')/(M*p_t)

                    It[k_t] <- which.min(cum_loss[,k_t])
                    cum_train_loss_min[k_t] <- cum_loss[It[k_t],k_t]

                    min_cum_err <- cum_train_loss_min[k_t]
                    T_t <- cum_accepts[k_t]
                    slack_t <- sqrt(T_t*log(T_t+1)) # a more aggresive slack term than IWAL paper
                    Ht[,k_t] <- (Ht[,k_t]  & cum_loss[,k_t] <= min_cum_err + slack_t)

                    curr_reg <- (cum_test_loss[k_t] - cum_train_loss_min[k_t]) / (2*cum_accepts[k_t])
                    dec_reg[k_t] <- last_reg[k_t] - curr_reg
                    last_reg[k_t] <- curr_reg
                }
            }

            # Loss
            p_tmp <- cum_samples/sum(cum_samples)

            # Prob of being observed
            P_w <- rep(0,num_policy); P_w[advice_t==0] <- 1; P_w[advice_t==1] <- sum(exp_w[advice_t==1])
            if(FLAGS$timevar){
                gamma_t <- sqrt(log(num_policy)/(i*(FLAGS$cost)^2))
            } else {
                gamma_t <- sqrt(log(num_policy)/(ntrain*(FLAGS$cost)^2))
            }

            # if action_t=1,  update both all experts; if action_t=0,  update only experts that not pass
            if (action_t==1){
                obj_tmp <- p_tmp * (cum_test_loss-cum_train_loss_min)/(2*cum_accepts) + (FLAGS$cost/ntrain) * cum_labels
                loss_1 <- i*sum(obj_tmp) - (i-1)*sum(objs)
                loss_0 <- sum(objs)

                loss_t <- rep(0, num_policy); loss_t[advice_t==0] <- loss_0; loss_t[advice_t==1] <- loss_1; loss_t <- loss_t/P_w
                loss_t[P_w==0] <- 0
                exp_w <- exp_w * exp(-gamma_t*loss_t); exp_w <- exp_w / sum(exp_w)
                objs <- obj_tmp
            } else {
                loss_0 <- sum(objs)
                loss_t <- rep(0, num_policy); loss_t[advice_t==0] <- loss_0;  loss_t <- loss_t/P_w; loss_t[advice_t==1] <- 0
                loss_t[P_w==0] <- 0
                exp_w <- exp_w * exp(-gamma_t*loss_t); exp_w <- exp_w / sum(exp_w)
            }

            Ht_sum_new <- sum(Ht[,k_t])
            if(Ht_sum_new < Ht_sum_old[k_t]){
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
            cat(exp_w,'\n')

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
    if(!opt2$timevar){opt2$timevar <- NULL}
    basefilename <- paste0(paste0(names(opt2),'_',opt2), collapse = '_')
    filename <- paste0(FLAGS$out_directory,'/',basefilename, '_obj_empreg_otb_rep',rep,'.csv')
    write.table(OTB_iwal,filename, sep = ',', row.names = FALSE)
}

