library(optparse)
option_list <- list(make_option(c("-c","--cost"), type="numeric", default=0.3, help="Cost for requesting label"),
                    make_option(c("-b","--budget"), type="numeric",default=Inf,help="file with unlabeled data"),
                    make_option(c("-m","--minus1"), type="logical", default=TRUE, help="interpret 0 as -1"),
                    make_option(c("-H","--host"), type="character", default="localhost",help="the machine VW is running on"),
                    make_option(c("-p","--port_range"), type="character", default=NULL,help="a-b: the range of the port VW is listening on"),
                    make_option(c("-u","--unlabeled_dataset"), type="character",default=NULL,help="file with unlabeled data"),
                    make_option(c("-i","--meta_file"), type="character",default=NULL,help="file with meta information: region indicator and labels")
                    )
opt_parser <- OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);


# Connect to server and perform handshake
min_port <- as.numeric(strsplit(opt$port_range,"-")[[1]][1])
max_port <- as.numeric(strsplit(opt$port_range,"-")[[1]][2])
for(port in c(min_port:max_port)){
    assign(paste0('con',port),
           socketConnection(host=opt$host, port = port, blocking=TRUE, server=FALSE, open="r+"))
}
cat(paste0('connecting to ',opt$host,":",opt$port_range,'...\n'))
cat('done\n')

# Read unlabeled samples and meta information
num_unlabel <- as.numeric(strsplit(system(paste0("wc -l ",opt$unlabeled_dataset),intern=TRUE)," ")[[1]][1])
input_file <- file(opt$unlabeled_dataset,"r")
meta_info <- read.table(opt$meta_file,sep=",") # first column: region, second column: label

# Check num_slaves is same as num_regions
num_slaves <- max_port-min_port+1
if (length(unique(meta_info[,1])) > num_slaves){
    stop("Need more slaves!")
}

# Transform label to {-1,1} if necessary
if (opt$minus1){ y <- meta_info[,2]; y[y==0] <- -1; meta_info[,2] <- y }

# Start Master (EXP4)...
cat('sending unlabeled examples ...\n')
set.seed(0)

# -- warmup
num_warmup <- 5

# -- policy set
num_policy <- 100
#policy_set <- 2^seq(-9,0,length.out=num_policy)
policy_set <- seq(-opt$cost/num_unlabel, opt$cost/num_unlabel, length.out=num_policy)

# -- learning rate
#gamma <- 0.1
gamma <- sqrt(log(num_policy)/(num_unlabel*(opt$cost)^2))

# -- book keeping
cum_samples <- cum_accepts  <- rep(0.5, num_slaves) # incoming unlabeled; passed on to slave; 
cum_labels <- rep(0, num_slaves)
exp4_w <- rep(1, num_policy)

p_k <- cum_samples/sum(cum_samples)
reg_k <- sqrt(log(1+cum_accepts)/(cum_accepts+1)) 
objs <- p_k * reg_k 

for (i in c(1:num_unlabel)){
    k <- meta_info[i,1]; port <- k-1 + min_port
    y_t <- meta_info[i,2]; 
    x_t <- readLines(input_file,n=1)
    currcon <- get(paste0('con',port))
    cum_samples[k] <- cum_samples[k]+1

    if (sum(cum_labels) > opt$budget){ break }
    if (cum_labels[k] < num_warmup){
        cum_labels[k] <- cum_labels[k]+1
        cum_accepts[k] <- cum_accepts[k]+1
        # Pass labeled sample to slave
        labeled_example <- paste0(y_t,x_t)
        write_resp <- writeLines(labeled_example, currcon)
        response <- readLines(currcon,1)
    } else {
        # Expert advice
        #advice_t <- as.numeric(objs[k] > policy_set)
        p_tmp <- cum_samples/sum(cum_samples)
        reg_diff_tmp <- sqrt(log(cum_accepts+1)/(cum_accepts+1)) - sqrt(log(cum_accepts+2)/(cum_accepts+2)) 
        req_prop_tmp <- cum_labels/cum_accepts
        advice_t <- as.numeric((p_tmp*reg_diff_tmp)[k] - opt$cost/num_unlabel * (req_prop_tmp)[k] > policy_set)

        # Vote
        pass_prob <- (1-gamma)*sum(exp4_w * advice_t)/sum(exp4_w) + gamma/2
        action_t <- runif(1) < pass_prob

        # Take action
        if (action_t){
            cum_accepts[k] <- cum_accepts[k]+1

            # Pass unlabeled sample to slave
            write_resp <- writeLines(x_t, currcon)
            response <- readLines(currcon,1)
            responselist <- strsplit(response, ' ')[[1]]
            if (length(responselist)==3){ 
                # Slave request label
                cum_labels[k] <- cum_labels[k]+1
                prediction <- responselist[1]
                tag <- responselist[2]
                if (tag == ''){tag <- "'empty"}
                importance <- responselist[3]

                # Pass labeled sample to slave
                rest <- strsplit(x_t,'\\|')[[1]][2]
                labeled_example <- paste0(y_t,' ',importance,' ',tag,' |',rest)
                write_resp <- writeLines(labeled_example, currcon)
                response <- readLines(currcon,1)
            }
        } 

        # Reward
        p_tmp <- cum_samples/sum(cum_samples)
        reg_tmp <- sqrt(log(1+cum_accepts)/(cum_accepts+1)) 
        obj_tmp <- p_tmp * reg_tmp + (opt$cost/num_unlabel) * cum_labels

        #reward_t <-  sum(objs) - sum(obj_tmp)
        loss_t <- i * sum(obj_tmp) - (i-1)*sum(objs)
        loss_t_policy <- rep(loss_t/pass_prob, num_policy); 
        loss_t_policy[advice_t != as.numeric(action_t)] <- 0
        #cat('action: ',action_t,', loss: ', loss_t/pass_prob,'\n')
        #cat(loss_t_policy)

        # Make EXP4 updates
        exp4_w <- exp4_w * exp(-gamma*loss_t_policy/2)

        # Update current objective value at each region
        objs <- obj_tmp
    }
}

for(port in c(min_port:max_port)){
    currcon <- get(paste0('con',port))
    close(currcon)
}
close(input_file)

cat("number of samples: ",i,"\n")
cat("number of accepts: ", cum_accepts,"\n")
cat("number of labels: ", cum_labels,"\n")

