library(optparse)
option_list <- list(#make_option(c("-s","--seed"), type="character", default=NULL, help="Initial labeled samples"), # remove seed for master
                    make_option(c("-c","--cost"), type="numeric", default=NULL, help="Cost for requesting label"),
                    make_option(c("-o","--output"), type="character", default=NULL, help="Output file"),
                    make_option(c("-m","--minus1"), type="logical", default=TRUE, help="interpret 0 as -1"),
                    make_option(c("-h","--host"), type="character", default="localhost",help="the machine VW is running on"),
                    make_option(c("-p","--port_range"), type="character", default=NULL,help="a-b: the range of the port VW is listening on"),
                    make_option(c("-u","--unlabeled_dataset"), type="character",default=NULL,help="file with unlabeled data"),
                    make_option(c("-i","--meta_file"), type="character",default=NULL,help="file with meta information: region indicator and labels")
                    )
opt_parser <- OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);


# Connect to server and perform handshake
min_port <- as.numeric(strsplit(opt$port_range)[[1]][1])
max_port <- as.numeric(strsplit(opt$port_range)[[1]][2])
for(port in c(min_port:max_port)){
    assign(paste0('con',port),
           socketConnection(host=opt$host, port = port, blocking=TRUE, server=FALSE, open="r+"))
}
cat(paste0('connecting to ',opt$host,":",opt$port_range,'...\n'))
cat('done\n')

# Read unlabeled samples and meta information
input_unlabel <- readLines(opt$unlabeled_dataset)
num_unlabel <- length(input_unlabel)
meta_info <- read.table(opt$meta_file)

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

# -- policy set
num_policy <- 100
policy_set <- 2^seq(-9,0,length.out=num_policy)

# -- learning rate
gamma <- 0.1

# -- book keeping
cum_samples <- cum_accepts <- cum_labels <- rep(0, num_slaves) # incoming unlabeled; passed on to slave; request label
exp4_w <- rep(1, num_slaves)

p_k <- rep(1/num_slaves, num_slaves)
reg_k <- sqrt(log(1.5)/0.5) 
objs <- p_k * reg_k

for (i in c(1:num_unlabel)){
    k <- meta_info[i,1]; port <- k-1 + min_port
    y_t <- meta_info[i,2]; 
    x_t <- input_unlabel[i]
    currcon <- get(paste0('con',port))
    cum_samples[k] <- cum_samples[k]+1

    # Expert advice
    advice_t <- as.numeric(objs[k] > policy_set)

    # Vote
    pass_prob <- (1-gamma)*sum(exp4_w * advice_t)/sum(exp4_w) + gamma/2
    action_t <- runif(1) < pass_prob

    # Take action
    if (action_t){
        cum_accepts[k] <- cum_accepts[k]+1

        # Pass unlabeled sample to slave
        cat(paste0('sending unlabeled ',substr(line,1,20),'\n'))
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
            queries <- queries+1
            labeled_example <- paste0(y_t,' ',importance,' ',tag,' |',rest)
            write_resp <- writeLines(labeled_example, currcon)
            response <- readLines(currcon,1)

            if (!is.null(opt$output)){
                cat(labeled_example,file=opt$output,append=TRUE)
            }
        }

        # Reward
        p_tmp <- cum_samples/i
        reg_tmp <- sqrt(log(1+cum_accepts)/cum_accepts) 
        obj_tmp <- p_tmp * reg_tmp + (opt$cost/num_unlabel) * cum_labels
        reward_t <-  sum(objs) - sum(obj_tmp)
        reward_t_policy <- rep(reward_t/pass_prob, num_policy); 
        reward_t_policy[advice_t==0] <- 0

        # Make EXP4 updates
        exp4_w <- exp4_w * exp(gamma*reward_t_policy/2)
    }

    # Update current objective value at each region
    p_k <- cum_samples/i
    reg_k <- sqrt(log(1+cum_accepts)/cum_accepts) 
    objs <- p_k * reg_k + (opt$cost/num_unlabel) * cum_labels
}


for(port in c(min_port:max_port)){
    currcon <- get(paste0('con',port))
    close(currcon)
}
