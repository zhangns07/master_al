library(optparse)
option_list <- list(#make_option(c("-s","--seed"), type="character", default=NULL, help="Initial labeled samples"), # remove seed for master
                    make_option(c("-b","--budget"), type="numeric",default=Inf,help="file with unlabeled data"),
                    make_option(c("-m","--minus1"), type="logical", default=TRUE, help="interpret 0 as -1"),
                    make_option(c("-H","--host"), type="character", default="localhost",help="the machine VW is running on"),
                    make_option(c("-p","--port_range"), type="character", default="6666-6669",help="a-b: the range of the port VW is listening on"),
                    make_option(c("-u","--unlabeled_dataset"), type="character",default="/home/nz695/vw/rcv1/unlabel.dat",help="file with unlabeled data"),
                    make_option(c("-i","--meta_file"), type="character",default="/home/nz695/vw/rcv1/meta.dat",help="file with meta information: region indicator and labels")
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

# -- warmup per region
cum_labels <- rep(0,num_slaves)

for (i in c(1:num_unlabel)){
    k <- meta_info[i,1]; port <- k-1 + min_port
    y_t <- meta_info[i,2]; 
    x_t <- readLines(input_file,n=1)
    currcon <- get(paste0('con',port))

    if (sum(cum_labels) > opt$budget){ break }
    if (i <= opt$budget){
        cum_labels[k] <- cum_labels[k]+1
        # Pass labeled sample to slave
        labeled_example <- paste0(y_t,x_t)
        write_resp <- writeLines(labeled_example, currcon)
        response <- readLines(currcon,1)
    } 
}

for(port in c(min_port:max_port)){
    currcon <- get(paste0('con',port))
    close(currcon)
}
close(input_file)
