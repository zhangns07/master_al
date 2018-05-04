library(optparse)
option_list <- list(make_option(c("-b","--budget"), type="numeric", default=Inf, help="label budget"),
                    make_option(c("-m","--minus1"), type="logical", default=TRUE, help="interpret 0 as -1"),
                    make_option(c("-H","--host"), type="character", default="localhost",help="the machine VW is running on"),
                    make_option(c("-p","--port"), type="character", default="6666",help="a-b: the range of the port VW is listening on"),
                    make_option(c("-u","--unlabeled_dataset"), type="character",default="/home/nz695/vw/rcv1/unlabel.dat",help="file with unlabeled data"),
                    make_option(c("-i","--meta_file"), type="character",default="/home/nz695/vw/rcv1/meta.dat",help="file with meta information: region indicator and labels")
                    )

opt_parser <- OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);


# Connect to server and perform handshake
con <- socketConnection(host=opt$host, port = opt$port, blocking=TRUE, server=FALSE, open="r+")
cat(paste0('connecting to ',opt$host,":",opt$port,'...\n'))
cat('done\n')

# Read unlabeled samples and meta information
num_unlabel <- as.numeric(strsplit(system(paste0("wc -l ",opt$unlabeled_dataset),intern=TRUE)," ")[[1]][1])
input_file <- file(opt$unlabeled_dataset,"r")
meta_info <- read.table(opt$meta_file,sep=",") # first column: region, second column: label

# Transform label to {-1,1} if necessary
if (opt$minus1){ y <- meta_info[,2]; y[y==0] <- -1; meta_info[,2] <- y }

# Start Master (EXP4)...
cat('sending unlabeled examples ...\n')
cum_labels <- 0
num_warmup <- 10
for (i in c(1:num_unlabel)){
    y_t <- meta_info[i,2]; 
    x_t <- readLines(input_file,n=1)

    if (cum_labels > opt$budget){ 
        break 
    }

    write_resp <- writeLines(paste0(y_t,x_t), con)
    response <- readLines(con,1)
    cum_labels <- cum_labels+1
    
}

cat("number of samples: ",i,"\n")
cat("number of labels: ",cum_labels,"\n")
close(input_file)
close(con)


