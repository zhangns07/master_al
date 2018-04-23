library(optparse)
option_list <- list(#make_option(c("-s","--seed"), type="character", default=NULL, help="Initial labeled samples"), # remove seed for master
                    make_option(c("-b","--budget"), type="numeric", default=Inf, help="label budget"),
                    make_option(c("-m","--minus1"), type="logical", default=TRUE, help="interpret 0 as -1"),
                    make_option(c("-h","--host"), type="character", default="localhost",help="the machine VW is running on"),
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
input_unlabel <- readLines(opt$unlabeled_dataset)
num_unlabel <- length(input_unlabel)
meta_info <- read.table(opt$meta_file,sep=",") # first column: region, second column: label

# Transform label to {-1,1} if necessary
if (opt$minus1){ y <- meta_info[,2]; y[y==0] <- -1; meta_info[,2] <- y }

# Start Master (EXP4)...
cat('sending unlabeled examples ...\n')
cum_labels <- 0
num_warmup <- 4
for (i in c(1:num_unlabel)){
    y_t <- meta_info[i,2]; 
    x_t <- input_unlabel[i]

    if (cum_labels > opt$budget){ break }
    if (i <= num_warmup ){
        write_resp <- writeLines(paste0(y_t,x_t), con)
        response <- readLines(con,1)
    } else {
        # Pass unlabeled sample to slave
        cat(paste0('sending unlabeled ',substr(x_t,1,20),'\n'))
        write_resp <- writeLines(x_t, con)
        response <- readLines(con,1)
        responselist <- strsplit(response, ' ')[[1]]
        if (length(responselist)==3){ 
            cum_labels <- cum_labels+1
            # Slave request label
            prediction <- responselist[1]
            tag <- responselist[2]
            if (tag == ''){tag <- "'empty"}
            importance <- responselist[3]

            # Pass labeled sample to slave
            rest <- strsplit(x_t,'\\|')[[1]][2]
            labeled_example <- paste0(y_t,' ',importance,' ',tag,' |',rest)
            write_resp <- writeLines(labeled_example, con)
            response <- readLines(con,1)
        }
    }
}

close(con)


