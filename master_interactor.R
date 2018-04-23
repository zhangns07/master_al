library(optparse)
option_list <- list(#make_option(c("-s","--seed"), type="character", default=NULL, help="Initial labeled samples"), # remove seed for master
                    make_option(c("-o","--output"), type="character", default=NULL, help="Output file"),
		    make_option(c("-m","--minus1"), type="logical", default=TRUE, help="interpret 0 as -1"),
		    make_(ption(c("-h","--host"), type="character", default="localhost",help="the machine VW is running on"),
		    make_option(c("-p","--port_range"), type="character", default=NULL,help="a-b: the range of the port VW is listening on"),
		    make_option(c("-u","--unlabeled_dataset"), type="character",default=NULL,help="file with unlabeled data"),
		    make_option(c("-i","--meta_file"), type="character",default=NULL,help="file with meta information: region indicator and labels")
		)
opt_parser <- OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);


min_port <- as.numeric(strsplit(opt$port_range)[[1]][1])
max_port <- as.numeric(strsplit(opt$port_range)[[1]][2])
for(port in c(min_port:max_port)){
assign(paste0('con',port),
socketConnection(host=opt$host, port = port, blocking=TRUE, server=FALSE, open="r+"))
}

# Connect to server and perform handshake
cat(paste0('connecting to ',opt$host,":",opt$port_range,'...\n'))
cat('done\n')

input_unlabel <- readLines(opt$unlabeled_dataset)
num_unlabel <- length(input_unlabel)
meta_info <- read.table(opt$meta_file)
if (opt$minus1){
y <- meta_info[,2]
y[y==0] <- -1
meta_info[,2] <- y}

cat('sending unlabeled examples ...\n')

queries=0
set.seed(0)
for (i in c(1:num_unlabel)){
k <- meta_info[i,1]; port <- k-1 + min_port
y <- meta_info[i,2]; 
line <- input_unlabel[i]
cat(paste0('sending unlabeled ',substr(line,1,20),'\n'))
currcon <- get(paste0('con',port))
write_resp <- writeLines(line, currcon)
response <- readLines(currcon,1)
responselist <- strsplit(response, ' ')[[1]]
if (length(responselist)==1){ next }
prediction <- responselist[1]
tag <- responselist[2]
importance <- responselist[3]

unif <- runif(1)
label <- ifelse(unif < 0.3, NA, y)
if (is.na(label)){next} 
if (tag == ''){tag <- "'empty"}
rest <- strsplit(line,'\\|')[[1]][2]
queries <- queries+1
labeled_example <- paste0(label,' ',importance,' ',tag,' |',rest)
write_resp <- writeLines(labeled_example, currcon)
response <- readLines(currcon,1)

if (!is.null(opt$output)){
cat(labeled_example,file=opt$output,append=TRUE)
}

}


for(port in c(min_port:max_port)){
currcon <- get(paste0('con',port))
close(currcon)
}
