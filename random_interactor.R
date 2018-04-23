library(optparse)
option_list <- list(make_option(c("-s","--seed"), type="character", default=NULL, help="Initial labeled samples"),
                    make_option(c("-o","--output"), type="character", default=NULL, help="Output file"),
		    make_option(c("-m","--minus1"), type="logical", default=TRUE, help="interpret 0 as -1"),
		    make_option(c("-h","--host"), type="character", default="localhost",help="the machine VW is running on"),
		    make_option(c("-p","--port"), type="numeric", default=NULL,help="the port VW is listening on"),
		    make_option(c("-u","--unlabeled_dataset"), type="character",default=NULL,help="file with unlabeled data"))
opt_parser <- OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);


con <- socketConnection(host=opt$host, port = opt$port, blocking=TRUE, server=FALSE, open="r+")

# Connect to server and perform handshake
cat(paste0('connecting to ',opt$host,":",opt$port,'...\n'))
cat('done\n')

# Send seed dataset

if (!is.null(opt$seed)){
cat('seeding vw ...\n')
input_seed <- readLines(opt$seed)
for (i in c(1:len(input_seed))){
line <- input_seed[i]
write_resp <- writeLines(line, con)
response <- readLines(con,1)
}
cat('done\n')
}

queries=0
input_unlabel <- readLines(opt$unlabeled_dataset)
num_unlabel <- length(input_unlabel)

cat('sending unlabeled examples ...\n')
for (i in c(1:num_unlabel)){
line <- input_unlabel[i]
cat(paste0('sending unlabeled ',substr(line,1,20),'\n'))
write_resp <- writeLines(line, con)
response <- readLines(con,1)
responselist <- strsplit(response, ' ')[[1]]
if (length(responselist)==1){ next }
prediction <- responselist[1]
tag <- responselist[2]
importance <- responselist[3]
queries <- queries+1
#        try:
#            imp=float(importance)}
#        except:
#            continue
unif <- runif(1)
neglabel <- ifelse(opt$minus1, -1, 0)
label <- ifelse(unif < 0.3, 1, ifelse(unif < 0.7, neglabel, NA))
if (is.na(label)){next}
if (tag == ''){tag <- "'empty"}
rest <- strsplit(line,'\\|')[[1]][2]
labeled_example <- paste0(label,' ',importance,' ',tag,' |',rest)
write_resp <- writeLines(labeled_example, con)
response <- readLines(con,1)

if (!is.null(opt$output)){
cat(labeled_example,file=opt$output,append=TRUE)
}
}

close(con)


