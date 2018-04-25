library(optparse)
option_list <- list(make_option(c("-p","--pred"), type="character",default=NULL,help="file with prediciton"),
                    make_option(c("-l","--label"), type="character",default=NULL,help="file with true label")
                    )
opt_parser <- OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

pred <- as.numeric(readLines(opt$pred))
label <- as.numeric(readLines(opt$label))
err <- sum(pred!=label)/length(pred)
cat("Err:",err,"\n")
