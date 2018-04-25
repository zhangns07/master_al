library(optparse)
option_list <- list(make_option(c("-t","--test_file"), type="character",default=NULL, help="file with unlabeled data"),
                    make_option(c("-i","--meta_file"), type="character",default=NULL, help="file with meta information: region indicator and labels"),
                    make_option(c("-o","--out_dir"), type="character",default='.', help="file with meta information: region indicator and labels")
                    )

opt_parser <- OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

meta <- read.table(opt$meta_file,sep=",")
region <- meta[,1]

#test_file <- file(opt$test_file,'r')
#num_test <- as.numeric(strsplit(system(paste0("wc -l ",opt$test_file),intern=TRUE)," ")[[1]][1])
#for (i in c(1:num_test)){
#    line  <- readLines(test_file,n=1)
#    k <- region[i]
#    cat(line,'\n',file= paste0(opt$out_dir,'/test',k,'.vw'),append=TRUE)
#}

test_input <- readLines(opt$test_file)
region_uniq <- unique(region)
for (k in region_uniq){
    cat(test_input[region == k],sep="\n", file = paste0(opt$out_dir,'/test',k,'.vw'))
}
