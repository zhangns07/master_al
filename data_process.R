#----------
filename <- '/home/nz695/vw/rcv1/train.dat'
metafilename <- '/home/nz695/vw/rcv1/meta.dat'

num_inputs <- as.numeric(strsplit(system(paste0("wc -l ",filename),intern=TRUE)," ")[[1]][1])
num_nonempty <- rep(0,num_inputs)
label <- rep(0,num_inputs)

input_file <- file(filename,'r')
for (i in seq_len(num_inputs)){
    line <- readLines(input_file,n=1)
    linesplit <- strsplit(line," \\|f ")[[1]]
    y <- linesplit[1]
    features <- strsplit(linesplit[2]," ")[[1]]
    num_nonempty[i] <- length(features)
    label[i] <- y
}

write.table(cbind(num_nonempty, label), file = '/home/nz695/vw/rcv1/rawmeta.dat', sep=",",col.names=F,row.names=F,quote=F)

num_slaves <- 10
thres <- c(0,10^seq(1,log(47236,10),length.out=num_slaves))

#sep <- quantile(num_nonempty, c(0.25, 0.5, 0.7,1))
#sep <- quantile(num_nonempty, c(1:10)/10)
#k <- apply(array(num_nonempty),1,function(x){which(x <= sep)[1]})

k <- as.numeric(cut(num_nonempty, thres))
meta <- cbind(k, label)
write.table(meta,metafilename, sep=",",col.names=F,row.names=F,quote=F)

testfilename <- '/home/nz695/vw/rcv1/test.dat'
inputs <- readLines(testfilename)
test_num_nonempty <- rep(0,length(inputs))
for (i in seq_along(inputs)){
    line <- inputs[i]
    line <- strsplit(line,"\\|")[[1]][2]
    line <- strsplit(line," ")[[1]]
    test_num_nonempty[i] <- length(line)-1
}

#testk <- apply(array(num_nonempty),1,function(x){which(x <= sep)[1]})
#testk[is.na(testk)] <- 4

testk <- as.numeric(cut(test_num_nonempty, thres))

for(r in c(1:10)){
    if (any(testk==r)){
        cat(inputs[testk==r], sep="\n",file = paste0('/home/nz695/vw/rcv1/test',r,'.dat'))
    }
}


inputs <- readLines(filename)
for(r in c(5:10)){
    if (any(k==r)){
        cat(inputs[k==r], sep="\n",file = paste0('/home/nz695/vw/rcv1/train',r,'.dat'))
    }
}



