#/home/nz695/vw/vowpal_wabbit-8.3.1/vowpalwabbit/vw --binary --random_seed 0 -d train.dat -f model0  
#/home/nz695/vw/vowpal_wabbit-8.3.1/vowpalwabbit/vw -t -d test.dat -i model0 --binary 
#0.052702

rm -rf p_out_passive
rm -rf testlabel_passive

for REGION in `seq 1 10`; do
    if [ -f train$REGION.dat ]; then
        /home/nz695/vw/vowpal_wabbit-8.3.1/vowpalwabbit/vw --binary --random_seed 0 -d train$REGION.dat -f model_passive$REGION 
        if [ -f test$REGION.dat ]; then
            /home/nz695/vw/vowpal_wabbit-8.3.1/vowpalwabbit/vw -t -d test$REGION.dat -i model_passive$REGION  --binary  -p passive_out$REGION
            cat passive_out$REGION >> p_out_passive
            cat test$REGION.dat |cut -d '|' -f 1 >> testlabel_passive
        fi
    fi 
done

Rscript /home/nz695/repo/master_al/perf.R --pred=p_out_passive --label=testlabel_passive
#Err: 0.04915979
