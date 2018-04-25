NTRAIN=196045
NTEST=49012
#for REP in `seq 1 10`; do 
for REP in `seq 1 2`; do 
    perl -MList::Util=shuffle -e  'srand('$REP');print shuffle(<STDIN>);' < skin.meta > shuffled.meta
    perl -MList::Util=shuffle -e  'srand('$REP');print shuffle(<STDIN>);' < skin.vw > shuffled.vw

    head -$NTRAIN shuffled.meta > train.meta
    head -$NTRAIN shuffled.vw  | ./scripts/rm_label > unlabel.vw 
    tail -$NTEST shuffled.vw > test.vw

    cat test.vw |cut -d '|' -f 1 > testlabel

    for LOGBUDGET in `seq 5 14`;do
        rm -rf model.reg
        rm -rf p_out
        BUDGET=$((2**$LOGBUDGET))  

        /home/nz695/vw/vowpal_wabbit-8.3.1/vowpalwabbit/vw --active --binary --port 6666 --mellowness 0.000001 --random_seed 0 -f model.reg  & 
        Rscript /home/nz695/repo/master_al/active_interactor.R --budget=$BUDGET --unlabeled_dataset=unlabel.vw --meta_file=train.meta
        sleep 1

        /home/nz695/vw/vowpal_wabbit-8.3.1/vowpalwabbit/vw -t -d test.vw -i model.reg  --binary -p p_out
        echo budget $BUDGET >> output_one_region_rep$REP
        Rscript /home/nz695/repo/master_al/perf.R --pred=p_out --label=testlabel >> output_one_region_rep$REP
    done
done
