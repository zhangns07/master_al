Rfolder=/home/nz695/repo/master_al
vwfolder=/home/nz695/vw/vowpal_wabbit-8.3.1/vowpalwabbit/
NTRAIN=196045
NTEST=49012
for REP in `seq 1 10`; do 
    rm -rf shuffled*
    perl -MList::Util=shuffle -e  'srand('$REP');print shuffle(<STDIN>);' < skin.meta > shuffled.meta
    perl -MList::Util=shuffle -e  'srand('$REP');print shuffle(<STDIN>);' < skin.vw > shuffled.vw

    head -$NTRAIN shuffled.meta > train.meta
    head -$NTRAIN shuffled.vw  | ./scripts/rm_label > unlabel.vw 

    rm -rf test*vw
    tail -$NTEST shuffled.vw > test.vw
    tail -$NTEST shuffled.meta > test.meta
    Rscript $Rfolder/split_test.R --test_file=test.vw --meta_file=test.meta

    ## Region based
    costarray=(0.1 0.2 0.3 0.4 0.5)
    for COST in "${costarray[@]}"; do
        OUTFILE='output_ten_region_master_cost'$COST'_rep'$REP
        for LOGBUDGET in `seq 5 15`;do
            BUDGET=$((2**$LOGBUDGET))   
            for PORT in `seq 6666 6675`; do
                rm -rf model$PORT
                $vwfolder/vw --active --binary --port $PORT --random_seed 0 -f model$PORT  & 
            done
            Rscript $Rfolder/master_interactor.R --budget=$BUDGET --port_range=6666-6675 --cost=$COST --unlabeled_dataset=unlabel.vw --meta_file=train.meta

            sleep 1

            rm -rf p_out
            rm -rf testlabel
            for PORT in `seq 6666 6675`; do
                REGION=$(($PORT-6665))
                if [ -f test$REGION.vw ]; then
                    rm -rf p_out$REGION
                    $vwfolder/vw -t -d test$REGION.vw -i model$PORT  --binary -p p_out$REGION
                    cat p_out$REGION >> p_out
                    cat test$REGION.vw |cut -d '|' -f 1 >> testlabel
                fi
            done

            echo budget $BUDGET >> $OUTFILE
            Rscript $Rfolder/perf.R --pred=p_out --label=testlabel >> $OUTFILE
        done
    done
done
