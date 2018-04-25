for LOGBUDGET in `seq 5 19`;do
    BUDGET=$((2**$LOGBUDGET))   
    for PORT in `seq 6666 6675`; do
        /home/nz695/vw/vowpal_wabbit-8.3.1/vowpalwabbit/vw --active --binary --port $PORT --mellowness 0.000001 -f model$PORT  & 
    done
    Rscript /home/nz695/repo/master_al/nomaster_interactor.R --budget=$BUDGET --port_range=6666-6675

    rm -rf p_out
    rm -rf testlabel
    for PORT in `seq 6666 6675`; do
        REGION=$(($PORT-6665))
        if [ -f test$REGION.dat ]; then
            /home/nz695/vw/vowpal_wabbit-8.3.1/vowpalwabbit/vw -t -d test$REGION.dat -i model$PORT  --binary -p p_out$REGION
            cat p_out$REGION >> p_out
            cat test$REGION.dat |cut -d '|' -f 1 >> testlabel
        fi
    done
    
    echo budget $BUDGET >> output_ten_region_nomaster
    Rscript /home/nz695/repo/master_al/perf.R --pred=p_out --label=testlabel >> output_ten_region_nomaster
done

# Err: 0.07970107
