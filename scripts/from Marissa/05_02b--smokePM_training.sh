#!/bin/bash
export IFS=","

cat 05_02c_smokePM_training_jobs.csv | while read a b; do 

job_file="train_smokePM_fold${a}_drop${b}.job"

echo "#!/bin/bash

#SBATCH  -p serc
#SBATCH --job-name=train_smokePM_fold${a}_drop${b}
#SBATCH --nodes=1              
#SBATCH --ntasks-per-node=20
#SBATCH --mem-per-cpu=20GB
#SBATCH --time=5-00:00:00
#SBATCH --mail-type=ALL
#SBATCH --output train_smokePM_fold${a}_drop${b}.log


ml R/4.0.2

Rscript ./02_train_smokePM.R "$a" "$b" " > $job_file

    sbatch $job_file

done



