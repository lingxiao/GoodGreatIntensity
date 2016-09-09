#!/bin/bash

# directory for logs
#$ -o $PWD/job-output
#$ -e $PWD/job-output


# $HOME/sample_applications/sample_sge_script_1.sh

# wallclock time reservation (format is hours:minutes:seconds).
# man 5 complex
#$ -l h_rt=0:10:0

# request 1 gigabyte of RAM 
# man 5 complex
#$ -l mem=40G

# name of job
# man 1 qsub
#$ -N main

# working directory (check for specific requirements for your research group)
# man 1 qsub


# make sure I set my $CWD (current working directory)
cd $HOME/xiao/GoodGreatIntensity/scripts


# when am I running
#/bin/date

# where am I running
#/bin/hostname

# what environment variables are available to this job script, e.g. $JOB_ID
#/usr/bin/env
#echo $JOB_ID $SGE_STDOUT_PATH 

# run my scripts
$HOME/xiao/GoodGreatIntensity/.stack-work/install/x86_64-linux/lts-6.11/7.10.3/bin/GoodGreatIntensity-exe
