#!/bin/bash

in_dir=$1
out_dir=$2
runner=$3
echo "Processing $in_dir to $out_dir"

if [ ! -d $in_dir ]
then
  echo "$in_dir doesn't exist. Abort!"
  exit -1
fi

mkdir -p $out_dir

flist=$(ls $in_dir/*.Rmd)
for fsc in $flist 
do
  echo "  Processing $fsc"
  R CMD BATCH --no-save --no-restore "--args $fsc $out_dir html" $runner
  if [ $? -ne 0 ]
  then
    echo "Error while executing $fsc"
    exit -1
  fi
  cat run_test_rmd.Rout
done

echo "Done"
