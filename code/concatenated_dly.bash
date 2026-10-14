#!/usr/bin/env bash

#pipe
#tar Oxvzf practice.tar.gz > practice.output
#gzip practice.output



# | the pipe, wc -l, to see how many entities are there
# ls data/ghcnd_all/ | wc -l 

mkdir -p data/temp

# tar Oxvzf data/ghcnd_all.tar.gz | grep "PRCP" | split -l 1000000 --filter 'gzip >  data/temp/$FILE.gz'
# | gzip > data/ghcnd_cat.fwf.gz


tar -Oxzf data/ghcnd_all.tar.gz \
  | awk 'substr($0, 18, 4) == "PRCP"' \
  | gsplit -l 500000 \
      --filter='gzip > "$FILE.gz"' \
      - data/temp/prcp_

code/read_split_dly_files.R

rm -rf data/temp