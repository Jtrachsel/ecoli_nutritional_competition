library(tidyverse)
library(pdtools)
library(Biostrings)
library(lubridate)


# generate ppanggolin file

all_fastas <- list.files(path='fastas', pattern = '.fna', full.names = T)

complete_genomes <- grep('GCF|Ecoli', all_fastas, value = T)
incomplete_genomes <- grep('GCF|Ecoli', all_fastas, value = T, invert = T)

build_ppanggolin_file_fastas(complete_genome_paths = complete_genomes,
                             incomplete_genome_paths = incomplete_genomes) %>%
  write_tsv('ppanggolin.file', col_names = F)


