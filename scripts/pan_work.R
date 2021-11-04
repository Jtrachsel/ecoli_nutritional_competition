library(pdtools)
library(tidyverse)
library(furrr)


pan_PA <-
  read_tsv('./pan99/gene_presence_absence.Rtab') |>
  column_to_rownames(var = 'Gene') |>
  as.matrix() |>
  t()

dim(pan_PA)

# pan_PA[1:10, 1:10]

# pan_PA <- pan_PA[,colSums(pan_PA) > 1]


# reps <- get_gene_content_reps(pan_mat = pan_PA)
#
# reps500 <- get_gene_content_reps(pan_mat = pan_PA,
#                                  starting_set_size = 5000,
#                                  desired_coverage = .98,
#                                  set_size_step = 100,
#                                  num_iters_per_size=500)

# reps$value
# reps$set_size

# pan_PA_filt <- pan_PA[,colSums(pan_PA) > 2]




# TST <- get_gene_content_reps2(pan_mat = pan_PA, desired_coverage = .975)


results_sets <-
  tibble(seed=c(1:100),
         set=map(.x = seed, ~ get_pangenome_representatives(pan_mat = pan_PA,SEED = .x, desired_coverage = .99)))

# TST[[1]]

seed

set_indexes <- map(results_sets$set,2)
map(set_indexes, length) %>% unlist() %>% hist()


meta <- read_tsv('./data/North_America_stx_metadata.tsv')

reps <- meta %>% filter(asm_acc %in% TST[[1]])


results_sets$set
res_plot_dat <-
  function(result, seed_num){
    scores <- result[[2]]
    # plot(1:length(scores), scores)
    tibble(seed_num,num_genomes=1:length(scores), scores)
  }

res_plot_dat(results_sets$set[[1]], 1)

map2(.x = results_sets$set,.y = results_sets$seed, res_plot_dat) %>%
  bind_rows() %>%
  ggplot(aes(x=num_genomes, y=scores, color=factor(seed_num))) + geom_point()

genome_set_frequency <-
  map(results_sets$set, 1) %>%
  purrr::reduce(c) %>%
  tibble(genome_name=.) %>%
  group_by(genome_name) %>%
  tally()

genome_set_frequency$n %>% hist
genome_set_frequency %>% filter(n == 10)


results_sets %>%
  mutate(genome_names=map(set, 1),
         scores=map(set, 2)) %>%
  select(-set) %>%
  mutate(scores=map_chr(scores, ~paste(.x, collapse = ',')),
         genome_names=map_chr(genome_names, ~paste(.x, collapse = ','))) %>%
  write_tsv('./pan_representative_sets_99.tsv')

meta <- read_tsv('./data/O157:H7_meta.tsv')

meta_filt <- meta %>% filter(asm_acc %in% genome_set_frequency$genome_name)
