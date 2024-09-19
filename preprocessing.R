library(tidyverse)

#all_data <- read_tsv("data/ptp_sites_hjs.txt") %>%
#  select(-site, -prot_description, -Notes)

all_data <- read_tsv("data/ptp_sites.txt") %>%
  select(-site, -prot_description, -blank)

sites <- all_data %>%
  pivot_longer(cols = starts_with("oxi"), names_sep = "[.]", names_to = c("tissue", "age")) %>%
  drop_na(value) %>%
  separate(col = tissue, into = c(NA, NA, "tissue"), sep = "_") %>%
  rename("oxi_percent" = value) %>%
  mutate(age = fct_relevel(age, "young")) %>%
  mutate(residue = as.factor(residue))


chosen_tissue <- "kidney"

p <- sites %>%
  filter(tissue == chosen_tissue) |>
  filter(Gene == "PTPRK") |>
  ggplot(aes(x = age, y = oxi_percent, label = residue)) +
  geom_line(aes(group = residue), linewidth = 0.8, linetype = "dashed", colour = "grey") +
  geom_point(size = 3) +
  ggtitle(chosen_tissue)# +
  #facet_wrap(~Gene, scales = "free")

nested_tissues <- sites |>
  dplyr::select(-Uniprot) |>
  tidyr::nest(.by = tissue)

nested_ptps <- sites |>
  dplyr::select(-Uniprot) |>
  tidyr::nest(.by = Gene)

saveRDS(object = nested_tissues, file = "data/nested_tissues.rds")
saveRDS(object = nested_ptps, file = "data/nested_ptps.rds")


# Hayley added some more columns to the file.
# N is that the protein has a second catalytic/backdoor, but not detectably oxidised. NA means the protein does not have a second PTP domain.
# 
# 
# E.g. PTPRK has a D1-D2 domain with both catalytic and backdoors so scores N if not there for D2
# PTPN1 only has one PTP domain so is NA for second set of  cysteines
# PTPRG has D1-D2, but no D2 catalytic/backdoor cysteines, so is NA.

all_data <- read_tsv("data/ptp_sites_hjs.txt") %>%
  select(-site, -prot_description, -Notes)

sites <- all_data %>%
  pivot_longer(cols = starts_with("oxi"), names_sep = "[.]", names_to = c("tissue", "age")) %>%
  drop_na(value) %>%
  separate(col = tissue, into = c(NA, NA, "tissue"), sep = "_") %>%
  rename("oxi_percent" = value) %>%
  mutate(age = fct_relevel(age, "young")) %>%
  mutate(residue = as.factor(residue))

nested_tissues <- sites |>
  dplyr::select(-Uniprot) |>
  tidyr::nest(.by = tissue)

nested_ptps <- sites |>
  dplyr::select(-Uniprot) |>
  tidyr::nest(.by = Gene)

saveRDS(object = nested_tissues, file = "data/nested_tissues.rds")
saveRDS(object = nested_ptps, file = "data/nested_ptps.rds")











