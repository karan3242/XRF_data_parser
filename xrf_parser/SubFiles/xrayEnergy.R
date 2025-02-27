source("./xrf_parser/SubFiles/preamble.R")

df <- read_xlsx("xray_energy_table.xlsx") %>%
  mutate(Element = word(Element,1))
