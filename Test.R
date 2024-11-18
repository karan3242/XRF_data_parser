library(tidyverse)

df <- Maras_a_din_chem_14_11_2024 %>%  select(id = Lab_ID, everything()) %>%
  arrange(id)

df[df == '<LOD'] <- NA

df <- df %>%
  mutate_at(
    vars(
      -id,-Date,-Time,-Units,-Description,-matches("Test Label"),-matches("Collimation Status"),-matches("Method Name"),-matches("Instrument Serial Num"),-matches("Reading #")
    ),
    as.numeric
  )

df[is.na(df)] <- 0

df$id <- as.character(df$id)
df$`Instrument Serial Num` <-
  as.integer(df$`Instrument Serial Num`)
df$`Reading #` <- as.integer(df$`Reading #`)
df$`Test Label` <- as.integer(df$`Test Label`)

df$Notes[df$Notes == 0] <- NA

df$Notes <- as.character(df$Notes)

df2 <- df %>% select(id, contains("Concentration")) %>%
  select(id, sort(names(.)))

df3 <- filter(df2, id == "I7") 

df4 <- pivot_longer(df3, 
                    cols = colnames(df3)[2:max(col(df3))], 
                    names_to = 'Elements', 
                    values_to = 'counts')

ggplot(df4, aes(x = Elements, y = counts)) + geom_boxplot() + geom_jitter()
