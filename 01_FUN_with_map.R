library(purrr)
library(dplyr)
library(tidyr)
library(stringr)
library(repurrrsive)
library(glue)
library(ggraph)

dat <- got_chars


dat_m <- dat %>%{
  tibble(name = map_chr(., "name"),
         gender = map_chr(., "gender"),
         culture = map_chr(., "culture"),
         aliases = map(., "aliases"), # using map since the lists aliases and allegiances have more than one entry. map_chr wants to return only one string
         allegiances = map(., "allegiances"))}

dat_m_bracket <- dat %>%{
  tibble(name = map_chr(., "name"),
    gender = map_chr(., "gender"),
    culture = map_chr(., "culture"),
    aliases = map(., "aliases"), # using map since the lists aliases and allegiances have more than one entry. map_chr wants to return only one string
    allegiances = map(., "allegiances"))} %>% # map inserts the list as vector. 
    unnest_wider(aliases, # We can then separate it further using dplyr
                 names_sep = "_") %>% # the names_sep keeps the column names
    unnest_wider(allegiances, names_sep = "_")
  


# Apply a custom function to a list ####
dead_or_alive <- function(x){
  ifelse(x[["alive"]], paste(x[["name"]], "is alive!"),
         paste(x[["name"]], "is dead!"))
}

map_chr(dat, dead_or_alive)


g <- map(dat, compact) # remove empty lists out of the data

# Function handing out all aliases of a person in a network graph format ####
also_known_as <- function(x){
  
  if("aliases" %in% names(x)){
    g <- tibble(
      from = x$name,
      to = x$aliases)
    
    g <- as_tbl_grph()
  }
}

g <- map(g, also_known_as)
# plot the network graph ####
 ggraph(g[[23]], layout = "graphopt") +
   geom_edge_link() + 
   geom_node_label(aes(label = name),
                   label.padding = unit(1, "lines"),
                   label.size = 0) + 
   theme_graph()

# pmap function ####

#function which lets us apply functions to each row. Iterates through dataframe, through each row

dat_p <- dat_m %>%
  mutate(stark_or_lannister = map(allegiances, ~str_extract(.x, "Lannister|Stark")))

dat_p$stark_or_lannister[16:18]

dat_p <- dat_p %>% # drop all NA values using the discard function
  mutate(stark_or_lannister = map(stark_or_lannister, ~discard(.x, is.na)))

dat_p$stark_or_lannister[16:18]

dat_p <- filter(dat_p, stark_or_lannister %in% c("Lannister", "Stark")) %>% # filter for all rows that are either stark or lannister
  unnest(stark_or_lannister) 
