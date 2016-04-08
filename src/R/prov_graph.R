library(dplyr)
library(tidyr)
library(readr)
library(DiagrammeR)

x <- read_csv('prep/spp_ico/prov/data_prep_spp.Rmd.csv') %>%
  as.data.frame()

x <- x %>% filter(run_id == max(run_id)) %>%
  mutate(from = rdf_subject,
         to   = rdf_object,
         rel  = rdf_predicate) %>%
  mutate(from = ifelse(type == 'output', rdf_object, from),
         to   = ifelse(type == 'output', rdf_subject, to),
         rel  = ifelse(type == 'output', 'generated', rel))

nodes_df <- x %>%
  select(file_loc, type, commit_url) %>%
  mutate(nodes = file_loc,
         label = basename(file_loc),
         shape = ifelse(type == 'input', 'oval', 'rectangle')) %>%
  unique()

edges_df <- x %>%
  select(from, to, rel) %>%
  filter(!from == to) %>%
  mutate(label = rel)
  unique()

prov_gr <- create_graph(nodes_df = nodes_df,
             edges_df = edges_df,
             graph_attrs  = NULL,
             node_attrs   = NULL,
             edge_attrs   = NULL,
             directed     = TRUE,
             graph_name   = NULL,
             graph_time   = NULL,
             graph_tz     = NULL,
             generate_dot = TRUE)

render_graph(prov_gr)

### organize better
### color code re: committed/unchanged = green, committed/changed = yellow, uncommitted = red?
### link to file on github or something?
### proper RDF predicates, shapes, etc
### info on plot: run type, date, name; sys.info, session.info

### on Rmd data.table (FOR DISPLAY ONLY):
### * truncate the commit URL: [six digits](url)
### * file path separate
ditch commit author, date,
