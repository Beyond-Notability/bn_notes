# general functions for network analysis ####

library(tidygraph)
library(ggraph)
library(widyr)




# create an undirected tbl_graph using person id as node id ####
# n = nodes list, e = edges list. need to be in the right sort of format! 
bn_tbl_graph <- function(n, e){
  tbl_graph(
    nodes= n,
    edges= e,
    directed = FALSE,
    node_key = "person"
  )
}




# directed version
bn_tbl_graph_dir <- function(n, e){
  tbl_graph(
    nodes= n,
    edges= e,
    directed = TRUE,
    node_key = "person"
  )
}



# example to make a network of people (using all data or an already filtered subset) ####
# using aggregated edges list
# will probably need to adapt per network

# bn_make_network_simple <- function(data){
#   nodes <-
#     data |>
#     group_by(person) |>
#     summarise(grp_*** = n() , .groups = "drop_last") |>
#     ungroup() |>
#     arrange(person) |>
#     inner_join(
#       data |>
#         count(person, personLabel, gender, year1, year2, name="all_***"), by="person")  |>
#     # should match the numerical ids in edges...
#     mutate(person_id = row_number())
#   
#   edges <-
#     data |>
#     pairwise_count(person, ***, upper=F) |>
#     select(from=item1, to=item2, weight=n) |>
#     arrange(from, to)
#   
#   bn_tbl_graph(nodes, edges)
# }


## possible basis for a sub network using time periods
# bn_sub_network <- function(data, start, period){
#   
#   # make end year
#   end <- start + period - 1  # have to subtract 1!
#   
#   # filter data.
#   filtered_data <-
#     data |>
#     filter(between(e_year1, start, end))
#   
#   nodes <-
#     filtered_data |>
#     group_by(person) |>
#     summarise(grp_excavations = n() , .groups = "drop_last") |>
#     ungroup() |>
#     arrange(person) |>
#     inner_join(
#       data |>
#         count(person, personLabel, gender, year1, year2, name="all_excavations"), by="person")  |>
#     mutate(person_id = row_number())
#   
#   edges <-
#     filtered_data |>
#     #distinct(excavation, person)
#     pairwise_count(person, excavation, upper=F) |>
#     select(from=item1, to=item2, weight=n) |>
#     arrange(from, to)
#   
#   bn_tbl_graph(nodes, edges)
# }
# 




## subsequent functions might not work with more complex network structures.

# centrality scores: degree, betweenness, centrality, harmony, eigenvector. must have weight col.
# network has to be a tbl_graph
bn_centrality <- function(network){
  network |>
    # don't use the weights column by default
    mutate(degree = centrality_degree(weights=weight),
           betweenness = centrality_betweenness(weights=weight), # number of shortest paths going through a node
           #closeness = centrality_closeness(weights=weight), # how many steps required to access every other node from a given node
           harmonic = centrality_harmonic(weights=weight), # variant of closeness for disconnected networks
           eigen = centrality_eigen(weights=weight) # how well connected to well-connected nodes
    )  |>
    # make rankings. wondering whether to use dense_rank which doesn't leave gaps.
    mutate(across(c(degree, betweenness, harmonic, eigen),  ~min_rank(desc(.)), .names = "{.col}_rank")) 
    # closeness lower=more central so needs to be ranked the other way round from the rest !
    # mutate(across(c(closeness),  min_rank, .names = "{.col}_rank"))
}


# community detection
# two methods for the moment, unweighted.

bn_clusters <- function(network){
  network |>
    mutate(infomap = as.factor(group_infomap())) |>
    mutate(e_btwnness = as.factor(group_edge_betweenness(directed=FALSE)))
}





## heatmap to compare different centrality rankings for top individuals

gg_centrality_heatmap <- function(df, tag){
  df |>
    mutate(rank2 = if_else(rank>10, NA, rank)) |>
    mutate(centrality = str_remove(centrality, "_rank")) |>
    # sort by median ranking rather than alphabetical
    group_by(person) |>
    mutate(sort = median(rank)) |>
    ungroup()  |>
    mutate(personLabel = fct_reorder(personLabel, -sort)) |>
    #mutate(personLabel=fct_rev(personLabel)) |>
    ggplot(aes(y=personLabel, x=centrality, fill=rank2, label=rank)) +
    geom_raster() +
    geom_text(colour="white") +
    labs(y=NULL, fill="rank", title=tag)
}


## get top r ranked persons for each centrality measure
## NB data must have "gender" column

network_centrality_top_r <- function(network, r){
  network |>
    as_tibble() |>
    select(person, personLabel, gender, degree_rank, betweenness_rank, eigen_rank, harmonic_rank) |>
    pivot_longer(ends_with("_rank"), names_to = "centrality", values_to = "rank") |>
    filter(rank <= r, .by=centrality)
}

## get top r ranked persons for each centrality measure plus their rankings for other measures
## NB data must have "gender" column (could be blank...)

network_centrality_top_r_plus <- function(network, r){
  # first get individuals who are in top r for any measure
  top_r <-
    network |>
    as_tibble() |>
    select(person, degree_rank, betweenness_rank, eigen_rank, harmonic_rank) |>
    pivot_longer(ends_with("_rank"), names_to = "centrality", values_to = "rank") |>
    filter(rank <= r, .by=centrality) |>
    distinct(person)
  
  # then get all the measures for those individuals (to include rankings that are not in the top r)
  top_r |>
    left_join(
      network |>
        as_tibble() |>
        select(person, personLabel, gender, degree_rank, betweenness_rank, eigen_rank, harmonic_rank) |>
        pivot_longer(ends_with("_rank"), names_to = "centrality", values_to = "rank")
      , by="person")
  
  #filter(rank <= p, .by=centrality) # could add a filter eg 100 here as well
}




# simple graph of the immediate connections of a specified individual. should be possible to do something cleverer later...
bn_one_person_circle <- function(network, q){
  
  focus <- 
    network |>
    as_tibble() |>
    filter(person==q) |>
    pull(person_id)
  
  
  
  network_filtered <-
    network |>
    activate(edges) |>
    filter(from==focus|to==focus) |> 
    activate(nodes) |>
    filter(!node_is_isolated()) |> # CHANGES ALL THE ID NUMBERS IN EDGES 
    # make new ids in the node table.
    mutate(id = row_number()) |>
    # node_is_center identifies the centre of the circle...  
    mutate(focus_id = if_else(node_is_center(), id, NA)) 
  
  nodes <-
    network_filtered |>
    as_tibble() |>
    # fill focus_id both directions. to be used in focus= in  ggraph
    fill(focus_id, .direction="downup") |>
    # new id to first col so it will be used to link from/to
    relocate(id) 
  
  edges <-
    network_filtered |>
    activate(edges) |>
    as_tibble() |>
    select(from, to, weight)
  
  tbl_graph(nodes=nodes, edges = edges, directed = FALSE)
  
}

## make network graph of the immediate circle

bn_circle_ggraph <- function(data){
  
  # name label for tag
  focus_name_label <-
    data |>
    as_tibble() |>
    filter(id==focus_id) |>
    pull(personLabel)
  
  # bn id of focus person
  focus_name_id <-
    data |>
    as_tibble() |>
    filter(id==focus_id) |>
    pull(person)
  
  
  # focus
  focus_person_id <-
    data |>
    activate(edges) |>
    mutate(focus_person = which(.N()$person == focus_name_id)) |>
    as_tibble() |>
    pull(unique(focus_person))
  
  ggraph(data, "focus", focus=focus_person_id, weights=1/weight) +
    geom_edge_link(aes(alpha=weight), show.legend = F) +
    geom_node_point(size=2,  aes(colour=gender), show.legend = F)+
    geom_node_text(aes(label = personLabel, colour=gender), size=3, show.legend = F, repel = T, max.overlaps=25)+
    coord_fixed() + # circular instead of oval
    scale_colour_tableau() +
    theme_graph() +
    labs(tag=focus_name_label)
}



bn_circle_ggraph2 <- function(data){
  
  # name label for tag
  focus_name_label <-
    data |>
    as_tibble() |>
    filter(id==focus_id) |>
    pull(personLabel)
  
  # bn id of focus person
  focus_name_id <-
    data |>
    as_tibble() |>
    filter(id==focus_id) |>
    pull(person)
  
  
  # focus
  focus_person_id <-
    data |>
    activate(edges) |>
    mutate(focus_person = which(.N()$person == focus_name_id)) |>
    as_tibble() |>
    pull(unique(focus_person))
  
  ggraph(data, "focus", focus=focus_person_id, weights=1/weight) +
    geom_edge_link(aes(alpha=weight), show.legend = F) +
    geom_node_point(size=1,  aes(colour=gender), show.legend = F)+
    geom_node_text(aes(label = personLabel, colour=gender), size=2, show.legend = F, repel = T, max.overlaps=30)+
    coord_fixed() + # circular instead of oval
    scale_colour_tableau() +
    theme_graph() +
    labs(tag=focus_name_label)
}


