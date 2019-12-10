library(PTTmineR)
library(widyr)
library(visNetwork)
library(igraph)
library(ggplot2)

foo_miner <- PTTmineR$new(task.name = "Recommend Post Analysis")

plan(multiprocess)

# mine & export data to global env
foo_miner %>% 
  mine_ptt(board = "Gossiping",
           recommend = 50, 
           min.date = "2019-12-01") %>% 
  export_ptt("tbl", "result_list")

# filter posts' last date we want to analysis
result_list$post_info_tbl <- 
  result_list$post_info_tbl %>%
  filter(post_date_time < as.POSIXct("2019-12-08 23:59:59"))

result_list$post_comment_tbl <- result_list$post_comment_tbl %>% 
  filter(post_id %in% result_list$post_info_tbl$post_id)

# calculate phi correlation between each ids
push_id_cors <- result_list$post_comment_tbl %>%
  select(1,3) %>% 
  unique() %>% 
  group_by(push_id) %>%
  filter(n() >= 10) %>%
  pairwise_cor(push_id, post_id, sort = TRUE)

# explore density plot
ggplot(push_id_cors, aes(correlation)) +
  geom_density() +
  theme_bw()

# define the nodes and the edges 

nodes <- push_id_cors %>%
  filter(correlation > .8) %>% 
  select(item1) %>% 
  unique()

colnames(nodes) <- c("id")

edges <- push_id_cors %>%
  filter(correlation > .8)

colnames(edges) <- c("from", "to", "width")

# network visualization

visNetwork(nodes, edges, height = "500px", width = "100%") %>%
  visIgraphLayout(layout = "layout_with_fr") %>%
  visNodes(
    shape = "dot",
    size = 10,
    font = list(
      size = 40,
      face = "verdana",
      strokeWidth = 3
    ),
    physics = TRUE,
    color = list(background = "#0085AF",
                 highlight = "#DD5437"),
    
  ) %>%
  visEdges(color = list(color = "#0085AF", highlight = "#C62F4B")) %>%
  visPhysics(barnesHut = list(
    gravitationalConstant = -5500,
    springLength = 400,
    centralGravity = 0.05
  )) %>%
  visOptions(highlightNearest = list(
    enabled = T,
    degree = 1,
    hover = T
  )) %>%
  visEvents(
    doubleClick =
      "function(e) {
                var win = window.open('https://www.plytic.com/authors/' + this.body.data.nodes.get(e.nodes[0]).id, '_blank');
                win.focus();
            }"
  )