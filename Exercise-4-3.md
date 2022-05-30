Exercise 4
================

## Illustration of friendship network

We’ll use `tidygraph` package to create and plot the friendship network
from the slides.

We’ll be relying on these tutorials:

-   <http://users.dimi.uniud.it/~massimo.franceschet/ns/syllabus/make/tidygraph/tidygraph.html>
-   <https://www.data-imaginist.com/2017/introducing-tidygraph/>
-   <https://www.data-imaginist.com/2018/tidygraph-1-1-a-tidy-hope/>

First, we define the network by manually entering all the nodes and the
connections between them. We’ll define `nodes` table that has two
columns: `id` and `name`. We will then define the connections among them
as an edgelist, where each element in column `from` corresponds to a
friend on one end of the relationship and each element in column `to`
corresponds to the person on the other end of this friendship tie.

``` r
# define nodes
node_names <- tibble(
  id   = c(1,2,3,4,5,6,7,8,9,10),
  name = c("1","2","3","4","5","6","A","B","C","D")
)
node_names
```

    ## # A tibble: 10 × 2
    ##       id name 
    ##    <dbl> <chr>
    ##  1     1 1    
    ##  2     2 2    
    ##  3     3 3    
    ##  4     4 4    
    ##  5     5 5    
    ##  6     6 6    
    ##  7     7 A    
    ##  8     8 B    
    ##  9     9 C    
    ## 10    10 D

``` r
# define connections (have to correspond to ties 1-2, 2-3, 2-4, 3-4)
# for each element in `from` there is a corresponding element in `to`
edge_list <- tibble(
  from = c(1,2,7,7,8,8,8,8,9,9,9,10,10,10,3,3,5),
  to   = c(2,7,8,9,9,10,3,6,10,3,4,3,6,5,5,4,6)
)
edge_list
```

    ## # A tibble: 17 × 2
    ##     from    to
    ##    <dbl> <dbl>
    ##  1     1     2
    ##  2     2     7
    ##  3     7     8
    ##  4     7     9
    ##  5     8     9
    ##  6     8    10
    ##  7     8     3
    ##  8     8     6
    ##  9     9    10
    ## 10     9     3
    ## 11     9     4
    ## 12    10     3
    ## 13    10     6
    ## 14    10     5
    ## 15     3     5
    ## 16     3     4
    ## 17     5     6

We can now combine these tables into a “graph” object that holds all of
our network data.

``` r
# combine this information into a network graph object
friendship_graph <- tbl_graph(nodes = node_names, edges = edge_list, directed = FALSE)
friendship_graph
```

    ## # A tbl_graph: 10 nodes and 17 edges
    ## #
    ## # An undirected simple graph with 1 component
    ## #
    ## # Node Data: 10 × 2 (active)
    ##      id name 
    ##   <dbl> <chr>
    ## 1     1 1    
    ## 2     2 2    
    ## 3     3 3    
    ## 4     4 4    
    ## 5     5 5    
    ## 6     6 6    
    ## # … with 4 more rows
    ## #
    ## # Edge Data: 17 × 2
    ##    from    to
    ##   <int> <int>
    ## 1     1     2
    ## 2     2     7
    ## 3     7     8
    ## # … with 14 more rows

We can now plot this network using `ggraph` package.

``` r
friendship_graph %>% 
    ggraph(layout = 'kk') + 
    geom_edge_link() + 
    geom_node_point(size = 8, colour = 'gray') +
    geom_node_text(aes(label = name), colour = 'steelblue', vjust = 0.4) + 
    ggtitle('Friendship network') + 
    theme_graph()
```

![](Exercise-4_files/figure-gfm/plot-graph-1.png)<!-- -->

We can use many of the functions in package `tidy_graph` to calculate
things we want to know about this network. For example, we may want to
know the centrality of each node in the network.

``` r
friendship_graph <- friendship_graph %>% 
  activate(nodes) %>% # we need to state we'll be adding to nodes, not edges
  mutate(d_centrality = centrality_degree()) %>%  # adding measure of degree centrality
  mutate(b_centrality = centrality_betweenness()) # adding betweenness centrality
```

    ## Warning in betweenness(graph = graph, v = V(graph), directed = directed, :
    ## 'nobigint' is deprecated since igraph 1.3 and will be removed in igraph 1.4

``` r
friendship_graph
```

    ## # A tbl_graph: 10 nodes and 17 edges
    ## #
    ## # An undirected simple graph with 1 component
    ## #
    ## # Node Data: 10 × 4 (active)
    ##      id name  d_centrality b_centrality
    ##   <dbl> <chr>        <dbl>        <dbl>
    ## 1     1 1                1        0    
    ## 2     2 2                2        8    
    ## 3     3 3                5        4.63 
    ## 4     4 4                2        0    
    ## 5     5 5                3        0.533
    ## 6     6 6                3        0.933
    ## # … with 4 more rows
    ## #
    ## # Edge Data: 17 × 2
    ##    from    to
    ##   <int> <int>
    ## 1     1     2
    ## 2     2     7
    ## 3     7     8
    ## # … with 14 more rows

Now let’s plot this with degree centrality determining the size of the
nodes and betweenness determining its color.

``` r
friendship_graph %>% 
  ggraph(layout = 'kk') + 
  geom_edge_link() + 
  geom_node_point(aes(size = d_centrality, colour = b_centrality)) + 
  scale_color_continuous(guide = 'legend') +
  geom_node_text(aes(label = name), colour = 'red', vjust = 1.6) + 
  ggtitle('Friendship network') + 
  theme_graph()
```

![](Exercise-4_files/figure-gfm/plot-centrality-1.png)<!-- -->

Seats B, C, and D all offer equally desirable degree centrality, but B
offers the the greatest betweenness centrality. Although A offers the
greatest betweenness centrality, its degree centrality is not as
desirable.

B is likely the best choice of a seat. The density of connections
(degree centrality) is probably the most important consideration in this
context. As an intern, you will want to make connections with as many
people as possible. In addition, its high betweenness centrality means
that you are be strategically located between two small clusters of
seats, which could also be useful.

There are some situations where you might want to choose an entirely
different seat. For example, if you notice that the person in seat 5 is
a manager in the department where you want to work, you should probably
choose seat D. Or, if you are more interested in forming a smaller
number of close connections, rather than a larger number that may not be
as close, you may want to consider seat A.

Team for this exercise: Ranvir, Patrick
