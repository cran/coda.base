## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(digits = 4)

## -----------------------------------------------------------------------------
library(coda.base)
X = parliament2017[,-1]

## -----------------------------------------------------------------------------
B1 = pb_basis(X, method = "exact")

## ---- fig.width=7.5, fig.height=4.5-------------------------------------------
plot(B1)

## -----------------------------------------------------------------------------
apply(coordinates(X, B1), 2, var)

## ---- echo=FALSE, fig.width=7.5, fig.height=4.5, warning=FALSE----------------
library(ggplot2)
D = 10

# time_ = sapply(5:20, function(D){
#   print(D)
#   x = matrix(rlnorm(100*D), ncol=D)
#   a = tic()
#   r = pb_basis(x, method = 'exact')
#   b = toc()
#   b$toc - b$tic
# })
# dplot = data.frame(
#   parts = tail(5:20, 11),
#   time = tail(time_, 11)
# )

dplot = structure(list(parts = 10:20, time = c(0.00499999999999545, 0.0119999999999436, 
0.0260000000000673, 0.0749999999999318, 0.243000000000052, 0.831999999999994, 
2.64400000000001, 9.56200000000001, 28.787, 95.4560000000001, 
311.133)), class = "data.frame", row.names = c(NA, -11L))

ggplot(data=dplot) +
  geom_point(aes(x=parts, y=time)) +
  geom_segment(aes(x=parts, xend =parts, y = 0, yend=time)) +
  labs(x = 'Number of parts', y = 'Time in seconds (logarithmic scale)', title = 'Time needed to calculate Principal Balances',
       caption = "Times measured in a MacBook Pro (13-inch, 2017) \n2,3 GHz Dual-Core Intel Core i5.16 GB 2133 MHz LPDDR3") +
  scale_x_continuous(breaks = tail(5:20, 11)) +
  scale_y_continuous(trans = 'log', breaks = c(0.004, 0.04, 0.4, 4, 40, 400), labels = c("0.004", "0.04", "0.4", 4, 40, 400)) +
  theme_minimal()

## -----------------------------------------------------------------------------
D = as.dist(variation_array(X, only_variation = TRUE))
D

## ---- fig.width=7.5, fig.height=4.5-------------------------------------------
B2 = pb_basis(X, method = 'cluster')
plot(B2)

## -----------------------------------------------------------------------------
apply(coordinates(X, B2), 2, var)

## ---- fig.width=7.5, fig.height=4.5-------------------------------------------
B3 = pb_basis(X, method = 'constrained')
plot(B3)

## -----------------------------------------------------------------------------
apply(coordinates(X, B3), 2, var)

## ---- eval=FALSE, echo=FALSE--------------------------------------------------
#  hc = hclust_dendrogram(B1)
#  hcd = as.dendrogram(hc)
#  dd = dendro_data(hcd)
#  ggdendrogram(dd)
#  dd$segments = dd$segments %>%
#    mutate(
#      end_node = yend == 0
#    )
#  
#  p <- ggplot(dd$segments) +
#    geom_segment(aes(x = x, y = y, xend = xend, yend = yend, linetype = end_node))+
#    geom_label(data = dd$labels, aes(x, y, label = label),
#              hjust = 0.5, angle = 90, size = 4) +
#    theme_void() + scale_linetype_discrete(guide=FALSE)
#  p
#  
#  ## Build tree
#  build_tree_order = function(B, ibalance){
#    balance = B[,ibalance]
#    if(sum(balance != 0) == 2){
#      return(ibalance)
#    }
#    L = NULL; R = NULL
#    left_ = balance < 0
#    if(sum(left_) > 1){
#      L = Recall(B, which(apply((B != 0 & left_) | (B == 0 & !left_), 2, all)))
#    }
#    right_ = balance > 0
#    if(sum(right_) > 1){
#      R = Recall(B, which(apply((B != 0 & right_) | (B == 0 & !right_), 2, all)))
#    }
#    return(unname(c(L, R, ibalance)))
#  }
#  ord_ = build_tree_order(B, which(apply(B != 0, 2, all)))
#  TREE = setNames(lapply(1:ncol(X), function(i) list('c' = i)), names(X)[ORDER])
#  for(i in ord_){
#    balance = B[,i]
#    name_ = paste(sort(names(balance)[balance != 0]), collapse = '_')
#    name_left = paste(sort(names(balance)[balance < 0]), collapse = '_')
#    name_right = paste(sort(names(balance)[balance > 0]), collapse = '_')
#    l_node = list(
#      list(
#        name = name_,
#        left = name_left,
#        right = name_right,
#        l = TREE[[name_left]]$c,
#        r = TREE[[name_right]]$c,
#        c = (TREE[[name_left]]$c+TREE[[name_right]]$c)/2
#    ))
#    names(l_node) = name_
#    TREE = c(TREE, l_node)
#  }
#  nodes = sapply(ord_, function(i){
#    balance = B[,i]
#    paste(sort(names(balance)[balance != 0]), collapse = '_')
#  })
#  TREE = TREE[nodes]
#  lapply(TREE, as_tibble) %>% bind_rows()
#  for(node in rev(nodes)){
#    TREE[[node]]
#  }
#  
#  # lapply(nodes, function(name_){
#  #   tibble(
#  #     name = name_,
#  #     name_l = TREE[[name_]]$left,
#  #     name_r = TREE[[name_]]$right,
#  #     l = TREE[[name_]]$l,
#  #     r = TREE[[name_]]$r,
#  #     c = TREE[[name_]]$c,
#  #     var = var(H[,i])
#  #   )
#  # }) %>% bind_rows()
#  
#  
#  H = coordinates(X, B)
#  H_mean = colMeans(H)
#  H_var = apply(H, 2, var)
#  
#  names(X)[ORDER]
#  
#  l_nodes = lapply(rev(ord_), function(i){
#    list(
#  
#      balance = B[,i],
#      mean = boot::inv.logit(H_mean[i]),
#      var = H_var[i]
#    )
#  })
#  names(l_nodes) = sapply(l_nodes, function(x) paste(sort(names(x$balance)[x$balance != 0]), collapse = '_'))
#  l_nodes

## ---- eval=FALSE, include=FALSE-----------------------------------------------
#  library(coda.base)
#  X = iris[,1:4]
#  B = pb_basis(X, method = 'exact')
#  rownames(B) = names(X)
#  H = coordinates(X, B)
#  apply(H, 2, var)

## ---- eval = FALSE, include=FALSE---------------------------------------------
#  plot(H[,1], H[,2])

## ---- eval=FALSE, include=FALSE-----------------------------------------------
#  lX = split(X, iris$Species)
#  m_ = lapply(lX, colMeans)
#  s_ = lapply(lX, cov)
#  S = cov(X)
#  S_ = replicate(3, S, simplify = FALSE)

## ---- eval=FALSE, include=FALSE-----------------------------------------------
#  Prob = mapply(function(m,s){
#    mvtnorm::dmvnorm(X, m, s)
#  }, m_, S_)

## ---- eval=FALSE, include=FALSE-----------------------------------------------
#  B_1 = pb_basis(Prob, method = 'exact')
#  H_1 = coordinates(Prob, B_1)
#  plot(H_1, col = iris$Species)

## ---- eval=FALSE, include=FALSE-----------------------------------------------
#  B_2 = pc_basis(Prob)
#  H_2 = coordinates(Prob, B_2)
#  plot(-H_2, col = iris$Species)

## ---- eval=FALSE, include=FALSE-----------------------------------------------
#  library(fpc)
#  dp = discrproj(X, iris$Species)
#  plot(dp$proj[,1:2], col = iris$Species)

## ---- eval=FALSE, echo=FALSE--------------------------------------------------
#  @article{cite-key,
#  	Date-Added = {2020-06-13 08:44:39 +0000},
#  	Date-Modified = {2020-06-13 08:44:39 +0000},
#  	Id = {ref25},
#  	Title = {Pawlowsky-Glahn V, Egozcue JJ, Tolosana-Delgado R (2011) Principal balances. In Egozcue JJ, Tolosana-Delgado R, Ortego M (eds) Proceedings of the 4th international workshop on compositional data analysis, Girona, Spain, pp 1--10},
#  	Ty = {STD}}
#  
#  @article{cite-key,
#  	Author = {Mart{\'\i}n-Fern{\'a}ndez, J. A. and Pawlowsky-Glahn, V. and Egozcue, J. J. and Tolosona-Delgado, R.},
#  	Da = {2018/04/01},
#  	Date-Added = {2020-06-13 08:42:47 +0000},
#  	Date-Modified = {2020-06-13 08:42:47 +0000},
#  	Doi = {10.1007/s11004-017-9712-z},
#  	Id = {Mart{\'\i}n-Fern{\'a}ndez2018},
#  	Isbn = {1874-8953},
#  	Journal = {Mathematical Geosciences},
#  	Number = {3},
#  	Pages = {273--298},
#  	Title = {Advances in Principal Balances for Compositional Data},
#  	Ty = {JOUR},
#  	Url = {https://doi.org/10.1007/s11004-017-9712-z},
#  	Volume = {50},
#  	Year = {2018},
#  	Bdsk-Url-1 = {https://doi.org/10.1007/s11004-017-9712-z}}
#  
#  Frank Ruskey LEcture note computer science 762 (1993) 205-206
#  @String{j-LECT-NOTES-COMP-SCI   = "Lecture Notes in Computer Science"}
#  @String{ack-nhfb = "Nelson H. F. Beebe,
#                      University of Utah,
#                      Department of Mathematics, 110 LCB,
#                      155 S 1400 E RM 233,
#                      Salt Lake City, UT 84112-0090, USA,
#                      Tel: +1 801 581 5254,
#                      FAX: +1 801 581 4148,
#                      e-mail: \path|beebe@math.utah.edu|,
#                              \path|beebe@acm.org|,
#                              \path|beebe@computer.org| (Internet),
#                      URL: \path|http://www.math.utah.edu/~beebe/|"}
#  @Article{Ruskey:1993:SCG,
#    author =       "F. Ruskey",
#    title =        "Simple Combinatorial Gray Codes Constructed by
#                   Reversing Sublists",
#    journal =      j-LECT-NOTES-COMP-SCI,
#    volume =       "762",
#    pages =        "201--208",
#    year =         "1993",
#    CODEN =        "LNCSD9",
#    ISSN =         "0302-9743 (print), 1611-3349 (electronic)",
#    ISSN-L =       "0302-9743",
#    bibdate =      "Wed Sep 15 10:01:31 MDT 1999",
#    bibsource =    "http://www.math.utah.edu/pub/tex/bib/lncs1993.bib",
#    acknowledgement = ack-nhfb,
#    keywords =     "algorithms; computation; ISAAC",
#  }

