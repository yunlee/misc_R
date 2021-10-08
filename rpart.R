fit = rpart(
    pp ~ .,
    data = trainData[, ..varF2RF],
    control = rpart.control(
      minsplit = minSplitValue,
      minbucket = minBucketValue,
      cp = cpValue,
      usesurrogate = usesurrogateValue,
      maxsurrogate = maxsurrogateValue,
      maxdepth = maxdepthValue
    )
  )


subset.rpart <- function(tree, node = 1L) {
  data <- eval(tree$call$data, parent.frame(1L))
  wh <- sapply(as.integer(rownames(tree$frame)), parent)
  wh <- unique(unlist(wh[sapply(wh, function(x) node %in% x)]))
  data[rownames(tree$frame)[tree$where] %in% wh[wh >= node], ]
}

parent <- function(x) {
  if (x[1] != 1)
    c(Recall(if (x %% 2 == 0L) x / 2 else (x - 1) / 2), x) else x
}

get_node_quantile <- function(node, tree, quantile_value, limit_floor){
  data_subset = subset.rpart(tree, node)
  quantile_result = min(limit_floor, quantile(data_subset$pp, probs = quantile_value))
  return(quantile_result)
}


pfit <- as.party(fit)
plot(pfit)

plot(fit)
text(fit, pretty=1)

postorder <- function(orderlist, s, all_nodes) {
  if ((s*2) %in% all_nodes) orderlist = postorder(orderlist, s*2, all_nodes)
  if ((s*2 +1)  %in% all_nodes) orderlist = postorder(orderlist, s*2+1, all_nodes)
  orderlist = c(orderlist, s)
  return(orderlist)
}

orderlist = NULL
orderlist = postorder(orderlist, 1, all_nodes)

all_paths <- path.rpart(fit,orderlist,print.it = FALSE)

result_list = sapply(orderlist, get_node_quantile, fit, quantile_value, limit_floor)




preorder <- function(orderlist, s, all_nodes) {
  orderlist = c(orderlist, s)
  if ((s*2) %in% all_nodes) orderlist = preorder(orderlist, s*2, all_nodes)
  if ((s*2 +1)  %in% all_nodes) orderlist = preorder(orderlist, s*2+1, all_nodes)
  return(orderlist)
}
