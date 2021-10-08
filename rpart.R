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
 
 pfit <- as.party(fit)
plot(pfit)



head(subset.rpart(fit, 5))
#    Kyphosis Age Number Start
# 2    absent 158      3    14
# 10  present  59      6    12
# 11  present  82      5    14
# 14   absent   1      4    12
# 18   absent 175      5    13
# 20   absent  27      4     9


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


paths <- path.rpart(fit,"8",print.it = FALSE)

pfit <- as.party(fit)
plot(pfit)

plot(fit)
text(fit, pretty=1)

setClass("tree", slots=list(left=NULL, right=NULL, value="numeric"))

max_node = max(as.numeric(rownames(fit$frame)))

postorder = NULL
layer = floor(log(max_node,2))

leftnum = 2^(seq(0,layer))
all_nodes = as.numeric(rownames(fit$frame))
postorder = c(postorder, max(intersect(leftnum, all_nodes)))

if (most_left+1 %in% all_nodes) {
  x = most_left+1
  postorder = c(postorder, max(intersect(all_nodes, x*(2^seq(0,floor(log(max_node/x,2))))))
  add
} else {

}

find_max <- function(x, max_node){
  while ( x < max_node){
    x <- x*2
  }
  return(x)
}

max(intersect(all_nodes, x*(2^seq(0,floor(log(max_node/x,2))))))


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
