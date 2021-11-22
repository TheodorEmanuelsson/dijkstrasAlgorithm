#' Dijkstra Algorithm
#' 
#' @param graph Data.frame with three variables (v1, v2 and w) that contains the edges of the graph (from v1 to v2) with the weight of the edge (w).
#' @param init_node Numeric scalar that is the starting node. 
#' @return Returns the shortest path to every other node from the starting node as a vector.
#' @examples
#' dijkstra(data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9)), 1)
#' dijkstra(data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9)), 3)
#' @source \url{https://en.wikipedia.org/wiki/Dijkstra's_algorithm}
#' @export

dijkstra <- function(graph,init_node){
  stopifnot(is.numeric(init_node), length(init_node) == 1, init_node %in% graph$v1, 
            is.data.frame(graph), ncol(graph) == 3, colnames(graph) == c("v1","v2","w"))
  
  Q_vert <- graph
  
  dist <- rep(Inf, max(Q_vert[,1:2]))
  dist[init_node] <- 0 
  
  coun <- 1:max(Q_vert[,1:2])
  prev <- rep(NA, max(Q_vert[,1:2]))
  prev[init_node] <- 0
  
  u <- which.min(dist)
  
  while(!all(!is.na(prev))){
    
    for(v in Q_vert$v2[which(Q_vert$v1 == u)]){
      if(v %in% coun){
        alt  <- dist[u] + Q_vert$w[which(Q_vert$v1 == u & Q_vert$v2 == v)]
        
        if(alt < dist[v]){
          dist[v] <- alt
          prev[v] <- u
        }
      }
    }
    
    coun <- coun[-which(coun == u)]
    u <- which(dist==min(dist[which(1:max(Q_vert[,1:2]) %in% coun)]))
  }
  return(dist)
}
