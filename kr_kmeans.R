
# x is the input vector
# k is the clusters count

kr_clust = function(x, k){
  id = 1:length(x)
  cent = x[sample(id, k)]
  if(length(unique(x))>=k){
    while(length(unique(cent))!=k){
      cent = x[sample(id, k)]
    }}else{stop("provide k greater than unique classes in input")}
  e = sum(cent)
  
  while(e>(0.005)){
    res = NULL
    print(cent)
    cent_prev = cent
    for(i in x){
      d_list = NULL
      for(j in cent){
        d = dist(c(j,i))
        d_list = append(d_list, d)
      }
      d_list = as.array(d_list)
      row.names(d_list) = 1:k
      val = as.numeric(row.names(as.array(d_list[which.min(d_list)])))
      res = append(res, val)
    }
  
    df = data.frame(x, clust = res)
    cent = tapply(df$x,df$clust, mean)
    sse = sum((df$x - cent[df$clust])^2)
    e = abs(sum(cent_prev - cent))
  }
  list(df,cent, sse)
}




