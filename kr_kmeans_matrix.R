
# x is the input vector
# k is the clusters count

kr_clust = function(x, k){
  x=as.matrix(x)
  id = 1:nrow(x)
  cent = x[sample(id, k),]
  if(length(unique(x))>=k){
    while(nrow(unique(cent))!=k){
      cent = x[sample(id, k),]
    }}else{stop("provide k greater than unique classes in input")}
  row.names(cent)=1:k
  e = sum(cent)
  
  while(e>(0.005)){
    res = NULL
    print(cent)
    cent_prev = cent
    for(i in 1:nrow(x)){
      d_list = NULL
      for(j in 1:nrow(cent)){
        d = dist(rbind(x[i,],cent[j,]))
        d_list = append(d_list, d)
      }
      d_list = as.array(d_list)
      row.names(d_list) = 1:k
      val = as.numeric(row.names(as.array(d_list[which.min(d_list)])))
      res = append(res, val)
    }
  
    df = data.frame(x, clust = res)
    cent = aggregate(df[,-ncol(df)], by = list(df$clust), mean)
    row.names(cent) = cent[,1]
    cent = cent[,-1]
    e = abs(sum(cent_prev - cent))
  }
  list(df,cent, e)
}




