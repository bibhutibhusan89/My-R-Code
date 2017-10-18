

funGenerateMat<-function(row_count=10,column_count=10){
  mat=matrix(nrow = row_count,ncol = column_count)
  for(i in 1:row_count){
    for(j in 1:column_count){
      if(i<j){mat[i,j]<-1}
      else if(i==j){mat[i,j]<-2}
      else if(i>j){mat[i,j]<--1}
    }
  }

  return(mat[,column_count:1])


}

funGenerateMat(row_count=10,column_count=10)
