# My-R-Code
## Conversion of a word contains letter to numeric
letterToNumeric<-function(x){
 sapply(unlist(strsplit(x,split='')),function(...){convert(...)}) 
}

## Conversion of a letter into consecutive number
convert<-function(x){
  if(length(which(x==LETTERS))!=0) which(x==LETTERS)
  else x
}

letterToNumeric('A1243Z') o/p - 1 1 2 4 3 26
