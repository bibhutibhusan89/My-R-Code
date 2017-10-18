funDateseq<-function(startdate,enddate, ...){
  ## Function Generate Date Sequence From Start Date to End Date and Returns invisibly ##############

  invisible(seq(as.Date(startdate),as.Date(enddate),"days"))
}



funGeneratePattern<-function(startdate,enddate,verbose=FALSE, ...){
  ### Function Generates the Given Pattern #################

  alldays<-funDateseq(startdate=startdate,enddate=enddate)
  wdays<-weekdays(alldays)
  monthall<-months(alldays)
  dayall<-as.numeric(sapply(strsplit(as.character(alldays),"-"),"[[",3))
  pattern<-paste(substr(wdays,start = 1,stop = 1),"-",dayall)
  if(verbose){print("########### pattern generated ########")}
  mat1<-data.frame(alldays,monthall,wdays,pattern,stringsAsFactors = FALSE)

  uqmonths<-unique(months(alldays))
  uqwdays<-rep(unique(weekdays(alldays)),length=length(uqmonths))

  g_pattern=NULL
  for(i in 1:length(uqmonths)){
    for(j in 1:length(uqwdays)){
      if(i==j){
        ext1<-mat1[mat1[,"monthall"] == uqmonths[i],]
        ext2<-ext1[ext1[,"wdays"] == uqwdays[j],][1,]
        if(verbose){print(i);print(ext2[,"pattern"])}
        g_pattern<-c(g_pattern,ext2[,"pattern"])
        break;
      }
    }
  }
  return(data.frame(column1=seq(1:length(uqmonths)),column2=g_pattern,stringsAsFactors=FALSE))
}

## Function Call
funGeneratePattern(startdate="2017-01-01",enddate="2017-07-31",verbose=FALSE)

