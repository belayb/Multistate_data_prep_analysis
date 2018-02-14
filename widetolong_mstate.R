widetolong_mstate <- function(data1, Card.number, Month.on.ART, Endtime, Current.ARV.regimen) {
  attach(data1)
  pos.ch <- c("1a","1b","1c","1d","1e","1f","2a","2b","2c","2d","2e")
  uni<-unique(Card.number)
  frq <- table(Card.number)
  rep.card.no <- rep(uni,times=frq*10)
  length(rep.card.no)
  rep.start <- rep(Month.on.ART,each=10) 
  
  endind1 <-(1:length(Endtime))[Month.on.ART!=0]
  endtime1 <- Endtime
  endtime1[endind1-1] <- Month.on.ART[endind1]
  mix.start.end <- endtime1
  
  rep.for.end <- rep(mix.start.end,each=10)
  
  rep.for.from <- rep( Current.ARV.regimen, each=10)
  
  exclude.current <- function(x=Current.ARV.regimen,y=pos.ch){
    y[!(x==y)]
  }
  
  for.to <- typeof(as.vector((sapply(Current.ARV.regimen,exclude.current,simplify=TRUE))))
  for.to <- as.vector((sapply(Current.ARV.regimen,exclude.current,simplify=TRUE)))
  
  rm.app <- function(x){
    no <- c(rep("no",10))
    ap <- append(x,no)
    ap[-(1:10)]
  }
  list.rff <- tapply(as.character(rep.for.from),rep.card.no,c)
  
  for.sta <-sapply(list.rff,rm.app,simplify=TRUE)
  
  for.sta.vec <- for.sta[[1]]
  for(i in 2:length(for.sta)){
    for.sta.vec <- append(for.sta.vec,for.sta[[i]])
  }
  
  status <- as.numeric(for.sta.vec==as.character( for.to))
  
  df.belaya<-data.frame(card.number=rep.card.no,Month.on.ART=rep.start,
                        Endtime=rep.for.end,From.regimen=rep.for.from,To.regimen=for.to,Status=status)
  return(df.belaya)
}