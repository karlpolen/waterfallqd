waterfall=function(dmat,ret,capital=100,invcost=100) {
  am=dmat$am
  pref=dmat$pref
  catchup=dmat$catchup
  carry=dmat$carry
  if(any(1<c(catchup,carry))) stop("catchup and carry must be stated as decimals<1")
  pref=c(pref,100000)
  am=c(am,0)
  stack=vector()
  lpcut=vector()
  typ=vector()
  nlayer=nrow(dmat)
  if(am[1]>0) {
    stack=c(stack,am[1])
    lpcut=c(lpcut,0)
    typ=c(typ,paste("Asset mgmt",0))
  }
  if (capital>0) {
    stack=c(stack,capital)
    lpcut=c(lpcut,1)
    typ=c(typ,paste("Return of Capital"))
  }
  if(pref[1]>0) {
    stack=c(stack,pref[1])
    lpcut=c(lpcut,1)
    typ=c(typ,paste("Preferred Return",1))
  }
  for (j in 1:nlayer) {
    if(am[j+1]>0) {
      stack=c(stack,am[j+1])
      lpcut=c(lpcut,0)
      typ=c(typ,paste("Asset Mgmt",j))
    }
    nextpref=pref[j+1]
    lpsofar=sum(stack*lpcut)-capital
    lpshort=nextpref-lpsofar
    cu=catchup[j]
    cy=carry[j]
    catchuplayer=0
    if(cu>cy) {
      catchuplayer=(lpsofar*cy)/(cu-cy)
      if (cu<1) catchuplayer=min(catchuplayer,lpshort/(1-cu))
      stack=c(stack,catchuplayer)
      lpcut=c(lpcut,(1-cu))    
      typ=c(typ,paste("Catchup",j))
    }
    lpsofar=sum(stack*lpcut)-capital
    lpshort=nextpref-lpsofar
    carrylayer=lpshort/(1-cy)
    if(carrylayer>0) {
      stack=c(stack,carrylayer)
      lpcut=c(lpcut,(1-cy))
      typ=c(typ,paste("Carry",j))
    }
  }
  ansmat=matrix(0,nrow=length(stack),ncol=length(ret))
  for (i in 1:length(ret)) {
    ansmat[,i]=wf(stack,ret[i])[-(1+length(stack))]
  }
  ans=list()
  ans$lpshare=matrix(lpcut,nrow=length(stack),ncol=length(ret))*ansmat
  rownames(ans$lpshare)=typ
  ans$gpshare=ansmat-ans$lpshare
  rownames(ans$gpshare)=typ
  ans$grossreturn=100*(ret-invcost)/invcost
  ans$netreturn=100*(colSums(ans$lpshare)-capital)/capital
  ans$stack=stack
  ans$lpcut=lpcut
  return(ans)
}
#given waterfall w in dollars and available cash c, distribute the cash to the waterfall
wf=function(w,c){
  x=c-cumsum(w)
  x[x>0]=0
  x=x+w
  x[x<0]=0
  c(x,c-sum(x))
}
testans=function(ans) {
  (colSums(ans$lpshare))+(colSums(ans$gpshare))
}
