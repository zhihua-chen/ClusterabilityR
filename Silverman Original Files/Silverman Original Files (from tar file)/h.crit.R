h.crit <-
function(x,k,prec=6){

  #temp function
  nor.kernel <- function(x,h){
    density(x,bw=h,kernel ="gaussian")$y
  }


	digits=prec
  prec=10^(-prec)
  x <- sort(x)
	minh <- min(diff(x))		#minimal possible h
	maxh <- diff(range(x))/2	#maximal possible h
	a <- maxh
	b <- minh
	zaehler=0

while (abs(b-a)>prec){
	m <- nr.modes(nor.kernel(x,a))
	
      b <- a
      if (m > k){
          minh <- a
          a <- (a + maxh)/2
      } 
      else {
          maxh <- a
          a <- (a - minh)/2
      }
}

a=round(a,digits)


if(nr.modes( nor.kernel(x,a) ) <= k){
 #subtract until more than k modes
 while(nr.modes( nor.kernel(x,a) ) <= k){
  a = a - prec
 }
 a=a+prec
}

if(nr.modes( nor.kernel(x,a) ) > k){
 #add until nr. of moodes correct
 while(nr.modes( nor.kernel(x,a) ) > k){
  a = a + prec
 }
}

a
}

