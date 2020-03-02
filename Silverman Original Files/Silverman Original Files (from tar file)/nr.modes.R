nr.modes <-
function(y){

  d1 <- diff(y)
	signs <- diff(d1/abs(d1))
	length(signs[signs==-2])

}

