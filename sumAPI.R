library(plumber)

#* @apiTitle Simple Addition API
#* @apiDescription This API adds two numbers.

#* Add two numbers using GET request
#* @post /plus
#* @param x:number First number
#* @param y:number Second number
function(x, y) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  return(list(result = x + y))
}

#* Add two numbers using GET request
#* @post /minus
#* @param x:number First number
#* @param y:number Second number
function(x, y) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  return(list(result = x - y))
}
