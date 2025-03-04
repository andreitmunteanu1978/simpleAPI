library(plumber)

#* @apiTitle Simple API
#* @apiDescription A free R Plumber API

#* @get /hello
function() {
  list(message = "Hello, world!")
}

#* @post /sum
#* @param a numeric
#* @param b numeric
function(a, b) {
  list(result = as.numeric(a) + as.numeric(b))
}