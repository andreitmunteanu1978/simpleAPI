library(plumber)

#* @apiTitle Sample Testing API
#* @apiDescription A free data summation API

#* @post /sum
#* @param a numeric
#* @param b numeric
function(a, b) {
  list(result = as.numeric(a)*10 + as.numeric(b))
}
