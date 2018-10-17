neutral <- "skyblue"

foregrad <- function(x){
  if (x == 1) return("olivedrab")
  colorRampPalette(c("orange","olivedrab",  "darkgreen"))(x)
}

backgrad <- function(x){
  if (x == 1) return("indianred3")
  colorRampPalette(c("darksalmon","indianred3", "purple4"))(x)
}
