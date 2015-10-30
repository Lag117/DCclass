library(stringr)
GCcontent <- function(sequence){
  sequence <- str_to_upper(sequence)
  Gs <- str_count(sequence, 'G')
  Cs <- str_count(sequence, 'C')
  gc_content <- (Gs + Cs) / str_length(sequence) * 100
  return(gc_content)
}
#test
GCcontent("agtgactcg")


