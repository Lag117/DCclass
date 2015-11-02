#This code takes the house elves data set and splits them into two
#groups, large and small, based on ear size and adds this column to the 
#dataset. It then calculates GC content of the provided DNA sequences
#and adds this percentage as a column to the dataset. It then writes
#a new csv file with the ID#, ear size class, and GC content columns.

library(dplyr)

#import data
#from web: data <- read.csv("http://www.datacarpentry.org/semester-biology/data/houseelf_earlength_dna_data.csv")
#from data subdirectory for problem 3
data <- read.csv("~/myclass/data/houseelf_earlength_dna_data_1.csv")

#function that splits things into two size classes, large and small,
#based on ear length and a threshold that the user can set
get_size_class <- function(earlength, threshold){
  if (earlength > threshold){
    size_class = "large"
  }
  else {
    size_class = "small"
  }
  return(size_class)
}

#function that uses the get_size_class function to separate
#all data into small and large size classes and adds this column
#to the existing data
add_size_classes <- function(df){
  #add size class data to a dataframe. input dataframe with weight column containing size info
  data_size_class <-
    df %>%
    na.omit() %>%
    rowwise() %>%
    mutate(size_class = get_size_class(earlength, 10))
  return(data_size_class)
}

#function that calculates GC content
GCcontent <- function(sequence){
  #added str_to_upper so it can take sequences with any intial case
  sequence <- str_to_upper(sequence)
  Gs <- str_count(sequence, 'G')
  Cs <- str_count(sequence, 'C')
  gc_content <- (Gs + Cs) / str_length(sequence) * 100
  return(gc_content)
}

#function that uses the GCcontent function to calculate GC content
#for each DNA sequence and adds a column with all percentages 
#to the existing dataset
add_GC <- function(df){
  gccontentadd <-
    df %>%
    na.omit() %>%
    rowwise() %>%
    mutate(gc_content = GCcontent(dnaseq))
  return(gccontentadd)
}

get_size_class <- function(seq){
   #splits ear lengths into size categories
   ear_lengths <- ifelse(seq > 10, "large", "small")
   return(ear_lengths)
}

#takes all previous functions and adds appropriate columns
#then writes a csv with the columns: id, size class, and gc content
data_sizes <- add_size_classes(data)
data_sizes_and_gc <- add_GC(data_sizes)

#writes csv with id, ear length class, & gc content for each individual
write.csv(data_sizes_and_gc[,c("id","size_class","gc_content")], file="id_earsize_and_gc.csv")
