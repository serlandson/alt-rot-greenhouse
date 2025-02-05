### Taxa Labels
# takes a taxa table -  taxa as rows, taxonomy as columns.
# needs to have columns for phylum, class, order, family, genus, species.
# function searches through each column to find the lowest taxonomic label for each taxa, 
#   then returns a vector of taxa labels.
# starts at genus + species and searches up to phylum.
library(stringr)
# get labels
taxaLabels <- function(x){
  final_taxa <- x
  final_taxa$phylum <- as.character(final_taxa$phylum)
  final_taxa$class <- as.character(final_taxa$class)
  final_taxa$order <- as.character(final_taxa$order)
  final_taxa$family <- as.character(final_taxa$family)
  final_taxa$genus <- as.character(final_taxa$genus)
  final_taxa$species <- as.character(final_taxa$species)
  
  final_taxa$phylum[is.na(final_taxa$phylum)] <- "unidentified"
  final_taxa$class[is.na(final_taxa$class)] <- "unidentified"
  final_taxa$order[is.na(final_taxa$order)] <- "unidentified"
  final_taxa$family[is.na(final_taxa$family)] <- "unidentified"
  final_taxa$genus[is.na(final_taxa$genus)] <- "unidentified"
  final_taxa$species[is.na(final_taxa$species)] <- "unidentified"
  
  taxa_labels <- final_taxa$species
  
  for (i in 1:length(taxa_labels)){
    if(final_taxa$species[i] != "unidentified"){
      taxa_labels[i] <- final_taxa$species[i]
    } else if(final_taxa$genus[i] != "unidentified"){
      taxa_labels[i] <- paste0(final_taxa$genus[i], "_", "sp")
    } else if(final_taxa$family[i] != "unidentified"){
      taxa_labels[i] <- paste0(final_taxa$family[i], "_", "sp")
    } else if(final_taxa$order[i] != "unidentified"){
      taxa_labels[i] <- paste0(final_taxa$order[i], "_", "sp")
    } else if(final_taxa$class[i] != "unidentified"){
      taxa_labels[i] <- paste0(final_taxa$class[i], "_", "sp")
    } else if(final_taxa$phylum[i] != "unidentified"){
      taxa_labels[i] <- paste0(final_taxa$phylum[i], "_", "sp")
    } else{
      taxa_labels[i] <- paste0(final_taxa$kingdom[i], "_", "sp")
    }
  }
  taxa_labels
}
