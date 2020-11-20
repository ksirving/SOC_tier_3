## chub hydraulic map
library(tidyverse)
library(tidyr)
## need hydrualics by node - remove slices and take overall

## upload data
setwd("/Users/katieirving/Documents/git/SOC_tier_3/Overall_Results")
HData <- read.csv ("Overall_class_with_each_hydraulic.csv")

### if one position is high then node is high
## subset by species, then time period, then node
hp_dfx <-HData %>%
  mutate(ClassOrder = Overall_Class) %>%
  mutate(ClassOrder = replace(ClassOrder, Overall_Class == "Low", 1)) %>%
  mutate(ClassOrder = replace(ClassOrder, Overall_Class == "Partial", 2)) %>%
  mutate(ClassOrder = replace(ClassOrder, Overall_Class == "High", 3))

node <- unique(hp_dfx$Node)
species <- unique(hp_dfx$sp_code)
hydraulic <- unique(hp_dfx$Hydraulic)
hydraulic
node
species

node_dfx <- NULL


for(s in 1: length(species)) {
  
  sp_df <- hp_dfx %>%
    filter(sp_code == species[s])
  
  hydraulic <- sp_df$Hydraulic
  
  cat(paste("Species", species[s]))
  
  for(h in 1:length(hydraulic)) {
    
    hd_df <- sp_df %>%
      filter(Hydraulic == hydraulic[h])

    nd <- unique(hd_df$Node)

    
    for(p in 1: length(nd)) {

      node_df <- hd_df %>%
        filter(Node == nd[p])
      # cat(paste("Node", nd[p]))
      
      if(length(unique(node_df$Overall_Class)) == 1) {
        node_df$Node_Class <- node_df$Overall_Class
      } else if (length(unique(node_df$Overall_Class)) > 1){
        ## if more than 1 suitbaility class then take the highest one
        MaxClass <- filter(node_df, ClassOrder == max(node_df$ClassOrder))
        node_df$Node_Class <- MaxClass$Overall_Class[1]

      }
      
      node_dfx <- rbind(node_dfx, node_df)
   }
  
  }
}

node_dfx <- node_dfx %>%
  select(Species, Life_Stage, Hydraulic, Node, Node_Class) %>%
  distinct()

write.csv(node_dfx, "Overall_Class_per_node_hydraulic.csv")
### high probs 

## upload data
setwd("/Users/katieirving/Documents/git/SOC_tier_3/Overall_Results")
HData <- read.csv ("Overall_class_with_each_hydraulic_high_probs_50.csv")

### if one position is high then node is high
## subset by species, then time period, then node
hp_dfx <-HData %>%
mutate(ClassOrder = Overall_Class) %>%
           mutate(ClassOrder = replace(ClassOrder, Overall_Class == "Low", 1)) %>%
           mutate(ClassOrder = replace(ClassOrder, Overall_Class == "Partial", 2)) %>%
           mutate(ClassOrder = replace(ClassOrder, Overall_Class == "High", 3))

node <- unique(hp_dfx$Node)
species <- unique(hp_dfx$sp_code)
hydraulic <- unique(hp_dfx$Hydraulic)
hydraulic
node
species

node_dfx <- NULL
s = 1
p = 1
h=1

for(s in 1: length(species)) {
  
  sp_df <- hp_dfx %>%
    filter(sp_code == species[s])
  
  hydraulic <- sp_df$Hydraulic
  
  cat(paste("Species", species[s]))
  
  for(h in 1:length(hydraulic)) {
    
    hd_df <- sp_df %>%
      filter(Hydraulic == hydraulic[h])
    
    nd <- unique(hd_df$Node)
    
    
    for(p in 1: length(nd)) {
      
      node_df <- hd_df %>%
        filter(Node == nd[p])
      # cat(paste("Node", nd[p]))
      
      if(length(unique(node_df$Overall_Class)) == 1) {
        node_df$Node_Class <- node_df$Overall_Class
      } else if (length(unique(node_df$Overall_Class)) > 1){
        ## if more than 1 suitbaility class then take the highest one
        MaxClass <- filter(node_df, ClassOrder ==max(node_df$ClassOrder))
        node_df$Node_Class <- MaxClass$Overall_Class[1]
      }
      
      node_dfx <- rbind(node_dfx, node_df)
    }
    
  }
}
head(node_dfx)

node_dfx <- node_dfx %>%
  select(Species, Life_Stage, Hydraulic, Node, Node_Class) %>%
  distinct()

write.csv(node_dfx,  "Overall_Class_per_node_hydraulic_high_probs_50.csv")
