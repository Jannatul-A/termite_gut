library(tidyverse)
library(xlsx)
library(pheatmap)
setwd("/Users/jannatulashpia/CSB-BIOL425/final_projects/group_4_termite_microbiome")

table_two <- read.xlsx("Mikaelyan_sup_data.xlsx", sheetIndex = 2)
subset_data <- table_two[6:nrow(table_two),] #start from row6
colnames(subset_data) <- c("Phylum",
                           "Class",
                           "Order",
                           "Family",
                           "Genus",
                           "Macrotermes sp.",
                           "Macrotermes subhyalinus",
                           "Odontotermes sp",
                           "Alyscotermes trestus",
                           "Cornitermes sp",
                           "Microcerotermes parvus",
                           "Microcerotermes sp",
                           "Neocapritermes taracua",
                           "Cubitermes ugandensis",
                           "Ophiotermes sp",
                           "Termes hospes",
                           "Termes fatalis",
                           "Promirotermes sp.",
                           "Atlantitermes sp.",
                           "Velocitermes sp.",
                           "Trinervitermes sp.",
                           "Nasutitermes corniger",
                           "Nasutitermes takasagoensis")

subset_data <- subset_data[-1,] #drop header repeat
subset_data <- subset_data[, -c(2, 3, 4, 5)]
df <- subset_data[complete.cases(subset_data$Phylum), ] #get rid of NA

######################################################################
#WITHOUT PHEATMAP
pivot_table <- df %>% 
  select(1:19) %>% 
  pivot_longer(2:19,  names_to = "species", values_to = "abundance")

new_df <- pivot_table %>% 
  filter(Phylum %in% c("Actinobacteria ", "Bacteroidetes ", "Fibrobacteres ", "Firmicutes ", "Spirochaetes ", "Proteobacteria ", "Candidate phylum TG3 "))

new_df$species[new_df$species == 'Macrotermes sp.'] <- 'Mac_1_F'
new_df$species[new_df$species == 'Macrotermes subhyalinus'] <- 'Mac_2_F'
new_df$species[new_df$species == 'Odontotermes sp'] <- 'Mac_3_F'
new_df$species[new_df$species == 'Alyscotermes trestus'] <- 'Apic_4_SO'
new_df$species[new_df$species == 'Cornitermes sp'] <- 'Syn_5_L'

new_df$species[new_df$species == 'Microcerotermes parvus'] <- 'Term_6_W'
new_df$species[new_df$species == 'Microcerotermes sp'] <- 'Term_7_W'
new_df$species[new_df$species == 'Neocapritermes taracua'] <- 'Term_8_H'
new_df$species[new_df$species == 'Cubitermes ugandensis'] <- 'Term_9_SO'
new_df$species[new_df$species == 'Ophiotermes sp'] <- 'Term_10_SO'

new_df$species[new_df$species == 'Termes hospes'] <- 'Term_11_H'
new_df$species[new_df$species == 'Termes fatalis'] <- 'Term_12_H'
new_df$species[new_df$species == 'Promirotermes sp.'] <- 'Term_13_H'
new_df$species[new_df$species == 'Atlantitermes sp.'] <- 'Nasu_14_H'
new_df$species[new_df$species == 'Velocitermes sp.'] <- 'Nasu_15_L'

new_df$species[new_df$species == 'Trinervitermes sp.'] <- 'Nasu_16_W'
new_df$species[new_df$species == 'Nasutitermes corniger'] <- 'Nasu_17_W'
new_df$species[new_df$species == 'Nasutitermes takasagoensis'] <- 'Nasu_18_W'




breaks = c(0, 10, 20, 30, 40)
new_df$abund <- round(as.numeric(new_df$abundance), 2)
ggplot(new_df, aes(x = Phylum, y = species, fill = abund)) +
  geom_tile() +
  labs(title = "Abundance Heatmap (%)", x = "Phylum", y = "Species") +
  geom_text(aes(label = abund)) +
  scale_fill_gradient(low = "white", high = "#0D0628", breaks = breaks) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#############################################################################
#USING Pheatmap
dataset_pheatmap <- df %>% 
  select(1:19) %>% 
  filter(Phylum %in% c("Actinobacteria ", 
                       "Bacteroidetes ", 
                       "Fibrobacteres ", 
                       "Firmicutes ", 
                       "Spirochaetes ", 
                       "Proteobacteria ", 
                       "Candidate phylum TG3 "))
dataset_pheatmap <- dataset_pheatmap[,-1] #remove first col
data2<- data.frame(lapply(dataset_pheatmap, function(x) round(as.numeric(x), 2)))
#convert str to numerics

data2<- as.matrix(data2) #have it matrix format
rownames(data2) <- c("Actinobacteria ", 
                     "Bacteroidetes ", 
                     "Candidate phylum TG3 ", 
                     "Fibrobacteres ", 
                     "Firmicutes ", 
                     "Proteobacteria ", 
                     "Spirochaetes ")

annotation_col = data.frame( Feeding = factor(c("F", "F", "F", "S", "L","W", "W", "H", "S",
                                                "S", "H", "H", "H", "H", "L", "W", "W", "W")), 
                             Subfamily = c("Macrotermes_sp.", "Macrotermes_sp.", "Macrotermes_sp.", "Apicotermitinae", 
                                           "Syntermitinae", "Termitinae", "Termitinae", "Termitinae", "Termitinae", "Termitinae",
                                           "Termitinae", "Termitinae", "Termitinae", "Nasutitermitinae", "Nasutitermitinae",
                                           "Nasutitermitinae", "Nasutitermitinae", "Nasutitermitinae"))
sub_fam <- c("Macrotermes sp.", "Apicotermitinae", "Syntermitinae", "Termitinae", "Nasutitermitinae") #groups
rownames(annotation_col)<- colnames(data2)

ann_colors = list(
  Feeding = c(F = "#c2c2c2", S = "#846908", L = "#decd87", W = "#e5ffd5", H = "#4f4f4f"),
  Subfamily = c(Macrotermes_sp. = "#d095de", Apicotermitinae = "#69ad85", Syntermitinae = "#748bae", Termitinae = "#fcca02", Nasutitermitinae = "#feaaaa"))

pheatmap(data2, annotation_col = annotation_col, 
         treeheight_col = 0, treeheight_row = 0,
         display_numbers = TRUE,
         color = colorRampPalette(c("white", "#d00000"))(50), #firebrick3
         annotation_colors = ann_colors,
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         main = "Relative Abundance (%)")




