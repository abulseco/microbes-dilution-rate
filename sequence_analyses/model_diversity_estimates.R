# Diversity estimates from modeled data
# Bulseco et al. - Microbial growth traits

# calculate diversity using this script
# plot in "Figure4_bray_shannon.R" 

# SETUP----
library(vegan)

# Import modeled data----
## model 1 (representing chemostat "A")
uniform_modelA <- read.csv("model_data/uniformASVs-betaA.csv", header = T)
uniform_modelA_rel <- read.csv('model_data/uniformASVs-beta-RelativeA.csv', header = T)
beta_modelA <- read.csv("model_data/betaASVs-betaA.csv", header = T)
beta_modelA_rel <- read.csv("model_data/betaASVs-beta-RelativeA.csv", header = T)

## model 2 (representing chemostat "B")
uniform_modelB <- read.csv("model_data/uniformASVsB.csv", header = T)
uniform_modelB_rel <- read.csv("model_data/uniformASVsRelativeB.csv", header = T)
beta_modelB <- read.csv("model_data/betaASVsB.csv", header = T)
beta_modelB_rel <- read.csv("model_data/betaASVsRelativeB.csv", header = T)

# General workflow----
# There are two model versions, A & B
# For each A and B, they are run using a (1) uniform and (2) beta distribution
# Need to calculate alpha diversity and beta diversity for each model
# Then plot for panels D, E in Fig 4

# Alpha diversity----
## Model A----
### Uniform distribution model----
uniform_modelA <- uniform_modelA[,2:13] # 12 columns of data
# maybe double check why this one only has 12 columns of data 
# while the rest have 14

uniformdf_A <- data.matrix(uniform_modelA) %>%
  t() %>%
  as.data.frame()

uniformH_A <- diversity(uniformdf_A, index = "shannon") # calculate shannon with vegan 
uniformH_A
write.table(uniformH_A, "intermediate_files/uniformH_A.txt") # export to intermediate files folder

### Beta distribution model----
beta_modelA <- beta_modelA[,2:15] # 15 columns of data 

beta_modeldf_A <- data.matrix(beta_modelA) %>%
  t() %>%
  as.data.frame()

betaH_A <- diversity(beta_modeldf_A, index = "shannon") # calculate shannon with vegan 
betaH_A
write.table(betaH_A, "intermediate_files/betaH_A.txt") # export to intermediate files folder

## Model B----
### Uniform distribution model----
uniform_modelB <- uniform_modelB[,2:15] # 14 columns of data

uniformdf_B <- data.matrix(uniform_modelB) %>%
  t() %>%
  as.data.frame()

uniformH_B <- diversity(uniformdf_B, index = "shannon") # calculate shannon with vegan 
uniformH_B
write.table(uniformH_B, "intermediate_files/uniformH_B.txt") # export to intermediate files folder

### Beta distribution model----
beta_modelB <- beta_modelB[,2:15] # 15 columns of data 

beta_modeldf_B <- data.matrix(beta_modelB) %>%
  t() %>%
  as.data.frame()

betaH_B <- diversity(beta_modeldf_B, index = "shannon") # calculate shannon with vegan 
betaH_B
write.table(betaH_B, "intermediate_files/betaH_B.txt") # export to intermediate files folder

# Beta diversity----
## Model A----

# already have our relevant dataframes from above: 
## uniformdf_A
## beta_modeldf_A

# OOPS. Probably needed to use the relative abundance for this 

# TEST============
# Probably needed to use relative abundances for this, let's see how much it changes things
# Redo for uniform A
uniform_modelA_rel <- uniform_modelA_rel[,2:15] # Not sure why the rel abundance has 14 columns?

uniformdf_A_REL <- data.matrix(uniform_modelA_rel) %>%
  t() %>%
  as.data.frame()

uniformBray_A_REL <- vegdist(uniformdf_A_REL, method = "bray")
uniformBray_A_REL
uniformBray_A_matrix_REL <- as.matrix(uniformBray_A_REL)
write.csv(uniformBray_A_matrix_REL, "intermediate_files/uniformBray_A_matrix_REL.csv")


# beta_model_relA <- beta_modelA_rel[,2:15] # 15 columns of data 
# 
# beta_model_relA_df <- data.matrix(beta_model_relA) %>%
#   t() %>%
#   as.data.frame()

### Uniform distribution model----
uniformBray_A <- vegdist(uniformdf_A, method = "bray")
uniformBray_A
uniformBray_A_matrix <- as.matrix(uniformBray_A)
write.csv(uniformBray_A_matrix, "intermediate_files/uniformBray_A_matrix.csv")

### Beta distribution model----
betaBray_A <- vegdist(beta_modeldf_A, method = "bray")
betaBray_A
betaBray_A_matrix <- as.matrix(betaBray_A)
write.csv(betaBray_A_matrix, "intermediate_files/betaBray_A_matrix.csv")

## Model B----

# already have our relevant dataframes from above:
  ## uniformdf_B
  ## beta_modeldf_B

### Uniform distribution model----
uniformBray_B <- vegdist(uniformdf_B, method = "bray")
uniformBray_B
uniformBray_B_matrix <- as.matrix(uniformBray_B)
write.csv(uniformBray_B_matrix, "intermediate_files/uniformBray_B_matrix.csv")

### Beta distribution model----
betaBray_B <- vegdist(beta_modeldf_B, method = "bray")
betaBray_B
betaBray_B_matrix <- as.matrix(betaBray_B)
write.csv(betaBray_B_matrix, "intermediate_files/betaBray_B_matrix.csv")



