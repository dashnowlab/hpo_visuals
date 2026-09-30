## HPO Frequency Plot Drafting for Final Quarto File
library(jsonlite)
library(tidyverse)
library(ontologyIndex)

# Read data from url into "loci" object
url <- "https://raw.githubusercontent.com/dashnowlab/STRchive/main/data/STRchive-loci.json"
loci <- fromJSON(url)

# Consolidate and unnest hpo_terms
hpo_terms <- loci |> select(c(disease_id, hpo_terms))
unnested_hpo_terms <- hpo_terms |> unnest_longer(hpo_terms) |> relocate(hpo_terms)

# Create `hpo_freq` data frame to cross reference with `hpo$name[hpo$children[["HP:000____"]]]` function from `ontologyIndex`
hpo_freq <- unnested_hpo_terms |> count(hpo_terms, sort = T)

# Generalize most frequent HPO terms (n >= 13) (Based on "children" using `ontologyIndex` package with `data(hpo)` as seen above)
hpo_frequency <- unnested_hpo_terms |> mutate(
  hpo_terms = fct_collapse(hpo_terms,
  "HP:0001251 Ataxia" = c("HP:0001251 Ataxia", "HP:0001310 Dysmetria", "HP:0002066 Gait ataxia", "HP:0002070 Limb ataxia", "HP:0002073 Progressive cerebellar ataxia", "HP:0002075 Dysdiadochokinesis", "HP:0002078 Truncal ataxia", "HP:0010867 Dyssynergia", "HP:0002070 Limb Ataxia"),
  "HP:0001288 Gait disturbance" = c("HP:0001288 Gait disturbance", "HP:0002136 Broad-based gait", "HP:0002141 Gait imbalance", "HP:0002317 Unsteady gait", "HP:0002362 Shuffling gait", "HP:0002515 Waddling gait", "HP:0002527 Falls", "HP:0002540 Inability to walk", "HP:0003376 Steppage gait", "HP:0030051 Tip-toe gait", "HP:0031629 Impaired tandem gait"),
  "HP:0001260 Dysarthria" = c("HP:0001260 Dysarthria", "HP:0002464 Spastic dysarthria", "HP:0007024 Pseudobulbar paralysis", "HP:0008376 Nasal dysarthria"),
  "HP:0002015 Dysphagia" = c("HP:0002015 Dysphagia", "HP:0007024 Pseudobulbar paralysis", "HP:0031162 Impaired oropharyngeal swallow response", "HP:0200136 Oral-pharyngeal dysphagia"),
  "HP:0000639 Nystagmus" = c("HP:0000639 Nystagmus", "HP:0000640 Gaze-evoked nystagmus", "HP:0000666 Horizontal nystagmus", "HP:0010542 Vestibular nystagmus", "HP:0010544 Vertical nystagmus"),
  "HP:0001272 Cerebellar atrophy" = c("HP:0001272 Cerebellar atrophy", "HP:0006855 Cerebellar vermis atrophy", "HP:0007263 Spinocerebellar atrophy", "HP:0012082 Cerebellar Purkinje layer atrophy", "HP:0100275 Diffuse cerebellar atrophy"),
  "HP:0001249 Intellectual disability" = c("HP:0001249 Intellectual disability", "HP:0001256 Mild intellectual disability", "HP:0002342 Moderate intellectual disability", "HP:0006889 Borderline intellectual disability", "HP:0010864 Severe intellectual disability"),
  "HP:0001337 Tremor" = c("HP:0001337 Tremor", "HP:0002322 Resting tremor", "HP:0002345 Action tremor", "HP:0030188 Tremor by anatomical site"),
  "HP:0001250 Seizure" = c("HP:0001250 Seizure", "HP:0002069 Bilateral tonic-clonic seizure", "HP:0002133 Status epilepticus"),
  "HP:0001347 Hyperreflexia" = c("HP:0001347 Hyperreflexia", "HP:0001348 Brisk reflexes", "HP:0002169 Clonus", "HP:0006801 Hyperactive deep tendon reflexes"),
  "HP:0000726 Dementia" = c("HP:0000726 Dementia", "HP:0000727 Frontal lobe dementia", "HP:0002145 Frontotemporal dementia", "HP:0007123 Subcortical dementia"),
  "HP:0001265 Hyporeflexia" = ("HP:0001265 Hyporeflexia"),
  "HP:0002067 Bradykinesia" = ("HP:0002067 Bradykinesia"),
  "HP:0003487 Babinski sign" = ("HP:0003487 Babinski sign")
  )) |> count(hpo_terms, sort = T) |> filter (n >= 13)

# Visualize hpo_terms with a frequency of 13+ across diseases with terms generalized
ggplot(hpo_frequency, aes(x = n, y = fct_infreq(hpo_terms, (n)) |> fct_rev())) + geom_col(fill = "lightblue") + labs(title = "Most Frequent HPOs", subtitle = "Phenotypes with a frequency of 13+ across STR diseases", x = "Frequency", y = "HPO Term")

# Trying this with ontologyX functions --> Did not work :/
  # First extract just HP ID numbers to simplify process
  unnested_hpo_terms <- unnested_hpo_terms |>
    mutate(hpo_id = str_extract(hpo_terms, "^HP:\\d+"))
  # Then see if works with `get_descendants()`
  hpo_frequency <- unnested_hpo_terms |> mutate(
    hpo_terms = fct_collapse(hpo_terms,
     "HP:0001251 Ataxia" = fct_collapse(hpo_id, get_descendants(hpo, "HP:0001251")),
     "HP:0001288 Gait disturbance" = fct_collapse(hpo_id, get_descendants(hpo, "HP:0001288")),
     "HP:0001260 Dysarthria" = fct_collapse(hpo_id, get_descendants(hpo, "HP:0001260"))
    )) |> count(hpo_terms, sort = T) |> filter (n >= 13)
  # Failed :/




# Function to fetch parent for a single HPO term
  # May result in multiple parents
data(hpo)
get_parent <- function(x) {
  id <- str_extract(x, "^HP:\\d+")
  return(hpo$name[hpo$parent[[id]]])
}
  # Add hpo_category (parent) to `unnested_hpo_terms`
  unnested_hpo_terms |> mutate(hpo_category = map(hpo_terms, get_parent)) -> unnested_hpo_terms

  # from harriet's memory probably wrong syntax, alternate expression of above code
  saved_vals <- c()
  for(x in hpo_terms) {
  saved_vals <- cbind(saved_valed, get_parent(x))
  }



# Trying to create loop to automate HPO term generalization (Made with Gabriel thank you Gabriel)
# First extract just HP ID numbers to simplify process
unnested_hpo_terms <- unnested_hpo_terms |>
  mutate(hpo_id = str_extract(hpo_terms, "^HP:\\d+"))
# Create parent_ids object to loop through
parent_ids <- c("HP:0001260", # Dysarthria
                "HP:0001251", # Ataxia
                "HP:0002015", # Dysphagia
                "HP:0001272", # Cerebellar atrophy
                "HP:0001288", # Gait disturbance
                "HP:0000639", # Nystagmus
                "HP:0001249", # Intellectual disability
                "HP:0001337", # Tremor
                "HP:0000726", # Dementia
                "HP:0001250", # Seizure
                "HP:0001265", # Hyporeflexia
                "HP:0001347", # Hyperreflexia
                "HP:0002067", # Bradykinesia
                "HP:0003487" # Babinski sign
)
# Build lookup table (child id -> parent label) by looping through descendants
recode_map <- map_dfr(parent_ids, function(parent_id) {
  parent_name <- hpo$name[[parent_id]]
  parent_label <- paste(parent_id, parent_name)
  descendant_ids <- get_descendants(hpo, roots = parent_id)
  tibble(hpo_id = descendant_ids, hpo_label = parent_label)
})
# Worked to automate finding children of HPO terms, but seemed to overpopulate some terms (i.e. seizure) 
# Can I automatically populate the hpo_frequency table with the generalized terms through this?




## Exploring `ontologySimilarity` similarity matrix
library(ontologyIndex)
library(ontologySimilarity)
data(hpo)
set.seed(1)
information_content <- descendants_IC(hpo)
# Generate terms
  # GroupA based on Ataxia descendants
  # GroupB based on Gait disturbance descendants
  # GroupC based on Seizure descendants
term_sets <- list(
  Ataxia = c("HP:0001251", "HP:0002066", "HP:0002070", "HP:0001310"),
  Gait_Disturbance = c("HP:0001288", "HP:0002066", "HP:0002141", "HP:0031952"),
  Seizure = c("HP:0001250", "HP:0011097", "HP:0001327")
)
# Trying alternate term generation to encapsulate all descendants of terms
term_sets <- list(
  Ataxia = get_descendants(hpo, "HP:0001251"),
  Gait_Disturbance = get_descendants(hpo, "HP:0001288"),
  Seizure = get_descendants(hpo, "HP:0001250")
)
term_sets
# Calculate similarity matrix
sim_mat <- get_sim_grid(ontology = hpo, term_sets = term_sets)
sim_mat
# Create cluster dendrogram
dist_mat <- max(sim_mat) - sim_mat
plot(hclust(as.dist(dist_mat)))

# Term set of individual hpo terms --> Heatmap first attempt
term_sets <- list(
  Ataxia = "HP:0001251",
  Gait_Ataxia = "HP:0002066",
  Gait_Disturbance = "HP:0001288",
  Seizure = "HP:0001250"
  )
term_sets
  # Similarity matrix
  sim_mat <- get_sim_grid(ontology = hpo, term_sets = term_sets)
  sim_mat
  # Cluster dendogram
  dist_mat <- max(sim_mat) - sim_mat
  plot(hclust(as.dist(dist_mat)))
    # Distance between Seizure and Ataxia vs. Gait disturbance changes based on if individual term or term with all descendants
    # Gait ataxia appears more closely related to ataxia than gait disturbance
  # Heatmap
  library(pheatmap)
  hclust_obj <- hclust(as.dist(dist_mat))
  pheatmap(sim_mat,
           cluster_rows = hclust_obj,
           cluster_cols = hclust_obj,
           show_rownames = TRUE,
           show_colnames = TRUE)
  

## Big HPO heatmap
  # Not familiar enough with function creation to do all unnested_hpo_terms, going to manually input hpo_freq terms
  library(ontologyIndex)
  library(ontologySimilarity)
  data(hpo)
  set.seed(50)
  information_content <- descendants_IC(hpo)
# First, similarity matrix of individual terms  
  # Generate terms
  term_sets <- list(
    Dysarthria = "HP:0001260",
    Ataxia = "HP:0001251",
    Dysphagia = "HP:0002015",
    Cerebellar_Atrophy = "HP:0001272",
    Gait_Disturbance = "HP:0001288",
    Nystagmus = "HP:0000639",
    Intellectual_Disability = "HP:0001249",
    Tremor = "HP:0001337",
    Dementia = "HP:0000726",
    Seizure = "HP:0001250",
    Hyporeflexia = "HP:0001265",
    Hyperreflexia = "HP:0001347",
    Gait_Ataxia = "HP:0002066",
    Limb_Ataxia = "HP:0002070",
    Bradykinesia = "HP:0002067",
    Babinski_Sign = "HP:0003487",
    Progressive_Cerebellar_Ataxia = "HP:0002073",
    Depression = "HP:0000716",
    Hypotonia = "HP:0001252",
    Global_Developmental_Delay = "HP:0001263",
    Dysmetria = "HP:0001310",
    Muscle_Weakness = "HP:0001324",
    Dystonia = "HP:0001332",
    Myoclonus = "HP:0001336",
    Ptosis = "HP:0000508",
    Rigidity = "HP:0002063",
    Distal_Muscle_Weakness = "HP:0002460",
    Skeletal_Muscle_Atrophy = "HP:0003202",
    Spasticity = "HP:0001257",
    Parkinsonism = "HP:0001300"
  )
  # Similarity matrix
  sim_mat <- get_sim_grid(ontology = hpo, term_sets = term_sets)
  sim_mat
  # Cluster dendrogram
  dist_mat <- max(sim_mat) - sim_mat
  plot(hclust(as.dist(dist_mat)))
  # Heat map
  hclust_obj <- hclust(as.dist(dist_mat))
  pheatmap(sim_mat,
           cluster_rows = hclust_obj,
           cluster_cols = hclust_obj,
           show_rownames = TRUE,
           show_colnames = TRUE,
           fontsize = 6.5
           )


  
## Disease Cluster Heat Map
# Tidy data --> "Nest" disease ids
library(jsonlite)
library(tidyverse)
url <- "https://raw.githubusercontent.com/dashnowlab/STRchive/main/data/STRchive-loci.json"
loci <- fromJSON(url)
disease_hpo <- loci |> select(c(disease_id, hpo_terms)) |> unnest_longer(hpo_terms) |> mutate(hpo_id = str_extract(hpo_terms, "^HP:\\d+")) |> select(disease_id, hpo_id) |> group_by(disease_id) |> nest(hpo_id = hpo_id) |> rowwise() |> mutate(hpo_id = paste(hpo_id, collapse = ', ')) |> ungroup()
# Similarity matrix
library(ontologyIndex)
library(ontologySimilarity)
data(hpo)
set.seed(50)
information_content <- descendants_IC(hpo) 
  # Generate terms (want to write loop/function to streamline but see successful painstaking method below)
  term_set <- list(
  disease_hpo$disease_id = disease_hpo$hpo_id
  )
  # Similarity matrix
  sim_mat <- get_sim_grid(ontology = hpo, term_sets = term_set)
  sim_mat
  
  
# Successful NOT STREAMLINED process
library(jsonlite)
library(tidyverse)
url <- "https://raw.githubusercontent.com/dashnowlab/STRchive/main/data/STRchive-loci.json"
loci <- fromJSON(url)
disease_hpo <- loci |> select(c(disease_id, hpo_terms)) |> unnest_longer(hpo_terms) |> mutate(hpo_id = str_extract(hpo_terms, "^HP:\\d+")) |> select(disease_id, hpo_id) |> group_by(disease_id) |> nest(hpo_id = hpo_id) |> rowwise() |> mutate(hpo_id = paste(hpo_id, collapse = ', ')) |> ungroup()
library(ontologyIndex)
library(ontologySimilarity)
data(hpo)
set.seed(50)
information_content <- descendants_IC(hpo)
library(pheatmap)
  # Generate terms (using str_view to copy HPO terms)
  term_set <- list(
  OPDM5 = str_view(disease_hpo$hpo_id[1]),
  FRAXE = str_view(disease_hpo$hpo_id[2]),
  SBMA = str_view(disease_hpo$hpo_id[3])
  )
  # Actual term generation --> Similarity matrix --> Heat map
  term_set <- list(
  OPDM5 = c("HP:0000183", "HP:0000218", "HP:0000301", "HP:0000408", "HP:0000590", "HP:0000597", "HP:0001284", "HP:0001288", "HP:0001604", "HP:0001824", "HP:0002058", "HP:0002091", "HP:0002100", "HP:0002505", "HP:0002705", "HP:0002747", "HP:0007149", "HP:0007838", "HP:0008376", "HP:0008756", "HP:0008944", "HP:0008959", "HP:0008963", "HP:0008997", "HP:0009027", "HP:0009053", "HP:0009063", "HP:0009073", "HP:0010550", "HP:0030192", "HP:0030319", "HP:0031162", "HP:0200136", "HP:0430015", "HP:3000005", "HP:3000010"),
  FRAXE = c("HP:0000252", "HP:0000256", "HP:0000286", "HP:0000426", "HP:0000713", "HP:0000718", "HP:0000722", "HP:0000729", "HP:0000750", "HP:0000752", "HP:0001249", "HP:0001328", "HP:0001511", "HP:0001609", "HP:0002311", "HP:0002312", "HP:0004209", "HP:0004322", "HP:0009904", "HP:0011341", "HP:0012172", "HP:0012471", "HP:0025116", "HP:0100023", "HP:0100710"),
  SBMA = c("HP:0000029", "HP:0000144", "HP:0000153", "HP:0000763", "HP:0000771", "HP:0001252", "HP:0001260", "HP:0001265", "HP:0001283", "HP:0001288", "HP:0001337", "HP:0001618", "HP:0002015", "HP:0002380", "HP:0003119", "HP:0003202", "HP:0003236", "HP:0003394", "HP:0003560", "HP:0003690", "HP:0005978", "HP:0008981", "HP:0009830", "HP:0100022", "HP:0100639"),
  EIEE1 = c("HP:0000054", "HP:0000252", "HP:0000568", "HP:0000817", "HP:0001249", "HP:0001263", "HP:0001266", "HP:0001276", "HP:0001285", "HP:0001332", "HP:0001347", "HP:0001357", "HP:0001510", "HP:0002015", "HP:0002094", "HP:0002119", "HP:0002123", "HP:0002188", "HP:0002283", "HP:0002421", "HP:0002521", "HP:0007256", "HP:0007359", "HP:0008936", "HP:0010851", "HP:0011153", "HP:0011344", "HP:0012469", "HP:0025357", "HP:0032792", "HP:0100660", "HP:0200134"),
  PRTS = c("HP:0000053", "HP:0000325", "HP:0000708", "HP:0000750", "HP:0001249", "HP:0001250", "HP:0001256", "HP:0001260", "HP:0001288", "HP:0001371", "HP:0002061", "HP:0002342", "HP:0002353", "HP:0002451", "HP:0004373", "HP:0007380", "HP:0012385", "HP:0012469"),
  DRPLA = c("HP:0000597", "HP:0000639", "HP:0000643", "HP:0000726", "HP:0001138", "HP:0001152", "HP:0001249", "HP:0001250", "HP:0001251", "HP:0001260", "HP:0001265", "HP:0001266", "HP:0001300", "HP:0001310", "HP:0001332", "HP:0001336", "HP:0002066", "HP:0002070", "HP:0002072", "HP:0002073", "HP:0002075", "HP:0002078", "HP:0002172", "HP:0002345", "HP:0002354", "HP:0004305", "HP:0007047", "HP:0010831", "HP:0010867", "HP:0012048", "HP:0030890", "HP:0100543"),
  SCA1 = c("HP:0000496", "HP:0000514", "HP:0000543", "HP:0000597", "HP:0000623", "HP:0000639", "HP:0000640", "HP:0000641", "HP:0000648", "HP:0001151", "HP:0001252", "HP:0001257", "HP:0001260", "HP:0001265", "HP:0001272", "HP:0001283", "HP:0001284", "HP:0001288", "HP:0001290", "HP:0001310", "HP:0001324", "HP:0001332", "HP:0001347", "HP:0001350", "HP:0002015", "HP:0002067", "HP:0002070", "HP:0002071", "HP:0002072", "HP:0002073", "HP:0002075", "HP:0002078", "HP:0002141", "HP:0002168", "HP:0002174", "HP:0002198", "HP:0002354", "HP:0002363", "HP:0002380", "HP:0002460", "HP:0002483", "HP:0002495", "HP:0002503", "HP:0002542", "HP:0002839", "HP:0002878", "HP:0003202", "HP:0003394", "HP:0003401", "HP:0003431", "HP:0003448", "HP:0003487", "HP:0003693", "HP:0003701", "HP:0006801", "HP:0006937", "HP:0007001", "HP:0007006", "HP:0007078", "HP:0007263", "HP:0007328", "HP:0007338", "HP:0007366", "HP:0007377", "HP:0007928", "HP:0009830", "HP:0010831", "HP:0025331", "HP:0025401", "HP:0030216", "HP:0040129", "HP:0100543", "HP:0410011"),
  SCA10 = c("HP:0000012", "HP:0000020", "HP:0000639", "HP:0000640", "HP:0000716", "HP:0000718", "HP:0000726", "HP:0000741", "HP:0000762", "HP:0001250", "HP:0001260", "HP:0001265", "HP:0001271", "HP:0001272", "HP:0001290", "HP:0001310", "HP:0001347", "HP:0002015", "HP:0002061", "HP:0002062", "HP:0002066", "HP:0002067", "HP:0002070", "HP:0002071", "HP:0002073", "HP:0002075", "HP:0002080", "HP:0002133", "HP:0002141", "HP:0002168", "HP:0002197", "HP:0002311", "HP:0002317", "HP:0002360", "HP:0002384", "HP:0002936", "HP:0003487", "HP:0007256", "HP:0007289", "HP:0007772", "HP:0011153", "HP:0011198", "HP:0030186", "HP:0100660"),
  SCA2 = c("HP:0000020", "HP:0000510", "HP:0000514", "HP:0000597", "HP:0000602", "HP:0000623", "HP:0000639", "HP:0000640", "HP:0000641", "HP:0000657", "HP:0000726", "HP:0001151", "HP:0001251", "HP:0001252", "HP:0001257", "HP:0001260", "HP:0001265", "HP:0001272", "HP:0001290", "HP:0001300", "HP:0001310", "HP:0001332", "HP:0001336", "HP:0002015", "HP:0002063", "HP:0002066", "HP:0002067", "HP:0002070", "HP:0002072", "HP:0002073", "HP:0002075", "HP:0002120", "HP:0002172", "HP:0002174", "HP:0002198", "HP:0002317", "HP:0002345", "HP:0002380", "HP:0002495", "HP:0002503", "HP:0002536", "HP:0002542", "HP:0002839", "HP:0003133", "HP:0003394", "HP:0003487", "HP:0003693", "HP:0006801", "HP:0006955", "HP:0008311", "HP:0012082", "HP:0012762", "HP:0025461", "HP:0030186", "HP:0045007"),
  SCA3_MJD = c("HP:0000508", "HP:0000520", "HP:0000544", "HP:0000590", "HP:0000623", "HP:0000639", "HP:0000640", "HP:0000641", "HP:0000651", "HP:0000726", "HP:0000750", "HP:0001151", "HP:0001251", "HP:0001257", "HP:0001260", "HP:0001272", "HP:0001300", "HP:0001332", "HP:0001347", "HP:0001605", "HP:0001751", "HP:0002015", "HP:0002063", "HP:0002067", "HP:0002070", "HP:0002071", "HP:0002073", "HP:0002078", "HP:0002171", "HP:0002172", "HP:0002198", "HP:0002312", "HP:0002380", "HP:0002495", "HP:0002503", "HP:0002839", "HP:0003202", "HP:0003394", "HP:0003438", "HP:0003487", "HP:0003693", "HP:0004370", "HP:0007089", "HP:0007256", "HP:0012332", "HP:0012532", "HP:0030454"),
  SCA7 = c("HP:0000514", "HP:0000529", "HP:0000548", "HP:0000572", "HP:0000580", "HP:0000597", "HP:0000602", "HP:0000608", "HP:0000613", "HP:0000618", "HP:0000623", "HP:0000639", "HP:0000648", "HP:0000709", "HP:0001098", "HP:0001251", "HP:0001257", "HP:0001260", "HP:0001263", "HP:0001268", "HP:0001270", "HP:0001272", "HP:0001310", "HP:0001319", "HP:0001324", "HP:0001337", "HP:0001347", "HP:0001508", "HP:0001635", "HP:0002015", "HP:0002059", "HP:0002071", "HP:0002072", "HP:0002073", "HP:0002075", "HP:0002310", "HP:0002542", "HP:0003474", "HP:0003487", "HP:0007663", "HP:0011968", "HP:0012047", "HP:0012452"),
  SCA8 = c("HP:0000020", "HP:0000273", "HP:0000514", "HP:0000639", "HP:0000641", "HP:0000716", "HP:0000763", "HP:0000802", "HP:0001251", "HP:0001257", "HP:0001260", "HP:0001272", "HP:0001332", "HP:0001337", "HP:0001347", "HP:0002015", "HP:0002062", "HP:0002063", "HP:0002066", "HP:0002067", "HP:0002070", "HP:0002073", "HP:0002172", "HP:0002311", "HP:0002317", "HP:0002464", "HP:0002495", "HP:0002835", "HP:0006855", "HP:0007256", "HP:0007772", "HP:0009830", "HP:0012110"),
  SCA31 = c("HP:0000365", "HP:0000407", "HP:0000639", "HP:0001251", "HP:0001257", "HP:0001260", "HP:0001265", "HP:0001272", "HP:0001337", "HP:0001347", "HP:0002066", "HP:0002070", "HP:0002495", "HP:0006801", "HP:0007979"),
  FTDALS1 = c("HP:0000605", "HP:0000716", "HP:0000726", "HP:0000738", "HP:0000741", "HP:0000746", "HP:0001260", "HP:0001300", "HP:0001324", "HP:0002059", "HP:0002145", "HP:0002171", "HP:0002186", "HP:0002273", "HP:0002366", "HP:0002385", "HP:0002442", "HP:0002529", "HP:0003202", "HP:0007308", "HP:0007354"),
  SCA6 = c("HP:0000504", "HP:0000639", "HP:0000643", "HP:0000651", "HP:0000763", "HP:0001251", "HP:0001260", "HP:0001272", "HP:0001310", "HP:0001332", "HP:0001347", "HP:0001350", "HP:0002015", "HP:0002017", "HP:0002066", "HP:0002073", "HP:0002078", "HP:0002080", "HP:0002083", "HP:0002120", "HP:0002172", "HP:0002311", "HP:0002317", "HP:0002321", "HP:0002359", "HP:0002505", "HP:0003487", "HP:0007670", "HP:0007772", "HP:0007979", "HP:0010544", "HP:0030511", "HP:0030842"),
  JBS = c("HP:0000003", "HP:0000023", "HP:0000028", "HP:0000047", "HP:0000060", "HP:0000066", "HP:0000126", "HP:0000174", "HP:0000238", "HP:0000243", "HP:0000252", "HP:0000256", "HP:0000286", "HP:0000316", "HP:0000319", "HP:0000324", "HP:0000343", "HP:0000347", "HP:0000348", "HP:0000358", "HP:0000369", "HP:0000431", "HP:0000463", "HP:0000465", "HP:0000470", "HP:0000482", "HP:0000486", "HP:0000494", "HP:0000499", "HP:0000506", "HP:0000508", "HP:0000518", "HP:0000567", "HP:0000568", "HP:0000579", "HP:0000612", "HP:0000625", "HP:0000646", "HP:0000648", "HP:0000656", "HP:0000767", "HP:0000921", "HP:0000964", "HP:0001104", "HP:0001156", "HP:0001161", "HP:0001249", "HP:0001250", "HP:0001257", "HP:0001263", "HP:0001274", "HP:0001290", "HP:0001302", "HP:0001360", "HP:0001371", "HP:0001508", "HP:0001510", "HP:0001511", "HP:0001622", "HP:0001629", "HP:0001631", "HP:0001650", "HP:0001680", "HP:0001734", "HP:0001763", "HP:0001770", "HP:0001831", "HP:0001847", "HP:0001863", "HP:0001873", "HP:0001883", "HP:0002007", "HP:0002019", "HP:0002021", "HP:0002059", "HP:0002119", "HP:0002205", "HP:0002247", "HP:0002414", "HP:0002566", "HP:0002650", "HP:0002827", "HP:0003196", "HP:0003312", "HP:0004209", "HP:0004322", "HP:0004378", "HP:0004383", "HP:0004397", "HP:0005280", "HP:0005469", "HP:0005528", "HP:0006101", "HP:0007018", "HP:0007302", "HP:0008872", "HP:0008947", "HP:0009906", "HP:0010059", "HP:0010761", "HP:0010806", "HP:0100753", "HP:0100840"),
  MODY8 = c("HP:0001738", "HP:0002027", "HP:0003074", "HP:0004904", "HP:0040217"),
  DM2 = c("HP:0000026", "HP:0000135", "HP:0000407", "HP:0000518", "HP:0000798", "HP:0000975", "HP:0001249", "HP:0001262", "HP:0001265", "HP:0001348", "HP:0001638", "HP:0001649", "HP:0001962", "HP:0002015", "HP:0002019", "HP:0002027", "HP:0002292", "HP:0002360", "HP:0002486", "HP:0002850", "HP:0002870", "HP:0002926", "HP:0003077", "HP:0003202", "HP:0003236", "HP:0003326", "HP:0003327", "HP:0003552", "HP:0003554", "HP:0003700", "HP:0003701", "HP:0003722", "HP:0004313", "HP:0004315", "HP:0005978", "HP:0006682", "HP:0007787", "HP:0007889", "HP:0008189", "HP:0008232", "HP:0008981", "HP:0011712", "HP:0012036", "HP:0012378", "HP:0012452", "HP:0012899", "HP:0030319", "HP:0030891", "HP:0031546", "HP:0100543"),
  EDM1_PSACH = c("HP:0000763", "HP:0000926", "HP:0001156", "HP:0001249", "HP:0001288", "HP:0001376", "HP:0001377", "HP:0001382", "HP:0001385", "HP:0001387", "HP:0001498", "HP:0001763", "HP:0002341", "HP:0002515", "HP:0002650", "HP:0002656", "HP:0002663", "HP:0002758", "HP:0002761", "HP:0002808", "HP:0002812", "HP:0002816", "HP:0002829", "HP:0002834", "HP:0002857", "HP:0002938", "HP:0002970", "HP:0003015", "HP:0003016", "HP:0003025", "HP:0003026", "HP:0003049", "HP:0003090", "HP:0003093", "HP:0003170", "HP:0003180", "HP:0003300", "HP:0003301", "HP:0003311", "HP:0003312", "HP:0003365", "HP:0003414", "HP:0003498", "HP:0003502", "HP:0003510", "HP:0003756", "HP:0004019", "HP:0004042", "HP:0004236", "HP:0004568", "HP:0005063", "HP:0005720", "HP:0005743", "HP:0006094", "HP:0006149", "HP:0006429", "HP:0006460", "HP:0006467", "HP:0006499", "HP:0008800", "HP:0008807", "HP:0008833", "HP:0008839", "HP:0008843", "HP:0008873", "HP:0009107", "HP:0009487", "HP:0009803", "HP:0009826", "HP:0009882", "HP:0010049", "HP:0010236", "HP:0010579", "HP:0010582", "HP:0010585", "HP:0010646", "HP:0011405", "HP:0012307", "HP:0020152", "HP:0030839", "HP:0030840", "HP:0030973", "HP:0045086", "HP:0100168", "HP:0100531", "HP:0100864"),
  EPM_DEE = c("HP:0000726", "HP:0000992", "HP:0001249", "HP:0001251", "HP:0001260", "HP:0001336", "HP:0002070", "HP:0002080", "HP:0002392", "HP:0007000")
  )
  sim_mat <- get_sim_grid(ontology = hpo, term_sets = term_set
  )
  sim_mat
  dist_mat <- max(sim_mat) - sim_mat
  plot(hclust(as.dist(dist_mat))
  )
  hclust_obj <- hclust(as.dist(dist_mat)
  )
  pheatmap(sim_mat,
           cluster_rows = hclust_obj,
           cluster_cols = hclust_obj,
           show_rownames = TRUE,
           show_colnames = TRUE,
           fontsize = 6.5
  )
  

    


   
  
# Term set of all hpo terms
term_sets <- list(unnested_hpo_terms$hpo_id)
  # Similarity matrix
  sim_mat <- get_sim_grid(ontology = hpo, term_sets = term_sets)
  sim_mat