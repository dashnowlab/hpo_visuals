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


# Function to fetch parent for a single HPO term
    # May result in multiple parents
get_parent <- function(x) {
  id <- str_extract(x, "^HP:\\d+")
  return(hpo$name[hpo$parent[[id]]])
}
# Add hpo_category (parent) to `unnested_hpo_terms`
unnested_hpo_terms |> mutate(hpo_category = map(hpo_terms, get_parent)) -> unnested_hpo_terms



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
