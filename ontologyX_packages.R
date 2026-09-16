### Exploring ontologyX Packages


## ontologyIndex
# Introduction to ontologyX (https://cran.r-project.org/web/packages/ontologyIndex/vignettes/intro-to-ontologyX.html)
library(ontologyIndex)
data(hpo)

# Using the `get_term_property()` function to quey the hpo `ontology_index` object and retrieve a particular attribute for a single term
get_term_property(ontology = hpo, property = "ancestors", term = "HP:0001873", as_names = T)

# Look up properties for a given term using "[" and "[["
  # ex: hpo$name
  hpo$name["HP:0001873"]
  # ex: hpo$id
  hpo$id[grep(x = hpo$name, pattern = "Thrombocytopenia")]
  #ex: hpo$ancestors
  hpo$ancestors[["HP:0001873"]]
  hpo$name[hpo$ancestors[["HP:0001873"]]]

# Removing redundant terms with `minimal_set()` function
  #ex: 
  terms <- c("HP:0001871", "HP:0001873", "HP:0011877")
  hpo$name[terms]
  minimal <- minimal_set(hpo, terms)  
  hpo$name[minimal]  

# Finding all ancestors of a set of terms with the `get_ancestors()` function
  get_ancestors(hpo, c("HP:0001873", "HP:0011877"))

# Operating on subclasses
  # Functions which allow set operations with respect to descendancy 
  intersection_with_descendants()
    # Transforms terms by retaining only those which are either in the set of roots or amongst the descendants of a term in roots
  exclude_descendants()
    # Transforms terms by removing terms which are either in the set roots or amongst the descendants of a term in roots
  prune_descendants()
    # Transforms terms by replacing terms which are either in the set roots or amongst the descendants of a term in roots with the associated set of terms in roots
  

    
## ontologySimilarity
# Introduction to ontologySimilarity (https://cran.r-project.org/web/packages/ontologySimilarity/vignettes/ontologySimilarity-introduction.html)
library(ontologyIndex)
library(ontologySimilarity)
data(hpo)
set.seed(1)
  
  # Step 1: Set information content for terms (based on population frequency (ex: frequency with which term is used))
  information_content <- descendants_IC(hpo)
  
  # Step 2: Generate random set of terms (ex: 5 random term sets of 8 terms and remove redundant terms with `minimal_set()`)
  term_sets <- replicate(simplify = F, n = 5, expr = minimal_set(hpo, sample(hpo$id, size = 8)))
  term_sets
  
  # Step 3: Calculate a similarity matrix containing pairwise term-set similarities
  sim_mat <- get_sim_grid(ontology = hpo, term_sets = term_sets)
  sim_mat
  
  # Step 4: Group similarity of phenotypes 1-3 based on `sim_mat` object
  get_sim(sim_mat, group = 1:3)
  
  # Step 5: Get p-value for significance of similarity of phenotypes 1-3
  get_sim_p(sim_mat, group = 1:3)
  
   
# ontologySimilarity Examples (https://cran.r-project.org/web/packages/ontologySimilarity/vignettes/ontologySimilarity-examples.html)
library(ontologyIndex)
library(ontologySimilarity)
data(hpo)
set.seed(1)

# Computing similarity matrices with `get_sim_grid()` function
  # Ingredients: 
    # 1. ontology_index object (ex: data(hpo))
    # 2. Numeric vector of information contents named by term (defaults to `descendants_IC(ontology)`)
    # 3. Some term sets (i.e. list of character vectors of term IDs; `strsplit()'-ting column often needed)
    # 4. Choice of term similarity (either "lin" or "resnik")
  # ex:
  information_content <- descendants_IC(hpo)
  term_sets <- replicate(simplify = F, n = 7, expr = minimal_set(hpo, sample(hpo$id, size = 8)))
  sim_mat <- get_sim_grid(ontology = hpo, term_sets = term_sets)
  sim_mat
  # Transform to a distance matrix to create cluster visualization
  dist_mat <- max(sim_mat) - sim_mat
  plot(hclust(as.dist(dist_mat)))  
      