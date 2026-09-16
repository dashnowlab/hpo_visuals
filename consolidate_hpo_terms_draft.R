## HPO Frequency Plot Drafting for Final Quarto File


# Visualize hpo_terms with a frequency of 13+ across diseases and with "Ataxia" types consolidated (Based on "children" using `ontologyIndex` package with `data(hpo)`
hpo_frequency <- unnested_hpo_terms |> mutate(
  hpo_terms = fct_recode(hpo_terms,
                         "HP:0001251 Ataxia" = "HP:0001251 Ataxia",
                         "HP:0001251 Ataxia" = "HP:0001310 Dysmetria",
                         "HP:0001251 Ataxia" = "HP:0002066 Gait ataxia",
                         "HP:0001251 Ataxia" = "HP:0002070 Limb ataxia",
                         "HP:0001251 Ataxia" = "HP:0002073 Progressive cerebellar ataxia",
                         "HP:0001251 Ataxia" = "HP:0002075 Dysdiadochokinesis",
                         "HP:0001251 Ataxia" = "HP:0002078 Truncal ataxia",
                         "HP:0001251 Ataxia" = "HP:0010867 Dyssynergia",
                         "HP:0001251 Ataxia" = "HP:0002070 Limb Ataxia",
                         "HP:0001260 Dysarthria" = "HP:0001260 Dysarthria",
                         "HP:0001260 Dysarthria" = "HP:0002464 Spastic dysarthria",
                         "HP:0001260 Dysarthria" = "HP:0007024 Pseudobulbar paralysis",
                         "HP:0001260 Dysarthria" = "HP:0008376 Nasal dysarthria",
                         "HP:0002015 Dysphagia" = "HP:0002015 Dysphagia",
                         "HP:0002015 Dysphagia" = "HP:0007024 Pseudobulbar paralysis",
                         "HP:0002015 Dysphagia" = "HP:0031162 Impaired oropharyngeal swallow response",
                         "HP:0002015 Dysphagia" = "HP:0200136 Oral-pharyngeal dysphagia",
                         "HP:0001272 Cerebellar atrophy" = "HP:0001272 Cerebellar atrophy",
                         "HP:0001272 Cerebellar atrophy" = "HP:0006855 Cerebellar vermis atrophy",
                         "HP:0001272 Cerebellar atrophy" = "HP:0007263 Spinocerebellar atrophy",
                         "HP:0001272 Cerebellar atrophy" = "HP:0012082 Cerebellar Purkinje layer atrophy",
                         "HP:0001272 Cerebellar atrophy" = "HP:0100275 Diffuse cerebellar atrophy",
                         "HP:0001288 Gait disturbance" = "HP:0001288 Gait disturbance",
                         "HP:0001288 Gait disturbance" = "HP:0002136 Broad-based gait",
                         "HP:0001288 Gait disturbance" = "HP:0002141 Gait imbalance",
                         "HP:0001288 Gait disturbance" = "HP:0002317 Unsteady gait",
                         "HP:0001288 Gait disturbance" = "HP:0002362 Shuffling gait",
                         "HP:0001288 Gait disturbance" = "HP:0002515 Waddling gait",
                         "HP:0001288 Gait disturbance" = "HP:0002527 Falls",
                         "HP:0001288 Gait disturbance" = "HP:0002540 Inability to walk",
                         "HP:0001288 Gait disturbance" = "HP:0003376 Steppage gait",
                         "HP:0001288 Gait disturbance" = "HP:0030051 Tip-toe gait",
                         "HP:0001288 Gait disturbance" = "HP:0031629 Impaired tandem gait"
  )) |> count(hpo_terms, sort = T) |> filter (n >= 13)
ggplot(hpo_frequency, aes(x = n, y = fct_infreq(hpo_terms, (n)) |> fct_rev())) + geom_col(fill = "lightblue") + labs(title = "Most Frequent HPOs", subtitle = "Phenotypes with a frequency of 13+ across STR diseases", x = "Frequency", y = "HPO Term")