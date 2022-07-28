# file naming conventions work

    Code
      file_namer("rds", "raw", "dinocyst_t_an_global")
    Output
      [1] "raw_dinocyst_t_an_global_species_prop"

---

    Code
      file_namer("rds", "engineering", "raw_dinocyst_t_an_global_prop_species", taxa = "genera",
        method = "partial_fit", trans = "lm")
    Output
      [1] "engineering_dinocyst_t_an_global_partial_fit_genera_lm"

# method for saving are selected

    Code
      method_selector(nm, type = "rds")
    Output
      # A tibble: 3,873 x 197
         sample_id sample_depth_mbsf hole_id site_hole longitude latitude
             <int>             <dbl>   <int> <chr>         <dbl>    <dbl>
       1         1                 0       1 E003          -69.5     48.2
       2         2                 0       2 E004          -69.6     48.2
       3         3                 0       3 E005          -69.3     48.2
       4         4                 0       4 E006          -69.4     48.3
       5         5                 0       5 E007          -69.4     48.3
       6         6                 0       6 E008          -69.3     48.3
       7         7                 0       7 E010          -69.0     48.5
       8         8                 0       8 E012          -68.5     48.6
       9         9                 0       9 E013          -68.6     48.8
      10        10                 0      10 E014          -68.7     48.9
      # ... with 3,863 more rows, and 191 more variables: source_citation <chr>,
      #   age_ma <dbl>, `Acanthaulax sp.` <dbl>, `Achomosphaera andalousiense` <dbl>,
      #   `Aireiana verrucosa` <dbl>, `Algidasphaeridium minutum var. cezare` <dbl>,
      #   `Algidasphaeridium minutum var. minutum` <dbl>,
      #   `Amiculosphaera umbracula` <dbl>, `Apteodinium australiense` <dbl>,
      #   `Areoligera semicirculata` <dbl>, `Areoligera spp.` <dbl>,
      #   `Ataxiodinium choane` <dbl>, `Ataxiodinium confusum` <dbl>, ...

# caching works

    Code
      method_selector(nm, type = "rds")
    Output
      # A tibble: 3,873 x 197
         sample_id sample_depth_mbsf hole_id site_hole longitude latitude
             <int>             <dbl>   <int> <chr>         <dbl>    <dbl>
       1         1                 0       1 E003          -69.5     48.2
       2         2                 0       2 E004          -69.6     48.2
       3         3                 0       3 E005          -69.3     48.2
       4         4                 0       4 E006          -69.4     48.3
       5         5                 0       5 E007          -69.4     48.3
       6         6                 0       6 E008          -69.3     48.3
       7         7                 0       7 E010          -69.0     48.5
       8         8                 0       8 E012          -68.5     48.6
       9         9                 0       9 E013          -68.6     48.8
      10        10                 0      10 E014          -68.7     48.9
      # ... with 3,863 more rows, and 191 more variables: source_citation <chr>,
      #   age_ma <dbl>, `Acanthaulax sp.` <dbl>, `Achomosphaera andalousiense` <dbl>,
      #   `Aireiana verrucosa` <dbl>, `Algidasphaeridium minutum var. cezare` <dbl>,
      #   `Algidasphaeridium minutum var. minutum` <dbl>,
      #   `Amiculosphaera umbracula` <dbl>, `Apteodinium australiense` <dbl>,
      #   `Areoligera semicirculata` <dbl>, `Areoligera spp.` <dbl>,
      #   `Ataxiodinium choane` <dbl>, `Ataxiodinium confusum` <dbl>, ...

