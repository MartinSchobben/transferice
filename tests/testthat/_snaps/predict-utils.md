# impute taxa

    Code
      impute_taxa(nm, fossil, "t_an", return_type = "impute")
    Output
      $model
      == Workflow [trained] ==========================================================
      Preprocessor: Recipe
      Model: linear_reg()
      
      -- Preprocessor ----------------------------------------------------------------
      4 Recipe Steps
      
      * step_log()
      * step_center()
      * step_pca()
      * step_zerogeodist()
      
      -- Model -----------------------------------------------------------------------
      Generalized least squares fit by REML
        Model: t_an ~ . - longitude - latitude 
        Data: data 
        Log-restricted-likelihood: -7277.792
      
      Coefficients:
      (Intercept)         PC1         PC2         PC3         PC4 
       10.6241079   3.8572483  -3.8201705   0.7372267  -2.3654352 
      
      Correlation Structure: Gaussian spatial correlation
       Formula: ~longitude | latitude 
       Parameter estimate(s):
           range     nugget 
      33.9576807  0.1319996 
      Degrees of freedom: 2360 total; 2355 residual
      Residual standard error: 5.488808 
      
      $new_data
      # A tibble: 190 x 102
         sample_id sample_depth_mbsf hole_id site_hole longitude latitude
             <int>             <dbl>   <int> <chr>         <dbl>    <dbl>
       1      3674              0.6     3614 189-1168A      144.    -42.6
       2      3675              7.24    3614 189-1168A      144.    -42.6
       3      3676              7.9     3614 189-1168A      144.    -42.6
       4      3677             10.9     3614 189-1168A      144.    -42.6
       5      3678             13.9     3614 189-1168A      144.    -42.6
       6      3679             17       3614 189-1168A      144.    -42.6
       7      3680             17.6     3614 189-1168A      144.    -42.6
       8      3681             20.4     3614 189-1168A      144.    -42.6
       9      3682             23.4     3614 189-1168A      144.    -42.6
      10      3683             26.7     3614 189-1168A      144.    -42.6
      # ... with 180 more rows, and 96 more variables: source_citation <chr>,
      #   age_ma <dbl>, `Pentapharsodinium dalei` <dbl>, `Brigantedinium spp.` <dbl>,
      #   `Selenopemphix quanta` <dbl>, `Bitectatodinium tepikiense` <dbl>,
      #   `Spiniferites spp.` <dbl>, `Ataxiodinium choane` <dbl>,
      #   `Impagidinium patulum` <dbl>, `Impagidinium pallidum` <dbl>,
      #   `Impagidinium sphaericum` <dbl>, `Impagidinium aculeatum` <dbl>,
      #   `Impagidinium paradoxum` <dbl>, `Trinovantedinium applanatum` <dbl>, ...
      

---

    Code
      impute_taxa(nm, fossil, "t_an", return_type = "percent")
    Output
      [1] 23.40426

---

    Code
      reduce_taxa(nm, fossil)
    Output
      $model
      parsnip model object
      
      Generalized least squares fit by REML
        Model: t_an ~ . - longitude - latitude 
        Data: data 
        Log-restricted-likelihood: -7277.792
      
      Coefficients:
      (Intercept)         PC1         PC2         PC3         PC4 
       10.6241079   3.8572483  -3.8201705   0.7372267  -2.3654352 
      
      Correlation Structure: Gaussian spatial correlation
       Formula: ~longitude | latitude 
       Parameter estimate(s):
           range     nugget 
      33.9576807  0.1319996 
      Degrees of freedom: 2360 total; 2355 residual
      Residual standard error: 5.488808 
      
      $new_data
      # A tibble: 190 x 12
         longitude latitude sample_id hole_id sample_depth_mbsf age_ma site_hole
             <dbl>    <dbl>     <int>   <int>             <dbl>  <dbl> <fct>    
       1      144.    -42.6      3674    3614              0.6  0.0722 189-1168A
       2      144.    -42.6      3675    3614              7.24 0.872  189-1168A
       3      144.    -42.6      3676    3614              7.9  0.951  189-1168A
       4      144.    -42.6      3677    3614             10.9  1.31   189-1168A
       5      144.    -42.6      3678    3614             13.9  1.67   189-1168A
       6      144.    -42.6      3679    3614             17    2.05   189-1168A
       7      144.    -42.6      3680    3614             17.6  2.12   189-1168A
       8      144.    -42.6      3681    3614             20.4  2.46   189-1168A
       9      144.    -42.6      3682    3614             23.4  2.63   189-1168A
      10      144.    -42.6      3683    3614             26.7  2.78   189-1168A
      # ... with 180 more rows, and 5 more variables: source_citation <fct>,
      #   PC1 <dbl>, PC2 <dbl>, PC3 <dbl>, PC4 <dbl>
      

# get age

    Code
      age_finder(fossil)
    Output
      # A tibble: 190 x 128
         sample_id sample_depth_mbsf hole_id site_hole longitude latitude
             <int>             <dbl>   <int> <chr>         <dbl>    <dbl>
       1      3674              0.6     3614 189-1168A      144.    -42.6
       2      3675              7.24    3614 189-1168A      144.    -42.6
       3      3676              7.9     3614 189-1168A      144.    -42.6
       4      3677             10.9     3614 189-1168A      144.    -42.6
       5      3678             13.9     3614 189-1168A      144.    -42.6
       6      3679             17       3614 189-1168A      144.    -42.6
       7      3680             17.6     3614 189-1168A      144.    -42.6
       8      3681             20.4     3614 189-1168A      144.    -42.6
       9      3682             23.4     3614 189-1168A      144.    -42.6
      10      3683             26.7     3614 189-1168A      144.    -42.6
      # ... with 180 more rows, and 122 more variables: source_citation <chr>,
      #   age_ma <dbl>, `Acanthaulax sp.` <dbl>, `Achomosphaera andalousiense` <dbl>,
      #   `Aireiana verrucosa` <dbl>, `Algidasphaeridium minutum var. cezare` <dbl>,
      #   `Algidasphaeridium minutum var. minutum` <dbl>,
      #   `Amiculosphaera umbracula` <dbl>, `Apteodinium australiense` <dbl>,
      #   `Areoligera semicirculata` <dbl>, `Areoligera spp.` <dbl>,
      #   `Ataxiodinium choane` <dbl>, `Ataxiodinium confusum` <dbl>, ...

