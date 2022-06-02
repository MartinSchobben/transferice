# file naming conventions work

    Code
      file_namer("rds", "raw", "dinocyst")
    Output
      [1] "raw_dinocyst_count_unprocessed"

---

    Code
      file_namer("rds", "prep", "dinocyst", trans = "lm")
    Output
      [1] "prep_dinocyst_count_lm"

# method for saving are selected

    Code
      method_selector(nm, type = "rds")
    Output
      # A tibble: 3,670 x 96
         sample_id hole_id    `22`    `32`    `33`  `43`   `45`  `49`   `55`    `61`
             <int>   <int>   <dbl>   <dbl>   <dbl> <dbl>  <dbl> <dbl>  <dbl>   <dbl>
       1         1       1 0.0335  0.0168  0.00559 0.330 0.0894 0.469 0.0503 0.00559
       2         2       2 0.0217  0.0109  0       0.207 0.0761 0.587 0.0435 0.0217 
       3         3       3 0.0556  0.00617 0       0.136 0.130  0.543 0.0494 0.0556 
       4         4       4 0.00714 0       0       0.271 0.121  0.486 0.0857 0.0214 
       5         5       5 0.0293  0.00488 0       0.293 0.0732 0.541 0.0146 0.0341 
       6         6       6 0.0520  0.00578 0       0.116 0.0925 0.624 0.104  0.00578
       7         7       7 0.0319  0.0213  0.0106  0.138 0.0745 0.617 0.0426 0.0106 
       8         8       8 0.0643  0.0175  0.00585 0.199 0.0234 0.579 0.0234 0.0760 
       9         9       9 0.02    0.0133  0       0.35  0.0567 0.387 0.0733 0.0767 
      10        10      10 0.00709 0.0177  0.00709 0.465 0.0355 0.277 0.184  0      
      # ... with 3,660 more rows, and 86 more variables: `3` <dbl>, `19` <dbl>,
      #   `41` <dbl>, `2` <dbl>, `46` <dbl>, `44` <dbl>, `10` <dbl>, `34` <dbl>,
      #   `8` <dbl>, `11` <dbl>, `38` <dbl>, `36` <dbl>, `63` <dbl>, `6` <dbl>,
      #   `9` <dbl>, `28` <dbl>, `51` <dbl>, `56` <dbl>, `57` <dbl>, `24` <dbl>,
      #   `29` <dbl>, `53` <dbl>, `12` <dbl>, `54` <dbl>, `35` <dbl>, `17` <dbl>,
      #   `31` <dbl>, `37` <dbl>, `64` <dbl>, `52` <dbl>, `62` <dbl>, `65` <dbl>,
      #   `48` <dbl>, `59` <dbl>, `60` <dbl>, `58` <dbl>, `23` <dbl>, `69` <dbl>, ...

