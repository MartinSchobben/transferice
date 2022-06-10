# partial model extract works

    Code
      ext
    Output
      # A tibble: 10 x 4
         splits             id     .extracts        .input            
         <list>             <chr>  <list>           <list>            
       1 <split [1698/189]> Fold01 <named list [2]> <df [1,698 x 101]>
       2 <split [1698/189]> Fold02 <named list [2]> <df [1,698 x 101]>
       3 <split [1698/189]> Fold03 <named list [2]> <df [1,698 x 101]>
       4 <split [1698/189]> Fold04 <named list [2]> <df [1,698 x 101]>
       5 <split [1698/189]> Fold05 <named list [2]> <df [1,698 x 101]>
       6 <split [1698/189]> Fold06 <named list [2]> <df [1,698 x 101]>
       7 <split [1698/189]> Fold07 <named list [2]> <df [1,698 x 101]>
       8 <split [1699/188]> Fold08 <named list [2]> <df [1,699 x 101]>
       9 <split [1699/188]> Fold09 <named list [2]> <df [1,699 x 101]>
      10 <split [1699/188]> Fold10 <named list [2]> <df [1,699 x 101]>

---

    Code
      out
    Output
      # A tibble: 10 x 2
         id     .output             
         <chr>  <list>              
       1 Fold01 <tibble [1,698 x 7]>
       2 Fold02 <tibble [1,698 x 7]>
       3 Fold03 <tibble [1,698 x 7]>
       4 Fold04 <tibble [1,698 x 7]>
       5 Fold05 <tibble [1,698 x 7]>
       6 Fold06 <tibble [1,698 x 7]>
       7 Fold07 <tibble [1,698 x 7]>
       8 Fold08 <tibble [1,699 x 7]>
       9 Fold09 <tibble [1,699 x 7]>
      10 Fold10 <tibble [1,699 x 7]>

---

    Code
      cv_extraction(tuned_cv, "recipe")
    Output
      # A tibble: 10 x 4
         splits             id     .extracts        .input  
         <list>             <chr>  <list>           <list>  
       1 <split [1698/189]> Fold01 <named list [2]> <recipe>
       2 <split [1698/189]> Fold02 <named list [2]> <recipe>
       3 <split [1698/189]> Fold03 <named list [2]> <recipe>
       4 <split [1698/189]> Fold04 <named list [2]> <recipe>
       5 <split [1698/189]> Fold05 <named list [2]> <recipe>
       6 <split [1698/189]> Fold06 <named list [2]> <recipe>
       7 <split [1698/189]> Fold07 <named list [2]> <recipe>
       8 <split [1699/188]> Fold08 <named list [2]> <recipe>
       9 <split [1699/188]> Fold09 <named list [2]> <recipe>
      10 <split [1699/188]> Fold10 <named list [2]> <recipe>

