# partial model extract works

    Code
      ext
    Output
      # A tibble: 40 x 5
         splits             id     num_comp .extracts        .input              
         <list>             <chr>     <int> <list>           <list>              
       1 <split [1809/204]> Fold01        1 <named list [3]> <tibble [1,720 x 4]>
       2 <split [1809/204]> Fold01        2 <named list [3]> <tibble [1,720 x 5]>
       3 <split [1809/204]> Fold01        3 <named list [3]> <tibble [1,720 x 6]>
       4 <split [1809/204]> Fold01        4 <named list [3]> <tibble [1,720 x 7]>
       5 <split [1809/204]> Fold02        1 <named list [3]> <tibble [1,729 x 4]>
       6 <split [1809/204]> Fold02        2 <named list [3]> <tibble [1,729 x 5]>
       7 <split [1809/204]> Fold02        3 <named list [3]> <tibble [1,729 x 6]>
       8 <split [1809/204]> Fold02        4 <named list [3]> <tibble [1,729 x 7]>
       9 <split [1809/204]> Fold03        1 <named list [3]> <tibble [1,719 x 4]>
      10 <split [1809/204]> Fold03        2 <named list [3]> <tibble [1,719 x 5]>
      # ... with 30 more rows

---

    Code
      out
    Output
      # A tibble: 40 x 3
         id     .input               .output             
         <chr>  <list>               <list>              
       1 Fold01 <tibble [1,720 x 4]> <tibble [1,720 x 1]>
       2 Fold01 <tibble [1,720 x 5]> <tibble [1,720 x 1]>
       3 Fold01 <tibble [1,720 x 6]> <tibble [1,720 x 1]>
       4 Fold01 <tibble [1,720 x 7]> <tibble [1,720 x 1]>
       5 Fold02 <tibble [1,729 x 4]> <tibble [1,729 x 1]>
       6 Fold02 <tibble [1,729 x 5]> <tibble [1,729 x 1]>
       7 Fold02 <tibble [1,729 x 6]> <tibble [1,729 x 1]>
       8 Fold02 <tibble [1,729 x 7]> <tibble [1,729 x 1]>
       9 Fold03 <tibble [1,719 x 4]> <tibble [1,719 x 1]>
      10 Fold03 <tibble [1,719 x 5]> <tibble [1,719 x 1]>
      # ... with 30 more rows

---

    Code
      cv_extraction(tuned_workflow, "recipe")
    Output
      # A tibble: 40 x 5
         splits             id     num_comp .extracts        .input  
         <list>             <chr>     <int> <list>           <list>  
       1 <split [1809/204]> Fold01        1 <named list [3]> <recipe>
       2 <split [1809/204]> Fold01        2 <named list [3]> <recipe>
       3 <split [1809/204]> Fold01        3 <named list [3]> <recipe>
       4 <split [1809/204]> Fold01        4 <named list [3]> <recipe>
       5 <split [1809/204]> Fold02        1 <named list [3]> <recipe>
       6 <split [1809/204]> Fold02        2 <named list [3]> <recipe>
       7 <split [1809/204]> Fold02        3 <named list [3]> <recipe>
       8 <split [1809/204]> Fold02        4 <named list [3]> <recipe>
       9 <split [1809/204]> Fold03        1 <named list [3]> <recipe>
      10 <split [1809/204]> Fold03        2 <named list [3]> <recipe>
      # ... with 30 more rows

