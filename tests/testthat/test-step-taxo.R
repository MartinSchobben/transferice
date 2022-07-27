test_that("recipe step works", {
  
  vars <- role_organizer(modern, "t_an")
  terms <- vars[names(vars) != "predictor"]

  rcp <- recipes::recipe(x = modern, vars = vars, roles = names(vars)) |> 
    step_taxo(dplyr::any_of(unname(terms))) |> 
    recipes::prep(training = modern) |> 
    recipes::bake(NULL)   
  
  custom <- tidyr::pivot_longer(modern, !dplyr::all_of(terms)) |> 
    tidyr::separate(name, into = c("genera", "species"), extra = "merge", 
                    fill = "right") |> 
    dplyr::group_by(!!!rlang::syms(c(unname(terms), "genera"))) |> 
    dplyr::summarise(value = sum(value), .groups = "drop") |> 
    tidyr::pivot_wider(names_from = "genera", values_from = "value") |> 
    dplyr::mutate(
      source_citation = as.factor(source_citation),
      site_hole = as.factor(site_hole)
    )
  
  expect_equal(rcp, custom)
  
})

