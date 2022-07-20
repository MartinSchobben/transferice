test_that("recipe step works", {
  
  vars <- role_organizer(dinodat, "t_an")
  terms <- vars[names(vars) != "predictor"]

  rcp <- recipes::recipe(x = dinodat, vars = vars, roles = names(vars)) |> 
    step_taxo(dplyr::any_of(unname(terms))) |> 
    recipes::prep(training = dinodat) |> 
    recipes::bake(NULL)   
  
  custom <- tidyr::pivot_longer(dinodat, !dplyr::all_of(terms)) |> 
    tidyr::separate(name, into = c("genera", "species"), extra = "merge", 
                    fill = "right") |> 
    dplyr::group_by(!!!rlang::syms(c(unname(terms), "genera"))) |> 
    dplyr::summarise(value = sum(value), .groups = "drop") |> 
    tidyr::pivot_wider(names_from = "genera", values_from = "value") 
  
  expect_equal(rcp, custom)
  
})

