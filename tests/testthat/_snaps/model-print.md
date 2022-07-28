# print model results works

    Code
      print_model(transferice:::modern_split, transferice:::modern_workflow, pred = "PC1",
      tune = 1, out = "t_an")
    Output
      <br/>
      <b>features</b>: <br/> <br/> outcome ($Y$): 1<br/> <br/> predictors ($X$): 1<br/> <br/> <b>operations</b>: <br/> <br/>  log center pca zerogeodist<br/> <br/> <b>model</b>: <br/> <br/> $Y = X \beta + \epsilon$<br/> <br/> $cor(\epsilon) = 0$
      <script>if (window.MathJax) MathJax.Hub.Queue(["Typeset", MathJax.Hub]);</script>
      <br/>
      <br/>
      <b>Click on the button 'Train model' to proceed</b>.

---

    Code
      print_model(transferice:::tuned_workflow, transferice:::modern_workflow, pred = "PC1",
      tune = 1, out = "t_an")
    Output
      $RMSE = \sqrt{\frac{1}{N}\sum{\left(Y - \hat{Y} \right)^2}}$<br/><br/> <b>Click the button 'Reset model' to select another model.</b>
      <script>if (window.MathJax) MathJax.Hub.Queue(["Typeset", MathJax.Hub]);</script>
      <br/>
      <br/>

---

    Code
      print_model(transferice:::validation_modern, transferice:::modern_workflow)
    Output
      <br/> <br/> <b>model fit metrics</b>: <br/> <br/>RMSE = 5.6<br/> <br/>
      <script>if (window.MathJax) MathJax.Hub.Queue(["Typeset", MathJax.Hub]);</script>

