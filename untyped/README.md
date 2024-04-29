An interpreter for the untyped lambda calculus

1. No Alpha-Conversion: For simplicity, we assume there are no variable naming conflicts (i.e., no need for alpha-conversion to avoid variable capture).

2. Call by Name: The evaluation strategy used is "call by name", where we only substitute the function's argument into the body without evaluating the argument first.