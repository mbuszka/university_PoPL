### Exercise 3 (3.36 from EoPL)

Files modified with respect to exercise 1
  - `data-strucutres.scm` lines 76-100
  - `environments.scm` lines 40-46

The file `tests.scm` contains simple test with two mutually recursive procedures
and of course all previous tests.

The main idea is to add another case to environment containing:
  - a list of identifiers
  - a vector of procedures
  - previous environment

And then when creating it with the function `extend-env-rec` create all procedures
with new environment and set contents of the vector appropriately.
