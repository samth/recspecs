# Building
* recompile with `raco setup -D recspecs` 
* recompile individual files that are changed with `raco make <filepath>`
* rebuild documentation with `raco setup specs`
* when new dependencies are added, update `info.rkt` by running `raco setup --fix-pkg-deps recspecs`
* If you need to install a new package, install it with `raco pkg install <pkg>`
* check the syntax of an individual file by running `raco make <filename>`

# Debugging
* If the stack trace from running a file isn't helpful, rerun using errortrace by running
  `racket -l errortrace -t <filename>`

# Testing
 * Find the CI plan in the `.github/workflows` folder.
 * test individual files with `raco test <filename>`
 * test the whole package with `raco test -p recspecs`

# Documentation
* find local documentation for Racket with `raco docs` and `raco docs <identifier>`
* find all documentation for Racket and Racket packages at https://docs.racket-lang.org

# Code style
* run `raco fmt -i <filename>` to format a file properly
* prefer `let loop` over defining a single use recursive function
  
# PR instructions
* don't include test output in the PR description


