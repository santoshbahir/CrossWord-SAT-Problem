This projects solves English Crossword puzzle using zChaff SAT solver.

Briefly, this is how it works:
In stores the english dictionary in in-memory Trie data-structure.
It reads the input puzzle and for each cell and English letter defines 
one boolean variable. Using these variables, puzzles it creates Boolean
constraints. This Boolean constraints are Boolean Formulas.

zChaff SAT solver needs input in 3-CNF form.
So the boolean constraints are converted to 2-CNF forms by one small
custom parser.

Once All constraints are in 3-CNF forms, these are feed to the zChaff 
SAT solver. And its output are decoded to get the output of the crossword
puzzle.
