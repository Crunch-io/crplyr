In order to allow crplyr vignettes to build both before and after we deprecate the `each` function in `zz9`
queries and replace it with the `dimension` function in `rcrunch`, we made duplicates for two of the cube 
result `.json` files.

`cube-1d0c4f.json` -> `cube-71c70e.json`
`cube-345eb3.json` -> `cube-43dc35.json`


Meaning `cube-1d0c4f.json` and `cube-345eb3.json` can be deleted once `each` is fully deprecated.

(Nothing bad will happen if you don't, but wanted to document this manual edit)
