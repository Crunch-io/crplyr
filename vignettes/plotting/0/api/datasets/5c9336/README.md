In order to allow crplyr vignettes to build both before and after we deprecate the `each` function in `zz9`
queries and replace it with the `dimension` function in `rcrunch`, we made duplicates for two of the cube 
result `.json` files. We also changed how measures are named so "count" became "count__count".

`cube-1d0c4f.json` -> `cube-8cbca1.json`
`cube-345eb3.json` -> `cube-c7c07d.json`
`cube-97abb9.json` -> `cube-1add39.json`
`cube-0302b1.json` -> `cube-a9f318.json`
`cube-873456.json` -> `cube-169e06.json`

Meaning `cube-1d0c4f.json` and `cube-345eb3.json` can be deleted once `each` is fully deprecated.

(Nothing bad will happen if you don't, but wanted to document this manual edit)
