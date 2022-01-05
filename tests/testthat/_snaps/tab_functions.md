# variable label table print is reproduced

    # A tibble: 2 x 2
      var   varlab      
      <chr> <chr>       
    1 x     varlab      
    2 y     char_var_lab

# value label table print is reproduced

    # A tibble: 2 x 2
      var   varlab      
      <chr> <chr>       
    1 x     varlab      
    2 y     char_var_lab

# all table print is reproduced

    # A tibble: 6 x 6
      var      nv cv        n vallab varlab      
      <chr> <int> <chr> <int> <chr>  <chr>       
    1 x         1 <NA>      1 1lab   varlab      
    2 x         2 <NA>      1 2lab   varlab      
    3 x         3 <NA>      1 3lab   varlab      
    4 y        NA A         1 1lab   char_var_lab
    5 y        NA B         1 2lab   char_var_lab
    6 y        NA C         1 3lab   char_var_lab

# all attributes table print is reproduced

    # A tibble: 3 x 7
      var   varlab    val       vallab    labels    label        class    
      <chr> <list>    <list>    <list>    <list>    <chr>        <chr>    
    1 id    <NULL>    <NULL>    <NULL>    <NULL>    <NA>         integer  
    2 x     <chr [1]> <int [3]> <chr [3]> <int [3]> varlab       integer  
    3 y     <chr [1]> <chr [3]> <chr [3]> <chr [3]> char_var_lab character

