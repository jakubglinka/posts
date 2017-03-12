Data preparation based on:

<http://tidytextmining.com/tidytext.html>

The tidy text format
--------------------

Tidy text format is defined as a table with one-term-per-row.

Parsing data to the tibble format
---------------------------------

    ## # A tibble: 22 × 4
    ##                      book   part                          chapter     n
    ##                     <chr>  <chr>                            <chr> <int>
    ## 1  Fellowship of the Ring Book I            A Long-expected Party   796
    ## 2  Fellowship of the Ring Book I           The Shadow of the Past   853
    ## 3  Fellowship of the Ring Book I                 Three is Company   780
    ## 4  Fellowship of the Ring Book I         A Short Cut to Mushrooms   463
    ## 5  Fellowship of the Ring Book I            A Conspiracy Unmasked   417
    ## 6  Fellowship of the Ring Book I                   The Old Forest   512
    ## 7  Fellowship of the Ring Book I     In the House of Tom Bombadil   419
    ## 8  Fellowship of the Ring Book I          Fog on the Barrow-Downs   505
    ## 9  Fellowship of the Ring Book I At the Sign of The Prancing Pony   503
    ## 10 Fellowship of the Ring Book I                          Strider   453
    ## # ... with 12 more rows
