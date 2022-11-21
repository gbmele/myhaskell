 writeFile "gg.txt" $ foldr (\a b -> a ('\n' : b)) "\n"
    (map shows ["fff",
                "the quick brown fox jumspver greg","the lazy doggdd",
                "array2d of this and that",
                "constraint of this and that",
                show(22),
              "cna you insert numbers in next"
          
                ]) 
