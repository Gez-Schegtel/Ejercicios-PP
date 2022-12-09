
"Contar las palabras que empiezen con prefijo algún dado:"

| texto prefijo str cont |
texto:= 'Quizás tenga flores en su ombligo... y además, en sus dedos que se vuelven
pan.' asLowercase.
prefijo:= 'ten'.
cont:= 0.
texto substrings do: [:each| str:= ''. (prefijo size <= each size) ifTrue: [ 1 to: (prefijo size) do: [:i| str:= str, (each at:i) asString ]. (prefijo = str) ifTrue: [ cont:= cont + 1 ] ] ].
^cont.
"1"