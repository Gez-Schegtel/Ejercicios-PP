
"Ejemplos sobre el uso de los bucles."
"Esto no funciona en Pharo 11 ya que en esa versión no se puede modificar el string sobre sí mismo."

| toyStory toyStory2 toyStory3 toyStory4 indie holanda |
Transcript clear open.
toyStory:= 'Hay una serpiente en mi bota.'.
toyStory2:= 'Hay una serpiente en mi bota.'.
toyStory3:= 'Hay una serpiente en mi bota'.
toyStory4:= 'Hay una serpiente en mi bota.' substrings.
holanda:= 'Hay una serpiente en mi bota.'.
"Este código toma cada caracter del string y lo reemplaza por sí mismo pero en mayúscula."

1 to: toyStory size do: [:i| toyStory at:i put: (toyStory at:i) asUppercase ].
Transcript show: 'Esto es toyStory después de ejecutar el bucle >> ', toyStory, String cr.

"Este código también toma cada caracter del string toyStory2. Lo malo es que no tenemos un índice para trabajar, por lo que tenemos que definir una cosa más."

indie:= 0.
toyStory2 do: [:each| indie:= indie + 1. toyStory2 at: indie put: each asUppercase ].
Transcript show: 'Esto es toyStory2 después de ejecutar el bucle >> ', toyStory2,
String cr.
toyStory3 substrings do: [:each| Transcript show: each, String cr. ].
Transcript show: 'Esto es toyStory3 después de ejecutar el bucle >> ', toyStory3,
String cr.
toyStory4 do: [:each| Transcript show: each, String cr. ].
Transcript show: 'Esto es toyStory4 después de ejecutar el bucle >> ', toyStory4
asString, String cr.