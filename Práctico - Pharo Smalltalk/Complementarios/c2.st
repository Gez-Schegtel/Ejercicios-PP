
"Dado un texto, determinar la cantidad de vocales participantes, totalizando solamente las que participan."

|n1 cont|

n1:= UIManager default request: 'Ingrese un texto: '.
(n1 = nil) ifTrue: [ ^nil ].
cont:=0.

n1 do:[:i | i isVowel ifTrue:[cont:=cont+1]].

^cont.