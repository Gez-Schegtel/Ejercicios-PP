
"Cálculo de PI con la fórmula de Leibniz:"
"Opción Juanchéscoli: "

| pi numerador denominador |
numerador:= [:n| (1 negated) raisedTo: n ].
denominador:= [:n| (2 * n) + 1 ].
pi:= 0.
0 to: 99 do: [:i| pi:= pi + ( (numerador value: i) / (denominador value: i) ) ].
^pi.