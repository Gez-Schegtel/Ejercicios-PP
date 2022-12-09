
"Mensajes que están buenísimos:"
#(11 38 3 -2 10) collect: [:each | each negated].
"#(-11 -38 -3 2 -10)"
#(11 38 3 -2 10) collect: [:each | each odd].
"#(true false true false false)"
#(11 38 3 -2 10) select: [:each | each odd].
"#(11 3)"
#(11 38 3 -2 10) collect: [:each | each > 10].
"#(true true false false false)"
#(11 38 3 -2 10) select: [:each | each > 10].
"#(11 38)"
#(11 38 3 -2 10) reject: [:each | each > 10].
"#(3 -2 10)"