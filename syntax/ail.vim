syntax keyword AilKeywords if while for from to by else when func proc global let in
syntax match AilCommentLine "//.*$"
syntax region AilCommentBlock start="/\*" end="\*/"
syntax region AilStringSingle start="'" skip="\\." end="'" contains=AilEscape
syntax region AilStringDouble start='"' skip="\\." end='"' contains=AilEscape
syntax match AilEscape "\\." contained
syntax match AilColor /#[0-9A-Fa-f]\{6\}/
syntax match AilNumberDec /-?[1-9][0-9]*\(\.[0-9]*\)?/
syntax match AilNumberHex /-?0[Xx][0-9A-Fa-f]\+/
syntax match AilNumberBin /-?0[Bb][01]\+/
syntax match AilNumberOct /-?0[0-7]*/
syntax keyword AilBool true false
syntax match AilComponentType /\$[A-Za-z_][A-Za-z0-9]*/

highlight default link AilKeywords Statement
highlight default link AilCommentLine Comment
highlight default link AilCommentBlock Comment
highlight default link AilStringSingle String
highlight default link AilStringDouble String
highlight default link AilEscape Special
highlight default link AilColor Constant
highlight default link AilNumberDec Number
highlight default link AilNumberHex Number
highlight default link AilNumberBin Number
highlight default link AilNumberOct Number
highlight default link AilBool Boolean
highlight default link AilComponentType Type
