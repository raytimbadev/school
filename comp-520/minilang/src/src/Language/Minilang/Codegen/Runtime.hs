module Language.Minilang.Codegen.Runtime where

import Language.C.Data as CD

minilangLastSize :: CD.Ident
minilangLastSize = internalIdent "minilang_lastSize"

minilangStrrev :: CD.Ident
minilangStrrev = internalIdent "minilang_strrev"

minilangStrcat :: CD.Ident
minilangStrcat = internalIdent "minilang_strcat"

cPrintf :: CD.Ident
cPrintf = internalIdent "printf"

cGetline :: CD.Ident
cGetline = internalIdent "getline"

cStdin :: CD.Ident
cStdin = internalIdent "stdin"

cMain :: CD.Ident
cMain = internalIdent "main"
