{-# LANGUAGE OverloadedStrings #-}

module Language.Minilang.Codegen.Runtime where

import Data.Text.Lazy.Builder
import Language.C.Data as CD

minilangLastSize :: CD.Ident
minilangLastSize = internalIdent "minilang_lastSize"

minilangStrrev :: CD.Ident
minilangStrrev = internalIdent "minilang_strrev"

minilangStrcat :: CD.Ident
minilangStrcat = internalIdent "minilang_strcat"

cPrintf :: CD.Ident
cPrintf = internalIdent "printf"

cScanf :: CD.Ident
cScanf = internalIdent "scanf"

cStdin :: CD.Ident
cStdin = internalIdent "stdin"

cMain :: CD.Ident
cMain = internalIdent "main"

minilangRuntime :: Builder
minilangRuntime = mconcat
    [ "#include <stdlib.h>\n"
    , "#include <stdio.h>\n"
    , "#include <string.h>\n"
    , "\n"
    , "char *minilang_strrev(char *str)\n"
    , "{\n"
    , "    int i;\n"
    , "    int n = strlen(str);\n"
    , "    char *revstr = strdup(str);\n"
    , "\n"
    , "    for(i = 0; i < n; i++)\n"
    , "        revstr[i] = str[n - i - 1];\n"
    , "\n"
    , "    return revstr;\n"
    , "}\n"
    , "\n"
    , "char *minilang_strcat(char *str1, char *str2)\n"
    , "{\n"
    , "    int n1 = strlen(str1);\n"
    , "    int n2 = strlen(str2);\n"
    , "    int ncat = n1 + n2;\n"
    , "\n"
    , "    char *cat = calloc(ncat + 1, sizeof(char));\n"
    , "    snprintf(cat, ncat + 1, \"%s%s\", str1, str2);\n"
    , "\n"
    , "    return cat;\n"
    , "}\n"
    ]
