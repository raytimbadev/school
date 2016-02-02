{-# LANGUAGE ViewPatterns #-}

module Language.Minilang.Codegen where

import Language.Minilang.Syntax as MS
import Language.Minilang.SrcAnn
import Language.Minilang.Typecheck
import Language.Minilang.Codegen.Runtime

import Data.Functor.Foldable
import Data.Monoid ( (<>) )
import Data.Text ( unpack )
import Data.Text.Lazy.Builder
import Language.C.Data as CD
import Language.C.Syntax as CS

-- | Translates a typechecked Minilang program into a C function with given
-- declaration specifiers and declarator.
translateProgram
    :: [CDeclSpec]
    -> CDeclr
    -> [CDecl]
    -> TySrcAnnProgram
    -> CFunDef
translateProgram declspecs declr args (Program decls stmts)
    = CFunDef
        declspecs
        declr
        args
        (CCompound
            []
            (map (CBlockDecl . translateDecl) decls
            ++ map (CBlockStmt . translateStmt) stmts)
            undefNode)
        undefNode

-- | Prepends the minilang runtime to some text.
prependRuntime :: Builder -> Builder
prependRuntime = (minilangRuntime <>)

-- | Translates a typechecked Minilang program into a C \"main\" function.
translateProgramMain :: TySrcAnnProgram -> CFunDef
translateProgramMain
    = translateProgram
        [CTypeSpec (CIntType undefNode)]
        (CDeclr
            (Just cMain)
            [ CFunDeclr
                (Right
                    ( [ CDecl
                        [ CTypeSpec (CIntType undefNode) ]
                        [ ( Just
                            (CDeclr
                                (Just (internalIdent "argc"))
                                []
                                Nothing
                                []
                                undefNode)
                          , Nothing
                          , Nothing
                          )
                        ]
                        undefNode
                    , CDecl
                        [ CTypeSpec (CCharType undefNode) ]
                        [ ( Just
                            (CDeclr
                                (Just (internalIdent "argv"))
                                [ CPtrDeclr [] undefNode
                                , CPtrDeclr [] undefNode
                                ]
                                Nothing
                                []
                                undefNode)
                          , Nothing
                          , Nothing
                          )
                        ]
                        undefNode
                    ]
                  , False
                  ))
                []
                undefNode
            ]
            Nothing
            []
            undefNode)
        []

-- | Translates a Minilang binary operator into its corresponding C binary
-- operator.
translateBinaryOp :: BinaryOp -> CBinaryOp
translateBinaryOp b = case b of
    Plus -> CAddOp
    Minus -> CSubOp
    Times -> CMulOp
    Divide -> CDivOp

-- | Translates user supplied identifiers by converting the "Data.Text.Text" to
-- a "String" and then appending an underscore. This is to avoid user-supplied
-- identitifers from colliding with minilang runtime identifiers.
translateIdent :: MS.Ident -> CD.Ident
translateIdent (unpack -> s) = internalIdent (s ++ "_")

translateDecl :: SrcAnnDeclaration -> CDecl
translateDecl (bareId -> d) = case d of
    Var (bareId -> i) (bareId -> t) ->
        let
            (ct, declr) = case t of
                TyInt -> (CIntType undefNode, [])
                TyString -> (CCharType undefNode, [CPtrDeclr [] undefNode])
                TyReal -> (CDoubleType undefNode, [])
        in
            CDecl
                [ CTypeSpec ct ]
                [ ( Just
                    (CDeclr
                        (Just (translateIdent i))
                        declr
                        Nothing
                        []
                        undefNode)
                  , Nothing
                  , Nothing
                  )
                ]
                undefNode

translateStmt :: TySrcAnnStatement -> CStat
translateStmt = cata f where
    f (Ann _ s) = case s of
        Assign (bareId -> i) e ->
            CExpr
                (Just
                    (CAssign
                        CAssignOp
                        (CVar (translateIdent i) undefNode)
                        (translateExpr e)
                        undefNode))
                undefNode

        While e body ->
            CWhile
                (translateExpr e)
                (CCompound [] (map CBlockStmt body) undefNode)
                False
                undefNode

        If e tb eb ->
            CIf
                (translateExpr e)
                (CCompound [] (map CBlockStmt tb) undefNode)
                (if null eb
                    then Nothing
                    else Just (CCompound [] (map CBlockStmt eb) undefNode))
                undefNode

        Print e ->
            let
                e' = translateExpr e
                t = tyExprType e
                fmt = case t of
                    -- the C pretty-printer takes care of escaping these for us
                    TyInt -> "%d\n"
                    TyReal -> "%lf\n"
                    TyString -> "%s\n"
            in
                CExpr
                    (Just
                        (CCall
                            (CVar cPrintf undefNode)
                            [ CConst (CStrConst (cString fmt) undefNode)
                            , e'
                            ]
                            undefNode))
                    undefNode

        Read i ->
            let
                raw = CVar (translateIdent $ tyIdentName i) undefNode
                deref = CUnary CAdrOp raw undefNode
                (var, fmt) = case tyIdentType i of
                    TyInt -> (deref, "%d")
                    TyReal -> (deref, "%lf")
                    TyString -> (raw, "%s")
            in
                CExpr
                    (Just
                        (CCall
                            (CVar cScanf undefNode)
                            [ CConst (CStrConst (cString fmt) undefNode)
                            , var
                            ]
                            undefNode))
                    undefNode

-- | Translates a typechecked Minilang expression into a C expression.
translateExpr :: TySrcAnnExpr -> CExpr
translateExpr = snd . cata f where
    -- during the recursion, we'll carry around the type annotations manually
    -- so that we can always have access to the determined types of
    -- subexpressions.
    f (Ann (_, ty) e) = (,) ty $ case e of
        Literal (bareId -> l) -> case l of
            Variable x -> CVar (translateIdent x) undefNode
            Int x -> CConst (CIntConst (CInteger x DecRepr noFlags) undefNode)
            Real x -> CConst (CFloatConst (CFloat (show x)) undefNode)
            String x -> CConst (CStrConst (cString (unpack x)) undefNode)

        UnaryOp (bareId -> o) (sty, se) -> case o of
            -- remark: ty = sty in all cases here; see Typecheck.hs
            Negative -> case sty of
                TyInt -> CUnary CMinOp se undefNode
                TyReal -> CUnary CMinOp se undefNode
                TyString ->
                    CCall
                        (CVar minilangStrrev undefNode)
                        [se]
                        undefNode

        BinaryOp (bareId -> o) (_, se1) (_, se2) -> case ty of
            -- if the minilang expression computes an integer, then we can
            -- infer that both subexpressions are also integers, regardless of
            -- the binary operator that is used
            TyInt -> CBinary (translateBinaryOp o) se1 se2 undefNode

            -- if the minilang expression computes a real, then at least one
            -- operand must be a real and both operands must be numeric. We can
            -- take advantage of C's numerics widening rules here and translate
            -- straightforwardly
            TyReal -> CBinary (translateBinaryOp o) se1 se2 undefNode

            -- if the minilang expression computes a string, then both operands
            -- must be strings. The semantics are that the strings should be
            -- concatenated somehow.
            -- The precise nature of that "somehow" depends on the operator
            -- type.
            TyString -> case o of
                -- In the case of addition, the strings are simply
                -- concatenated, so we will call minilang_strcat
                Plus ->
                    CCall
                        (CVar minilangStrcat undefNode)
                        [se1, se2]
                        undefNode

                -- The semantics of Minilang for string subtraction follow the
                -- arithmetic semantics: a - b = a + (-b)
                Minus ->
                    CCall
                        (CVar minilangStrcat undefNode)
                        [ se1
                        , CCall
                            (CVar minilangStrrev undefNode)
                            [se2]
                            undefNode
                        ]
                        undefNode

                -- Only binary operands "Plus" and "Minus" can compute values
                -- of type "TyString" (see Typecheck.hs), so we can safely make
                -- this pattern match partial.
                _ -> error "TyString expression invariant broken"
