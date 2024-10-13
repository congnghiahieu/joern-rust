# Syn AST

- https://docs.rs/syn/latest/syn/index.html#structs
- https://docs.rs/syn/latest/syn/index.html#enums

# Joern CPG

- https://cpg.joern.io

# Syn AST -> Joern CPG

## Auto gen

- `FILE`
- `SOURCE_FILE`
- `NAMESPACE`

## Manual gen

- ... -> `CODE`, `FULL_NAME`, `INDEX`, `IS_EXTERNAL`, `NAME`
- ... -> `META_DATA`, `LANGUAGE`, `ROOT`, `OVERLAYS`
- ... -> `FILENAME` field
- ... -> `METHOD_REF`, `TYPE_REF`

## Enum

- `AttrStyle` ->
- `BinOp` ->
- `Dataderive` ->
- `Expr` ->
- `FieldMutability` ->
- `Fields` ->
- `FnArg` ->
- `ForeignItem` ->
- `GenericArgument` ->
- `GenericParam` ->
- `ImplItem` ->
- `ImplRestriction` ->
- `Item` ->
- `Lit` ->
- `MacroDelimiter` ->
- `Member` ->
- `Meta` ->
- `Pat` ->
- `PathArguments` ->
- `RangeLimits` ->
- `ReturnType` ->
- `StaticMutability` ->
- `Stmt` ->
- `TraitBoundModifier` ->
- `TraitItem` ->
- `Type` ->
- `TypeParamBound` ->
- `UnOp` ->
- `UseTree` ->
- `Visibility` ->
- `WherePredicate` ->

## Struct

- `Abi` -> `NewNamespaceBlock` | `NewIdentifier`
- `AngleBracketedGenericArguments` -> `NewTypeArgument` | `NewTypeParameter`
- `Arm` -> `NewControlStructure`
- `AssocConst` -> `NewTypeArgument`
- `AssocType` -> `NewTypeArgument`
- `Attribute` -> `NewAnnotation`
- `BareFnArg` ->
- `BareVariadic` ->
- `Block` -> `NewBlock`
- `BoundLifetimes` -> `NewTypeParameter`
- `ConstParam` -> `NewTypeParameter`
- `Constraint` -> `NewTypeArgument`
- `ExprArray` -> `NewArrayInitializer`
- `ExprAssign` ->
- `ExprAsync` -> `NewBlock`
- `ExprAwait` -> `NewCall`
- `ExprBinary` ->
- `ExprBlock` -> `NewBlock`
- `ExprBreak` -> `NewControlStructure`
- `ExprCall` -> `NewCall`
- `ExprCast` -> `NewCall`
- `ExprClosure` -> `NewMethod`
- `ExprConst` -> `NewLocal`
- `ExprContinue` -> `NewControlStructure`
- `ExprField` -> `NewFieldIdentifier`
- `ExprForLoop` -> `NewControlStructure`
- `ExprGroup` ->
- `ExprIf` -> `NewControlStructure`
- `ExprIndex` -> `NewFieldIdentifier`
- `ExprInfer` -> `NewLocal`
- `ExprLet` -> `NewLocal`
- `ExprLit` -> `NewLiteral`
- `ExprLoop` -> `NewControlStructure`
- `ExprMacro` -> `NewCall`
- `ExprMatch` -> `NewControlStructure`
- `ExprMethodCall` -> `NewCall`
- `ExprParen` ->
- `ExprPath` -> `NewTypeRef` | `NewMethodRef`
- `ExprRange` -> `NewArrayInitializer`
- `ExprReference` -> `NewTypeRef`
- `ExprRepeat` -> `NewArrayInitializer`
- `ExprReturn` -> `NewReturn`
- `ExprStruct` -> `NewLocal`
- `ExprTry` -> `NewControlStructure`
- `ExprTryBlock` -> `NewBlock`
- `ExprTuple` -> `NewArrayInitializer`
- `ExprUnary` ->
- `ExprUnsafe` -> `NewBlock`
- `ExprWhile` -> `NewControlStructure`
- `ExprYield` -> `NewReturn`
- `Field` -> `NewMember`
- `FieldPat` -> `NewMember`
- `FieldValue` -> `NewMember`
- `FieldsNamed` ->
- `FieldsUnnamed` ->
- `File` -> `NewFile`
- `ForeignItemFn` -> `NewMember` && `NewMethod`
- `ForeignItemMacro` -> `NewMember` && `NewCall`
- `ForeignItemStatic` -> `NewMember` && `NewLocal`
- `ForeignItemType` -> `NewMember`&&`NewTypeDecl`
- `Generics` -> `NewTypeParameter`
- `Ident` -> `NewIdentifier`
- `ImplItemConst` -> `NewMember` && `NewLocal`
- `ImplItemFn` -> `NewMember` && `NewMethod`
- `ImplItemMacro` -> `NewMember` && `NewCall`
- `ImplItemType` -> `NewMember`&&`NewTypeDecl`
- `Index` -> `NewFieldIdentifier`
- `ItemConst` -> `NewLocal`
- `ItemEnum` -> `NewTypeDecl`
- `ItemExternCrate` -> `NewImport`
- `ItemFn` -> `NewMethod`
- `ItemForeignMod` -> `NewNamespaceBlock`
- `ItemImpl` -> `NewTypeDecl`
- `ItemMacro` -> `NewCall`
- `ItemMod` -> `NewImport` | `NewNamespaceBlock`
- `ItemStatic` -> `NewLocal`
- `ItemStruct` -> `NewTypeDecl`
- `ItemTrait` -> `NewTypeDecl`
- `ItemTraitAlias` -> `NewTypeDecl`
- `ItemType` -> `NewTypeDecl`
- `ItemUnion` -> `NewTypeDecl`
- `ItemUse` -> `NewImport`
- `Label` -> `NewJumpLabel`
- `Lifetime` -> `NewTypeArgument` | `NewTypeParameter`
- `LifetimeParam` -> `NewTypeParameter`
- `LitBool` -> `NewLiteral`
- `LitByte` -> `NewLiteral`
- `LitByteStr` -> `NewLiteral`
- `LitCStr` -> `NewLiteral`
- `LitChar` -> `NewLiteral`
- `LitFloat` -> `NewLiteral`
- `LitInt` -> `NewLiteral`
- `LitStr` -> `NewLiteral`
- `Local` -> `NewLocal`
- `LocalInit` ->
- `Macro` -> `NewCall`
- `MetaList` -> `NewAnnotationParameter`
- `MetaNameValue` -> `NewAnnotationParameterAssign`
- `ParenthesizedGenericArguments` -> `NewTypeArgument` | `NewTypeParameter`
- `PatConst` ->
- `PatIdent` ->
- `PatLit` ->
- `PatMacro` ->
- `PatOr` ->
- `PatParen` ->
- `PatPath` ->
- `PatRange` ->
- `PatReference` ->
- `PatRest` ->
- `PatSlice` ->
- `PatStruct` ->
- `PatTuple` ->
- `PatTupleStruct` ->
- `PatType` -> `NewMethodParameterIn`
- `PatWild` ->
- `Path` -> `NewTypeRef` | `NewMethodRef`
- `PathSegment` ->
- `PredicateLifetime` -> `NewTypeParameter`
- `PredicateType` -> `NewTypeParameter`
- `QSelf` -> `NewTypeParameter`
- `Receiver` -> `NewMethodParameterIn`
- `Signature` ->
- `StmtMacro` -> `NewCall`
- `TraitBound` -> `NewTypeParameter`
- `TraitItemConst` -> `NewMember` && `NewLocal`
- `TraitItemFn` -> `NewMember` && `NewMethod`
- `TraitItemMacro` -> `NewMember` && `NewCall`
- `TraitItemType` ->`NewMember` && `NewTypeDecl`
- `TypeArray` -> `NewTypeRef`
- `TypeBareFn` -> `NewTypeRef`
- `TypeGenerics` -> `NewTypeRef`
- `TypeGroup` -> `NewTypeRef`
- `TypeImplTrait` -> `NewTypeRef`
- `TypeInfer` -> `NewTypeRef`
- `TypeMacro` -> `NewTypeRef`
- `TypeNever` -> `NewTypeRef`
- `TypeParam` -> `NewTypeRef`
- `TypeParen` -> `NewTypeRef`
- `TypePath` -> `NewTypeRef`
- `TypePtr` -> `NewTypeRef`
- `TypeReference` -> `NewTypeRef`
- `TypeSlice` -> `NewTypeRef`
- `TypeTraitObject` -> `NewTypeRef`
- `TypeTuple` -> `NewTypeRef`
- `UseGlob` -> `NewImport`
- `UseGroup` -> `NewImport`
- `UseName` -> `NewImport`
- `UsePath` -> `NewImport`
- `UseRename` -> `NewImport`
- `Variadic` -> `NewMethodParameterIn`
- `Variant` -> `NewMember`
- `VisRestricted` -> `NewModifier`
- `WhereClause` -> `NewTypeParameter`

`NewAnnotation`
`NewAnnotationLiteral`
`NewAnnotationParameter`
`NewAnnotationParameterAssign`
`NewArrayInitializer`
`NewBinding`
`NewBlock`
`NewCall`
`NewClosureBinding`
`NewComment`
`NewConfigFile`
`NewControlStructure`
`NewDependency`
`NewFieldIdentifier`
`NewFile`
`NewFinding`
`NewIdentifier`
`NewImport`
`NewJumpLabel`
`NewJumpTarget`
`NewKeyValuePair`
`NewLiteral`
`NewLocal`
`NewLocation`
`NewMember`
`NewMetaData`
`NewMethod`
`NewMethodParameterIn`
`NewMethodParameterOut`
`NewMethodRef`
`NewMethodReturn`
`NewModifier`
`NewNamespace`
`NewNamespaceBlock`
`NewReturn`
`NewTag`
`NewTagNodePair`
`NewTemplateDom`
`NewType`
`NewTypeArgument`
`NewTypeDecl`
`NewTypeParameter`
`NewTypeRef`
`NewUnknown`
