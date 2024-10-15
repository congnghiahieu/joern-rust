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

## CPG Node

- `NewAnnotation`:
  - fullname
- `NewAnnotationLiteral`
- `NewAnnotationParameter`
- `NewAnnotationParameterAssign`
- `NewArrayInitializer`
- `NewBinding`:
  - methodfullname
- `NewBlock`
- `NewCall`
  - methodfullname
  - dispatchType
- `NewClosureBinding`:
  - evaluationStrategy
- `NewComment`: filename
- `NewConfigFile`
- `NewControlStructure`:
  - controlStructureType
- `NewDependency`
- `NewFieldIdentifier`: : No child
- `NewFile`
- `NewFinding`
- `NewIdentifier`
- `NewImport`: No child
- `NewJumpLabel`
- `NewJumpTarget`
- `NewKeyValuePair`
- `NewLiteral`
- `NewLocal`: No child
- `NewLocation`:
  - filename
  - methodfullname
- `NewMember`
- `NewMetaData`
- `NewMethod`:
  - Phải có con là 1 node `NewMethodReturn`, `NewMethodParameterIn`
  - filename
  - fullname
- `NewMethodParameterIn`:
  - evaluationStrategy
  - isVariadic
- `NewMethodParameterOut`:
  - evaluationStrategy
  - isVariadic
- `NewMethodRef`
  - methodfullname
- `NewMethodReturn`:
  - evaluationStrategy
- `NewModifier`:
  - modifierType
- `NewNamespace`
- `NewNamespaceBlock`:
  - filename
  - fullname
- `NewReturn`
- `NewTag`
- `NewTagNodePair`
- `NewTemplateDom`
- `NewType`
  - fullname
  - typeDeclFullName
- `NewTypeArgument`
- `NewTypeDecl`:
  - filename
  - fullName
- `NewTypeParameter`
- `NewTypeRef`: No child
- `NewUnknown`

## CPG Property

- `NAME`
  Name of represented object, e.g., method name (e.g. "run")

- `FULL_NAME`
  This is the fully-qualified name of an entity, e.g., the fully-qualified name of a method or type. The details of what constitutes a fully-qualified name are language specific. This field SHOULD be human readable.

- `CODE`
  This field holds the code snippet that the node represents.

- `METHOD_FULL_NAME`
  The FULL_NAME of a method. Used to link CALL and METHOD nodes. It is required to have exactly one METHOD node for each METHOD_FULL_NAME

- `EVALUATION_STRATEGY`
  For formal method input parameters, output parameters, and return parameters, this field holds the evaluation strategy, which is one of the following: 1) `BY_REFERENCE` indicates that the parameter is passed by reference, 2) `BY_VALUE` indicates that it is passed by value, that is, a copy is made, 3) `BY_SHARING` the parameter is a pointer/reference and it is shared with the caller/callee. While a copy of the pointer is made, a copy of the object that it points to is not made.

- `DISPATCH_TYPE`
  This field holds the dispatch type of a call, which is either `STATIC_DISPATCH` or `DYNAMIC_DISPATCH`. For statically dispatched method calls, the call target is known at compile time while for dynamically dispatched calls, it can only be determined at runtime as it may depend on the type of an object (as is the case for virtual method calls) or calculation of an offset.

- `ORDER`
  This integer indicates the position of the node among its siblings in the AST. The left-most child has an order of 0.

- `CONTROL_STRUCTURE_TYPE`
  The `CONTROL_STRUCTURE_TYPE` field indicates which kind of control structure a `CONTROL_STRUCTURE` node represents. The available types are the following: BREAK, CONTINUE, DO, WHILE, FOR, GOTO, IF, ELSE, TRY, THROW and SWITCH.

- `MODIFIER_TYPE`
  The modifier type is a free-form string. The following are known modifier types: `STATIC`, `PUBLIC`, `PROTECTED`, `PRIVATE`, `ABSTRACT`, `NATIVE`, `CONSTRUCTOR`, `VIRTUAL`

- `TYPE_DECL_FULL_NAME`
  The static type decl of a TYPE. This property is matched against the FULL_NAME of TYPE_DECL nodes. It is required to have exactly one TYPE_DECL for each different TYPE_DECL_FULL_NAME

- `TYPE_FULL_NAME`
  This field contains the fully-qualified static type name of the program construct represented by a node. It is the name of an instantiated type, e.g., `java.util.List<Integer>`, rather than `java.util.List[T]`. If the type cannot be determined, this field should be set to the empty string.

- `IS_VARIADIC`
  Specifies whether a parameter is the variadic argument handling parameter of a variadic method. Only one parameter of a method is allowed to have this property set to true.

- `SIGNATURE`
  The method signature encodes the types of parameters in a string. The string SHOULD be human readable and suitable for differentiating methods with different parameter types sufficiently to allow for resolving of function overloading. The present specification does not enforce a strict format for the signature, that is, it can be chosen by the frontend implementor to fit the source language.

- `FILENAME`
  The path of the source file this node was generated from, relative to the root path in the meta data node. This field must be set but may be set to the value `<unknown>` to indicate that no source file can be associated with the node, e.g., because the node represents an entity known to exist because it is referenced, but for which the file that is is declared in is unknown.

- `ROOT`
  The path to the root directory of the source/binary this CPG is generated from.

- `LANGUAGE`
  This field indicates which CPG language frontend generated the CPG. Frontend developers may freely choose a value that describes their frontend so long as it is not used by an existing frontend. Reserved values are to date: C, LLVM, GHIDRA, PHP.

## Error

- Exception in thread "Writer" java.lang.RuntimeException: Edge with type='ARGUMENT' with direction='IN' not supported by nodeType='ARRAY_INITIALIZER'

- Exception in thread "Writer" java.lang.RuntimeException: Edge with type='CONDITION' with direction='IN' not supported by nodeType='LOCAL'

- Exception in thread "Writer" java.lang.RuntimeException: Edge with type='RECEIVER' with direction='IN' not supported by nodeType='FIELD_IDENTIFIER'

- Exception in thread "Writer" java.lang.RuntimeException: Edge with type='AST' with direction='OUT' not supported by nodeType='TYPE_PARAMETER'

- Exception in thread "Writer" java.lang.RuntimeException: Edge with type='RECEIVER' with direction='IN' not supported by nodeType='LOCAL'
