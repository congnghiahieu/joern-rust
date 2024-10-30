# Joern - The Bug Hunter's Workbench

[![release](https://github.com/joernio/joern/actions/workflows/release.yml/badge.svg)](https://github.com/joernio/joern/actions/workflows/release.yml)
[![Joern SBT](https://index.scala-lang.org/joernio/joern/latest.svg)](https://index.scala-lang.org/joernio/joern)
[![Github All Releases](https://img.shields.io/github/downloads/joernio/joern/total.svg)](https://github.com/joernio/joern/releases/)
[![Gitter](https://img.shields.io/badge/-Discord-lime?style=for-the-badge&logo=discord&logoColor=white&color=black)](https://discord.com/invite/vv4MH284Hc)

Joern is a platform for analyzing source code, bytecode, and binary
executables. It generates code property graphs (CPGs), a graph
representation of code for cross-language code analysis. Code property
graphs are stored in a custom graph database. This allows code to be
mined using search queries formulated in a Scala-based domain-specific
query language. Joern is developed with the goal of providing a useful
tool for vulnerability discovery and research in static program
analysis.

Website: https://joern.io

Documentation: https://docs.joern.io/

Specification: https://cpg.joern.io

## News / Changelog

- Joern v2.0.0 [upgrades from Scala2 to Scala3](changelog/2.0.0-scala3.md)
- Joern v1.2.0 removes the `overflowdb.traversal.Traversal` class. This change is not completely backwards compatible. See [here](changelog/traversal_removal.md) for a detailed writeup.

## Requirements

- JDK 19 (other versions _might_ work, but have not been properly tested)
- _optional_: gcc and g++ (for auto-discovery of C/C++ system header files if included/used in your C/C++ code)

## Development Requirements

- mvn https://maven.apache.org/install.html

## Quick Installation

```bash
wget https://github.com/joernio/joern/releases/latest/download/joern-install.sh
chmod +x ./joern-install.sh
sudo ./joern-install.sh
joern

     ██╗ ██████╗ ███████╗██████╗ ███╗   ██╗
     ██║██╔═══██╗██╔════╝██╔══██╗████╗  ██║
     ██║██║   ██║█████╗  ██████╔╝██╔██╗ ██║
██   ██║██║   ██║██╔══╝  ██╔══██╗██║╚██╗██║
╚█████╔╝╚██████╔╝███████╗██║  ██║██║ ╚████║
 ╚════╝  ╚═════╝ ╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝
Version: 2.0.1
Type `help` to begin

joern>
```

If the installation script fails for any reason, try

```bash
./joern-install --interactive
```

## Docker based execution

```bash
docker run --rm -it -v /tmp:/tmp -v $(pwd):/app:rw -w /app -t ghcr.io/joernio/joern joern
```

To run joern in server mode:

```bash
docker run --rm -it -v /tmp:/tmp -v $(pwd):/app:rw -w /app -t ghcr.io/joernio/joern joern --server
```

Almalinux 9 requires the CPU to support SSE4.2. For kvm64 VM use the Almalinux 8 version instead.

```bash
docker run --rm -it -v /tmp:/tmp -v $(pwd):/app:rw -w /app -t ghcr.io/joernio/joern-alma8 joern
```

## Releases

A new release is [created automatically](.github/workflows/release.yml) once per day. Contributers can also manually run the [release workflow](https://github.com/joernio/joern/actions/workflows/release.yml) if they need the release sooner.

## Developers

### Contribution Guidelines

Thank you for taking time to contribute to Joern! Here are a few guidelines to ensure your pull request will get merged as soon as possible:

- Try to make use of the templates as far as possible, however they may not suit all needs. The minimum we would like to see is:
  - A title that briefly describes the change and purpose of the PR, preferably with the affected module in square brackets, e.g. `[javasrc2cpg] Addition Operator Fix`.
  - A short description of the changes in the body of the PR. This could be in bullet points or paragraphs.
  - A link or reference to the related issue, if any exists.
- Do not:
  - Immediately CC/@/email spam other contributors, the team will review the PR and assign the most appropriate contributor to review the PR. Joern is maintained by industry partners and researchers alike, for the most part with their own goals and priorities, and additional help is largely volunteer work. If your PR is going stale, then reach out to us in follow-up comments with @'s asking for an explanation of priority or planning of when it may be addressed (if ever, depending on quality).
  - Leave the description body empty, this makes reviewing the purpose of the PR difficult.
- Remember to:
  - Remember to format your code, i.e. run `sbt scalafmt Test/scalafmt`
  - Add a unit test to verify your change.

### IDE setup

#### Intellij IDEA

- [Download Intellij Community](https://www.jetbrains.com/idea/download)
- Install and run it
- Install the [Scala Plugin](https://plugins.jetbrains.com/plugin/1347-scala) - just search and install from within Intellij.
- Important: open `sbt` in your local joern repository, run `compile` and keep it open - this will allow us to use the BSP build in the next step
- Back to Intellij: open project: select your local joern clone: select to open as `BSP project` (i.e. _not_ `sbt project`!)
- Await the import and indexing to complete, then you can start, e.g. `Build -> build project` or run a test

#### VSCode

- Install VSCode and Docker
- Install the plugin `ms-vscode-remote.remote-containers`
- Open Joern project folder in [VSCode](https://docs.microsoft.com/en-us/azure-sphere/app-development/container-build-vscode#build-and-debug-the-project)
  Visual Studio Code detects the new files and opens a message box saying: `Folder contains a Dev Container configuration file. Reopen to folder to develop in a container.`
- Select the `Reopen in Container` button to reopen the folder in the container created by the `.devcontainer/Dockerfile` file
- Switch to `scalameta.metals` sidebar in VSCode, and select `import build` in `BUILD COMMANDS`
- After `import build` succeeds, you are ready to start writing code for Joern

## QueryDB (queries plugin)

Quick way to develop and test QueryDB:

```bash
sbt stage
./querydb-install.sh
./joern-scan --list-query-names
```

The last command prints all available queries - add your own in querydb, run the above commands again to see that your query got deployed.
More details in the [separate querydb readme](querydb/README.md)

# Thứ tự chạy

- joern-cli/frontends/x2cpg/src/main/scala/io/joern/x2cpg/X2Cpg.scala `X2CpgMain`
- joern-cli/frontends/x2cpg/src/main/scala/io/joern/x2cpg/X2Cpg.scala `main()`
- joern-cli/frontends/rustsrc2cpg/src/main/scala/io/joern/rustsrc2cpg/Main.scala `rustcpg.run(config)`
- joern-cli/frontends/rustsrc2cpg/src/main/scala/io/joern/rustsrc2cpg/parser/RustCpg.scala `createCpg`
- joern-cli/frontends/rustsrc2cpg/src/main/scala/io/joern/rustsrc2cpg/ast/GoModule.scala `loadModule()`
- joern-cli/frontends/x2cpg/src/main/scala/io/joern/x2cpg/passes/frontend/MetaDataPass.scala `MetaDataPass`
- joern-cli/frontends/rustsrc2cpg/src/main/scala/io/joern/rustsrc2cpg/passes/AstCreationPass.scala `AstCreationPass`
- joern-cli/frontends/rustsrc2cpg/src/main/scala/io/joern/rustsrc2cpg/passes/AstCreationPass.scala `generateParts`
- joern-cli/frontends/rustsrc2cpg/src/main/scala/io/joern/rustsrc2cpg/passes/AstCreationPass.scala `runOnPart`
- joern-cli/frontends/rustsrc2cpg/src/main/scala/io/joern/rustsrc2cpg/parser/JsonParser.scala `parse`
- joern-cli/frontends/rustsrc2cpg/src/main/scala/io/joern/rustsrc2cpg/parser/JsonParser.scala `mapParent` (recursive)
- joern-cli/frontends/rustsrc2cpg/src/main/scala/io/joern/rustsrc2cpg/astcreation/AstCreator.scala `AstCreator`
- joern-cli/frontends/rustsrc2cpg/src/main/scala/io/joern/rustsrc2cpg/astcreation/AstCreator.scala `createAst`
- joern-cli/frontends/rustsrc2cpg/src/main/scala/io/joern/rustsrc2cpg/astcreation/AstCreator.scala `astForTranslationUnit`
- joern-cli/frontends/rustsrc2cpg/src/main/scala/io/joern/rustsrc2cpg/astcreation/AstCreator.scala `astForPackageNode`
- joern-cli/frontends/rustsrc2cpg/src/main/scala/io/joern/rustsrc2cpg/astcreation/AstCreator.scala `astForGoAstNode`
- joern-cli/frontends/rustsrc2cpg/src/main/scala/io/joern/rustsrc2cpg/passes/TypeResolverPass.scala `TypeResolverPass`
- joern-cli/frontends/rustsrc2cpg/src/main/scala/io/joern/rustsrc2cpg/passes/TypeResolverPass.scala `run`
- joern-cli/frontends/rustsrc2cpg/src/main/scala/io/joern/rustsrc2cpg/passes/ModuleResolverPass.scala `ModuleResolverPass`
- joern-cli/frontends/rustsrc2cpg/src/main/scala/io/joern/rustsrc2cpg/passes/ModuleResolverPass.scala `run`

convert below rust to scala in these line, with NOTE what:

- using `class`
- declare variable inside class curly brace
- do not generate constructor
- use None value for Option type
- Use `ListBuffer` to replace `Vec` and `Punctuated`
- When you see attribute `skip_serializing_if`, use Option as type wrapper
- When you see attribute `#[serde(flatten)]`, using `@JsonUnwrapped` property of jackson for that field
- When you see attribute `#[serde(transparent)]`, generate a comment above that variable
- Replace `Box` type with `Option` in scala
- Special notice for `rename` attribute in `#[serde(rename = "...")]`. For example, have `#[serde(rename = "stmts")]`, so use `stmts` as variable name
- Do not use `defaultness`, use `default`
- Do not use `unsafety`, use `unsafe`
- Do nut use `mutability` use `mut`
- Dot not use `block` use `stmts`

- We already have `attrs`, skip it

#file:Item.scala apply all class (from ItemForeignMod) variable to these case, note that:

Given class, apply variable to these case, note that

- ignore variable `attrs`
- use variable name instead of `case _:`
- see above examples
- ignore variable that have been commented
- ignore variable `mac`, `unsafe`, `ident`, `move`, `label`, `const`, `static`, `async`, `dot2_token`, `lifetime`
- ignore variable that have type `String`, `Indent`, `Index`, `Option[Boolean]`, `Boolean`
- For type `Item`, use `mapParentForItem`
- For type `Visibility`, use `mapParentForVisibility`
- For type `Fields`, use `mapParentForFields`
- For type `FieldsNamed`, use `mapParentForFieldsNamed`
- For type `FieldsUnnamed`, use `mapParentForFieldsUnnamed`
- For type `Type`, use `mapParentForType`
- For type `Pat`, use `mapParentForPat`
- For type `Expr`, use `mapParentForExpr`
- For type `Stmt`, use `mapParentForStmt`
- For type `FnArg`, use `mapParentForFnArg`
- For type `ReturnType`, use `mapParentForReturnType`
- For type `BoundLifetimes`, use `mapParentForBoundLifetimes`
- For type `GenericParam`, use `mapParentForGenericParam`
- For type `WherePredicate`, use `mapParentForWherePredicate`
- For type `TokenStream`, use `mapParentForTokenStream`
- For type `TokenTree`, use `mapParentForTokenTree`
- For type `ForeignItem`, use `mapParentForForeignItem`
- For type `ImplItem`, use `mapParentForImplItem`
- For type `TypeParamBound`, use `mapParentForTypeParamBound`
- For type `TraitItem`, use `mapParentForTraitItem`
- For type `UseTree`, use `mapParentForUseTree`
- For type `GenericArgument`, use `mapParentForGenericArgument`
- For type `PathArguments`, use `mapParentForPathArguments`

- convert this to fit all below function, use `else if` instead of `if`. At the end put a `else` cause that throw error
- Use `astFor` function below, corresspoding their variable name

- see #, # (function ``) and # , generate compatible file with the same pattern
- convert this to fit all below function, use `else if` instead of `if`. At the end put a `else` cause that throw error
- Do not use `addChild`

- generate 1 more trait CodeFor... based on this trait, function return String instead of Ast

- With class of function, for example `ExprClosure` generate missing code for each property of class. See above function as examples

- create a variable equal `NewTypeArgument()`, then wrapped it in `Ast()`
- use `NewTypeArgument()` only, remove .name

- give related information to `TraitItemConst...`.
- create a variable equal `New...()` (NewLocal for example), then wrapped it in `Ast(NewMember())`
- Use Ast(NewMember()).withChild(...)
- Remove comment

- rename all function name to pattern `astForPat...`

- convert all the `if ... else Nil` to using flatmap to check Option
- Correspoding variable then using `withChildren` instead of `withChild`
- See this examples: `val annotationsAst = macroStmtInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))`

For each function, generate the missing code:

- a variable typeFullname equal to corresspoding `typeFullnameForType...` of function
- a variable node equal `NewTypeRef()` with `.typeFullname` set to variable `typeFullname` above
- return `Ast(node)`
- see function `astForTypeArray` for example

```
- Convert all code using `.toList.flatMap(_.map)` to using `match`

For example from this:
`val annotationsAst = arrayExprInstance.attrs.toList.flatMap(_.map(astForAttribute(filename, parentFullname, _)))`

Convert to:
`val annotationsAst = arrayExprInstance.attrs match {
case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, \_)).toList
case None => List()
}`

add annotationsAst vairable for all of these

For example:

`val annotationsAst = arrayExprInstance.attrs match {
case Some(attrs) => attrs.map(astForAttribute(filename, parentFullname, _)).toList
case None => List()
}`

Combile with return Ast, for example

controlStructureAst(armNode, Some(conditionAst), bodyAst)
.withChildren(annotationsAst)

move `val annotationsAst` to head of function body
```

- convert all

```
val genericsAst = typeImplItemInstance.generics match {
      case Some(generics) => List(astForGenerics(filename, parentFullname, generics))
      case None           => List()
    }
```

to

```
val genericsAst = typeImplItemInstance.generics match {
      case Some(generics) => astForGenerics(filename, parentFullname, generics)
      case None           => Ast()
    }
```

and use `withChild` instead of `withChildren` for `genericsAst`

- replace `unknownNode(UnknownAst(), "")` with `unknownNode(variableName, "")`

For example: `unknownNode(UnknownAst(), "")` to `unknownNode(referencePatInstance, "")`
