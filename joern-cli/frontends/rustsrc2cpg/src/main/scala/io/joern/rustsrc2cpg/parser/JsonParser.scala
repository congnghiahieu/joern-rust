package io.joern.rustsrc2cpg.parser

import com.fasterxml.jackson.databind.DeserializationFeature
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.module.SimpleModule
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.google.gson.Gson
import com.google.gson.JsonDeserializer
import com.google.gson.JsonElement
import com.google.gson.JsonParseException
import com.google.gson.JsonSerializer
import io.joern.rustsrc2cpg.ast.*
import io.shiftleft.utils.IOUtils

import java.nio.file.Paths
import scala.collection.mutable.ListBuffer

class JsonParser {
  private val gson                     = new Gson()
  private val objectMapper             = new ObjectMapper()
  private val customDeserializerModule = new SimpleModule()

  objectMapper
    .registerModule(DefaultScalaModule)
    .configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, true)
    .configure(DeserializationFeature.FAIL_ON_READING_DUP_TREE_KEY, true)
    .configure(DeserializationFeature.FAIL_ON_NULL_FOR_PRIMITIVES, true)
    .configure(DeserializationFeature.FAIL_ON_NUMBERS_FOR_ENUMS, true)
    .configure(DeserializationFeature.FAIL_ON_INVALID_SUBTYPE, true)
    .configure(DeserializationFeature.FAIL_ON_IGNORED_PROPERTIES, true)
    .configure(DeserializationFeature.FAIL_ON_UNRESOLVED_OBJECT_IDS, true)
  // .configure(DeserializationFeature.FAIL_ON_MISSING_CREATOR_PROPERTIES, true)

  // Register custom deserializer
  customDeserializerModule.addDeserializer(classOf[Type], new TypeDeserializer)
  customDeserializerModule.addDeserializer(classOf[Fields], new FieldsDeserializer)
  customDeserializerModule.addDeserializer(classOf[PathArguments], new PathArgumentsDeserializer)
  customDeserializerModule.addDeserializer(classOf[PathArguments], new PathArgumentsDeserializer)
  customDeserializerModule.addDeserializer(classOf[UseTree], new UseTreeDeserializer)
  customDeserializerModule.addDeserializer(classOf[Visibility], new VisibilityDeserializer)
  objectMapper.registerModule(customDeserializerModule)

  def parse(filepath: String): FileAst = {
    val fileContent = IOUtils.readEntireFile(Paths.get(filepath))
    val fileAst     = objectMapper.readValue(fileContent, classOf[FileAst])
    fileAst
    // mapParent(Some(fileAst), None)
  }

  private def mapParentForItem(itemInstance: Item, parent: Option[RustAst]): Unit = {
    if (!parent.isDefined) {
      return
    }

    if (itemInstance.constItem.isDefined) {
      mapParent(itemInstance.constItem, parent)
    }
    if (itemInstance.enumItem.isDefined) {
      mapParent(itemInstance.enumItem, parent)
    }
    if (itemInstance.externCrateItem.isDefined) {
      mapParent(itemInstance.externCrateItem, parent)
    }
    if (itemInstance.fnItem.isDefined) {
      mapParent(itemInstance.fnItem, parent)
    }
    if (itemInstance.foreignModItem.isDefined) {
      mapParent(itemInstance.foreignModItem, parent)
    }
    if (itemInstance.implItem.isDefined) {
      mapParent(itemInstance.implItem, parent)
    }
    if (itemInstance.macroItem.isDefined) {
      mapParent(itemInstance.macroItem, parent)
    }
    if (itemInstance.modItem.isDefined) {
      mapParent(itemInstance.modItem, parent)
    }
    if (itemInstance.staticItem.isDefined) {
      mapParent(itemInstance.staticItem, parent)
    }
    if (itemInstance.structItem.isDefined) {
      mapParent(itemInstance.structItem, parent)
    }
    if (itemInstance.traitItem.isDefined) {
      mapParent(itemInstance.traitItem, parent)
    }
    if (itemInstance.traitAliasItem.isDefined) {
      mapParent(itemInstance.traitAliasItem, parent)
    }
    if (itemInstance.typeItem.isDefined) {
      mapParent(itemInstance.typeItem, parent)
    }
    if (itemInstance.unionItem.isDefined) {
      mapParent(itemInstance.unionItem, parent)
    }
    if (itemInstance.useItem.isDefined) {
      mapParent(itemInstance.useItem, parent)
    }
    if (itemInstance.verbatimItem.isDefined) {
      mapParentForTokenStream(itemInstance.verbatimItem.get, parent)
    }
  }

  private def mapParentForVisibility(visibilityInstance: Visibility, parent: Option[RustAst]): Unit = {
    if (!parent.isDefined) {
      return
    }

    visibilityInstance match {
      case _: VisibilityString => {
        mapParent(Some(visibilityInstance.asInstanceOf[VisibilityString]), parent)
      }
      case _: VisibilityOther => {
        mapParent(visibilityInstance.asInstanceOf[VisibilityOther].restricted, parent)
      }
    }
  }

  private def mapParentForFields(fieldsInstance: Fields, parent: Option[RustAst]): Unit = {
    if (!parent.isDefined) {
      return
    }

    fieldsInstance match {
      case _: FieldsUnit => {
        mapParent(Some(fieldsInstance.asInstanceOf[FieldsUnit]), parent)
      }
      case _: FieldsNotUnit => {
        if (fieldsInstance.asInstanceOf[FieldsNotUnit].named.isDefined) {
          mapParentForFieldsNamed(fieldsInstance.asInstanceOf[FieldsNotUnit].named.get, parent)
        }
        if (fieldsInstance.asInstanceOf[FieldsNotUnit].unnamed.isDefined) {
          mapParentForFieldsUnnamed(fieldsInstance.asInstanceOf[FieldsNotUnit].unnamed.get, parent)
        }
      }
    }
  }

  private def mapParentForFieldsNamed(fieldsNamedInstance: FieldsNamed, parent: Option[RustAst]): Unit = {
    if (!parent.isDefined) {
      return
    }

    fieldsNamedInstance.foreach(field => {
      mapParent(Some(field), parent)
    })

  }

  private def mapParentForFieldsUnnamed(fieldsUnnamedInstance: FieldsUnnamed, parent: Option[RustAst]): Unit = {
    if (!parent.isDefined) {
      return
    }

    fieldsUnnamedInstance.foreach(field => {
      mapParent(Some(field), parent)
    })

  }

  private def mapParentForType(typeInstance: Type, parent: Option[RustAst]): Unit = {
    if (!parent.isDefined) {
      return
    }

    typeInstance match {
      case _: TypeNoValue => {
        mapParent(Some(typeInstance.asInstanceOf[TypeNoValue]), parent)
      }
      case typeHasValue: TypeHasValue => {
        if (typeHasValue.arrayType.isDefined) {
          mapParent(typeHasValue.arrayType, parent)
        }
        if (typeHasValue.bareFnType.isDefined) {
          mapParent(typeHasValue.bareFnType, parent)
        }
        if (typeHasValue.groupType.isDefined) {
          mapParent(typeHasValue.groupType, parent)
        }
        if (typeHasValue.implTraitType.isDefined) {
          mapParent(typeHasValue.implTraitType, parent)
        }
        if (typeHasValue.macroType.isDefined) {
          mapParent(typeHasValue.macroType, parent)
        }
        if (typeHasValue.parenType.isDefined) {
          mapParent(typeHasValue.parenType, parent)
        }
        if (typeHasValue.pathType.isDefined) {
          mapParent(typeHasValue.pathType, parent)
        }
        if (typeHasValue.ptrType.isDefined) {
          mapParent(typeHasValue.ptrType, parent)
        }
        if (typeHasValue.referenceType.isDefined) {
          mapParent(typeHasValue.referenceType, parent)
        }
        if (typeHasValue.sliceType.isDefined) {
          mapParent(typeHasValue.sliceType, parent)
        }
        if (typeHasValue.traitObjectType.isDefined) {
          mapParent(typeHasValue.traitObjectType, parent)
        }
        if (typeHasValue.tupleType.isDefined) {
          mapParent(typeHasValue.tupleType, parent)
        }
        if (typeHasValue.verbatimType.isDefined) {
          mapParentForTokenStream(typeHasValue.verbatimType.get, parent)
        }
      }
    }
  }
  private def mapParentForPat(patInstance: Pat, parent: Option[RustAst]): Unit = {
    if (!parent.isDefined) {
      return
    }

    if (patInstance.constPat.isDefined) {
      mapParent(patInstance.constPat, parent)
    }
    if (patInstance.identPat.isDefined) {
      mapParent(patInstance.identPat, parent)
    }
    if (patInstance.litPat.isDefined) {
      mapParent(patInstance.litPat, parent)
    }
    if (patInstance.macroPat.isDefined) {
      mapParent(patInstance.macroPat, parent)
    }
    if (patInstance.orPat.isDefined) {
      mapParent(patInstance.orPat, parent)
    }
    if (patInstance.parenPat.isDefined) {
      mapParent(patInstance.parenPat, parent)
    }
    if (patInstance.pathPat.isDefined) {
      mapParent(patInstance.pathPat, parent)
    }
    if (patInstance.rangePat.isDefined) {
      mapParent(patInstance.rangePat, parent)
    }
    if (patInstance.referencePat.isDefined) {
      mapParent(patInstance.referencePat, parent)
    }
    if (patInstance.restPat.isDefined) {
      mapParent(patInstance.restPat, parent)
    }
    if (patInstance.slicePat.isDefined) {
      mapParent(patInstance.slicePat, parent)
    }
    if (patInstance.structPat.isDefined) {
      mapParent(patInstance.structPat, parent)
    }
    if (patInstance.tuplePat.isDefined) {
      mapParent(patInstance.tuplePat, parent)
    }
    if (patInstance.tupleStructPat.isDefined) {
      mapParent(patInstance.tupleStructPat, parent)
    }
    if (patInstance.typePat.isDefined) {
      mapParent(patInstance.typePat, parent)
    }
    if (patInstance.verbatimPat.isDefined) {
      mapParentForTokenStream(patInstance.verbatimPat.get, parent)
    }
    if (patInstance.wildPat.isDefined) {
      mapParent(patInstance.wildPat, parent)
    }

  }

  private def mapParentForExpr(exprInstance: Expr, parent: Option[RustAst]): Unit = {
    if (!parent.isDefined) {
      return
    }

    if (exprInstance.arrayExpr.isDefined) {
      mapParent(exprInstance.arrayExpr, parent)
    }
    if (exprInstance.assignExpr.isDefined) {
      mapParent(exprInstance.assignExpr, parent)
    }
    if (exprInstance.asyncExpr.isDefined) {
      mapParent(exprInstance.asyncExpr, parent)
    }
    if (exprInstance.awaitExpr.isDefined) {
      mapParent(exprInstance.awaitExpr, parent)
    }
    if (exprInstance.binaryExpr.isDefined) {
      mapParent(exprInstance.binaryExpr, parent)
    }
    if (exprInstance.blockExpr.isDefined) {
      mapParent(exprInstance.blockExpr, parent)
    }
    if (exprInstance.breakExpr.isDefined) {
      mapParent(exprInstance.breakExpr, parent)
    }
    if (exprInstance.callExpr.isDefined) {
      mapParent(exprInstance.callExpr, parent)
    }
    if (exprInstance.castExpr.isDefined) {
      mapParent(exprInstance.castExpr, parent)
    }
    if (exprInstance.closureExpr.isDefined) {
      mapParent(exprInstance.closureExpr, parent)
    }
    if (exprInstance.constExpr.isDefined) {
      mapParent(exprInstance.constExpr, parent)
    }
    if (exprInstance.continueExpr.isDefined) {
      mapParent(exprInstance.continueExpr, parent)
    }
    if (exprInstance.fieldExpr.isDefined) {
      mapParent(exprInstance.fieldExpr, parent)
    }
    if (exprInstance.forLoopExpr.isDefined) {
      mapParent(exprInstance.forLoopExpr, parent)
    }
    if (exprInstance.groupExpr.isDefined) {
      mapParent(exprInstance.groupExpr, parent)
    }
    if (exprInstance.ifExpr.isDefined) {
      mapParent(exprInstance.ifExpr, parent)
    }
    if (exprInstance.indexExpr.isDefined) {
      mapParent(exprInstance.indexExpr, parent)
    }
    if (exprInstance.inferExpr.isDefined) {
      mapParent(exprInstance.inferExpr, parent)
    }
    if (exprInstance.letExpr.isDefined) {
      mapParent(exprInstance.letExpr, parent)
    }
    if (exprInstance.litExpr.isDefined) {
      mapParent(exprInstance.litExpr, parent)
    }
    if (exprInstance.loopExpr.isDefined) {
      mapParent(exprInstance.loopExpr, parent)
    }
    if (exprInstance.macroExpr.isDefined) {
      mapParent(exprInstance.macroExpr, parent)
    }
    if (exprInstance.matchExpr.isDefined) {
      mapParent(exprInstance.matchExpr, parent)
    }
    if (exprInstance.methodCallExpr.isDefined) {
      mapParent(exprInstance.methodCallExpr, parent)
    }
    if (exprInstance.parenExpr.isDefined) {
      mapParent(exprInstance.parenExpr, parent)
    }
    if (exprInstance.pathExpr.isDefined) {
      mapParent(exprInstance.pathExpr, parent)
    }
    if (exprInstance.rangeExpr.isDefined) {
      mapParent(exprInstance.rangeExpr, parent)
    }
    if (exprInstance.referenceExpr.isDefined) {
      mapParent(exprInstance.referenceExpr, parent)
    }
    if (exprInstance.repeatExpr.isDefined) {
      mapParent(exprInstance.repeatExpr, parent)
    }
    if (exprInstance.returnExpr.isDefined) {
      mapParent(exprInstance.returnExpr, parent)
    }
    if (exprInstance.structExpr.isDefined) {
      mapParent(exprInstance.structExpr, parent)
    }
    if (exprInstance.tryExpr.isDefined) {
      mapParent(exprInstance.tryExpr, parent)
    }
    if (exprInstance.tryBlockExpr.isDefined) {
      mapParent(exprInstance.tryBlockExpr, parent)
    }
    if (exprInstance.tupleExpr.isDefined) {
      mapParent(exprInstance.tupleExpr, parent)
    }
    if (exprInstance.unaryExpr.isDefined) {
      mapParent(exprInstance.unaryExpr, parent)
    }
    if (exprInstance.unsafeExpr.isDefined) {
      mapParent(exprInstance.unsafeExpr, parent)
    }
    if (exprInstance.verbatimExpr.isDefined) {
      mapParentForTokenStream(exprInstance.verbatimExpr.get, parent)
    }
    if (exprInstance.whileExpr.isDefined) {
      mapParent(exprInstance.whileExpr, parent)
    }
    if (exprInstance.yieldExpr.isDefined) {
      mapParent(exprInstance.yieldExpr, parent)
    }
  }

  private def mapParentForStmt(stmtInstance: Stmt, parent: Option[RustAst]): Unit = {
    if (!parent.isDefined) {
      return
    }

    if (stmtInstance.letStmt.isDefined) {
      mapParent(stmtInstance.letStmt, parent)
    }
    if (stmtInstance.itemStmt.isDefined) {
      mapParentForItem(stmtInstance.itemStmt.get, parent)
    }
    if (stmtInstance.exprStmt.isDefined) {
      mapParentForExpr(stmtInstance.exprStmt.get._1, parent)
    }
    if (stmtInstance.macroStmt.isDefined) {
      mapParent(stmtInstance.macroStmt, parent)
    }
  }

  private def mapParentForFnArg(fnArgInstance: FnArg, parent: Option[RustAst]): Unit = {
    if (!parent.isDefined) {
      return
    }

    if (fnArgInstance.receiverFnArg.isDefined) {
      mapParent(fnArgInstance.receiverFnArg, parent)
    }
    if (fnArgInstance.typedFnArg.isDefined) {
      mapParent(fnArgInstance.typedFnArg, parent)
    }
  }

  private def mapParentForReturnType(returnTypeInstance: ReturnType, parent: Option[RustAst]): Unit = {
    if (!returnTypeInstance.isDefined) {
      return
    }
    if (!parent.isDefined) {
      return
    }

    mapParentForType(returnTypeInstance.get, parent)

  }
  private def mapParentForBoundLifetimes(boundLifetimesInstance: BoundLifetimes, parent: Option[RustAst]): Unit = {
    if (!parent.isDefined) {
      return
    }

    boundLifetimesInstance.foreach(boundLifetime => {
      mapParentForGenericParam(boundLifetime, parent)
    })

  }

  private def mapParentForGenericParam(genericParamInstance: GenericParam, parent: Option[RustAst]): Unit = {
    if (!parent.isDefined) {
      return
    }

    if (genericParamInstance.lifetimeGenericParam.isDefined) {
      mapParent(genericParamInstance.lifetimeGenericParam, parent)
    }
    if (genericParamInstance.typeGenericParam.isDefined) {
      mapParent(genericParamInstance.typeGenericParam, parent)
    }
    if (genericParamInstance.constGenericParam.isDefined) {
      mapParent(genericParamInstance.constGenericParam, parent)
    }
  }
  private def mapParentForWherePredicate(wherePredicateInstance: WherePredicate, parent: Option[RustAst]): Unit = {
    if (!parent.isDefined) {
      return
    }

    if (wherePredicateInstance.lifetimeWherePredicate.isDefined) {
      mapParent(wherePredicateInstance.lifetimeWherePredicate, parent)
    }
    if (wherePredicateInstance.typeWherePredicate.isDefined) {
      mapParent(wherePredicateInstance.typeWherePredicate, parent)
    }
  }

  private def mapParentForTokenStream(tokenStreamInstance: TokenStream, parent: Option[RustAst]): Unit = {
    if (!parent.isDefined) {
      return
    }

    tokenStreamInstance.foreach(token => {
      mapParentForTokenTree(token, parent)
    })
  }

  private def mapParentForTokenTree(tokenTreeInstance: TokenTree, parent: Option[RustAst]): Unit = {
    if (!parent.isDefined) {
      return
    }

    if (tokenTreeInstance.group.isDefined) {
      mapParent(tokenTreeInstance.group, parent)
    }
    if (tokenTreeInstance.punct.isDefined) {
      mapParent(tokenTreeInstance.punct, parent)
    }
  }

  private def mapParentForForeignItem(foreignItemInstance: ForeignItem, parent: Option[RustAst]): Unit = {
    if (!parent.isDefined) {
      return
    }

    if (foreignItemInstance.fnForeignItem.isDefined) {
      mapParent(foreignItemInstance.fnForeignItem, parent)
    }
    if (foreignItemInstance.staticForeignItem.isDefined) {
      mapParent(foreignItemInstance.staticForeignItem, parent)
    }
    if (foreignItemInstance.typeForeignItem.isDefined) {
      mapParent(foreignItemInstance.typeForeignItem, parent)
    }
    if (foreignItemInstance.macroForeignItem.isDefined) {
      mapParent(foreignItemInstance.macroForeignItem, parent)
    }
    if (foreignItemInstance.verbatimForeignItem.isDefined) {
      mapParentForTokenStream(foreignItemInstance.verbatimForeignItem.get, parent)
    }
  }

  private def mapParentForImplItem(implItemInstance: ImplItem, parent: Option[RustAst]): Unit = {
    if (!parent.isDefined) {
      return
    }

    if (implItemInstance.constImplItem.isDefined) {
      mapParent(implItemInstance.constImplItem, parent)
    }
    if (implItemInstance.fnImplItem.isDefined) {
      mapParent(implItemInstance.fnImplItem, parent)
    }
    if (implItemInstance.typeImplItem.isDefined) {
      mapParent(implItemInstance.typeImplItem, parent)
    }
    if (implItemInstance.macroImplItem.isDefined) {
      mapParent(implItemInstance.macroImplItem, parent)
    }
    if (implItemInstance.verbatimImplItem.isDefined) {
      mapParentForTokenStream(implItemInstance.verbatimImplItem.get, parent)
    }
  }

  private def mapParentForTypeParamBound(typeParamBoundInstance: TypeParamBound, parent: Option[RustAst]): Unit = {
    if (!parent.isDefined) {
      return
    }

    if (typeParamBoundInstance.traitParamBound.isDefined) {
      mapParent(typeParamBoundInstance.traitParamBound, parent)
    }
    if (typeParamBoundInstance.verbatimParamBound.isDefined) {
      mapParentForTokenStream(typeParamBoundInstance.verbatimParamBound.get, parent)
    }
  }

  private def mapParentForTraitItem(traitItemInstance: TraitItem, parent: Option[RustAst]): Unit = {
    if (!parent.isDefined) {
      return
    }

    if (traitItemInstance.constTraitItem.isDefined) {
      mapParent(traitItemInstance.constTraitItem, parent)
    }
    if (traitItemInstance.fnTraitItem.isDefined) {
      mapParent(traitItemInstance.fnTraitItem, parent)
    }
    if (traitItemInstance.typeTraitItem.isDefined) {
      mapParent(traitItemInstance.typeTraitItem, parent)
    }
    if (traitItemInstance.macroTraitItem.isDefined) {
      mapParent(traitItemInstance.macroTraitItem, parent)
    }
    if (traitItemInstance.verbatimTraitItem.isDefined) {
      mapParentForTokenStream(traitItemInstance.verbatimTraitItem.get, parent)
    }
  }

  private def mapParentForMeta(metaInstance: Meta, parent: Option[RustAst]): Unit = {
    if (!parent.isDefined) {
      return
    }

    if (metaInstance.path.isDefined) {
      mapParent(metaInstance.path, parent)
    }
    if (metaInstance.list.isDefined) {
      mapParent(metaInstance.list, parent)
    }
    if (metaInstance.nameValue.isDefined) {
      mapParent(metaInstance.nameValue, parent)
    }
  }

  private def mapParentForUseTree(useTreeInstance: UseTree, parent: Option[RustAst]): Unit = {
    if (!parent.isDefined) {
      return
    }

    useTreeInstance match {
      case _: UseTreeGlob => {
        mapParent(Some(useTreeInstance.asInstanceOf[UseTreeGlob]), parent)
      }
      case useTreeNotGlob: UseTreeNotGlob => {
        if (useTreeNotGlob.pathUseTree.isDefined) {
          mapParent(useTreeNotGlob.pathUseTree, parent)
        }
        if (useTreeNotGlob.renameUseTree.isDefined) {
          mapParent(useTreeNotGlob.renameUseTree, parent)
        }
        if (useTreeNotGlob.groupUseTree.isDefined) {
          useTreeNotGlob.groupUseTree.get.foreach(group => {
            mapParentForUseTree(group, parent)
          })
        }
      }
    }
  }

  private def mapParentForPathArguments(pathArgumentsInstance: PathArguments, parent: Option[RustAst]): Unit = {
    if (!parent.isDefined) {
      return
    }

    pathArgumentsInstance match {
      case _: PathArgumentsNone => {
        mapParent(Some(pathArgumentsInstance.asInstanceOf[PathArgumentsNone]), parent)
      }
      case _: PathArgumentsNotNone => {
        if (pathArgumentsInstance.asInstanceOf[PathArgumentsNotNone].angleBracketed.isDefined) {
          mapParent(pathArgumentsInstance.asInstanceOf[PathArgumentsNotNone].angleBracketed, parent)
        }
        if (pathArgumentsInstance.asInstanceOf[PathArgumentsNotNone].parenthesized.isDefined) {
          mapParent(pathArgumentsInstance.asInstanceOf[PathArgumentsNotNone].parenthesized, parent)
        }
      }
    }
  }

  private def mapParentForGenericArgument(genericArgumentInstance: GenericArgument, parent: Option[RustAst]): Unit = {
    if (!parent.isDefined) {
      return
    }

    if (genericArgumentInstance.typeGenericArgument.isDefined) {
      mapParentForType(genericArgumentInstance.typeGenericArgument.get, parent)
    }
    if (genericArgumentInstance.constGenericArgument.isDefined) {
      mapParentForExpr(genericArgumentInstance.constGenericArgument.get, parent)
    }
    if (genericArgumentInstance.assocTypeGenericArgument.isDefined) {
      mapParent(genericArgumentInstance.assocTypeGenericArgument, parent)
    }
    if (genericArgumentInstance.assocConstGenericArgument.isDefined) {
      mapParent(genericArgumentInstance.assocConstGenericArgument, parent)
    }
    if (genericArgumentInstance.constraintGenericArgument.isDefined) {
      mapParent(genericArgumentInstance.constraintGenericArgument, parent)
    }
  }

  // This function is used for types which inherit from RustAstNode
  // For other types that not inherit from RustAstNode, use the mapParentFor... function (Ex: mapParentForItem, mapParentForVisibility, ...)
  private def mapParent(node: Option[RustAst], parent: Option[RustAst]): Unit = {
    if (!node.isDefined) {
      return
    }

    if (parent.isDefined) {
      node.get.setParent(parent.get)
      parent.get.addChild(node.get)
    }

    node.get match {
      case fileAst: FileAst => {
        fileAst.items.foreach(item => {
          mapParentForItem(item, Some(fileAst))
        })
        if (fileAst.attrs.isDefined) {
          fileAst.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(fileAst))
          })
        }
      }

      case itemConst: ItemConst => {
        if (itemConst.attrs.isDefined) {
          itemConst.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(itemConst))
          })
        }
        if (itemConst.vis.isDefined) {
          mapParentForVisibility(itemConst.vis.get, Some(itemConst))
        }
        if (itemConst.generics.isDefined) {
          mapParent(itemConst.generics, Some(itemConst))
        }
        if (itemConst.ty.isDefined) {
          mapParentForType(itemConst.ty.get, Some(itemConst))
        }
        if (itemConst.expr.isDefined) {
          mapParentForExpr(itemConst.expr.get, Some(itemConst))
        }
      }
      case itemEnum: ItemEnum => {
        if (itemEnum.attrs.isDefined) {
          itemEnum.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(itemEnum))
          })
        }
        if (itemEnum.vis.isDefined) {
          mapParentForVisibility(itemEnum.vis.get, Some(itemEnum))
        }
        if (itemEnum.generics.isDefined) {
          mapParent(itemEnum.generics, Some(itemEnum))
        }
        itemEnum.variants.foreach(variant => {
          mapParent(Some(variant), Some(itemEnum))
        })
      }
      case itemExternCrate: ItemExternCrate => {
        if (itemExternCrate.attrs.isDefined) {
          itemExternCrate.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(itemExternCrate))
          })
        }
        if (itemExternCrate.vis.isDefined) {
          mapParentForVisibility(itemExternCrate.vis.get, Some(itemExternCrate))
        }
      }
      case itemFn: ItemFn => {
        if (itemFn.attrs.isDefined) {
          itemFn.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(itemFn))
          })
        }
        if (itemFn.vis.isDefined) {
          mapParentForVisibility(itemFn.vis.get, Some(itemFn))
        }
        itemFn.stmts.foreach(stmt => {
          mapParentForStmt(stmt, Some(itemFn))
        })
        if (itemFn.abi.isDefined) {
          mapParent(itemFn.abi, Some(itemFn))
        }
        if (itemFn.generics.isDefined) {
          mapParent(itemFn.generics, Some(itemFn))
        }
        itemFn.inputs.foreach(input => {
          mapParentForFnArg(input, Some(itemFn))
        })
        if (itemFn.variadic.isDefined) {
          mapParent(itemFn.variadic, Some(itemFn))
        }
        if (itemFn.output.isDefined) {
          mapParentForReturnType(itemFn.output, Some(itemFn))
        }
      }
      case itemForeignMod: ItemForeignMod => {
        if (itemForeignMod.attrs.isDefined) {
          itemForeignMod.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(itemForeignMod))
          })
        }
        if (itemForeignMod.abi.isDefined) {
          mapParent(itemForeignMod.abi, Some(itemForeignMod))
        }
        itemForeignMod.items.foreach(item => {
          mapParentForForeignItem(item, Some(itemForeignMod))
        })
      }
      case itemImpl: ItemImpl => {
        if (itemImpl.attrs.isDefined) {
          itemImpl.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(itemImpl))
          })
        }
        if (itemImpl.generics.isDefined) {
          mapParent(itemImpl.generics, Some(itemImpl))
        }
        if (itemImpl.traitImpl.isDefined) {
          mapParent(Some(itemImpl.traitImpl.get._2), Some(itemImpl))
        }
        if (itemImpl.self_ty.isDefined) {
          mapParentForType(itemImpl.self_ty.get, Some(itemImpl))
        }
        itemImpl.items.foreach(item => {
          mapParentForImplItem(item, Some(itemImpl))
        })
      }
      case itemMacro: ItemMacro => {
        if (itemMacro.attrs.isDefined) {
          itemMacro.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(itemMacro))
          })
        }
        if (itemMacro.path.isDefined) {
          mapParent(itemMacro.path, Some(itemMacro))
        }
        if (itemMacro.delimiter.isDefined) {
          mapParent(itemMacro.delimiter, Some(itemMacro))
        }
        if (itemMacro.tokens.isDefined) {
          mapParentForTokenStream(itemMacro.tokens.get, Some(itemMacro))
        }
      }
      case itemMod: ItemMod => {
        if (itemMod.attrs.isDefined) {
          itemMod.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(itemMod))
          })
        }
        if (itemMod.vis.isDefined) {
          mapParentForVisibility(itemMod.vis.get, Some(itemMod))
        }
        itemMod.content.foreach(content => {
          content.foreach(item => {
            mapParentForItem(item, Some(itemMod))
          })
        })
      }
      case itemStatic: ItemStatic => {
        if (itemStatic.attrs.isDefined) {
          itemStatic.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(itemStatic))
          })
        }
        if (itemStatic.vis.isDefined) {
          mapParentForVisibility(itemStatic.vis.get, Some(itemStatic))
        }
        if (itemStatic.mut.isDefined) {
          mapParent(itemStatic.mut, Some(itemStatic))
        }
        if (itemStatic.ty.isDefined) {
          mapParentForType(itemStatic.ty.get, Some(itemStatic))
        }
        if (itemStatic.expr.isDefined) {
          mapParentForExpr(itemStatic.expr.get, Some(itemStatic))
        }
      }
      case itemStruct: ItemStruct => {
        if (itemStruct.attrs.isDefined) {
          itemStruct.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(itemStruct))
          })
        }
        if (itemStruct.vis.isDefined) {
          mapParentForVisibility(itemStruct.vis.get, Some(itemStruct))
        }
        if (itemStruct.generics.isDefined) {
          mapParent(itemStruct.generics, Some(itemStruct))
        }
        if (itemStruct.fields.isDefined) {
          mapParentForFields(itemStruct.fields.get, Some(itemStruct))
        }

      }
      case itemTrait: ItemTrait => {
        if (itemTrait.attrs.isDefined) {
          itemTrait.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(itemTrait))
          })
        }
        if (itemTrait.vis.isDefined) {
          mapParentForVisibility(itemTrait.vis.get, Some(itemTrait))
        }
        if (itemTrait.restriction.isDefined) {
          mapParent(itemTrait.restriction, Some(itemTrait))
        }
        if (itemTrait.generics.isDefined) {
          mapParent(itemTrait.generics, Some(itemTrait))
        }
        itemTrait.supertraits.foreach(supertrait => {
          mapParentForTypeParamBound(supertrait, Some(itemTrait))
        })
        itemTrait.items.foreach(item => {
          mapParentForTraitItem(item, Some(itemTrait))
        })
      }
      case itemTraitAlias: ItemTraitAlias => {
        if (itemTraitAlias.attrs.isDefined) {
          itemTraitAlias.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(itemTraitAlias))
          })
        }
        if (itemTraitAlias.vis.isDefined) {
          mapParentForVisibility(itemTraitAlias.vis.get, Some(itemTraitAlias))
        }
        if (itemTraitAlias.generics.isDefined) {
          mapParent(itemTraitAlias.generics, Some(itemTraitAlias))
        }
        itemTraitAlias.bounds.foreach(bound => {
          mapParentForTypeParamBound(bound, Some(itemTraitAlias))
        })
      }
      case itemType: ItemType => {
        if (itemType.attrs.isDefined) {
          itemType.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(itemType))
          })
        }
        if (itemType.vis.isDefined) {
          mapParentForVisibility(itemType.vis.get, Some(itemType))
        }
        if (itemType.generics.isDefined) {
          mapParent(itemType.generics, Some(itemType))
        }
        if (itemType.ty.isDefined) {
          mapParentForType(itemType.ty.get, Some(itemType))
        }
      }
      case itemUnion: ItemUnion => {
        if (itemUnion.attrs.isDefined) {
          itemUnion.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(itemUnion))
          })
        }
        if (itemUnion.vis.isDefined) {
          mapParentForVisibility(itemUnion.vis.get, Some(itemUnion))
        }
        if (itemUnion.generics.isDefined) {
          mapParent(itemUnion.generics, Some(itemUnion))
        }
        if (itemUnion.fields.isDefined) {
          mapParentForFieldsNamed(itemUnion.fields.get, Some(itemUnion))
        }

      }
      case itemUse: ItemUse => {
        if (itemUse.attrs.isDefined) {
          itemUse.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(itemUse))
          })
        }
        if (itemUse.vis.isDefined) {
          mapParentForVisibility(itemUse.vis.get, Some(itemUse))
        }
        if (itemUse.tree.isDefined) {
          mapParentForUseTree(itemUse.tree.get, Some(itemUse))
        }
      }

      case typeArray: TypeArray => {
        if (typeArray.elem.isDefined) {
          mapParentForType(typeArray.elem.get, Some(typeArray))
        }
        if (typeArray.len.isDefined) {
          mapParentForExpr(typeArray.len.get, Some(typeArray))
        }
      }
      case typeBareFn: TypeBareFn => {
        if (typeBareFn.lifetimes.isDefined) {
          mapParentForBoundLifetimes(typeBareFn.lifetimes.get, Some(typeBareFn))
        }
        if (typeBareFn.abi.isDefined) {
          mapParent(typeBareFn.abi, Some(typeBareFn))
        }
        typeBareFn.inputs.foreach(input => {
          mapParent(Some(input), Some(typeBareFn))
        })
        if (typeBareFn.variadic.isDefined) {
          mapParent(typeBareFn.variadic, Some(typeBareFn))
        }
        if (typeBareFn.output.isDefined) {
          mapParentForReturnType(typeBareFn.output, Some(typeBareFn))
        }
      }
      case typeGroup: TypeGroup => {
        if (typeGroup.elem.isDefined) {
          mapParentForType(typeGroup.elem.get, Some(typeGroup))
        }
      }
      case typeImplTrait: TypeImplTrait => {
        typeImplTrait.bounds.foreach(bound => {
          mapParentForTypeParamBound(bound, Some(typeImplTrait))
        })
      }
      case typeMacro: TypeMacro => {
        if (typeMacro.path.isDefined) {
          mapParent(typeMacro.path, Some(typeMacro))
        }
        if (typeMacro.delimiter.isDefined) {
          mapParent(typeMacro.delimiter, Some(typeMacro))
        }
        if (typeMacro.tokens.isDefined) {
          mapParentForTokenStream(typeMacro.tokens.get, Some(typeMacro))
        }
      }
      case typeParen: TypeParen => {
        if (typeParen.elem.isDefined) {
          mapParentForType(typeParen.elem.get, Some(typeParen))
        }
      }
      case typePath: TypePath => {
        if (typePath.qself.isDefined) {
          mapParent(typePath.qself, Some(typePath))
        }
        if (typePath.qself.isDefined) {
          mapParent(typePath.qself, Some(typePath))
        }
        typePath.segments.foreach(segment => {
          mapParent(Some(segment), Some(typePath))
        })
      }
      case typePtr: TypePtr => {
        if (typePtr.elem.isDefined) {
          mapParentForType(typePtr.elem.get, Some(typePtr))
        }
      }
      case typeReference: TypeReference => {
        if (typeReference.elem.isDefined) {
          mapParentForType(typeReference.elem.get, Some(typeReference))
        }
      }
      case typeSlice: TypeSlice => {
        if (typeSlice.elem.isDefined) {
          mapParentForType(typeSlice.elem.get, Some(typeSlice))
        }
      }
      case typeTraitObject: TypeTraitObject => {
        typeTraitObject.bounds.foreach(bound => {
          mapParentForTypeParamBound(bound, Some(typeTraitObject))
        })
      }
      case typeTuple: TypeTuple => {
        typeTuple.elems.foreach(elem => {
          mapParentForType(elem, Some(typeTuple))
        })
      }

      case exprArray: ExprArray => {
        if (exprArray.attrs.isDefined) {
          exprArray.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprArray))
          })
        }
        exprArray.elems.foreach(elem => {
          mapParentForExpr(elem, Some(exprArray))
        })
      }
      case exprAssign: ExprAssign => {
        if (exprAssign.attrs.isDefined) {
          exprAssign.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprAssign))
          })
        }
        if (exprAssign.left.isDefined) {
          mapParentForExpr(exprAssign.left.get, Some(exprAssign))
        }
        if (exprAssign.right.isDefined) {
          mapParentForExpr(exprAssign.right.get, Some(exprAssign))
        }
      }
      case exprAsync: ExprAsync => {
        if (exprAsync.attrs.isDefined) {
          exprAsync.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprAsync))
          })
        }
        exprAsync.stmts.foreach(stmt => {
          mapParentForStmt(stmt, Some(exprAsync))
        })
      }
      case exprAwait: ExprAwait => {
        if (exprAwait.attrs.isDefined) {
          exprAwait.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprAwait))
          })
        }
        if (exprAwait.base.isDefined) {
          mapParentForExpr(exprAwait.base.get, Some(exprAwait))
        }
      }
      case exprBinary: ExprBinary => {
        if (exprBinary.attrs.isDefined) {
          exprBinary.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprBinary))
          })
        }
        if (exprBinary.left.isDefined) {
          mapParentForExpr(exprBinary.left.get, Some(exprBinary))
        }
        if (exprBinary.op.isDefined) {
          mapParent(exprBinary.op, Some(exprBinary))
        }
        if (exprBinary.right.isDefined) {
          mapParentForExpr(exprBinary.right.get, Some(exprBinary))
        }
      }
      case exprBlock: ExprBlock => {
        if (exprBlock.attrs.isDefined) {
          exprBlock.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprBlock))
          })
        }
        exprBlock.stmts.foreach(stmt => {
          mapParentForStmt(stmt, Some(exprBlock))
        })
      }
      case exprBreak: ExprBreak => {
        if (exprBreak.attrs.isDefined) {
          exprBreak.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprBreak))
          })
        }
        if (exprBreak.expr.isDefined) {
          mapParentForExpr(exprBreak.expr.get, Some(exprBreak))
        }
      }
      case exprCall: ExprCall => {
        if (exprCall.attrs.isDefined) {
          exprCall.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprCall))
          })
        }
        if (exprCall.func.isDefined) {
          mapParentForExpr(exprCall.func.get, Some(exprCall))
        }
        exprCall.args.foreach(arg => {
          mapParentForExpr(arg, Some(exprCall))
        })
      }
      case exprCast: ExprCast => {
        if (exprCast.attrs.isDefined) {
          exprCast.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprCast))
          })
        }
        if (exprCast.expr.isDefined) {
          mapParentForExpr(exprCast.expr.get, Some(exprCast))
        }
        if (exprCast.ty.isDefined) {
          mapParentForType(exprCast.ty.get, Some(exprCast))
        }
      }
      case exprClosure: ExprClosure => {
        if (exprClosure.attrs.isDefined) {
          exprClosure.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprClosure))
          })
        }
        if (exprClosure.lifetimes.isDefined) {
          mapParentForBoundLifetimes(exprClosure.lifetimes.get, Some(exprClosure))
        }
        exprClosure.inputs.foreach(input => {
          mapParentForPat(input, Some(exprClosure))
        })
        if (exprClosure.output.isDefined) {
          mapParentForReturnType(exprClosure.output, Some(exprClosure))
        }
        if (exprClosure.body.isDefined) {
          mapParentForExpr(exprClosure.body.get, Some(exprClosure))
        }
      }
      case exprConst: ExprConst => {
        if (exprConst.attrs.isDefined) {
          exprConst.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprConst))
          })
        }
        exprConst.stmts.foreach(stmt => {
          mapParentForStmt(stmt, Some(exprConst))
        })
      }
      case exprContinue: ExprContinue => {
        if (exprContinue.attrs.isDefined) {
          exprContinue.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprContinue))
          })
        }
      }

      case exprField: ExprField => {
        if (exprField.attrs.isDefined) {
          exprField.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprField))
          })
        }
        if (exprField.base.isDefined) {
          mapParentForExpr(exprField.base.get, Some(exprField))
        }
      }

      case exprForLoop: ExprForLoop => {
        if (exprForLoop.attrs.isDefined) {
          exprForLoop.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprForLoop))
          })
        }
        if (exprForLoop.pat.isDefined) {
          mapParentForPat(exprForLoop.pat.get, Some(exprForLoop))
        }
        if (exprForLoop.expr.isDefined) {
          mapParentForExpr(exprForLoop.expr.get, Some(exprForLoop))
        }
        exprForLoop.body.foreach(stmt => {
          mapParentForStmt(stmt, Some(exprForLoop))
        })
      }
      case exprGroup: ExprGroup => {
        if (exprGroup.attrs.isDefined) {
          exprGroup.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprGroup))
          })
        }
        if (exprGroup.expr.isDefined) {
          mapParentForExpr(exprGroup.expr.get, Some(exprGroup))
        }
      }
      case exprIf: ExprIf => {
        if (exprIf.attrs.isDefined) {
          exprIf.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprIf))
          })
        }
        if (exprIf.cond.isDefined) {
          mapParentForExpr(exprIf.cond.get, Some(exprIf))
        }
        exprIf.then_branch.foreach(stmt => {
          mapParentForStmt(stmt, Some(exprIf))
        })
        if (exprIf.else_branch.isDefined) {
          mapParentForExpr(exprIf.else_branch.get, Some(exprIf))
        }
      }
      case exprIndex: ExprIndex => {
        if (exprIndex.attrs.isDefined) {
          exprIndex.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprIndex))
          })
        }
        if (exprIndex.expr.isDefined) {
          mapParentForExpr(exprIndex.expr.get, Some(exprIndex))
        }
        if (exprIndex.index.isDefined) {
          mapParentForExpr(exprIndex.index.get, Some(exprIndex))
        }
      }
      case exprInfer: ExprInfer => {
        if (exprInfer.attrs.isDefined) {
          exprInfer.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprInfer))
          })
        }
      }
      case exprLet: ExprLet => {
        if (exprLet.attrs.isDefined) {
          exprLet.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprLet))
          })
        }
        if (exprLet.pat.isDefined) {
          mapParentForPat(exprLet.pat.get, Some(exprLet))
        }
        if (exprLet.expr.isDefined) {
          mapParentForExpr(exprLet.expr.get, Some(exprLet))
        }
      }

      case exprLit: ExprLit => {
        if (exprLit.attrs.isDefined) {
          exprLit.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprLit))
          })
        }
      }
      case exprLoop: ExprLoop => {
        if (exprLoop.attrs.isDefined) {
          exprLoop.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprLoop))
          })
        }
        exprLoop.body.foreach(stmt => {
          mapParentForStmt(stmt, Some(exprLoop))
        })
      }
      case exprMacro: ExprMacro => {
        if (exprMacro.attrs.isDefined) {
          exprMacro.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprMacro))
          })
        }
        if (exprMacro.path.isDefined) {
          mapParent(exprMacro.path, Some(exprMacro))
        }
        if (exprMacro.delimiter.isDefined) {
          mapParent(exprMacro.delimiter, Some(exprMacro))
        }
        if (exprMacro.tokens.isDefined) {
          mapParentForTokenStream(exprMacro.tokens.get, Some(exprMacro))
        }
      }
      case exprMatch: ExprMatch => {
        if (exprMatch.attrs.isDefined) {
          exprMatch.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprMatch))
          })
        }
        if (exprMatch.expr.isDefined) {
          mapParentForExpr(exprMatch.expr.get, Some(exprMatch))
        }
        exprMatch.arms.foreach(arm => {
          mapParent(Some(arm), Some(exprMatch))
        })
      }
      case exprMethodCall: ExprMethodCall => {
        if (exprMethodCall.attrs.isDefined) {
          exprMethodCall.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprMethodCall))
          })
        }
        if (exprMethodCall.receiver.isDefined) {
          mapParentForExpr(exprMethodCall.receiver.get, Some(exprMethodCall))
        }
        exprMethodCall.args.foreach(arg => {
          mapParentForExpr(arg, Some(exprMethodCall))
        })
      }
      case exprParen: ExprParen => {
        if (exprParen.attrs.isDefined) {
          exprParen.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprParen))
          })
        }
        if (exprParen.expr.isDefined) {
          mapParentForExpr(exprParen.expr.get, Some(exprParen))
        }
      }
      case exprPath: ExprPath => {
        if (exprPath.attrs.isDefined) {
          exprPath.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprPath))
          })
        }
        if (exprPath.qself.isDefined) {
          mapParent(exprPath.qself, Some(exprPath))
        }
        exprPath.segments.foreach(segment => {
          mapParent(Some(segment), Some(exprPath))
        })
      }
      case exprRange: ExprRange => {
        if (exprRange.attrs.isDefined) {
          exprRange.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprRange))
          })
        }
        if (exprRange.start.isDefined) {
          mapParentForExpr(exprRange.start.get, Some(exprRange))
        }
        if (exprRange.end.isDefined) {
          mapParentForExpr(exprRange.end.get, Some(exprRange))
        }
      }
      case exprReference: ExprReference => {
        if (exprReference.attrs.isDefined) {
          exprReference.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprReference))
          })
        }
        if (exprReference.expr.isDefined) {
          mapParentForExpr(exprReference.expr.get, Some(exprReference))
        }
      }
      case exprRepeat: ExprRepeat => {
        if (exprRepeat.attrs.isDefined) {
          exprRepeat.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprRepeat))
          })
        }
        if (exprRepeat.expr.isDefined) {
          mapParentForExpr(exprRepeat.expr.get, Some(exprRepeat))
        }
        if (exprRepeat.len.isDefined) {
          mapParentForExpr(exprRepeat.len.get, Some(exprRepeat))
        }
      }
      case exprReturn: ExprReturn => {
        if (exprReturn.attrs.isDefined) {
          exprReturn.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprReturn))
          })
        }
        if (exprReturn.expr.isDefined) {
          mapParentForExpr(exprReturn.expr.get, Some(exprReturn))
        }
      }
      case exprStruct: ExprStruct => {
        if (exprStruct.attrs.isDefined) {
          exprStruct.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprStruct))
          })
        }
        if (exprStruct.qself.isDefined) {
          mapParent(exprStruct.qself, Some(exprStruct))
        }
        if (exprStruct.path.isDefined) {
          mapParent(exprStruct.path, Some(exprStruct))
        }
        exprStruct.fields.foreach(field => {
          mapParent(Some(field), Some(exprStruct))
        })
        if (exprStruct.rest.isDefined) {
          mapParentForExpr(exprStruct.rest.get, Some(exprStruct))
        }
      }
      case exprTry: ExprTry => {
        if (exprTry.attrs.isDefined) {
          exprTry.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprTry))
          })
        }
        if (exprTry.expr.isDefined) {
          mapParentForExpr(exprTry.expr.get, Some(exprTry))
        }
      }
      case exprTryBlock: ExprTryBlock => {
        if (exprTryBlock.attrs.isDefined) {
          exprTryBlock.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprTryBlock))
          })
        }
        exprTryBlock.stmts.foreach(stmt => {
          mapParentForStmt(stmt, Some(exprTryBlock))
        })
      }
      case exprTuple: ExprTuple => {
        if (exprTuple.attrs.isDefined) {
          exprTuple.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprTuple))
          })
        }
        exprTuple.elems.foreach(elem => {
          mapParentForExpr(elem, Some(exprTuple))
        })
      }
      case exprUnary: ExprUnary => {
        if (exprUnary.attrs.isDefined) {
          exprUnary.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprUnary))
          })
        }
        if (exprUnary.op.isDefined) {
          mapParent(exprUnary.op, Some(exprUnary))
        }
        if (exprUnary.expr.isDefined) {
          mapParentForExpr(exprUnary.expr.get, Some(exprUnary))
        }
      }
      case exprUnsafe: ExprUnsafe => {
        if (exprUnsafe.attrs.isDefined) {
          exprUnsafe.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprUnsafe))
          })
        }
        exprUnsafe.stmts.foreach(stmt => {
          mapParentForStmt(stmt, Some(exprUnsafe))
        })
      }
      case exprWhile: ExprWhile => {
        if (exprWhile.attrs.isDefined) {
          exprWhile.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprWhile))
          })
        }
        if (exprWhile.cond.isDefined) {
          mapParentForExpr(exprWhile.cond.get, Some(exprWhile))
        }
        exprWhile.body.foreach(stmt => {
          mapParentForStmt(stmt, Some(exprWhile))
        })
      }
      case exprYield: ExprYield => {
        if (exprYield.attrs.isDefined) {
          exprYield.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(exprYield))
          })
        }
        if (exprYield.expr.isDefined) {
          mapParentForExpr(exprYield.expr.get, Some(exprYield))
        }
      }

      case patIdent: PatIdent => {
        if (patIdent.attrs.isDefined) {
          patIdent.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(patIdent))
          })
        }
        if (patIdent.subpat.isDefined) {
          mapParentForPat(patIdent.subpat.get, Some(patIdent))
        }
      }
      case patOr: PatOr => {
        if (patOr.attrs.isDefined) {
          patOr.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(patOr))
          })
        }
        patOr.cases.foreach(casePat => {
          mapParentForPat(casePat, Some(patOr))
        })
      }
      case patParen: PatParen => {
        if (patParen.attrs.isDefined) {
          patParen.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(patParen))
          })
        }
        if (patParen.pat.isDefined) {
          mapParentForPat(patParen.pat.get, Some(patParen))
        }
      }
      case patReference: PatReference => {
        if (patReference.attrs.isDefined) {
          patReference.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(patReference))
          })
        }
        if (patReference.pat.isDefined) {
          mapParentForPat(patReference.pat.get, Some(patReference))
        }
      }
      case patRest: PatRest => {
        if (patRest.attrs.isDefined) {
          patRest.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(patRest))
          })
        }
      }
      case patSlice: PatSlice => {
        if (patSlice.attrs.isDefined) {
          patSlice.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(patSlice))
          })
        }
        patSlice.elems.foreach(elem => {
          mapParentForPat(elem, Some(patSlice))
        })
      }
      case patStruct: PatStruct => {
        if (patStruct.attrs.isDefined) {
          patStruct.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(patStruct))
          })
        }
        if (patStruct.qself.isDefined) {
          mapParent(patStruct.qself, Some(patStruct))
        }
        if (patStruct.path.isDefined) {
          mapParent(patStruct.path, Some(patStruct))
        }
        patStruct.fields.foreach(field => {
          mapParent(Some(field), Some(patStruct))
        })
        if (patStruct.rest.isDefined) {
          mapParent(patStruct.rest, Some(patStruct))
        }
      }
      case patTuple: PatTuple => {
        if (patTuple.attrs.isDefined) {
          patTuple.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(patTuple))
          })
        }
        patTuple.elems.foreach(elem => {
          mapParentForPat(elem, Some(patTuple))
        })
      }
      case patTupleStruct: PatTupleStruct => {
        if (patTupleStruct.attrs.isDefined) {
          patTupleStruct.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(patTupleStruct))
          })
        }
        if (patTupleStruct.qself.isDefined) {
          mapParent(patTupleStruct.qself, Some(patTupleStruct))
        }
        if (patTupleStruct.path.isDefined) {
          mapParent(patTupleStruct.path, Some(patTupleStruct))
        }
        patTupleStruct.elems.foreach(elem => {
          mapParentForPat(elem, Some(patTupleStruct))
        })
      }
      case patType: PatType => {
        if (patType.attrs.isDefined) {
          patType.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(patType))
          })
        }
        if (patType.pat.isDefined) {
          mapParentForPat(patType.pat.get, Some(patType))
        }
        if (patType.ty.isDefined) {
          mapParentForType(patType.ty.get, Some(patType))
        }
      }
      case patWild: PatWild => {
        if (patWild.attrs.isDefined) {
          patWild.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(patWild))
          })
        }
      }

      case variant: Variant => {
        if (variant.attrs.isDefined) {
          variant.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(variant))
          })
        }
        if (variant.fields.isDefined) {
          mapParentForFields(variant.fields.get, Some(variant))
        }
        if (variant.discriminant.isDefined) {
          mapParentForExpr(variant.discriminant.get, Some(variant))
        }
      }
      case variadic: Variadic => {
        if (variadic.attrs.isDefined) {
          variadic.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(variadic))
          })
        }
        if (variadic.pat.isDefined) {
          mapParentForPat(variadic.pat.get, Some(variadic))
        }
      }

      case local: Local => {
        if (local.attrs.isDefined) {
          local.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(local))
          })
        }
        if (local.pat.isDefined) {
          mapParentForPat(local.pat.get, Some(local))
        }
        if (local.init.isDefined) {
          mapParent(local.init, Some(local))
        }
      }
      case stmtMacro: StmtMacro => {
        if (stmtMacro.attrs.isDefined) {
          stmtMacro.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(stmtMacro))
          })
        }
        if (stmtMacro.path.isDefined) {
          mapParent(stmtMacro.path, Some(stmtMacro))
        }
        if (stmtMacro.delimiter.isDefined) {
          mapParent(stmtMacro.delimiter, Some(stmtMacro))
        }
        if (stmtMacro.tokens.isDefined) {
          mapParentForTokenStream(stmtMacro.tokens.get, Some(stmtMacro))
        }
      }
      case localInit: LocalInit => {
        if (localInit.expr.isDefined) {
          mapParentForExpr(localInit.expr.get, Some(localInit))
        }
        if (localInit.diverge.isDefined) {
          mapParentForExpr(localInit.diverge.get, Some(localInit))
        }
      }

      case receiver: Receiver => {
        if (receiver.attrs.isDefined) {
          receiver.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(receiver))
          })
        }
        if (receiver.ty.isDefined) {
          mapParentForType(receiver.ty.get, Some(receiver))
        }
      }
      case bareFnArg: BareFnArg => {
        if (bareFnArg.attrs.isDefined) {
          bareFnArg.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(bareFnArg))
          })
        }
        if (bareFnArg.ty.isDefined) {
          mapParentForType(bareFnArg.ty.get, Some(bareFnArg))
        }
      }
      case bareVariadic: BareVariadic => {
        if (bareVariadic.attrs.isDefined) {
          bareVariadic.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(bareVariadic))
          })
        }
      }

      case lifetimeParam: LifetimeParam => {
        if (lifetimeParam.attrs.isDefined) {
          lifetimeParam.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(lifetimeParam))
          })
        }
      }
      case typeParam: TypeParam => {
        if (typeParam.attrs.isDefined) {
          typeParam.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(typeParam))
          })
        }
        typeParam.bounds.foreach(bound => {
          mapParentForTypeParamBound(bound, Some(typeParam))
        })
        if (typeParam.default.isDefined) {
          mapParentForType(typeParam.default.get, Some(typeParam))
        }
      }
      case constParam: ConstParam => {
        if (constParam.attrs.isDefined) {
          constParam.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(constParam))
          })
        }
        if (constParam.ty.isDefined) {
          mapParentForType(constParam.ty.get, Some(constParam))
        }
        if (constParam.default.isDefined) {
          mapParentForExpr(constParam.default.get, Some(constParam))
        }
      }

      case predicateType: PredicateType => {
        if (predicateType.lifetimes.isDefined) {
          mapParentForBoundLifetimes(predicateType.lifetimes.get, Some(predicateType))
        }
        if (predicateType.bounded_ty.isDefined) {
          mapParentForType(predicateType.bounded_ty.get, Some(predicateType))
        }
        predicateType.bounds.foreach(bound => {
          mapParentForTypeParamBound(bound, Some(predicateType))
        })
      }

      case group: Group => {
        if (group.delimiter.isDefined) {
          mapParent(group.delimiter, Some(group))
        }
        if (group.stream.isDefined) {
          mapParentForTokenStream(group.stream.get, Some(group))
        }
      }
      case punct: Punct => {
        if (punct.spacing.isDefined) {
          mapParent(punct.spacing, Some(punct))
        }
      }

      case attribute: Attribute => {
        if (attribute.style.isDefined) {
          mapParent(attribute.style, Some(attribute))
        }
        if (attribute.meta.isDefined) {
          mapParentForMeta(attribute.meta.get, Some(attribute))
        }
      }

      case foreignItemFn: ForeignItemFn => {
        if (foreignItemFn.attrs.isDefined) {
          foreignItemFn.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(foreignItemFn))
          })
        }
        if (foreignItemFn.vis.isDefined) {
          mapParentForVisibility(foreignItemFn.vis.get, Some(foreignItemFn))
        }
        if (foreignItemFn.abi.isDefined) {
          mapParent(foreignItemFn.abi, Some(foreignItemFn))
        }
        if (foreignItemFn.generics.isDefined) {
          mapParent(foreignItemFn.generics, Some(foreignItemFn))
        }
        foreignItemFn.inputs.foreach(input => {
          mapParentForFnArg(input, Some(foreignItemFn))
        })
        if (foreignItemFn.variadic.isDefined) {
          mapParent(foreignItemFn.variadic, Some(foreignItemFn))
        }
        if (foreignItemFn.output.isDefined) {
          mapParentForReturnType(foreignItemFn.output, Some(foreignItemFn))
        }

      }
      case foreignItemStatic: ForeignItemStatic => {
        if (foreignItemStatic.attrs.isDefined) {
          foreignItemStatic.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(foreignItemStatic))
          })
        }
        if (foreignItemStatic.vis.isDefined) {
          mapParentForVisibility(foreignItemStatic.vis.get, Some(foreignItemStatic))
        }
        if (foreignItemStatic.mut.isDefined) {
          mapParent(foreignItemStatic.mut, Some(foreignItemStatic))
        }
        if (foreignItemStatic.ty.isDefined) {
          mapParentForType(foreignItemStatic.ty.get, Some(foreignItemStatic))
        }
      }
      case foreignItemType: ForeignItemType => {
        if (foreignItemType.attrs.isDefined) {
          foreignItemType.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(foreignItemType))
          })
        }
        if (foreignItemType.vis.isDefined) {
          mapParentForVisibility(foreignItemType.vis.get, Some(foreignItemType))
        }
        if (foreignItemType.generics.isDefined) {
          mapParent(foreignItemType.generics, Some(foreignItemType))
        }
      }
      case foreignItemMacro: ForeignItemMacro => {
        if (foreignItemMacro.attrs.isDefined) {
          foreignItemMacro.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(foreignItemMacro))
          })
        }
        if (foreignItemMacro.path.isDefined) {
          mapParent(foreignItemMacro.path, Some(foreignItemMacro))
        }
        if (foreignItemMacro.delimiter.isDefined) {
          mapParent(foreignItemMacro.delimiter, Some(foreignItemMacro))
        }
        if (foreignItemMacro.tokens.isDefined) {
          mapParentForTokenStream(foreignItemMacro.tokens.get, Some(foreignItemMacro))
        }
      }

      case traitBound: TraitBound =>
        if (traitBound.modifier.isDefined) {
          mapParent(traitBound.modifier, Some(traitBound))
        }
        if (traitBound.lifetimes.isDefined) {
          mapParentForBoundLifetimes(traitBound.lifetimes.get, Some(traitBound))
        }
        if (traitBound.path.isDefined) {
          mapParent(traitBound.path, Some(traitBound))
        }
      case _: TraitBoundModifier => {}

      case traitItemConst: TraitItemConst => {
        if (traitItemConst.attrs.isDefined) {
          traitItemConst.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(traitItemConst))
          })
        }
        if (traitItemConst.generics.isDefined) {
          mapParent(traitItemConst.generics, Some(traitItemConst))
        }
        if (traitItemConst.ty.isDefined) {
          mapParentForType(traitItemConst.ty.get, Some(traitItemConst))
        }
        if (traitItemConst.default.isDefined) {
          mapParentForExpr(traitItemConst.default.get, Some(traitItemConst))
        }
      }
      case traitItemFn: TraitItemFn => {
        if (traitItemFn.attrs.isDefined) {
          traitItemFn.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(traitItemFn))
          })
        }
        if (traitItemFn.default.isDefined) {
          traitItemFn.default.get.foreach(stmt => {
            mapParentForStmt(stmt, Some(traitItemFn))
          })
        }
        if (traitItemFn.abi.isDefined) {
          mapParent(traitItemFn.abi, Some(traitItemFn))
        }
        if (traitItemFn.generics.isDefined) {
          mapParent(traitItemFn.generics, Some(traitItemFn))
        }
        traitItemFn.inputs.foreach(input => {
          mapParentForFnArg(input, Some(traitItemFn))
        })
        if (traitItemFn.variadic.isDefined) {
          mapParent(traitItemFn.variadic, Some(traitItemFn))
        }
        if (traitItemFn.output.isDefined) {
          mapParentForReturnType(traitItemFn.output, Some(traitItemFn))
        }
      }
      case traitItemType: TraitItemType => {
        if (traitItemType.attrs.isDefined) {
          traitItemType.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(traitItemType))
          })
        }
        if (traitItemType.generics.isDefined) {
          mapParent(traitItemType.generics, Some(traitItemType))
        }
        traitItemType.bounds.foreach(bound => {
          mapParentForTypeParamBound(bound, Some(traitItemType))
        })
        if (traitItemType.default.isDefined) {
          mapParentForType(traitItemType.default.get, Some(traitItemType))
        }
      }
      case traitItemMacro: TraitItemMacro => {
        if (traitItemMacro.attrs.isDefined) {
          traitItemMacro.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(traitItemMacro))
          })
        }
        if (traitItemMacro.path.isDefined) {
          mapParent(traitItemMacro.path, Some(traitItemMacro))
        }
        if (traitItemMacro.delimiter.isDefined) {
          mapParent(traitItemMacro.delimiter, Some(traitItemMacro))
        }
        if (traitItemMacro.tokens.isDefined) {
          mapParentForTokenStream(traitItemMacro.tokens.get, Some(traitItemMacro))
        }
      }

      case usePath: UsePath => {
        if (usePath.tree.isDefined) {
          mapParentForUseTree(usePath.tree.get, Some(usePath))
        }
      }

      case path: Path => {
        path.segments.foreach(segment => {
          mapParent(Some(segment), Some(path))
        })
      }

      case pathSegment: PathSegment => {
        if (pathSegment.arguments.isDefined) {
          mapParentForPathArguments(pathSegment.arguments.get, Some(pathSegment))
        }
      }

      case metaList: MetaList => {
        if (metaList.path.isDefined) {
          mapParent(metaList.path, Some(metaList))
        }
        if (metaList.delimiter.isDefined) {
          mapParent(metaList.delimiter, Some(metaList))
        }
        if (metaList.tokens.isDefined) {
          mapParentForTokenStream(metaList.tokens.get, Some(metaList))
        }
      }

      case metaNameValue: MetaNameValue => {
        if (metaNameValue.path.isDefined) {
          mapParent(metaNameValue.path, Some(metaNameValue))
        }
        if (metaNameValue.value.isDefined) {
          mapParentForExpr(metaNameValue.value.get, Some(metaNameValue))
        }
      }

      case qSelf: QSelf => {
        if (qSelf.ty.isDefined) {
          mapParentForType(qSelf.ty.get, Some(qSelf))
        }
      }

      case arm: Arm => {
        if (arm.attrs.isDefined) {
          arm.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(arm))
          })
        }
        if (arm.pat.isDefined) {
          mapParentForPat(arm.pat.get, Some(arm))
        }
        if (arm.guard.isDefined) {
          mapParentForExpr(arm.guard.get, Some(arm))
        }
        if (arm.body.isDefined) {
          mapParentForExpr(arm.body.get, Some(arm))
        }
      }

      case implItemConst: ImplItemConst =>
        if (implItemConst.attrs.isDefined) {
          implItemConst.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(implItemConst))
          })
        }
        if (implItemConst.vis.isDefined) {
          mapParentForVisibility(implItemConst.vis.get, Some(implItemConst))
        }
        if (implItemConst.generics.isDefined) {
          mapParent(implItemConst.generics, Some(implItemConst))
        }
        if (implItemConst.ty.isDefined) {
          mapParentForType(implItemConst.ty.get, Some(implItemConst))
        }
        if (implItemConst.expr.isDefined) {
          mapParentForExpr(implItemConst.expr.get, Some(implItemConst))
        }

      case implItemFn: ImplItemFn =>
        if (implItemFn.attrs.isDefined) {
          implItemFn.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(implItemFn))
          })
        }
        if (implItemFn.vis.isDefined) {
          mapParentForVisibility(implItemFn.vis.get, Some(implItemFn))
        }
        implItemFn.stmts.foreach(stmt => {
          mapParentForStmt(stmt, Some(implItemFn))
        })
        if (implItemFn.abi.isDefined) {
          mapParent(implItemFn.abi, Some(implItemFn))
        }
        if (implItemFn.generics.isDefined) {
          mapParent(implItemFn.generics, Some(implItemFn))
        }
        implItemFn.inputs.foreach(input => {
          mapParentForFnArg(input, Some(implItemFn))
        })
        if (implItemFn.variadic.isDefined) {
          mapParent(implItemFn.variadic, Some(implItemFn))
        }
        if (implItemFn.output.isDefined) {
          mapParentForReturnType(implItemFn.output, Some(implItemFn))
        }

      case implItemType: ImplItemType =>
        if (implItemType.attrs.isDefined) {
          implItemType.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(implItemType))
          })
        }
        if (implItemType.vis.isDefined) {
          mapParentForVisibility(implItemType.vis.get, Some(implItemType))
        }
        if (implItemType.generics.isDefined) {
          mapParent(implItemType.generics, Some(implItemType))
        }
        if (implItemType.ty.isDefined) {
          mapParentForType(implItemType.ty.get, Some(implItemType))
        }

      case implItemMacro: ImplItemMacro => {
        if (implItemMacro.attrs.isDefined) {
          implItemMacro.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(implItemMacro))
          })
        }
        if (implItemMacro.path.isDefined) {
          mapParent(implItemMacro.path, Some(implItemMacro))
        }
        if (implItemMacro.delimiter.isDefined) {
          mapParent(implItemMacro.delimiter, Some(implItemMacro))
        }
        if (implItemMacro.tokens.isDefined) {
          mapParentForTokenStream(implItemMacro.tokens.get, Some(implItemMacro))
        }
      }

      case angleBracketedGenericArguments: AngleBracketedGenericArguments => {
        angleBracketedGenericArguments.args.foreach(arg => {
          mapParentForGenericArgument(arg, Some(angleBracketedGenericArguments))
        })
      }
      case parenthesizedGenericArguments: ParenthesizedGenericArguments => {
        parenthesizedGenericArguments.inputs.foreach(input => {
          mapParentForType(input, Some(parenthesizedGenericArguments))
        })
        if (parenthesizedGenericArguments.output.isDefined) {
          mapParentForReturnType(parenthesizedGenericArguments.output, Some(parenthesizedGenericArguments))
        }
      }

      case fieldPat: FieldPat => {
        if (fieldPat.attrs.isDefined) {
          fieldPat.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(fieldPat))
          })
        }
        if (fieldPat.pat.isDefined) {
          mapParentForPat(fieldPat.pat.get, Some(fieldPat))
        }
      }
      case fieldValue: FieldValue => {
        if (fieldValue.attrs.isDefined) {
          fieldValue.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(fieldValue))
          })
        }
        if (fieldValue.expr.isDefined) {
          mapParentForExpr(fieldValue.expr.get, Some(fieldValue))
        }
      }

      case field: Field => {
        if (field.attrs.isDefined) {
          field.attrs.get.foreach(attr => {
            mapParent(Some(attr), Some(field))
          })
        }
        if (field.vis.isDefined) {
          mapParentForVisibility(field.vis.get, Some(field))
        }
        if (field.mut.isDefined) {
          mapParent(field.mut, Some(field))
        }
        if (field.ty.isDefined) {
          mapParentForType(field.ty.get, Some(field))
        }
      }

      case generics: Generics => {
        generics.params.foreach(params => {
          params.foreach(param => {
            mapParentForGenericParam(param, Some(generics))
          })
        })
        generics.whereClause.foreach(whereClause => {
          whereClause.foreach(wherePredicate => {
            mapParentForWherePredicate(wherePredicate, Some(generics))
          })
        })
      }

      case assocType: AssocType => {
        if (assocType.generics.isDefined) {
          mapParent(assocType.generics, Some(assocType))
        }
        if (assocType.ty.isDefined) {
          mapParentForType(assocType.ty.get, Some(assocType))
        }
      }
      case assocConst: AssocConst => {
        if (assocConst.generics.isDefined) {
          mapParent(assocConst.generics, Some(assocConst))
        }
        if (assocConst.value.isDefined) {
          mapParentForExpr(assocConst.value.get, Some(assocConst))
        }
      }
      case constraint: Constraint => {
        if (constraint.generics.isDefined) {
          mapParent(constraint.generics, Some(constraint))
        }
        constraint.bounds.foreach(bound => {
          mapParentForTypeParamBound(bound, Some(constraint))
        })
      }

      case visRestricted: VisibilityRestricted => {
        if (visRestricted.path.isDefined) {
          mapParent(visRestricted.path, Some(visRestricted))
        }
      }

      // Rust AST nodes have children but each children do not need to have parent
      case abi: Abi                             => {}
      case useRename: UseRename                 => {}
      case predicateLifetime: PredicateLifetime => {}
      case implRestriction: ImplRestriction     => {}

      // Rust AST nodes which is enum type
      case _: TypeNoValue       => {}
      case _: UseTreeGlob       => {}
      case _: BinOp             => {}
      case _: UnOp              => {}
      case _: AttrStyle         => {}
      case _: Delimiter         => {}
      case _: Spacing           => {}
      case _: MacroDelimiter    => {}
      case _: StaticMutability  => {}
      case _: FieldsUnit        => {}
      case _: VisibilityString  => {}
      case _: RangeLimits       => {}
      case _: PathArgumentsNone => {}

      case unknown => throw new IllegalArgumentException(s"Unknown node type: ${unknown.getClass.getSimpleName}")
    }
  }
}
