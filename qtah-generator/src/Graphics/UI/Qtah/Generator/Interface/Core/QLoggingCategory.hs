module Graphics.UI.Qtah.Generator.Interface.Core.QLoggingCategory (
  aModule,
  c_QLoggingCategory,
  categoryFilter,
  ) where


import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
  Operator (OpCall),
  Type,
  addReqIncludes,
  classSetEntityPrefix,
  ident,
  ident1,
  ident2,
  includeLocal,
  includeStd,
  makeClass,
  makeFnMethod,
  mkConstMethod,
  mkConstMethod',
  mkStaticMethod,
  mkStaticMethod',
  mkCtor,
  mkMethod',
  mkMethod
  )
  
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Foreign.Hoppy.Generator.Types (charT, voidT, boolT, enumT, bitspaceT, constT, objT, ptrT, refT, fnT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types
import Graphics.UI.Qtah.Generator.Interface.Core.Types (e_QtMsgType)


{-# ANN module "HLint: ignore Use camelCase" #-}



aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QLoggingCategory"] [5, 2] $
  [QtExport $ ExportClass c_QLoggingCategory]
  

c_QLoggingCategory =
  addReqIncludes [ includeStd "QLoggingCategory" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QLoggingCategory") Nothing [] $
  collect
  [ test (qtVersion >= [5, 4]) $ mkCtor "newWithMsgType" [ptrT $ constT charT, enumT e_QtMsgType]
  , just $ mkCtor "new" [ptrT $ constT charT]
  , just $ mkConstMethod "categoryName" [] $ ptrT $ constT charT
  , just $ mkStaticMethod "defaultCategory" [] $ ptrT $ objT c_QLoggingCategory
  , just $ mkStaticMethod "installFilter" [categoryFilter] categoryFilter
  , just $ mkConstMethod "isCriticalEnabled" [] boolT
  , just $ mkConstMethod "isDebugEnabled" [] boolT
  , just $ mkConstMethod "isEnabled" [enumT e_QtMsgType] boolT
  , test (qtVersion >= [5, 5]) $ mkConstMethod "isInfoEnabled" [] boolT
  , just $ mkConstMethod "isWarningEnabled" [] boolT
  , just $ mkMethod "setEnabled" [enumT e_QtMsgType, boolT] voidT
  , just $ mkStaticMethod "setFilterRules" [refT $ constT $ objT c_QString] voidT
  , just $ mkMethod' OpCall "call" [] $ refT $ objT c_QLoggingCategory
  , just $ mkConstMethod' OpCall "callConst" [] $ refT $ constT $ objT c_QLoggingCategory
  ]
  

categoryFilter :: Type
categoryFilter = ptrT $ fnT [ptrT $ objT c_QLoggingCategory] voidT  


