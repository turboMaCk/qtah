module Graphics.UI.Qtah.Generator.Interface.Core.QFileSelector (
  aModule,
  c_QFileSelector,
  ) where



import Foreign.Hoppy.Generator.Spec (
  Export (ExportClass),
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
  

import Graphics.UI.Qtah.Generator.Interface.Core.QObject (c_QObject)
import Foreign.Hoppy.Generator.Types (voidT, enumT, bitspaceT, constT, objT, ptrT, refT)
import Foreign.Hoppy.Generator.Version (collect, just, test)
import Graphics.UI.Qtah.Generator.Flags (qtVersion)
import Graphics.UI.Qtah.Generator.Interface.Core.QString (c_QString)
import Graphics.UI.Qtah.Generator.Interface.Core.QStringList (c_QStringList)
import Graphics.UI.Qtah.Generator.Module (AModule (AQtModule), makeQtModuleWithMinVersion)
import Graphics.UI.Qtah.Generator.Types


{-# ANN module "HLint: ignore Use camelCase" #-}


aModule =
  AQtModule $
  makeQtModuleWithMinVersion ["Core", "QFileSelector"] [5, 2] $
  [QtExport $ ExportClass c_QFileSelector]
  

c_QFileSelector =
  addReqIncludes [ includeStd "QFileSelector" ] $
  classSetEntityPrefix "" $
  makeClass (ident "QFileSelector") Nothing [c_QObject] $
  collect
  [ just $ mkCtor "new" []
  , just $ mkCtor "newWithParent" [ptrT $ objT c_QObject]
  , just $ mkConstMethod "allSelectors" [] $ objT c_QStringList
  , just $ mkConstMethod "extraSelectors" [] $ objT c_QStringList
  , just $ mkConstMethod "select" [refT $ constT $ objT c_QString] $ objT c_QString
  -- TODO QUrl QFileSelector::select(const QUrl &filePath) const
  , just $ mkMethod "setExtraSelectors" [refT $ constT $ objT c_QStringList] voidT
  ]
  