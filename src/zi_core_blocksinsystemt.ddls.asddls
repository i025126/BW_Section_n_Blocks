@AbapCatalog.sqlViewName: 'ZPCOREBLOCKST'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CORE: Get all blocks in the systems'

@ObjectModel.dataCategory: #TEXT

@AbapCatalog.buffering.status: #SWITCHED_OFF
@AbapCatalog.buffering.type: #NONE

define view ZI_CORE_BlocksInSystemT 
      as select from 
         ZP_CORE_BLOCKSINSYSTEMT as z
           left outer join 
         rsdareat as t
           on t.infoarea = z.InfoArea and
              t.objvers  = 'A'
 association [0..1] to ZI_CORE_BlocksInSystem as _Blocks on _Blocks.DocBlock = $projection.DocBlock
 association [0..1] to I_Language as _Language on _Language.Language = $projection.Language
 {
    @ObjectModel.foreignKey.association: '_Blocks'
    @ObjectModel.text.element: ['TXTLG']
    key z.DocBlock,
    @ObjectModel.foreignKey.association: '_Language'
    key t.langu as Language,
    @Semantics.text: true
      t.txtlg as txtlg ,
        
//    _Blocks,
    _Language
} 

