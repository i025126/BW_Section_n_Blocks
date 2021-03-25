@AbapCatalog.sqlViewName: 'ZIROLEGEN'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Role Generation'

@ClientHandling.algorithm: #SESSION_VARIABLE

-- 1 First level
-- /IWFND/MAINT_SERVICE
@OData.publish: true

@UI.headerInfo: {
   typeName: 'Role to be generated',
   typeNamePlural: 'Roles to be generated' }

-- 2 Searchable
@Search.searchable: true

-- 3 transaction like
@ObjectModel: {
   modelCategory: #BUSINESS_OBJECT,
   compositionRoot: true,
   transactionalProcessingEnabled: true,
   createEnabled: true,
   updateEnabled: true,
   deleteEnabled: true,
   writeActivePersistence: 'ZCORE_ROLEGEN' }

define view ZI_RoleGeneration as select from zcore_rolegen 
  association [0..1] to ZI_CORE_BlocksInSystemT as _Blocks on $projection.Docblock = _Blocks.DocBlock
  association [0..1] to ZI_CORE_Roletype as _RoleType on $projection.Roletype = _RoleType.RoleType
{
  @UI.selectionField.position: 10
  @UI.lineItem.position: 10
  @UI.identification.position: 10
-- 2 Searcha
  @Search: {
       defaultSearchElement: true,
       fuzzinessThreshold: 0.7 }
  @ObjectModel.foreignKey.association: '_Blocks'
  key docblock as Docblock,
  @UI.selectionField.position: 20
  @UI.lineItem.position: 20
  @UI.identification.position: 20
  @ObjectModel.foreignKey.association: '_RoleType'
  key roletype as Roletype    ,
  _Blocks,
  _RoleType
}
