@AbapCatalog.sqlViewName: 'ZICOREROLETYPE'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_ALLOWED
@EndUserText.label: 'Role Type'

@ObjectModel.dataCategory: #TEXT

define view ZI_CORE_Roletype as select from dd07v 
  association [0..1] to I_Language as _Language on $projection.Language = _Language.Language 
{
   @Semantics.language: true
   key ddlanguage       as Language,
   @ObjectModel.text.element: ['RoleTypeDescription']
   key cast( domvalue_l as zcore_roletype ) as RoleType,
   @Semantics.text: true
   ddtext               as RoleTypeDescription,
   
   _Language
}
  where domname = 'ZCORE_ROLETYPE'
   
