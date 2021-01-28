@AbapCatalog.sqlViewName: 'ZICOREAGR2AUTH'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Shows that value of AUTH for the data roles'
define view ZI_CORE_DataRole2Auth as select from
          agr_define as role left outer join
          agr_1250   as prof on prof.agr_name = role.agr_name left outer join
          agr_1251   as auth on auth.agr_name = prof.agr_name and
                                auth.auth     = prof.auth
{
   role.agr_name,
   substring(role.agr_name, 4, 1) as DocCluster,
   substring(role.agr_name, 4, 4) as DocBlock,
   substring(role.agr_name, 8,1 ) as Roletype,
   prof.object                    as object,
   auth.field                     as field,
   auth.low                       as AUTH
}
where 
  prof.object   = 'S_RS_AUTH' and 
  auth.field    = 'BIAUTH' and
  role.agr_name between 'BW4' and 'BW4ZZZZZZZZZZZZZZZZZZZZZZZZZZZ';
      
