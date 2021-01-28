@AbapCatalog.sqlViewName: 'ZICOREIOBJBLCKCL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Gives all InfoObject with Block and Cluster'
define view ZI_CORE_InfoObjectBlockCluster as select from 
    ( rsdiobj as io inner join
      rsdcha  as ch on 
        io.iobjnm  = ch.chanm and
        io.objvers = ch.objvers ) 
      left outer join  
          ZI_CORE_ContentView as cb on io.mtinfoarea = cb.InfoArea
{
   key io.iobjnm           as InfoObject,
       io.fieldnm          as Fieldname,
       io.mtinfoarea       as InfoArea,
       cb.DocCluster       as DocCluster,
       cb.DocSection       as DocSection,
       cb.DocBlock         as DocBlock,
       ch.authrelfl        as RelevantForAuthorization
}
  where io.objvers = 'A'
        
   
