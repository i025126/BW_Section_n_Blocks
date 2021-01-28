@AbapCatalog.sqlViewName: 'ZICORECONTENTV'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED


@EndUserText.label: 'CDS of all entries in the content build with CTIE'
define view ZI_CORE_ContentView as select from 
      rsdarea as blocks 
         inner join
      rsdarea as section on
           blocks.infoarea_p = section.infoarea and
           blocks.objvers    = section.objvers
         inner join 
      rsdarea as root on
           section.infoarea_p = root.infoarea and
           section.objvers    = root.objvers      
  association [0..1] to ZI_CORE_Cluster as _cluster on $projection.DocCluster = _cluster.DocCluster
{
    key blocks.infoarea          as InfoArea,
        case blocks.infoarea_p 
          when 'SYSX' then 'X'
          else              
            cast( left(blocks.infoarea,1) as zcore_cluster ) 
        end as DocCluster,
        cast( left(section.infoarea,3) as zcore_section ) as DocSection,    
        cast( left(blocks.infoarea,4) as zcore_block )  as DocBlock
}
  where blocks.objvers = 'A' and
        ( root.infoarea      between 'ROOTA' and 'ROOTZ' or blocks.infoarea_p = 'SYSX_ALL' )
        
