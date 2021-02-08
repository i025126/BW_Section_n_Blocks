@AbapCatalog.sqlViewName: 'ZICOREBLOCKS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CORE: Get all blocks in the systems'

define view ZI_CORE_BlocksInSystem as 
    select distinct from 
      ZP_CORE_BLOCKSINSYSTEM as z
      
  association[0..*] to ZI_CORE_BlocksInSystemT as _Text on _Text.DocBlock = $projection.DocBlock
  association[0..1] to ZI_CORE_Cluster as _Cluster on _Cluster.DocCluster = $projection.DocCluster
{
    @ObjectModel.text.association: '_Text'
    key z.DocBlock    as DocBlock,
        z.DocCluster  as DocCluster,
        _Text,
        _Cluster
} 
