@AbapCatalog.sqlViewName: 'ZICORECLUSTERT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED

@ObjectModel.dataCategory: #TEXT

@EndUserText.label: 'Text for Clusters'
define view ZI_CORE_ClusterT as select from 
    rsdareat as t inner join ZI_CORE_Cluster as m on t.infoarea = m.ClusterInfoArea
  association[0..1] to I_Language as _Language on _Language.Language = $projection.Language
  association[0..1] to ZI_CORE_Cluster as _Cluster on _Cluster.DocCluster = $projection.DocCluster
{
    @ObjectModel.foreignKey.association: '_Cluster'
    key cast( left( t.infoarea, 1) as zcore_cluster) as DocCluster,
    @ObjectModel.foreignKey.association: '_Language'
    key t.langu as Language,
    @Semantics.text: true
        t.txtlg as ClusterText,

    _Language,
    _Cluster
}
  where 
    t.objvers    = 'A'
