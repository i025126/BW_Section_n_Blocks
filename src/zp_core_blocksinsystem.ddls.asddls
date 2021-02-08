@AbapCatalog.sqlViewName: 'ZPCOREBLOCKS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CORE: Get all blocks in the systems'

@AbapCatalog.buffering.status: #SWITCHED_OFF
@AbapCatalog.buffering.type: #NONE

-- Maybe someone later will come up with a better idea - I need to have 
-- one and only one unique block for one or more InfoAreas, the problem
-- is that i cannot say what section the block is from, hence this weird
-- construct. The logic is:
-- any block will have will have an entry in one of the section, otherwise it will not be listed
-- so we expect this, the continueation is in the ZP_CORE_BlocksInSystemT
define view ZP_CORE_BLOCKSINSYSTEM as 
    select distinct from 
      ZI_CORE_ContentView as a inner join zcore_setup as zs on zs.customizid = 'BW'
{
    key a.DocBlock,
        a.DocCluster,
        concat( concat( a.DocBlock, '_' ) , zs.mda_section ) as mda,
        concat( concat( a.DocBlock, '_' ) , zs.edw_section ) as edw,
        concat( concat( a.DocBlock, '_' ) , zs.iap_section ) as iap,
        concat( concat( a.DocBlock, '_' ) , zs.edw_section ) as exz,
        concat( concat( a.DocBlock, '_' ) , zs.aav_section ) as aav,
        concat( concat( a.DocBlock, '_' ) , zs.sys_section ) as sys, 
        DocBlock                     as cor        
} 
