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
      ZI_CORE_ContentView as a  
{
    key a.DocBlock,
        a.DocCluster,
        concat( a.DocBlock, '_MDA' ) as mda,
        concat( a.DocBlock, '_EDW' ) as edw,
        concat( a.DocBlock, '_IAP' ) as iap,
        concat( a.DocBlock, '_EXZ' ) as exz,
        concat( a.DocBlock, '_AAV' ) as aav,
        concat( a.DocBlock, '_SYS' ) as sys, 
        DocBlock                     as cor        
} 
