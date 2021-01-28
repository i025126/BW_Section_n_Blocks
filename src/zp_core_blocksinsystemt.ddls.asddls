@AbapCatalog.sqlViewName: 'ZICOREBLOCKST'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck:#NOT_REQUIRED
-- So the logic is that we find the first InfoArea that is not null
-- starting from 
-- MDA -> EDW -> IAP -> EXZ -> AAV -> SYS -> COR
-- so the "master InfoArea for a block is found and this is
-- used for text etc... all the mda, edw, iap ... a void now
@EndUserText.label: 'Private view the get ONE InfoArea'
define view ZP_CORE_BLOCKSINSYSTEMT as select from ZP_CORE_BLOCKSINSYSTEM as z

 association [0..1] to rsdarea as _mda on _mda.infoarea = $projection.mda and _mda.objvers = 'A'
 association [0..1] to rsdarea as _edw on _edw.infoarea = $projection.edw and _edw.objvers = 'A'
 association [0..1] to rsdarea as _iap on _iap.infoarea = $projection.iap and _iap.objvers = 'A'
 association [0..1] to rsdarea as _exz on _exz.infoarea = $projection.exz and _exz.objvers = 'A'
 association [0..1] to rsdarea as _aav on _aav.infoarea = $projection.aav and _aav.objvers = 'A'
 association [0..1] to rsdarea as _sys on _sys.infoarea = $projection.sys and _sys.objvers = 'A'
 association [0..1] to rsdarea as _cor on _cor.infoarea = $projection.cor and _cor.objvers = 'A'
{
    key z.DocBlock,
        z.DocCluster,
        z.mda,
        z.edw,
        z.iap,
        z.exz,
        z.aav,
        z.sys,
        z.cor,
    coalesce( coalesce( coalesce( coalesce( coalesce( coalesce( _mda.infoarea, _edw.infoarea) , _iap.infoarea ), _exz.infoarea ) , _aav.infoarea ) , _sys.infoarea), _cor.infoarea) as InfoArea
} 
