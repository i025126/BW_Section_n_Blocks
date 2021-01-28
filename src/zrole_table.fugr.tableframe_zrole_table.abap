*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZROLE_TABLE
*   generation date: 18.11.2020 at 09:10:08
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZROLE_TABLE        .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
