*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 18.11.2020 at 09:10:09
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZCORE_ROLEGEN...................................*
DATA:  BEGIN OF STATUS_ZCORE_ROLEGEN                 .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZCORE_ROLEGEN                 .
CONTROLS: TCTRL_ZCORE_ROLEGEN
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZCORE_ROLEGEN                 .
TABLES: ZCORE_ROLEGEN                  .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
