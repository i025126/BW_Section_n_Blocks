*&---------------------------------------------------------------------*
*& Report zcore_role_admin
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcore_role_admin.

DATA:
  lv_cluster TYPE zcore_cluster,
  lv_block   TYPE zcore_block,
  lv_rtype   TYPE zcore_roletype.

"" First block is the START the generation of the role
SELECTION-SCREEN BEGIN OF BLOCK bl1.
PARAMETERS:
  p_genr TYPE rs_bool RADIOBUTTON GROUP gr1 DEFAULT 'X'.
SELECTION-SCREEN BEGIN OF BLOCK gn1 WITH FRAME TITLE TEXT-gn1.
PARAMETERS:
  pv_1rty TYPE zcore_roletype DEFAULT 'D',  "Only one role type
  pv_1cls TYPE zcore_cluster DEFAULT 'A',  "Only one cluster
  pv_txt  TYPE rsoadsonm DEFAULT 'ASYSAWSTX',
  pv_val  TYPE rsoadsonm DEFAULT 'ASYSAWSAT'.

SELECT-OPTIONS:
  pt_1blk FOR lv_block  NO INTERVALS.                "Enter the blocks
SELECTION-SCREEN END OF BLOCK gn1.

"" Adapt existing roles of a roletype, can be split by cluster
"" this is to do a succesive role-out
PARAMETERS:
  p_adapt TYPE rs_bool RADIOBUTTON GROUP gr1.
SELECTION-SCREEN BEGIN OF BLOCK ad1 WITH FRAME TITLE TEXT-ad1.
select-OPTIONS:
  pt_2rty for lv_rtype   no intervals,
  pt_2cls for lv_cluster no INTERVALS.
SELECTION-SCREEN END OF BLOCK ad1.

PARAMETERS:
  p_veri type rs_bool RADIOBUTTON GROUP gr1.

"" Trigger the ZCORE_ROLE_ACTION
PARAMETERS:
  p_trig TYPE rs_bool RADIOBUTTON GROUP gr1.
SELECTION-SCREEN BEGIN OF BLOCK tr1 WITH FRAME TITLE TEXT-tr1.
PARAMETERS:
  pv_3blk TYPE zcore_block.
SELECTION-SCREEN END OF BLOCK tr1.

SELECTION-SCREEN END OF BLOCK bl1.

AT SELECTION-SCREEN ON pv_1rty.
  IF p_genr = rs_c_true.
    IF pv_1rty IS INITIAL.
      MESSAGE |You must selection on Role type for generation| TYPE 'W'.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON pt_2rty.
  IF p_adapt = rs_c_true.
    IF pt_2rty IS INITIAL.
      MESSAGE |You must selection on Role type for adapting existing generated roles| TYPE 'W'.
    ENDIF.
  ENDIF.


AT SELECTION-SCREEN ON pv_3blk.
  IF p_trig = rs_c_true.
    IF pv_3blk IS INITIAL.
      MESSAGE |You must specific a block, to mimic a create of an infoArea| TYPE 'E'.
    ENDIF.
  ENDIF.

START-OF-SELECTION.

  IF p_genr = rs_c_true.
    IF pv_1rty = zcl_core_role_admin=>gc_roletype-data.
      TRY.
          CALL METHOD zcl_core_role_admin=>generate
            EXPORTING
              iv_roletype = pv_1rty
              iv_cluster  = pv_1cls
              it_block    = pt_1blk[]
              iv_txt_adso = pv_txt
              iv_authadso = pv_val.
        CATCH cx_rs_error INTO data(lrx_error).
          MESSAGE lrx_error->get_text(  ) TYPE 'W'.
      ENDTRY.
    ENDIF.

    call METHOD zcl_core_role_admin=>static_show_log.

  ENDIF.

  IF p_adapt = rs_c_true.
    do.
      "" Find all defined roletypes - I've not made a DDL for this, se take it from the once we are processing
      ASSIGN COMPONENT sy-index of STRUCTURE zcl_core_role_admin=>gc_roletype to FIELD-SYMBOL(<lv_roletype>).
      if sy-subrc <> 0.
        exit.
      endif.
      if <lv_roletype> in pt_2rty.
        " And find all the selected cluster... I could do this into an internal table, out
        " there is only very few
        select doccluster
            from zi_core_cluster
            where doccluster in @pt_2cls
            into table @data(lt_cluster).
        loop at lt_cluster into data(ls_cluster).
          CALL METHOD zcl_core_role_admin=>adapt
            EXPORTING
              iv_roletype = <lv_roletype>
              iv_cluster  = ls_cluster-DocCluster.
        ENDLOOP..
      endif.
    enddo.
    call METHOD zcl_core_role_admin=>static_show_log.
  ENDIF.

  if p_veri = rs_c_true.
    try.
        call METHOD zcl_core_role_admin=>verify.
      catch cx_root.
    endtry.
    call METHOD zcl_core_role_admin=>static_show_log.

  endif.

  IF p_trig = rs_c_true.
    IF pv_3blk IS NOT INITIAL.
      CALL FUNCTION 'BP_EVENT_RAISE'
        EXPORTING
          eventid                = 'ZROLE_EVENT'
          eventparm              = pv_3blk
        EXCEPTIONS
          bad_eventid            = 1
          eventid_does_not_exist = 2
          eventid_missing        = 3
          raise_failed           = 4
          OTHERS                 = 5.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.
  ENDIF.
