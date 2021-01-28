*&---------------------------------------------------------------------*
*& Report zcore_role_action
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcore_role_action.


DATA:
  lv_cluster TYPE zcore_cluster,
  lv_block   TYPE zcore_block,
  lv_rtype   TYPE zcore_roletype.

PARAMETERS:
  " If execute without parameter all roles for the entire
  " solution is create/adjust to the templates
  p_block TYPE zcore_block DEFAULT 'GALL'.

START-OF-SELECTION.

** Usually this program is triggered using an event
** but if started in foreground the eventparameter is taken from p_block

  IF sy-batch = rs_c_false.
    lv_block = p_block.
  ELSE.
    DATA:
      lv_eventid   TYPE tbtcm-eventid,
      lv_eventparm TYPE tbtcm-eventparm.

    CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
      IMPORTING
        eventid   = lv_eventid
        eventparm = lv_eventparm.

    lv_block = lv_eventparm.

    TRY.
        SELECT SINGLE docblock
            FROM zi_core_blocksinsystem
            WHERE docblock = @lv_block
            INTO @lv_block.
      CATCH cx_sy_open_sql_error INTO DATA(lrx_sql).
        MESSAGE lrx_sql->get_text(  ) TYPE 'E'.
        RETURN.
    ENDTRY.

    IF sy-subrc <> 0 OR lv_block <> lv_eventparm.
      MESSAGE |The "Block" { lv_eventparm } given is not an actual block... no action required| TYPE 'I'.
      RETURN.
    ENDIF.

    if zcl_core_role_admin=>get_virtual_block( iv_blocktype = 'AUTH' ) = lv_block.
      RETURN.
    endif.

    MESSAGE s101(zcore) WITH lv_block.

    lv_cluster = lv_block(1).
    if zcl_core_role_admin=>get_virtual_block( iv_blocktype = 'ALL' iv_cluster = lv_cluster ) <> lv_block .
      " Yes - you could argue that this could be one call - as the Create also needs to include the
      " So create a content role for development and one for operation
      CALL METHOD zcl_core_role=>create( zcl_core_role_admin=>get_rolename( iv_block = lv_block iv_roletype = zcl_core_role_admin=>gc_roletype-content ) ).
      CALL METHOD zcl_core_role=>adjust( zcl_core_role_admin=>get_rolename( iv_block = lv_block iv_roletype = zcl_core_role_admin=>gc_roletype-content ) ).
      CALL METHOD zcl_core_role=>create( zcl_core_role_admin=>get_rolename( iv_block = lv_block iv_roletype = zcl_core_role_admin=>gc_roletype-operation ) ).
      CALL METHOD zcl_core_role=>adjust( zcl_core_role_admin=>get_rolename( iv_block = lv_block iv_roletype = zcl_core_role_admin=>gc_roletype-operation ) ).
    ENDIF.
    "" Adjust virtual block or create the "Cluster" we don't need to create a Block role, only to update
    "" the cluster role - this means that when the block is created the block role is created and assgined to the ALL role and ready for usage
    CALL METHOD zcl_core_role=>manage_cluster_role( iv_cluster = lv_cluster iv_roletype = zcl_core_role_admin=>gc_roletype-content ).
    CALL METHOD zcl_core_role=>manage_cluster_role( iv_cluster = lv_cluster iv_roletype = zcl_core_role_admin=>gc_roletype-operation ).

  endif.
