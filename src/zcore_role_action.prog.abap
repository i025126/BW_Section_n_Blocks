*&---------------------------------------------------------------------*
*& Report zcore_role_action
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcore_role_action.


DATA:
  lv_cluster TYPE zcore_cluster,
  lv_block   TYPE zcore_block,
  lv_infoarea type rsinfoarea,
  lv_rtype   TYPE zcore_roletype.

PARAMETERS:
  " If execute without parameter all roles for the entire
  " solution is create/adjust to the templates
  p_param type tbtcm-eventparm default 'CREATE '.

class lcl_create_package DEFINITION
  create PUBLIC.

  PUBLIC SECTION.

    class-METHODS:
      create_package
        IMPORTING
          iv_infoarea type rsinfoarea
        RETURNING
          VALUE(rv_parent) type string
        RAISING
          cx_rs_msg.
endclass.

class lcl_create_package IMPLEMENTATION.

  METHOD create_package.

    select single rsdarea~infoarea as infoarea,
                  rsdarea~infoarea_p as infoarea_parent,
                  rsdareat~txtlg as infoarea_text
        from rsdarea left outer join rsdareat on
                     rsdarea~infoarea = rsdareat~infoarea and
                     rsdarea~objvers = rsdareat~objvers and
                     rsdareat~langu = @sy-langu
        where rsdarea~infoarea = @iv_infoarea and
              rsdarea~objvers = @rs_c_objvers-active
        into @data(ls_infoarea).

  if sy-subrc <> 0.
    RAISE EXCEPTION type cx_rs_msg.
  endif.

  if ls_infoarea-infoarea_parent is initial.
    rv_parent = ''.
  else.
    rv_parent = create_package( ls_infoarea-infoarea_parent ).
  endif.

  rv_parent = zcl_core_basis_tools=>hana_package_create(
      iv_parent    = rv_parent
      iv_package   = |{ ls_infoarea-infoarea }|
      iv_txtlg     = |{ ls_infoarea-infoarea_text }| ).


  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.

** Usually this program is triggered using an event
** but if started in foreground the eventparameter is taken from p_block
    DATA:
      lv_eventid   TYPE tbtcm-eventid,
      lv_action    type tbtcm-eventparm,
      lv_eventparm TYPE tbtcm-eventparm.

  IF sy-batch = rs_c_false.
    lv_eventparm = p_param.
  ELSE.
    CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
      IMPORTING
        eventid   = lv_eventid
        eventparm = lv_eventparm.
  ENDIF.

  " Find out what to do and the object to do it for
  split lv_eventparm at ' ' into lv_action lv_infoarea.
  message |Action { lv_action }, for InfoArea { lv_infoarea }| type rs_c_success.

  if lv_action = 'DELETE'.
    " We onlt clean up roles
    lv_block = lv_infoarea(4).
    SELECT SINGLE docblock
        FROM zi_core_blocksinsystem
        WHERE DocBlock = @lv_block
        INTO @lv_block.
    if sy-subrc <> 0.
      " No block with that id... can be removed
      do.
        ASSIGN COMPONENT sy-index of STRUCTURE zcl_core_role_admin=>gc_roletype to FIELD-SYMBOL(<lv_roletype>).
        if sy-subrc <> 0.
          exit.
        endif.
        " Delete any role associated with the
        call METHOD zcl_core_role=>delete( zcl_core_role_admin=>get_rolename( iv_block = lv_block iv_roletype = <lv_roletype> ) ).
      enddo.
    endif.
  else.
    TRY.
        SELECT SINGLE docblock
            FROM zi_core_contentview
            WHERE InfoArea = @lv_infoarea
            INTO @lv_block.
      CATCH cx_sy_open_sql_error INTO DATA(lrx_sql).
        MESSAGE lrx_sql->get_text(  ) TYPE 'E'.
        RETURN.
    ENDTRY.

    IF sy-subrc <> 0.
      " This means that the infoarea that have been created is not a block
      MESSAGE |The "Block" { lv_eventparm } given is not an actual block... no action required| TYPE 'I'.
      RETURN.
    ENDIF.

    IF zcl_core_role_admin=>get_virtual_block( iv_blocktype = zcl_core_role_admin=>gc_prefix-global_block ) = lv_block.
      RETURN.
    ENDIF.

    MESSAGE s101(zcore) WITH lv_block.

    lv_cluster = zcl_core_role_admin=>get_cluster_from_block( lv_block ).
    IF zcl_core_role_admin=>get_virtual_block( iv_blocktype = zcl_core_role_admin=>gc_prefix-cluster_fix iv_cluster = lv_cluster ) = lv_block .
      " Some one have created a new cluster
      data:
        lv_cluster_block type rsinfoarea,
        lv_section_block type rsinfoarea.

      " Le's make it easy and create the required InfoArea's
      " First the 'ROOTx'
      lv_cluster_block = |{ zcl_core_basis_tools=>get_c( 'PREFIX_CLUSTER' ) }{ lv_cluster }|.
      call METHOD zcl_core_basis_tools=>infoarea_create( iv_infoarea = lv_cluster_block iv_parent = '' iv_txtlg = |New Cluster { lv_cluster }| ).
      call METHOD zcl_core_basis_tools=>hana_package_create( iv_package = |{ lv_cluster_block }| iv_parent = '' iv_txtlg = |New Cluster { lv_cluster }| ).

      " Now all the sub InfoArea's and packages
      do.
        ASSIGN COMPONENT sy-index of STRUCTURE zcl_core_role_admin=>gc_section TO FIELD-SYMBOL(<lv_section>).
        if sy-subrc <> 0.
           exit.
        endif.
      lv_section_block = |{ <lv_section> }{ lv_cluster }|.
      call METHOD zcl_core_basis_tools=>infoarea_create( iv_infoarea = lv_section_block iv_parent = lv_cluster_block iv_txtlg = |Section { lv_section_block } for Cluster { lv_cluster }| ).
      call METHOD zcl_core_basis_tools=>hana_package_create( iv_package = |{ lv_section_block }| iv_parent = |{ lv_cluster_block }| iv_txtlg = |Section { lv_section_block } for Cluster { lv_cluster }| ).
    enddo.
  else.
    " Since this one will be trigger when the creation of the infoarea as a block... we only need to create the block in section according to the naming
    call METHOD lcl_create_package=>create_package( iv_infoarea = lv_infoarea ).

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

  call METHOD zcl_core_role_admin=>static_show_log(  ).
endif.
