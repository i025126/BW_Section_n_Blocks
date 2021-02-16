*&---------------------------------------------------------------------*
*& Report zcore_role_action
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcore_role_action.


DATA:
  lv_cluster  TYPE zcore_cluster,
  lv_block    TYPE zcore_block,
  lv_infoarea TYPE rsinfoarea,
  lv_rtype    TYPE zcore_roletype.

PARAMETERS:
  " If execute without parameter all roles for the entire
  " solution is create/adjust to the templates
  p_param TYPE tbtcm-eventparm DEFAULT 'CREATE '.

START-OF-SELECTION.

** Usually this program is triggered using an event
** but if started in foreground the eventparameter is taken from p_block
  DATA:
    lv_eventid   TYPE tbtcm-eventid,
    lv_action    TYPE tbtcm-eventparm,
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
  SPLIT lv_eventparm AT ' ' INTO lv_action lv_infoarea.
  MESSAGE |Action { lv_action }, for InfoArea { lv_infoarea }| TYPE rs_c_success.

  " Now we know what to do and with what to do it.
  IF lv_action = 'DELETE'.
    " We onlt clean up roles
    lv_block = lv_infoarea(4).
    SELECT SINGLE docblock
        FROM zi_core_blocksinsystem
        WHERE docblock = @lv_block
        INTO @lv_block.
    IF sy-subrc <> 0.
      " No block with that id... can be removed
      DO.
        ASSIGN COMPONENT sy-index OF STRUCTURE zcl_core_role_admin=>gc_roletype TO FIELD-SYMBOL(<lv_roletype>).
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        " Delete any role associated with the
        CALL METHOD zcl_core_role=>delete( zcl_core_role_admin=>get_rolename( iv_block = lv_block iv_roletype = <lv_roletype> ) ).
        CALL METHOD zcl_core_basis_tools=>hana_role_delete( zcl_core_role_admin=>get_rolename( iv_block = lv_block iv_roletype = <lv_roletype> ) ).
      ENDDO.
    ENDIF.
  ELSEIF lv_action = 'CREATE'.
    " We need to create a lot of stuff
    " But first we need to check if the block is an actual block or something that is not
    " really a block
    TRY.
        SELECT SINGLE docblock
            FROM zi_core_contentview
            WHERE infoarea = @lv_infoarea
            INTO @lv_block.
      CATCH cx_sy_open_sql_error INTO DATA(lrx_sql).
        MESSAGE lrx_sql->get_text(  ) TYPE 'E'.
        RETURN.
    ENDTRY.

    IF sy-subrc <> 0.
      " This means that the infoarea that have been created is not a block
      MESSAGE |The "EVENT" { lv_eventparm } given is dealing with an actual block... no action required| TYPE 'I'.
      RETURN.
    ENDIF.

    IF zcl_core_role_admin=>get_virtual_block( iv_blocktype = zcl_core_role_admin=>gc_prefix-global_block ) = lv_block.
      " If someone creates the AUTH block don't do anything
      RETURN.
    ENDIF.

    MESSAGE s101(zcore) WITH lv_block.

    lv_cluster = zcl_core_role_admin=>get_cluster_from_block( lv_block ).
    IF zcl_core_role_admin=>get_virtual_block( iv_blocktype = zcl_core_role_admin=>gc_prefix-cluster_fix iv_cluster = lv_cluster ) = lv_block .
      "--- just to be 100 procent sure before starting the engine...
      SELECT SINGLE infoarea_p
          FROM rsdarea
          WHERE infoarea = @lv_infoarea
          INTO @DATA(lv_parent).
      ASSERT sy-subrc = 0.
      " Is the Infoarea xALL placed correctly
      CHECK lv_parent = zcl_core_basis_tools=>get_c( zcl_core_basis_tools=>gc_parameters-infoareacluster ).
      " Some one have created a new cluster
      DATA:
        lv_cluster_block TYPE rsinfoarea,
        lv_section_block TYPE rsinfoarea.

      " Le's make it easy and create the required InfoArea's
      " First the 'ROOTx'
      lv_cluster_block = |{ zcl_core_basis_tools=>get_c( zcl_core_basis_tools=>gc_parameters-prefix_cluster ) }{ lv_cluster }|.
      " Here we create the root infoarea
      CALL METHOD zcl_core_basis_tools=>infoarea_create( iv_infoarea = lv_cluster_block iv_parent = '' iv_txtlg = |New Cluster { lv_cluster }| ).
      " and repository package root
      CALL METHOD zcl_core_basis_tools=>hana_package_create( iv_package = |{ lv_cluster_block }| iv_parent = '' iv_txtlg = |New Cluster { lv_cluster }| ).
      " and a role in HANA. This role is slightly different than the BW role as the role cluster role does not need an architect role
      " the xALL roles is create to have access to the ROOTx repository package and below
      CALL METHOD zcl_core_basis_tools=>hana_role_create( zcl_core_role_admin=>get_rolename( iv_block = lv_block iv_roletype = zcl_core_role_admin=>gc_roletype-content ) ).
      " Now all the sub InfoArea's and repository package
      DO.
        ASSIGN COMPONENT sy-index OF STRUCTURE zcl_core_role_admin=>gc_section TO FIELD-SYMBOL(<lv_section>).
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        lv_section_block = |{ <lv_section> }{ lv_cluster }|.
        CALL METHOD zcl_core_basis_tools=>infoarea_create( iv_infoarea = lv_section_block iv_parent = lv_cluster_block iv_txtlg = |Section { lv_section_block } for Cluster { lv_cluster }| ).
        CALL METHOD zcl_core_basis_tools=>hana_package_create( iv_package = |{ <lv_section> }| iv_parent = |{ lv_cluster_block }| iv_txtlg = |Section { lv_section_block } for Cluster { lv_cluster }| ).
      ENDDO.
    ELSE.
      " Since this one will be trigger when the creation of the infoarea as a block... we only need to create the block in section according to the naming
      " This one will also create the create repository package in HANA, as it otherwise would be impossible to activate the role
      " rolename is the same as in BW... BW4ABFMC_SIFIN
      " Create
      " ROOTx
      " ROOTx.EDW
      " ROOTx.EDW.ABFM
      " AUTH.ROOTx:: BW4ABFMC_SIFIN
      CALL METHOD zcl_core_basis_tools=>hana_role_create( zcl_core_role_admin=>get_rolename( iv_block = lv_block iv_roletype = zcl_core_role_admin=>gc_roletype-content ) ).

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

    CALL METHOD zcl_core_role_admin=>static_show_log( ).
  ENDIF.
