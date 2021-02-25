CLASS zcl_core_role_admin DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

*    TYPES:
*      BEGIN OF gtys_datetime,
*        c_date TYPE char50,
*        c_time TYPE char50,
*      END OF gtys_datetime,
*      gtyt_datetim     TYPE STANDARD TABLE OF gtys_datetime WITH NON-UNIQUE DEFAULT KEY,
*      gtyt_release     TYPE STANDARD TABLE OF syst_saprl WITH NON-UNIQUE DEFAULT KEY,
*      gtyt_loaded_agrs TYPE STANDARD TABLE OF agr_name WITH NON-UNIQUE DEFAULT KEY,
*      BEGIN OF gtys_role,
*        date        TYPE gtyt_datetim,
*        release     TYPE gtyt_release,
*        loaded_agrs TYPE gtyt_loaded_agrs,
*      END OF gtys_role.

    TYPES:
      gtyv_action       type char1,
      gtyv_rolekind     type char1,
      gtyv_rolename     TYPE agr_name,
      gtyv_roletemplate TYPE gtyv_rolename,
      BEGIN OF gtys_role_processing,
        doccluster TYPE zcore_cluster,
        docblock   TYPE zcore_block,
        roletype   TYPE zcore_roletype,
      END OF gtys_role_processing,
      gtyts_role_processing TYPE SORTED TABLE OF gtys_role_processing WITH UNIQUE KEY doccluster docblock roletype.

    TYPES:
      "! Range of role type for processing
      gtyt_rng_roletype TYPE RANGE OF zcore_roletype,
      "! Range of blocks to be processed
      gtyt_rng_block    TYPE RANGE OF zcore_block.

    class-data:
      begin of gc_rolekind,
        generic type gtyv_rolekind value ' ',
        bw4     type gtyv_rolekind value 'B',
        hana    type gtyv_rolekind value 'H',
        unknown type gtyv_rolekind value 'Z',
      end of gc_rolekind,
      BEGIN OF gc_section,
        sys TYPE zcore_section VALUE 'SYS',
        exz TYPE zcore_section VALUE 'EXZ',
        aav TYPE zcore_section VALUE 'AAV',
        iap TYPE zcore_section VALUE 'IAP',
        edw TYPE zcore_section VALUE 'EDW',
        mda TYPE zcore_section VALUE 'MDA',
      END OF gc_section,
      BEGIN OF gc_prefix,
        role_prefix     TYPE string VALUE 'BW4',
        global_block    TYPE string VALUE 'AUTH',
        cluster_fix     TYPE string VALUE 'ALL',
        " This one controls the length of the cluster
        " be sure to change how this one looks
        garbage_cluster TYPE string VALUE 'Z',
        adso_for_text   type string value 'SYSAWSTX',
        adso_for_auth   type string value 'SYSAWSAT',
        compositerole   type agr_name value 'BW4AUTHTCOMPOSITE',
      END OF gc_prefix,
      gv_auth_block  type zcore_block value 'AUTH',
      gv_all_postfix type zcore_block value 'ALL',
      gv_crossclusterdesc type string value 'ALL'.

    CONSTANTS:
      begin of gc_action,
        report        type gtyv_action value 'R',
        cleandroles   type gtyv_action value 'D',
        Notmaintained type gtyv_action value 'N',
        all           type gtyv_action value 'A',
      end of gc_action,
      BEGIN OF gc_roletype,
        access    TYPE zcore_roletype  VALUE 'A',
        architect TYPE zcore_roletype  VALUE 'B',
        content   TYPE zcore_roletype  VALUE 'C',
        data      TYPE zcore_roletype  VALUE 'D',
        operation TYPE zcore_roletype  VALUE 'O',
        super     TYPE zcore_roletype  VALUE 'S',
        technical TYPE zcore_roletype  VALUE 'T',
        query     TYPE zcore_roletype  VALUE 'Q',
      END OF gc_roletype.

    TYPES:
      gtyt_roletype TYPE STANDARD TABLE OF zcore_roletype WITH NON-UNIQUE DEFAULT KEY,
      gtyt_rolename TYPE STANDARD TABLE OF gtyv_rolename WITH NON-UNIQUE DEFAULT KEY,
      gtyt_block    TYPE STANDARD TABLE OF zcore_block WITH NON-UNIQUE DEFAULT KEY.

    CLASS-DATA:
      gv_detlevel   type ballevel,
      gv_n_detlevel type i,
      gv_message    TYPE string,
      gv_log_handle TYPE balloghndl.


    CLASS-METHODS:
      class_constructor,
      "! Very basic method will delete all content of an aDSO
      "! inteded to be used on the w/o aDSO that is used for output for analysis authorization
      "! @parameter iv_adsonum | aDSO to be truncated
      "! @raising cx_rs_error  | generic error from aDSO
      static_delete_data_in_adso
        IMPORTING
          iv_adsonum TYPE rsoadsonm
        RAISING
          cx_rs_error,
      "! Finds all block defined in the ZCORE_ROLEGEN table and makes sure these are generated!
      "! The assumption is the aDSO follow the naming [cluster]SYSAWSTX and AT for Text and value
      "! the method will process one cluster at the time
      generate_all
        IMPORTING
          iv_with_adjustment type rs_bool DEFAULT rs_c_false,
      generate
        IMPORTING
          iv_roletype TYPE zcore_roletype
          iv_cluster  TYPE zcore_cluster
          it_block    TYPE gtyt_rng_block
          iv_txt_adso TYPE rsoadsonm
          iv_authadso TYPE rsoadsonm
        RAISING
          cx_rs_error,
      adapt
        IMPORTING
          iv_roletype TYPE zcore_roletype
          iv_cluster  TYPE zcore_cluster,
      verify
        IMPORTING
          iv_action   type gtyv_action OPTIONAL
        RAISING
          cx_rs_error,
      static_show_log,
      static_set_detlevel
        IMPORTING
          iv_detlevel      type i DEFAULT 0,
      static_do_message
        IMPORTING
          iv_message       TYPE string OPTIONAL
          iv_message_type  TYPE sy-msgty DEFAULT rs_c_info
          iv_detlevel      type i DEFAULT 0
        PREFERRED PARAMETER iv_message
        RETURNING
          VALUE(rs_msglog) TYPE balmsghndl,
      is_framework_role
        IMPORTING
          iv_rolename     type clike
          iv_rolekind     type gtyv_rolekind DEFAULT gc_rolekind-generic
        RETURNING
          VALUE(rv_rolekind) type gtyv_rolekind,
      get_bwrole_from_hana_role
        IMPORTING
          iv_rolename     type clike
        RETURNING
          VALUE(rv_rolename) type agr_name,
      get_hana_auth_from_rolename
        IMPORTING
          iv_rolename     TYPE agr_name
        RETURNING
          VALUE(rv_package) type string,
      get_block_from_rolename
        IMPORTING
          iv_rolename     TYPE gtyv_rolename
        RETURNING
          VALUE(rv_block) TYPE zcore_block,
      get_roletype_from_rolename
        IMPORTING
          iv_rolename        TYPE gtyv_rolename
        RETURNING
          VALUE(rv_roletype) TYPE zcore_roletype,
      get_cluster_from_block
        IMPORTING
                  iv_block          TYPE zcore_block
        RETURNING VALUE(rv_cluster) TYPE zcore_cluster,
      get_abbreviation_from_rolename
        IMPORTING
                  iv_rolename            TYPE gtyv_rolename
        RETURNING VALUE(rv_abbreviation) TYPE zcore_abbreviation,
      get_cluster_from_rolename
        IMPORTING
                  iv_rolename       TYPE gtyv_rolename
        RETURNING VALUE(rv_cluster) TYPE zcore_cluster,
      get_profile_status
        IMPORTING
                  iv_rolename      TYPE gtyv_rolename
        RETURNING VALUE(rv_status) TYPE char6,
      get_block_description
        IMPORTING
          iv_block        TYPE zcore_block
          iv_language     TYPE langu DEFAULT 'E'
        RETURNING
          VALUE(rv_txtlg) TYPE rstxtlg
        RAISING
          cx_rs_msg,
      get_cluster_description
        IMPORTING
          iv_cluster            TYPE zcore_cluster
        RETURNING
          VALUE(rv_description) TYPE rstxtlg,
      get_roletype_description
        IMPORTING
          iv_roletype           TYPE zcore_roletype
        RETURNING
          VALUE(rv_description) TYPE val_text,
      get_template_from_roletype
        IMPORTING
          iv_roletype                TYPE zcore_roletype
        RETURNING
          VALUE(rv_roletypetemplate) TYPE gtyv_roletemplate,
      get_virtual_block
        IMPORTING
          iv_blocktype            TYPE clike
          iv_cluster              TYPE zcore_cluster DEFAULT 'X'
        RETURNING
          VALUE(rv_virtual_block) TYPE zcore_block,
      check_role_exists
        IMPORTING
                  iv_rolename      TYPE agr_name
        RETURNING VALUE(rv_exists) TYPE rs_bool,
      get_role_description
        IMPORTING
                  iv_rolename     TYPE agr_name
                  ir_authname     TYPE REF TO zbapi_data_role_value OPTIONAL
        RETURNING VALUE(rt_txtlg) TYPE zif_data_role_value=>gtyt_agr_texts,
      get_rolename
        IMPORTING
                  iv_block           TYPE zcore_block
                  iv_roletype        TYPE zcore_roletype
        RETURNING VALUE(rv_rolename) TYPE gtyv_rolename.

  PROTECTED SECTION.
    CLASS-DATA:
      nv_simulate TYPE rs_bool VALUE rs_c_true.

  PRIVATE SECTION.

    class-METHODS:
      verify_count_blocks,
      verify_check_roles,
      verify_orphen_roles.

ENDCLASS.



CLASS zcl_core_role_admin IMPLEMENTATION.


  METHOD class_constructor.

    gc_section-mda         = zcl_core_basis_tools=>get_c( 'MDA_SECTION' ).
    gc_section-edw         = zcl_core_basis_tools=>get_c( 'EDW_SECTION' ).
    gc_section-iap         = zcl_core_basis_tools=>get_c( 'IAP_SECTION' ).
    gc_section-aav         = zcl_core_basis_tools=>get_c( 'AAV_SECTION' ).
    gc_section-sys         = zcl_core_basis_tools=>get_c( 'SYS_SECTION' ).
    gc_section-exz         = zcl_core_basis_tools=>get_c( 'EXZ_SECTION' ).
    gc_prefix-role_prefix  = zcl_core_basis_tools=>get_c( 'PREFIX_ROLE' ).
    gc_prefix-adso_for_text = zcl_core_basis_tools=>get_c( 'POSTFIX_ADSO_AUTH' ).
    gc_prefix-adso_for_auth = zcl_core_basis_tools=>get_c( 'POSTFIX_ADSO_TEXT' ).
    gc_prefix-compositerole = zcl_core_basis_tools=>get_c( 'COMPOSITE_ROLE' ).

    gv_auth_block  = zcl_core_basis_tools=>get_c( 'AUTH_BLOCK' ).
    gv_all_postfix = zcl_core_basis_tools=>get_c( 'ALL_POSTFIX' ).

  ENDMETHOD.

  METHOD static_show_log.

    DATA lt_log_handle TYPE bal_t_logh.

    APPEND gv_log_handle TO lt_log_handle.

    IF sy-batch = rs_c_false.
      DATA:
        ls_display_profile TYPE bal_s_prof.

* get variant which creates hierarchy according to field DETLEVEL
      CALL FUNCTION 'BAL_DSP_PROFILE_DETLEVEL_GET'
        IMPORTING
          e_s_display_profile = ls_display_profile
        EXCEPTIONS
          OTHERS              = 1.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      APPEND gv_log_handle TO lt_log_handle.
      CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
        EXPORTING
          i_s_display_profile  = ls_display_profile
          i_t_log_handle       = lt_log_handle
        EXCEPTIONS
          profile_inconsistent = 1
          internal_error       = 2
          no_data_available    = 3
          no_authority         = 4
          OTHERS               = 5.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ELSE.
      CALL FUNCTION 'BAL_DB_SAVE'
        EXPORTING
          i_t_log_handle   = lt_log_handle
          i_save_all       = rs_c_true
        EXCEPTIONS
          log_not_found    = 1
          save_not_allowed = 2
          numbering_error  = 3
          OTHERS           = 4.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.


  ENDMETHOD.

  METHOD check_role_exists.
    " Find if a role exists on the DB
    DATA
      lt_agr_texts TYPE STANDARD TABLE OF  agr_texts WITH NON-UNIQUE DEFAULT KEY.

    rv_exists = rs_c_false.
    CALL FUNCTION 'PRGN_ACTIVITY_GROUPS_LOAD'
      EXPORTING
        activity_group               = iv_rolename
      TABLES
        activity_groups_texts        = lt_agr_texts
      EXCEPTIONS
        no_activity_groups_available = 4
        OTHERS                       = 8.
    CASE sy-subrc.
      WHEN 0.
        rv_exists = rs_c_true.
      WHEN 4.
        rv_exists = rs_c_false.
      WHEN 8.
        message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        rv_exists = rs_c_false.
    ENDCASE.

  ENDMETHOD.

  METHOD get_hana_auth_from_rolename.
    " standard is the the repository package AUTH is given a sub package ROOTx ROOT is defined and 'x' is the cluster
    " so the package is given as AUTH.ROOTA
    data lv_rolename type agr_name.
    lv_rolename = iv_rolename.
    rv_package = |{ zcl_core_basis_tools=>get_c( zcl_core_basis_tools=>gc_parameters-auth_block ) }.| &&
                 |{ zcl_core_basis_tools=>get_c( zcl_core_basis_tools=>gc_parameters-prefix_cluster ) }{ get_cluster_from_rolename( lv_rolename ) }|.
  ENDMETHOD.

  METHOD get_bwrole_from_hana_role.
    data lv_char72 type char72.

    clear rv_rolename.

    lv_char72 = iv_rolename.
    data lv_block type zcore_block.
    " We know that this must be a block
    lv_block = zcl_core_basis_tools=>get_c( zcl_core_basis_tools=>gc_parameters-auth_block ).
    " We can get a rolename
    data(lv_rolename)   = get_rolename( iv_block = lv_block iv_roletype = gc_roletype-content ).
    " we can generate a hana prefix
    data(lv_hanaprefix) = get_hana_auth_from_rolename( lv_rolename ).
    " We can get the length of the prefix, minus the length of the cluster in the end
    data(lv_authlength) = strlen( lv_hanaprefix ) - strlen( gc_prefix-garbage_cluster ) - 1.

    if lv_hanaprefix(lv_authlength) = iv_rolename(lv_authlength).
      " At least the first part of the prefix matchs
      lv_authlength = lv_authlength + 3.
      SHIFT lv_char72 left by lv_authlength PLACES.
      if is_framework_role( iv_rolename = lv_char72 iv_rolekind = gc_rolekind-bw4 ).
        " If the check if the remainder of the role is an actual framework role, if it is the translation if cool
        rv_rolename = lv_char72.
      endif.
    endif.

  ENDMETHOD.

  METHOD is_framework_role.

    rv_rolekind = gc_rolekind-unknown.
    " This will make a check on the role
    if iv_rolekind = gc_rolekind-generic or iv_rolekind = gc_rolekind-bw4.
      data(lv_prefix_role) = strlen( zcl_core_basis_tools=>get_c( zcl_core_basis_tools=>gc_parameters-prefix_role ) ).
      if zcl_core_basis_tools=>get_c( zcl_core_basis_tools=>gc_parameters-prefix_role ) = iv_rolename(lv_prefix_role).
        " Could be a BW role
        data lv_rolename type agr_name.
        lv_rolename = iv_rolename.
        if get_cluster_description(  get_cluster_from_rolename( lv_rolename ) ) <> get_cluster_from_rolename( lv_rolename ).
          " The get_cluster description will if all is valid return a description and not only the cluster
          rv_rolekind = gc_rolekind-bw4.
        endif.
      endif.
    endif.

    if iv_rolekind = gc_rolekind-generic or iv_rolekind = gc_rolekind-hana.
      if is_framework_role( iv_rolename = get_bwrole_from_hana_role( iv_rolename ) iv_rolekind = gc_rolekind-bw4 ) = gc_rolekind-bw4.
        rv_rolekind = gc_rolekind-hana.
      endif.
    endif.

  ENDMETHOD.

  METHOD get_profile_status.

    DATA:

      recommended_action           TYPE smensapnew-customized,
      profile_generation_necessary TYPE smensapnew-customized,
      invalid_activity_group       TYPE smensapnew-customized,
      no_authorization_data        TYPE smensapnew-customized.

    rv_status = 'RED'.

    CALL FUNCTION 'PRGN_CHECK_PROFILE_STATUS'
      EXPORTING
        activity_group               = iv_rolename
      IMPORTING
        led_color                    = rv_status
        recommended_action           = recommended_action
        profile_generation_necessary = profile_generation_necessary
        invalid_activity_group       = invalid_activity_group
        no_authorization_data        = no_authorization_data.

    IF invalid_activity_group EQ 'X' OR
       no_authorization_data EQ 'X' OR
       recommended_action EQ 'M' OR
       profile_generation_necessary EQ 'X'.
      rv_status = 'RED'.
    ENDIF.
  ENDMETHOD.

  METHOD get_block_description.
    rv_txtlg = iv_block.
    SELECT SINGLE txtlg
      INTO @rv_txtlg
      FROM zi_core_blocksinsystemt
      WHERE docblock = @iv_block AND
            language = @iv_language.
    IF sy-subrc <> 0.
      " In no text found for the langauge, find something
      SELECT  txtlg
        INTO @rv_txtlg
        FROM zi_core_blocksinsystemt UP TO 1 ROWS
        WHERE docblock = @iv_block.
      ENDSELECT.
    ENDIF.
  ENDMETHOD.


  METHOD get_block_from_rolename.
    DATA(lv_prefix_length) = strlen( gc_prefix-role_prefix ) .
    rv_block = iv_rolename+lv_prefix_length(4).
  ENDMETHOD.


  METHOD get_cluster_description.

    SELECT SINGLE clustertext
      FROM zi_core_clustert
      WHERE doccluster = @iv_cluster
      INTO @rv_description.
    IF sy-subrc <> 0.
      rv_description = iv_cluster.
    ENDIF.
  ENDMETHOD.


  METHOD get_cluster_from_block.
    " From a given block return the Cluster
    " I know that it must always be the first character
    " must someone somewhere might find another way, so this
    " one finds the last letter in ROOTx and that is the cluster
    SELECT SINGLE doccluster
      INTO @rv_cluster
      FROM zi_core_contentview
      WHERE docblock = @iv_block.
    IF sy-subrc <> 0.
      rv_cluster = iv_block(1).
    ENDIF.
  ENDMETHOD.

  METHOD get_abbreviation_from_rolename.
    DATA(lv_prefix_length) = strlen( gc_prefix-role_prefix ) +
                             strlen( get_virtual_block( iv_blocktype = gc_prefix-global_block ) ) +
                             strlen( gc_roletype-access ).
    rv_abbreviation = iv_rolename+lv_prefix_length(2).
  ENDMETHOD.

  METHOD get_cluster_from_rolename.
    rv_cluster = get_cluster_from_block( get_block_from_rolename( iv_rolename ) ).
  ENDMETHOD.

  METHOD get_role_description.
    " importing
    "   iv_rolename   type agr_name
    TRY.
        IF ir_authname IS SUPPLIED.
** Basically this section is only used when you deal with data roles
          CALL BADI ir_authname->get_texts
            EXPORTING
              iv_rolename = iv_rolename
            RECEIVING
              rt_texts    = rt_txtlg.
        ELSE.
** If call without <Block><roletype description> - <Block description>
          APPEND VALUE #( agr_name  = iv_rolename
                          line      = '000000'
                          text      = |{ zcl_core_role_admin=>get_block_from_rolename( iv_rolename ) } | &&
                                      |{ zcl_core_role_admin=>get_roletype_description( zcl_core_role_admin=>get_roletype_from_rolename( iv_rolename ) ) } - | &&
                                      |{ zcl_core_role_admin=>get_block_description( zcl_core_role_admin=>get_block_from_rolename( iv_rolename ) ) }| ) TO rt_txtlg ASSIGNING FIELD-SYMBOL(<ls_texts>).
          <ls_texts>-spras = 'E'.
          APPEND <ls_texts> TO rt_txtlg ASSIGNING <ls_texts>.
          <ls_texts>-spras = 'F'.
        ENDIF.
      CATCH cx_rs_msg.
        APPEND VALUE #( agr_name  = iv_rolename
                        line      = '000000'
                        text      = |{ iv_rolename } No text found| ) TO rt_txtlg ASSIGNING <ls_texts>.
        <ls_texts>-spras = 'E'.
        APPEND <ls_texts> TO rt_txtlg ASSIGNING <ls_texts>.
        <ls_texts>-spras = 'F'.
    ENDTRY.
  ENDMETHOD.

  METHOD get_rolename.
    " From a block, a type and a scope
    " find the role name needed

    " BWAUTHC for Content
    " BWAUTHD for developer
    " BWAUTHA for access etc...

**    DATA(lv_description) = to_upper( get_cluster_description(  get_cluster_from_block( iv_block ) ) ).
    CASE iv_roletype.
      WHEN gc_roletype-data.
        rv_rolename = |{ gc_prefix-role_prefix }{ iv_block }{ iv_roletype }|.
      WHEN OTHERS.
        if iv_block = zcl_core_role_admin=>get_virtual_block( iv_blocktype = zcl_core_role_admin=>gc_prefix-global_block ).
          rv_rolename = |{ gc_prefix-role_prefix }{ iv_block }{ iv_roletype }_{ gv_crossclusterdesc }|.
        else.
          rv_rolename = |{ gc_prefix-role_prefix }{ iv_block }{ iv_roletype }_{ to_upper( get_cluster_description(  get_cluster_from_block( iv_block ) ) ) }|.
        endif.
    ENDCASE.

  ENDMETHOD.


  METHOD get_roletype_description.
    CALL FUNCTION 'SXMS_GET_DOMAIN_TEXT'
      EXPORTING
        domname = 'ZCORE_ROLETYPE'
        value   = CONV domvalue_l( iv_roletype )
      IMPORTING
        text    = rv_description
      EXCEPTIONS
        OTHERS  = 8.
    assert sy-subrc = 0.
  ENDMETHOD.


  METHOD get_roletype_from_rolename.
    DATA(lv_prefix_length) = strlen( gc_prefix-role_prefix ) + strlen( get_virtual_block( iv_blocktype = gc_prefix-global_block ) ).
    rv_roletype = iv_rolename+lv_prefix_length(1).
  ENDMETHOD.


  METHOD get_template_from_roletype.
    " Basically we find the roletype and the corresponding
    " template, if the template have been created, we list
    " otherwise we leave if for next time
    DATA:
      lrs_roletype TYPE REF TO cl_abap_structdescr.

    IF iv_roletype = ' '.
      " if Roletype is empty use the composite role template
      rv_roletypetemplate = zcl_core_role_admin=>gc_prefix-compositerole.
    ELSE.
      rv_roletypetemplate = |{ gc_prefix-role_prefix }AUTH{ iv_roletype }TEMPLATE|.
    ENDIF.

    " The template must exists
    assert check_role_exists( rv_roletypetemplate ) = rs_c_true.

  ENDMETHOD.


  METHOD get_virtual_block.

    " Keep in mind that block type is not the same as the pre and postfix needed
    CASE iv_blocktype.
      WHEN gc_prefix-cluster_fix.
        rv_virtual_block = |{ iv_cluster }{ gv_all_postfix }|.
      WHEN gc_prefix-global_block.
        rv_virtual_block = gv_auth_block.
      WHEN OTHERS.
        " Programming error in the calling program
        ASSERT 1 = 0.
    ENDCASE.

  ENDMETHOD.

  METHOD static_set_detlevel.

    if iv_detlevel <> 0.
      gv_n_detlevel = gv_n_detlevel + iv_detlevel.
      if gv_n_detlevel < 1.
        gv_n_detlevel = 1.
      endif.
      if gv_n_detlevel > 9.
        gv_detlevel = '9'.
      else.
        move gv_n_detlevel to gv_detlevel.
      endif.
    endif.

  ENDMETHOD.


  METHOD static_do_message.

    DATA:
      ls_log    TYPE bal_s_log,
      lv_dummy  TYPE string,
      lv_ext_no TYPE bal_s_log-extnumber,
      ls_mdef   TYPE bal_s_mdef.

    IF gv_log_handle IS INITIAL.
      SELECT SINGLE object, subobject
        FROM balsub
        WHERE object    = 'ZBW' AND
              subobject = 'ZROLEGEN'
        INTO CORRESPONDING FIELDS OF @ls_log.
      " use transaction SLG0 and create the object
      " ZAUTH and subobject ZROLEGEN, you can add your own
      " description
      ASSERT sy-subrc = 0.

      " This is maybe not the most elegant way
      " we assume that the first message is the one
      " that tells the purpose
      ls_log-extnumber = iv_message.

      CALL FUNCTION 'BAL_LOG_CREATE'
        EXPORTING
          i_s_log                 = ls_log
        IMPORTING
          e_log_handle            = gv_log_handle
        EXCEPTIONS
          log_header_inconsistent = 4
          OTHERS                  = 8.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE rs_c_warning NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      gv_detlevel = '1'.
      gv_n_detlevel = 1.
    ENDIF.

    " a message comes as a text with information
    IF iv_message IS SUPPLIED.
      DATA lv_char1024 TYPE char1024.
      lv_char1024 = iv_message.
      CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
        EXPORTING
          i_log_handle   = gv_log_handle
          i_msgty        = iv_message_type
          i_text         = lv_char1024
          i_detlevel     = gv_detlevel
        IMPORTING
          e_s_msg_handle = rs_msglog
        EXCEPTIONS
          OTHERS         = 8.
      IF sy-subrc <> 0.
      ENDIF.
      IF ( iv_message_type = 'S' OR iv_message_type = 'I' OR iv_message_type = 'W' ).
        IF sy-batch = rs_c_true. "Background processing
          MESSAGE iv_message  TYPE iv_message_type.
        ELSE.
          CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
            EXPORTING
              text = iv_message.
        ENDIF.
      ELSE. " Critical message
        MESSAGE iv_message  TYPE iv_message_type DISPLAY LIKE rs_c_warning.
      ENDIF.
    ELSE. "" iv_message not supplied
      DATA ls_msg TYPE bal_s_msg.
      MOVE-CORRESPONDING syst TO ls_msg.
      ls_msg-detlevel = gv_detlevel.
      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle   = gv_log_handle
          i_s_msg        = ls_msg
        IMPORTING
          e_s_msg_handle = rs_msglog
        EXCEPTIONS
          OTHERS         = 0.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE rs_c_warning NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      IF ( sy-msgty = 'S' OR sy-msgty = 'I' OR sy-msgty = 'W' ).
        IF sy-batch = rs_c_true.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ELSE.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO gv_message.
          CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
            EXPORTING
              text = gv_message.
        ENDIF.
      ELSE.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE rs_c_warning.
      ENDIF. " non critical
    ENDIF. "" iv_message not supplied

    call METHOD static_set_detlevel( iv_detlevel ).

  ENDMETHOD.

  METHOD adapt.
*        IMPORTING
*          iv_roletype TYPE zcore_roletype
*          iv_cluster  TYPE zcore_cluster,
    TRY.
        CALL METHOD zcl_core_role=>adjust_from_template
          EXPORTING
            iv_cluster  = iv_cluster
            iv_roletype = iv_roletype.
      CATCH cx_rs_error INTO DATA(lrx_error).
        static_do_message( lrx_error->get_text(  ) ).
    ENDTRY.
  ENDMETHOD.

  METHOD static_delete_data_in_adso.

    CALL METHOD zcl_core_role_admin=>static_do_message( |Deleting the content of aDSO { iv_adsonum }| ).
    DATA(lr_adso) = cl_rso_adso=>factory(  iv_adsonum ).
    CALL METHOD lr_adso->if_rsd_dta~delete_data
      EXPORTING
        i_with_dialog  = rs_c_false
        i_incl_dime    = rs_c_true
        i_del_nls      = rs_c_false
      EXCEPTIONS
        cancelled      = 1
        not_found      = 2
        failed         = 3
        dta_used       = 4
        not_authorized = 5
        OTHERS         = 6.
    IF sy-subrc <> 0.
      CALL METHOD zcl_core_role_admin=>static_do_message( ).
      RAISE EXCEPTION TYPE cx_rs_error.
    ELSE.
      CALL METHOD zcl_core_role_admin=>static_do_message( |Data in aDSO deleted| ).
    ENDIF.

  ENDMETHOD.

  METHOD generate_all.
    TYPES:
      BEGIN OF ltys_block,
        doccluster TYPE zcore_cluster,
        roletype   TYPE zcore_roletype,
        docblock   TYPE zcore_block,
      END OF ltys_block,
      ltyt_block TYPE STANDARD TABLE OF ltys_block WITH NON-UNIQUE DEFAULT KEY.

    DATA lt_block TYPE ltyt_block.
    DATA lv_txt_adso TYPE rsoadsonm.
    DATA lv_authadso TYPE rsoadsonm.
    DATA lt_rng_block  TYPE RANGE OF zcore_block.

    SELECT b~doccluster, z~roletype, z~docblock
        FROM zcore_rolegen AS z INNER JOIN zi_core_blocksinsystem AS b
                ON z~docblock = b~docblock
        ORDER BY b~doccluster, z~roletype, z~docblock
        INTO CORRESPONDING FIELDS OF TABLE @lt_block.

    LOOP AT lt_block INTO DATA(ls_block).
      " and why do we have thie at new and at end... you see the GENERATE
      " procedure what originally created to be called from the program itself
      " but have now been promoted to handle the collective processing from
      " ZCORE_ROLEGEN

      " That will place GENERATE to the one stop for the creation of roles
      AT NEW doccluster.
        lv_txt_adso = |{ ls_block-doccluster }{ gc_prefix-adso_for_text }|.
        lv_authadso = |{ ls_block-doccluster }{ gc_prefix-adso_for_auth }|.
      ENDAT.

      APPEND VALUE #( sign   = rs_c_range_sign-including
                      option = rs_c_range_opt-equal
                      low    = ls_block-docblock ) TO lt_rng_block.

      AT END OF doccluster.
        TRY.
            CALL METHOD zcl_core_role_admin=>generate
              EXPORTING
                iv_roletype = ls_block-roletype
                iv_cluster  = zcl_core_role_admin=>get_cluster_from_block( ls_block-docblock )
                it_block    = lt_rng_block[]
                iv_txt_adso = lv_txt_adso
                iv_authadso = lv_authadso.
          CATCH cx_rs_not_found
                cx_rs_error INTO DATA(lrx_error).
            zcl_core_role_admin=>static_do_message( iv_message = lrx_error->get_text(  ) iv_message_type = rs_c_error ).
        ENDTRY.
      ENDAT.
    ENDLOOP.
  ENDMETHOD.

  METHOD verify_count_blocks.
    zcl_core_role_admin=>static_do_message( iv_message = |Counting the number of blocks per cluster| iv_detlevel = 1 ).
    select doccluster,
           count( DISTINCT docblock ) as NumBlock
      from zi_core_contentview
      where DocBlock not like '_ALL'
      group by DocCluster
      into table @data(lt_cluster_count_blocks).

    loop at lt_cluster_count_blocks ASSIGNING FIELD-SYMBOL(<ls_cluster_count_block>).
      if <ls_cluster_count_block>-numblock <= 1.
        zcl_core_role_admin=>static_do_message( iv_message = |For Cluster { <ls_cluster_count_block>-DocCluster } - | &&
                      |{ zcl_core_role_admin=>get_cluster_description( <ls_cluster_count_block>-DocCluster ) } number of blocks are { <ls_cluster_count_block>-numblock }| iv_message_type = rs_c_warning ).
      else.
        zcl_core_role_admin=>static_do_message( iv_message = |For Cluster { <ls_cluster_count_block>-DocCluster } - | &&
                      |{ zcl_core_role_admin=>get_cluster_description( <ls_cluster_count_block>-DocCluster ) } number of blocks are { <ls_cluster_count_block>-numblock }| ).
      endif.
    ENDLOOP.
    zcl_core_role_admin=>static_set_detlevel( -1 ).
  ENDMETHOD.

  METHOD verify_check_roles.
    zcl_core_role_admin=>static_do_message( iv_message = |What roles have been created for blocks in a cluster | iv_detlevel = 1 ).
    select *
        from zi_core_blocksinsystem
        order by DocCluster, DocBlock
        into table @data(lt_blocksinsystem).

    loop at lt_blocksinsystem into data(ls_clusterinsystem) GROUP BY ls_clusterinsystem-DocCluster.
      zcl_core_role_admin=>static_do_message( iv_message = |What roles have been created for blocks in cluster { ls_clusterinsystem-DocCluster } | iv_detlevel = 1 ).
      do.
        ASSIGN COMPONENT sy-index OF STRUCTURE zcl_core_role_admin=>gc_roletype to FIELD-SYMBOL(<lv_roletype>).
        if sy-subrc <> 0.
          exit.
        endif.
        zcl_core_role_admin=>static_do_message( iv_message = |Roles { zcl_core_role_admin=>get_roletype_description( <lv_roletype> ) } for cluster { ls_clusterinsystem-DocCluster }| iv_detlevel = 1 ).

        loop at group ls_clusterinsystem into data(ls_blocksinsystem).
          data(lv_rolename) = zcl_core_role_admin=>get_rolename( iv_block = ls_blocksinsystem-DocBlock iv_roletype = <lv_roletype> ).

          data(lv_msgtype) = rs_c_success.
          case <lv_roletype>.
            when zcl_core_role_admin=>gc_roletype-architect.
              if ls_blocksinsystem-DocBlock <> zcl_core_role_admin=>get_virtual_block( iv_blocktype = 'ALL' iv_cluster = ls_clusterinsystem-DocCluster ).
                continue.
              endif.
              lv_msgtype = rs_c_warning.
            when OTHERS.
              if ls_blocksinsystem-DocBlock = zcl_core_role_admin=>get_virtual_block( iv_blocktype = 'ALL' iv_cluster = ls_clusterinsystem-DocCluster ).
                lv_msgtype = rs_c_warning.
              endif.
          endcase.

          try.
              if zcl_core_role_admin=>check_role_exists( lv_rolename ) = rs_c_true.
                zcl_core_role_admin=>static_do_message( iv_message = |Cluster { ls_clusterinsystem-DocCluster } - { zcl_core_role_admin=>get_roletype_description( <lv_roletype> ) } -> Role { lv_rolename }  EXISTS| iv_message_type = lv_msgtype ).
              else.
                zcl_core_role_admin=>static_do_message( iv_message = |Cluster { ls_clusterinsystem-DocCluster } - { zcl_core_role_admin=>get_roletype_description( <lv_roletype> ) } -> Role { lv_rolename }  No role| iv_message_type = lv_msgtype ).
              endif.
            catch cx_rs_msg into data(lrx_msg).
              zcl_core_role_admin=>static_do_message( iv_message = lrx_msg->get_text( ) iv_message_type = lrx_msg->msgty ).
          endtry.
        ENDLOOP.
        zcl_core_role_admin=>static_set_detlevel( -1 ).
      enddo.
      zcl_core_role_admin=>static_set_detlevel( -1 ).
    ENDLOOP.
    zcl_core_role_admin=>static_set_detlevel( -1 ).
  ENDMETHOD.

  METHOD verify_orphen_roles.
    zcl_core_role_admin=>static_do_message( iv_message = |Find ourphen Roles| iv_detlevel = 1 ).

    data:
      lth_rolegen type HASHED TABLE OF zcore_rolegen with UNIQUE key docblock roletype,
      lth_cluster type HASHED TABLE OF zcore_cluster with UNIQUE key table_line,
      lth_block_data type HASHED TABLE OF zcore_block with UNIQUE key table_line,
      lth_block   type HASHED TABLE OF zcore_block   with UNIQUE key TABLE_LINE.

    select doccluster
        from zi_core_cluster
        into table @lth_cluster.
    select docblock
        from zi_core_blocksinsystem
        into table @lth_block.

    select *
        from zcore_rolegen
        into CORRESPONDING FIELDS OF table @lth_rolegen.

    select agr_name
       from agr_define
       where agr_name like 'BW4%' and
             agr_name not like 'BW4AUTH%'
       order by agr_name
       into table @data(lt_rolename).

    zcl_core_role_admin=>static_do_message( iv_message = |Verification of roles...| iv_detlevel = 1 ).

    data(lv_rolename) = lt_rolename[ 1 ]-agr_name.
    data(lv_cluster_text)  = zcl_core_role_admin=>get_cluster_description( zcl_core_role_admin=>get_cluster_from_rolename( lv_rolename ) ).
    zcl_core_role_admin=>static_do_message( iv_message = |Processing roles from cluster { lv_cluster_text }| iv_detlevel = 1 ).
    loop at lt_rolename into data(ls_rolename).

      if lv_cluster_text <> zcl_core_role_admin=>get_cluster_description( zcl_core_role_admin=>get_cluster_from_rolename( ls_rolename-agr_name ) ).
        zcl_core_role_admin=>static_set_detlevel( -1 ).
        lv_cluster_text  = zcl_core_role_admin=>get_cluster_description( zcl_core_role_admin=>get_cluster_from_rolename( lv_rolename ) ).
        zcl_core_role_admin=>static_do_message( iv_message = |Processing roles from cluster { lv_cluster_text }| iv_detlevel = 1 ).
      endif.
      lv_rolename = ls_rolename-agr_name.


      zcl_core_role_admin=>static_do_message( iv_message = |Verification of role { lv_rolename }| iv_detlevel = 1 ).
      data(lv_cluster)  = zcl_core_role_admin=>get_cluster_from_rolename( lv_rolename ).
      data(lv_block)    = zcl_core_role_admin=>get_block_from_rolename( lv_rolename ).
      data(lv_roletype) = zcl_core_role_admin=>get_roletype_from_rolename( lv_rolename ).

      read table lth_cluster TRANSPORTING NO FIELDS
        with TABLE KEY table_line = lv_cluster.
      if sy-subrc <> 0.
        zcl_core_role_admin=>static_do_message( iv_message = |Role { lv_rolename } - no cluster { lv_cluster }| iv_message_type = rs_c_error ).
      endif.

      read table lth_block TRANSPORTING NO FIELDS
        WITh TABLE KEY table_line = lv_block.
      if sy-subrc <> 0.
        zcl_core_role_admin=>static_do_message( iv_message = |Role { lv_rolename } - no Block { lv_block }| iv_message_type = rs_c_error ).
      endif.

      if not zcl_core_role_admin=>gc_roletype cs lv_roletype.
        zcl_core_role_admin=>static_do_message( iv_message = |Role { lv_rolename } - unknown role type: { lv_roletype }| iv_message_type = rs_c_error ).
      endif.

      if lv_block <> zcl_core_role_admin=>get_virtual_block( iv_blocktype = gc_prefix-cluster_fix iv_cluster = lv_cluster ) and
         lv_block <> zcl_core_role_admin=>get_virtual_block( iv_blocktype = gc_prefix-global_block ).
        read table lth_rolegen TRANSPORTING NO FIELDS
          with TABLE KEY docblock = lv_block
                         roletype = lv_roletype.
        if sy-subrc <> 0.
          zcl_core_role_admin=>static_do_message( iv_message = |Role { ls_rolename-agr_name } - not maintained in ZCORE_ROLEGEN for automatic generation | iv_message_type = rs_c_warning ).
        endif.
      endif.

      if lv_roletype = zcl_core_role_admin=>gc_roletype-data.
        read table lth_block_data TRANSPORTING NO FIELDS
          with TABLE KEY table_line = lv_block.
        if sy-subrc <> 0.
          "" Before we start we need to know what roles that need to be create
          zcl_core_role_admin=>static_do_message( iv_message = |Simulation of the Data Role Creation for block { lv_block }| iv_detlevel = 1 ).
          data lt_rng_block type range of zcore_block.
          refresh lt_rng_block.
          append value #( sign   = rs_c_range_sign-including
                          option = rs_c_range_opt-equal
                          low    = lv_block ) to lt_rng_block.
          try.
              call METHOD zcl_core_data_values=>create_data_role
                EXPORTING
                  iv_cluster = lv_cluster
                  it_block   = lt_rng_block[].
            CATCH cx_root into data(lrx_root).
              zcl_core_role_admin=>static_do_message( iv_message = |Error calling CREATE_DATA_ROLE => { lrx_root->get_text(  ) }| iv_message_type = rs_c_error ).
          endtry.
          zcl_core_role_admin=>static_set_detlevel( -1 ).
          insert lv_block into table lth_block_data.
        endif.

        try.
            call METHOD zcl_core_data_values=>get_authname_from_rolename( lv_rolename ).
          catch cx_rs_not_found.
            zcl_core_role_admin=>static_do_message( |The data role: { lv_rolename }, is not longer required| ).
        endtry.

      endif.
      zcl_core_role_admin=>static_set_detlevel( -1 ).

    endloop.
    "" Cluster inforamtion
    zcl_core_role_admin=>static_set_detlevel( -1 ).

    zcl_core_role_admin=>static_set_detlevel( -1 ).
  ENDMETHOD.


  METHOD verify.
    " A kind of clean up function - Find out if a role exist that should not exist and delete it
    " Let's say you delete a block in a cluster those

    zcl_core_role_admin=>static_do_message( iv_message = |Complete verification of setup and roles { sy-datum }/{ sy-uzeit }| iv_detlevel = 1 ).

    call METHOD:
      verify_count_blocks,
      verify_check_roles,
      verify_orphen_roles.

  ENDMETHOD.

  METHOD generate.
    " Finds all block defined in the ZCORE_ROLEGEN table and makes sure these are generated!
    " first make sure that the role must be created - if not delete it, if possible
    IF iv_roletype = zcl_core_role_admin=>gc_roletype-data.
      CALL METHOD:
         static_delete_data_in_adso( iv_authadso ),
         static_delete_data_in_adso( iv_txt_adso ).

      CALL METHOD zcl_core_data_values=>create_data_role_for_user
        EXPORTING
          iv_cluster  = iv_cluster
          iv_authadso = iv_authadso
          iv_txt_adso = iv_txt_adso.
      CALL METHOD zcl_core_data_values=>create_data_role
        EXPORTING
          iv_cluster  = iv_cluster
          it_block    = it_block
          iv_authadso = iv_authadso
          iv_txt_adso = iv_txt_adso.
    ELSE.
      SELECT *
        FROM zi_core_blocksinsystem
        WHERE doccluster = @iv_cluster AND
              docblock  IN @it_block
        INTO TABLE @DATA(lt_block).

      LOOP AT lt_block INTO DATA(lv_block)
             WHERE docblock <> zcl_core_role_admin=>get_virtual_block( iv_blocktype = 'ALL' iv_cluster = iv_cluster ).
        TRY.
            CALL METHOD zcl_core_role=>create( zcl_core_role_admin=>get_rolename( iv_block = lv_block-docblock iv_roletype = iv_roletype ) ).
            CALL METHOD zcl_core_role=>adjust( zcl_core_role_admin=>get_rolename( iv_block = lv_block-docblock iv_roletype = iv_roletype ) ).
          CATCH cx_rs_not_found INTO DATA(lrx_error).
            RAISE EXCEPTION TYPE cx_rs_error
              EXPORTING
                previous = lrx_error.
        ENDTRY.
      ENDLOOP.
    ENDIF.
    " Make sure to update the cluster role for the role type, regardless if this is DATA or anything else
    CALL METHOD zcl_core_role=>manage_cluster_role( iv_cluster = iv_cluster iv_roletype = iv_roletype ).

  ENDMETHOD.

ENDCLASS.
