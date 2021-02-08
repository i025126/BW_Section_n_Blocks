CLASS zcl_core_role DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

** This class deals with an adjust roles in the roles concept according to the templates
** it cannot create new roles - meaning in cases where new blocks are created you need
** to have a download executed and be able to upload the roles using PFCG.

** but the program will now ask for download in case you need to adjust existing roles
** from the templates - The setting for gv_adjustmentype controls what is done

  PUBLIC SECTION.

    CLASS-DATA:
      lr_admin TYPE REF TO zcl_core_role_admin,
      _message TYPE string.

    CLASS-METHODS:
      class_constructor,
      create
        IMPORTING
          iv_rolename TYPE agr_name
        RAISING
          cx_rs_not_found
          cx_rs_error,
      manage_cluster_role
        IMPORTING
          iv_roletype TYPE zcore_roletype
          iv_cluster  TYPE zcore_cluster
        RAISING
          cx_rs_error,
      adjust_from_template
        IMPORTING
          iv_roletype TYPE zcore_roletype
          iv_cluster  TYPE zcore_cluster OPTIONAL
        RAISING
          cx_rs_error,
      delete
        IMPORTING
          iv_rolename TYPE agr_name
        RAISING
          cx_rs_not_found
          cx_rs_error,
      adjust
        IMPORTING
          iv_rolename TYPE agr_name
        RAISING
          cx_rs_not_found
          cx_rs_error.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CLASS-METHODS:
      refresh_role_from_template_1
        IMPORTING
          iv_rolename TYPE agr_name
        RAISING
          cx_rs_msg
          cx_rs_not_found,
      refresh_role_from_template_2
        IMPORTING
          iv_rolename TYPE agr_name
        RAISING
          cx_rs_msg
          cx_rs_not_found,
      refresh_role_from_template_3
        IMPORTING
          iv_rolename TYPE agr_name
        RAISING
          cx_rs_msg
          cx_rs_not_found,
      _do_adjust_virtual_role
        IMPORTING
          iv_rolename TYPE agr_name
        RAISING
          cx_rs_error,
      _do_adjust_field_values
        IMPORTING
          iv_rolename          TYPE agr_name
          iv_block             TYPE zcore_block   OPTIONAL
          iv_cluster           TYPE zcore_cluster OPTIONAL
        CHANGING
          cv_something_changed TYPE rs_bool OPTIONAL
          cv_value             TYPE clike,
      _do_convert_to_rolename
        IMPORTING
          iv_rolename TYPE agr_name
        CHANGING
          ct_table    TYPE ANY TABLE,
      _do_generate_profile
        IMPORTING
          iv_rolename TYPE agr_name
        RAISING
          cx_rs_error.

ENDCLASS.

CLASS zcl_core_role IMPLEMENTATION.

  METHOD class_constructor.
    CREATE OBJECT lr_admin.
  ENDMETHOD.

  METHOD manage_cluster_role.

    DATA:
      lt_messages TYPE sprot_u_tab.

    MESSAGE s102(zcore) WITH iv_cluster.

    TRY.
        DATA(lv_all_block)    = zcl_core_role_admin=>get_virtual_block( iv_cluster = iv_cluster iv_blocktype = zcl_core_role_admin=>gc_prefix-cluster_fix ).

        IF iv_roletype = zcl_core_role_admin=>gc_roletype-operation OR iv_roletype = zcl_core_role_admin=>gc_roletype-content.
          " --
          " We only need the architect role for 'C' and 'O'
          " --
          " Make sure to create the architect role
          " Copy the role type 'B'... this will be BW4xALLB
          " Role name is now BW4xALLB_.... =
          DATA(lv_roletype) = zcl_core_role_admin=>gc_roletype-architect.
          DATA(lv_role_architect) = zcl_core_role_admin=>get_rolename( iv_block = lv_all_block iv_roletype = lv_roletype ).
          "" create the role - if not already
          CALL METHOD zcl_core_role=>create( lv_role_architect ).
          "" adjust the profile to cluster - if not generated
          CALL METHOD zcl_core_role=>adjust( lv_role_architect ).
        ENDIF.

        " --- Create the of type intended ---
        " Since the create function will use a composite role template for the AUTH and xALL
        " not do an activation of the profile - we can reuse the function
        DATA(lv_all_rolename) = zcl_core_role_admin=>get_rolename( iv_block = lv_all_block iv_roletype = iv_roletype ).
        CALL METHOD zcl_core_role=>create( iv_rolename = lv_all_rolename ).
        CALL METHOD zcl_core_role=>adjust( iv_rolename = lv_all_rolename ).

      CATCH cx_rs_msg.
        zcl_core_role_admin=>static_do_message(  ).
    ENDTRY.
  ENDMETHOD.

  METHOD _do_adjust_virtual_role.

    DATA(lv_cluster) = zcl_core_role_admin=>get_cluster_from_rolename( iv_rolename ).
    DATA(lv_roletype) = zcl_core_role_admin=>get_roletype_from_rolename( iv_rolename ).

    IF lv_roletype = zcl_core_role_admin=>gc_roletype-content OR lv_roletype = zcl_core_role_admin=>gc_roletype-operation.
      DATA(lv_role_architect) = zcl_core_role_admin=>get_rolename( iv_block = zcl_core_role_admin=>get_block_from_rolename( iv_rolename ) iv_roletype = zcl_core_role_admin=>gc_roletype-architect ).
    ELSE.
      lv_role_architect = 'DFWEFEFGRGBRTGDF'.
    ENDIF.

    DATA lt_roles_in_cluster TYPE STANDARD TABLE OF agr_txt.
    "" Find all roles for cluster available... regardless of status of profile
    DATA(lv_where) = |BW4{ lv_cluster }___{ lv_roletype }%|.
    SELECT
        agr_name
      FROM agr_define
      WHERE ( agr_name LIKE @lv_where OR             " Find all roles in cluster of same type
              agr_name = @lv_role_architect ) AND    " including the architext
              agr_name <> @iv_rolename               " but not the role it self
            INTO CORRESPONDING FIELDS OF TABLE @lt_roles_in_cluster.

    CALL FUNCTION 'PRGN_RFC_ADD_AGRS_TO_COLL_AGR'
      EXPORTING
        activity_group                = iv_rolename
        check_namespace               = 'X'
        enqueue                       = 'X'
        profile_comparison            = 'X'
        no_dialog                     = 'X'
      TABLES
        activity_groups               = lt_roles_in_cluster
      EXCEPTIONS
        activity_group_does_not_exist = 1
        no_collective_activity_group  = 2
        activity_group_enqueued       = 3
        namespace_problem             = 4
        not_authorized                = 5
        authority_incomplete          = 6
        OTHERS                        = 7.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_rs_msg.
    ENDIF.

    DATA(lv_auth_rolename) = zcl_core_role_admin=>get_rolename( iv_block = zcl_core_role_admin=>get_virtual_block( iv_blocktype = zcl_core_role_admin=>gc_prefix-global_block )
                                                                iv_roletype = lv_roletype ).
    IF zcl_core_role_admin=>check_role_exists( lv_auth_rolename ) = rs_c_true.
      " So the BW4AUTH<roletype>_ALL does exist
      REFRESH lt_roles_in_cluster.
      APPEND VALUE #( agr_name = iv_rolename ) TO lt_roles_in_cluster.
      DATA lt_bapiret2 TYPE STANDARD TABLE OF bapiret2.
      CALL FUNCTION 'PRGN_RFC_ADD_AGRS_TO_COLL_AGR'
        EXPORTING
          activity_group                = lv_auth_rolename
          check_namespace               = 'X'
          enqueue                       = 'X'
          profile_comparison            = 'X'
          no_dialog                     = 'X'
        TABLES
          activity_groups               = lt_roles_in_cluster
          return                        = lt_bapiret2
        EXCEPTIONS
          activity_group_does_not_exist = 1
          no_collective_activity_group  = 2
          activity_group_enqueued       = 3
          namespace_problem             = 4
          not_authorized                = 5
          authority_incomplete          = 6
          OTHERS                        = 7.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH
                       sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'W'.
        zcl_core_role_admin=>static_do_message(  ).
      ELSE.
        LOOP AT lt_bapiret2 ASSIGNING FIELD-SYMBOL(<ls_bapiret2>).
          MESSAGE ID <ls_bapiret2>-id TYPE <ls_bapiret2>-type NUMBER <ls_bapiret2>-number
                         WITH <ls_bapiret2>-message_v1 <ls_bapiret2>-message_v2 <ls_bapiret2>-message_v3 <ls_bapiret2>-message_v4 INTO _message.
          zcl_core_role_admin=>static_do_message(  ).
        ENDLOOP.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD create.
*        IMPORTING
*          iv_rolename TYPE agr_name
*        RAISING
*          cx_rs_not_found
*          cx_rs_error,

** The creation of an InfoArea is done in the code
** IF_RSAWBN_FOLDER_TREE~CREATE_NODE
**
    MESSAGE s108(zcore) WITH iv_rolename.
    zcl_core_role_admin=>static_do_message( ).
    zcl_core_role_admin=>static_set_detlevel( 1 ).

    IF zcl_core_role_admin=>check_role_exists( iv_rolename = iv_rolename ) = rs_c_false.

      " Never create AUTH roles
      if  zcl_core_role_admin=>get_block_from_rolename( iv_rolename ) = zcl_core_role_admin=>get_virtual_block( iv_blocktype = zcl_core_role_admin=>gc_prefix-global_block ).
        return.
      endif.
      IF zcl_core_role_admin=>get_block_from_rolename( iv_rolename ) = zcl_core_role_admin=>get_virtual_block( iv_blocktype = zcl_core_role_admin=>gc_prefix-cluster_fix
                                                                                                               iv_cluster = zcl_core_role_admin=>get_cluster_from_rolename( iv_rolename ) ) AND
           "" Architect roles as ALWAYS created on cluster level
           zcl_core_role_admin=>get_roletype_from_rolename( iv_rolename ) <> zcl_core_role_admin=>gc_roletype-architect .
        DATA(lv_composite_role) = rs_c_true.
        DATA(lv_role_template) = zcl_core_role_admin=>gc_prefix-compositerole.
      ELSE.
        lv_composite_role = rs_c_false.
        lv_role_template = zcl_core_role_admin=>get_template_from_roletype( zcl_core_role_admin=>get_roletype_from_rolename( iv_rolename ) ).
      ENDIF.

      DATA(lv_txtlg) = zcl_core_role_admin=>get_role_description( iv_rolename ).

      MESSAGE s109(zcore) WITH iv_rolename.
      zcl_core_role_admin=>static_do_message(  ).

      DATA: lt_messages TYPE sprot_u_tab.
* Copy procedure
      CALL FUNCTION 'PRGN_COPY_ACTIVITY_GROUP'
        EXPORTING
          source_activity_group = lv_role_template
          target_activity_group = iv_rolename
          display_log           = rs_c_false
        IMPORTING
          messages              = lt_messages
        EXCEPTIONS
          action_cancelled      = 1
          not_authorized        = 2
          target_already_exists = 3
          source_does_not_exist = 4
          internal_error        = 5
          OTHERS                = 6.
      CASE sy-subrc.
        WHEN 0.
          LOOP AT lt_messages ASSIGNING FIELD-SYMBOL(<ls_messages>).
            MESSAGE ID     <ls_messages>-ag TYPE   'S' NUMBER <ls_messages>-msgnr WITH   <ls_messages>-var1 <ls_messages>-var2 <ls_messages>-var3 <ls_messages>-var4 INTO _message.
            zcl_core_role_admin=>static_do_message( ).
          ENDLOOP.
          MESSAGE s407(s#) INTO _message.
          zcl_core_role_admin=>static_do_message( ).
        WHEN OTHERS.
          IF NOT lt_messages IS INITIAL.
            LOOP AT lt_messages ASSIGNING <ls_messages>.
              MESSAGE ID     <ls_messages>-ag TYPE   'S' NUMBER <ls_messages>-msgnr WITH   <ls_messages>-var1 <ls_messages>-var2 <ls_messages>-var3 <ls_messages>-var4 INTO _message.
              zcl_core_role_admin=>static_do_message( ).
            ENDLOOP.
          ELSE.
            MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E'.
            zcl_core_role_admin=>static_do_message( ).
          ENDIF.
          RAISE EXCEPTION TYPE cx_rs_msg.
      ENDCASE.
      zcl_core_role_admin=>static_do_message( |Copy done... Create the role { iv_rolename } | ).

      TRY.
          " Try to see if a BADI implementation of the zbapi_data_role_value
          " can be accompslied, since this is the same context the conversion
          " from rolename to InfoObject will work... otherwise we just take default
          DATA:
            lr_infoobjecct TYPE REF TO zbapi_data_role_value.
          DATA(lv_infoobject) = zcl_core_data_values=>get_infoobject_from_rolename( iv_rolename = iv_rolename ).
          GET BADI lr_infoobjecct
            FILTERS
              zcore_role_data_filter = lv_infoobject.
          " Here the badi Implementation, must supply the text
          DATA(lt_texts) = zcl_core_role_admin=>get_role_description( iv_rolename = iv_rolename ir_authname = lr_infoobjecct ).

          IF lt_texts IS INITIAL.
            " No data returned, let's take standard anyway
            RAISE EXCEPTION TYPE cx_rs_not_found.
          ENDIF.

        CATCH cx_rs_not_found
              cx_badi_not_implemented.
          " No conversion, no badi, no implementation
          lt_texts = zcl_core_role_admin=>get_role_description( iv_rolename ).
      ENDTRY.

      CALL FUNCTION 'PRGN_RFC_CHANGE_TEXTS'
        EXPORTING
          activity_group = iv_rolename
          no_dialog      = rs_c_true
        TABLES
          texts          = lt_texts.

      MESSAGE |Assuming that the role { iv_rolename } have been created from template { lv_role_template }...| TYPE 'S'.

      IF lv_composite_role = rs_c_false.
        " The profile for a composite role cannot be generated
        CALL FUNCTION 'SUPRN_PROFILE_BATCH'
          EXPORTING
            act_objid        = iv_rolename
            enqueue          = 'X'
          EXCEPTIONS
            objid_not_found  = 1
            no_authorization = 2
            enqueue_failed   = 3
            not_generated    = 4
            OTHERS           = 5.
        IF sy-subrc <> 0.
          zcl_core_role_admin=>static_do_message(  ).
        ELSE.
          zcl_core_role_admin=>static_do_message( |Profile for { iv_rolename }-{ lt_texts[ 1 ]-text } generated| ).
        ENDIF.
      ENDIF.

      zcl_core_role_admin=>static_do_message( |Role { iv_rolename } is created from { lv_role_template }, named { lt_texts[ 1 ]-text }| ).
    ELSE.
      zcl_core_role_admin=>static_do_message( |Role { iv_rolename } already existsw | ).
    ENDIF.
    zcl_core_role_admin=>static_set_detlevel( -1 ).

  ENDMETHOD.

  METHOD delete.

    DATA
      lv_error_flag TYPE char1.

    CALL METHOD zcl_core_role_admin=>static_do_message( iv_message = |Start delete of role { iv_rolename }| iv_detlevel = 1 ).

    CALL FUNCTION 'PRGN_ACTIVITY_GROUP_DELETE'
      EXPORTING
        activity_group               = iv_rolename
        distribute                   = 'X'
      IMPORTING
        error_flag                   = lv_error_flag
      EXCEPTIONS
        child_agr_exists             = 1
        deletion_in_target_cancelled = 2
        user_cancels_action          = 3
        OTHERS                       = 3.
    CASE sy-subrc.
      WHEN 0.
        CASE lv_error_flag.
          WHEN 'A'.
            zcl_core_role_admin=>static_do_message( |No delete => Missing authorizations for object PLOG| ).
          WHEN 'H'.
            zcl_core_role_admin=>static_do_message( |No delete => Deletion of HR-ORG assignments failed| ).
          WHEN 'L'.
            zcl_core_role_admin=>static_do_message( |No delete => Lock of single role to be deleted or assigned collective roles| ).
          WHEN 'R'.
            zcl_core_role_admin=>static_do_message( |No delete =>  Automatic recording failed| ).
          WHEN 'T'.
            zcl_core_role_admin=>static_do_message( |No delete => Technical settings forbid role deletion| ).
          WHEN 'U'.
            zcl_core_role_admin=>static_do_message( |No delete => Problems when deleting user assignments| ).
          WHEN ' '.
            MESSAGE s428(s#) WITH iv_rolename INTO _message.
            zcl_core_role_admin=>static_do_message( ).
        ENDCASE.
      WHEN OTHERS.
        zcl_core_role_admin=>static_do_message( ).
    ENDCASE.
    CALL METHOD zcl_core_role_admin=>static_set_detlevel( -1 ).

  ENDMETHOD.

  METHOD adjust_from_template.
*        IMPORTING
*          iv_roletype TYPE zcore_roletype
*          iv_cluster  type zcore_cluster OPTIONAL
*        RAISING
*          cx_rs_error

    "" do make sure new transaction codes are available
    DATA lt_rolenames TYPE STANDARD TABLE OF agr_name.

    IF iv_cluster IS INITIAL.
      "" First we select all the roles that are to be there
      SELECT concat( concat( concat( concat( '_ZZ_' , docblock ), @iv_roletype ), '_' ), \_cluster\_text-clustertext ) AS agr_name
        FROM zi_core_blocksinsystem
        WHERE \_cluster\_text-language = 'E'
        INTO TABLE @lt_rolenames.
    ELSE.
      "" First we select all the roles that are to be there
      SELECT concat( concat( concat( concat( '_ZZ_', docblock ), @iv_roletype ), '_' ), \_cluster\_text-clustertext ) AS agr_name
        FROM zi_core_blocksinsystem
        WHERE \_cluster\_text-language = 'E' AND
              doccluster = @iv_cluster
        INTO TABLE @lt_rolenames.
    ENDIF.

    "
    replace all OCCURRENCES OF '_ZZ_' IN TABLE lt_rolenames WITH zcl_core_role_admin=>gc_prefix-role_prefix.

    "" then the once that are there
    IF lt_rolenames IS NOT INITIAL.
      SELECT DISTINCT
          agr_name
        FROM agr_flags
        FOR ALL ENTRIES IN @lt_rolenames
        WHERE agr_name   = @lt_rolenames-table_line AND
              flag_type  = 'COLL_AGR' AND
              flag_value = @rs_c_false
        INTO TABLE @lt_rolenames. "" this is just for testing.
    ENDIF.

    zcl_core_role_admin=>static_do_message( iv_message = |Found { lines( lt_rolenames ) } roles of the template { zcl_core_role_admin=>get_template_from_roletype( iv_roletype ) } - Roletype { iv_roletype } | iv_detlevel = 1 ).

    LOOP AT lt_rolenames INTO DATA(lv_rolename).
      TRY.
          CALL METHOD:
            refresh_role_from_template_1( lv_rolename ),
            refresh_role_from_template_2( lv_rolename ),
            refresh_role_from_template_3( lv_rolename ),
            _do_generate_profile( lv_rolename ).

        CATCH cx_rs_msg.
          zcl_core_role_admin=>static_do_message( ).
      ENDTRY.
    ENDLOOP.

*    CALL METHOD _do_adjust_structure
*      EXPORTING
*        iv_roletype  = iv_roletype
*        it_rolenames = lt_rolenames.
*
*    CALL METHOD _do_adjust_transactions
*      EXPORTING
*        iv_roletype  = iv_roletype
*        it_rolenames = lt_rolenames.
*
*    CALL METHOD _do_adjust_authorization
*      EXPORTING
*        iv_roletype  = iv_roletype
*        it_rolenames = lt_rolenames.
*
*    zcl_core_role_admin=>static_do_message( iv_message = |Generation of profiles| iv_detlevel = 1 ).
*    LOOP AT lt_rolenames INTO DATA(lv_rolename).
*      TRY.
*          _do_generate_profile( lv_rolename ).
*        CATCH cx_rs_error.
*      ENDTRY.
*    ENDLOOP.
    zcl_core_role_admin=>static_set_detlevel( -1 ).

  ENDMETHOD.

  METHOD adjust.
    " so now we have to read the information from
    " the template role and adjust the role me

    TRY.
        MESSAGE s110(zcore) WITH iv_rolename INTO _message.
        zcl_core_role_admin=>static_do_message( iv_detlevel = 1 ).
        " The role must exists
        IF zcl_core_role_admin=>check_role_exists( iv_rolename ) <> rs_c_true.
          zcl_core_role_admin=>static_do_message( |Role { iv_rolename } does not exists and cannot be adjusted| ).
          RAISE EXCEPTION TYPE cx_spau_object_processing.
        ENDIF.
        " The profile must be green
        IF zcl_core_role_admin=>get_profile_status( iv_rolename ) <> 'GREEN'.
          zcl_core_role_admin=>static_do_message( |Role { iv_rolename } does not have an active profile (Green) - will try and generate| ).
          _do_generate_profile( iv_rolename ).
        ENDIF.
        " not a template
        IF iv_rolename = zcl_core_role_admin=>get_template_from_roletype( zcl_core_role_admin=>get_roletype_from_rolename( iv_rolename ) ).
          zcl_core_role_admin=>static_do_message( |Role { iv_rolename } is a template role| ).
          RAISE EXCEPTION TYPE cx_spau_object_processing.
        ENDIF.

        IF ( zcl_core_role_admin=>get_virtual_block( zcl_core_role_admin=>gc_prefix-global_block ) = zcl_core_role_admin=>get_block_from_rolename( iv_rolename ) OR
             zcl_core_role_admin=>get_virtual_block( iv_blocktype = zcl_core_role_admin=>gc_prefix-cluster_fix
                                                      iv_cluster = zcl_core_role_admin=>get_cluster_from_rolename( iv_rolename ) ) =  zcl_core_role_admin=>get_block_from_rolename( iv_rolename ) ) and
             zcl_core_role_admin=>get_roletype_from_rolename( iv_rolename ) <> zcl_core_role_admin=>gc_roletype-architect.
          CALL METHOD _do_adjust_virtual_role( iv_rolename ).
        ELSE.

          DATA(lv_block)   = zcl_core_role_admin=>get_block_from_rolename( iv_rolename ).
          DATA(lv_cluster) = zcl_core_role_admin=>get_cluster_from_rolename( iv_rolename ).

          IF lv_cluster IS INITIAL OR lv_block IS INITIAL.
            MESSAGE w112(zcore) WITH iv_rolename.
            zcl_core_role_admin=>static_do_message( ).
            RAISE EXCEPTION TYPE cx_spau_object_processing.
          ENDIF.

          DATA:
            lt_1251 TYPE STANDARD TABLE OF pt1251.


          zcl_core_role_admin=>static_do_message( |Loading profile (1251) for role { iv_rolename }| ).

          CALL FUNCTION 'PRGN_1251_READ_FIELD_VALUES'
            EXPORTING
              activity_group    = iv_rolename
            TABLES
              field_values      = lt_1251
            EXCEPTIONS
              no_data_available = 8
              error_message     = 7
              OTHERS            = 4.
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE cx_rs_msg.
          ELSE.
            zcl_core_role_admin=>static_do_message( |Loaded { lines( lt_1251 ) } field values from profile of role { iv_rolename }| ).
          ENDIF.

          DATA lv_update TYPE rs_bool VALUE rs_c_false.
          LOOP AT lt_1251 ASSIGNING FIELD-SYMBOL(<ls_1251>).
            _do_adjust_field_values( EXPORTING iv_rolename = iv_rolename CHANGING cv_something_changed = lv_update cv_value = <ls_1251>-low ).
            _do_adjust_field_values( EXPORTING iv_rolename = iv_rolename CHANGING cv_something_changed = lv_update cv_value = <ls_1251>-high ).
          ENDLOOP.

          IF lv_update = rs_c_false.
            zcl_core_role_admin=>static_do_message( |No adjustment of role { iv_rolename } required| ).
            RAISE EXCEPTION TYPE cx_spau_object_processing.
          ENDIF.

          zcl_core_role_admin=>static_do_message( |Update field values for role { iv_rolename }| ).
          CALL FUNCTION 'PRGN_1251_SAVE_FIELD_VALUES'
            EXPORTING
              activity_group = iv_rolename
            TABLES
              field_values   = lt_1251
            EXCEPTIONS
              error_message  = 7
              OTHERS         = 4.
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE cx_rs_msg.
          ENDIF.

          CALL FUNCTION 'PRGN_UPDATE_DATABASE'
            EXCEPTIONS
              error_message = 7
              OTHERS        = 4.
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE cx_rs_msg.
          ENDIF.

          CALL FUNCTION 'PRGN_CLEAR_BUFFER'
            EXCEPTIONS
              error_message = 7
              OTHERS        = 4.
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE cx_rs_msg.
          ENDIF.

          zcl_core_role_admin=>static_do_message( |Generation of profile for role { iv_rolename }| ).
          _do_generate_profile( iv_rolename ).

        ENDIF.

        MESSAGE s111(zcore) WITH iv_rolename INTO _message.
        zcl_core_role_admin=>static_do_message( ).


      CATCH cx_rs_msg into data(lrx_msg).
        zcl_core_role_admin=>static_do_message(   ).
        zcl_core_role_admin=>static_do_message( |Error occoured during update of profile of role { iv_rolename }... see previous message| ).
      CATCH cx_spau_object_processing.
        " Some of the checks was executed and the profile was not to be corrected
      CATCH cx_rs_not_found.
        " Either this is a role template or this is processing of a role with a template
        BREAK-POINT.
    ENDTRY.
    zcl_core_role_admin=>static_set_detlevel( -1 ).

  ENDMETHOD.

  METHOD _do_generate_profile.
    SELECT SINGLE flag_value
        FROM agr_flags
        WHERE agr_name  = @iv_rolename AND
              flag_type = 'COLL_AGR'
        INTO @DATA(lv_is_coll_role).
    IF sy-subrc <> 0 OR lv_is_coll_role = rs_c_true.
      " Composite role
    ELSE.
      CALL FUNCTION 'SUPRN_PROFILE_BATCH'
        EXPORTING
          act_objid        = iv_rolename
          enqueue          = 'X'
        EXCEPTIONS
          objid_not_found  = 1
          no_authorization = 2
          enqueue_failed   = 3
          not_generated    = 4
          OTHERS           = 5.
      IF sy-subrc <> 0.
        zcl_core_role_admin=>static_do_message( ).
        zcl_core_role_admin=>static_do_message( |Could not generate the profile for role { iv_rolename }| ).
        RAISE EXCEPTION TYPE cx_rs_msg.
      ELSE.
        zcl_core_role_admin=>static_do_message( |Profile successfully generate for role { iv_rolename }| ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD refresh_role_from_template_1.
    DATA:
      lt_definition     TYPE STANDARD TABLE OF agr_define,
      lt_actgroup_flags TYPE STANDARD TABLE OF agr_flags,
      lt_actgroup_atts  TYPE STANDARD TABLE OF agr_atts,
      lt_actgroup_mini  TYPE STANDARD TABLE OF agr_mini,
      lt_actgroup_minit TYPE STANDARD TABLE OF agr_minit,
      lt_actgroup_mapp  TYPE STANDARD TABLE OF agr_mapp,
      lt_actgroup_hpage TYPE STANDARD TABLE OF agr_hpage,
      lt_return         TYPE STANDARD TABLE OF bapiret2.

    DATA(lv_role_template) = zcl_core_role_admin=>get_template_from_roletype( zcl_core_role_admin=>get_roletype_from_rolename( iv_rolename ) ).

    CALL METHOD zcl_core_role_admin=>static_do_message( iv_message = |Adjustment 1 role { iv_rolename } from template { lv_role_template }| iv_detlevel = 1 ).

    CALL FUNCTION 'PRGN_STRU_LOAD_DEFINITION'
      EXPORTING
        activity_group   = lv_role_template
      TABLES
        i_definition     = lt_definition
        i_actgroup_flags = lt_actgroup_flags
        i_actgroup_atts  = lt_actgroup_atts
        i_actgroup_mini  = lt_actgroup_mini
        i_actgroup_minit = lt_actgroup_minit
        i_actgroup_mapp  = lt_actgroup_mapp
        i_actgroup_hpage = lt_actgroup_hpage
        return           = lt_return
      EXCEPTIONS
        not_authorized   = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      zcl_core_role_admin=>static_do_message( ).
    ENDIF.

    zcl_core_role_admin=>static_do_message( |Processing adjustment of structure - role { iv_rolename }| ).

    CALL METHOD _do_convert_to_rolename( EXPORTING iv_rolename = iv_rolename CHANGING ct_table = lt_actgroup_flags ).
    CALL METHOD _do_convert_to_rolename( EXPORTING iv_rolename = iv_rolename CHANGING ct_table = lt_actgroup_atts ).
    CALL METHOD _do_convert_to_rolename( EXPORTING iv_rolename = iv_rolename CHANGING ct_table = lt_actgroup_mini ).
    CALL METHOD _do_convert_to_rolename( EXPORTING iv_rolename = iv_rolename CHANGING ct_table = lt_actgroup_minit ).
    CALL METHOD _do_convert_to_rolename( EXPORTING iv_rolename = iv_rolename CHANGING ct_table = lt_actgroup_mapp ).
    CALL METHOD _do_convert_to_rolename( EXPORTING iv_rolename = iv_rolename CHANGING ct_table = lt_actgroup_hpage ).

    zcl_core_role_admin=>static_do_message( |Processing changes to structure for role { iv_rolename }| ).
    CALL FUNCTION 'PRGN_STRU_SAVE_DEFINITION'
      EXPORTING
        activity_group   = iv_rolename
      TABLES
        i_actgroup_flags = lt_actgroup_flags
        i_actgroup_atts  = lt_actgroup_atts
        i_actgroup_mini  = lt_actgroup_mini
        i_actgroup_minit = lt_actgroup_minit
        i_actgroup_mapp  = lt_actgroup_mapp
        i_actgroup_hpage = lt_actgroup_hpage
        return           = lt_return
      EXCEPTIONS
        not_authorized   = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_rs_msg.
    ENDIF.

    CALL FUNCTION 'PRGN_UPDATE_DATABASE'.
    CALL FUNCTION 'PRGN_CLEAR_BUFFER'.

    zcl_core_role_admin=>static_set_detlevel( -1 ).
  ENDMETHOD.

  METHOD refresh_role_from_template_2.

    DATA:
      lt_agr_tcodes TYPE STANDARD TABLE OF agr_tcodes.

    DATA(lv_role_template) = zcl_core_role_admin=>get_template_from_roletype( zcl_core_role_admin=>get_roletype_from_rolename( iv_rolename ) ).

    CALL METHOD zcl_core_role_admin=>static_do_message( iv_message = |Adjustment of transaction role { iv_rolename } from template { lv_role_template }| iv_detlevel = 1 ).

    CALL FUNCTION 'PRGN_STRU_GET_TCODES'
      EXPORTING
        activity_group = lv_role_template
      TABLES
        i_agr_tcodes   = lt_agr_tcodes.

    zcl_core_role_admin=>static_do_message( |Check with assignment of { lines( lt_agr_tcodes ) } transaction from role { lv_role_template } | ).
    CALL METHOD _do_convert_to_rolename( EXPORTING iv_rolename = iv_rolename CHANGING ct_table = lt_agr_tcodes ).

    TRY.
        zcl_core_role_admin=>static_do_message( |Update role { iv_rolename } with new transaction| ).
        CALL FUNCTION 'PRGN_1221_SAVE_TRANSACTIONS'
          EXPORTING
            activity_group = iv_rolename
          TABLES
            i_agr_tcodes   = lt_agr_tcodes.
        CALL FUNCTION 'PRGN_UPDATE_DATABASE'.
        CALL FUNCTION 'PRGN_CLEAR_BUFFER'.
      CATCH cx_rs_msg.
        zcl_core_role_admin=>static_do_message( ).
    ENDTRY.
    zcl_core_role_admin=>static_set_detlevel( -1 ).

  ENDMETHOD.

  METHOD refresh_role_from_template_3.

    TYPES:
      ltyt_agr_1250 TYPE STANDARD TABLE OF pt1250   WITH NON-UNIQUE DEFAULT KEY,
      ltyt_agr_1251 TYPE STANDARD TABLE OF pt1251   WITH NON-UNIQUE DEFAULT KEY
                      WITH NON-UNIQUE SORTED KEY auth_key COMPONENTS object auth.

    DATA:
      lt_role_1250     TYPE ltyt_agr_1250,
      lt_role_1251     TYPE ltyt_agr_1251,
      lt_template_1250 TYPE ltyt_agr_1250,
      lt_template_1251 TYPE ltyt_agr_1251.

    DATA:
      lv_role_profile     TYPE agprofile,
      lv_template_profile TYPE agprofile.

    TRY.
        DATA(lv_role_template) = zcl_core_role_admin=>get_template_from_roletype( zcl_core_role_admin=>get_roletype_from_rolename( iv_rolename ) ).
        SELECT SINGLE profile FROM agr_1016 WHERE agr_name = @lv_role_template AND counter = '000001' INTO @lv_template_profile.
        CALL FUNCTION 'PRGN_1250_READ_AUTH_DATA'
          EXPORTING
            activity_group = lv_role_template
          TABLES
            auth_data      = lt_template_1250
          EXCEPTIONS
            OTHERS         = 8.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE cx_rs_msg.
        ENDIF.

        CALL FUNCTION 'PRGN_1251_READ_FIELD_VALUES'
          EXPORTING
            activity_group = lv_role_template
          TABLES
            field_values   = lt_template_1251
          EXCEPTIONS
            OTHERS         = 8.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE cx_rs_msg.
        ENDIF.

        CALL METHOD zcl_core_role_admin=>static_do_message( iv_message = |Adjustment of 125x authorization in role { iv_rolename } from template { lv_role_template }| iv_detlevel = 1 ).

        TRY.
            REFRESH:
              lt_role_1250,
              lt_role_1251.
            DATA(lv_block)   = zcl_core_role_admin=>get_block_from_rolename( iv_rolename ).
            DATA(lv_cluster) = zcl_core_role_admin=>get_cluster_from_block( lv_block ).
            "" load all requiredi nformation of the role
            "" first find the profile that will be the first 10 character of any profile
            SELECT SINGLE profile FROM agr_1016 WHERE agr_name = @iv_rolename AND counter = '000001' INTO @lv_role_profile.
            ASSERT sy-subrc = 0.

            LOOP AT lt_template_1250 ASSIGNING FIELD-SYMBOL(<ls_template_1250>).

              " get the role PROFILE from the template profile
              DATA(lv_role_auth) = <ls_template_1250>-auth.
              REPLACE FIRST OCCURRENCE OF lv_template_profile IN lv_role_auth WITH lv_role_profile.

              LOOP AT lt_template_1251 ASSIGNING FIELD-SYMBOL(<ls_template_1251>) USING KEY auth_key
                             WHERE object   = <ls_template_1250>-object AND
                                   auth     = <ls_template_1250>-auth.
                APPEND <ls_template_1251> TO lt_role_1251 ASSIGNING FIELD-SYMBOL(<ls_role_1251>).
                <ls_role_1251>-auth      = lv_role_auth.
                _do_adjust_field_values( EXPORTING iv_rolename = iv_rolename CHANGING cv_value = <ls_role_1251>-low ).
                _do_adjust_field_values( EXPORTING iv_rolename = iv_rolename CHANGING cv_value = <ls_role_1251>-high ).
              ENDLOOP.
              APPEND <ls_template_1250> TO lt_role_1250 ASSIGNING FIELD-SYMBOL(<ls_role_1250>).
              <ls_role_1250>-auth        = lv_role_auth.
            ENDLOOP.

            CALL FUNCTION 'PRGN_1250_SAVE_AUTH_DATA'
              EXPORTING
                activity_group = iv_rolename
              TABLES
                auth_data      = lt_role_1250
              EXCEPTIONS
                OTHERS         = 8.
            IF sy-subrc <> 0.
              RAISE EXCEPTION TYPE cx_rs_msg.
            ENDIF.
            CALL FUNCTION 'PRGN_1251_SAVE_FIELD_VALUES'
              EXPORTING
                activity_group = iv_rolename
              TABLES
                field_values   = lt_role_1251
              EXCEPTIONS
                OTHERS         = 8.
            IF sy-subrc <> 0.
              RAISE EXCEPTION TYPE cx_rs_msg.
            ENDIF.

            CALL FUNCTION 'PRGN_UPDATE_DATABASE'.
            CALL FUNCTION 'PRGN_CLEAR_BUFFER'.
            zcl_core_role_admin=>static_do_message( |Role { iv_rolename } authorization adjusted from tempalte { lv_role_template } | ).

          CATCH cx_rs_not_found.
          CATCH cx_rs_msg.
            zcl_core_role_admin=>static_do_message( |Error occoured during update of profile of role { iv_rolename }... see next message| ).
            zcl_core_role_admin=>static_do_message( ).
        ENDTRY.

      CATCH cx_rs_msg.
        zcl_core_role_admin=>static_do_message( ).
    ENDTRY.
    zcl_core_role_admin=>static_set_detlevel( -1 ).

  ENDMETHOD.

  METHOD _do_adjust_field_values.
    "importing
    "  iv_block   type zcore_block
    "  iv_cluster type zcore_cluster
    "  iv_auth    type rsauth
    "changing
    "  cv_value   type clike

    IF iv_block IS SUPPLIED.
      DATA(lv_block) = iv_block.
    ELSE.
      lv_block = zcl_core_role_admin=>get_block_from_rolename( iv_rolename ).
    ENDIF.

    IF iv_cluster IS SUPPLIED.
      DATA(lv_cluster) = iv_cluster.
    ELSE.
      lv_cluster = zcl_core_role_admin=>get_cluster_from_rolename(  iv_rolename ).
    ENDIF.
    TRY.
        DATA(lv_auth) = zcl_core_data_values=>get_authname_from_rolename( iv_rolename ).
      CATCH cx_rs_not_found.
        " Should not happen
        lv_auth = 'EMPTY'.
    ENDTRY.

    " Find all occourences of the text BLOCK and replace with the value of lv_block igven from rolename
    " is given in the call -
    REPLACE ALL OCCURRENCES OF 'BLOCK'   IN cv_value WITH lv_block IN CHARACTER MODE.
    IF sy-subrc = 0. cv_something_changed = rs_c_true. ENDIF.
    REPLACE ALL OCCURRENCES OF 'CLUSTER' IN cv_value WITH lv_cluster IN CHARACTER MODE.
    IF sy-subrc = 0. cv_something_changed = rs_c_true. ENDIF.
    REPLACE '0BI_ALL' IN cv_value WITH lv_auth IN CHARACTER MODE.
    IF sy-subrc = 0. cv_something_changed = rs_c_true. ENDIF.

  ENDMETHOD.

  METHOD _do_convert_to_rolename.
    " Take as input at table where you might find one field anme AGR_NAME
    " replace the content of this field with the value of iv_rolename
    FIELD-SYMBOLS <lv_agr_name> TYPE agr_name.

    LOOP AT ct_table ASSIGNING FIELD-SYMBOL(<ls_data>).
      ASSIGN COMPONENT 'AGR_NAME' OF STRUCTURE <ls_data> TO <lv_agr_name>.
      IF sy-subrc = 0.
        <lv_agr_name> = iv_rolename.
      ELSE.
        " If we fail once in table loop, we continue to fail
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
