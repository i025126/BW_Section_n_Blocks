CLASS zcl_core_role2hana DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_identity_update .
    INTERFACES if_badi_interface .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_core_role2hana IMPLEMENTATION.

  METHOD if_badi_identity_update~save.
* Importing parameter:
* - IV_UPDATE_TASK                In case of 'X', database operations must be
*                                 written in an update module.
*                                 => E-Mail should be send only in UPDATE TASK
*                                    Otherwise no rollback is possible
* - IT_BADI_IDENTITY_UPDATE       List of all Identities processed in current transaction.
* - IT_BADI_IDENTITY_CUA_SYSTEMS  Relevant only in CUA central system.
*                                 List of new or deleted systems per user.
* - IT_BADI_IDENTITY_CUA_ROLES    Relevant only in CUA central system.
*                                 Actual and before image of cua role assignments of processed users.
* - IT_BADI_IDENTITY_ROLES        Actual and before image of role assignments of processed users.
* - IT_BADI_IDENTITY_GROUPS       Actual and before image of group assignments of processed users.
*
* Changing parameter:
* - CT_BADI_MESSAGE               Success, Warning or Info message can be added per user.
*                                 Error messages are not allowed.
    DATA ls_db_user_data TYPE if_dbms_user=>ty_s_db_user_data_alter.

    DATA:
      lr_identity TYPE REF TO if_dbms_user.

    CALL METHOD cl_dbms_user_factory=>get_dbms_user_provider
      IMPORTING
        eo_db_user_provider = lr_identity.

    " Copy passwords of every user
    LOOP AT it_badi_identity_update ASSIGNING FIELD-SYMBOL(<ls_identity_update>).

      SELECT SINGLE dbms_user
          FROM usr_dbms_user
          WHERE bname = @<ls_identity_update>-bname
          INTO @ls_db_user_data-user_name.
      " Find the DBMS User and check if it does exist.
      CHECK sy-subrc = 0.

      DATA(lv_do_update) = rs_c_false.
      CASE <ls_identity_update>-modus.
        WHEN cl_identity_persistence=>co_ta_modus_create.
          lv_do_update = rs_c_true.
        WHEN cl_identity_persistence=>co_ta_modus_modify.
          lv_do_update = rs_c_true.
        WHEN cl_identity_persistence=>co_ta_modus_assign.
          lv_do_update = rs_c_true.
        WHEN OTHERS.
      ENDCASE.

      TRY.
          IF lv_do_update = rs_c_true AND
                  it_badi_identity_roles[ bname = <ls_identity_update>-bname ]-before_image <> it_badi_identity_roles[ bname = <ls_identity_update>-bname ]-actual.
            " So there is an update
            CALL METHOD lr_identity->get_user
              EXPORTING
                iv_user_name  = ls_db_user_data-user_name
              IMPORTING
                et_role_names = DATA(lt_role_names).

            LOOP AT lt_role_names ASSIGNING FIELD-SYMBOL(<ls_hana_roles>).
              DATA lv_rolename TYPE agr_name.
              lv_rolename = <ls_hana_roles>-role_name.
              IF zcl_core_role_admin=>is_framework_role( lv_rolename ) = zcl_core_role_admin=>gc_rolekind-hana.
                " Delete all entries with a role that is part of the framework, but retain the rest
                " The idea is to retain all roles that is not generated from the framework. All roles from the
                " framework is coming from "AUTH.ROOTx"
                DELETE lt_role_names.
              ENDIF.
            ENDLOOP.
            LOOP AT it_badi_identity_roles[ bname = <ls_identity_update>-bname ]-actual ASSIGNING FIELD-SYMBOL(<ls_roles>)
                " Add all the roles that that is content roles
                      WHERE from_dat <= sy-datum AND to_dat >= sy-datum.
              " For the moment we only push develer roles to HANA
              CHECK zcl_core_role_admin=>get_roletype_from_rolename( <ls_roles>-agr_name ) = zcl_core_role_admin=>gc_roletype-content.
              " Make sure only to try and move BW roles
              CHECK zcl_core_role_admin=>is_framework_role( <ls_roles>-agr_name ) = zcl_core_role_admin=>gc_rolekind-bw4.
              " Make sure the role exists
              CHECK zcl_core_basis_tools=>hana_role_read( EXPORTING iv_rolename = <ls_roles>-agr_name ) IS NOT INITIAL.

              APPEND VALUE #( role_name = |{ zcl_core_role_admin=>get_hana_auth_from_rolename( <ls_roles>-agr_name ) }::{ <ls_roles>-agr_name }| ) TO lt_role_names.
            ENDLOOP.

*        IF iv_update_task = rs_c_true.
*          " Since we techincally are doing some updates, if found it best
*          " to also have a function module.
*          CALL FUNCTION 'ZCORE_ROLE2HANA_UPDATE' IN UPDATE TASK
*            EXPORTING
*              is_db_user_data = ls_db_user_data
*              it_role_names   = lt_role_names.
*        else.
            " But this is where i put my money
            CALL METHOD lr_identity->alter_user
              EXPORTING
                is_db_user_data = ls_db_user_data
                it_role_names   = lt_role_names
              IMPORTING
                ev_failed       = DATA(lv_failed)
                et_messages     = DATA(lt_messages).

            LOOP AT lt_messages INTO DATA(ls_message).
              APPEND VALUE #( bname = ls_db_user_data-user_name
                              message = VALUE #( msgty    = ls_message-type
                                                 msgid    = ls_message-id
                                                 msgno    = ls_message-number
                                                 msgv1    = ls_message-message_v1
                                                 msgv2    = ls_message-message_v2
                                                 msgv3    = ls_message-message_v3
                                                 msgv4    = ls_message-message_v4 ) ) TO ct_badi_message.
            ENDLOOP.
          ENDIF.
        catch cx_rs2hana_view_nhi into data(lrx_nhi).
          message lrx_nhi->get_text(  ) type rs_c_error.
        CATCH cx_sy_itab_line_not_found.
          "" Not reason to work on it, no update of PFCG roles
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.


ENDCLASS.
