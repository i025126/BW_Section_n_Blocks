CLASS zcl_core_data_acompauth DEFINITION
  PUBLIC
  INHERITING FROM zcl_core_data_cc
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      zif_data_role_value~get_values_from_master REDEFINITION,
      zif_data_role_value~do_check_authorization REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_core_data_acompauth IMPLEMENTATION.

  METHOD zif_data_role_value~get_values_from_master.
*    rts_chavl = super->zif_data_role_value~get_values_from_master(  ).

    "" Since the only values we need authorization for is in the ADSO
    "" ASYSPDSTB

    data: lt_acompauth type STANDARD TABLE OF /bic/oiacompauth.
    append lines of super->zif_data_role_value~get_values_from_master(  ) to lt_acompauth.

    TRY.
        DATA(lt_tabname) = cl_rso_adso=>get_tablnm( i_adsonm = 'ASYSPDSTB' i_objvers = rs_c_objvers-active ).
        DATA(lv_tabname) = lt_tabname[ dsotabtype = 'VX' ]-name.

        SELECT DISTINCT /bic/acompauth
            FROM (lv_tabname)
            FOR ALL ENTRIES IN @lt_acompauth
            WHERE /bic/acompauth = @lt_acompauth-table_line
            INTO TABLE @rts_chavl.
      CATCH cx_rs_not_found
            cx_sy_itab_line_not_found
            cx_sy_sql_error INTO DATA(lrx_error).
        RAISE EXCEPTION TYPE cx_rs_error
          EXPORTING
            previous = lrx_error.
    ENDTRY.
  ENDMETHOD.

  METHOD zif_data_role_value~do_check_authorization.
*        IMPORTING
*          iv_user            TYPE sy-uname
*          iv_chavl           TYPE rschavl
*        EXPORTING
*          ev_iobjnm          type rsiobjnm
*          ev_sign            type /bi0/oitctsign
*          ev_option          type /bi0/oitctoption
*          ev_low             type /bi0/oitctlow
*          ev_high           type /bi0/oitcthigh

    DATA:
      lrx_msg      TYPE REF TO cx_rs_msg,
      _rfc_message TYPE char72.

    DATA(lv_rfcdest) = get_connection_to_source( 'CDSCLNT040' ).

** This might not work
    CALL FUNCTION 'AUTHORITY_CHECK'
      DESTINATION lv_rfcdest
      EXPORTING
        user                  = iv_user
        object                = 'F_BKPF_BUK'
        field1                = 'BUKRS'
        value1                = CONV ust12-von( iv_chavl )
        field2                = 'ACTVT'
        value2                = '03'
      EXCEPTIONS
        user_dont_exist       = 1
        user_is_authorized    = 0
        user_is_locked        = 3
        user_not_authorized   = 4
        error_message         = 8
        communication_failure = 12 MESSAGE _rfc_message
        system_failure        = 12 MESSAGE _rfc_message.
    CASE sy-subrc.
      WHEN 0.
        rs_range-sign   = rs_c_range_sign-including.
        rs_range-opt    = rs_c_range_opt-equal.
        rs_range-low    = iv_chavl.
        rs_range-high   = ''.
      WHEN 1.
        RAISE EXCEPTION TYPE zcx_rs_no_user_in_source
          EXPORTING
            user = iv_user.
      WHEN 3.
        RAISE EXCEPTION TYPE zcx_rs_locked_in_source
          EXPORTING
            user = iv_user.
      WHEN 4.
        RAISE EXCEPTION TYPE cx_no_authorization.
      WHEN 8.
        CREATE OBJECT lrx_msg
          EXPORTING
            msgid = sy-msgid
            msgty = sy-msgty
            msgno = sy-msgno
            msgv1 = sy-msgv1
            msgv2 = sy-msgv2
            msgv3 = sy-msgv3
            msgv4 = sy-msgv4.
        RAISE EXCEPTION TYPE cx_rs_error
          EXPORTING
            previous = lrx_msg.
      WHEN 12.
        CREATE OBJECT lrx_msg
          EXPORTING
            msgid = 'ZCORE'
            msgty = rs_c_error
            msgno = '021'
            msgv1 = CONV sy-msgv1( lv_rfcdest )
            msgv2 = 'AUTHORITY_CHECK'
            msgv3 = CONV sy-msgv3( _rfc_message ).
        RAISE EXCEPTION TYPE cx_rs_error
          EXPORTING
            previous = lrx_msg.
      WHEN OTHERS.
        " What...
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
