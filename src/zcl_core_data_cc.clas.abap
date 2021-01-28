CLASS zcl_core_data_cc DEFINITION INHERITING FROM zcl_core_data_default
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      zif_data_role_value~get_all_authname REDEFINITION,
      zif_data_role_value~get_abbreviation REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_core_data_cc IMPLEMENTATION.

  METHOD zif_data_role_value~get_abbreviation.
    rv_abbreviation = 'CC'.
  ENDMETHOD.

  METHOD zif_data_role_value~get_all_authname.
    rt_all_authname = super->zif_data_role_value~get_all_authname( iv_block ).

    " Add also a Blank and the ':' value

    DATA:
      ls_all_authname TYPE gtys_all_authname.
    "" Add the all values
    CLEAR ls_all_authname.
    TRY.
        ls_all_authname-authname = get_auth_name( iv_block = iv_block iv_valuecode = zcl_core_data_values=>gc_valuecode-value iv_chavl = zcl_core_data_values=>gc_auth_long_values-blank ).
        " the way to get the rolename is in this case by adding the BW4 in font and the 'D' for data role, rest is the same as the authorization
        ls_all_authname-rolename = get_role_name( iv_block = iv_block iv_valuecode = zcl_core_data_values=>gc_valuecode-value iv_chavl = zcl_core_data_values=>gc_auth_long_values-blank ).
        APPEND VALUE #( sign     = rs_c_range_sign-including
                        opt      = rs_c_range_opt-equal
                        low      = '    ' ) TO ls_all_authname-t_range.
        INSERT ls_all_authname-authname INTO TABLE rt_all_authname.
        INSERT ls_all_authname INTO TABLE gth_all_authname.
        ls_all_authname-authname = get_auth_name( iv_block = iv_block iv_valuecode = zcl_core_data_values=>gc_valuecode-colon iv_chavl = zcl_core_data_values=>gc_auth_long_values-colon ).
        " the way to get the rolename is in this case by adding the BW4 in font and the 'D' for data role, rest is the same as the authorization
        ls_all_authname-rolename = get_role_name( iv_block = iv_block iv_valuecode = zcl_core_data_values=>gc_valuecode-colon iv_chavl = zcl_core_data_values=>gc_auth_long_values-colon ).
        refresh ls_all_authname-t_range.
        APPEND VALUE #( sign     = rs_c_range_sign-including
                        opt      = rs_c_range_opt-equal
                        low      = ':' ) TO ls_all_authname-t_range.
        INSERT ls_all_authname-authname INTO TABLE rt_all_authname.
        INSERT ls_all_authname INTO TABLE gth_all_authname.
      CATCH cx_rs_error INTO data(lrx_error).
        BREAK-POINT.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
