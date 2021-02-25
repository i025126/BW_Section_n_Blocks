FUNCTION ZCORE_ROLE2HANA_UPDATE.
*"----------------------------------------------------------------------
*"*"Update Function Module:
*"
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IT_ROLE_NAMES) TYPE  IF_DBMS_USER=>TY_T_DB_ROLE_INFO
*"     VALUE(IS_DB_USER_DATA) TYPE
*"        IF_DBMS_USER=>TY_S_DB_USER_DATA_ALTER
*"----------------------------------------------------------------------
  DATA:
    lr_identity TYPE REF TO if_dbms_user.

  CALL METHOD cl_dbms_user_factory=>get_dbms_user_provider
    IMPORTING
      eo_db_user_provider = lr_identity.

  CALL METHOD lr_identity->alter_user
    EXPORTING
      is_db_user_data = is_db_user_data
      it_role_names   = it_role_names
    IMPORTING
      ev_failed       = DATA(lv_failed)
      et_messages     = DATA(lt_messages).

ENDFUNCTION.
