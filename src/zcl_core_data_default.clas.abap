CLASS zcl_core_data_default DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES:
      if_badi_interface,
      zif_data_role_value.

    METHODS
      _add_values_to_text
        IMPORTING
          it_range    TYPE zif_data_role_value=>gtyt_range
        CHANGING
          ct_agr_text TYPE zif_data_role_value=>gtyt_agr_texts.

  PROTECTED SECTION.

    TYPES:
      " This implementation generates all the authorization that are needed in the method
      " GET_ALL_AUTHNAME, inside this method the ROLENAME is established and the t_range
      " is filled with the values that the AUTH/ROle is intended to include.
      " The text is take from these values
      BEGIN OF gtys_all_authname,
        authname TYPE rsauth,
        rolename TYPE agr_name,
        t_range  TYPE zif_data_role_value=>gtyt_range,
      END OF gtys_all_authname,
      gtyth_authname TYPE HASHED TABLE OF gtys_all_authname WITH UNIQUE KEY authname
         WITH UNIQUE SORTED KEY rolename COMPONENTS rolename.

    CLASS-DATA:
      gth_all_authname TYPE gtyth_authname.

    DATA:
      nv_infoobject      TYPE rsiobjnm,  "Authorization relevant InfoObject
      nr_infoobject      TYPE REF TO if_rsd_cha_prop,
      nt_chanm_fieldname TYPE hds_tab_fieldname,
      nv_chanm_sidtable  TYPE tabname,   "Table of SID's
      nv_chanm_chktable  TYPE tabname,   "Table containing the values for the selection
      nrs_chanm_key      TYPE REF TO data,
      nrt_chanm_key      TYPE REF TO data.

    CLASS-METHODS:
      get_connection_to_source
        IMPORTING
                  iv_original      TYPE rfcdest
        RETURNING VALUE(rv_source) TYPE rfcdest.

    METHODS get_auth_name
      IMPORTING
        iv_block       TYPE zcore_block
        iv_valuecode   TYPE char1
        iv_chavl       TYPE rschavl
      RETURNING
        VALUE(rv_auth) TYPE rsauth
      RAISING
        cx_rs_error .

    METHODS get_role_name
      IMPORTING
        iv_block           TYPE zcore_block
        iv_valuecode       TYPE char1
        iv_chavl           TYPE rschavl
      RETURNING
        VALUE(rv_rolename) TYPE agr_name.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_core_data_default IMPLEMENTATION.

  METHOD zif_data_role_value~get_texts.
    "
    READ TABLE gth_all_authname ASSIGNING FIELD-SYMBOL(<ls_all_authname>)
          WITH KEY rolename COMPONENTS rolename = iv_rolename .
    rt_texts = zcl_core_role_admin=>get_role_description( <ls_all_authname>-rolename ).
    CALL METHOD _add_values_to_text( EXPORTING it_range = <ls_all_authname>-t_range CHANGING ct_agr_text = rt_texts ).

  ENDMETHOD.

  METHOD _add_values_to_text.
*       IMPORTING
*         it_range    type zif_data_role_value=>gtyt_range
*       changing
*         ct_agr_text type zif_data_role_value=>gtyt_agr_texts.

    DO 2 TIMES.
      CASE sy-index.
        WHEN 1.
          DATA(lv_spras) = 'E'.
        WHEN 2.
          lv_spras = 'F'.
      ENDCASE.

** First the first line
      READ TABLE ct_agr_text INTO DATA(ls_text_first)
        WITH KEY spras = lv_spras
                 line  = '00000'.
      IF sy-subrc <> 0.
        ls_text_first-line = '00000'.
      ENDIF.
** Find the last line
      LOOP AT ct_agr_text INTO DATA(ls_text) WHERE spras = lv_spras.
        IF ls_text-line > ls_text_first-line.
          ls_text_first-line = ls_text-line.
        ENDIF.
      ENDLOOP.
** Give all values are text lines
      LOOP AT it_range INTO DATA(ls_range).
        ls_text-line = ls_text-line + 1.
        ls_text-text = |{ nv_infoobject } member value: { ls_range-opt } { ls_range-low }|.
        APPEND ls_text TO ct_agr_text.
      ENDLOOP.
    ENDDO.

  ENDMETHOD.

  METHOD get_connection_to_source.

    SELECT SINGLE logsysnew
      INTO @rv_source
      FROM  rslogsysmap
      WHERE logsysorg = @iv_original.
    IF sy-subrc <> 0.
      rv_source = iv_original.
    ENDIF.
  ENDMETHOD.

  METHOD zif_data_role_value~get_all_authname.

    " this method must return all the authorization that are to be created
    " to make this flexible - the authorization given, must also come with
    " corresponding role and values that are to be included in the role

    DATA:
      ls_all_authname TYPE gtys_all_authname.

    TRY.
        LOOP AT zif_data_role_value~get_values_from_master(  ) ASSIGNING FIELD-SYMBOL(<lv_chavl>).
          TRY.
              CLEAR ls_all_authname.
              " The assignment of AUTHNAME is individual to each implementation, this is the standard
              " one for one member of an entity to one authorization and role
              ls_all_authname-authname = get_auth_name( iv_block = iv_block iv_valuecode = zcl_core_data_values=>gc_valuecode-value iv_chavl = <lv_chavl> ).
              " the way to get the rolename is in this case by adding the BW4 in font and the 'D' for data role, rest is the same as the authorization
              ls_all_authname-rolename = get_role_name( iv_block = iv_block iv_valuecode = zcl_core_data_values=>gc_valuecode-value iv_chavl = <lv_chavl> ).
              APPEND VALUE #( sign     = rs_c_range_sign-including
                              opt      = rs_c_range_opt-equal
                              low      = <lv_chavl> ) TO ls_all_authname-t_range.
              "" This is to be able to translate between the Authorization name of the Range and role name
              INSERT ls_all_authname INTO TABLE gth_all_authname.
              "" This gives the return as all the Authorization that must be generated
              INSERT ls_all_authname-authname INTO TABLE rt_all_authname.
            CATCH cx_rs_error INTO DATA(lrx_error).
              BREAK-POINT.
          ENDTRY.
        ENDLOOP.
        "" Add the all values
        CLEAR ls_all_authname.
        ls_all_authname-authname = get_auth_name( iv_block = iv_block iv_valuecode = zcl_core_data_values=>gc_valuecode-all iv_chavl = zcl_core_data_values=>gc_auth_long_values-all ).
        " the way to get the rolename is in this case by adding the BW4 in font and the 'D' for data role, rest is the same as the authorization
        ls_all_authname-rolename = get_role_name( iv_block = iv_block iv_valuecode = zcl_core_data_values=>gc_valuecode-all iv_chavl = zcl_core_data_values=>gc_auth_long_values-all ).
        APPEND VALUE #( sign     = rs_c_range_sign-including
                        opt      = rs_c_range_opt-pattern
                        low      = zcl_core_data_values=>gc_anyvalue ) TO ls_all_authname-t_range.

        INSERT ls_all_authname-authname INTO TABLE rt_all_authname.
        INSERT ls_all_authname INTO TABLE gth_all_authname.

      CATCH cx_rs_error INTO lrx_error.
        BREAK-POINT.
    ENDTRY.

  ENDMETHOD.

  METHOD zif_data_role_value~get_table_of_range.
    READ TABLE gth_all_authname ASSIGNING FIELD-SYMBOL(<ls_all_authname>)
      WITH TABLE KEY authname = iv_authname.
    ASSERT sy-subrc = 0.
    rt_range = <ls_all_authname>-t_range.
  ENDMETHOD.

  METHOD zif_data_role_value~get_abbreviation.
    " This must be implemented
    RAISE EXCEPTION TYPE cx_rs_error.
  ENDMETHOD.

  METHOD zif_data_role_value~set_infoobject.
    " This method implemented a kind of tool, that
    " will help getting the details if the standard
    " is to find the values from an InfoObject

    IF nv_infoobject IS INITIAL.
      nv_infoobject = iv_infoobject.
      " There is an exception, but since this is a call within
      " calls of existing InfoObject, who cares

      DATA:
        " This is the Main object
        lv_infoobject TYPE rsiobjnm,
        lv_chanm      TYPE rsiobjnm.
      " Since InfoObject can be either an InfoObject or a navigational
      " InfoObject - let's do the split
      CALL FUNCTION 'RSD_IOBJNM_PARSE'
        EXPORTING
          i_iobjnm  = nv_infoobject
          i_objvers = rs_c_objvers-active
        IMPORTING
          e_chanm   = lv_chanm
          e_iobjnm  = lv_infoobject.

      IF lv_chanm IS INITIAL.
        lv_chanm = lv_infoobject.
      ENDIF.

      nr_infoobject = cl_rsd_iobj_prop_cache=>get_cha( lv_chanm ).
      " Make sure to get the

      DATA(lr_chanm) = cl_rsd_iobj_prop_cache=>get_cha( nr_infoobject->n_s_cha-chabasnm ).
      CALL METHOD lr_chanm->get_master_data_table_names
        IMPORTING
          e_chktab = nv_chanm_chktable.

      " This object contains a shitload of nice features for reading
      " inforobject from InfoOBject

      " get a place where to get the information from
      " we might need to refine this, if the authorization
      " is on a referenced InfoOBject
      CALL METHOD lr_chanm->get_master_data_table_names
        IMPORTING
          e_sidtab = nv_chanm_sidtable.

      DATA lrs_key TYPE REF TO cl_abap_structdescr.
      lrs_key ?= lr_chanm->get_key_structdescr(  ).

      " This will list the key fields for selection
      LOOP AT lrs_key->get_components(  ) ASSIGNING FIELD-SYMBOL(<ls_component>).
        APPEND <ls_component>-name TO nt_chanm_fieldname.
      ENDLOOP.

      DATA(lrt_key) = cl_abap_tabledescr=>create( lrs_key ).

      CREATE DATA nrs_chanm_key TYPE HANDLE lrs_key.
      CREATE DATA nrt_chanm_key TYPE HANDLE lrt_key.

      lrs_key ?= nr_infoobject->get_key_structdescr(  ).
    ENDIF.

  ENDMETHOD.

  METHOD zif_data_role_value~do_check_authorization.
    " Must be implemented in order to give the value
    RAISE EXCEPTION TYPE cx_rs_not_authorized.
  ENDMETHOD.

  METHOD get_auth_name.
    DATA(lv_chavl) = iv_chavl.
    " replace '*' with 'X'
    "         '?' with 'Q' to get a valid name
    REPLACE ALL OCCURRENCES OF zcl_core_data_values=>gc_anyvalue IN lv_chavl WITH zcl_core_data_values=>gc_anyvalue2 IN CHARACTER MODE.
    REPLACE ALL OCCURRENCES OF zcl_core_data_values=>gc_anychar  IN lv_chavl WITH zcl_core_data_values=>gc_anychar2  IN CHARACTER MODE.
    rv_auth = |{ iv_block }{ zif_data_role_value~get_abbreviation(  ) }{ iv_valuecode }{ lv_chavl }|.
  ENDMETHOD.

  METHOD zif_data_role_value~get_infoobject.
    rv_infoobject = nv_infoobject.
  ENDMETHOD.

  METHOD get_role_name.
    " Local implementation to get a rolename
    DATA(lv_chavl) = iv_chavl.
    " replace '*' with 'X'
    "         '?' with 'Q' to get a valid name
    REPLACE ALL OCCURRENCES OF zcl_core_data_values=>gc_anyvalue IN lv_chavl WITH zcl_core_data_values=>gc_anyvalue2 IN CHARACTER MODE.
    REPLACE ALL OCCURRENCES OF zcl_core_data_values=>gc_anychar  IN lv_chavl WITH zcl_core_data_values=>gc_anychar2  IN CHARACTER MODE.

    CASE iv_valuecode.
      WHEN zcl_core_data_values=>gc_valuecode-pattern.
        REPLACE ALL OCCURRENCES OF zcl_core_data_values=>gc_anyvalue IN lv_chavl WITH zcl_core_data_values=>gc_anyvalue2 IN CHARACTER MODE.
        REPLACE ALL OCCURRENCES OF zcl_core_data_values=>gc_anychar  IN lv_chavl WITH zcl_core_data_values=>gc_anychar2  IN CHARACTER MODE.
      WHEN zcl_core_data_values=>gc_valuecode-colon.
        lv_chavl = 'COLON'.
      WHEN OTHERS.
        lv_chavl = lv_chavl.
    ENDCASE.

    TRY.
        rv_rolename = |{ zcl_core_role_admin=>get_rolename( iv_block = iv_block iv_roletype = zcl_core_role_admin=>gc_roletype-data ) }| &&
                      |{ zif_data_role_value~get_abbreviation(  ) }| &&
                      |{ iv_valuecode }{ lv_chavl }|.
      CATCH cx_rs_error INTO DATA(lrx_error).
    ENDTRY.

  ENDMETHOD.

  METHOD zif_data_role_value~get_role_name.
    READ TABLE gth_all_authname ASSIGNING FIELD-SYMBOL(<ls_all_authname>)
      WITH TABLE KEY authname = iv_authname.
    ASSERT sy-subrc = 0.
    rv_rolename = <ls_all_authname>-rolename.
  ENDMETHOD.

  METHOD zif_data_role_value~get_values_from_master.

    FIELD-SYMBOLS:
      <lt_key> TYPE STANDARD TABLE.

    " This assignment and the next select
    " means that all keys are stored in the
    " object itself... we all compounding
    ASSIGN nrt_chanm_key->* TO <lt_key>.

    TRY.
        SELECT DISTINCT (nt_chanm_fieldname)
          FROM (nv_chanm_chktable)
          INTO TABLE @<lt_key>
          WHERE objvers = @rs_c_objvers-active
          ORDER BY (nt_chanm_fieldname).

      CATCH cx_sy_sql_error INTO DATA(lrx_sql).
        DATA(lv_text) = lrx_sql->get_text(  ).
        RAISE EXCEPTION TYPE cx_rs_msg
          EXPORTING
            msgid = 'ZOCRE'
            msgty = rs_c_error
            msgno = '020'
            msgv1 = CONV sy-msgv1( nv_infoobject )
            msgv2 = CONV sy-msgv2( nv_chanm_sidtable )
            msgv3 = CONV sy-msgv3( lv_text ).
    ENDTRY.

    DATA lv_chavl TYPE rschavl.
    LOOP AT <lt_key> ASSIGNING FIELD-SYMBOL(<ls_key>).
      " Since we have selection what might be a structure
      " of key fields (Compounding, navigational etccc...
      " let's make sure that we return a single value
      " in the sorted table
      lv_chavl = <ls_key>.
      " Technically the blank value is not to be in,
      " but must be added as an extra values
      CHECK lv_chavl IS NOT INITIAL.
      CHECK lv_chavl <> zcl_core_data_values=>gc_anyvalue.
      CHECK lv_chavl <> zcl_core_data_values=>gc_anychar.
      INSERT lv_chavl INTO TABLE rts_chavl.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
