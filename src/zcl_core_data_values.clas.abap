CLASS zcl_core_data_values DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      gtyv_valuecode TYPE char1.

    TYPES:
      BEGIN OF gtys_2_infoobject,
        cluster      TYPE zcore_cluster,
        abbrebiation TYPE zcore_abbreviation,
        infoobject   TYPE rsiobjnm,
      END OF gtys_2_infoobject,
      gtyth_2_infoobject TYPE HASHED TABLE OF gtys_2_infoobject WITH UNIQUE KEY cluster abbrebiation.

    CONSTANTS:
      gc_anyvalue  TYPE char1 VALUE '*',
      gc_anychar   TYPE char1 VALUE '?',
      gc_anyvalue2 TYPE char1 VALUE 'X',
      gc_anychar2  TYPE char1 VALUE 'Q',
      BEGIN OF gc_auth_long_values,
        all   TYPE rschavl VALUE 'ALL',
        colon TYPE rschavl VALUE 'COLON',
        blank TYPE rschavl VALUE 'BLANK',
        rest  TYPE rschavl VALUE 'MISC',
      END OF gc_auth_long_values,
      BEGIN OF gc_valuecode,
        all      TYPE gtyv_valuecode VALUE 'A',
        value    TYPE gtyv_valuecode VALUE '_',
        interval TYPE gtyv_valuecode VALUE 'I',
        pattern  TYPE gtyv_valuecode VALUE 'P',
        colon    TYPE gtyv_valuecode VALUE 'C',
        hiernode TYPE gtyv_valuecode VALUE 'H',
        misc     TYPE gtyv_valuecode VALUE 'M',
      END OF gc_valuecode.

    TYPES:
      BEGIN OF gtys_analysisauthorizationtext,
        recordmode type char1,
        tctauth  TYPE /bi0/oitctauth,
        tcalang  TYPE /bi0/oitcalang,
        tctadto  TYPE /bi0/oitctadto,
        tcatxtlg TYPE /bi0/oitcatxtlg,
        tcatxtmd TYPE /bi0/oitcatxtmd,
        tcatxtsh TYPE /bi0/oitcatxtsh,
      END OF gtys_analysisauthorizationtext,
      gtyt_analysisauthorizationtxt  TYPE STANDARD TABLE OF gtys_analysisauthorizationtext WITH NON-UNIQUE DEFAULT KEY,
      gtyts_analysisauthorizationtxt TYPE SORTED TABLE OF gtys_analysisauthorizationtext
            WITH UNIQUE KEY tctauth tcalang tctadto,
      BEGIN OF gtys_analysisauthorization,
        tctusernm  TYPE /bi0/oitctusernm,
        tctauth    TYPE /bi0/oitctauth,
        tctadto    TYPE /bi0/oitctadto,
        tctiobjnm  TYPE /bi0/oitctiobjnm,
        tctsign    TYPE /bi0/oitctsign,
        tctoption  TYPE /bi0/oitctoption,
        tctlow     TYPE /bi0/oitctlow,
        tcthigh    TYPE /bi0/oitcthigh,
        tctobjvers TYPE /bi0/oitctobjvers,
        tctsysid   TYPE /bi0/oitctsysid,
        tctadfrom  TYPE /bi0/oitctadfrom,
      END OF gtys_analysisauthorization,
      gtyt_analysisauthorization  TYPE STANDARD TABLE OF gtys_analysisauthorization WITH NON-UNIQUE DEFAULT KEY,
      gtyts_analysisauthorization TYPE SORTED TABLE OF gtys_analysisauthorization WITH UNIQUE KEY
        tctusernm
        tctauth
        tctadto
        tctiobjnm
        tctsign
        tctoption
        tctlow
        tcthigh
        tctobjvers
        tctsysid .

    TYPES:
      BEGIN OF gtys_rolename,
        prefix    TYPE char3,
        block     TYPE zcore_block,
        roletype  TYPE zcore_abbreviation,
        valuecode TYPE gtyv_valuecode,
        chavl     TYPE rschavl,
      END OF gtys_rolename,
      BEGIN OF gtys_authname,
        block     TYPE zcore_block,
        valuecode TYPE gtyv_valuecode,
        chavl     TYPE rschavl,
      END OF gtys_authname.


    TYPES:
      gtyv_action type char1,
      gtyt_range TYPE STANDARD TABLE OF rrrange WITH NON-UNIQUE DEFAULT KEY.

    CONSTANTS:
      begin of gc_action,
        create  type gtyv_action value 'C',
        nothing type gtyv_action value 'N',
      end of gc_action.

    TYPES:
      BEGIN OF gtys_role_2_auth,
        rolename TYPE agr_name,
        authname TYPE rsauth,
      END OF gtys_role_2_auth,
      gtyth_role_2_auth TYPE HASHED TABLE OF gtys_role_2_auth WITH UNIQUE KEY rolename.

    TYPES:
      BEGIN OF gtys_objects,
        doccluster TYPE zcore_cluster,
        docsection TYPE zcore_section,
        docblock   TYPE zcore_block,
        infoobject TYPE rsiobjnm,
      END OF gtys_objects,
      gtyts_objects TYPE SORTED TABLE OF gtys_objects WITH UNIQUE DEFAULT KEY.

    CLASS-DATA:
      "_message           TYPE char255,
      _message         TYPE string,
      gth_role_2_auth  TYPE gtyth_role_2_auth,
      gth_2_infoobject TYPE gtyth_2_infoobject.
    "      gth_infoobjectlist TYPE gtyth_infoobjectlist.

    DATA:
      nv_cluster    TYPE zcore_cluster.

    CLASS-METHODS:
      " Calls the method above for any block listed/given in the db table
      "
      create_data_role
        IMPORTING
          iv_cluster  TYPE zcore_cluster
          it_block    TYPE zcl_core_role_admin=>gtyt_rng_block
          iv_txt_adso TYPE rsoadsonm OPTIONAL
          iv_authadso TYPE rsoadsonm OPTIONAL
        RAISING
          cx_rs_msg
          cx_rs_error,
      create_data_role_for_user
        IMPORTING
          iv_cluster  TYPE zcore_cluster
          iv_authadso TYPE rsoadsonm
          iv_txt_adso TYPE rsoadsonm
        RAISING
          cx_rs_msg
          cx_rs_error,
      get_all_rolename
        RETURNING
          VALUE(rth_rolename) type gtyth_role_2_auth,
      get_authname_from_rolename
        IMPORTING
          iv_rolename        TYPE agr_name
        RETURNING
          VALUE(rv_authname) TYPE rsauth
        RAISING
          cx_rs_not_found,
      get_connection_to_source
        IMPORTING
          iv_rfcdest        TYPE rfcdest
        RETURNING
          VALUE(rv_rfcdest) TYPE rfcdest,
      get_infoobject_from_rolename
        IMPORTING
                  iv_rolename      TYPE agr_name
        RETURNING VALUE(rv_iobjnm) TYPE rsiobjnm
        RAISING
                  cx_rs_not_found,
      create_data_role_for_block
        IMPORTING
          iv_block     TYPE zcore_block
          iv_action    type gtyv_action DEFAULT gc_action-create
        EXPORTING
          et_allvalues TYPE gtyts_analysisauthorization
          et_alltext   TYPE gtyts_analysisauthorizationtxt
        RAISING
          cx_rs_error,
      get_users
        RETURNING
          VALUE(rt_users) TYPE hrbas_bapiusname_table.

    METHODS:
      constructor
        IMPORTING
          iv_cluster TYPE zcore_cluster.

  PROTECTED SECTION.

    CLASS-METHODS:
      set_authname_for_rolename
        IMPORTING
          iv_rolename TYPE agr_name
          iv_authname TYPE rsauth.

    METHODS:
      " Create the roles required for
      " all infoobjects and all values for ONE Block
      do_move_values_2_adso
        IMPORTING
          it_allvalues TYPE gtyts_analysisauthorization
          iv_adsonm    TYPE rsoadsonm
        RAISING
          cx_rs_msg,
      do_move_texts_2_adso
        IMPORTING
          it_alltext TYPE gtyts_analysisauthorizationtxt
          iv_adsonm  TYPE rsoadsonm
        RAISING
          cx_rs_msg,
      get_infoobject_for_processing
        RETURNING
          VALUE(rts_objects) TYPE gtyts_objects,
      get_entry_for_user_access
        IMPORTING
          iv_username   TYPE xubname
          ir_infoobject TYPE REF TO zbapi_data_role_value
          it_range      TYPE gtyt_range
        EXPORTING
          et_alltext    TYPE gtyts_analysisauthorizationtxt
          et_allvalues  TYPE gtyts_analysisauthorization
        RAISING
          cx_rs_error,
      get_entry_for_auth
        IMPORTING
          iv_authname   TYPE rsauth
          ir_infoobject TYPE REF TO zbapi_data_role_value
        EXPORTING
          et_allvalues  TYPE gtyts_analysisauthorization
          et_alltext    TYPE gtyts_analysisauthorizationtxt
        RAISING
          cx_rs_error.
    CLASS-METHODS:
      get_split_authname
        IMPORTING
          iv_authname        TYPE rsauth
        RETURNING
          VALUE(rs_authname) TYPE gtys_authname,
      get_split_rolename
        IMPORTING
          iv_rolename        TYPE agr_name
        RETURNING
          VALUE(rs_rolename) TYPE gtys_rolename.

  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_core_data_values IMPLEMENTATION.

  METHOD get_all_rolename.
    rth_rolename = gth_role_2_auth.
  ENDMETHOD.

  METHOD get_entry_for_user_access.
    " Basis service method
    " that will create an entry for the aDSO from the Authorization name
    " If the block of the authorization is either AUTH or <cluster>ALL
    " all blocks in the cluster is added for access for InfoProvider
    " this is instance method, hence know ths infoObject and can be
    " redefined... if needed

    DATA lv_authname TYPE zcore_abbreviation.
    CALL BADI ir_infoobject->get_abbreviation
      RECEIVING
        rv_abbreviation = DATA(lv_abbreviation).

    DATA ls_allvalues TYPE gtys_analysisauthorization.
    DATA ls_txtvalues TYPE gtys_analysisauthorizationtext.

    " Basic entries - same for all entries
    ls_allvalues-tctauth    = |{ nv_cluster }{ lv_abbreviation }{ iv_username }|.
    ls_allvalues-tctusernm  = iv_username.
    ls_allvalues-tctadto    = '99991231'.
    ls_allvalues-tctobjvers =  rs_c_objvers-active.
    ls_allvalues-tctsysid   = sy-sysid.
    ls_allvalues-tctadfrom  = sy-datum.


    " ACTIVITY - DISPLAY
    ls_allvalues-tctiobjnm  = '0TCAACTVT'.
    ls_allvalues-tctsign    = rs_c_range_sign-including.
    ls_allvalues-tctoption  = rs_c_range_opt-equal.
    ls_allvalues-tctlow     = '03'.
    ls_allvalues-tcthigh    = ''.
    INSERT ls_allvalues INTO TABLE et_allvalues.

    " What InfoProviders
    ls_allvalues-tctiobjnm  = '0TCAIPROV'.
    ls_allvalues-tctsign    = rs_c_range_sign-including.
    ls_allvalues-tctoption  = rs_c_range_opt-pattern.
    " Any InfoProvider in the virtual Section
    ls_allvalues-tctlow     = |{ nv_cluster }???VM{ gc_anyvalue }|.
    ls_allvalues-tcthigh    = ''.
    INSERT ls_allvalues INTO TABLE et_allvalues.

    CALL BADI ir_infoobject->get_infoobject
      RECEIVING
        rv_infoobject = DATA(lv_infoobject).
    " InfoObject dimension and values
    loop at it_range into data(ls_range).
    ls_allvalues-tctiobjnm  = lv_infoobject.
    ls_allvalues-tctsign    = ls_range-sign.
    ls_allvalues-tctoption  = ls_range-opt.
    ls_allvalues-tctlow     = ls_range-low.
    ls_allvalues-tcthigh    = ls_range-high.
    INSERT ls_allvalues INTO TABLE et_allvalues.
    endloop.

    ls_txtvalues-tcatxtsh  = |User { iv_username } and cluster { nv_cluster }|.
    ls_txtvalues-tcatxtmd  = |User { iv_username } and cluster { zcl_core_role_admin=>get_cluster_description( nv_cluster ) }|.
    "" I know... we will only get the last value from the range
    ls_txtvalues-tcatxtlg  = |From source: User { iv_username } and cluster { zcl_core_role_admin=>get_cluster_description( nv_cluster ) } - autogenerated|.

    ls_txtvalues-tctauth   = ls_allvalues-tctauth.
    ls_txtvalues-tctadto   = '99991231'.
    ls_txtvalues-tcalang  = 'E'.
    INSERT ls_txtvalues INTO TABLE et_alltext.
    ls_txtvalues-tcalang  = 'F'.
    INSERT ls_txtvalues INTO TABLE et_alltext.

  ENDMETHOD.

  METHOD get_entry_for_auth.
    " Basis service method
    " that will create an entry for the aDSO from the Authorization name
    " If the block of the authorization is either AUTH or <cluster>ALL
    " all blocks in the cluster is added for access for InfoProvider
    " this is instance method, hence know ths infoObject and can be
    " redefined... if needed
    DATA ls_allvalues TYPE gtys_analysisauthorization.
    DATA ls_textauth  TYPE gtys_analysisauthorizationtext.

    " Basic entries - same for all entries
    ls_allvalues-tctauth    = iv_authname.
    ls_allvalues-tctusernm  = ''.
    ls_allvalues-tctadto    = '99991231'.
    ls_allvalues-tctobjvers =  rs_c_objvers-active.
    ls_allvalues-tctsysid   = sy-sysid.
    ls_allvalues-tctadfrom  = sy-datum.

    CALL FUNCTION 'RSKC_CHAVL_OF_IOBJ_CHECK'
      EXPORTING
        i_chavl                    = ls_allvalues-tctauth
        i_iobjnm                   = '0TCTAUTH'
      EXCEPTIONS
        chavl_not_allowed          = 1
        chavl_not_convexit_conform = 2
        OTHERS                     = 3.
    IF sy-subrc <> 0.
      BREAK-POINT.
    ENDIF.
    " ACTIVITY - DISPLAY
    ls_allvalues-tctiobjnm  = '0TCAACTVT'.
    ls_allvalues-tctsign    = rs_c_range_sign-including.
    ls_allvalues-tctoption  = rs_c_range_opt-equal.
    ls_allvalues-tctlow     = '03'.
    ls_allvalues-tcthigh    = ''.
    INSERT ls_allvalues INTO TABLE et_allvalues.

    CALL BADI ir_infoobject->get_infoobject
      RECEIVING
        rv_infoobject = DATA(lv_infoobject).
    " InfoObject dimension and values
    ls_allvalues-tctiobjnm  = lv_infoobject.

    CALL BADI ir_infoobject->get_table_of_range
      EXPORTING
        iv_authname = iv_authname
      RECEIVING
        rt_range    = DATA(lt_range).
    LOOP AT lt_range INTO DATA(ls_range).
      ls_allvalues-tctsign    = ls_range-sign.
      ls_allvalues-tctoption  = ls_range-opt.
      ls_allvalues-tctlow     = ls_range-low.
      ls_allvalues-tcthigh    = ls_range-high.
      INSERT ls_allvalues INTO TABLE et_allvalues.
    ENDLOOP.

    CALL BADI ir_infoobject->get_role_name
      EXPORTING
        iv_authname = iv_authname
      RECEIVING
        rv_rolename = DATA(lv_rolename).

    " What InfoProviders
    ls_allvalues-tctiobjnm  = '0TCAIPROV'.
    ls_allvalues-tctsign    = rs_c_range_sign-including.
    ls_allvalues-tctoption  = rs_c_range_opt-pattern.
    IF zcl_core_role_admin=>get_block_from_rolename( lv_rolename ) = zcl_core_role_admin=>get_virtual_block( iv_blocktype = 'ALL' iv_cluster = zcl_core_role_admin=>get_cluster_from_block( zcl_core_role_admin=>get_block_from_rolename( lv_rolename ) ) ).
      ls_allvalues-tctlow   = |{ zcl_core_role_admin=>get_cluster_from_block( zcl_core_role_admin=>get_block_from_rolename( lv_rolename ) ) }???VM{ gc_anyvalue }|.
    ELSEIF zcl_core_role_admin=>get_block_from_rolename( lv_rolename ) = zcl_core_role_admin=>get_virtual_block( iv_blocktype = 'AUTH' ).
      ls_allvalues-tctlow   = gc_anyvalue.
    ELSE.
      ls_allvalues-tctlow    = |{ zcl_core_role_admin=>get_block_from_rolename( lv_rolename ) }{ gc_anyvalue }|.
    ENDIF.
    INSERT ls_allvalues INTO TABLE et_allvalues.

    " We must grant acccess also to the InfoObject as an InfoProvider
    " for this we need to know if the authorization flags is on a nav-attr
    " and grant the access on the InfoObject and not the attribute
    DATA lv_iobjnm TYPE rsiobjnm.
    CALL FUNCTION 'RSD_IOBJNM_PARSE'
      EXPORTING
        i_iobjnm  = lv_infoobject
        i_objvers = rs_c_objvers-active
      IMPORTING
        e_chanm   = lv_iobjnm
      EXCEPTIONS
        OTHERS    = 8.
    ASSERT sy-subrc = 0.
    IF lv_iobjnm IS INITIAL.
      lv_iobjnm = lv_infoobject.
    ENDIF.
    ls_allvalues-tctiobjnm  = '0TCAIPROV'.
    ls_allvalues-tctsign    = rs_c_range_sign-including.
    ls_allvalues-tctoption  = rs_c_range_opt-equal.
    ls_allvalues-tctlow     = lv_iobjnm.
    INSERT ls_allvalues INTO TABLE et_allvalues.

    """ Adding the text """
    ls_textauth-tctauth   = iv_authname.
    ls_textauth-tctadto   = '99991231'.
    ls_textauth-tcatxtsh  = |For role { lv_rolename }|.
    ls_textauth-tcatxtmd  = |For role { lv_rolename }|.
    "" I know... we will only get the last value from the range
    ls_textauth-tcatxtlg  = |Role: { lv_rolename } InfoObject: { lv_infoobject } Value: { ls_range-opt }/{ ls_range-low }|.
    ls_textauth-tcalang  = 'E'.
    INSERT ls_textauth INTO TABLE et_alltext.
    ls_textauth-tcalang  = 'F'.
    INSERT ls_textauth INTO TABLE et_alltext.

  ENDMETHOD.

  METHOD get_split_rolename.

    DATA:
      BEGIN OF ls_rolename,
        prefix    TYPE char3,
        block     TYPE zcore_block,
        roletype  TYPE zcore_abbreviation,
        valuecode TYPE gtyv_valuecode,
        chavl     TYPE rschavl60,
      END OF ls_rolename.
    ls_rolename = iv_rolename.
    CASE ls_rolename-valuecode.
      WHEN gc_valuecode-all.
        ls_rolename-chavl  = gc_anyvalue.
      WHEN gc_valuecode-colon.
        ls_rolename-chavl = ':'.
      WHEN gc_valuecode-pattern.
        REPLACE ALL OCCURRENCES OF gc_anyvalue2  IN ls_rolename-chavl WITH gc_anyvalue.
        REPLACE ALL OCCURRENCES OF gc_anychar2   IN ls_rolename-chavl WITH gc_anychar.
    ENDCASE.
    MOVE-CORRESPONDING ls_rolename TO rs_rolename.

  ENDMETHOD.

  METHOD get_split_authname.

    DATA:
      BEGIN OF ls_authname,
        block     TYPE zcore_block,
        valuecode TYPE gtyv_valuecode,
        chavl     TYPE rschavl60,
      END OF ls_authname.

    ls_authname = iv_authname.
    CASE ls_authname-valuecode.
      WHEN gc_valuecode-all.
        ls_authname-chavl  = gc_anyvalue.
      WHEN gc_valuecode-colon.
        ls_authname-chavl = ':'.
      WHEN gc_valuecode-pattern.
        REPLACE ALL OCCURRENCES OF gc_anyvalue2  IN ls_authname-chavl WITH gc_anyvalue.
        REPLACE ALL OCCURRENCES OF gc_anychar2   IN ls_authname-chavl WITH gc_anychar.
    ENDCASE.
    MOVE-CORRESPONDING ls_authname TO rs_authname.
  ENDMETHOD.

  METHOD get_infoobject_from_rolename.

    IF zcl_core_role_admin=>get_roletype_from_rolename( iv_rolename ) <> zcl_core_role_admin=>gc_roletype-data.
      " Nope should not be converted
      RAISE EXCEPTION TYPE cx_rs_not_found.
    ENDIF.

    zcl_core_role_admin=>get_abbreviation_from_rolename( iv_rolename ).

    DATA(lv_cluster) = zcl_core_role_admin=>get_cluster_from_rolename( iv_rolename ).

    DATA:
      ls_2_infoobject TYPE gtys_2_infoobject.

    ls_2_infoobject-cluster      = zcl_core_role_admin=>get_cluster_from_rolename( iv_rolename ).
    ls_2_infoobject-abbrebiation = zcl_core_role_admin=>get_abbreviation_from_rolename( iv_rolename ).

    READ TABLE gth_2_infoobject ASSIGNING FIELD-SYMBOL(<ls_2_infoobject>)
      FROM ls_2_infoobject.

    IF sy-subrc <> 0.
      DATA:
        lr_processing      TYPE REF TO zcl_core_data_values,
        lr_badi_infoobject TYPE REF TO zbapi_data_role_value.

      CREATE OBJECT lr_processing
        EXPORTING
          iv_cluster = lv_cluster.

      rv_iobjnm = ''.
      LOOP AT lr_processing->get_infoobject_for_processing(  ) ASSIGNING FIELD-SYMBOL(<ls_infoobject>).
        TRY.
            GET BADI lr_badi_infoobject
              FILTERS
                zcore_role_data_filter = <ls_infoobject>-infoobject.
            CALL BADI lr_badi_infoobject->set_infoobject
              EXPORTING
                iv_infoobject = <ls_infoobject>-infoobject.
            CALL BADI lr_badi_infoobject->get_abbreviation
              RECEIVING
                rv_abbreviation = DATA(lv_abbreviation).
            IF lv_abbreviation = zcl_core_role_admin=>get_abbreviation_from_rolename( iv_rolename ).
              rv_iobjnm = <ls_infoobject>-infoobject.
              EXIT.
            ENDIF.
          CATCH cx_rs_error.
          CATCH cx_badi_not_implemented.
        ENDTRY.
      ENDLOOP.
      ls_2_infoobject-infoobject = rv_iobjnm.
      INSERT ls_2_infoobject INTO TABLE gth_2_infoobject ASSIGNING <ls_2_infoobject>.
    ENDIF.

    rv_iobjnm = <ls_2_infoobject>-infoobject.

    IF rv_iobjnm IS INITIAL.
      RAISE EXCEPTION TYPE cx_rs_not_found
        EXPORTING
          object = |{ iv_rolename }|
          key    = |Could not find InfoObject|.
    ENDIF.

  ENDMETHOD.

  METHOD constructor.
    nv_cluster = iv_cluster.
  ENDMETHOD.

  METHOD create_data_role.
*        IMPORTING
*          iv_cluster  TYPE zcore_cluster
*          iv_txt_adso TYPE rsoadsonm
*          iv_authadso TYPE rsoadsonm,
    DATA lr_processing TYPE REF TO zcl_core_data_values.
    CREATE OBJECT lr_processing
      EXPORTING
        iv_cluster = iv_cluster.

      if iv_txt_adso is SUPPLIED and iv_authadso is SUPPLIED.
        data(lv_action) = gc_action-create.
      else.
        lv_action = gc_action-nothing.
      endif.

    data(lv_auth_block) = zcl_core_role_admin=>get_virtual_block( iv_blocktype = 'AUTH' ).
    data(lv_all_block)  = zcl_core_role_admin=>get_virtual_block( iv_blocktype = 'ALL' iv_cluster = iv_cluster ).
    "" We by default is processing the xALL block
    CALL METHOD lr_processing->create_data_role_for_block
      EXPORTING
        iv_block     = lv_all_block
        iv_action    = lv_action
      IMPORTING
        et_alltext   = DATA(lt_text)
        et_allvalues = DATA(lt_values).

    DATA(lt_alltext)   = lt_text.
    DATA(lt_allvalues) = lt_values.

    SELECT docblock
        FROM zi_core_blocksinsystem
        WHERE doccluster = @iv_cluster AND
              docblock   IN @it_block and
              docblock   <> @lv_auth_block and
              docblock   <> @lv_all_block
        INTO TABLE @DATA(lt_block).

    DATA lv_block TYPE zcore_block.

    LOOP AT lt_block INTO lv_block.
      CHECK lv_block <> zcl_core_role_admin=>get_virtual_block( iv_blocktype = 'ALL' iv_cluster = iv_cluster ).

      CALL METHOD lr_processing->create_data_role_for_block
        EXPORTING
          iv_block     = lv_block
          iv_action    = lv_action
        IMPORTING
          et_alltext   = lt_text
          et_allvalues = lt_values.

      INSERT LINES OF lt_text INTO TABLE lt_alltext.
      INSERT LINES OF lt_values INTO TABLE lt_allvalues.

    ENDLOOP.

    if lv_action = gc_action-create.
      CALL METHOD lr_processing->do_move_texts_2_adso( it_alltext = lt_alltext iv_adsonm = iv_txt_adso ).
      CALL METHOD lr_processing->do_move_values_2_adso( it_allvalues = lt_allvalues iv_adsonm = iv_authadso ).
    endif.

  ENDMETHOD.

  METHOD create_data_role_for_block.
*        IMPORTING
*          iv_block     TYPE zcore_block
*        EXPORTING
*          et_allvalues TYPE gtyts_analysisauthorization
*          et_alltext   TYPE gtyts_analysisauthorizationtxt,
    DATA:
      lr_processing      TYPE REF TO zcl_core_data_values,
      lr_badi_infoobject TYPE REF TO zbapi_data_role_value.

    zcl_core_role_admin=>static_do_message( iv_message = |Start processing of Data roles for block { iv_block }| iv_detlevel = 1 ).

    CREATE OBJECT lr_processing
      EXPORTING
        iv_cluster = zcl_core_role_admin=>get_cluster_from_block( iv_block ).

    LOOP AT lr_processing->get_infoobject_for_processing(  ) ASSIGNING FIELD-SYMBOL(<ls_infoobject>).
      " During processing the cluster is given and this will list any
      " InfoObject with a flag as authorization relevant
      zcl_core_role_admin=>static_do_message( iv_message = |Start Processing { iv_block } and Authorization relevant Iobj { <ls_infoobject>-infoobject }| iv_detlevel = 1 ).
      TRY.
          " Get a BADI implementation for that perticular InfoObject
          " Tx: SE19
          " Enhancement Spot: ZCORE_ROLE_DATA
          " BADI: ZBAPI_DATA_ROLE_VALUE
          " In order to make life easy... name the enhancement implementation
          " ZCORE_ROLE_<InfoObject>
          " Name the BAPI implemenation
          " ZBAPI_CORE_ROLE_<InfoObject>
          " The implemenation class
          " ZCL_CORE_ROLE_<InfoObject>
          " To make your life easy - Inheriate from the class
          " ZCL_CORE_DATA_DEFAULT
          " You must at least redefine the methods
          " - ZIF_DATA_ROLE_VALUE~GET_ABBREVIATION
          " this method provides the short name of an InfoObject, in case your
          " infoObject sits on something similar to Company code, you can choose the
          " class ZCL_CORE_DATA_CC... this already gives the abbreviation as 'CC'
          GET BADI lr_badi_infoobject
            FILTERS
              zcore_role_data_filter = <ls_infoobject>-infoobject.

          CALL BADI lr_badi_infoobject->set_infoobject
            EXPORTING
              iv_infoobject = <ls_infoobject>-infoobject.

          CALL BADI lr_badi_infoobject->get_all_authname
            EXPORTING
              iv_block        = iv_block
            RECEIVING
              rt_all_authname = DATA(lt_all_authname).

          zcl_core_role_admin=>static_do_message( |Processing block { iv_block }/{ <ls_infoobject>-infoobject } => Found { lines( lt_all_authname ) } S_RS_AUTH to process| ).
          LOOP AT lt_all_authname ASSIGNING FIELD-SYMBOL(<lv_authname>).

            zcl_core_role_admin=>static_do_message( iv_message = |Processing block { iv_block }/{ <ls_infoobject>-infoobject } => AUTH { <lv_authname> }| iv_detlevel = 1 ).

            " So now we have all the values
            CALL BADI lr_badi_infoobject->get_role_name
              EXPORTING
                iv_authname = <lv_authname>
              RECEIVING
                rv_rolename = DATA(lv_rolename).

            zcl_core_role_admin=>static_do_message( |Processing block { iv_block }/{ <ls_infoobject>-infoobject } => Role { lv_rolename }| ).

            " This must be called in order to makes sure the "adjust" method can find the value
            " for BWAUTH
            CALL METHOD set_authname_for_rolename
              EXPORTING
                iv_authname = <lv_authname>
                iv_rolename = lv_rolename.

            " Next update the aDSO that will contain the
            CALL METHOD lr_processing->get_entry_for_auth
              EXPORTING
                iv_authname   = <lv_authname>
                ir_infoobject = lr_badi_infoobject
              IMPORTING
                et_alltext    = DATA(lt_alltext)
                et_allvalues  = DATA(lt_allvalues).

            " next create the data role and insert the authorization lv_authname
            " if the action is anthing but 'C' we don't do anything
            if iv_action = gc_action-create.
              CALL METHOD zcl_core_role=>create( lv_rolename ).
              CALL METHOD zcl_core_role=>adjust( lv_rolename ).
            endif.

            INSERT LINES OF lt_alltext   INTO TABLE et_alltext.
            INSERT LINES OF lt_allvalues INTO TABLE et_allvalues.
            REFRESH: lt_alltext, lt_allvalues.

            zcl_core_role_admin=>static_set_detlevel( -1 ).

          ENDLOOP.

        CATCH cx_rs_error.
        CATCH cx_badi_not_implemented.
          zcl_core_role_admin=>static_do_message( |Found no BADI implementation for the InfoObject { <ls_infoobject>-infoobject }| ).

      ENDTRY.
      zcl_core_role_admin=>static_set_detlevel( -1 ).

    ENDLOOP.

    zcl_core_role_admin=>static_set_detlevel( -1 ).


  ENDMETHOD.

  METHOD create_data_role_for_user.
*        IMPORTING
*          iv_cluster TYPE zcore_cluster,
    " This method is intended to create the standard user entries per cluster
    " 1 find the authorization relevant InfoObject in the cluster
    " 2 Find the values of these infoObjects
    " 3 check is any user in BW do have access to the values
    DATA:
      lr_processing      TYPE REF TO zcl_core_data_values,
      lr_badi_infoobject TYPE REF TO zbapi_data_role_value.

    DATA:
      lt_range          TYPE gtyt_range,
      lt_allvalues      TYPE gtyts_analysisauthorization,
      lt_alltext        TYPE gtyts_analysisauthorizationtxt,
      lt_user_allvalues TYPE gtyts_analysisauthorization,
      lt_user_alltext   TYPE gtyts_analysisauthorizationtxt.

    CREATE OBJECT lr_processing
      EXPORTING
        iv_cluster = iv_cluster.

    zcl_core_role_admin=>static_do_message( |Start processing of User access for cluster { iv_cluster }| ).

    LOOP AT lr_processing->get_infoobject_for_processing(  ) ASSIGNING FIELD-SYMBOL(<ls_infoobject>).
      TRY.

          zcl_core_role_admin=>static_do_message( |Processing cluster { iv_cluster }/{ <ls_infoobject>-infoobject }| ).
          GET BADI lr_badi_infoobject
            FILTERS
              zcore_role_data_filter = <ls_infoobject>-infoobject.

          CALL BADI lr_badi_infoobject->set_infoobject
            EXPORTING
              iv_infoobject = <ls_infoobject>-infoobject.


          CALL BADI lr_badi_infoobject->get_values_from_master
            RECEIVING
              rts_chavl = DATA(lts_chavl).

          zcl_core_role_admin=>static_do_message( |Processing cluster { iv_cluster }/{ <ls_infoobject>-infoobject } ==> members found { lines( lts_chavl ) }| ).
          LOOP AT lr_processing->get_users(  ) ASSIGNING FIELD-SYMBOL(<ls_users>).
            DATA(lv_all_access) = rs_c_true.
            REFRESH:
              lt_user_allvalues, lt_user_alltext.

            LOOP AT lts_chavl ASSIGNING FIELD-SYMBOL(<lv_chavl>).
              TRY.
                  CALL BADI lr_badi_infoobject->do_check_authorization
                    EXPORTING
                      iv_chavl = <lv_chavl>
                      iv_user  = <ls_users>-username
                    RECEIVING
                      rs_range = DATA(ls_range).
                  APPEND ls_range TO lt_range.

                CATCH zcx_rs_no_user_in_source
                      zcx_rs_locked_in_source
                      cx_no_authorization.
                  " We could log this, but that should be an exception
                  " it will generated a lot of entries
                  lv_all_access = rs_c_false.
              ENDTRY.
            ENDLOOP.

            IF lv_all_access = rs_c_true.
              REFRESH lt_range.
              " The user did not miss out on any values, hence the '*' is granted
              ls_range-sign   = rs_c_range_sign-including.
              ls_range-opt    = rs_c_range_opt-pattern.
              ls_range-low    = zcl_core_data_values=>gc_anyvalue.
              ls_range-high   = ''.
              APPEND ls_range TO lt_range.
            ENDIF.

            CALL METHOD lr_processing->get_entry_for_user_access
              EXPORTING
                iv_username   = <ls_users>-username
                ir_infoobject = lr_badi_infoobject
                it_range      = lt_range
              IMPORTING
                et_alltext    = lt_user_alltext
                et_allvalues  = lt_user_allvalues.

            " insert the values to the colletive tables
            INSERT LINES OF lt_user_allvalues INTO TABLE lt_allvalues.
            INSERT LINES OF lt_user_alltext   INTO TABLE lt_alltext.

          ENDLOOP.
        CATCH cx_rs_error INTO DATA(lrx_error).
          zcl_core_role_admin=>static_do_message( iv_message = lrx_error->get_text( ) iv_message_type = rs_c_error ).
        CATCH cx_badi_not_implemented.
          " If there is no implementation - there is not autogenerated roles
          " or access given
          zcl_core_role_admin=>static_do_message( |Found no BADI implementation for the InfoObject { <ls_infoobject>-infoobject }| ).
      ENDTRY.
    ENDLOOP.

    CALL METHOD lr_processing->do_move_values_2_adso( it_allvalues = lt_allvalues iv_adsonm = iv_authadso ).
    CALL METHOD lr_processing->do_move_texts_2_adso( it_alltext = lt_alltext iv_adsonm = iv_txt_adso ).

    zcl_core_role_admin=>static_do_message( |Start processing of User access for cluster { iv_cluster }| ).

  ENDMETHOD.

  METHOD get_users.

    CALL FUNCTION 'BAPI_USER_GETLIST'
      EXPORTING
        with_username = rs_c_true
      TABLES
        userlist      = rt_users.

    delete rt_users where username = 'SAP*'.
    LOOP AT rt_users INTO DATA(ls_user).

      DATA ls_logon TYPE bapilogond.
      DATA lt_return TYPE STANDARD TABLE OF bapiret2.
      CALL FUNCTION 'BAPI_USER_GET_DETAIL'
        EXPORTING
          username  = ls_user-username
        IMPORTING
          logondata = ls_logon
        TABLES
          return    = lt_return.

      IF  ( ls_logon-gltgb is not initial and ls_logon-gltgb < sy-datum ) OR  " Not valid anymore
          ( ls_logon-gltgv is not initial and ls_logon-gltgv > sy-datum ) OR  " not value yet
            ls_logon-ustyp <>  'A'.       " Not a dialog user
        DELETE rt_users.
      ENDIF.

    ENDLOOP.
    CALL METHOD zcl_core_role_admin=>static_do_message( |Selected { lines( rt_users ) } for processing | ) .

  ENDMETHOD.


  METHOD get_connection_to_source.
    " From the the source system conversion table
    " find the RFC Destination or other form
    " of connection that is used
    SELECT SINGLE logsysnew
      INTO @rv_rfcdest
      FROM  rslogsysmap
      WHERE logsysorg = @iv_rfcdest.
    IF sy-subrc <> 0.
      rv_rfcdest = iv_rfcdest.
    ENDIF.
    CALL METHOD zcl_core_role_admin=>static_do_message( |The RFC Connection { iv_rfcdest } was translated to { rv_rfcdest }| ).

  ENDMETHOD.

  METHOD get_infoobject_for_processing.
    " This will get any InfoObject flagged as relevant for authorization
    "
    DATA: lt_objects TYPE STANDARD TABLE OF gtys_objects.

    " Create a cross access set of any InfoObject that is relevant
    " for authhoriation in the cluster
    SELECT DISTINCT infoobject
        FROM zi_core_infoobjectblockcluster
        WHERE
           relevantforauthorization = @rs_c_true AND
           doccluster               = @nv_cluster
        INTO CORRESPONDING FIELDS OF TABLE @lt_objects.
    SELECT r~doccluster, r~docsection, r~docblock, d~atrnavnm AS infoobject
        FROM rsdatrnav AS d
              INNER JOIN
             zi_core_infoobjectblockcluster AS r
                ON d~chanm = r~infoobject AND
                   d~objvers = 'A'
      WHERE d~authrelfl = @rs_c_true AND
            r~doccluster = @nv_cluster
        APPENDING CORRESPONDING FIELDS OF TABLE @lt_objects.

    " Slightly confusion... but this will return ONLY the InfoObject for a cluster... as being part of
    " xALL
    LOOP AT lt_objects INTO DATA(ls_objects).
      ls_objects-doccluster = nv_cluster.
      ls_objects-docsection = 'AAV'.
      ls_objects-docblock   = zcl_core_role_admin=>get_virtual_block( iv_blocktype = 'ALL' iv_cluster = nv_cluster ).
      INSERT ls_objects INTO TABLE rts_objects.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_authname_from_rolename.
*        IMPORTING
*          iv_rolename  type agr_name
*        RETURNING
*          VALUE(rv_authname) type rsauth,
    READ TABLE gth_role_2_auth INTO DATA(ls_role_2_auth)
      WITH TABLE KEY rolename = iv_rolename.
    IF sy-subrc <> 0.
      SELECT SINGLE agr_name AS rolename,
             auth     AS authname
         FROM zi_core_datarole2auth
         WHERE agr_name = @iv_rolename
         INTO CORRESPONDING FIELDS OF @ls_role_2_auth.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE cx_rs_not_found.
      ELSE.
        CALL METHOD set_authname_for_rolename
          EXPORTING
            iv_rolename = ls_role_2_auth-rolename
            iv_authname = ls_role_2_auth-authname.
      ENDIF.
    ENDIF.
    rv_authname = ls_role_2_auth-authname.

  ENDMETHOD.

  METHOD set_authname_for_rolename.
*        IMPORTING
*          iv_rolename  type agr_name
*          iv_authname  type agr_name,
    DELETE gth_role_2_auth
      WHERE rolename = iv_rolename.
    INSERT VALUE #( rolename = iv_rolename
                    authname = iv_authname ) INTO TABLE gth_role_2_auth.
  ENDMETHOD.

  METHOD do_move_values_2_adso.
    DATA:
      lv_lines_inserted TYPE int4,
      lt_msg            TYPE  rs_t_msg,
      lv_upd_req_tsn    TYPE  rspm_request_tsn,
      lv_act_req_tsn    TYPE  rsdso_t_tsn.

    DATA lt_allvalues TYPE gtyt_analysisauthorization.
    APPEND LINES OF it_allvalues TO lt_allvalues.

    CALL METHOD zcl_core_role_admin=>static_do_message( iv_message = |Start loading authorization data into aDSO { iv_adsonm } lines { lines( it_allvalues ) }| iv_detlevel = 1 ).

    CALL FUNCTION 'RSDSO_WRITE_API'
      EXPORTING
        i_adsonm            = iv_adsonm
        it_data             = lt_allvalues
      IMPORTING
        e_lines_inserted    = lv_lines_inserted
        et_msg              = lt_msg
        e_upd_req_tsn       = lv_upd_req_tsn
        et_act_req_tsn      = lv_act_req_tsn
      EXCEPTIONS
        write_failed        = 1
        activation_failed   = 2
        datastore_not_found = 3
        OTHERS              = 4.
    DATA(lv_subrc) = sy-subrc.
    LOOP AT lt_msg ASSIGNING FIELD-SYMBOL(<ls_msg>).
      MESSAGE ID <ls_msg>-msgid TYPE <ls_msg>-msgty NUMBER <ls_msg>-msgno WITH <ls_msg>-msgv1 <ls_msg>-msgv2 <ls_msg>-msgv3 <ls_msg>-msgv4 INTO _message.
      CALL METHOD zcl_core_role_admin=>static_do_message( ).
    ENDLOOP.
    IF lv_subrc <> 0.
      call METHOD zcl_core_role_admin=>static_set_detlevel( -1 ).
      RAISE EXCEPTION TYPE cx_rs_msg
        EXPORTING
          msgid = sy-msgid
          msgty = sy-msgty
          msgno = sy-msgno
          msgv1 = sy-msgv1
          msgv2 = sy-msgv2
          msgv3 = sy-msgv3
          msgv4 = sy-msgv4.
    ENDIF.

    call METHOD zcl_core_role_admin=>static_set_detlevel( -1 ).

  ENDMETHOD.

  METHOD do_move_texts_2_adso.
    DATA:
      lv_lines_inserted TYPE int4,
      lt_msg            TYPE  rs_t_msg,
      lv_upd_req_tsn    TYPE  rspm_request_tsn,
      lv_act_req_tsn    TYPE  rsdso_t_tsn.

    DATA lt_alltext TYPE gtyt_analysisauthorizationtxt.
    APPEND LINES OF it_alltext TO lt_alltext.

    CALL METHOD zcl_core_role_admin=>static_do_message( iv_message = |Start loading authorization data into aDSO { iv_adsonm } lines { lines( it_alltext ) }| iv_detlevel = 1 ).

    CALL FUNCTION 'RSDSO_WRITE_API'
      EXPORTING
        i_adsonm            = iv_adsonm
        it_data             = lt_alltext
      IMPORTING
        e_lines_inserted    = lv_lines_inserted
        et_msg              = lt_msg
        e_upd_req_tsn       = lv_upd_req_tsn
        et_act_req_tsn      = lv_act_req_tsn
      EXCEPTIONS
        write_failed        = 1
        activation_failed   = 2
        datastore_not_found = 3
        OTHERS              = 4.
    DATA(lv_subrc) = sy-subrc.
    LOOP AT lt_msg ASSIGNING FIELD-SYMBOL(<ls_msg>).
      MESSAGE ID <ls_msg>-msgid TYPE <ls_msg>-msgty NUMBER <ls_msg>-msgno WITH <ls_msg>-msgv1 <ls_msg>-msgv2 <ls_msg>-msgv3 <ls_msg>-msgv4 INTO _message.
      CALL METHOD zcl_core_role_admin=>static_do_message( ).
    ENDLOOP.
    IF lv_subrc <> 0.
      call METHOD zcl_core_role_admin=>static_set_detlevel( -1 ).
      RAISE EXCEPTION TYPE cx_rs_msg
        EXPORTING
          msgid = sy-msgid
          msgty = sy-msgty
          msgno = sy-msgno
          msgv1 = sy-msgv1
          msgv2 = sy-msgv2
          msgv3 = sy-msgv3
          msgv4 = sy-msgv4.
    ENDIF.

    call METHOD zcl_core_role_admin=>static_set_detlevel( -1 ).

  ENDMETHOD.
ENDCLASS.
