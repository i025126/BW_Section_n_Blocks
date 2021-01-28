INTERFACE zif_data_role_value
  PUBLIC .


  INTERFACES if_badi_interface .

  TYPES:
    gtyt_range     TYPE STANDARD TABLE OF rrrange WITH NON-UNIQUE DEFAULT KEY,
    gtyt_agr_texts type STANDARD TABLE OF agr_texts WITH NON-UNIQUE DEFAULT KEY,
    gtyt_authname  TYPE STANDARD TABLE OF rsauth WITH NON-UNIQUE DEFAULT KEY.

  " Give the values of the authorization object, this will usually be a select on
  " an InfoObject master table. But it can also be SELECT DISTINCT on a transactional
  " table, to avoid bad data and reduce the number of distinct values
  METHODS get_values_from_master
    RETURNING
      VALUE(rts_chavl) TYPE rsd_ts_chavl
    RAISING
      cx_rs_error .
  " Returns the list of the Authorization that must be generated and inserted into
  " the aDSO that will be used for generation in RSECADMIN
  METHODS get_all_authname
    IMPORTING
      iv_block              TYPE zcore_block
    RETURNING
      VALUE(rt_all_authname) TYPE gtyt_authname.
  " Returns the text for the role that is generated, the desciption of the AUTH is
  " fixed. But it is possible to added multiple lines of text to the role, if this
  " is multiple values etc...
  METHODS get_texts
    IMPORTING
      iv_rolename    TYPE agr_name
    RETURNING
      VALUE(rt_texts) TYPE gtyt_agr_texts.
  " Get the ranges for the AUTH that is inserted in to the aDSO and generated - this
  " means that one authorization can contain multiple values and you can edit the
  " value part in the postfix to match what you need
  METHODS get_table_of_range
    IMPORTING
      !iv_authname    TYPE rsauth
    RETURNING
      VALUE(rt_range) TYPE gtyt_range.
  " The very important on - this does the translation between an infoobject and the
  " two character abbreviation Company Code => CC f.x. Funds Center => FC
  methods set_infoobject
    IMPORTING
      iv_infoobject   type rsiobjnm.
  METHODS get_infoobject
    RETURNING
      VALUE(rv_infoobject) type rsiobjnm.
  METHODS get_abbreviation
    RETURNING
      VALUE(rv_abbreviation) TYPE zcore_abbreviation
    RAISING
      cx_rs_error .
  METHODS do_check_authorization
    IMPORTING
      iv_user        TYPE syst_uname
      iv_chavl       TYPE rschavl
    RETURNING
      VALUE(rs_range) TYPE rrrange
    RAISING
      cx_no_authorization
      zcx_rs_no_user_in_source
      zcx_rs_locked_in_source
      cx_rs_error .
  METHODS get_role_name
    IMPORTING
      iv_authname       TYPE rsauth
    RETURNING
      VALUE(rv_rolename) TYPE agr_name.

ENDINTERFACE.
