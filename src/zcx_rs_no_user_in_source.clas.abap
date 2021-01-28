CLASS zcx_rs_no_user_in_source DEFINITION
  PUBLIC
  INHERITING FROM cx_rs_error
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_dyn_msg .
    INTERFACES if_t100_message .

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !user     like sy-uname.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcx_rs_no_user_in_source IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    CLEAR me->textid.

    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key-msgid = '/UI2/FDM'.
      if_t100_message~t100key-msgno = '005'.
      if_t100_message~t100key-attr1 = user.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
