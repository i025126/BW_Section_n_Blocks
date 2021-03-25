class ZCL_ZI_ROLEGENERATION definition
  public
  inheriting from CL_SADL_GTK_EXPOSURE_MPC
  final
  create public .

public section.
protected section.

  methods GET_PATHS
    redefinition .
  methods GET_TIMESTAMP
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZI_ROLEGENERATION IMPLEMENTATION.


  method GET_PATHS.
et_paths = VALUE #(
( |CDS~ZI_ROLEGENERATION| )
).
  endmethod.


  method GET_TIMESTAMP.
RV_TIMESTAMP = 20210322115238.
  endmethod.
ENDCLASS.
