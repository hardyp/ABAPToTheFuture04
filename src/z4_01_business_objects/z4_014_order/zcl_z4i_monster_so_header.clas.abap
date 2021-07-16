class ZCL_Z4I_MONSTER_SO_HEADER definition
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



CLASS ZCL_Z4I_MONSTER_SO_HEADER IMPLEMENTATION.


  method GET_PATHS.
et_paths = VALUE #(
( |CDS~Z4I_MONSTER_SO_HEADER| )
).
  endmethod.


  method GET_TIMESTAMP.
RV_TIMESTAMP = 20210701084339.
  endmethod.
ENDCLASS.
