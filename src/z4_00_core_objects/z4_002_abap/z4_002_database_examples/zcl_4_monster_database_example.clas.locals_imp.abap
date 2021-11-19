*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_monster_functions DEFINITION ##CLASS_FINAL.
  PUBLIC SECTION.
    METHODS hat_size_of_the_day RETURNING VALUE(rt_hat_size) TYPE z4de_monster_hat_size.

ENDCLASS.

CLASS lcl_monster_functions IMPLEMENTATION.

  METHOD hat_size_of_the_day.

    rt_hat_size = 5.

  ENDMETHOD.

ENDCLASS.
