class ZCL_4_MONSTER_BUNNY_JUPITER definition
  public
  create public .

public section.

  interfaces IF_SADL_EXIT .
  interfaces IF_SADL_EXIT_CALC_ELEMENT_READ .

  aliases CALCULATE
    for IF_SADL_EXIT_CALC_ELEMENT_READ~CALCULATE .
  aliases GET_CALCULATION_INFO
    for IF_SADL_EXIT_CALC_ELEMENT_READ~GET_CALCULATION_INFO .
  aliases TT_ELEMENTS
    for IF_SADL_EXIT_CALC_ELEMENT_READ~TT_ELEMENTS .
protected section.
private section.
ENDCLASS.



CLASS ZCL_4_MONSTER_BUNNY_JUPITER IMPLEMENTATION.


  METHOD if_sadl_exit_calc_element_read~calculate ##NEEDED.

* To be Implemented

  ENDMETHOD.


  METHOD if_sadl_exit_calc_element_read~get_calculation_info ##NEEDED.

* To be implemented

  ENDMETHOD.
ENDCLASS.
