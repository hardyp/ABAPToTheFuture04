*--------------------------------------------------------------------*
* Listing 05.11: Monster Object Factory Class Definition
*--------------------------------------------------------------------*
class ZCL_4_MONSTER_FACTORY definition
  public
  final
  create public

  global friends ZCL_4_MONSTER_INJECTOR .

public section.

  class-methods GET_INSTANCE
    returning
      value(RO_FACTORY) type ref to ZCL_4_MONSTER_FACTORY .
  methods GET_LOGGER
    returning
      value(RO_LOGGER) type ref to ZIF_4_MONSTER_LOGGER .
  methods GET_MONSTER_BO_PL
    returning
      value(RO_PL) type ref to ZIF_4_MONSTERMODEL_PERS_LAYER .
  methods GET_MONSTER_MAKING_MACHINE
    returning
      value(RO_MONSTER_MAKING_MACHINE) type ref to ZIF_4_MONSTER_MAKING_MACHINE .
  methods GET_MONSTER_SIM_PL
    importing
      !ID_VALID_ON type SY-DATUM optional
      !IO_LOGGER type ref to ZIF_4_MONSTER_LOGGER optional
    returning
      value(RO_MONSTER_SIM_PL) type ref to ZIF_4_MONSTER_SIM_PERS_LAYER .
protected section.
private section.

  data MO_MONSTER_BO_PL type ref to ZIF_4_MONSTERMODEL_PERS_LAYER .
  data MO_MONSTER_SIM_PL type ref to ZIF_4_MONSTER_SIM_PERS_LAYER .
  data MO_LOGGER type ref to ZIF_4_MONSTER_LOGGER .
  data MO_MONSTER_MAKING_MACHINE type ref to ZIF_4_MONSTER_MAKING_MACHINE .
  class-data MO_FACTORY type ref to ZCL_4_MONSTER_FACTORY .
ENDCLASS.



CLASS ZCL_4_MONSTER_FACTORY IMPLEMENTATION.


  METHOD get_instance.

    IF mo_factory IS NOT BOUND.
      mo_factory = NEW #( ).
    ENDIF.

    ro_factory = mo_factory.

  ENDMETHOD.


  METHOD get_logger.

    IF mo_logger IS NOT BOUND.
      mo_logger = NEW zcl_4_monster_logger( ).
    ENDIF.

    ro_logger = mo_logger.

  ENDMETHOD.


  METHOD get_monster_bo_pl.

    IF mo_monster_bo_pl IS NOT BOUND.
      mo_monster_bo_pl = NEW zcl_4_monstermodel_pers_bopf( ).
    ENDIF.

    ro_pl = mo_monster_bo_pl.

  ENDMETHOD.


  METHOD get_monster_making_machine.
*--------------------------------------------------------------------*
* Listing 05.13: Monster Object Factory Returning Instance
*--------------------------------------------------------------------*
    IF mo_monster_making_machine IS NOT BOUND.
      mo_monster_making_machine = NEW zcl_4_monster_making_machine( ).
    ENDIF.

    ro_monster_making_machine = mo_monster_making_machine.

  ENDMETHOD.


  METHOD get_monster_sim_pl.

    DATA: ld_valid_on TYPE sy-datum,
          lo_logger   TYPE REF TO zif_4_monster_logger.

    IF id_valid_on IS NOT INITIAL.
      ld_valid_on = id_valid_on.
    ELSE.
      ld_valid_on = sy-datum.
    ENDIF.

    IF io_logger IS BOUND.
      lo_logger = io_logger.
    ELSE.
      lo_logger = get_logger( ).
    ENDIF.

    IF mo_monster_sim_pl IS NOT BOUND.
      mo_monster_sim_pl = NEW zcl_4_monster_sim_pers_layer(
          id_valid_on = ld_valid_on
          io_logger   = lo_logger ).
    ENDIF.

    ro_monster_sim_pl = mo_monster_sim_pl.

  ENDMETHOD.
ENDCLASS.
