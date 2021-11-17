class ZCL_4_MONSTER_INJECTOR definition
  public
  create public .

public section.

  methods CONSTRUCTOR .
  methods INJECT_MONSTER_SIM_PL
    importing
      !IO_PERS_LAYER type ref to ZIF_4_MONSTER_SIM_PERS_LAYER .
  methods INJECT_LOGGER
    importing
      !IO_LOGGER type ref to ZIF_4_MONSTER_LOGGER .
  methods INJECT_MONSTER_MAKING_MACHINE
    importing
      !IO_MONSTER_MAKING_MACHINE type ref to ZIF_4_MONSTER_MAKING_MACHINE .
  methods INJECT_MONSTER_BO_PL
    importing
      !IO_MONSTER_BO_PL type ref to ZIF_4_MONSTERMODEL_PERS_LAYER .
protected section.
private section.

  data MO_FACTORY type ref to ZCL_4_MONSTER_FACTORY .
ENDCLASS.



CLASS ZCL_4_MONSTER_INJECTOR IMPLEMENTATION.


  METHOD constructor.

    CLEAR zcl_4_monster_factory=>mo_factory.

    mo_factory = zcl_4_monster_factory=>get_instance( ).

  ENDMETHOD.


  METHOD INJECT_LOGGER.

    mo_factory->mo_logger = io_logger.

  ENDMETHOD.


  METHOD inject_monster_bo_pl.

    mo_factory->mo_monster_bo_pl = io_monster_bo_pl.

  ENDMETHOD.


  METHOD INJECT_MONSTER_MAKING_MACHINE.
*--------------------------------------------------------------------*
* Listing 05.12: Monster Object Injector
*--------------------------------------------------------------------*
    mo_factory->mo_monster_making_machine = io_monster_making_machine.

  ENDMETHOD.


  METHOD INJECT_MONSTER_SIM_PL.

    mo_factory->mo_monster_sim_pl = io_pers_layer.

  ENDMETHOD.
ENDCLASS.
