class ZCL_4_MONSTER_SIMULATOR definition
  public
  create public .

public section.

  interfaces ZIF_4_MONSTER_SIMULATOR .

  aliases CALCULATE_SCARINESS
    for ZIF_4_MONSTER_SIMULATOR~CALCULATE_SCARINESS .
  aliases DERIVE_ALL_HEADS_UNDER_BED
    for ZIF_4_MONSTER_SIMULATOR~DERIVE_ALL_HEADS_UNDER_BED .
  aliases GET_COMPONENT_SPLIT
    for ZIF_4_MONSTER_SIMULATOR~GET_COMPONENT_SPLIT .
  aliases OPEN_MONSTERS_EYES
    for ZIF_4_MONSTER_SIMULATOR~OPEN_MONSTERS_EYES .
  aliases SIMULATE_MONSTER_BOM
    for ZIF_4_MONSTER_SIMULATOR~SIMULATE_MONSTER_BOM .

  data MD_SCARINESS type Z4DE_MONSTER_SCARINESS read-only .
  data MD_FLUFFINESS type Z4DE_MONSTER_FLUFFINESS read-only .
  data MD_BOLTS_IN_NECK type Z4DE_BOLTS_IN_NECK read-only .
  data MD_COLOR type Z4DE_MONSTER_COLOR read-only .

  methods CONSTRUCTOR
    importing
      !IO_PERS_LAYER type ref to ZIF_4_MONSTER_SIM_PERS_LAYER optional
      !IO_LOGGER type ref to ZIF_4_MONSTER_LOGGER optional
      !IO_MONSTER_MAKING_MACHINE type ref to ZIF_4_MONSTER_MAKING_MACHINE optional .
  PROTECTED SECTION.
private section.

*----------------------------------------------------------------------------*
* Listing 05.07: Helper Classes as Private Instance Variables of Main Class
*----------------------------------------------------------------------------*
  data MO_LOGGER type ref to ZIF_4_MONSTER_LOGGER .
  data MO_PERS_LAYER type ref to ZIF_4_MONSTER_SIM_PERS_LAYER .
  data MO_MONSTER_MAKING_MACHINE type ref to ZIF_4_MONSTER_MAKING_MACHINE .
ENDCLASS.



CLASS ZCL_4_MONSTER_SIMULATOR IMPLEMENTATION.


  METHOD constructor.
*-----------------------------------------------------------------------*
* Listing 05.08 : Variables Set Up during Construction of Object Instance
* Listing 05.09 : Constructor Implementation
* Listing 05.10 : Instance Creation via Factory
*-----------------------------------------------------------------------*

*--------------------------------------------------------------------*
* Listing 05.08: - Bad way to do things (Direct Creation)
*--------------------------------------------------------------------*
    mo_logger = NEW zcl_4_monster_logger( ).

    mo_monster_making_machine = NEW zcl_4_monster_making_machine( ).

    mo_pers_layer = NEW zcl_4_monster_sim_pers_layer( io_logger   = mo_logger
                                                      id_valid_on = sy-datum ).

*--------------------------------------------------------------------*
* Listing 05.09: - Better way to do things (Parameter Injection)
*--------------------------------------------------------------------*
    IF io_logger IS BOUND.
      mo_logger = io_logger.
    ELSE.
      mo_logger = NEW zcl_4_monster_logger( ).
    ENDIF.

    IF io_monster_making_machine IS BOUND.
      mo_monster_making_machine = io_monster_making_machine.
    ELSE.
      mo_monster_making_machine = NEW zcl_4_monster_making_machine( ).
    ENDIF.

    IF io_pers_layer IS BOUND.
      mo_pers_layer = io_pers_layer.
    ELSE.
      mo_pers_layer = NEW zcl_4_monster_sim_pers_layer( io_logger   = mo_logger
                                                        id_valid_on = sy-datum ).
    ENDIF.

*-----------------------------------------------------------------------*
* Listing 05.10: - Best way to do things (Instance Creation via Factory)
*-----------------------------------------------------------------------*
    DATA(lo_factory) = zcl_4_monster_factory=>get_instance( ).

    mo_logger = lo_factory->get_logger( ).

    "During creation of the PL by the factory the correct instance
    "of the logger is inserted (using the factory!)
    mo_pers_layer = lo_factory->get_monster_sim_pl( ).

    mo_monster_making_machine = lo_factory->get_monster_making_machine( ).

*--------------------------------------------------------------------------------------------------*
* Each of the five Suspect Monsters has a different HEIGHT. One is really tall, one really small,
* but all are different heights.
*--------------------------------------------------------------------------------------------------*
  ENDMETHOD.


  METHOD zif_4_monster_simulator~calculate_scariness ##NEEDED.

* To Be Implemented

  ENDMETHOD.


  METHOD zif_4_monster_simulator~derive_all_heads_under_bed.

    DATA(factory) = zcl_4_monster_factory=>get_instance( ).
    DATA(pers_layer) = factory->get_monster_sim_pl( ).

    DATA(monsters_types_under_bed) = pers_layer->derive_monsters_under_bed( id_bed_name ).

    LOOP AT monsters_types_under_bed ASSIGNING FIELD-SYMBOL(<monsters_types>).

      DATA(heads_per_monster) = pers_layer->derive_heads_per_monster_type( <monsters_types>-monster_type ).

      result = result + ( heads_per_monster * <monsters_types>-total_of_type ).

    ENDLOOP.

  ENDMETHOD.


  METHOD ZIF_4_MONSTER_SIMULATOR~GET_COMPONENT_SPLIT ##NEEDED.

* Real Business Logic Goes Here
* BRF+ Decision most likely

  ENDMETHOD.


  METHOD ZIF_4_MONSTER_SIMULATOR~OPEN_MONSTERS_EYES ##NEEDED.

  ENDMETHOD.


  METHOD ZIF_4_MONSTER_SIMULATOR~SIMULATE_MONSTER_BOM ##NEEDED.

  ENDMETHOD.
ENDCLASS.
