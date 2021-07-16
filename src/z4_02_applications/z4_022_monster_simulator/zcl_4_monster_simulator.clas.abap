class ZCL_4_MONSTER_SIMULATOR definition
  public
  create public .

public section.

  interfaces ZIF_4_MONSTER_SIMULATOR .

  aliases CALCULATE_SCARINESS
    for ZIF_4_MONSTER_SIMULATOR~CALCULATE_SCARINESS .
  aliases DERIVE_ALL_HEADS_UNDER_BED
    for ZIF_4_MONSTER_SIMULATOR~DERIVE_ALL_HEADS_UNDER_BED .

  data MD_SCARINESS type ZDE_MONSTER_SCARINESS read-only .
  data MD_FLUFFINESS type ZDE_MONSTER_FLUFFINESS read-only .
  data MD_BOLTS_IN_NECK type ZDE_BOLTS_IN_NECK read-only .
  data MD_COLOR type ZDE_MONSTER_COLOR read-only .

  methods SIMULATE_MONSTER_BOM
    importing
      !IS_BOM_INPUT_DATA type Z4S_MONSTER_INPUT_DATA
    exporting
      !ET_BOM_DATA type Z4TT_MONSTER_ITEMS .
  methods CONSTRUCTOR
    importing
      !IO_PERS_LAYER type ref to ZIF_4_MONSTER_SIM_PERS_LAYER optional
      !IO_LOGGER type ref to ZIF_4_MONSTER_LOGGER optional
      !IO_MONSTER_MAKING_MACHINE type ref to ZIF_4_MONSTER_MAKING_MACHINE optional .
  methods OPEN_MONSTERS_EYES
    raising
      ZCX_4_STATIC_ELECTRICITY .
  methods GET_COMPONENT_SPLIT
    importing
      !ID_STRENGTH type ZDE_MONSTER_STRENGTH
      !ID_BRAIN_SIZE type ZDE_MONSTER_BRAIN_SIZE
      !ID_SANITY type ZDE_MONSTER_SANITY
    exporting
      !ID_SSATN type ZDE_COMPONENT_TYPE_PERCENTAGE
      !ID_SSPDT type ZDE_COMPONENT_TYPE_PERCENTAGE .
  PROTECTED SECTION.
private section.

*--------------------------------------------------------------------*
* Listing 05.06 : Helper Classes as Private Instance Variables of Main Class
*--------------------------------------------------------------------*
  data MO_LOGGER type ref to ZIF_4_MONSTER_LOGGER .
  data MO_PERS_LAYER type ref to ZIF_4_MONSTER_SIM_PERS_LAYER .
  data MO_MONSTER_MAKING_MACHINE type ref to ZIF_4_MONSTER_MAKING_MACHINE .
ENDCLASS.



CLASS ZCL_4_MONSTER_SIMULATOR IMPLEMENTATION.


  METHOD CONSTRUCTOR.
*--------------------------------------------------------------------*
* Listing 05.07 : Variables Set Up during Construction of Object Instance
* Listing 05.08 : Constructor Implementation
*--------------------------------------------------------------------*

*--------------------------------------------------------------------*
* 05.07 - Bad way to do things (Direct Creation)
*--------------------------------------------------------------------*
    CREATE OBJECT mo_logger TYPE zcl_4_monster_logger.

    CREATE OBJECT mo_monster_making_machine TYPE zcl_4_monster_making_machine.

    CREATE OBJECT mo_pers_layer TYPE zcl_4_monster_sim_pers_layer
      EXPORTING
        io_logger   = mo_logger    " Logging Class
        id_valid_on = sy-datum.    " Validaty Date

*--------------------------------------------------------------------*
* 05.08 - Better way to do things (Parameter Injection)
*--------------------------------------------------------------------*
    IF io_logger IS BOUND.
      mo_logger = io_logger.
    ELSE.
      CREATE OBJECT mo_logger TYPE zcl_4_monster_logger.
    ENDIF.

    IF io_monster_making_machine IS BOUND.
      mo_monster_making_machine = io_monster_making_machine.
    ELSE.
      CREATE OBJECT mo_monster_making_machine TYPE zcl_4_monster_making_machine.
    ENDIF.

    IF io_pers_layer IS BOUND.
      mo_pers_layer = io_pers_layer.
    ELSE.
      CREATE OBJECT mo_pers_layer TYPE zcl_4_monster_sim_pers_layer
        EXPORTING
          io_logger   = mo_logger
          id_valid_on = sy-datum.
    ENDIF.

*--------------------------------------------------------------------*
* 05.09 - Best way to do things (Instance Creation via Factory)
*--------------------------------------------------------------------*
    DATA(lo_factory) = zcl_4_monster_factory=>get_instance( ).

    mo_logger = lo_factory->get_logger( ).

    "During creation of the PL by the factory the correct instance
    "of the logger is inserted (using the factory!)
    mo_pers_layer = lo_factory->get_monster_sim_pl( ).

    mo_monster_making_machine = lo_factory->get_monster_making_machine( ).

  ENDMETHOD.


  METHOD GET_COMPONENT_SPLIT.

* Real Business Logic Goes Here
* BRF+ Decision most likely

  ENDMETHOD.


  METHOD OPEN_MONSTERS_EYES.

  ENDMETHOD.


  METHOD SIMULATE_MONSTER_BOM.

  ENDMETHOD.


  METHOD zif_4_monster_simulator~calculate_scariness.

* To Be Implemented

  ENDMETHOD.


  METHOD zif_4_monster_simulator~derive_all_heads_under_bed.

    DATA(factory)    = zcl_monster_object_factory=>get_instance(  ).
    DATA(pers_layer) = factory->get_persistency_layer( ).

    DATA(monsters_types_under_bed) = pers_layer->derive_monsters_under_bed( id_bed_name ).

    LOOP AT monsters_types_under_bed ASSIGNING FIELD-SYMBOL(<monsters_types>).

      DATA(heads_per_monster) = pers_layer->derive_heads_per_monster_type( <monsters_types>-monster_type ).

      result = result + ( heads_per_monster * <monsters_types>-total_of_type ).

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
