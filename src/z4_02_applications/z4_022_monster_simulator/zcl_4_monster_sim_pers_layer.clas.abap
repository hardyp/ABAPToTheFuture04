class ZCL_4_MONSTER_SIM_PERS_LAYER definition
  public
  create public .

public section.

  interfaces ZIF_4_MONSTER_SIM_PERS_LAYER .

  methods CONSTRUCTOR
    importing
      !ID_VALID_ON type SY-DATUM
      !IO_LOGGER type ref to ZIF_4_MONSTER_LOGGER optional .
protected section.
private section.

  data MD_VALID_ON type SY-DATUM ##NEEDED.
  data MO_LOGGER type ref to ZIF_4_MONSTER_LOGGER ##NEEDED.
ENDCLASS.



CLASS ZCL_4_MONSTER_SIM_PERS_LAYER IMPLEMENTATION.


METHOD constructor.

  md_valid_on = id_valid_on.

  IF io_logger IS SUPPLIED.
    mo_logger = io_logger.
  ELSE.
    mo_logger = NEW zcl_4_monster_logger( ).
  ENDIF.

ENDMETHOD.
ENDCLASS.
