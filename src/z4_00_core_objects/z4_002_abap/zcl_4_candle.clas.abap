class ZCL_4_CANDLE definition
  public
  create public .

public section.

  methods LIGHT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_4_CANDLE IMPLEMENTATION.


METHOD LIGHT.

  MESSAGE 'Lighting Candle' TYPE 'I'.

ENDMETHOD.
ENDCLASS.
