class ZCX_4_POWER_FAILURE definition
  public
  inheriting from CX_NO_CHECK
  create public .

public section.

  constants ZCX_4_POWER_FAILURE type SOTR_CONC value '005056B074C91EDB9F97947C3EF7A12A' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_4_POWER_FAILURE IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
 IF textid IS INITIAL.
   me->textid = ZCX_4_POWER_FAILURE .
 ENDIF.
  endmethod.
ENDCLASS.
