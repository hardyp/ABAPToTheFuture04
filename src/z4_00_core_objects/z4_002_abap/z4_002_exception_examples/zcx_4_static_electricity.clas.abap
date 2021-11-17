class ZCX_4_STATIC_ELECTRICITY definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  constants ZCX_4_STATIC_ELECTRICITY type SOTR_CONC value '005056B074C91EDB9EFC9095B923E12A' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_4_STATIC_ELECTRICITY IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
 IF textid IS INITIAL.
   me->textid = ZCX_4_STATIC_ELECTRICITY .
 ENDIF.
  endmethod.
ENDCLASS.
