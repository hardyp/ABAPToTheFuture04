class ZCX_4_USER_CANCELLED definition
  public
  inheriting from CX_NO_CHECK
  create public .

public section.

  constants ZCX_4_USER_CANCELLED type SOTR_CONC value '005056B074C91EDB9EFCBC7E1C59812A' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_4_USER_CANCELLED IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
 IF textid IS INITIAL.
   me->textid = ZCX_4_USER_CANCELLED .
 ENDIF.
  endmethod.
ENDCLASS.
