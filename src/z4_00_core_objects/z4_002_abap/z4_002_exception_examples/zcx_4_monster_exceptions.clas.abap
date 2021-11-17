class ZCX_4_MONSTER_EXCEPTIONS definition
  public
  inheriting from CX_NO_CHECK
  create public .

public section.

  constants ZCX_4_MONSTER_EXCEPTIONS type SOTR_CONC value '005056B074C91EDB9EFDA1DE26C3C12A' ##NO_TEXT.
  constants MONSTER_DESIGN_PROBLEM type SOTR_CONC value '005056B074C91EDB9EFE2CCBC3B8C12A' ##NO_TEXT.
  constants WIBBLY_WOBBLY_WOO_PROBLEM type SOTR_CONC value '005056B074C91EDB9FAD3591A942A12A' ##NO_TEXT.
  data WIBBLY_WOBBLY_WOOS type I .
  data HEADS type I .
  data ARMS type I .
  data LEGS type I .
  data TAILS type I .
  data TENTACLES type I .
  data LOGGER type ref to ZCL_LOGGER .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !WIBBLY_WOBBLY_WOOS type I optional
      !HEADS type I optional
      !ARMS type I optional
      !LEGS type I optional
      !TAILS type I optional
      !TENTACLES type I optional
      !LOGGER type ref to ZCL_LOGGER optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_4_MONSTER_EXCEPTIONS IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
TEXTID = TEXTID
PREVIOUS = PREVIOUS
.
 IF textid IS INITIAL.
   me->textid = ZCX_4_MONSTER_EXCEPTIONS .
 ENDIF.
me->WIBBLY_WOBBLY_WOOS = WIBBLY_WOBBLY_WOOS .
me->HEADS = HEADS .
me->ARMS = ARMS .
me->LEGS = LEGS .
me->TAILS = TAILS .
me->TENTACLES = TENTACLES .
me->LOGGER = LOGGER .
  endmethod.
ENDCLASS.
