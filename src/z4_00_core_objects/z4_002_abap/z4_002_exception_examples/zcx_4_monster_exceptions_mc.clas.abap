class ZCX_4_MONSTER_EXCEPTIONS_MC definition
  public
  inheriting from CX_NO_CHECK
  final
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  constants:
    begin of ZCX_4_MONSTER_EXCEPTIONS_MC,
      msgid type symsgid value 'Z4MONSTERS',
      msgno type symsgno value '000',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of ZCX_4_MONSTER_EXCEPTIONS_MC .
  constants:
    begin of FAR_TOO_MANY_HEADS,
      msgid type symsgid value 'Z4MONSTERS',
      msgno type symsgno value '006',
      attr1 type scx_attrname value 'MONSTER_HEAD_COUNT',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of FAR_TOO_MANY_HEADS .
  constants:
    begin of COMPONENT_PROBLEM,
      msgid type symsgid value 'Z4MONSTERS',
      msgno type symsgno value '008',
      attr1 type scx_attrname value 'SCARINESS',
      attr2 type scx_attrname value 'HEADS',
      attr3 type scx_attrname value 'ARMS',
      attr4 type scx_attrname value 'LEGS',
    end of COMPONENT_PROBLEM .
  constants:
    begin of HEAD_HAT_DISPARITY,
      msgid type symsgid value 'Z4MONSTERS',
      msgno type symsgno value '001',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of HEAD_HAT_DISPARITY .
  constants:
    begin of NO_HEAD_HOWLING_PROBLEM,
      msgid type symsgid value 'Z4MONSTERS',
      msgno type symsgno value '002',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NO_HEAD_HOWLING_PROBLEM .
  constants:
    begin of DATABASE_UPDATE_FAILURE,
      msgid type symsgid value 'Z4MONSTERS',
      msgno type symsgno value '009',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of DATABASE_UPDATE_FAILURE .
  constants:
    begin of MEMORY_UPDATE_FAILURE,
      msgid type symsgid value 'Z4MONSTERS',
      msgno type symsgno value '010',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of MEMORY_UPDATE_FAILURE .
  data MONSTER_HEAD_COUNT type I .
  data SCARINESS type STRING .
  data HEADS type INT4 .
  data ARMS type INT4 .
  data LEGS type INT4 .
  data BOLTS type INT4 .
  data WIBBLY_WOBBLY_WOOS type I .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MONSTER_HEAD_COUNT type I optional
      !SCARINESS type STRING optional
      !HEADS type INT4 optional
      !ARMS type INT4 optional
      !LEGS type INT4 optional
      !BOLTS type INT4 optional
      !WIBBLY_WOBBLY_WOOS type I optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_4_MONSTER_EXCEPTIONS_MC IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MONSTER_HEAD_COUNT = MONSTER_HEAD_COUNT .
me->SCARINESS = SCARINESS .
me->HEADS = HEADS .
me->ARMS = ARMS .
me->LEGS = LEGS .
me->BOLTS = BOLTS .
me->WIBBLY_WOBBLY_WOOS = WIBBLY_WOBBLY_WOOS .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_4_MONSTER_EXCEPTIONS_MC .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
