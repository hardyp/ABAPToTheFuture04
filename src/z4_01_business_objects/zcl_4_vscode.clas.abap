CLASS zcl_4_vscode DEFINITION
  PUBLIC
  CREATE PRIVATE .

  PUBLIC SECTION.
    METHODS do_something_wonderful.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS call_non_existant_method.
    METHODS test.
ENDCLASS.

CLASS zcl_4_vscode IMPLEMENTATION.
  METHOD do_something_wonderful.
    call_non_existant_method(  ).
  ENDMETHOD.


  METHOD call_non_existant_method.
    DATA: monster_is_bonkers TYPE bool.

  ENDMETHOD.

  METHOD test.
    " given

    " when

    " then

  ENDMETHOD.

ENDCLASS.
