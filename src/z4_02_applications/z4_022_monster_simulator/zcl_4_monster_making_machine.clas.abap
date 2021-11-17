class ZCL_4_MONSTER_MAKING_MACHINE definition
  public
  create public

  global friends ZCL_4_MONSTER_FACTORY .

public section.

  interfaces ZIF_4_MONSTER_MAKING_MACHINE .

  aliases MAKE_MONSTER
    for ZIF_4_MONSTER_MAKING_MACHINE~MAKE_MONSTER .
protected section.
private section.
ENDCLASS.



CLASS ZCL_4_MONSTER_MAKING_MACHINE IMPLEMENTATION.


  METHOD zif_4_monster_making_machine~make_monster ##NEEDED.

  ENDMETHOD."#EC EMPTY_PROCEDURE
ENDCLASS.
