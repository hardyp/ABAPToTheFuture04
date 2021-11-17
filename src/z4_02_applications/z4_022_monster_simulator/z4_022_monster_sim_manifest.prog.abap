*&---------------------------------------------------------------------*
*& Report Z4_022_MONSTER_SIM_MANIFEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z4_022_monster_sim_manifest.

DATA: go_logger     TYPE REF TO zcl_4_monster_logger ##NEEDED,
      go_machine    TYPE REF TO zcl_4_monster_making_machine ##NEEDED,
      go_simulator  TYPE REF TO zcl_4_monster_simulator ##NEEDED,
      go_sim_pl     TYPE REF TO zcl_4_monster_sim_pers_layer ##NEEDED,
      go_logger_if  TYPE REF TO zif_4_monster_logger ##NEEDED,
      go_machine_if TYPE REF TO zif_4_monster_making_machine ##NEEDED,
      go_sim_if     TYPE REF TO zif_4_monster_simulator ##NEEDED,
      go_sim_pl_if  TYPE REF TO zif_4_monster_sim_pers_layer ##NEEDED.

START-OF-SELECTION.
