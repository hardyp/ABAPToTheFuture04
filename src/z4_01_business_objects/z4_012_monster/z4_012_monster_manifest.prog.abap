*&---------------------------------------------------------------------*
*& Report Z4_012_MONSTER_MANIFEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z4_012_monster_manifest.

"Base Monster Classes
DATA: go_model_intf TYPE REF TO zif_4_monster_model ##NEEDED,
      go_model      TYPE REF TO zcl_4_monster_model ##NEEDED,
      go_model_pli  TYPE REF TO zif_4_monstermodel_pers_layer ##NEEDED,
      go_factory    TYPE REF TO zcl_4_monster_factory ##NEEDED,
      go_injector   TYPE REF TO zcl_4_monster_injector ##NEEDED.

"Virtual CDS Element
DATA: go_jupiter TYPE REF TO zcl_4_monster_bunny_jupiter ##NEEDED.

"BOPF Classes
DATA: go_bopf_helper    TYPE REF TO zcl_4_bc_bopf_pl_helper ##NEEDED,
      go_bopf_pl        TYPE REF TO zcl_4_monstermodel_pers_bopf ##NEEDED,
      go_consts_intf    TYPE REF TO zif_4_monster_c ##NEEDED,
      go_bopf_query     TYPE REF TO zcl_4_village_monster_query ##NEEDED,
      go_action_explode TYPE REF TO zcl_4_a_explode_all_heads ##NEEDED,
      go_action_howl    TYPE REF TO zcl_4_a_howl_at_the_moon ##NEEDED,
      go_action_check   TYPE REF TO zcl_4_a_v_check_howling_status ##NEEDED,
      go_derive_texts   TYPE REF TO zcl_4_d_monsterheader_texts ##NEEDED,
      go_validation     TYPE REF TO zcl_4_v_monsterheader_con_chk ##NEEDED.

"SEGW Classes
DATA: go_dpc     TYPE REF TO zcl_z_4_monster_dpc ##NEEDED,
      go_dpc_ext TYPE REF TO zcl_z_4_monster_dpc_ext ##NEEDED,
      go_mpc     TYPE REF TO zcl_z_4_monster_mpc ##NEEDED,
      go_mpc_ext TYPE REF TO zcl_z_4_monster_mpc_ext ##NEEDED.

"Web Dynpro Classes
DATA: go_fpm_search     TYPE REF TO zcl_4_fpm_monstersearch_feeder ##NEEDED,
      go_wda_unit_tests TYPE REF TO zcl_4_wda_monster_test_class ##NEEDED,
      go_wda_cc_intf    TYPE REF TO ziwci_wdc_4_monster_list ##NEEDED.

START-OF-SELECTION.
