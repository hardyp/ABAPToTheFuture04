*&---------------------------------------------------------------------*
*& Report Z4_015_DELIVERY_MANIFEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z4_015_delivery_manifest.

* The Delivery is PULLED from a CDS View to create a BOPF Oject + SEGW Service
DATA: go_draft_dodger     TYPE REF TO zcl_au_4cds_monster_deliveries ##NEEDED,
      go_derivation       TYPE REF TO zcl_d_4cds_monster_deliveries0 ##NEEDED,
      go_delivery_dpc     TYPE REF TO zcl_z_4_monster_delive_dpc ##NEEDED,
      go_delivery_dpc_ext TYPE REF TO zcl_z_4_monster_delive_dpc_ext ##NEEDED,
      go_delivery_mpc     TYPE REF TO zcl_z_4_monster_delive_mpc ##NEEDED,
      go_delivery_mpc_ext TYPE REF TO zcl_z_4_monster_delive_mpc_ext ##NEEDED,
      go_constants        TYPE REF TO zif_4cds_monster_deliveries_c ##NEEDED.

START-OF-SELECTION.
