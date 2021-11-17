*&---------------------------------------------------------------------*
*& Report Z4_014_ORDER_MAINFEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z4_014_order_mainfest.

* These classes are all automatically generated as a CDS View gets turned into a BOPF Object
DATA: go_draft_dodger          TYPE REF TO zcl_au_4i_monster_so_header ##NEEDED,
      go_header_deteremination TYPE REF TO zcl_d_4i_monster_so_header_act ##NEEDED,
      go_item_determination    TYPE REF TO zcl_d_4i_monster_so_items_acti ##NEEDED,
      go_saddle_bag            TYPE REF TO zcl_z4i_monster_so_header ##NEEDED,
      go_constants_ineterface  TYPE REF TO zif_4i_monster_so_header_c ##NEEDED.

START-OF-SELECTION.
