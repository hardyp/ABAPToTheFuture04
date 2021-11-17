*&---------------------------------------------------------------------*
*& Report Z4_002_ALV_FRAMEWORK_MANIFEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z4_002_alv_framework_manifest.

DATA: go_event_handler TYPE REF TO zcl_bc_salv_event_handler ##NEEDED,
      go_view          TYPE REF TO zcl_bc_view_salv_table ##NEEDED,
      go_model         TYPE REF TO zcl_salv_model ##NEEDED.

START-OF-SELECTION.
