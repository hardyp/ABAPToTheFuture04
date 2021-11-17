*&---------------------------------------------------------------------*
*& Report Z4_002_ABAP2EXECL_MANIFEST
*&---------------------------------------------------------------------*
* List of Classes in Z4_002_ABAP2EXCEL_EXAMPLES Package
*&---------------------------------------------------------------------*
REPORT z4_002_abap2execl_manifest.

* Class List
DATA: go_emailer   TYPE REF TO zcl_excel_emailer ##NEEDED,
      go_extender  TYPE REF TO zcl_excel_extender ##NEEDED,
      go_converter TYPE REF TO zcl_excel_fieldcat_converter ##NEEDED.

START-OF-SELECTION.
