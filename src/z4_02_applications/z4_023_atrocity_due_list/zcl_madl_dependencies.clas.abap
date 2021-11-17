class ZCL_MADL_DEPENDENCIES definition
  public
  final
  create public .

public section.

  interfaces ZIF_APACK_MANIFEST .

  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_MADL_DEPENDENCIES IMPLEMENTATION.


  METHOD constructor.

    zif_apack_manifest~descriptor = VALUE #(
    group_id        = 'hardyp'
    artifact_id     = 'ABAPToTheFuture04'
    version         = '0.1'
    repository_type = zif_apack_manifest~co_abap_git
    git_url         = 'https://github.com/hardyp/ABAPToTheFuture04'
    dependencies    = VALUE #(
      ( group_id    = 'sapmentors'
        artifact_id = 'abap2xlsx'
        git_url     = 'https://github.com/sapmentors/abap2xlsx' ) ) ).

  ENDMETHOD.
ENDCLASS.
