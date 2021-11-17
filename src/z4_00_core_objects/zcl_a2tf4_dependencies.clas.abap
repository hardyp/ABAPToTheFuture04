class ZCL_A2TF4_DEPENDENCIES definition
  public
  final
  create public .

public section.

  interfaces ZIF_APACK_MANIFEST .

  methods CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_A2TF4_DEPENDENCIES IMPLEMENTATION.


  METHOD constructor.
*---------------------------------------------------------------------------------*
* To run the code samples in the book you need assorted open source projects
* installed. A few were created by me, most by sensible people
* The URLS of the Github repositories are listed below
* When installing a pacakge via abapGit all the dependencies that are bot already
* in your system are automatically installed
*---------------------------------------------------------------------------------*
    zif_apack_manifest~descriptor = VALUE #(
      group_id        = 'hardyp'
      artifact_id     = 'ABAPToTheFuture04'
      version         = '0.1'
      repository_type = zif_apack_manifest~co_abap_git
      git_url         = 'https://github.com/hardyp/ABAPToTheFuture04'
      dependencies    = VALUE #(
        ( group_id    = 'sapmentors'
          artifact_id = 'abap2xlsx'
          git_url     = 'https://github.com/sapmentors/abap2xlsx' )
        ( group_id    = 'AntonSikidin'
          artifact_id = 'zcl_docx'
          git_url     = 'https://github.com/AntonSikidin/zcl_docx' )
        ( group_id    = 'sbcgua'
          artifact_id = 'mockup_loader'
          git_url     = 'https://github.com/sbcgua/mockup_loader' )
        ( group_id    = 'hardyp'
          artifact_id = 'DesignByContract'
          git_url     = 'https://github.com/hardyp/DesignByContract' )
        ( group_id    = 'hardyp'
          artifact_id = 'FunctionModuleWrapper'
          git_url     = 'https://github.com/hardyp/FunctionModuleWrapper' )
        ( group_id    = 'hardyp'
          artifact_id = 'OCPFactory'
          git_url     = 'https://github.com/hardyp/OCPFactory' ) ) ).

  ENDMETHOD.
ENDCLASS.
