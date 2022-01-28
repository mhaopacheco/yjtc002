*=======================================================================
* PROGRAMA REPORTE # YJTC002
* TRANSACCIÓN # YJTC002
*----------------------------------------------------------------------*
* VERSION # S/4 Hana On Premise 1909
* FECHA CREACION # 09.05.2021 00:16:50
* MODULO # Cross Application
* TIPO # Report
* CONSULTOR ABAP # Mauricio Pacheco
* CONSULTOR FUNCIONAL # N/A
*-----------------------------------------------------------------------
* PROPOSITO #
* Generar un transporte de copias y lo libera a partir de una orden de transporte principal
*-----------------------------------------------------------------------
* MODIFICACIONES
*-----------------------------------------------------------------------
* VERS FECHA ORDEN DESCRIPCION DEL CAMBIO USUARIO
*=======================================================================

REPORT yjtc002.

TABLES: tmsbuftxt, e070___preview, ccprofsr.

TYPES: BEGIN OF ty_e070,
         trkorr  TYPE e070-trkorr,
         strkorr TYPE e070-strkorr,
       END OF ty_e070.

TYPES: BEGIN OF ty_log.
TYPES: light.
INCLUDE STRUCTURE bapireturn1.
TYPES: END OF ty_log.

DATA it_e070   TYPE STANDARD TABLE OF ty_e070.
DATA it_log    TYPE STANDARD TABLE OF ty_log.

DATA wa_log    TYPE ty_log.

DATA ld_req_id TYPE trkorr.

FIELD-SYMBOLS: <fs_e070> LIKE LINE OF it_e070.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001. .
PARAMETERS: p_trkorr LIKE e070-trkorr               OBLIGATORY MODIF ID dst.
PARAMETERS: p_name   LIKE tmsbuftxt-text.
PARAMETERS: p_type   LIKE e070___preview-trfunction OBLIGATORY MODIF ID typ DEFAULT 'T'.
PARAMETERS: p_targ   LIKE ccprofsr-target_sys       OBLIGATORY MODIF ID dst DEFAULT TEXT-003 .
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002 .
PARAMETERS: p_import TYPE xfeld,
            p_client TYPE mandt DEFAULT TEXT-004.
SELECTION-SCREEN: END OF BLOCK b2.

*INCLUDE zcreate_transport_request_top.
*INCLUDE y_changerequest_create_f01.
*INCLUDE zcreate_transport_request_f01.

*&---------------------------------------------------------------------
*&  Include           ZCREATE_TRANSPORT_REQUEST_F01
*&---------------------------------------------------------------------
FORM chk_tr_type.

  IF p_type NE 'K' AND p_type NE 'T' AND p_type NE 'W'.
    MESSAGE e208(00) WITH TEXT-t01.
  ENDIF.

ENDFORM.

FORM create_tr .

  DATA wa_req_header TYPE trexreqhd.
  DATA ld_msg        TYPE tr004-msgtext.
  DATA ld_exception  TYPE tr007-exception.

  CLEAR ld_req_id.

  CALL FUNCTION 'TR_EXT_CREATE_REQUEST'
    EXPORTING
      iv_request_type = p_type
      iv_target       = p_targ
      iv_author       = sy-uname
      iv_text         = p_name
    IMPORTING
      es_req_id       = ld_req_id
      es_req_header   = wa_req_header
      es_msg          = ld_msg
      ev_exception    = ld_exception
    EXCEPTIONS
      OTHERS          = 1.

  IF sy-subrc EQ 0.
    wa_log-light = '3'.
    CONCATENATE TEXT-t04 ld_req_id INTO wa_log-message SEPARATED BY space.
    APPEND wa_log TO it_log[].
    CLEAR wa_log.
  ELSE.
    wa_log-light = '1'.
    wa_log-message = TEXT-t05.
    APPEND wa_log TO it_log[].
    CLEAR wa_log.
  ENDIF.

ENDFORM.
FORM add_objects .

  DATA ld_from TYPE e070-trkorr.
  DATA ld_to   TYPE e070-trkorr.

  SELECT trkorr strkorr
    INTO TABLE it_e070
    FROM e070
    WHERE strkorr EQ p_trkorr.

  ld_from = p_trkorr.
  ld_to   = ld_req_id.

  CALL FUNCTION 'TR_COPY_COMM'
    EXPORTING
      wi_dialog                = abap_false
      wi_trkorr_from           = ld_from
      wi_trkorr_to             = ld_to
      wi_without_documentation = space
    EXCEPTIONS
      db_access_error          = 1
      trkorr_from_not_exist    = 2
      trkorr_to_is_repair      = 3
      trkorr_to_locked         = 4
      trkorr_to_not_exist      = 5
      trkorr_to_released       = 6
      user_not_owner           = 7
      no_authorization         = 8
      wrong_client             = 9
      wrong_category           = 10
      object_not_patchable     = 11
      message                  = 12
      OTHERS                   = 13.

  IF sy-subrc NE 0.
    wa_log-light = '1'.
    wa_log-message = TEXT-t06.
    wa_log-message_v1 = ld_from.
    wa_log-message_v2 = ld_to.
    APPEND wa_log TO it_log[].
    CLEAR wa_log.
  ENDIF.

  IF it_e070[] IS NOT INITIAL.

    LOOP AT it_e070 ASSIGNING <fs_e070>.

      ld_from = <fs_e070>-trkorr.
      ld_to   = ld_req_id.

      CALL FUNCTION 'TR_COPY_COMM'
        EXPORTING
          wi_dialog                = abap_false
          wi_trkorr_from           = ld_from
          wi_trkorr_to             = ld_to
          wi_without_documentation = space
        EXCEPTIONS
          db_access_error          = 1
          trkorr_from_not_exist    = 2
          trkorr_to_is_repair      = 3
          trkorr_to_locked         = 4
          trkorr_to_not_exist      = 5
          trkorr_to_released       = 6
          user_not_owner           = 7
          no_authorization         = 8
          wrong_client             = 9
          wrong_category           = 10
          object_not_patchable     = 11
          message                  = 12
          OTHERS                   = 13.

      IF sy-subrc <> 0.
        wa_log-light = '1'.
        wa_log-message = TEXT-t06.
        wa_log-message_v1 = ld_from.
        wa_log-message_v2 = ld_to.
        APPEND wa_log TO it_log[].
        CLEAR wa_log.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.
FORM release_tr .

  DATA wa_request       TYPE trwbo_request.
  DATA it_deleted_tasks TYPE trwbo_t_e070.

  CALL FUNCTION 'TR_RELEASE_REQUEST'
    EXPORTING
      iv_trkorr                  = ld_req_id
      iv_dialog                  = 'X'
      iv_as_background_job       = ' '
      iv_success_message         = 'X'
      iv_display_export_log      = 'X'
      iv_simulation              = ' '
*     iv_without_locking         = ' '
    IMPORTING
      es_request                 = wa_request
      et_deleted_tasks           = it_deleted_tasks
    EXCEPTIONS
      cts_initialization_failure = 1
      enqueue_failed             = 2
      no_authorization           = 3
      invalid_request            = 4
      request_already_released   = 5
      repeat_too_early           = 6
      error_in_export_methods    = 7
      object_check_error         = 8
      docu_missing               = 9
      db_access_error            = 10
      action_aborted_by_user     = 11
      export_failed              = 12
      OTHERS                     = 13.

  IF sy-subrc <> 0.
    wa_log-light = '1'.
    wa_log-message = TEXT-t07.
    wa_log-message_v1 = ld_req_id.
    APPEND wa_log TO it_log[].
    CLEAR wa_log.
  ENDIF.

ENDFORM.

FORM get_tr_to .

  DATA wa_request TYPE trwbo_request_header.

  CALL FUNCTION 'TR_REQUEST_CHOICE'
    IMPORTING
      es_request           = wa_request
    EXCEPTIONS
      invalid_request      = 1
      invalid_request_type = 2
      user_not_owner       = 3
      no_objects_appended  = 4
      enqueue_error        = 5
      cancelled_by_user    = 6
      recursive_call       = 7
      OTHERS               = 8.

  IF sy-subrc <> 0.
    wa_log-light = '1'.
    wa_log-message = TEXT-t03.
    APPEND wa_log TO it_log[].
    CLEAR wa_log.
  ELSE.
    p_trkorr = wa_request-trkorr.
  ENDIF.

  BREAK-POINT .

  CHECK p_trkorr IS NOT INITIAL .
  SELECT SINGLE as4text INTO p_name FROM e07t WHERE trkorr = p_trkorr AND langu = sy-langu .

ENDFORM.
*&---------------------------------------------------------------------
*&      Form  SHOW_LOG
*&---------------------------------------------------------------------
*       text
*----------------------------------------------------------------------
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------
FORM show_log .

  DATA go_alv          TYPE REF TO cl_salv_table.
  DATA gr_display      TYPE REF TO cl_salv_display_settings.
  DATA lr_functions    TYPE REF TO cl_salv_functions_list.
  DATA lr_columns      TYPE REF TO cl_salv_columns_table.
  DATA lr_column       TYPE REF TO cl_salv_column_table.
*  DATA lr_function     TYPE REF TO cl_salv_functions.
  DATA ls_color        TYPE lvc_s_colo.
  DATA ls_aggregations TYPE REF TO cl_salv_aggregations.


  TRY.
      cl_salv_table=>factory(
      IMPORTING
        r_salv_table = go_alv
      CHANGING
        t_table      = it_log[] ).

    CATCH cx_salv_msg.
  ENDTRY.

  lr_functions = go_alv->get_functions( ).
  lr_functions->set_all( abap_true ).
  lr_functions->set_default( abap_true ).
  lr_functions->set_all( if_salv_c_bool_sap=>true ).

  gr_display = go_alv->get_display_settings( ).
  gr_display->set_striped_pattern( abap_true ).

  lr_columns = go_alv->get_columns( ).
  lr_columns->set_optimize( abap_true ).

  ls_aggregations = go_alv->get_aggregations( ).

  lr_columns->set_exception_column( value = 'LIGHT' ).

  TRY.
      lr_column ?= lr_columns->get_column( 'LIGHT' ).
      lr_column->set_medium_text( 'STATUS' ).
      lr_column->set_long_text( 'STATUS' ).
      lr_column->set_short_text( 'STATUS' ).
    CATCH cx_salv_not_found.
  ENDTRY.

  IF go_alv IS BOUND.
    go_alv->display( ).
  ENDIF.

ENDFORM.

**********************************************************************

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF p_type IS INITIAL OR p_type NE 'T'.
      IF screen-group1 EQ 'DST'.
        screen-input = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN.
  PERFORM chk_tr_type.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_trkorr.
  PERFORM get_tr_to.


START-OF-SELECTION.

  PERFORM create_tr.
  IF ld_req_id IS NOT INITIAL AND p_type = 'T'.
    PERFORM add_objects.
    PERFORM release_tr.
*   Import order in target
    IF p_import IS NOT INITIAL .
      PERFORM import_tr USING ld_req_id .
    ENDIF.

    PERFORM display_tr USING ld_req_id .
  ELSE.
    PERFORM show_log.
  ENDIF.

*&---------------------------------------------------------------------*
*& Form import_tr
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM import_tr USING uv_tr TYPE trkorr .

  DATA: lv_retcode   TYPE stpa-retcode,
        lv_alog      TYPE stpa-file,
        lv_slog      TYPE stpa-file,
        lv_pid       TYPE stpa-pid,
        lv_tpstat    TYPE tmstpkey,
        lv_exception TYPE stmscalert,
        lv_imports   TYPE stms_tp_imports.

  DATA: lt_logtpr TYPE TABLE OF tplogptr,
        lt_stdout TYPE TABLE OF tpstdout.

  CALL FUNCTION 'TMS_MGR_IMPORT_TR_REQUEST'
    EXPORTING
      iv_system                  = p_targ
*     iv_domain                  =
      iv_request                 = uv_tr
      iv_client                  = p_client
*     iv_ctc_active              =
      iv_overtake                = abap_true
      iv_import_again            = abap_true
*     iv_ignore_originality      =
*     iv_ignore_repairs          =
*     iv_ignore_transtype        =
*     iv_ignore_tabletype        =
*     iv_ignore_qaflag           =
*     iv_ignore_predec           =
      iv_ignore_cvers            = abap_true
*     iv_ignore_spam             =
*     iv_test_import             =
*     iv_cmd_import              =
*     iv_no_delivery             =
*     iv_subset                  =
*     iv_offline                 =
*     iv_feedback                =
      iv_monitor                 = 'X'
*     iv_force                   =
      iv_verbose                 = ' '
*     is_batch                   =
*     it_requests                =
*     it_clients                 =
    IMPORTING
      ev_tp_ret_code             = lv_retcode
      ev_tp_alog                 = lv_alog
      ev_tp_slog                 = lv_slog
      ev_tp_pid                  = lv_pid
      ev_tpstat_key              = lv_tpstat
      es_exception               = lv_exception
      et_tp_imports              = lv_imports
    TABLES
      tt_logptr                  = lt_logtpr
      tt_stdout                  = lt_stdout
    EXCEPTIONS
      read_config_failed         = 1
      table_of_requests_is_empty = 2
      OTHERS                     = 3.


  DATA(ls_syst) = CORRESPONDING syst( lv_exception ) .

  IF ls_syst-msgty = 'E'.
    MESSAGE ID ls_syst-msgid TYPE ls_syst-msgty NUMBER ls_syst-msgno
      WITH ls_syst-msgv1 ls_syst-msgv2 ls_syst-msgv3 ls_syst-msgv4.
  ELSE.
    MESSAGE 'Import Correcto' TYPE 'S' DISPLAY LIKE 'I' .
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form display_tr
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_tr USING uv_tr TYPE trkorr .


  CALL FUNCTION 'TRINT_TDR_USER_COMMAND'
    EXPORTING
      iv_object  = uv_tr
      iv_type    = 'T'
      iv_command = 'TAST'
*    IMPORTING
*     ev_exit    =
    .

ENDFORM.
