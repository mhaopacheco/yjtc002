*&---------------------------------------------------------------------*
*& Include          ZWF_MASS_EVENTS_F01
*&---------------------------------------------------------------------*

FORM get_filename  USING pfile.

  DATA: lst_file TYPE file_table OCCURS 0,
        rc       TYPE i,
        txt_g00  TYPE string.

  txt_g00 = TEXT-g00.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = txt_g00
      default_extension       = '.'
    CHANGING
      file_table              = lst_file
      rc                      = rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      OTHERS                  = 5.

  CHECK sy-subrc = 0.
  READ TABLE lst_file INDEX 1 INTO p_file .

ENDFORM.

FORM load_file.

  DATA name_file                TYPE string.
  name_file = p_file .

  CALL METHOD cl_gui_frontend_services=>gui_upload
    EXPORTING
      filename            = name_file
      filetype            = 'ASC'
      has_field_separator = 'X'
    CHANGING
      data_tab            = gt_data[]
    EXCEPTIONS
      file_open_error     = 1
      file_read_error     = 2
      bad_data_format     = 3
      OTHERS              = 5.

  IF sy-subrc <> 0.
    CASE sy-subrc.
      WHEN : '1'.
        WRITE :/ 'Error al abrir el archivo:', p_file.
      WHEN : '2'.
        WRITE :/ 'Error al leer el archivo:', p_file.
      WHEN : '3'.
        WRITE :/ 'Error de formato en el archivo:', p_file.
      WHEN OTHERS.
        MESSAGE e301(bd) WITH p_file.
    ENDCASE.
  ENDIF.
ENDFORM.

FORM execute_load .

  "MPACHECO: 09.11.2017 09:12:20: Aqui va pegar el codigo que se genera
  "de la grabación, aqui se crean los paquetes a enviar al bdc o la bapi

  DATA: lt_return   TYPE STANDARD TABLE OF bapiret2 .

  LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<ls_data>).
    CASE p_type .
      WHEN cl_swf_evt_event=>if_swf_evt_event~co_objcateg_cl .
        PERFORM call_magic TABLES lt_return[] CHANGING <ls_data> .
      WHEN cl_swf_evt_event=>if_swf_evt_event~co_objcateg_bor .
        PERFORM call_bo_event TABLES lt_return[] CHANGING <ls_data> .
      WHEN OTHERS.
    ENDCASE.

  ENDLOOP.

  DATA(ld_message) = 'Se enviaron ' && gv_cont && ' Eventos' .
  MESSAGE ld_message TYPE 'I' .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_MAGIC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_RETURN[]
*&      <-- <LS_DATA>
*&---------------------------------------------------------------------*
FORM call_magic  TABLES  t_return STRUCTURE bapiret2
                 CHANGING cs_data STRUCTURE gs_data.

  DATA : lv_objtype TYPE sibftypeid,
         lv_event   TYPE sibfevent,
         lv_objkey  TYPE sibfinstid.

  TRY .
      lv_objtype  = p_object .
      lv_event    = p_event .
      lv_objkey   = cs_data-objid .

      CALL METHOD cl_swf_evt_event=>raise
        EXPORTING
          im_objcateg = p_type
          im_objtype  = lv_objtype
          im_event    = lv_event
          im_objkey   = lv_objkey.
*          im_event_container = io_container.

      ADD 1 TO gv_cont .

    CATCH cx_swf_evt_exception .

  ENDTRY .

  COMMIT WORK .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_BO_EVENT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_RETURN[]
*&      <-- <LS_DATA>
*&---------------------------------------------------------------------*
FORM call_bo_event TABLES t_return STRUCTURE bapiret2
                 CHANGING cs_data STRUCTURE gs_data.

  DATA: ls_swr    TYPE swr_struct ,
        lv_subrc  TYPE sy-subrc .

  ls_swr-object_typ  = p_object .
  ls_swr-object_key  = cs_data-objid .
  ls_swr-event       = p_event .
  ls_swr-commitflag = 'X' .

  CALL FUNCTION 'SAP_WAPI_CREATE_EVENT'
  EXPORTING
    object_type       = ls_swr-object_typ
    object_key        = ls_swr-object_key
    event             = ls_swr-event
    commit_work       = ls_swr-commitflag
    event_language    = sy-langu
    language          = sy-langu
    user              = sy-uname
*     IFS_XML_CONTAINER =
  IMPORTING
    return_code       = lv_subrc .
*    event_id          = ed_return_event
*  TABLES
*    input_container   = lit_container
*     MESSAGE_LINES     =
*     MESSAGE_STRUCT    =

  IF lv_subrc = 0 .
    ADD 1 TO gv_cont .
  ENDIF.

ENDFORM.
