*&---------------------------------------------------------------------*
*& Report ZGETUS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZGETUS1.
 DATA : ls_ac_container      type swcont,
         lt_ac_container      TYPE TABLE OF swcont,
         lt_approver          TYPE TABLE OF swhactor.
    clear ls_ac_container.
    MOVE :   'OBJID'  TO ls_ac_container-element ,
             '012'         TO ls_ac_container-elemlength ,
             'C'           TO ls_ac_container-type ,
             '00000057'   TO ls_ac_container-value .
    APPEND ls_ac_container to lt_ac_container.
    MOVE :   'OTYPE'  TO ls_ac_container-element ,
            '002'         TO ls_ac_container-elemlength ,
             'C'           TO ls_ac_container-type ,
             'P'   TO ls_ac_container-value .
    APPEND ls_ac_container to lt_ac_container.
    REFRESH lt_approver.

    CALL FUNCTION 'SWX_GET_MANAGER'
      TABLES
        actor_tab    = lt_approver
        ac_container = lt_ac_container
      EXCEPTIONS
        nobody_found = 1
        OTHERS       = 2.


BREAK-POINT.
