*&---------------------------------------------------------------------*
*& Report Z_STRING
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z_STRING1.
data a TYPE p DECIMALS 2.
data b TYPE c LENGTH 3 VALUE 0.
data c TYPE p DECIMALS 2.
data message TYPE string.


MESSAGE = '0000000000000000000000000000000000000000000000000000000000000000000000000000000000'.

a = STRLEN( MESSAGE )  / 50 .
        a = floor( a ).
        DO a TIMES.
        MESSAGE S398(00) WITH  message+b(50) '/'.
        WRITE message+b(50).
        b = b + 50.
        ENDDO.
        c = STRLEN( MESSAGE )  mod 50 .
        MESSAGE S398(00) WITH  message+b(C) '/'.
        WRITE message+b(C).
