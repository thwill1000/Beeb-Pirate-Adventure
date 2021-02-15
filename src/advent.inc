' Read adventure data files in ScottFree format.
' For Colour Maximite 2, MMBasic 5.06
' Copyright (c) 2020-2021 Thomas Hugo Williams
' Developed with the assistance of Bill McKinley
' Based on original TRS-80 Level II BASIC code (c) 1978 Scott Adams

On Error Skip 1 : Dim sys.VERSION$ = ""
If sys.VERSION$ = "" Then Error "'system.inc' not included"
sys.requires("file", "string")
sys.provides("advent")
If sys.err$ <> "" Then Error sys.err$

Const advent.DIR$ = fil.PROG_DIR$ + "/../adventures"
Const advent.FD% = 1

Dim advent.file$

Dim advent.txt_sz% ' Minimum number of bytes required to contain all text (UNUSED)
Dim il ' Highest numbered object 0..il
Dim cl ' Highest action number 0..cl
Dim nl ' Highest vocabulary number 0..nl
Dim rl ' Highest room number 0..rl
Dim mx ' Maximum number of objects carried
Dim ar ' Starting room
Dim tt ' Number of treasures
Dim ln ' Word length
Dim lt ' Time limit
Dim ml ' Highest message number
Dim tr ' Treasure room

Dim advent.buf$
Dim advent.buf_ptr%

Function advent.find$(f$)
  f$ = str.unquote$(str.trim$(f$))
  If LCase$(fil.get_extension$(f$)) <> ".dat" Then Cat f$, ".dat"

  ' Is f$ just a simple absolute or relative path to the file ?
  If fil.exists%(f$) Then
    ' Do nothing.

  ' Is f$ a path relative to advent.DIR$ ?
  ElseIf fil.exists%(advent.DIR$ + "/" + f$) Then
    f$ = advent.DIR$ +  "/" + f$

  ' Is f$ a path relative to a sub-directory of advent.DIR$ ?
  Else
    Local num_dirs% = fil.count_files%(advent.DIR$, "*", "Dir")
    Local all_dirs$(num_dirs% - 1) Length 128
    fil.get_files(advent.DIR$, "*", "Dir", all_dirs$())
    Local i%
    For i% = 0 To num_dirs% - 1
      If fil.exists%(advent.DIR$ + "/" + all_dirs$(i%) + "/" + f$) Then
        f$ = advent.DIR$ + "/" + all_dirs$(i%) + "/" + f$
        Exit For
      EndIf
    Next
  EndIf

  If Not fil.exists%(f$) Then Error "File not found: " + f$

  advent.find$ = f$
End Function

Sub advent.get_all(m$())
  Local dirs$(list.new%(10)) Length 64
  list.init(dirs$())
  list.add(dirs$(), ".")
  Local f$ = Dir$(advent.DIR$ + "/*", Dir)
  Do While (f$ <> "") And (list.size%(dirs$()) <= list.capacity%(dirs$()))
    ' TODO: Add list.is_full%()
    ' What does list do if you add when full ?
    If list.size%(dirs$()) = list.capacity%(dirs$()) Then Exit Do
    list.add(dirs$(), f$)
    f$ = Dir$()
  Loop

  Local i%, lb% = 0, ub% = list.size%(dirs$()) - 1
  Local name$
  Local d$
  For i% = lb% To ub%
    d$ = advent.DIR$ + "/" + dirs$(i%) + "/"
    f$ = Dir$(d$ + "*.dat")
    Do While f$ <> ""
      ' TODO: Add map.is_full%()
      If map.size%(m$()) = map.capacity%(m$()) Then Exit Do
      name$ = fil.trim_extension$(f$)
      If fil.exists%(d$ + name$ + ".ext") Then
        Open d$ + name$ + ".ext" For Input As #1
        Line Input #1, name$
        Close #1
      Else
        name$ = UCase$(Left$(name$, 1)) + Mid$(name$, 2)
      EndIf
      map.put(m$(), name$, f$)
      f$ = Dir$()
    Loop
  Next
End Sub

Sub advent.read_description(f$, out%())
  If Not fil.exists%(f$) Then
    LongString Append out%(), "No description provided."
    Exit Sub
  EndIf

  Local max_sz% = (Bound(out%(), 1) - Bound(out%(), 0) + 1) * 8
  Local s$
  Open f$ For Input As #1
  Do
    s$ = Input$(255, #1)
    LongString Append out%(), s$
  Loop While Not Eof(#1)
  Close #1
End Sub

Sub advent.read(f$)
  advent.file$ = f$
  Open advent.file$ For Input As advent.FD%
  advent.buf$ = ""
  advent.buf_ptr% = 1
  advent.read_header()
  advent.read_action_table()
  advent.read_vocab_table()
  advent.read_rooms()
  advent.read_messages()
  advent.read_objects()
  Close #1
End Sub

Sub advent.read_header()
  advent.txt_sz% = advent.read_int%()
  il = advent.read_int%()
  cl = advent.read_int%()
  nl = advent.read_int%()
  rl = advent.read_int%()
  mx = advent.read_int%()
  ar = advent.read_int%()
  tt = advent.read_int%()
  ln = advent.read_int%()
  lt = advent.read_int%()
  ml = advent.read_int%()
  tr = advent.read_int%()

  Dim ca(cl, 7)      ' action table
  Dim nv_str$(nl, 1) ' vocabulary table - verbs at index 0, nouns at index 1
  Dim ia_str$(il)    ' object descriptions
  Dim ia(il)         ' object locations
  Dim i2(il)         ' backup copy of object locations
  Dim rs$(rl)        ' room descriptions
  Dim rm(rl, 5)      ' room exits: N, S, E, W, U, D
  Dim ms$(ml)        ' messages table
End Sub

Function advent.read_int%()
  Local ch%, s$

  ' Look for opening minus-sign or digit.
  Do
    ch% = advent.peek_char%()
    Select Case ch%
      Case 10, 13, 32
        Inc advent.buf_ptr%
      Case 45 ' minus sign
        s$ = "-"
        Inc advent.buf_ptr%
        Exit Do
      Case 48 To 57 ' digits
        Exit Do
      Case Else
        Error "looking for integer, found: " + Chr$(ch%)
    End Select
  Loop

  ' Accumulate digits.
  Do
    ch% = advent.peek_char%()
    Inc advent.buf_ptr%
    Select Case ch%
      Case 48 To 57 : Cat s$, Chr$(ch%)
      Case Else     : Exit Do
    End Select
  Loop

  advent.read_int% = Val(s$)
End Function

Function advent.peek_char%()
  If advent.buf_ptr% > Len(advent.buf$) Then
    advent.buf_ptr% = 1
    advent.buf$ = Input$(255, #advent.FD%)
  EndIf
  advent.peek_char% = Peek(Var advent.buf$, advent.buf_ptr%)
End Function

' Reads a quoted string that might be spread over several lines.
Function advent.read_string$()
  Local ch%, s$

  ' Look for opening quote.
  Do
    ch% = advent.peek_char%()
    Select Case ch%
      Case 10, 13, 32 ' line-feed, carriage-return and space
        Inc advent.buf_ptr%
      Case 34 ' double quotation mark
        Inc advent.buf_ptr%
        Exit Do
      Case Else
        Error "looking for opening-quote, found: " + Chr$(ch%)
    End Select
  Loop

  ' Accumulate string until find closing quote.
  Do
    ch% = advent.peek_char%()
    Inc advent.buf_ptr%
    Select Case ch%
      Case 10 ' \n - line feed
        Cat s$, sys.CRLF$
      Case 13 ' \r - carriage return
        If advent.peek_char%() <> 10 Then Cat s$, sys.CRLF$
      Case 34
        Exit Do
      Case Else
        Cat s$, Chr$(ch%)
    End Select
  Loop

  advent.read_string$ = s$
End Function

Sub advent.read_action_table()
  Local i, j
  For i = 0 To cl
    For j = 0 To 7
      ca(i, j) = advent.read_int%()
    Next j
  Next i
End Sub

Sub advent.read_vocab_table()
  Local i
  For i = 0 To nl
    nv_str$(i, 0) = advent.read_string$() ' verb
    nv_str$(i, 1) = advent.read_string$() ' noun
  Next
End Sub

Sub advent.read_rooms()
  Local i, j
  For i = 0 To rl
    For j = 0 To 5
      rm(i, j) = advent.read_int%()
    Next
    rs$(i) = advent.read_string$()
  Next
End Sub

Sub advent.read_messages()
  Local i
  For i = 0 To ml
    ms$(i) = advent.read_string$()
  Next
End Sub

Sub advent.read_objects()
  Local i
  For i = 0 To il
    ia_str$(i) = advent.read_string$()
    ia(i) = advent.read_int%()
    i2(i) = ia(i)
  Next
End Sub

Sub advent.clear()
  il = 0 : cl = 0 : nl = 0 : rl = 0 : mx = 0 : ar = 0
  tt = 0 : ln = 0 : lt = 0 : ml = 0 : tr = 0
  Erase ca, nv_str$, ia_str$, ia, i2, rs$, rm, ms$
End Sub
