'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
'*  SD_Archivos.bas                                                        *
'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
'*                                                                             *
'*  Variables, Subrutinas y Funciones                                          *
'* WATCHING SOLUCIONES TECNOLOGICAS                                            *
'* 25.06.2015                                                                  *
'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

$nocompile

$projecttime = 93
'*******************************************************************************
'Declaracion de subrutinas
'*******************************************************************************
Declare Sub Inivar()
Declare Sub Procser()
Declare Sub Inimdm()

Declare Sub Atcmdsnd(byval Atcmd As String * 100 , Byval T0ratedat As Integer)
Declare Sub Espera(byval Tespera As Word)                   'Subrutina para delay
Declare Sub Vergsm()
Declare Sub Vergprs()
Declare Sub Smstx()
Declare Sub Procsms()
Declare Sub Leersms()
Declare Sub Txdata(byval Trama As Byte)                     ' Trama 0=Server WATCHING, 1=TS, 2=TS Talkback
Declare Sub Sendtrans()
Declare Sub Procdaa()
Declare Sub Genjsonts()
Declare Sub Defaultvalues()
Declare Sub Tstavion()
Declare Sub Leered()
Declare Sub Proced()

'RTC
Declare Sub Getdatetimeds3231()
Declare Sub Error(byval Genre As Byte)
Declare Sub Setdateds3231()
Declare Sub Settimeds3231()

Declare Sub Leeradc()
Declare Sub Apagarmdm()
Declare Sub Leertc()
Declare Sub Getip()


'*******************************************************************************
'Declaracion de variables
'*******************************************************************************
Dim Tmpb As Byte , Tmpb2 As Byte , Tmpb3 As Byte , K As Byte
Dim Tmpb4 As Byte
Dim Jed As Byte , J As Byte
Dim Tmpl As Long , Tmpl2 As Long , Tmplisr As Long , Lsyssec As Long
Dim Tmpw As Word
Dim Tmps As Single
Dim Enabug As Byte
Dim Resets As Bit
Dim Cntrerrtx As Byte
Dim Cntrerrtxant As Byte

Dim Cmdtmp As String * 6
Dim Atsnd As String * 200
Dim Cmderr As Byte
Dim Tmpstr8 As String * 16
Dim Tmpstr52 As String * 52
Dim Tmpstr3 As String * 3


Dim Apn As String * 32 , Apneep As Eram String * 32,        'APN


'Variables TIMER0
Dim T0c As Byte
Dim Num_ventana As Byte
Dim Estado As Long
Dim Estado_led As Byte
Dim Iluminar As Bit
Dim T0cntr As Word
Dim T0tout As Bit , T0ini As Bit
Dim T0rate As Word
Dim Inidelay As Bit
Dim Cntrdelay As Word
Dim T0delay As Bit
Dim Topdelay As Word
Dim T0cntrsms As Word , T0inisms As Bit , T0toutsms As Bit


'TIMER1
Dim Newseg As Bit
Dim T1wait As Byte
Dim T1cntr As Byte
Dim T1tout As Bit
Dim T1ini As Bit
Dim Iniauto As Byte
Dim T1tick As Byte
Dim T1tmp As Byte
Dim T1tmp0 As Byte
Dim Cntrtredmenos(ednum) As Word
Dim Cntrtredmas(ednum) As Word
Dim Evmenos As Byte                                         ' Almacena en un bit nuevo evento de mas a menos
Dim Evmas As Byte                                           ' Almacena en un bit nuevo evento de menos a mas
Dim Tmped As Byte

'timer 2
Dim Tmpt2 As Byte
Dim Tmpt21 As Byte
'dim tmplisr as long
Dim Tbl_timeaut(numtxaut) As Long
Dim Inipoll As Bit
Dim Inicpoll As Bit
Dim Cntrpoll As Byte
Dim Cntrpollant As Byte

'AT COMANDOS
Dim At_cntrlf As Byte
Dim Initx As Bit
'Dim Inicio As Bit
Dim Inicon As Bit
Dim Initxsms As Byte
Dim Inileer As Bit
Dim Inimodem As Bit
Dim Tmpsms As String * 20
Dim Mdmon As Bit
Dim Tf_error As Byte
Dim Mdmerror As Byte
Dim Mdmready As Bit
Dim At_respcreg As String * 20
Dim At_respcgreg As String * 20
Dim Gprsok As Bit
Dim Gsmok As Bit
Dim Cntracterr As Byte
Dim Newcreg As Bit
Dim Newcgreg As Bit
Dim Smstxsta As String * 6
Dim Gprstxsta As String * 6
Dim Fono As String * 32
Dim Newsms As Bit
Dim Tmpat As String * 16
Dim Iniapagarmdm As Bit
Dim Cntrerrts As Byte
Dim Mdmoff As Bit

'SMS
Dim Ls_str2 As String * 2
Dim Tf_num1 As String * 15                                  ', Tf_num As String * 10
Dim Fecha_rx As String * 17 , Msglong As Byte
Dim Smsok As Bit

'GPRS
Dim Newtx As Bit
Dim Atok As Bit
Dim Csttok As Bit
Dim Ciicrok As Bit
Dim Cifsrok As Bit
Dim Sendok As Bit
Dim Closeok As Bit
Dim Shutok As Bit
Dim Modoavion As Bit
Dim Modonormal As Bit
Dim Txok As Bit
Dim Fallagprs As Bit
Dim Respsrvok As Bit
DIM Cntrtx AS BYTE
Dim Cntrcgreg As Byte

'THINGSPEAK
Dim Keyapitmp As String * 64
Dim Habfeed(8) As Byte                                      'Arreglo para indicar habilitacion de feed en TS
Dim Valfeed(8) As String * 64                               ' Arreglo para almacenar valor de feed
Dim Fechaed As String * 10
Dim Horaed As String * 10
Dim Datamdc As String * 255
Dim Cntrdata As Byte
Dim Keyapi As String * 20
Dim Keyapieep As Eram String * 20


'OPERACION
Dim Cntrini As Word
Dim Cntrinieep As Eram Word
Dim Cntrinimdc As Word
Dim Cntrinimdceep As Eram Word
Dim Inivariables As Bit
Dim Iniavion As Bit
Dim Tmpbit As Bit

' Variables para ADC
Dim Tmpwadc As Word
Dim Adccntri As Single
Dim Cntrsmpli As Byte
Dim Adcvali As Single
Dim Adck As Single                                          'Constante de proporcionalidad ADC
Dim Adckeep As Eram Single
Dim Iniadc As Bit
Dim Samplerdy As Bit
Dim Horamin As Long
Dim Horamineep As Eram Long

'RTC
Dim Dow As Byte

'ED
'Variables ED
Dim Tredmas(ednum) As Word
Dim Tredmaseep(ednum) As Eram Word
Dim Tredmenos(ednum) As Word
Dim Tredmenoseep(ednum) As Eram Word
Dim Edpol As Byte
Dim Edpoleep As Eram Byte
Dim Edname(ednum) As String * 6
Dim Ednameeep(ednum) As Eram String *
Dim Eddat As Byte
Dim Edsta(ednum) As Byte
Dim Edstaant(ednum) As Byte
'Textos reportes ED
Dim Txtrepmas(ednum) As String * 64
Dim Txtrepmaseep(ednum) As Eram String * 64
Dim Txtrepmenos(ednum) As String * 64
Dim Txtrepmenoseep(ednum) As Eram String * 64
Dim Edhab As Byte
Dim Edhabeep As Eram Byte


'Variables para transmisiones automáticas
Dim Autoval(numtxaut) As Long , Autovaleep(numtxaut) As Eram Long
Dim Offset(numtxaut) As Long , Offseteep(numtxaut) As Eram Long
'Dim Iniauto As Word

'Variables SERIAL1
Dim Ser_ini As Bit , Sernew As Bit
Dim Numpar As Byte
Dim Cmdsplit(16) As String * 64
Dim Iptx As String * 64
Dim Iptxeep As Eram String * 64
Dim Prtotx As String * 64
Dim Prtotxeep As Eram String * 64

Dim Gws_ini As Bit , Gwsnew As Bit
Dim Gwsdata As String * 255 , Gwsproc As String * 255
Dim Mdcbuf(numbuf) As String * 200
Dim Txbuf(numbuf) As Byte
Dim Ptrbuf As Byte
Dim Ptrtx As Byte

DIM Cntrsms AS BYTE
Dim Cntrsmseep As Eram Byte

Dim Serdata As String * 200 , Serrx As Byte , Serproc As String * 200

'SERIAL 2
Dim Newrd As Bit
Dim Ptrrd As Word
Dim Udrt As Byte
Dim At_data1 As String * 255
Dim At_loop As String * 255
Dim At_resp As String * 255
Dim At_data As String * 255
Dim At_serial As String * 255
Dim Bufrd(numrd) As Byte
Dim Bufferrx As String * Numrd At Bufrd(1) Overlay



'*******************************************************************************
'* END public part                                                             *
'*******************************************************************************


Goto Loaded_arch

'*******************************************************************************
' INTERRUPCIONES
'*******************************************************************************

'*******************************************************************************
' Subrutina interrupcion de puerto serial 1
'*******************************************************************************
At_ser1:
   Serrx = Udr
   Select Case Serrx
      Case "%":
         Ser_ini = 1
         Serdata = ""
      Case "$":
         Gws_ini = 1
         Gwsdata = ""
      Case 13:
         If Ser_ini = 1 Then
            Ser_ini = 0
            Serdata = Serdata + Chr(0)
            Serproc = Serdata
            Sernew = 1
            Enable Timer0
         End If
         If Gws_ini = 1 Then
            Gws_ini = 0
            Gwsdata = Gwsdata + Chr(0)
            'Gwsproc = Gwsdata
            Ptrbuf = Ptrbuf Mod Numbuf
            Incr Ptrbuf
            Mdcbuf(ptrbuf) = Gwsdata
            'If Gwsdata = "OK" Then Set Dataok
            Txbuf(ptrbuf) = 1
            Gwsdata = ""
            Gwsnew = 1
         End If
      Case Is > 31
         If Ser_ini = 1 Then
            Serdata = Serdata + Chr(serrx)
         End If
         If Gws_ini = 1 Then
            Gwsdata = Gwsdata + Chr(serrx)
         End If
   End Select

Return


Return

'*******************************************************************************
'SERIAL 2
'*******************************************************************************

At_serialint:
   Udrt = Udr1
   If Newrd = 1 Then
      Incr Ptrrd
      Bufrd(ptrrd) = Udrt
      Ptrrd = Ptrrd Mod Numrd
   End If
   If Chr(udrt) = ">" Then
      Initxsms = 1
      Initx = 1
   End If
   Select Case Udrt
     Case 10:
         At_serial = At_serial + Chr(0)
         Incr At_cntrlf
         Select Case At_cntrlf
            Case 1:
               At_loop = At_serial
            Case 2:
               At_resp = At_serial
            Case 3:
               At_data = At_serial
            Case 4
               At_data1 = At_serial
            Case 5
               At_data1 = At_serial
               Set T0tout
         End Select

         If At_serial = "OK" Then Set T0tout
         If At_serial = "CONNECT OK" Then Set Inicon
         If At_serial = "ALREADY CONNECT" Then Set Inicon
         If At_serial = "RDY" Then Set Mdmready
         'If At_serial = "+CIPSEND:" Then Set Initx
         If At_serial = "SEND OK" Then Set Sendok
         If At_serial = "CLOSE OK" Then Set Closeok
         If At_serial = "SHUT OK" Then Set Shutok
         If At_serial = "+CFUN: 4" Then Set Modoavion
         If At_serial = "+CFUN: 1" Then Set Modonormal
         If At_serial = "+PDP: DEACT" Then Set Fallagprs
         If At_serial = "NORMAL POWER DOWN" Then Set Mdmoff
         'If At_serial = "HTTP/1.1 200 OK" Then Set Respsrvok

         If Mid(at_serial , 1 , 5) = "+CMGS" Then Set Smsok
         If Mid(at_serial , 1 , 4) = "+IPD" Then Set Respsrvok
         If Mid(at_serial , 1 , 5) = "+CREG" Then Set Newcreg
         If Mid(at_serial , 1 , 6) = "+CGREG" Then Set Newcgreg
         If Mid(at_serial , 6 , 5) = "ERROR" Then Set T0tout
         If Mid(at_serial , 6 , 5) = "ERROR" Then Set T0tout
         If Mid(at_serial , 1 , 6) = "+CMTI:" Then
            Set Inileer
            Tmpsms = At_serial
         End If
         At_serial = ""
     Case Is <> 10
         If Udrt > 31 Then
            At_serial = At_serial + Chr(udrt)
         End If
   End Select
Return

'*******************************************************************************



'*******************************************************************************
' TIMER0
'*******************************************************************************
Int_timer0:
   Timer0 = 184
   Incr T0c
   T0c = T0c Mod 8
   If T0c = 0 Then
      Num_ventana = Num_ventana Mod 32
      Estado = Lookup(estado_led , Tabla_estado)
      Iluminar = Estado.num_ventana
      Toggle Iluminar
      Led1 = Iluminar
      Incr Num_ventana
   End If

   If T0ini = 1 Then
      Incr T0cntr
      If T0cntr = T0rate Then
         Set T0tout
      End If
   Else
      T0cntr = 0
   End If

   If Inidelay = 1 Then
      Incr Cntrdelay
      If Cntrdelay = Topdelay Then
         Set T0delay
         Reset Inidelay
      End If
   End If

   If T0inisms = 1 Then
      Incr T0cntrsms
      If T0cntrsms = 1800 Then
         Set T0toutsms
         T0cntrsms = 0
      End If
   Else
      T0cntrsms = 0
   End If

Return


Int_timer1:
   'Timer1 = &H8F80                                          'Ints cada segundo
   'Timer1 = &HFC3B                                          'Ints cada 0.1 segundo
   Timer1 = &HF4C0
   Incr T1tick
   T1tick = T1tick Mod 10
   If T1tick = 0 Then
      If T1ini = 1 Then
         Incr T1cntr
         T1cntr = T1cntr Mod T1wait
         If T1cntr = 0 Then
            Set T1tout
         End If
      Else
         T1cntr = 0
      End If

   End If
   'Timer1 = &HCF2C
   Tmpbit = Ed0 Xor Edpol.0
   Eddat.0 = Tmpbit
   Tmpbit = Ed1 Xor Edpol.1
   Eddat.1 = Tmpbit
   Tmpbit = Ed2 Xor Edpol.2
   Eddat.2 = Tmpbit
   Tmpbit = Ed3 Xor Edpol.3
   Eddat.3 = Tmpbit

   For T1tmp = 1 To Ednum
      T1tmp0 = T1tmp - 1
      Select Case Edsta(t1tmp)

         Case 0:                                               'Normal
            If Eddat.t1tmp0 = 0 Then
               Edsta(t1tmp) = 2
            End If

         Case 1:                                               'Alarma
            If Eddat.t1tmp0 = 1 Then
               Edsta(t1tmp) = 3
            End If


         Case 2:                                               'Prealarma
            If Eddat.t1tmp0 = 1 Then
               Edsta(t1tmp) = 0
               Cntrtredmenos(t1tmp) = 0
            Else
               Incr Cntrtredmenos(t1tmp)
               Cntrtredmenos(t1tmp) = Cntrtredmenos(t1tmp) Mod Tredmenos(t1tmp)
               If Cntrtredmenos(t1tmp) = 0 Then
                  Edsta(t1tmp) = 1
                  Set Evmenos.t1tmp0
                  'Incr Cntr_off(t1tmp)
               End If
            End If

         Case 3:                                               'Prenormal
            If Eddat.t1tmp0 = 0 Then
               Edsta(t1tmp) = 1
               Cntrtredmas(t1tmp) = 0
            Else
               Incr Cntrtredmas(t1tmp)
               Cntrtredmas(t1tmp) = Cntrtredmas(t1tmp) Mod Tredmas(t1tmp)
               If Cntrtredmas(t1tmp) = 0 Then
                  Edsta(t1tmp) = 0
                  Set Evmas.t1tmp0
                 ' Incr Cntr_on(t1tmp)
               End If
            End If

      End Select
   Next

Return

Sectic:
   Set Newseg
   Lsyssec = Syssec()
   For Tmpt2 = 1 To Numtxaut
      Tmpt21 = Tmpt2 - 1
      Tmplisr = Lsyssec + Offset(tmpt2)
      Tmplisr = Tmplisr Mod Autoval(tmpt2)
      If Tmplisr = 0 Then
       Set Iniauto.tmpt21
       Tbl_timeaut(tmpt2) = Lsyssec
      End If
   Next

   Tmplisr = Lsyssec Mod Tsampling
   If Tmplisr = 0 Then Set Iniadc

   If Inicpoll = 1 Then
      Incr Cntrpoll
      Cntrpoll = Cntrpoll Mod Tpoll
      If Cntrpoll = 0 Then
         Set Inipoll
         Reset Inicpoll
      End If
   End If

Return

'(
Getdatetime:
   'Toggle Pinbug
Return

Setdate:
Return

Settime:
Return
')

'*******************************************************************************
' SUBRUTINAS
'*******************************************************************************

'*******************************************************************************
' Inicialización de variables
'*******************************************************************************
Sub Inivar()
Reset Led1
Print #1 , "************ DRIVER SIM908 ************"
Print #1 , Version(1)
Print #1 , Version(2)
Print #1 , Version(3)
Estado_led = 1

Apn = Apneep
 'Apn = "watching.porta.com.ec"
Print #1 , "APN=" ; Apn

Iptx = Iptxeep
Prtotx = Prtotxeep
'Iptx = "181.199.76.185"
'Prtotx = "3000"

Print #1 , "IP <" ; Iptx ; ">"
Print #1 , "Puerto <" ; Prtotx ; ">"
'Keyapitmp = "3F0J7U4QBNDJDOVA"

Keyapi = Keyapieep
Print #1 , "Keyapi <" ; Keyapi ; ">"
Cntrini = Cntrinieep
Incr Cntrini
Cntrinieep = Cntrini
Print #1 , "CNTRINI=" ; Cntrini
Cntrinimdc = Cntrinimdceep
Print #1 , "CntriniMDC=" ; Cntrinimdc

   For Tmpb = 1 To Ednum
      Edname(tmpb) = Ednameeep(tmpb)
      Print #1 , "EDname " ; Tmpb ; "=" ; Edname(tmpb)
      Tredmas(tmpb) = Tredmaseep(tmpb)
      Print #1 , "T red + ED " ; Tmpb ; "=" ; Tredmas(tmpb) ; "x 0.1s"
      Tredmenos(tmpb) = Tredmenoseep(tmpb)
      Print #1 , "T red - ED " ; Tmpb ; "=" ; Tredmenos(tmpb) ; "x 0.1s"
      Edstaant(tmpb) = 5
      Txtrepmas(tmpb) = Txtrepmaseep(tmpb)
      Txtrepmenos(tmpb) = Txtrepmenoseep(tmpb)
      Print #1 , "TXT Rep. + ED" ; Tmpb ; " <" ; Txtrepmas(tmpb) ; ">"
      Print #1 , "TXT Rep. - ED" ; Tmpb ; " <" ; Txtrepmenos(tmpb) ; ">"
      'Cntr_on(tmpb) = Cntr_oneep(tmpb)
      'Print #1 , "CNTR ON=" ; Cntr_on(tmpb)
      'Cntr_off(tmpb) = Cntr_offeep(tmpb)
      'Print #1 , "CNTR OFF=" ; Cntr_off(tmpb)
   Next

   Edpol = Edpoleep
   Print #1 , "EDpol=" ; Bin(edpol)
   Edhab = Edhabeep
   Print #1 , "EDhab=" ; Bin(edhab)

   Tmpbit = Ed0 Xor Edpol.0
   If Tmpbit = 0 Then
      Edsta(1) = 2
   Else
      Edsta(1) = 3
   End If
   Tmpbit = Ed1 Xor Edpol.1
   If Tmpbit = 0 Then
      Edsta(2) = 2
   Else
      Edsta(2) = 3
   End If
   Tmpbit = Ed2 Xor Edpol.2

'(
   If Tmpbit = 0 Then
      Edsta(3) = 2
   Else
      Edsta(3) = 3
   End If
   Tmpbit = Ed3 Xor Edpol.3
   If Tmpbit = 0 Then
      Edsta(4) = 2
   Else
      Edsta(4) = 3
   End If
')
   Horaed = "11:12:00"
   Fechaed = "10/01/19"
   Tmpl = Syssec(horaed , Fechaed)
   'Date$ = Date(tmpl)
   'Time$ = Time(tmpl)
   Horamin = Horamineep
   Print #1 , "Horamin=" ; Horamin
   Print #1 , Date(horamin)
   Print #1 , Time(horamin)

   Adck = Adckeep

   Print #1 , "ADCK=" ; Adck

   For J = 1 To Numtxaut
      Autoval(j) = Autovaleep(j)
      Offset(j) = Offseteep(j)
      Print #1 , "Aut" ; J ; "=" ; Autoval(j) ; ", OFF" ; J ; "=" ; Offset(j)
   Next
   Set Inipoll
   Reset Resets

   Cntrsms = Cntrsmseep
   Print #1 , "CntrSMS=" ; Cntrsms

End Sub


'*******************************************************************************
' Procesamiento de comandos
'*******************************************************************************
Sub Procser()
   'Print #1 , "$" ; Serproc
   Tmpstr52 = Mid(serproc , 1 , 6)
   Numpar = Split(serproc , Cmdsplit(1) , ",")
   'If Numpar > 0 Then
   '   For Tmpb = 1 To Numpar
   '      Print #1 , Tmpb ; ":" ; Cmdsplit(tmpb)
   '   Next
   'End If

   If Len(cmdsplit(1)) = 6 Then
      Cmdtmp = Cmdsplit(1)
      Cmdtmp = Ucase(cmdtmp)
      Cmderr = 255
      Select Case Cmdtmp
         Case "LEEVFW"
            Cmderr = 0
            Atsnd = "Version FW: Fecha <"
            Tmpstr52 = Version(1)
            Atsnd = Atsnd + Tmpstr52 + ">, Archivo <"
            Tmpstr52 = Version(3)
            Atsnd = Atsnd + Tmpstr52 + ">"

         Case "SETLED"
            If Numpar = 2 Then
               Tmpb = Val(cmdsplit(2))
               If Tmpb < 17 Then
                  Cmderr = 0
                  Atsnd = "Se configura setled a " + Str(tmpb)
                  Estado_led = Tmpb
               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If

         Case "SETCMD"
            If Numpar = 2 Then
'               Tmpstr52 = Cmdsplit(2)
               Cmderr = 0
               Atsnd = "AT CMD <" + Cmdsplit(2) + ">"
               Call Atcmdsnd(cmdsplit(2) , 500)
               Print #1 , "1: " ; At_loop
               Print #1 , "2: " ; At_resp
               Print #1 , "3: " ; At_data
               Print #1 , "4: " ; At_data1
            Else
               Cmderr = 4
            End If

         Case "SNDSMS"
            Cmderr = 4
            If Numpar = 2 Then
               Cmderr = 0
               Set Newsms
'               Ser1snd = "Envia SMS a " + Cmdsplit(3)
               Atsnd = "TEST envio SMS"
               Fono = Cmdsplit(2)
            End If

            If Numpar = 3 Then
               Cmderr = 0
               Fono = Cmdsplit(2)
               Atsnd = Cmdsplit(3)
'               Ser1snd = "Envia SMS a " + Fono + ", MSG=" + Atsnd
               Set Newsms
            End If

         Case "SETAPN"
            If Numpar = 2 Then
               Cmderr = 0
               Apn = Cmdsplit(2)
               Apneep = Apn
               Atsnd = "Se configuro APN a <" + Apn + ">"
            Else
               Cmderr = 4
            End If

         Case "LEEAPN"
            Cmderr = 0
            Atsnd = "APN <" + Apn + ">"

         Case "TXGPRS"
            If Numpar = 1 Then
               Cmderr = 0
               Atsnd = "Datos GPSRS"
               Set Newtx
            Else
               If Numpar = 2 Then
                  Cmderr = 0
                  Atsnd = Cmdsplit(2)
                  Set Newtx
               Else
                  Cmderr = 4
               End If
            End If

         Case "SETIPX"
            If Numpar = 2 Then
               Cmderr = 0
               Iptx = Cmdsplit(2)
               Iptxeep = Iptx
               Atsnd = "Se configuro Ip <" + Iptx + ">"
            Else
               Cmderr = 4
            End If

         Case "SETPTX"
            If Numpar = 2 Then
               Cmderr = 0
               Prtotx = Cmdsplit(2)
               Prtotxeep = Prtotx
               Atsnd = "Se configuro Puerto <" + Prtotx + ">"
            Else
               Cmderr = 4
            End If

         Case "LEEIPX"
            If Numpar = 1 Then
               Cmderr = 0
               Atsnd = "Ip <" + Iptx + ">"
            Else
               Cmderr = 4
            End If

         Case "LEEPTX"
            If Numpar = 1 Then
               Cmderr = 0
               Atsnd = "Puerto <" + Prtotx + ">"
            Else
               Cmderr = 4
            End If

         Case "RSTVAR"
            Cmderr = 0
            Call Defaultvalues()
            Atsnd = "Se configuran parametros a valores por defecto"

         Case "SETKDA"
            If Numpar = 2 Then
               Cmderr = 0
               Keyapi = Cmdsplit(2)
               Keyapieep = Keyapi
               Atsnd = "Se configuro APIkey <" + Keyapi + ">"
            Else
               Cmderr = 4
            End If

         Case "LEEKDA"
            Cmderr = 0
            Atsnd = "APIkey <" + Keyapi + ">"

         Case "PWRSIM"
            Cmderr = 0
            Atsnd = "Pulso PWR SIM908"
            Print #1 , "PWRSIM908=1"
            Set Pwrsim908
            Reset Watchdog
            Call Espera(100)
            Reset Pwrsim908
            Call Espera(300)
            Print #1 , "PWRSIM908=0"

         Case "INIAVI"
            Cmderr = 0
            Atsnd = "TST MODO AVION"
            Set Iniavion

        Case "SETCLK"
            If Numpar = 2 Then
               If Len(cmdsplit(2)) = 12 Then
                  Cmderr = 0
                  Tmpstr52 = Mid(cmdsplit(2) , 7 , 2) + ":" + Mid(cmdsplit(2) , 9 , 2) + ":" + Mid(cmdsplit(2) , 11 , 2)
                  Print #1 , Tmpstr52
                  Tmpstr3 = Mid(cmdsplit(2) , 7 , 2)
                  _hour = Val(tmpstr3)
                  Tmpstr3 = Mid(cmdsplit(2) , 9 , 2)
                  _min = Val(tmpstr3)
                  Tmpstr3 = Mid(cmdsplit(2) , 11 , 2)
                  _sec = Val(tmpstr3)

                  'Time$ = Tmpstr52
                  Horaed = Tmpstr52
                  'Print #1 , "T>" ; Time$
                  Tmpstr52 = Mid(cmdsplit(2) , 1 , 2) + "/" + Mid(cmdsplit(2) , 3 , 2) + "/" + Mid(cmdsplit(2) , 5 , 2)
                  Print #1 , Tmpstr52
                  Tmpstr3 = Mid(cmdsplit(2) , 1 , 2)
                  _day = Val(tmpstr3)
                  Tmpstr3 = Mid(cmdsplit(2) , 3 , 2)
                  _month = Val(tmpstr3)
                  Tmpstr3 = Mid(cmdsplit(2) , 5 , 2)
                  _year = Val(tmpstr3)

                  'Date$ = Tmpstr52
                  Fechaed = Tmpstr52
                  'Print #1 , "D>" ; Date$
                  Tmpl = Syssec(horaed , Fechaed)
                  Print #1 , Tmpl
                  Tmpl = Syssec()
                  Print #1 , Tmpl

                  'Date$ = Date(tmpl)
                  'Date$ = Fechaed
                  'Time$ = Time(tmpl)
                  Atsnd = "WATCHING INFORMA. Se configuro reloj en " + Date(tmpl) + " a " + Time(tmpl)
                  Dow = Dayofweek()
                  Call Setdateds3231()
                  Call Settimeds3231()
                  Call Getdatetimeds3231()
                  Horamin = Syssec()
                  Horamineep = Horamin
                  'Set Actclkok
               Else
                  Cmderr = 6
               End If
            Else
               Cmderr = 4
            End If

         Case "LEERTC"
            Cmderr = 0
            Atsnd = "Lee RTC"
            Call Getdatetimeds3231()

         Case "SISCLK"
            Cmderr = 0
            Tmpl = Syssec()
            'Tmpstr52 = Time(tmpl)
            Tmpstr52 = Time$
            Atsnd = "Hora actual=" + Tmpstr52 + ", Fecha actual="
            'Tmpstr52 = Date(tmpl)
            Tmpstr52 = Date$
            Atsnd = Atsnd + Tmpstr52

         Case "LEECLK"
            Cmderr = 0
            Tmpstr52 = Time(horamin)
            Atsnd = "Ultima ACT CLK a =" + Tmpstr52 + ", del "
            'Tmpstr52 = Date(horamin)
            Tmpstr52 = Date$
            Atsnd = Atsnd + Tmpstr52

         Case "ENABUG"
            Cmderr = 0
            If Numpar = 2 Then
               Enabug = Val(cmdsplit(2))
               Atsnd = "Se habilita debug a" + Str(enabug)
            Else
               Cmderr = 6
            End If

         Case "LEEBUG"
            Cmderr = 0
            Atsnd = "ENABUG=" + Str(enabug)

         Case "SETADC"
            If Numpar = 2 Then
               Tmps = Val(cmdsplit(2))
               Adck = Tmps
               Adckeep = Adck
               Cmderr = 0
               Atsnd = "Se configuro K de ADC a  " + Str(adck)
            Else
               Cmderr = 4
            End If

         Case "LEEADC"
            If Numpar = 1 Then
               Cmderr = 0
               Atsnd = "K de ADC =" + Str(adck)
            Else
               Cmderr = 4
            End If

         Case "LEEVDC"
            Cmderr = 0
            Atsnd = "Voltaje de alimentacion=" + Fusing(adcvali , "#.##")

         Case "SETAUT"
            If Numpar = 3 Then
               J = Val(cmdsplit(2))
               If J > 0 And J < Numtxaut_mas_uno Then
                 Tmpstr3 = Cmdsplit(3)
                 Tmpl2 = Val(tmpstr3)
                 Autoval(j) = Tmpl2
                 Autovaleep(j) = Tmpl2
                 Cmderr = 0
                 Print #1 , "$" ; J ; "," ; Autoval(j)
                 Print #1 , "$OK"
                 Atsnd = "Se configuro tx AUT " + Str(j) + ":" + Str(autoval(j))
               Else
                  Cmderr = 3
               End If
            Else
               Cmderr = 4
            End If

         Case "SETOFF"
            If Numpar = 3 Then
               J = Val(cmdsplit(2))
               If J > 0 And J < Numtxaut_mas_uno Then
                 Tmpstr3 = Cmdsplit(3)
                 Tmpl2 = Val(tmpstr3)
                 Offset(j) = Tmpl2
                 Offseteep(j) = Tmpl2
                 Cmderr = 0
                 Print #1 , "$" ; J ; "," ; Offset(j)
                 Print #1 , "$OK"
                 Atsnd = "Se configuro tx AUT " + Str(j) + ":" + Str(offset(j))
               Else
                  Cmderr = 3
               End If
            Else
               Cmderr = 4
            End If

         Case "LEEAUT"                                      'Habilitaciones de Usuario
            If Numpar = 2 Then
               J = Val(cmdsplit(2))
               If J > 0 And J < Numtxaut_mas_uno Then
                  Tmpstr3 = Cmdsplit(3)
                  Atsnd = "Tx Aut " + Str(j) + ":" + Str(autoval(j))
                  Cmderr = 0
                  Print #1 , "$" ; J ; "," ; Autoval(j)
                  Print #1 , "$OK"
               Else
                  Cmderr = 3
               End If
            Else
               Cmderr = 4
            End If

         Case "LEEOFF"                                      'Habilitaciones de Usuario
            If Numpar = 2 Then
               J = Val(cmdsplit(2))
               If J > 0 And J < Numtxaut_mas_uno Then
                  Tmpstr3 = Cmdsplit(3)
                  Atsnd = "Offset Aut " + Str(j) + ":" + Str(offset(j))
                  Cmderr = 0
                  Print #1 , "$" ; J ; "," ; Offset(j)
                  Print #1 , "$OK"
               Else
                  Cmderr = 3
               End If
            Else
               Cmderr = 4
            End If

         Case "SETNEW"
            If Numpar = 2 Then
               J = Val(cmdsplit(2))
               If J > 0 And J < Numtxaut_mas_uno Then
                  Cmderr = 0
                  Tmpb = J - 1
                  Set Iniauto.tmpb
                  Atsnd = "Se activo Tx. AUT " + Str(j) + "," + Bin(iniauto)
               Else
                  Cmderr = 3
               End If

            Else
               Cmderr = 4
            End If

         Case "SETINI"
            If Numpar = 2 Then
               Cntrini = Val(cmdsplit(2))
               Cntrinieep = Cntrini
               Cmderr = 0
               Atsnd = "Se configuro Contador Inicios: " + Str(cntrini)
            Else
               Cmderr = 4
            End If

         Case "LEEINI"
            If Numpar = 1 Then
               Cmderr = 0
               Atsnd = "Contador inicios <" + Str(cntrini) + ">"
            Else
               Cmderr = 4
            End If

         Case "SETINM"
            If Numpar = 2 Then
               Cntrinimdc = Val(cmdsplit(2))
               Cntrinimdceep = Cntrinimdc
               Cmderr = 0
               Atsnd = "Se configuro Contador Inicios MDC: " + Str(cntrinimdc)
            Else
               Cmderr = 4
            End If

         Case "LEEINM"
            If Numpar = 1 Then
               Cmderr = 0
               Atsnd = "Contador inicios MDC<" + Str(cntrinimdc) + ">"
            Else
               Cmderr = 4
            End If

         Case "ESTADO"
            Cmderr = 0
            Atsnd = "WATCHING INFORMA. AC="
            Tmpbit = Ed0 Xor Edpol.0
            If Tmpbit = 0 Then
               Atsnd = Atsnd + "FALLA, PTA="
            Else
               Atsnd = Atsnd + "OK, PTA="
            End If
            Tmpbit = Ed1 Xor Edpol.1
            If Tmpbit = 0 Then
               Atsnd = Atsnd + "ABIERTA, VBAT="
            Else
               Atsnd = Atsnd + "CERRADA, VBAT="
            End If
            Atsnd = Atsnd + Fusing(adcvali , "#.##")

         Case "LEERED"
            Cmderr = 0
            Tmpbit = Ed0 Xor Edpol.0
            Atsnd = "WATCHING INFORMA. ED1=" + Str(tmpbit)
            Tmpbit = Ed1 Xor Edpol.1
            Atsnd = Atsnd + ", ED2=" + Str(tmpbit)
            Tmpbit = Ed2 Xor Edpol.2
            Atsnd = Atsnd + ", ED3=" + Str(tmpbit)
            Tmpbit = Ed3 Xor Edpol.3
            Atsnd = Atsnd + ", ED4=" + Str(tmpbit)

         Case "SETPAR"
            If Numpar = 5 Then
               Tmpb = Val(cmdsplit(2))
               Tmpb2 = Val(cmdsplit(3))
               Tmpb3 = Val(cmdsplit(4))

               Select Case Tmpb
                  Case 1:                                   ' ED
                     If Tmpb2 > 0 And Tmpb2 < Ednum_masuno Then
                        Select Case Tmpb3
                           Case 1:                          'Nombre
                              Edname(tmpb2) = Cmdsplit(5)
                              Ednameeep(tmpb2) = Edname(tmpb2)
                              Cmderr = 0
                              Atsnd = "Se config nombre ED" + Str(tmpb2) + "=" + Edname(tmpb2)

                           Case 2:                          ' Polaridad
                              Tmpb4 = Val(cmdsplit(5))
                              If Tmpb4 < 2 Then
                                 J = Tmpb2 - 1
                                 If Tmpb4 = 0 Then
                                    Reset Edpol.j
                                 Else
                                    Set Edpol.j
                                 End If
                                 Edpoleep = Edpol
                                 Cmderr = 0
                                 Atsnd = "EDpol" + Str(tmpb2) + "=" + Str(edpol.j)
                              Else
                                 Cmderr = 0
                                 Atsnd = "Valor de polaridad no valido"
                              End If

                           Case 3:                          ' Tactivacion +
                              Tredmas(tmpb2) = Val(cmdsplit(5))
                              Tredmaseep(tmpb2) = Tredmas(tmpb2)
                              Cmderr = 0
                              Atsnd = "Se config Tactivacion + ED" + Str(tmpb2) + "=" + Str(tredmas(tmpb2)) + "x100 ms"

                           Case 4:                          'T activacion -
                              Tredmenos(tmpb2) = Val(cmdsplit(5))
                              Tredmenoseep(tmpb2) = Tredmenos(tmpb2)
                              Cmderr = 0
                              Atsnd = "Se config Tactivacion - ED" + Str(tmpb2) + "=" + Str(tredmenos(tmpb2)) + "x100 ms"


                           Case Else
                              Cmderr = 0
                              Atsnd = "Parametro ED no valido"

                        End Select
                     Else
                        Cmderr = 0
                        Atsnd = "Num ED no valido"
                     End If

                  Case 2:                                   'EA
                     Cmderr = 0
                     Atsnd = "EA no hab en esta version"
'                     If Tmpb2 > 0 And Tmpb2 < Eanum_masuno Then
'                        Select Case Tmpb3
'                           Case 1:                          'Nombre
'                              Eaname(tmpb2) = Cmdsplit(5)
'                              Eanameeep(tmpb2) = Eaname(tmpb2)
'                              Cmderr = 0
'                              Atsnd = "Se config nombre EA" + Str(tmpb2) + "=" + Eaname(tmpb2)

'                           Case 2:                          ' Inicio de escala
'                              Iniescala(tmpb2) = Val(cmdsplit(5))
'                              Iniescalaeep(tmpb2) = Iniescala(tmpb2)
'                              Cmderr = 0
'                              Atsnd = "Inicio escala EA" + Str(tmpb2) + "=" + Str(iniescala(tmpb2))

'                           Case 3:                          ' Fondo de escala
'                              Fondoescala(tmpb2) = Val(cmdsplit(5))
'                              Fondoescalaeep(tmpb2) = Fondoescala(tmpb2)
'                              Cmderr = 0
'                              Atsnd = "Fondo escala EA" + Str(tmpb2) + "=" + Str(fondoescala(tmpb2))

'                           Case Else
'                              Cmderr = 0
'                              Atsnd = "Parametro EA no valido"

'                        End Select
'                     Else
'                        Cmderr = 5
'                     End If

'                  Case 3:                                   'MODBUS
'                     Cmderr = 0
'                     Atsnd = "MODBUS no implementado en esta version"

                  Case Else:
                     Cmderr = 0
                     Atsnd = "Tipo de entrada incorrecto"

               End Select

            Else
               Cmderr = 4
            End If

         Case "LEEPAR"
            If Numpar = 3 Then
               Tmpb = Val(cmdsplit(2))
               Tmpb2 = Val(cmdsplit(3))
               Select Case Tmpb
                  Case 1:
                     Cmderr = 0                             'ED
                     If Tmpb2 > 0 And Tmpb2 < Ednum_masuno Then
                        J = Tmpb2 - 1
                        Atsnd = "ED" + Str(tmpb2) + ";Nombre=" + Edname(tmpb2) + ";POL=" + Str(edpol.j) + ";Tact+=" + Str(tredmas(tmpb2)) + "x100ms ;Tact-=" + Str(tredmenos(tmpb2)) + "x100ms"
                     Else
                        Cmderr = 0
                        Atsnd = "Num ED no valido"
                     End If

                  Case 2:                                   ' EA
                     Cmderr = 0                             'ED
                     Atsnd = "EA no hab en esta version"
'                     If Tmpb2 > 0 And Tmpb2 < Eanum_masuno Then
'                        Atsnd = "EA" + Str(tmpb2) + ";Nombre=" + Eaname(tmpb2) + ";Iniescala=" + Str(iniescala(tmpb2)) + ";Fondoescala=" + Str(fondoescala(tmpb2))
'                     Else
'                        Cmderr = 0
'                        Atsnd = "Num ED no valido"
'                     End If

'                  Case 3:                                      'MODBUS


               End Select
            Else
               Cmderr = 4
            End If

         Case "INIMDM"
            If Numpar = 2 Then
               Cmderr = 0
               Tmpb = Val(cmdsplit(2))
               If Tmpb = 0 Then
                  Set Iniapagarmdm
                  Atsnd = "Apagar MDM"
               Else
                  Set Inimodem
                  Atsnd = "Iniciar MDM"
               End If
            Else
               Cmderr = 4
            End If

         Case "RESETS"
            Cmderr = 0
            Atsnd = "Reset por software"
            Set Resets

         Case "INIVAR"
            Cmderr = 0
            Atsnd = "Reinicio de variables"
            Set Inivariables

         Case "SETHAB"
            If Numpar = 3 Then
               Tmpb = Val(cmdsplit(2))
               If Tmpb > 0 And Tmpb < Ednum_masuno Then
                  Cmderr = 0
                  Tmpb3 = Tmpb - 1
                  Tmpb2 = Val(cmdsplit(3))
                  If Tmpb2 = 0 Then
                     Reset Edhab.tmpb3
                  Else
                     Set Edhab.tmpb3
                  End If
                  Edhabeep = Edhab
                  Atsnd = "Se configuro HAB ED" + Str(tmpb) + "=" + Str(edhab.tmpb3)
               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If

         Case "LEEHAB"
            Select Case Numpar
               Case 1:
                  Cmderr = 0
                  Atsnd = "Hab ED=" + Bin(edhab)
               Case 2:
                  Tmpb = Val(cmdsplit(2))
                  If Tmpb > 0 And Tmpb < Ednum_masuno Then
                     Tmpb3 = Tmpb - 1
                     Cmderr = 0
                     Atsnd = "HAB ED" + Str(tmpb) + "=" + Str(edhab.tmpb3)
                  Else
                     Cmderr = 5
                  End If
               Case Else
                  Cmderr = 4
            End Select


         Case "SETPOL"
            If Numpar = 3 Then
               Tmpb = Val(cmdsplit(2))
               If Tmpb > 0 And Tmpb < Ednum_masuno Then
                  Cmderr = 0
                  Tmpb3 = Tmpb - 1
                  Tmpb2 = Val(cmdsplit(3))
                  If Tmpb2 = 0 Then
                     Reset EdPOL.tmpb3
                  Else
                     Set Edpol.tmpb3
                  End If
                  Edpoleep = Edpol
                  Atsnd = "Se configuro Pol ED" + Str(tmpb) + "=" + Str(edpol.tmpb3)
               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If

         Case "LEEPOL"
            Select Case Numpar
               Case 1:
                  Cmderr = 0
                  Atsnd = "POL ED=" + Bin(edpol)
               Case 2:
                  Tmpb = Val(cmdsplit(2))
                  If Tmpb > 0 And Tmpb < Ednum_masuno Then
                     Tmpb3 = Tmpb - 1
                     Cmderr = 0
                     Atsnd = "POL ED" + Str(tmpb) + "=" + Str(edPOL.tmpb3)
                  Else
                     Cmderr = 5
                  End If
               Case Else
                  Cmderr = 4
            End Select

         Case "SETSMS"
            If Numpar = 2 Then
               Cmderr = 0
               Cntrsms = Val(cmdsplit(2))
               Cntrsmseep = Cntrsms
               Atsnd = "SE conf CntrSMS=" + Str(cntrsms)

            Else
               Cmderr = 4
            End If

         Case "LEESMS"
            Cmderr = 0
            Atsnd = "CntrSMS=" + Str(cntrsms)

         Case Else
            Cmderr = 1

      End Select

   Else
        Cmderr = 2
   End If

   If Cmderr > 0 Then
      Atsnd = Lookupstr(cmderr , Tbl_err)
   End If

   Print #1 , Atsnd

End Sub


Sub Inimdm()
   Estado_led = 4
   Print #1 , "Inicio MDM"
   Call Espera(100)
   Print #1 , "Ver. si MDM ON?"
   Incr Cntrinimdc
   Cntrinimdceep = Cntrinimdc

   Do
      Do
         If Pwrctl = 1 Then
            Print #1 , "MDM ON"
            Set Mdmon
         Else
            Incr Tmpb
            Print #1 , "Int. de encender MDM " ; Tmpb ; " de " ; Tryon
            Print #1 , "Ini MDM..."
            Print #1 , "HW OFF"
            Set Pwrsim908
            Reset Watchdog
            Call Espera(100)
            Reset Pwrsim908
            Call Espera(300)
            T1wait = 5
            T1cntr = 0                                         'Tout=5s
            Reset T1tout
            Set T1ini
            Do
               Reset Watchdog
            Loop Until Pwrctl = 1 Or T1tout = 1

            If Pwrctl = 1 Then
               Set Mdmon
            Else
               Print #1 , "PWRCTL=0"
            End If

         End If
         Reset Watchdog
      Loop Until Mdmon = 1 Or Tmpb = Tryon

      If Mdmon = 1 Then
         Reset Watchdog
         Call Espera(100)

         Tmpb3 = 0
         Do
            Tf_error = 0
            Mdmerror = 0
            Print #1 , "AT?"
            Do
               Call Atcmdsnd( "AT" , 10)
               Print #1 , "1: " ; At_loop
               Print #1 , "2: " ; At_resp
               Print #1 , "3: " ; At_data
               Print #1 , "4: " ; At_data1
               Call Espera(100)
               Incr Tf_error
            Loop Until At_resp = "OK" Or At_data = "OK" Or At_data1 = "OK" Or Pwrctl = 0 Or Tf_error = 5

            If At_resp = "OK" Or At_data = "OK" Or At_data1 = "OK" Then
               'Tmpb3 = 1
               Tmpb = 0
               Print #1 , "SIM?"
               Do
                  Incr Tmpb
                  Print #1 , "Intento MDM SIM " ; Tmpb ; " de " ; Trysimon
                  Call Atcmdsnd( "AT+CPIN?" , 10)
                  Print #1 , "1: " ; At_loop
                  Print #1 , "2: " ; At_resp
                  Print #1 , "3: " ; At_data
                  Print #1 , "4: " ; At_data1
         '         Wait 1
                  Call Espera(100)
               Loop Until At_resp = "+CPIN: READY" Or At_data = "+CPIN: READY" Or At_data1 = "+CPIN: READY" Or Tmpb = Trysimon

               If At_resp = "+CPIN: READY" Or At_data = "+CPIN: READY" Or At_data1 = "+CPIN: READY" Then
                  'Tmpb3 = 1
                  Tmpb = 0
                  Do
                     Tmpstr52 = Lookupstr(tmpb , Tbl_msgat)
                     Print #1 , Tmpstr52
                     Tmpstr52 = Lookupstr(tmpb , Tbl_cmdat)
                     Atsnd = Tmpstr52
                     Tmpstr52 = ""
                     If Tmpb = 3 Then
                        Apn = Apneep
                        Tmpstr52 = Apn
                     End If
'                     Select Case Tmpb
'                        Case 2
'                           Tmpstr52 = Apn
'                     End Select
                     Atsnd = Atsnd + Tmpstr52
                     Tmpstr52 = Lookupstr(tmpb , Tbl_cmdfin)
                     Atsnd = Atsnd + Tmpstr52
                     Tf_error = 0
                     Do
                        Call Atcmdsnd(atsnd , 10)
                        Print #1 , "1: " ; At_loop
                        Print #1 , "2: " ; At_resp
                        Print #1 , "3: " ; At_data
                        Print #1 , "4: " ; At_data1
                        Call Espera(100)
                        Incr Tf_error
                     Loop Until At_resp = "OK" Or At_data = "OK" Or At_data1 = "OK" Or Pwrctl = 0 Or Tf_error = 10

                     If Tf_error = 10 Then
                        Mdmerror = 2
                        Print #1 , "ERROR CMD<" ; Atsnd ; ">"
                     End If
                     'Print Atcmd
                     Incr Tmpb
                  Loop Until Tmpb = Numcmd Or Mdmerror = 2

                  Print #1 , "Cerrando sesiones anteriores"
                  Call Atcmdsnd( "AT+CIPSHUT" , 100)
                  Print #1 , "1: " ; At_loop
                  Print #1 , "2: " ; At_resp
                  Print #1 , "3: " ; At_data
                  Print #1 , "4: " ; At_data1

                  Print #1 , "Configurando APN"
                  Apn = Apneep
                  Tmpstr52 = "AT+CSTT=" + Chr(34) + Apn + Chr(34) + "," + Chr(34) + Chr(34) + "," + Chr(34) + Chr(34)       '"{034},{034}{034},{034}{034}"
                  Call Atcmdsnd(tmpstr52 , 500)
                  Print #1 , "1: " ; At_loop
                  Print #1 , "2: " ; At_resp
                  Print #1 , "3: " ; At_data
                  Print #1 , "4: " ; At_data1
                  Estado_led=1

                  If Tmpb = Numcmd Then
                     Tmpb3 = 1
                  End If

               End If

            Else
               Print #1 , "MDM ON"
               Set Pwrsim908
               Reset Watchdog
               Call Espera(100)
               Reset Pwrsim908
               Call Espera(300)
               Print #1 , "Wait AT OK"
               Tmpb = 0
               Do
                  Call Espera(100)
                  Incr Tmpb
                  Print #1 , "1: " ; At_loop
                  Print #1 , "2: " ; At_resp
                  Print #1 , "3: " ; At_data
                  Print #1 , "4: " ; At_data1
               Loop Until Mdmready = 1 Or Tmpb = 4

            End If

         Loop Until Tmpb3 = 1

      Else
         Mdmerror = 1

      End If

   Loop Until Tmpb3 = 1

   Print #1 , "BorrSMS"
   Call Espera(100)
   Tmpstr52 = "AT+CMGD=1"
   Call Atcmdsnd(tmpstr52 , 10)
   Print #1 , "1: " ; At_loop
   Print #1 , "2: " ; At_resp
   Print #1 , "3: " ; At_data
   Print #1 , "4: " ; At_data1

   For J = 2 To 8
      Reset T1tout
      T1cntr = 0
      T1wait = 15
      Set T1ini
      Tmpb3 = 0
      Do
         Tmpstr52 = "AT+CMGD=" + Str(j)
         Call Atcmdsnd(tmpstr52 , 10)
         Print #1 , "1: " ; At_loop
         Print #1 , "2: " ; At_resp
         Print #1 , "3: " ; At_data
         Print #1 , "4: " ; At_data1
         If Mid(at_loop , 1 , 8) = "AT+CMGD=" And At_resp = "OK" Then
            Tmpb3 = 1
         End If
         If Mid(at_loop , 1 , 8) = "AT+CMGD=" And At_resp = "ERROR" Then
            Tmpb3 = 1
         End If
      Loop Until Tmpb3 = 1 Or T1tout = 1
   Next


End Sub


' Subrutina para enviar comandos al MDM
'*******************************************************************************
Sub Atcmdsnd(byval Atcmd As String * 100 , Byval T0ratedat As Byte )
   At_loop = ""
   At_resp = ""
   At_data = ""
   At_data1 = ""
   At_cntrlf = 0
   Print #2 , Atcmd ; Chr(13);
   Reset Watchdog
   T0rate = T0ratedat
   T0cntr = 0
   Set T0ini
   Reset T0tout
'   While T0tout = 0
   Do
      Reset Watchdog
'   Wend
   Loop Until T0tout = 1 Or At_loop <> "" Or At_resp <> "" Or At_data <> "" Or At_data1 <> ""

   If T0tout = 1 Then
      At_serial = ""
   End If

   Reset T0tout
   Reset T0ini
End Sub

'******************************************************************************
' Comprueba registro GSM
'******************************************************************************
Sub Vergsm()
   Tmpstr52 = "AT+CREG?"
   Tmpat = ""
   Call Atcmdsnd(tmpstr52 , 500)
   Call Espera(5)

   If Mid(at_loop , 1 , 5) = "+CREG" Then
      Tmpat = At_loop
   Else
      If Mid(at_resp , 1 , 5) = "+CREG" Then
         Tmpat = At_resp
      Else
         If Mid(at_data , 1 , 5) = "+CREG" Then
            Tmpat = At_data
         Else
            If Mid(at_data1 , 1 , 5) = "+CREG" Then
               Tmpat = At_data1
            End If
         End If
      End If
   End If

'   If Tmpat <> "" Then
'      Print #1 , "TMPAT=" ; Tmpat
'   End If

   If Tmpat <> At_respcreg Then
      If Mid(tmpat , 1 , 5) = "+CREG" Then
         At_respcreg = Tmpat
         Print #1 , "Nuevo estado CREG"
         Print #1 , "1: " ; At_loop
         Print #1 , "2: " ; At_resp
         Print #1 , "3: " ; At_data
         Print #1 , "4: " ; At_data1
         Print #1 , Time$ ; "," ; Date$
      End If
   End If

   Tmpstr3 = Mid(tmpat , 10 , 1)
   If Tmpstr3 = "1" Or Tmpstr3 = "5" Then
      Set Gsmok
   Else
      Reset Gsmok
   End If
   Tmpstr52 = "AT"
   Call Atcmdsnd(tmpstr52 , 10)
   Tmpat = ""
End Sub

'******************************************************************************
' Comprueba registro GPRS
'******************************************************************************

Sub Vergprs()
   Tmpat = ""
   Tmpstr52 = "AT+CGREG?"
   Call Atcmdsnd(tmpstr52 , 200)
   Call Espera(5)
   If Mid(at_loop , 1 , 6) = "+CGREG" Then
      Tmpat = At_loop
   Else
      If Mid(at_resp , 1 , 6) = "+CGREG" Then
         Tmpat = At_resp
      Else
         If Mid(at_data , 1 , 6) = "+CGREG" Then
            Tmpat = At_data
         Else
            If Mid(at_data1 , 1 , 6) = "+CGREG" Then
               Tmpat = At_data1
            End If
         End If
      End If
   End If


   If Tmpat <> At_respcgreg Then
      If Mid(tmpat , 1 , 6) = "+CGREG" Then
         Print #1 , "Nuevo estado CGREG"
         At_respcgreg = Tmpat
         Print #1 , "1: " ; At_loop
         Print #1 , "2: " ; At_resp
         Print #1 , "3: " ; At_data
         Print #1 , "4: " ; At_data1
         Print #1 , Time$ ; "," ; Date$
      End If
   End If
   Tmpstr3 = Mid(tmpat , 11 , 1)
   If Tmpstr3 = "1" Or Tmpstr3 = "5" Then
      Set Gprsok
      Cntracterr = 0
   Else
      Reset Gprsok
      Incr Cntracterr
      Print #1 , Cntracterr
      Cntracterr = Cntracterr Mod 12
      If Cntracterr = 0 Then
         Gprstxsta = "NO REG"
         Call Tstavion()
      End If

   End If
   Tmpstr52 = "AT"
   Call Atcmdsnd(tmpstr52 , 10)
End Sub

' SUBRUTINA DE ENVIO DE SMS EN FORMATO TEXTO
'*******************************************************************************
Sub Smstx()
   Reset Watchdog
   Estado_led = 9
   Print #1 , "MSG>" ; Atsnd
   If Len(atsnd) > 160 Then
      Atsnd = Mid(atsnd , 1 , 160)
      Print #1 , "MSGt>" ; Atsnd
   End If
   Print #1 , "TLF>" ; Fono
   Tf_error = 0
   Initxsms = 0
   Do
      Reset Watchdog
      Call Atcmdsnd( "AT+CMGF=1" , 100)
      Print #1 , "1: " ; At_loop
      Print #1 , "2: " ; At_resp
      Print #1 , "3: " ; At_data
      Print #1 , "4: " ; At_data1
      Incr Tf_error
   Loop Until At_loop = "OK" Or At_resp = "OK" Or At_data = "OK" Or At_data1 = "OK" Or Tf_error > 20

   If Tf_error < 20 Then
      Tmpstr52 = "AT+CMGS=" + Chr(34) + Fono + Chr(34)      'Activo el modo de texto
      Print #1 , Tmpstr52 ; Chr(13)
      Print #2 , Tmpstr52 ; Chr(13)

      Reset T1ini
      Reset T1tout
      T1cntr = 0
      T1wait = 8
      Set T1ini
      Do
         Reset Watchdog
      Loop Until T1tout = 1 Or Initxsms = 1
   '   Call Espera(200)
      Print #1 , Atsnd ;                                       '
      Print #2 , Atsnd ;                                       '
      At_serial = ""
      At_loop = ""
      At_resp = ""
      At_data = ""
      At_data1 = ""
      At_cntrlf = 0
      Reset T1tout
      Reset T0toutsms
      Reset Smsok
      Print #1 , Chr(26);
      Print #2 , Chr(26);

      Set T0inisms
      Do
      Loop Until Smsok = 1 Or T0toutsms = 1                 'Or At_resp <> "" Or At_data <> "" Or At_data1 <> ""

      Reset T0toutsms
      Reset T0inisms

      At_cntrlf = 0
      Print #1 , "1: " ; At_loop
      Print #1 , "2: " ; At_resp
      Print #1 , "3: " ; At_data
      Print #1 , "4: " ; At_data1

      If Mid(at_resp , 1 , 5) = "+CMGS" Then
         Smstxsta = "TX OK"
         Estado_led = 1
      Else
         Smstxsta = "NO TX"
         Estado_led = 3
      End If

      Print #1 , Smstxsta
   Else
      Print #1 , "ERROR en cambio de formatode texto"
      Estado_led =3
   End If

End Sub

'*******************************************************************************
' Subrutinas para procesar SMS
'*******************************************************************************
Sub Procsms()
      Print #1 , "N:" ; Tmpsms
      Tmpb = Instr(1 , Tmpsms , ",")
      Tmpb2 = Tmpb + 1
      Tmpb3 = Len(tmpsms)
      Tmpb3 = Tmpb3 - Tmpb
      Tmpstr8 = Mid(tmpsms , Tmpb2 , Tmpb3)
      Print #1 , "T:" ; Tmpstr8
      Ls_str2 = Tmpstr8
      'Reset Waittx
      Call Leersms()
End Sub

Sub Leersms()
   Tf_error = 0
   Print #1 , "Sms"
   Do
      Call Atcmdsnd( "AT+CMGF=1" , 100)
         Print #1 , "1: " ; At_loop
         Print #1 , "2: " ; At_resp
         Print #1 , "3: " ; At_data
         Print #1 , "4: " ; At_data1
      Incr Tf_error
   Loop Until At_resp = "OK" Or Tf_error > 200

   Tf_error = 0
   Do
      Tmpstr52 = "AT+CMGR=" + Ls_str2
      Call Atcmdsnd(tmpstr52 , 100)
      Print #1 , "1: " ; At_loop
      Print #1 , "2: " ; At_resp
      Print #1 , "3: " ; At_data
      Print #1 , "4: " ; At_data1
      Incr Tf_error
   Loop Until At_data1 = "OK" Or Mid(at_resp , 6 , 5 ) = "ERROR" Or Mid(at_data , 6 , 5 ) = "ERROR" Or Tf_error > 200 Or Pwrctl = 0 Or Mid(at_resp , 9 , 10) = "REC UNREAD" Or At_resp = "OK"

   If At_data1 = "OK" Or Mid(at_resp , 9 , 10) = "REC UNREAD" Then
      Numpar = Split(at_resp , Cmdsplit(1) , Chr(34))
      Atsnd = At_data
      Print #1 , "MSG>" ; Atsnd
      Tf_num1 = Cmdsplit(4)
      Print #1 , "FONO>" ; Tf_num1
      Fecha_rx = Cmdsplit(8)
      Print #1 , "Fecha_rx>" ; Fecha_rx
      Msglong = Len(atsnd)
       Do
         Call Atcmdsnd( "AT+CMGD=1" , 100)         'Print #4 , "Borrando mensaje..."
         Print #1 , "1: " ; At_loop
         Print #1 , "2: " ; At_resp
         Print #1 , "3: " ; At_data
         Print #1 , "4: " ; At_data1
       Loop Until At_resp = "OK" Or Mid(at_resp , 6 , 5 ) = "ERROR" Or Pwrctl = 0
   End If

End Sub

' Subrutina para generar espera
'*******************************************************************************
Sub Espera(byval Tespera As Word)
   Topdelay = Tespera
   Cntrdelay = 0
   Reset T0delay
   Set Inidelay
   While T0delay = 0
      Reset Watchdog
   Wend

End Sub

'*******************************************************************************
' Subrutina para enviar datos GPRS
'*******************************************************************************
Sub Sendtrans()
   Print #2 , Atsnd;
   Print #1 , Atsnd
End Sub

'*******************************************************************************
' Procesamiento de tramas para TS
'*******************************************************************************
Sub Procdaa()
   Print #1 , "DAA_data>" ; Gwsproc
   Numpar = Split(gwsproc , Cmdsplit(1) , ",")
   Print #1 , "Numpar>" ; Numpar
   If Numpar = 12 Then
      For K = 1 To 8
         Habfeed(k) = 0
      Next
      Fechaed = Cmdsplit(2)
      Horaed = Cmdsplit(3)
      If Fechaed <> "" And Horaed <> "" Then
         Tmpl = Syssec(horaed , Fechaed)
         'Tmpl = Tmpl - Husos
         Tmpl = Tmpl + 18000
         Fechaed = Date(tmpl)
         Horaed = Time(tmpl)
      Else
         Fechaed = ""
         Horaed = ""
      End If
      Keyapitmp = Cmdsplit(4)
      For K = 1 To 8
         Tmpb = K + 4
         If Cmdsplit(tmpb) <> "" Then
            Habfeed(k) = 1
            Valfeed(k) = Cmdsplit(tmpb)
         Else
            Print #1 , "No data field" ; K
         End If
      Next
      Set Newtx


   Else
      Print #1 , "Trama TS no valida"
   End If
End Sub

'*******************************************************************************
' Formatea trama en formato JSON
'*******************************************************************************
Sub Genjsonts()
   Datamdc = ""
   For K = 1 To 8
      If Habfeed(k) = 1 Then
         Datamdc = Datamdc + "field" + Str(k) + "=" + Valfeed(k) + "&"
      End If
   Next

   If Fechaed <> "" And Horaed <> "" Then
      Tmpstr52 = "created_at=20" + Mid(fechaed , 7 , 2) + "-" + Mid(fechaed , 4 , 2)
      Tmpstr52 = Tmpstr52 + "-" + Mid(fechaed , 1 , 2) + "%20"
      Tmpstr52 = Tmpstr52 + Horaed
      Datamdc = Datamdc + Tmpstr52
   End If

   Atsnd = "POST /update HTTP/1.1" + Chr(13) + Chr(10)
   Call Sendtrans()
   Atsnd = "Host: api.thingspeak.com" + Chr(13) + Chr(10)
   Call Sendtrans()
   Atsnd = "Connection: close" + Chr(13) + Chr(10)
   Call Sendtrans()
   Atsnd = "X-THINGSPEAKAPIKEY:" + Keyapitmp + Chr(13) + Chr(10)
   Call Sendtrans()
   Atsnd = "Content-Type: application/x-www-form-urlencoded" + Chr(13) + Chr(10)
   Call Sendtrans()
   Atsnd = "Content-Length: "
   Call Sendtrans()
   Tmpb = Len(datamdc)
   Atsnd = Str(tmpb) + Chr(13) + Chr(10)
   Call Sendtrans()

   Atsnd = Chr(13) + Chr(10)
   Call Sendtrans()
   Atsnd = Datamdc
   Call Sendtrans()
End Sub


Sub Getip()
   Print #1 , "Sinc MTCP"
   Tf_error = 0
   Gprstxsta = "NO TX"
   Reset Atok
   Apn = Apneep
   Do
      Call Atcmdsnd( "AT" , 100)
      Print #1 , "1: " ; At_loop
      Print #1 , "2: " ; At_resp
      Print #1 , "3: " ; At_data
      Print #1 , "4: " ; At_data1
      Call Espera(50)
      If At_resp = "OK" Or At_data = "OK" Or At_data1 = "OK" Then
         Set Atok
      End If
      Incr Tf_error
   Loop Until Atok = 1 Or Pwrctl = 0 Or Tf_error > 10

   If Atok = 1 Then
      Print #1 , "Cierra conexiones anteriores"
      Reset T1tout
      T1cntr = 0
      T1wait = 10
      Set T1ini
      Reset Shutok
      Do
         Call Atcmdsnd( "AT+CIPSHUT" , 100)
         Call Espera(100)
      Loop Until Shutok = 1 Or T1tout = 1    'Or Dcdgsm = 1
      Print #1 , "1: " ; At_loop
      Print #1 , "2: " ; At_resp
      Print #1 , "3: " ; At_data
      Print #1 , "4: " ; At_data1
      Tf_error = 0
      Reset Csttok
      Call Atcmdsnd( "AT+CIFSR" , 500)
      Print #1 , "1: " ; At_loop
      Print #1 , "2: " ; At_resp
      Print #1 , "3: " ; At_data
      Print #1 , "4: " ; At_data1
      Numpar = Split(at_resp , Cmdsplit(1) , ".")
      If Numpar = 4 Then
         Print #1 , "VER IP"
         If At_resp <> "0.0.0.0" Then
            Set Cifsrok
            Print #1 , "IP OK"
         Else
            Print #1 , "NO IP val"
         End If
      Else
         Print #1 , "IP FAIL"
         Reset Cifsrok
         Call Atcmdsnd( "AT+CSTT" , 100)
         If At_resp = "OK" Then
            Set Csttok
         Else
            Do
               Call Atcmdsnd( "AT+CSTT?" , 100)
               Print #1 , "1: " ; At_loop
               Print #1 , "2: " ; At_resp
               Print #1 , "3: " ; At_data
               Print #1 , "4: " ; At_data1
               Call Espera(50)
               Numpar = Split(at_resp , Cmdsplit(1) , Chr(34))
               If Cmdsplit(2) = Apn Then
                  Set Csttok
               Else
                  If At_loop = "OK" Or At_resp = "OK" Or At_data = "OK" Or At_data1 = "OK" Then
                     Set Csttok
                  End If
               End If
               Incr Tf_error
            Loop Until Csttok = 1 Or Pwrctl = 0 Or Tf_error > 10
         End If
         If Csttok = 1 Then
            Reset Ciicrok
            Call Atcmdsnd( "AT+CIICR" , 100)
            Print #1 , "1: " ; At_loop
            Print #1 , "2: " ; At_resp
            Print #1 , "3: " ; At_data
            Print #1 , "4: " ; At_data1
            Reset T1tout
            T1cntr = 0
            T1wait = 45
            Set T1ini
            Do
               If At_resp = "OK" Or At_data = "OK" Or At_data1 = "OK" Then
                Set Ciicrok
               End If
               If At_resp = "+PDP: DEACT" Or At_data = "+PDP: DEACT" Or At_data1 = "+PDP: DEACT" Then
                Set T1tout
               End If
               If At_resp = "ERROR" Or At_data = "ERROR" Or At_data1 = "ERROR" Then
                Set Ciicrok
               End If
            Loop Until Ciicrok = 1 Or T1tout = 1
         Else
            Print #1 , "No se pudo obtener IP"
            Reset T1tout
            Print #1 , "Cerrando conexión"
            Reset T1tout
            T1cntr = 0
            T1wait = 10
            Set T1ini
            Reset Shutok
            Do
               Call Atcmdsnd( "AT+CIPSHUT" , 100)
               Call Espera(100)
            Loop Until Shutok = 1 Or T1tout = 1    'Or Dcdgsm = 1
            Print #1 , "1: " ; At_loop
            Print #1 , "2: " ; At_resp
            Print #1 , "3: " ; At_data
            Print #1 , "4: " ; At_data1
            Tmpstr52 = "AT+CSTT=" + Chr(34) + Apn + Chr(34) + "," + Chr(34) + Chr(34) + "," + Chr(34) + Chr(34)       '"{034},{034}{034},{034}{034}"
            Call Atcmdsnd(tmpstr52 , 500)
            Print #1 , "1: " ; At_loop
            Print #1 , "2: " ; At_resp
            Print #1 , "3: " ; At_data
            Print #1 , "4: " ; At_data1
         End If
         If Ciicrok = 1 Then
            Reset Cifsrok
            Call Atcmdsnd( "AT+CIFSR" , 500)
            Print #1 , "1: " ; At_loop
            Print #1 , "2: " ; At_resp
            Print #1 , "3: " ; At_data
            Print #1 , "4: " ; At_data1
            Numpar = Split(at_resp , Cmdsplit(1) , ".")
            If Numpar = 4 Then
               Print #1 , "IP OK"
               Set Cifsrok
            Else
               Print #1 , "IP FAIL"
               Reset Cifsrok
            End If
         End If
      End If
   Else
      Print #1 , "No SINC M"
      Gprstxsta = "NO MDM"
      Set Inimodem
   End If

End Sub


'*******************************************************************************
'TX DATOS GPRS
'*******************************************************************************
Sub Txdata(byval Trama As Byte)
   Estado_led = 8
   Reset Txok
   Print #1,
   Print #1 , "Ini TX"
   Print #1 , "Ver IP"
   If Cifsrok = 0 Then
      Call Getip()
      If Cifsrok = 0 Then
         Print #1 , "Err por IP fail"
         Print #1 , "Cntrerrtx=";Cntrerrtx
         Incr Cntrerrtx
      Else
         Cntrerrtx = 0
      End If
   Else
      Reset Inicon
      Tmpb2 = 0
      Tmpstr52 = "AT+CIPSTART=" + Chr(34) + "TCP" + Chr(34) + "," + Chr(34) + Iptx + Chr(34) + "," + Chr(34) + Prtotx + Chr(34)
      Call Atcmdsnd(tmpstr52 , 500)
      Print #1 , "1: " ; At_loop
      Print #1 , "2: " ; At_resp
      Print #1 , "3: " ; At_data
      Print #1 , "4: " ; At_data1
      At_loop = ""
      At_resp = ""
      At_data = ""
      At_data1 = ""

      Reset T1tout
      T1cntr = 0
      T1wait = 40
      Set T1ini
      Print #1 , "Espera conexion"
      Do
         Reset Watchdog
      Loop Until Inicon = 1 Or T1tout = 1
      Print #1 , "1: " ; At_loop
      Print #1 , "2: " ; At_resp
      Print #1 , "3: " ; At_data
      Print #1 , "4: " ; At_data1
      If Inicon = 1 Then
         Reset Initx
         Tmpstr52 = "AT+CIPSEND?"
         Call Atcmdsnd(tmpstr52 , 200)
         Print #1 , "1: " ; At_loop
         Print #1 , "2: " ; At_resp
         Print #1 , "3: " ; At_data
         Print #1 , "4: " ; At_data1
         Tmpstr52 = "AT+CIPSEND"
         Call Atcmdsnd(tmpstr52 , 40)
         Reset T1tout
         T1cntr = 0
         T1wait = 15
         Set T1ini
         Do
            Reset Watchdog
         Loop Until Initx = 1 Or T1tout = 1
         Print #1 , "1: " ; At_loop
         Print #1 , "2: " ; At_resp
         Print #1 , "3: " ; At_data
         Print #1 , "4: " ; At_data1
         If Initx = 1 Then
            Reset Initx
            If Trama = 0 Then                            ' WATCHING SERVER
               Call Sendtrans()
            End If
            If Trama = 1 Then                            ' WATCHING SERVER
               Call Genjsonts()
            End If
            Reset Sendok
            Reset Respsrvok
            Set Newrd
            Print #1 , Chr(26);
            Print #2 , Chr(26);
            At_loop = ""
            At_resp = ""
            At_data = ""
            At_data1 = ""

            Reset T1tout
            T1cntr = 0
            T1wait = 20
            Set T1ini
            Do
               Reset Watchdog
            Loop Until At_loop <> "" Or At_resp <> "" Or At_data <> "" Or At_data1 <> "" Or T1tout = 1 Or Sendok = 1       ' Or Sendfail = 1
            Reset T1tout
            Print #1 , "1: " ; At_loop
            Print #1 , "2: " ; At_resp
            Print #1 , "3: " ; At_data
            Print #1 , "4: " ; At_data1

            Print #1 , "Espera respuesta server"

            Reset T1tout
            T1cntr = 0
            T1wait = 6
            Set T1ini
            Do
            Loop Until Respsrvok = 1 Or T1tout = 1
            Print #1 , "FIN ESPERA"

            Reset T1tout
            T1cntr = 0
            T1wait = 10
            Set T1ini
            Print #1 , "Close"
            Reset Closeok
            Call Atcmdsnd( "AT+CIPCLOSE" , 100)
            Call Espera(100)

            Do
            Loop Until Closeok = 1 Or T1tout = 1         'Or Dcdgsm = 1
            Print #1 , "1: " ; At_loop
            Print #1 , "2: " ; At_resp
            Print #1 , "3: " ; At_data
            Print #1 , "4: " ; At_data1

            Print #1 , "Bye"
            If Closeok = 1 Then
               Print #1 , "CLOSE OK"
            Else
               Reset T1tout
               Print #1 , "NO CLOSE OK rx"
               Reset T1tout
               T1cntr = 0
               T1wait = 10
               Set T1ini
               Reset Shutok
               Do
                  Call Atcmdsnd( "AT+CIPSHUT" , 100)
                  Call Espera(100)
               Loop Until Shutok = 1 Or T1tout = 1    'Or Dcdgsm = 1
               Print #1 , "1: " ; At_loop
               Print #1 , "2: " ; At_resp
               Print #1 , "3: " ; At_data
               Print #1 , "4: " ; At_data1
            End If
            If Ptrrd > 1 Then
               Print #1 , "BUFRD"
               For Tmpw = 1 To Ptrrd
                  Print #1 , Chr(bufrd(tmpw));
               Next
               Print #1,
               Print #1 , "%FINBUFRD%"
               Tmpw = Instr(bufferrx , "Status: 200 OK")
               If Tmpw > 0 Then
                  Print #1 , "TS WR OK"
                  Cntrerrts = 0
               Else
                  Incr Cntrerrts
                  If Cntrerrts = 6 Then
                     Set Iniapagarmdm

                  End If
               End If
            End If
            Ptrrd = 0
            Reset Newrd
            Set Txok
            Cntracterr = 0
            Cntrerrtx = 0
            Reset T1ini
            Estado_led = 1
         Else
            Print #1 , "Error TX"
            Incr Cntrerrtx
         End If
      Else
         Print #1 , "No se pudo establecer conexión con el servidor"
         Print #1 , "Reset sesion IP"
         Reset T1tout
         Incr Cntrerrtx
         T1cntr = 0
         T1wait = 10
         Set T1ini
         Reset Shutok
         Reset Cifsrok
         Do
            Call Atcmdsnd( "AT+CIPSHUT" , 100)
            Call Espera(100)
         Loop Until Shutok = 1 Or T1tout = 1    'Or Dcdgsm = 1
         Print #1 , "1: " ; At_loop
         Print #1 , "2: " ; At_resp
         Print #1 , "3: " ; At_data
         Print #1 , "4: " ; At_data1
         Estado_led = 2
      End If
   End If
End Sub

'*******************************************************************************
' Valores por defecto
'*******************************************************************************
Sub Defaultvalues()
   'Apneep = "watching.porta.com.ec"
   Apneep = "kitem2m.movistar.com.ec"
   'Iptxeep = "api.thingspeak.com"
   Iptxeep = "iot.watching.com.ec"
   Prtotxeep = "3000"
   Cntrinieep = 0
   Cntrinimdceep = 0
   For J = 1 To Ednum
      Tmpstr52 = "ED" + Str(j)
      Ednameeep(j) = Tmpstr52
      Tredmaseep(j) = 10
      Tredmenoseep(j) = 10
      Tmpstr52 = Lookupstr(j , Tbl_repmas)
      Txtrepmaseep(j) = Tmpstr52
      Tmpstr52 = Lookupstr(j , Tbl_repmenos)
      Txtrepmenoseep(j) = Tmpstr52
   Next
   Edpoleep = 0
   Edhabeep = 0

   For Tmpb = 1 To Numtxaut
      Autoval(tmpb) = 300
      Autovaleep(tmpb) = Autoval(tmpb)
      Offset(tmpb) = 0
      Offseteep(tmpb) = 0
   Next


   Autovaleep(2) = 10
   Offseteep(2) = 0

   Autovaleep(3) = 43200
   Offseteep(3) = 30
   Adckeep = 0.009736415
   Cntrsmseep = 0
   Set Inivariables
End Sub

'*******************************************************************************
' Modo avion
'*******************************************************************************


Sub Tstavion()
   Print #1 , "Paso a modo avion"
   Reset Modoavion
   Tmpstr52 = "AT+CFUN=4"
   Call Atcmdsnd(tmpstr52 , 10)
   Print #1 , "1: " ; At_loop
   Print #1 , "2: " ; At_resp
   Print #1 , "3: " ; At_data
   Print #1 , "4: " ; At_data1
   Tmpb = 0
   Do
      Incr Tmpb
      Print #1 , "Test modo avion " ; Tmpb
      Tmpstr52 = "AT+CFUN?"
      Call Atcmdsnd(tmpstr52 , 10)
      Print #1 , "1: " ; At_loop
      Print #1 , "2: " ; At_resp
      Print #1 , "3: " ; At_data
      Print #1 , "4: " ; At_data1
      Call Espera(200)
   Loop Until Tmpb = 10 Or Modoavion = 1

   If Modoavion = 1 Then
      Print #1 , "ESPERA 4 SEG en modo avion"
      Call Espera(400)
      Print #1 , "Paso a modo NORMAL"
      Reset Modonormal
      Tmpstr52 = "AT+CFUN=1"
      Call Atcmdsnd(tmpstr52 , 10)
      Print #1 , "1: " ; At_loop
      Print #1 , "2: " ; At_resp
      Print #1 , "3: " ; At_data
      Print #1 , "4: " ; At_data1
      Tmpb = 0
      Do
         Incr Tmpb
         Print #1 , "Test modo normal " ; Tmpb
         Tmpstr52 = "AT+CFUN?"
         Call Atcmdsnd(tmpstr52 , 10)
         Print #1 , "1: " ; At_loop
         Print #1 , "2: " ; At_resp
         Print #1 , "3: " ; At_data
         Print #1 , "4: " ; At_data1
         Call Espera(200)
      Loop Until Tmpb = 10 Or Modonormal = 1
      If Modonormal = 1 Then
         Print #1 , "Cambio a modo normal OK"
      Else
         Set Inimodem
         Print #1 , "Apago MDM por no resp Modo avion"
         Set Pwrsim908
         Reset Watchdog
         Call Espera(100)
         Reset Pwrsim908
         Print #1 , "ESPERE 3 SEG"
         Call Espera(300)
      End If
   Else
      Set Inimodem
      Print #1 , "Apago MDM por no resp Modo avion"
      Set Pwrsim908
      Reset Watchdog
      Call Espera(100)
      Reset Pwrsim908
      Print #1 , "ESPERE 3 SEG"
      Call Espera(300)
   End If
End Sub


'Subrutina para leer ED
'*******************************************************************************
'Subrutina para leer ED
'*******************************************************************************
Sub Leered()
   For Jed = 1 To Ednum
      If Edstaant(jed) <> Edsta(jed) Then
         Print #1 , "ED" ; Jed ; " de " ; Edstaant(jed) ; " a " ; Edsta(jed)
         Edstaant(jed) = Edsta(jed)
      End If
   Next

   For Tmped = 0 To Ednum_1
      Jed = Tmped + 1
      If Edhab.tmped = 1 Then
         If Evmas.tmped = 1 Then
            Reset Evmas.tmped
            Print #1 , "Nuevo evento Mas en ED" ; Jed
            Tmpl = Syssec()
            Horaed = Time$
            Fechaed = Date$
            Gwsproc = "$ED" + Str(tmped) + "," + Fechaed + "," + Horaed + "," + Keyapi
            If Samplerdy = 1 Then
               Gwsproc = Gwsproc + "," + Fusing(adcvali , "#.##")
            Else
               Gwsproc = Gwsproc + ","
            End If

            Tmpbit = Ed0 Xor Edpol.0
            Gwsproc = Gwsproc + "," + Str(tmpbit)
            Tmpbit = Ed1 Xor Edpol.1
            Gwsproc = Gwsproc + "," + Str(tmpbit)
            Tmpbit = Ed2 Xor Edpol.2
            Gwsproc = Gwsproc + "," + Str(tmpbit)
            Tmpbit = Ed3 Xor Edpol.3
            Gwsproc = Gwsproc + "," + Str(tmpbit)
            Gwsproc = Gwsproc + "," + Str(cntrini)
            Gwsproc = Gwsproc + "," + Str(cntrinimdc) + "," + Str(cntrsms)
            Print #1 , Gwsproc
            Ptrbuf = Ptrbuf Mod Numbuf
            Incr Ptrbuf
            Mdcbuf(ptrbuf) = Gwsproc
            Txbuf(ptrbuf) = 1
            Print #1 , "PTRBUF=" ; Ptrbuf
         End If
         If Evmenos.tmped = 1 Then
            Reset Evmenos.tmped
            Print #1 , "Nuevo evento Menos en ED" ; Jed
            Tmpl = Syssec()
            Horaed = Time$
            Fechaed = Date$
            Gwsproc = "$ED" + Str(tmped) + "," + Fechaed + "," + Horaed + "," + Keyapi
            If Samplerdy = 1 Then
               Gwsproc = Gwsproc + "," + Fusing(adcvali , "#.##")
            Else
               Gwsproc = Gwsproc + ","
            End If
            Tmpbit = Ed0 Xor Edpol.0
            Gwsproc = Gwsproc + "," + Str(tmpbit)
            Tmpbit = Ed1 Xor Edpol.1
            Gwsproc = Gwsproc + "," + Str(tmpbit)
            Tmpbit = Ed2 Xor Edpol.2
            Gwsproc = Gwsproc + "," + Str(tmpbit)
            Tmpbit = Ed3 Xor Edpol.3
            Gwsproc = Gwsproc + "," + Str(tmpbit)
            Gwsproc = Gwsproc + "," + Str(cntrini)
            Gwsproc = Gwsproc + "," + Str(cntrinimdc) + "," + Str(cntrsms)
            Print #1 , Gwsproc
            Ptrbuf = Ptrbuf Mod Numbuf
            Incr Ptrbuf
            Mdcbuf(ptrbuf) = Gwsproc
            Txbuf(ptrbuf) = 1
            Print #1 , "PTRBUF=" ; Ptrbuf
         End If
      End If
   Next

End Sub


'*****************************************************************************
'---------routines I2C for  RTC DS3231----------------------------------------

'*****************************************************************************
Sub Getdatetimeds3231()
  I2cstart                                                  ' Generate start code
  If Err = 0 Then
     I2cwbyte Ds3231w                                       ' send address
     I2cwbyte 0                                             ' start address in 1307
     I2cstart                                               ' Generate start code
     If Err = 0 Then
        I2cwbyte Ds3231r                                    ' send address
        I2crbyte _sec , Ack
        I2crbyte _min , Ack       ' MINUTES
        I2crbyte _hour , Ack       ' Hours
        I2crbyte Dow , Ack                                        ' Day of Week
        I2crbyte _day , Ack                                       ' Day of Month
        I2crbyte _month , Ack       ' Month of Year
        I2crbyte _year , Nack       ' Year
        I2cstop
        If Err <> 0 Then
         Call Error(15)
        Else
           _sec = Makedec(_sec) : _min = Makedec(_min) : _hour = Makedec(_hour)
           _day = Makedec(_day) : _month = Makedec(_month) : _year = Makedec(_year)
        End If
     Else
      Print #1 , "No se encontro DS3231 en Getdatetime 2"
     End If
  Else
   Print #1 , "No se encontro DS3231 en Getdatetime 1"
  End If
End Sub
'-----------------------
Sub Setdateds3231()
  I2cstart                                                  ' Generate start code
  If Err = 0 Then
     _day = Makebcd(_day) : _month = Makebcd(_month) : _year = Makebcd(_year)
     I2cwbyte Ds3231w                                       ' send address
     I2cwbyte 3                                                ' starting address in 1307
     I2cwbyte Dow
     I2cwbyte _day                                             ' Send Data to day
     I2cwbyte _month       ' Month
     I2cwbyte _year       ' Year
     I2cstop
     If Err <> 0 Then call Error(15)
  Else
   Print #1 , "No se encontro DS3231 en Setdate"
  End If
end sub
'-----------------------
Sub Settimeds3231()
  I2cstart                                                  ' Generate start code
  If Err = 0 Then
     _sec = Makebcd(_sec) : _min = Makebcd(_min) : _hour = Makebcd(_hour)
     I2cwbyte Ds3231w                                       ' send address
     I2cwbyte 0                                                ' starting address in 1307
     I2cwbyte _sec                                             ' Send Data to SECONDS
     I2cwbyte _min                                             ' MINUTES
     I2cwbyte _hour                                         ' Hours
     I2cstop
     If Err <> 0 Then call Error(15)
  Else
   Print #1 , "No se encontro DS3231 en Settime"
  End If
 end sub
 '----------------------

 '********définition des erreurs***********************************************
Sub Error(byval genre As Byte )
Local Mes_error As String * 20
Select Case Genre
   Case 1
   Mes_error = " Reset  "
   Case 2
   Mes_error = " DFH "
   Case 3
   Mes_error = "set params  "
   Case 4
   Mes_error = "start "
  Case 5
   Mes_error = "Hardstop "
   Case 6
   Mes_error = "Status "
   Case 7
   Mes_error = "Getposition "
   Case 8
   Mes_error = "pas de module"
   Case 9
   Mes_error = "9xx"
   Case 10
   Mes_error = "10xx"
   Case 11
   Mes_error = "11xx"
   Case 12
   Mes_error = "12xx"
   Case 13
   Mes_error = "13xx"
   Case 14
   Mes_error = "ecriture clock"
   Case 15
   Mes_error = "lecture clock"
   Case Else
    Mes_error = "Autre erreur"
End Select
'Cls
Print #1 , "error=" ; Mes_error                             '; Adr_ax
'If Strerr <> "" Then
'   Locate 2 , 1 : Lcd Strerr
'End If
'Stop

End Sub


'*******************************************************************************
'Subrutina para leer ADC
'*******************************************************************************
Sub Leeradc()
   Tmpwadc = Getadc(2)                                      'Valor instantaneo
   'Print #1 , "ADCHEX=" ; Tmpwadc
   Adccntri = Adccntri + Tmpwadc
   Incr Cntrsmpli
   Cntrsmpli = Cntrsmpli Mod 4
   If Cntrsmpli = 0 Then
       Adcvali = Adccntri * Adck
       Adcvali = Adcvali + 0.3
       Adccntri = 0
       Set Samplerdy
   End If
End Sub

Sub Proced()
   Tmpl = Tbl_timeaut(1)
   Fechaed = Date$
'   Horaed = Time(tmpl)
   Horaed = Time$
   Gwsproc = "$EDSTA" + "," + Fechaed + "," + Horaed + "," + Keyapi
   If Samplerdy = 1 Then
      Gwsproc = Gwsproc + "," + Fusing(adcvali , "#.##")
   Else
      Gwsproc = Gwsproc + ","
   End If
   Tmpbit = Ed0 Xor Edpol.0
   Gwsproc = Gwsproc + "," + Str(tmpbit)
   'Gwsproc = Gwsproc + "," + Str(cntr_on(1))
   Tmpbit = Ed1 Xor Edpol.1
   Gwsproc = Gwsproc + "," + Str(tmpbit)
   'Gwsproc = Gwsproc + "," + Str(cntr_on(2))
   Tmpbit = Ed2 Xor Edpol.2
   Gwsproc = Gwsproc + "," + Str(tmpbit)
   'Gwsproc = Gwsproc + "," + Str(cntr_off(1))
   Tmpbit = Ed3 Xor Edpol.3
   Gwsproc = Gwsproc + "," + Str(tmpbit)
   'Gwsproc = Gwsproc + "," + Str(cntr_off(2))
   Gwsproc = Gwsproc + "," + Str(cntrini)
   Gwsproc = Gwsproc + "," + Str(cntrinimdc) + "," + Str(cntrsms)
   Print #1 , Gwsproc
End Sub

Sub Apagarmdm()
   Tmpb = 0
   Reset Mdmoff
   Do
      Incr Tmpb
      Print #1 , "Intento APAGAR mdm " ; Tmpb
      Set Pwrsim908
      Reset Watchdog
      Call Espera(100)
      Reset Pwrsim908
      Call Espera(300)
      Reset T1tout
      T1cntr = 0
      T1wait = 10
      Set T1ini
      Do

      Loop Until Pwrctl = 0 Or T1tout = 1 Or Mdmoff = 1
   Loop Until Pwrctl = 0 Or Tmpb = 5 Or Mdmoff = 1
   Print #1 , "MDM OFF"


End Sub

Sub Leertc()
Print #1 , "Leer RTC DS3231"
Call Getdatetimeds3231()
If Err = 0 Then
   Tmpl = Syssec()
   Print #1 , "RTC Hora=" ; Time(tmpl) ; ",Fecha=" ; Date(tmpl)
   Print #1 , "RTC Hora=" ; Time$ ; ",Fecha=" ; Date$
   Print #1 , Date$
   Fechaed = Date$
   Print#1 , Fechaed
   Tmpl2 = Syssec()
   If Tmpl2 > 598798055 Then
      Print #1 , "Hora valida, no es necesario ACTCLK"
      Estado_led = 7
   End If
Else
   Print # 1 , "ERROR CLK"
   Estado_led = 3
End If

End Sub

'*******************************************************************************
'TABLA DE DATOS
'*******************************************************************************

Tbl_msgat:
Data "PWR GPS"                                              '0
Data "RST GPS"                                              '1
Data "Contexto GPRS"                                        '2
Data "APN GPRS"                                             '3
Data "Linea DCD"                                            '4
Data "Network time"                                         '5
Data "Codigo no sol. CREG"                                  '6
Data "Codigo no sol. CGREG"                                 '7
Data "IP HEADER"                                            '8


Tbl_cmdat:
Data "AT+CGPSPWR=1"                                         '0
Data "AT+CGPSRST=1"                                         '1
Data "AT+SAPBR=3,1,{034}Contype{034},{034}GPRS{034}"        '2
Data "AT+SAPBR=3,1,{034}APN{034},{034}"                     '3 AT+SAPBR=3,1,"APN","watching.porta.com.ec"
Data "AT&C1"                                                '4
Data "AT+CLTS=1"                                            '5
Data "AT+CREG=1"                                            '6
Data "AT+CGREG=1"                                           '7
Data "AT+CIPHEAD=1"                                         '8


'******************************************************************************
Tbl_cmdfin:
Data ""                                                     '0
Data ""                                                     '1
Data ""                                                     '2
Data "{034}"                                                '3
Data ""                                                     '4
Data ""                                                     '5
Data ""                                                     '6
Data ""                                                     '7
Data ""                                                     '8


Tbl_err:
Data "OK"                                                   '0
Data "Comando no reconocido"                                '1
Data "Longitud comando no valida"                           '2
Data "Numero de usuario no valido"                          '3
Data "Numero de parametros invalido"                        '4
Data "Error longitud parametro 1"                           '5
Data "Error longitud parametro 2"                           '6
Data "Parametro no valido"                                  '7
Data "ERROR8"                                               '8
Data "ERROR SD. Intente de nuevo"                           '9

Tbl_repmas:
Data "1"                                                    'dummy data
Data "Reporte Mas ED1"
Data "Reporte Mas ED2"
Data "Reporte Mas ED3"
Data "Reporte Mas ED4"


Tbl_repmenos:
Data "0"                                                    'dummy data
Data "Reporte Menos ED1"
Data "Reporte Menos ED2"
Data "Reporte Menos ED3"
Data "Reporte Menos ED4"

Tabla_estado:
Data &B00000000000000000000000000000000&                    'Estado 0
Data &B00000000000000000000000000000011&                    'Estado 1
Data &B00000000000000000000000000110011&                    'Estado 2
Data &B00000000000000000000001100110011&                    'Estado 3
Data &B00000000000000000011001100110011&                    'Estado 4
Data &B00000000000000110011001100110011&                    'Estado 5
Data &B00000000000011001100000000110011&                    'Estado 6
Data &B00001111111111110000111111111111&                    'Estado 7
Data &B01010101010101010101010101010101&                    'Estado 8
Data &B00110011001100110011001100110011&                    'Estado 9
Data &B01110111011101110111011101110111&                    'Estado 10
Data &B11111111111111000000000000001100&                    'Estado 11
Data &B11111111111111000000000011001100&                    'Estado 12
Data &B11111111111111000000110011001100&                    'Estado 13
Data &B11111111111111001100110011001100&                    'Estado 14
Data &B11111111111111000000000000001100&                    'Estado 15
Data &B11111111111111111111111111110000&                    'Estado 16



Loaded_arch: