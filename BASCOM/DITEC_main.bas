'Main.bas
'
'                 WATCHING Soluciones Tecnológicas
'                    Fernando Vásquez - 25.06.15
'
' Programa para almacenar los datos que se reciben por el puerto serial a una
' memoria SD
'


$version 0 , 1 , 158
$regfile = "m1284pdef.dat"
$crystal = 7372800
$baud = 9600
$baud1 = 57600


$hwstack = 256
$swstack = 256
$framesize = 256

$projecttime = 19


'CONSTANTES
Const Numrd = 1000
Const Tryon = 5
Const Trysimon = 15
Const Numcmd = 9
Const Numbuf = 16

Const Ednum = 2
Const Ednum_1 = Ednum - 1
Const Ednum_masuno = Ednum + 1

Const Numtxaut = 3                                          ' 1 Mainboard, 2 No usado, 3 ACTCLK, 4-11 es T1-8
Const Numtxaut_mas_uno = Numtxaut + 1

Const Tsampling = 5

'RTC
Const Ds3231r = &B11010001                                  'DS3231 is very similar to DS1307 but it include a precise crystal
Const Ds3231w = &B11010000

Const Tpoll = 20

'Configuracion de entradas/salidas
Led1 Alias Portc.0                                          'LED ROJO
Config Led1 = Output

Led2 Alias Portc.1                                          'LED ROJO
Config Led2 = Output
Set Led2

'ENTRADAS
Ed0 Alias Pinb.0
Ed1 Alias Pinb.1
Ed2 Alias Pina.0
Ed3 Alias Pina.1

Config Ed0 = Input
Config Ed1 = Input
Config Ed2 = Input
Config Ed3 = Input

Set Portb.0
Set Portb.1
Set Porta.0
Set Porta.1


'SIM908
Pwrsim908 Alias Portb.3
Reset Pwrsim908
Config Pwrsim908 = Output                                   'SIM908 Power control

Pwrctl Alias Pina.5
Config Pwrctl = Input                                       'SIM908 Power
'Set Porta.0


'Configuración de Interrupciones
'TIMER0
Config Timer0 = Timer , Prescale = 1024                     'Ints a 100Hz si Timer0=184
On Timer0 Int_timer0
Enable Timer0
Start Timer0

'TIMER1
Config Timer1 = Timer , Prescale = 256                      'Ints a 100Hz si Timer0=184
On Timer1 Int_timer1
Enable Timer1
Start Timer1

' Puerto serial 1
Open "com1:" For Binary As #1
On Urxc At_ser1
Enable Urxc

On Urxc1 At_serialint
Enable Urxc1
Open "com2:" For Binary As #2

Dim Dummy As Byte
Config Date = Dmy , Separator = /                           ' ANSI-Format, Timer2
Config Clock = soft , Gosub = Sectic

Config Sda = Portb.5
Config Scl = Portb.6
Config I2cdelay = 10
I2cinit

'Set Portb.5
'Set Portb.6

'ADC
Config Adc = Single , Prescaler = Auto                      ', Reference = Internal
Enable Adc
Start Adc

Enable Interrupts

'*******************************************************************************
'* Archivos incluidos
'*******************************************************************************
$include "DITEC_archivos.bas"



'Programa principal
Call Leertc()

Do
   Call Inivar()
   Call Inimdm()

   Print #1 , "MAIN"
   set Iniauto.1 'Para tener reg GSM

   Do

      If Sernew = 1 Then                                       'DATOS SERIAL 1
         Reset Sernew
         Print #1 , "SER1=" ; Serproc
         Call Procser()
      End If

      Call Leered()

      If Iniadc = 1 Then
         Reset Iniadc
         Call Leeradc()
          If Enabug = 1 Then
            Print #1 , "ADC1=" ; Fusing(adcvali , "#.##")
          End If
      End If

      If Newseg = 1 Then
         Reset Newseg
         If Newcreg = 1 Then
            'Reset Newcreg
            Call Vergsm()
            Reset Newcreg
         End If
         If Newcgreg = 1 Then
            'Reset Newcgreg
            Call Vergprs()
            Reset Newcgreg
         Else
            Incr Cntrcgreg
            Cntrcgreg = Cntrcgreg
            If Cntrcgreg = 0 Then
               Call Vergprs()
            End If
         End If
      End If

      If Newsms = 1 Then
         Reset Newsms
         Print #1 , Atsnd ; ", FONO:" ; Fono
         Call Vergsm()
          If Gsmok = 1 Then
            Call Smstx()
         Else
            Print #1 , "NO REG GSM"
         End If
      End If

      If Inileer = 1 Then
         Reset Inileer
         'Set Cmdsms
         Call Procsms()
         Serproc = Atsnd
         Call Procser()
         If Cmderr <> 2 Then
            Fono = Tf_num1
            If Gsmok = 1 Then
               Call Smstx()
            Else
               Print #1 , "NO REG GSM"
            End If
         End If
      End If

      If Inipoll = 1 Then
      '   Reset Inipoll
         Ptrtx = Ptrtx Mod Numbuf
         Incr Ptrtx
      End If

      If Enabug = 2 Then
         'Print #1 , "PTRTX=" ; Ptrtx
         If Cntrpollant <> Cntrpoll Then
            Print #1 , "Cntrpoll=" ; Cntrpoll
            Cntrpollant = Cntrpoll
         End If
      End If

      If Txbuf(ptrtx) = 1 Then
         Print #1 , "Ptrtx=" ; Ptrtx
         Txbuf(ptrtx) = 0
         Print #1 , "GWS=" ; Mdcbuf(ptrtx)
         Gwsproc = Mdcbuf(ptrtx)
         Call Procdaa()
         If Newtx = 1 Then
            Reset Newtx
            Txok = 0
            Cntrtx = 0
            Do
               Call Vergprs()
               If Gprsok = 1 Then
                  Call Txdata(1)
               End If
               Incr Cntrtx
            Loop Until Txok = 1 Or Cntrtx = 2
            If Txok = 1 Then
               Txbuf(ptrtx) = 0
            End If
            Reset Inipoll
            Set Inicpoll
         End If
      End If


      If Newtx = 1 Then
         Reset Newtx
         Print #1 , "New TX GPRS"
         Txok = 0
         Cntrtx = 0
         Do
            Call Vergprs()
            If Gprsok = 1 Then
               Call Txdata(0)
            End If
            Incr Cntrtx
         Loop Until Txok = 1 Or Cntrtx = 2
      End If

      If Inimodem = 1 Then
         Reset Inimodem
         Call Inimdm()
      End If

      If Fallagprs = 1 Then
         Reset Fallagprs
         Print #1 , "<+PDP: DEACT> detectado"
      End If

      If Iniauto.0 = 1 Then
         Reset Iniauto.0
         Incr Cntrdata
         Print #1 , "TX PER"
         Call Proced()
         Call Procdaa()
         If Newtx = 1 Then
            Reset Newtx
            Txok = 0
            Cntrtx = 0
            Do
               Call Vergprs()
               If Gprsok = 1 Then
                  Call Txdata(1)
               End If
               Incr Cntrtx
            Loop Until Txok = 1 Or Cntrtx = 2
            If Txok = 1 Then
               Txbuf(ptrtx) = 0
            End If
            Reset Inipoll
            Set Inicpoll
         End If

      End If

      If Inivariables = 1 Then
         Reset Inivariables
         Call Inivar()
      End If

      If Iniavion = 1 Then
         Reset Iniavion
         Call Tstavion()
      End If

      If Iniauto.1 = 1 Then
         Print #1 , "AUT2"
         Reset Iniauto.1
         Set Newcreg
         Set Newcgreg
      End If

      If Iniauto.2 = 1 Then
         Print #1 , "AUT3"
         Reset Iniauto.2
         Call Leertc()
      End If

      If Iniapagarmdm = 1 Then
         Reset Iniapagarmdm
         Call Apagarmdm()
      End If

   Loop Until Resets = 1
Loop