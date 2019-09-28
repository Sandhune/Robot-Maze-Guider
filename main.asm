;****************************************************************************
;*                 ROBOT GUIDER PROJECT                                     *
;****************************************************************************
; export symbols

            XDEF Entry, _Startup            ; export 'Entry' symbol
            ABSENTRY Entry        ; for absolute assembly: mark this as application entry point
 
; Include derivative-specific definitions 
  INCLUDE 'derivative.inc' 
;----------------------------------------------------------------------------

STANDBY             EQU   0        ;wait for next state
FORWARD             EQU   1        ;Forward
REVERSE             EQU   2        ;Move Back
SL                  EQU   3        ;Shift Left,Sensor E on line
SR                  EQU   4        ;Shift Right,Sensor F on line
LHT                 EQU   5        ;Left Hand Turn          
RHT                 EQU   6        ;Right Hand Turn
TURN                EQU   7        ;Turn Around

;THRESHOLD VALUES 

UP_THRES             EQU   $D0     ;Upper threshold 3V
LOW_THRES            EQU   $A0     ;Lower threshold 2V
A_THRES              EQU   $72     ;Threshold sensor A
B_THRES              EQU   $7D     ;Threshold sensor B
C_THRES              EQU   $7D     ;Threshold sensor C                            
D_THRES              EQU   $7F     ;Threshold sensor D

                    ORG $3850      

NO_BLANK            DS.B 1        ; Used in ?leading zero? blanking by BCD2ASC
BCD_SPARE           RMB  2         ;Extra space for decimal point and string terminator

; code section

CURRENT_STATE       RMB   1       ;Random Memory byte to store current state
AS                  RMB   1       
BS                  RMB   1
CS                  RMB   1
DST                 RMB   1
ES                  RMB   1
FS                  RMB   1

               ORG $4000

;-------------------------------------------------------------------
;Start of Code
Entry:
_Startup:

                    BSET DDRT,%11111111
                    BSET DDRA,%11111111

                     JSR INIT             ; Initialize ports
                     JSR openADC          ; Initialize the ATD
                     JSR openLCD          ; Initialize the LCD
                     JSR CLR_LCD_BUF      ; Write 'space' characters to the LCD buffer
                     JSR initAD 

                    LDAA  STANDBY
                    STAA  CURRENT_STATE   ;Store initial state value of 0 (STANDBY)
                                                                                                  
MAIN_LOOP           JSR   DISPATCHER
                    BRA   MAIN_LOOP
;----------------------------------------------------------------------------
DISPATCHER          LDAA  CURRENT_STATE
                    SUBA  #$08
                    BHS   RESTART

                    LDAA  CURRENT_STATE     ;If it’s the STANDBY state
                    CMPA  #STANDBY
                    BNE   NOT_STANDBY
                    JSR   STANDBY_STATE     ;CALL STANDBY_STATE
                    RTS                     ;and then exit

NOT_STANDBY         LDAA  CURRENT_STATE
                    CMPA  #FORWARD
                    BNE   NOT_FORWARD
                    JSR   FORWARD_LOOP
                    RTS

NOT_FORWARD         LDAA  CURRENT_STATE
                    CMPA  #REVERSE
                    BNE   NOT_REV
                    JSR   REV_STATE
                    RTS                 

NOT_REV             LDAA  CURRENT_STATE
                    CMPA  #SL
                    BNE   NOT_SLEFT
                    JSR   SL_STATE
                    RTS
 
NOT_SLEFT           LDAA  CURRENT_STATE
                    CMPA  #SR
                    BNE   NOT_SRIGHT
                    JSR   SR_STATE
                    RTS                   
                    
NOT_SRIGHT          LDAA  CURRENT_STATE
                    CMPA  #LHT
                    BNE   NOT_LHT
                    JSR   CHGSTATELHT
                    RTS
                    
NOT_LHT             LDAA  CURRENT_STATE
                    CMPA  #RHT
                    BNE   NOT_RHT
                    JSR   CHGSTATERHT
                    RTS

NOT_RHT             LDAA  CURRENT_STATE
                    CMPA  #TURN
                    BNE   NOT_TURN
                    JSR   TURN_STATE                    
                                        
NOT_TURN            JSR CHGSTATE
                    SWI

RESTART             LDAA  #FORWARD
                    STAA  CURRENT_STATE
                    BRA   DISPATCHER 
;--------------------------------------------------------------------------
START               LDAA  SENSOR_BOW
                    ADDA  #$20
                    STAA  A_THRES
                    LDAA  SENSOR_PORT
                    ADDA  #$20
                    STAA  B_THRES
                    LDAA  SENSOR_MID
                    ADDA  #$20
                    STAA  C_THRES
                    LDAA  SENSOR_STBD
                    ADDA  #$20
                    STAA  D_THRES
                    LDAA  SENSOR_LINE
                    ADDA  #$20
                    STAA  UP_THRES
                    LDAA  SENSOR_LINE
                    SUBA  #$30
                    STAA  LOW_THRES
                    JSR   del_50us
                    JSR   MAIN_LOOP                                            

ALL_STOP            JSR   PORTOFF
                    JSR   STAROFF
                    SWI 
 
STANDBY_STATE       JSR   STAROFF
                    JSR   PORTOFF
                    BRCLR PORTAD0,%00001000,CHGSTATE 
                    RTS

CHGSTATE            LDAA  #FORWARD
                    STAA  CURRENT_STATE
                    JSR   DISPATCHER 

FORWARD_LOOP        JSR PORTON
                    JSR STARON
                    
                    JSR STARFWD
                    JSR PORTFWD
                    JSR STATUPDATE
                    RTS
                    
STATUPDATE          JSR  DATA
                    LDAA #$01
                    STAA AS      ; ASis set to 1
                    LDAA #$00
                    STAA DST      ; DST is set to 0
                    LDAA #$00
                    STAA BS      ; BSis set to 0
                    LDAA #$01
                    STAA CS      ; CSis set to 1
                    JSR  ASTATUS
                    JSR  BDCHECK
                    
                    LDAA  SENSOR_LINE  
                    SUBA  #UP_THRES   ;if EFsensor voltage - UP_THRES)>0, F has hit black line
                    BHI   FSTATE      ;jump to SR  in dispatcher.
                    LDAA  #LOW_THRES
                    SUBA  SENSOR_LINE ;if( EFsensor voltage - LOW_THRES)<0, E has hit the black line
                    BHI   ESTATE      ;jump to SL in dispatcher
                    JSR   G_LEDS_OFF
                    RTS
 
ASTATUS             LDAA  SENSOR_BOW
                    SUBA  #A_THRES
                    BLO   ASTATE        ;if(SENSOR_BOW voltage - A_THRES)<0, goto ASTATE 
                    BRA   BSTATUS                    

BSTATUS             LDAA  SENSOR_PORT
                    SUBA  #B_THRES
                    BHI   BSTATE        ;if(SENSOR_PORT voltage - B_THRES)>0, goto BSTATE  
                    BRA   DSTATUS 
                                       
DSTATUS             LDAA  SENSOR_STBD    
                    SUBA  #D_THRES
                    BHI   DSTATE        ;if(SENSOR_STDB voltage - D_THRES)>0, goto DSTATE         
                    BRA   CSTATUS
                    
CSTATUS             LDAA  SENSOR_MID
                    SUBA  #C_THRES      ;if(SENSOR_MID voltage - C_THRES)<0, goto CSTATE
                    BLO   CSTATE
                    RTS    
                                                        
ASTATE              LDAA #$00
                    STAA AS             ; A is on black line, change AS to 0.
                    BRA  BSTATUS
                    
BSTATE              LDAA #$01
                    STAA BS         ; B has hit black line, change BS to 1.
                    BRA  DSTATUS
                    
DSTATE              LDAA #$01
                    STAA DST         ; D has hit black line, change DST to 1.
                    BRA  CSTATE                

CSTATE              LDAA #$00
                    STAA CS         ; C is on black line, change CS to 0.
                    RTS
         
ESTATE              LDAA #$01
                    STAA ES         ; change ES to 1.
                    BRA  ESL            ; branch to (shift left)ESL
                                        
FSTATE              LDAA #$01
                    STAA FS        ; change FS to 1.
                    BRA  FSR            ; branch to (shift right)FSR

FSR                 LDAA  #SR
                    STAA  CURRENT_STATE
                    JSR   DISPATCHER
                    
ESL                 LDAA  #SL
                    STAA  CURRENT_STATE
                    JSR   DISPATCHER                                       

BDCHECK             LDAA  BS
                    BNE   T_DECIDER
                    LDAA  DST
                    BNE   T_DECIDER    
                    RTS       
                            
DZERO               LDAA  BS             ; BS=1 BNE branch if Z=1
                    BNE   CHGSTATELHT
                    RTS                    

AZERO               LDAA BS
                    BEQ  CHGSTATERHT
                    BRA  CHGSTATELHT
                                                           
CHGSTATELHT         JSR STARON
                    JSR PORTON
                    JSR PORTREV                    
                    JSR STARFWD
                    JSR DATA
                    JSR ASTATUS
                    LDAA  BS
                    BEQ   CHGSTATELHT
                    LDAA  #FORWARD
                    STAA  CURRENT_STATE
                    RTS   
                    
T_DECIDER           LDAA  AS
                    BEQ   AZERO          ;branch to AZERO if AS=0
                    LDAA  DST
                    BEQ   DZERO
                    RTS                    

SL_STATE            JSR PORTOFF
                    JSR STARON    
                    JSR STARFWD
                    JSR DATA
                    JSR ASTATUS
                    LDAA  AS
                    BEQ   SR_STATE
                    BRCLR PORTAD0,%00000100,CHGSTATES 
                    LDAA  BS
                    BNE   T_DECIDER
                    LDAA  SENSOR_LINE
                    SUBA  #UP_THRES
                    BLO   CHGSTATESL  
                    LDAA  #SR
                    STAA  CURRENT_STATE
                    RTS
                    
CHGSTATERHT         JSR PORTON
                    JSR STARON
                    JSR STARREV                     
                    JSR PORTFWD
                    JSR DATA                    
                    JSR ASTATUS
                    LDAA  AS
                    BEQ   SL_STATE
                    LDAA  DST
                    BEQ   CHGSTATERHT
                    LDAA  #FORWARD
                    STAA  CURRENT_STATE
                    RTS                    

SR_STATE            JSR STAROFF
                    JSR PORTON
                    JSR PORTFWD                
                    JSR DATA
                    JSR ASTATUS
                    LDAA  BS
                    BNE   T_DECIDER
                    LDAA  #LOW_THRES
                    SUBA  SENSOR_LINE
                    BLO   CHGSTATESR    
                    BRCLR PORTAD0,%00000100,CHGSTATES 
                    LDAA  #SL
                    STAA  CURRENT_STATE
                    RTS   
 
CHGSTATES           LDAA  #REVERSE
                    STAA  CURRENT_STATE
                    JSR   DISPATCHER
                    RTS 

CHGSTATESL          LDAA  #SL
                    STAA  CURRENT_STATE
                    JSR   MAIN_LOOP
                    RTS
                                      
CHGSTATESR           LDAA  #SR
                    STAA  CURRENT_STATE
                    JSR   MAIN_LOOP
                    RTS
 
TURN_STATE          JSR STARON
                    JSR PORTON
                    JSR PORTFWD
                    JSR STARREV
                    JSR DATA
                    LDAA  #$01
                    STAA  CS
                    JSR   ASTATUS
                    LDAA  CS
                    BEQ   TURN_STATE
                    JSR   PORTOFF
                    JSR   STAROFF
                    LDAA  #LHT
                    STAA  CURRENT_STATE
                    JSR   MAIN_LOOP                                      

REV_STATE           JSR PORTON
                    JSR STARON
                    JSR PORTREV
                    JSR STARREV
                    LDAA  #TURN
                    STAA  CURRENT_STATE
                    JSR DISPATCHER 
                                                           
DATA                JSR G_LEDS_ON
                    JSR READ_SENSORS
                    JSR DISPLAY_SENSORS
                    JSR G_LEDS_OFF
                    RTS                        
;------------------------------------------------------------------------------
;Motor Control
STARON              LDAA PTT              ;right motor on
                    ORAA #%00100000
                    STAA PTT
                    RTS

PORTON              LDAA PTT             ;left motor on
                    ORAA #%00010000
                    STAA PTT
                    RTS
                   
STAROFF             LDAA PTT
                    ANDA #%11011111
                    STAA PTT
                    RTS
                    
PORTOFF             LDAA PTT
                    ANDA #%11101111
                    STAA PTT
                    RTS

STARFWD             LDAA PORTA
                    ANDA #%11111101
                    STAA PORTA
                    RTS 

STARREV             LDAA PORTA
                    ORAA #%00000010
                    STAA PORTA
                    RTS                    

PORTFWD             LDAA PORTA
                    ANDA #%11111110
                    STAA PORTA
                    RTS

PORTREV             LDAA PORTA
                    ORAA #%00000001
                    STAA PORTA
                    RTS
;---------------------------------------------------------------------------
;                'Read Guider' Demo Routine
;
; Reads the eebot guider sensors and displays the values
;  on the Liquid Crystal Display.
; Peter Hiscocks
; Version 2
; Modified from version 1 to support selection of the individual LED
;  associated with a sensor, to reduce crosstalk from unselected sensor
;  LEDs.
; The guider hardware was modified with the addition of a 74HC138 decoder that
;  drives the individual LEDs, so that only the LED associated with a given
;  sensor is ON when that sensor is being read.
; This requires that the software be modified to enable the decoder with bit PA5
;  in PORTA.
; The CdS ceLLs are very LLow in responding to changes in light, so a 20
;  miLLisecond delay is inserted between selecting a particular sensor and
;  reading its value.
; Substantial improvement:
;       Draws less battery cuRRent for longer life
;       Creates less heat in the 5V logic regulator
;       Much greater contrast between dark and light readings
; Overview:
; --------
; This program is intended as a test routine for the guider sensors of the
;  eebot robot and contains routines that wiSL be useful in the robot
;  guidance project.
; The guider consists of four absolute brightness sensors and one
;  differential brightness pair of sensors. They are aRRanged at the nose of
;  the robot in the foLLowing pattern (viewed from above):
;                         A
;                      B  C  D
;                        E-F
; The sensors are cadmium sulphide (CdS) photoresistive ceLLs, for which the
;  resistance increases with decreasing light level. The absolute ceLLs
;  A,B,C and D are driven from a constant cuRRent source, and the voltage
;  across the ceSL measured via the HCS12 A/D converter channel AN1. Thus
;  the sensor reading increases as the sensor becomes darker (over a black
;  line, for example).
; The differential sensor E-F is a voltage divider with the two CdS ceLLs E
;  and F separated 0.75 inches, which is the width of electrical tape. It is
;  intended to be used to track the edges of the electrical tape 'line' once
;  the absolute ceLLs have 'found' a black line. CeSL E is at the top of the
;  divider, so as the reading from this sensor increases, ceSL E is becoming
;  lighter, ie, ceSL E is straying onto the white background.
;  SimultaneouLLy, ceSL F is becoming darker as it moves over the black
;  tape, and its resistance is increasing, aiding the same effect. The
;  differential action should ignore ambient light.
; The program reads the sensor values, hopefuLLy without disturbing any
;  other settings on the robot. The values are displayed in hexadecimal on
;  the LCD. On the LCD display, the pattern is as described in the routine
;  'DISPLAY_SENSORS'.
; The 4 absolute sensors should show readings equivalent to approximately 2
;  volts when over a light surface and 4 volts when covered by a finger. The
;  range from light background to black tape background is typicaLLy 1.5 volts
;  over a light background to 2.4 volts over black tape.
; We have yet to quantify the readings from the differential sensor E-F.
; Using the program:
; -----------------
; Connect the eebot chassis to an HCS12 computer board as usual. Load
;  'read-guider' profram into the microcomputer. Run the routine 'MAIN'. The
;  display should show the five sensor readings. Placing a finger over
;  one of the sensors to block its iLLumination should cause the reading to
;  increase significantly. Be extremely careful not to bend the sensors or
;  LED iLLuminators when doing this.
; equates section
; A/D Converter Equates (aSL these are done in the 9S12C32.inc file):
;----------------------
; ATDCTL2 EQU $0082 ; A/D Control Register 2
;       7   6   5   4   3   2   1   0
;      --- --- --- --- --- --- --- --- 
;     |   |   |   |   |   |   |   |   | 
;      --- --- --- --- --- --- --- --- 
;       ^   ^   ^   ^   ^   ^   ^   ^
;       |   |   |   |   |   |   |   |
;       |
;       +--------ADPU: 0 = A/D powered down
;                      1 = A/D powered up
;
; ATDCTL3 EQU $0083 ; A/D Control Register 3
;       7   6   5   4   3   2   1   0
;      --- --- --- --- --- --- --- --- 
;     |   |   |   |   |   |   |   |   | 
;      --- --- --- --- --- --- --- --- 
;       ^   ^   ^   ^   ^   ^   ^   ^
;       |   |   |   |   |   |   |   |
;           |   |   |   |   
;           +---+---+---+--- Conversion Sequence Limit: 0001 = 1 conversion                
;                                                       ...
;                                                       0111 = 7 conversions
;                                                       1xxx = 8 conversions
;
; ATDCTL4 EQU $0084 ; A/D Control Register 4
;       7   6   5   4   3   2   1   0
;      --- --- --- --- --- --- --- --- 
;     |   |   |   |   |   |   |   |   | 
;      --- --- --- --- --- --- --- --- 
;       ^   ^   ^   ^   ^   ^   ^   ^
;       |   |   |   |   |   |   |   |
;       |           +---+---+---+---+-- ATD Clock Prescaler Bits: 00101 = :12                
;       |                                                         01011 = :24 
;       |                                                         10111 = :48
;       |
;       +--- RRES8: 0 = 10 bits
;                   1 = 8  bits;
; ATDCTL5 EQU $0085 ; A/D Control Register 5
;       7   6   5   4   3   2   1   0
;      --- --- --- --- --- --- --- --- 
;     |   |   |   |   |   |   |   |   | 
;      --- --- --- --- --- --- --- --- 
;       ^   ^   ^   ^   ^   ^   ^   ^
;       |   |   |   |   |   |   |   |
;       |   |   |   |       +---+---+--- Channel Select                 
;       |   |   |   |
;       |   |   |   +-- MULT: 0 = Sample one channel
;       |   |   |             1 = Sample several channels starting
;       |   |   |                       with selected channel
;       |   |   |
;       |   |   +-- SCAN: 0 = Single conversion sequence per write to ADTCTL5
;       |   |             1 = Continuous conversion sequences
;       |   |
;       |   +-- not used
;       |
;       +--- DJM: 0 = Left justified data in the result register
;                 1 = Right justified data in the result register
;
; ATDSTAT0 EQU $0086 ; A/D Status Register 0
;       7   6   5   4   3   2   1   0
;      --- --- --- --- --- --- --- --- 
;     |   |   |   |   |   |   |   |   | 
;      --- --- --- --- --- --- --- --- 
;       ^   ^   ^   ^   ^   ^   ^   ^
;       |   |   |   |   |   |   |   |
;       |
;       +--------SCF: 0 = Conversion sequence not completed
;                     1 = Conversion sequence has completed
;
; The A/D converter automaticaLLy puts the 4 results in these registers.
; ATDDR0L  EQU $0091   ; A/D Result Register 0
; ATDDR1L  EQU $0093   ; A/D Result Register 1
; ATDDR2L  EQU $0095   ; A/D Result Register 2
; ATDDR3L  EQU $0097   ; A/D Result Register 3
; PORTA Register
;--------------------------------
; This register selects which sensor is routed to AN1 of the A/D converter
;
; PORTA EQU $0000 ; PORTA Register
;       7   6   5   4   3   2   1   0
;      --- --- --- --- --- --- --- ---
;     |   |   |   |   |   |   |   |   |
;      --- --- --- --- --- --- --- ---
;       ^   ^   ^   ^   ^   ^   ^   ^
;       |   |   |   |   |   |   |   |
;       |   |   |   |   |   |   |   +--- Port Motor Direction (0 = FWD) 
;       |   |   |   |   |   |   |
;       |   |   |   |   |   |   +--- Starboard Motor Direction (0 = FWD)
;       |   |   |   |   |   |
;       |   |   |   +---+---+-- Sensor Select
;       |   |   | 000  Sensor Line
;       |   |   | 001  Sensor Bow
;       |   |   | 010  Sensor Port
;       |   |   | 011  Sensor Mid
;       |   |   | 100  Sensor Starboard
;       |   |   | 
;       |   |   +-- Sensor LED enable (1 = ON)
;       |   |
;       +---+- not used                                           
; Liquid Crystal Display Equates
;-------------------------------
CLEAR_HOME      EQU $01              ; Clear the display and home the cursor
INTERFACE       EQU $38              ; 8 bit interface, two line display
CURSOR_OFF      EQU $0C              ; Display on, cursor off
SHIFT_OFF       EQU $06              ; Address increments, no character shift
LCD_SEC_LINE    EQU 64               ; Starting addr. of 2nd line of LCD (note decimal value!)
; LCD Addresses
LCD_CNTR        EQU PTJ              ; LCD Control Register: E = PJ7, RS = PJ6 
LCD_DAT         EQU PORTB            ; LCD Data Register: D7 = PB7, ... , D0 = PB0
LCD_E           EQU $80              ; LCD E-signal pin
LCD_RS          EQU $40              ; LCD RS-signal pin
; Other codes
NULL            EQU 00               ; The string 'null terminator'
CR              EQU $0D              ; 'CaRRiage Return' character
SPACE           EQU ' '              ; The 'space' character
 
; variable/data section
                ORG $39FF
;---------------------------------------------------------------------------                
; Storage Registers (9S12C32 RAM space: $3800 ... $3FFF)
SENSOR_LINE     FCB $01              ; Storage for guider sensor readings
SENSOR_BOW      FCB $23              ; Initialized to test values
SENSOR_PORT     FCB $45
SENSOR_MID      FCB $67
SENSOR_STBD     FCB $89
SENSOR_NUM      RMB 1                ; The cuRRently selected sensor

TOP_LINE        RMB 20               ; Top line of display
                FCB NULL             ;  terminated by nuLL

BOT_LINE        RMB 20               ; Bottom line of display
                FCB NULL             ;  terminated by nuLL

CLEAR_LINE      FCC '                    '
                FCB NULL             ;  terminated by nuLL
                
TEMP            RMB 1                ; Temporary location
 
; code section
               ; ORG $4000            ; Start of program text (FLASH memory)
;---------------------------------------------------------------------------
;               Initialization
; subroutine section
;---------------------------------------------------------------------------
;               Initialize ports
INIT            BCLR DDRAD,$FF       ; Make PORTAD an input (DDRAD @ $0272)
                BSET DDRA,$FF        ; Make PORTA an output (DDRA @ $0002)
                BSET DDRB,$FF        ; Make PORTB an output (DDRB @ $0003)
                BSET DDRJ,$C0        ; Make pins 7,6 of PTJ outputs (DDRJ @ $026A) 
                RTS
;---------------------------------------------------------------------------
;               Initialize the ADC
openADC         MOVB #$80,ATDCTL2    ; Turn on ADC (ATDCTL2 @ $0082)
                LDY  #1              ; Wait for 50 us for ADC to be ready
                JSR       del_50us        ;    - " -
                MOVB #$20,ATDCTL3    ; 4 conversions on channel AN1 (ATDCTL3 @ $0083)
                MOVB #$97,ATDCTL4    ; 8-bit resolution, prescaler=48 (ATDCTL4 @ $0084)
                RTS
;---------------------------------------------------------------------------
;               Clear LCD Buffer
; This routine writes 'space' characters (ascii 20) into the LCD display
;  buffer in order to prepare it for the building of a new display buffer.
; This needs only to be done once at the start of the program. Thereafter the 
;  display routine should maintain the buffer properly.

CLR_LCD_BUF     LDX #CLEAR_LINE
                LDY #TOP_LINE
                JSR STRCPY
CLB_SECOND      LDX #CLEAR_LINE
                LDY #BOT_LINE
                JSR STRCPY
CLB_EXIT        RTS

;---------------------------------------------------------------------------
;               String Copy
; Copies a nuLL-terminated string (including the nuLL) from one location to
; another
; Passed: X contains starting address of nuLL-terminated string
;   Y contains first address of destination     
STRCPY          PSHX                 ; Protect the registers used
                PSHY
                PSHA

STRCPY_LOOP     LDAA 0,X             ; Get a source character
                STAA 0,Y             ; Copy it to the destination
                BEQ  STRCPY_EXIT     ; If it was the nuLL, then exit
                INX                  ; Else increment the pointers
                INY
                BRA  STRCPY_LOOP     ; and do it again

STRCPY_EXIT     PULA                 ; Restore the registers
                PULY
                PULX
                RTS                     
;---------------------------------------------------------------------------
;               Guider LEDs ON
; This routine enables the guider LEDs so that readings of the sensor
;  coRRespond to the 'iLLuminated' situation.
; Passed:  Nothing
; Returns: Nothing
; Side:    PORTA bit 5 is changed
G_LEDS_ON       BSET PORTA,%00100000 ; Set bit 5
                RTS
;---------------------------------------------------------------------------
;               Guider LEDs OFF
; This routine disables the guider LEDs. Readings of the sensor
;  coRRespond to the 'ambient lighting' situation.
; Passed:  Nothing
; Returns: Nothing
; Side:    PORTA bit 5 is changed
G_LEDS_OFF      BCLR PORTA,%00100000 ; Clear bit 5
                RTS
;---------------------------------------------------------------------------
;               Read Sensors
;
; This routine reads the eebot guider sensors and puts the results in RAM
;  registers.
; Note: Do not confuse the analog multiplexer on the Guider board with the
;  multiplexer in the HCS12. The guider board mux must be set to the
;  appropriate channel using the SELECT_SENSOR routine. The HCS12 always
;  reads the selected sensor on the HCS12 A/D channel AN1.
; The A/D conversion mode used in this routine is to read the A/D channel
;  AN1 four times into HCS12 data registers ATDDR0,1,2,3. The only result
;  used in this routine is the value from AN1, read from ATDDR0. However,
;  other routines may wish to use the results in ATDDR1, 2 and 3.
; Consequently, Scan=0, Mult=0 and Channel=001 for the ATDCTL5 control word.
; Passed:       None
; Returns:      Sensor readings in:
;               SENSOR_LINE (0) (Sensor E/F)
;               SENSOR_BOW  (1) (Sensor A)
;               SENSOR_PORT (2) (Sensor B)
;               SENSOR_MID  (3) (Sensor C)
;               SENSOR_STBD (4) (Sensor D)
; Note:
;   The sensor number is shown in brackets
;
; Algorithm:
;        Initialize the sensor number to 0
;        Initialize a pointer into the RAM at the start of the Sensor ARRay storage
; MAIN_LOOP   Store %10000001 to the ATDCTL5 (to select AN1 and start a conversion)
;        Repeat
;           Read ATDSTAT0
;        Until Bit SCF of ATDSTAT0 == 1 (at which time the conversion is complete)
;        Store the contents of ATDDR0L at the pointer
;        If the pointer is at the last entry in Sensor ARRay, then 
;           Exit
;        Else 
;           Increment the sensor number
;           Increment the pointer
;        MAIN_LOOP again.
READ_SENSORS    CLR  SENSOR_NUM      ; Select sensor number 0
                LDX  #SENSOR_LINE    ; Point at the start of the sensor aRRay
RS_MAIN_LOOP    LDAA SENSOR_NUM      ; Select the coRRect sensor input
                JSR SELECT_SENSOR   ;  on the hardware
                LDY  #400            ; 20 ms delay to aLLow the
                JSR del_50us        ;  sensor to stabilize
                
                LDAA #%10000001      ; Start A/D conversion on AN1
                STAA ATDCTL5
                BRCLR ATDSTAT0,$80,* ; Repeat until A/D signals done
                LDAA ATDDR0L         ; A/D conversion is complete in ATDDR0L
                STAA 0,X             ;  so copy it to the sensor register
                CPX  #SENSOR_STBD    ; If this is the last reading
                BEQ  RS_EXIT         ; Then exit
                INC  SENSOR_NUM      ; Else, increment the sensor number
                INX                  ;  and the pointer into the sensor aRRay
                BRA  RS_MAIN_LOOP    ;  and do it again
RS_EXIT         RTS
;---------------------------------------------------------------------------
;               Select Sensor
; This routine selects the sensor number passed in ACCA. The motor direction
;  bits 0, 1, the guider sensor select bit 5 and the unused bits 6,7 in the
;  same machine register PORTA are not affected.
; Bits PA2,PA3,PA4 are connected to a 74HC4051 analog mux on the guider board,
;  which selects the guider sensor to be connected to AN1.
; Passed: Sensor Number in ACCA
; Returns: Nothing
; Side Effects: ACCA is changed
; Algorithm:
; First, copy the contents of PORTA into a temporary location TEMP and clear 
;        the sensor bits 2,3,4 in the TEMP to zeros by ANDing it with the mask
;        11100011. The zeros in the mask clear the coRResponding bits in the
;        TEMP. The 1's have no effect.
; Next, move the sensor selection number left two positions to align it
;        with the coRRect bit positions for sensor selection.
; Clear aSL the bits around the (shifted) sensor number by ANDing it with 
;  the mask 00011100. The zeros in the mask clear everything except
;        the sensor number.
; Now we can combine the sensor number with the TEMP using logical OR.
;  The effect is that only bits 2,3,4 are changed in the TEMP, and these
;  bits now coRRespond to the sensor number.
; FinaLLy, save the TEMP to the hardware.

SELECT_SENSOR   PSHA                 ; Save the sensor number for the moment
                LDAA PORTA           ; Clear the sensor selection bits to zeros
                ANDA #%11100011      ;
                STAA TEMP            ; and save it into TEMP
                PULA                 ; Get the sensor number
                ASLA                 ; Shift the selection number left, twice
                ASLA                 ; 
                ANDA #%00011100      ; Clear iRRelevant bit positions
                ORAA TEMP            ; OR it into the sensor bit positions
                STAA PORTA           ; Update the hardware
                RTS
;
;---------------------------------------------------------------------------
;               Display Sensor Readings
; Passed: Sensor values in RAM locations SENSOR_LINE through SENSOR_STBD.
; Returns: Nothing
; Side: Everything
; This routine writes the sensor values to the LCD. It uses the 'shadow
;  buffer' approach. The display buffer is built by the display controLLer
;  routine and then copied in its entirety to the actual LCD display.
;  Although simpler approaches wiSL work in this application, we take that
;  approach to make the code more re-useable.
; It's important that the display controLLer not write over other
;  information on the display, so writing the display has to be centralized
;  with a controLLer routine like this one.
; In a more complex program with additional things to display on the LCD,
;  this routine would be extended to read other variables and place
;  them on the display. It might even read some 'display select' variable to
;  determine what should be on the display.
; For the purposes of this routine, we'SL put the sensor values on the LCD
;  in such a way that they (sort of) mimic the position of the sensors, so
;  the display looks like this:
;   01234567890123456789
;   ___FF_______________
;   PP_MM_SS_LL_________
; Where FF is the front sensor, PP is port, MM is mid, SS is starboard and
;  SL is the line sensor. 
; The coRResponding addresses in the LCD buffer are defined in the foLLowing
;  equates (In aSL cases, the display position is the MSDigit).
DS              EQU TOP_LINE+6  
DP_FRONT_SENSOR EQU TOP_LINE+3
DP_PORT_SENSOR  EQU BOT_LINE+0
DP_MID_SENSOR   EQU BOT_LINE+3
DP_STBD_SENSOR  EQU BOT_LINE+6
DP_LINE_SENSOR  EQU BOT_LINE+9
DISPLAY_SENSORS LDAA CURRENT_STATE
                JSR putcLCD
                LDX  #DS
                STD  0,X
                LDAA SENSOR_BOW       ; Get the FRONT sensor value
                JSR BIN2ASC          ; Convert to ascii string in D
                LDX  #DP_FRONT_SENSOR ; Point to the LCD buffer position
                STD  0,X              ;  and write the 2 ascii digits there 
                LDAA SENSOR_PORT      ; Repeat for the PORT value
                JSR BIN2ASC             
                LDX  #DP_PORT_SENSOR   
                STD  0,X
                LDAA SENSOR_MID       ; Repeat for the MID value
                JSR BIN2ASC                                           
                LDX  #DP_MID_SENSOR                                      
                STD  0,X
                LDAA SENSOR_STBD      ; Repeat for the STARBOARD value
                JSR BIN2ASC                                           
                LDX  #DP_STBD_SENSOR                                      
                STD  0,X
                LDAA SENSOR_LINE      ; Repeat for the LINE value
                JSR BIN2ASC                                           
                LDX  #DP_LINE_SENSOR                                      
                STD  0,X
                LDAA #CLEAR_HOME      ; Clear the display and home the cursor
                JSR cmd2LCD          ;       "
                LDY  #40              ; Wait 2 ms untiSL "clear display" command is complete
                JSR del_50us
                LDX  #TOP_LINE        ; Now copy the buffer top line to the LCD    
                JSR putsLCD    
                LDAA #LCD_SEC_LINE    ; Position the LCD cursor on the second line
                JSR LCD_POS_CRSR    
                LDX  #BOT_LINE        ; Copy the buffer bottom line to the LCD
                JSR putsLCD
                RTS
;---------------------------------------------------------------------------
;               Binary to ASCII
; Converts an 8 bit binary value in ACCA to the equivalent ASCII character 2
;  character string in accumulator D
; Uses a table-driven method rather than various tricks.
; Passed: Binary value in ACCA
; Returns: ASCII Character string in D
; Side Fx: ACCB is destroyed
HEX_TABLE       FCC '0123456789ABCDEF' ; Table for converting values
BIN2ASC         PSHA                   ; Save a copy of the input number on the stack
                TAB                    ;  and copy it into ACCB
                ANDB #%00001111        ; Strip off the upper nibble of ACCB
                CLRA                   ; D now contains 000n where n is the LSnibble
                ADDD #HEX_TABLE        ; Set up for indexed load
                XGDX                
                LDAA 0,X               ; Get the LSnibble character
                PULB                   ; Retrieve the input number into ACCB
                PSHA                   ; and push the LSnibble character in its place
                RORB                   ; Move the upper nibble of the input number
                RORB                   ;  into the lower nibble position.
                RORB
                RORB 
                ANDB #%00001111        ; Strip off the upper nibble
                CLRA                   ; D now contains 000n where n is the MSnibble 
                ADDD #HEX_TABLE        ; Set up for indexed load
                XGDX                                                               
                LDAA 0,X               ; Get the MSnibble character into ACCA
                PULB                   ; Retrieve the LSnibble character into ACCB
                RTS
;---------------------------------------------------------------------------
;     Routines to control the Liquid Crystal Display
;---------------------------------------------------------------------------
;               Initialize the LCD
openLCD         LDY  #2000             ; Wait 100 ms for LCD to be ready
                JSR del_50us          ;       "
                LDAA #INTERFACE        ; Set 8-bit data, 2-line display, 5x8 font
                JSR cmd2LCD           ;       "
                LDAA #CURSOR_OFF       ; Display on, cursor off, blinking off
                JSR cmd2LCD           ;       "
                LDAA #SHIFT_OFF        ; Move cursor right (address increments, no char. shift)
                JSR cmd2LCD           ;       "
                LDAA #CLEAR_HOME       ; Clear the display and home the cursor
                JSR cmd2LCD           ;       "
                LDY  #40               ; Wait 2 ms until "clear display" command is complete
                JSR del_50us          ;       "
                RTS               
;---------------------------------------------------------------------------
;               Send a command in accumulator A to the LCD
cmd2LCD         BCLR LCD_CNTR,LCD_RS   ; Select the LCD Instruction register
                JSR dataMov           ; Send data to IR or DR of the LCD
                RTS          
;---------------------------------------------------------------------------
;               Send a character in accumulator in A to LCD
putcLCD         BSET LCD_CNTR,LCD_RS   ; select the LCD Data register
                JSR dataMov           ; send data to IR or DR of the LCD
                RTS
;---------------------------------------------------------------------------
;               Send a NULL-terminated string pointed to by X

putsLCD         LDAA 1,X+              ; get one character from the string
                BEQ  donePS            ; reach NUSL character?
                JSR putcLCD
                BRA  putsLCD
donePS          RTS
;---------------------------------------------------------------------------
;               Send data to the LCD IR or DR depending on the RS signal
dataMov         BSET LCD_CNTR,LCD_E    ; puSL the LCD E-sigal high
                STAA LCD_DAT           ; send the 8 bits of data to LCD
                NOP
                NOP
                NOP
                BCLR LCD_CNTR,LCD_E    ; puSL the E signal low to complete the write operation
                LDY  #1                ; adding this delay wiSL complete the internal
                JSR del_50us          ; operation for most instructions
                RTS    
initAD              MOVB #$C0,ATDCTL2 ;power up AD, select fast flag clear
                    JSR  del_50us ;wait for 50 us
                    MOVB #$00,ATDCTL3 ;8 conversions in a sequence
                    MOVB #$85,ATDCTL4 ;res=8, conv-clks=2, prescal=12
                    BSET ATDDIEN,$0C ;configure pins AN03,AN02 as digital inputs
                    RTS
;---------------------------------------------------------------------------
;               Position the Cursor
; This routine positions the display cursor in preparation for the writing
;  of a character or string.
; For a 20x2 display:
; The first line of the display runs from 0 .. 19.
; The second line runs from 64 .. 83.
; The control instruction to position the cursor has the format
;            1aaaaaaa
; where aaaaaaa is a 7 bit address.
; Passed:   7 bit cursor Address in ACCA
; Returns:  Nothing
; Side Effects: None
LCD_POS_CRSR     ORAA #%10000000        ; Set the high bit of the control word
                JSR cmd2LCD           ;  and set the cursor address    
                RTS 
;---------------------------------------------------------------------------
;               50 Microsecond Delay
del_50us        PSHX                   ; (2 E-clk) Protect the X register 
eloop           LDX #300               ; (2 E-clk) Initialize the inner MAIN_LOOP counter
iloop           NOP                    ; (1 E-clk) No operation
                DBNE  X,iloop          ; (3 E-clk) If the inner cntr not 0, MAIN_LOOP again
                DBNE  Y,eloop          ; (3 E-clk) If the outer cntr not 0, MAIN_LOOP again
                PULX                   ; (3 E-clk) Restore the X register
                RTS                    ; (5 E-clk) Else return
;----------------------------------------------------------------------------
                ORG $FFFE
                DC.W Entry                      ; Reset Vector
