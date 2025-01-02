
//==================================================================================================================================================================================
//  PYRAMIDION NON-RELATIONAL DATABASE, v.0.16 (alpha version)
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//  (c) 2020 Igor Voloshin ivoloshin@hotmail.com
//==================================================================================================================================================================================
//  Unit Pleximo016
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//  Contains command line parser and other Levels 3 procedures and functions of Desmous DBMS,
//  basic to the Sophos Knowledge Base Management System (Sophos KBMS).
//
//	LEVEL 3. PLEXIMO MECHANICS
//		3.0. PParse - Command line parser;
//
//==================================================================================================================================================================================

unit Pleximo016;

{$mode objfpc}

interface

uses
  Classes, SysUtils;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	Identified objects type Declaration
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
type	ObjRef =	record			// Special type is required to store objects references linked to their content.
	ObjID:	longint;			// Object Node ID;
	ObjVal:	rawbytestring;			// Current content of the object.
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	Pleximo Class Declaration
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
type	TPleximo =		class(TObject)
	private
	public
		function	PParse(InputStr: rawbytestring): integer;
end;


var

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	Instances of the main program objects
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Pl:	TPleximo;			// All strings are parsed here

implementation

uses Aesthesia016, Apostolia016;

//==================================================================================================================================================================================
//	LEVEL 3. PLEXIMO MECHANICS
//==================================================================================================================================================================================
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	3.0. PParse - Parses strings (of any origin) to abstract syntactic trees corresonding to the content of the Antikeymena knowledge base
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// >>>>>>>>>>>>>> Forgot to put check for deleted nodes/sags!
function TPleximo.PParse(InputStr: rawbytestring): longint;
var
	Cnt11, Cnt12: longint;
        FStr: rawbytestring;
        FByte: char;
        AllSymbols: array of ObjRef;
	PrsSymbols: array of longint;									// Identified symbols array
        ArID: longint;	     										// ID of the 'Arithmetica' node
        NumID: longint;											// ID of the 'Numerus' node
        CommID: longint;										// ID of the 'Comma' node
        OpID: longint;											// ID of the 'Operatio' node
        ParAntID: longint;										// ID of the 'Parenthesis Anterioris' node
        ParPostID: longint;										// ID of the 'Parenthesis Posterioris' node
	PrsInput: array of ObjRef;									// Identified objects array
        SitID: integer;	   										// Situation ID to handle symbol processing rules
	PointFlg: boolean;										// Digital point flag
        BrckCnt: integer;										// Braqckets counter
begin
     	Result := 51;
        //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        // Filtering input string for allowed symbols
        FStr := '';
        for Cnt11 := 0 to High(InputStr) do begin
		FByte := InputStr[Cnt11];
                if not (FByte in ['0'..'9', '.', ',', '(', ')', '+', '-', '/', '*']) then FByte := #0;	// 18 symbols are allowed yet
                if FByte <> #0 then FStr := FStr + FByte;
        end;
        //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        // Linking allowed symbols to symbol nodes
        Cnt11 := 0;
        for Cnt11 := 0 to High(Nod) do begin
                if Nod[Cnt11].Com = 'Symboli' then ArID := Nod[Cnt11].ID;
	end;
	repeat Cnt11 := Cnt11 + 1 until ((Nod[Cnt11].Com = 'Symboli') or (Cnt11 > High(Nod)));
        for Cnt12 := 0 to High(Nod[Cnt11].Edg) do begin
                SetLength(AllSymbols, Length(AllSymbols) + 1);
                if Nod[Cnt11].Edg[Cnt12].ETp = Nod[Cnt11].ID then begin
                        AllSymbols[High(AllSymbols)].ObjID := Nod[Cnt11].Edg[Cnt12].ID;
                        AllSymbols[High(AllSymbols)].ObjVal := Nod[Ap.Idx(Nod[Cnt11].Edg[Cnt12].ID)].Com;
                end;
	end;
	for Cnt11 := 0 to High(FStr) do begin
                Cnt12 := -1;
                repeat Inc(Cnt12) until FStr[Cnt11] = AllSymbols[Cnt12].ObjVal;
                SetLength(PrsSymbols, Length(PrsSymbols) + 1);
                PrsSymbols[High(PrsSymbols)] := AllSymbols[Cnt12].ObjID;
        end;
        //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        // Getting references to arithmetic objects
        Cnt11 := 0;
        repeat Inc(Cnt11) until Nod[Cnt11].Com = 'Arithmetica';
        ArID := Nod[Cnt11].ID;
        Cnt11 := 0;
        repeat Inc(Cnt11) until Nod[Ap.Idx(Nod[Ap.Idx(ArID)].Edg[Cnt11].ID)].Com = 'Numerus';
        NumID := Nod[Ap.Idx(Nod[Ap.Idx(ArID)].Edg[Cnt11].ID)].ID;
        Cnt11 := 0;
        repeat Inc(Cnt11) until Nod[Ap.Idx(Nod[Ap.Idx(ArID)].Edg[Cnt11].ID)].Com = 'Comma';
        CommID := Nod[Ap.Idx(Nod[Ap.Idx(ArID)].Edg[Cnt11].ID)].ID;
        Cnt11 := 0;
        repeat Inc(Cnt11) until Nod[Ap.Idx(Nod[Ap.Idx(ArID)].Edg[Cnt11].ID)].Com = 'Operatio';
        OpID := Nod[Ap.Idx(Nod[Ap.Idx(ArID)].Edg[Cnt11].ID)].ID;
        Cnt11 := 0;
        repeat Inc(Cnt11) until Nod[Ap.Idx(Nod[Ap.Idx(ArID)].Edg[Cnt11].ID)].Com = 'Parenthesis Anterioris';
        ParAntID := Nod[Ap.Idx(Nod[Ap.Idx(ArID)].Edg[Cnt11].ID)].ID;
        Cnt11 := 0;
        repeat Inc(Cnt11) until Nod[Ap.Idx(Nod[Ap.Idx(ArID)].Edg[Cnt11].ID)].Com = 'Parenthesis Posterioris';
        ParPostID := Nod[Ap.Idx(Nod[Ap.Idx(ArID)].Edg[Cnt11].ID)].ID;
        //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        // Processing situations and corresponding rules for each arithmetic object
        PointFlg := False;
        for Cnt11 := 0 to High(PrsSymbols) do begin
                // Defining situations IDs
                //------------------------------------------------------------------------------------------------------------------------------------------------------------------
                if Length(PrsInput) = 0 then SitID := 0
                else if PrsInput[High(PrsInput)].ObjID = NumID then SitID := 1
                else if PrsInput[High(PrsInput)].ObjID = CommID then SitID := 2
                else if PrsInput[High(PrsInput)].ObjID = OpID then SitID := 3
                else if PrsInput[High(PrsInput)].ObjID = ParAntID then SitID := 4
                else if PrsInput[High(PrsInput)].ObjID = ParPostID then SitID := 5;
                Cnt12 := -1;
                repeat Inc(Cnt12) until Nod[Ap.Idx(PrsSymbols[Cnt11])].Edg[Cnt12].ETp = ArID;		// Look for symbol sag typed as 'Arithmetica', there must be only one
                // If current symbol is a digit
                //------------------------------------------------------------------------------------------------------------------------------------------------------------------
                if Nod[Ap.Idx(Nod[Ap.Idx(PrsSymbols[Cnt11])].Edg[Cnt12].ID)].ID = NumID then begin	// If the current symbol type is 'Numerus'
                        case SitID of
                        0, 3, 4:	begin
                        		SetLength(PrsInput, Length(PrsInput) + 1);
                        		PrsInput[High(PrsInput)].ObjID := NumID;  	     	  	// 'Numerus' node ID
                        		PrsInput[High(PrsInput)].ObjVal := Nod[Ap.Idx(PrsSymbols[Cnt11])].Com;
                        	end;
                        1, 2:	PrsInput[High(PrsInput)].ObjVal :=  PrsInput[High(PrsInput)].ObjVal + Nod[Ap.Idx(PrsSymbols[Cnt11])].Com;
                        else	Result := -1;
			end;
		end;
                // If current symbol is digital point or comma
                //------------------------------------------------------------------------------------------------------------------------------------------------------------------
                if Nod[Ap.Idx(Nod[Ap.Idx(PrsSymbols[Cnt11])].Edg[Cnt12].ID)].ID = CommID then begin	// If the current symbol type is 'Comma'
                        PointFlg := False;
                        case SitID of
			1:	if not PointFlg then begin
                                	PrsInput[High(PrsInput)].ObjVal :=  PrsInput[High(PrsInput)].ObjVal + Nod[Ap.Idx(PrsSymbols[Cnt11])].Com;
                        		PointFlg := True;
                        	end
                        	else Result := -1;
                        else	Result := -1;
			end;
		end;
                // If current symbol is an arithmetic operation symbol
                //------------------------------------------------------------------------------------------------------------------------------------------------------------------
                if Nod[Ap.Idx(Nod[Ap.Idx(PrsSymbols[Cnt11])].Edg[Cnt12].ID)].ID = OpID then begin		// If the current symbol type is 'Operatio'
                        case SitID of
                        1, 5:	begin
                        		SetLength(PrsInput, Length(PrsInput) + 1);
                        		PrsInput[High(PrsInput)].ObjID :=  OpID;  	    		// 'Operatio' node ID
                        		PrsInput[High(PrsInput)].ObjVal :=  Nod[Ap.Idx(PrsSymbols[Cnt11])].Com;
                        	end;
                        else	Result := -1;
			end;
		end;
                // If current symbol is the left bracket
                //------------------------------------------------------------------------------------------------------------------------------------------------------------------
                if Nod[Ap.Idx(Nod[Ap.Idx(PrsSymbols[Cnt11])].Edg[Cnt12].ID)].ID = ParAntID then begin	// If the current symbol type is 'Parenthesis Anterioris'
                        case SitID of
                        0, 3, 4:begin
                                	SetLength(PrsInput, Length(PrsInput) + 1);
                                        PrsInput[High(PrsInput)].ObjID := ParAntID;    			// 'Parenthesis Anterioris' node ID
                        		PrsInput[High(PrsInput)].ObjVal := Nod[Ap.Idx(PrsSymbols[Cnt11])].Com;
                        		Inc(BrckCnt);
                        	end;
                        else	Result := -1;
			end;
		end;
                // If current symbol is the right bracket
                //------------------------------------------------------------------------------------------------------------------------------------------------------------------
                if Nod[Ap.Idx(Nod[Ap.Idx(PrsSymbols[Cnt11])].Edg[Cnt12].ID)].ID = ParPostID then begin	// If the current symbol type is 'Parenthesis Posterioris'
                        case SitID of
                        1, 5:	begin
                                	SetLength(PrsInput, Length(PrsInput) + 1);
                                        PrsInput[High(PrsInput)].ObjID := ParPostID;   			// 'Parenthesis Posterioris' node ID
                        		PrsInput[High(PrsInput)].ObjVal := Nod[Ap.Idx(PrsSymbols[Cnt11])].Com;
                        		Dec(BrckCnt);
                        	end;
                        else	Result := -1;
			end;
		end;
	end;
        if BrckCnt <> 0 then Result := -1;                                                              // Check for unpaired brackets
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
end.

