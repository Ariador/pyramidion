
//==================================================================================================================================================================================
//	PYRAMIDION NON-RELATIONAL DATABASE, v.0.16 (alpha version)
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	(c) 2020 Igor Voloshin ivoloshin@hotmail.com
//==================================================================================================================================================================================
//	Unit Aesthesia016
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	Contains declarations for basic Pyramidion DBMS types, objects and variables, also internal tools (Level -1) and user interface (Level 0).
//	Functions and procedures implemented so far are the following:
//
//	LEVEL -1. INTERNAL PROGRAM TOOLS
//		-1.0. ByteToBitStr - Convert a byte to '00000000' type string;
//		-1.1. BitStrToByte - Convert '00000000' type string to a byte;
//
//	LEVEL 0. AESTHESIA - PYRAMIDION INTERFACE
//		0.00. FormActivate - Initial program actions with the database
//		0.01. Redraw - Fill all the form fields with Nodes array data
//		0.02. AMonitor - Check current status of Nod and Undo chain and show it in console window
//		0.03. ALog - Log message handling
//		0.04. SConEdit - Process input from the main source - Session Console
//		0.05. CNodIDKeyPress - Filter for current node ID editing
//		0.06. CNodIDEdit - Current node ID editing done
//		0.08. CNodFlgKeyPress - Current node flags editing
//		0.09. CNodFlgEditingDone - Current node flags editing done
//		0.10. CNodComEdit - Current node comment editing done
//		0.11. TNodIDKeyPress - Filter for target node ID editing
//		0.12. TNodIDEdit - Target node ID editing done
//		0.13. ETpKeyPress - Filter for target edge type editing
//		0.14. TETpEdit - Target edge type editing done
//		0.15. TNodComEdit - Target node comment editing done
//		0.16. AddAscEdgBtnClick - Add ascending edge button click
//		0.17. AddDscEdgBtnClick - Add descending edge button click
//		0.18. DelCNodBtnClick - Delete current node button click
//		0.19. DelCEdgBtnClick - Delete current edge button click
//		0.26. CommitBtnClick - Commit button click
//		0.27. ExitBtnClick - Exit button click
//		0.28. UndoBtnClick - Undo last action done (if the base not committed)
//		0.29. RedoBtnClick - Redo last action undone (if the base not committed)
//		0.30. FormClose - Form close without saving any logs or changes to IDS file
//		0.31. MonitorBtnClick - Types all of the Nod array content to session console
//	JUST FOR NICE LOOK
//		0.-1. Color manipulations for the form elements (4 procedures).
//
//==================================================================================================================================================================================

unit Aesthesia016;

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids, ExtCtrls, Buttons, LCLProc, LResources, Menus, LvlGraphCtrl, RichMemo, RichMemoUtils, ueled;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	Aesthesia Form Declaration
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
type

{ TAesthesiaForm }

 TAesthesiaForm = class(TForm)
        	PopImage:	TImage;
		LvlGraph:	TLvlGraphControl;
                //--------------------------
                NodCntPnl:	TPanel;
 		NodCntLbl:	TLabel;
                NodCntPlq:	TPanel;
		NodDLbl:	TLabel;
		NodDPlq:	TPanel;
                //--------------------------
                SagCntPnl:	TPanel;
		SagCntLbl:	TLabel;
                SagCntPlq:	TPanel;
                SagDLbl:	TLabel;
                SagDPlq:	TPanel;
                //--------------------------
                BtnPnl:	TPanel;
                AddAscEdgBtn:	TPanel;
                AddDscEdgBtn:	TPanel;
                DelCNodBtn:	TPanel;
                DelCEdgBtn:	TPanel;
                CommitBtn:	TPanel;
                ExitBtn:	TPanel;
                MonitorBtn:	TPanel;
                UndoBtn:	TPanel;
                RedoBtn:	TPanel;
		UndoLvlBtn:	TPanel;
		uELED1:		TuELED;
		uELED2:		TuELED;
		uELED3:		TuELED;
                PyramidionLogo:	TImage;
                RFileOpen:	TOpenDialog;
                RFileSave:	TSaveDialog;
                //--------------------------
		SNodPnl:	TPanel;
		SNodLbl:	TLabel;
                SNodGrd:	TStringGrid;
                //--------------------------
		CurrStatePnl:	TPanel;
		CNodLbl:	TLabel;
		CNodIDLbl:	TLabel;
		CNodID:		TEdit;
		CNodFlgLbl:	TLabel;
		CNodFlg:	TEdit;
		CNodComLbl:	TLabel;
		CNodCom:	TEdit;
		CEdgLbl:	TLabel;
 		EdgDirLbl:	TLabel;
		EdgDir:		TPanel;
                TNodIDLbl:	TLabel;
		TNodID:		TEdit;
		ETpLbl:		TLabel;
		ETp:		TEdit;
		ETpTxt:		TPanel;
                //--------------------------
		INodPnl:	TPanel;
                INodLbl:	TLabel;
                INodGrd:	TStringGrid;
                //--------------------------
		UNodPnl:	TPanel;
                UNodLbl:	TLabel;
                UNodGrd:	TStringGrid;
                //--------------------------
		SConPnl:	TPanel;
                SConLbl:	TLabel;
                SCon:		TRichMemo;
		SConStatLbl:	TLabel;
                //--------------------------
		PopMenu:	TPopupMenu;
		SetCurrI:	TMenuItem;
		DelCEdgI:	TMenuItem;
		AddSEdgI:	TMenuItem;
		AddIEdgI:	TMenuItem;
          	AddSNodI:	TMenuItem;
          	AddINodI:	TMenuItem;
          	AddUNodI:	TMenuItem;
          	DelNodI:	TMenuItem;
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	//	Interface procedures
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                procedure	FormActivate(Sender: TObject);
		procedure	Redraw();
		procedure	AMonitor();
		procedure	ALog(Msg: integer);
		procedure	SConClick(Sender: TObject);
		procedure	SConKeyUp(Sender: TObject);
		procedure	SConKeyPress(Sender: TObject; var Key: char);
		procedure	SConEdit();
		procedure	CNodIDKeyPress(Sender: TObject; var Key: char);
		procedure	CNodIDEdit(Sender: TObject);
		procedure	CNodFlgEnter(Sender: TObject);
		procedure	CNodFlgKeyPress(Sender: TObject; var Key: char);
		procedure	CNodFlgEditingDone(Sender: TObject);
		procedure	CNodComEnter(Sender: TObject);
		procedure	CNodComEdit(Sender: TObject);
		procedure	TNodIDKeyPress(Sender: TObject; var Key: char);
		procedure	TNodIDEdit(Sender: TObject);
		procedure	ETpKeyPress(Sender: TObject; var Key: char);
		procedure	TETpEdit(Sender: TObject);
		procedure	AddAscEdgBtnClick(Sender: TObject);
		procedure	AddDscEdgBtnClick(Sender: TObject);
		procedure	DelCNodBtnClick(Sender: TObject);
		procedure	DelCEdgBtnClick(Sender: TObject);
		procedure	SNodGrdMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
		procedure	SNodGrdDblClick(Sender: TObject);
		procedure	SNodGrdEditingDone(Sender: TObject);
		procedure	INodGrdMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
		procedure	INodGrdDblClick(Sender: TObject);
		procedure	INodGrdEditingDone(Sender: TObject);
		procedure	UNodGrdMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
		procedure	UNodGrdDblClick(Sender: TObject);
		procedure	UNodGrdEditingDone(Sender: TObject);
		procedure	CommitBtnClick(Sender: TObject);
		procedure	ExitBtnClick(Sender: TObject);
		procedure	UndoBtnClick(Sender: TObject);
		procedure	RedoBtnClick(Sender: TObject);
		procedure	FormClose(Sender: TObject);
		procedure	MonitorBtnClick(Sender: TObject);
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	//	'Cosmetic' procedures
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
		procedure	CMouseEnter(Sender: TObject);
		procedure	CMouseLeave(Sender: TObject);
        	procedure	CMouseDown(Sender: TObject);			//; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        	procedure	CMouseUp(Sender: TObject);			//; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
	private
	public
end;

var
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	Instance of the main program form
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        Form:	TAesthesiaForm;			// This is where all the user interface is located
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	Global variables - all names start with 'G'
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	GIDSFile:	string;			// Current IDS file in use
	GCRC32:		qword;			// CRC sum to check IDS file consistency
        GUpperID:	longint;  		// Upper ID value for the current Nod array
        GNodDCnt:	longint;		// Global deleted nodes counter
        GEdgeCnt:	longint;		// Global edges counter
        GEdgDCnt:	longint;		// Global deleted edges counter
	GCurrIdx:	longint;		// Index of the current node
	GTargIdx:	longint;		// Index of the target node
        GCNodFlg:	byte;			// Current node flags
        GCurrEdge:	longint;		// Index of the current edge at the current node
        GTargEdge:	longint;		// Index of the current edge at the target node
	GCurrComment:	rawbytestring;		// Accessory var to store current node comment
	GTargComment:	rawbytestring;		// Accessory var to store target node comment
	GCurrMsg:	integer;		// Code of the current status message
        GQueryAsked:	Boolean;		// Flag of console query
	GAllowUndo:	byte;			// Switches undo service between 0 (off), 1 (limited to the last commit), 255 (unlimited)
	GUndo:		longint;		// Current undo/redo array position

implementation

{$R *.lfm}
uses Apostolia016;

//==================================================================================================================================================================================
//	LEVEL -1. INTERNAL PROGRAM TOOLS
//==================================================================================================================================================================================
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	-1.0. ByteToBitStr - Convert a byte to '00000000' type string
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
function ByteToBitStr(ByteValue: byte): string;
begin
        Result := BinStr(integer(ByteValue), 8);
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	-1.1. BitStrToByte - Convert '00000000' type string to byte
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
function BitStrToByte(StrValue: string): byte;
var
	Cnt: byte;
begin
	Result := 0;
	for Cnt := 0 to 7 do Result := Result + (byte(StrToInt(RightStr(LeftStr(StrValue, 8 - Cnt), 1))) shl Cnt);
end;

//==================================================================================================================================================================================
//	LEVEL 0. AESTHESIA INTERFACE
//==================================================================================================================================================================================
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.0. FormActivate - Initial program actions with the database
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.FormActivate(Sender: TObject);
begin
	if RFileOpen.Execute then GIDSFile := RFileOpen.Filename else GIDSFile := 'Pyramidion 016.ids';
        Alog(00);
        GQueryAsked := True;
        SConEdit;
        Redraw;
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.1. Redraw - Fill all the form fields with Nod array data
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.Redraw();
var
	Cnt1, Cnt2, TRow, CIdx: longint;
	SNod, INod, UNod: array of longint;								// Superior, inferior and unlinked nodes
begin
        if GCurrIdx = GTargIdx then GTargIdx := -1;							// Prevent cyclic references
	SetLength(SNod, 0);
	SetLength(INod, 0);
	SetLength(UNod, 0);
        TRow := 0;
	NodCntPlq.Caption := IntToStr(Length(Nod) - GNodDCnt);						// Refresh (non-deleted) nodes count
        NodDPlq.Caption := IntToStr(GNodDCnt);   							// Refresh deleted nodes count
        SagCntPlq.Caption := IntToStr(GEdgeCnt);							// Refresh (non-deleted) edges count
        SagDPlq.Caption := IntToStr(GEdgDCnt);								// Refresh deleted edges count

	// -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	//	Accessory arrays filling
	// -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	if ((Nod[GCurrIdx].Flg and 16) = 0) then if ((Nod[GCurrIdx].Flg and 6) > 0) then		// Check if the node is not deleted and has edges
                for Cnt1 := 0 to High(Nod[GCurrIdx].Edg) do
		if (Nod[GCurrIdx].Edg[Cnt1].Flg and 2) = 0 then if (Nod[GCurrIdx].Edg[Cnt1].Flg and 1) > 0 then begin // Fill descending nodes array
			SetLength(INod, Length(INod) + 1);
			INod[High(INod)] := Nod[GCurrIdx].Edg[Cnt1].ID;
                        CIdx := Ap.Idx(Nod[GCurrIdx].Edg[Cnt1].ID);
			Nod[CIdx].Flg := Nod[CIdx].Flg or 8;	       					// Raise Lnk flag
		end
		else begin										// Fill ascending nodes array
			SetLength(SNod, Length(SNod) + 1);
			SNod[High(SNod)] := Nod[GCurrIdx].Edg[Cnt1].ID;
                        CIdx := Ap.Idx(Nod[GCurrIdx].Edg[Cnt1].ID);
			Nod[CIdx].Flg := Nod[CIdx].Flg or 8;	       					// Raise Lnk flag
		end;
	for Cnt1 := 0 to High(Nod) do 	 	       	  						// Fill unlinked nodes array
		if (Nod[Cnt1].Flg and 8) = 0 then begin							// Check Lnk flag
			if Cnt1 <> GCurrIdx then
				if Cnt1 = 0 then begin
					SetLength(UNod, Length(UNod) + 1);
					UNod[High(UNod)] := 0;	       	  				// Node 0
				end
				else if (Nod[Cnt1].Flg and 16) = 0 then begin
					SetLength(UNod, Length(UNod) + 1);
					UNod[High(UNod)] := Nod[Cnt1].ID;
				end;
		end
		else Nod[Cnt1].Flg := Nod[Cnt1].Flg and 247;						// Lower Lnk flag
	// -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	//	Superior Nodes grid filling
	// -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	SNodGrd.RowCount := Length(SNod) + 1;
	SNodGrd.Cells[0, 0] := 'SNodes';
	SNodGrd.Row := 0;
	for Cnt1 := 0 to High(SNod) do begin
		Cnt2 := Ap.Idx(SNod[Cnt1]);
		SNodGrd.Cells[0, Cnt1 + 1] := IntToStr(Cnt1 + 1);
		SNodGrd.Cells[1, Cnt1 + 1] := IntToStr(Nod[Cnt2].ID);
		SNodGrd.Cells[2, Cnt1 + 1] := ByteToBitStr(Nod[Cnt2].Flg);
		SNodGrd.Cells[3, Cnt1 + 1] := Nod[Cnt2].Com;
		if Cnt2 = Ap.Idx(Nod[GTargIdx].ID) then TRow := Cnt1 + 1;
	end;
	// -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	//	Inferior Nodes grid filling
	// -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	INodGrd.RowCount := Length(INod) + 1;
	INodGrd.Cells[0, 0] := 'INodes';
	INodGrd.Row := 0;
	for Cnt1 := 0 to High(INod) do begin
		Cnt2 := Ap.Idx(INod[Cnt1]);
		INodGrd.Cells[0, Cnt1 + 1] := IntToStr(Cnt1 + 1);
		INodGrd.Cells[1, Cnt1 + 1] := IntToStr(Nod[Cnt2].ID);
		INodGrd.Cells[2, Cnt1 + 1] := ByteToBitStr(Nod[Cnt2].Flg);
		INodGrd.Cells[3, Cnt1 + 1] := Nod[Cnt2].Com;
		if Cnt2 = Ap.Idx(Nod[GTargIdx].ID) then TRow := Cnt1 + 1;
	end;
	// -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	//	Unlinked Nodes grid filling
	// -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	UNodGrd.RowCount := Length(UNod) + 1;
	UNodGrd.Cells[0, 0] := 'UNodes';
	UNodGrd.Row := 0;
	for Cnt1 := 0 to High(UNod) do begin
		Cnt2 := Ap.Idx(UNod[Cnt1]);
		UNodGrd.Cells[0, Cnt1 + 1] := IntToStr(Cnt1 + 1);
		UNodGrd.Cells[1, Cnt1 + 1] := IntToStr(Nod[Cnt2].ID);
		UNodGrd.Cells[2, Cnt1 + 1] := ByteToBitStr(Nod[Cnt2].Flg);
		UNodGrd.Cells[3, Cnt1 + 1] := Nod[Cnt2].Com;
		if Cnt2 = Ap.Idx(Nod[GTargIdx].ID) then TRow := Cnt1 + 1;
	end;
	// -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	//	Refreshing interface fields
	// -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	//	Fill current node fields
	CNodID.Text := IntToStr(Nod[GCurrIdx].ID);
	CNodFlg.Caption := ByteToBitStr(Nod[GCurrIdx].Flg);
	CNodCom.Text := Nod[GCurrIdx].Com;
	// Fill target node fields
	if GTargIdx = -1 then begin
		TNodID.Text := '';
		EdgDir.Caption := 'Undef.';
		GCurrEdge := -1;
                GTargEdge := -1;
	end
	else begin
		TNodID.Text := IntToStr(Nod[GTargIdx].ID);
		for Cnt1 := 0 to High(SNod) do if SNod[Cnt1] = Nod[GTargIdx].ID then begin
			EdgDir.Caption := 'Asc.';
			ETp.Text := IntToStr(Nod[GCurrIdx].Edg[GCurrEdge].ETp);
			SNodGrd.Row := TRow;
		end;
		for Cnt1 := 0 to High(INod) do if INod[Cnt1] = Nod[GTargIdx].ID then begin
			EdgDir.Caption := 'Desc.';
			ETp.Text := IntToStr(Nod[GCurrIdx].Edg[GCurrEdge].ETp);
			INodGrd.Row := TRow;
		end;
		for Cnt1 := 0 to High(UNod) do if UNod[Cnt1] = Nod[GTargIdx].ID then begin
			EdgDir.Caption := 'Undef.';
			UNodGrd.Row := TRow;
			GCurrEdge := -1;
		end;
		GTargComment := Nod[GTargIdx].Com;
	end;
	case ETp.Text of
		'' : ETpTxt.Caption := 'Undef.';
                else ETpTxt.Caption := LeftStr(Nod[Ap.Idx(StrToInt(ETp.Text))].Com, Pos(' ', Nod[Ap.Idx(StrToInt(ETp.Text))].Com) - 1);
	end;
        case GAllowUndo of
                0: begin
                        uELED1.Color := clRed;
                        uELED2.Color := $00004040;
                        uELED3.Color := $00004000;
		   end;
                1: begin
                        uELED1.Color := $00000040;
                        uELED2.Color := clYellow;
                        uELED3.Color := $00004000;
		   end;
                else begin
                        uELED1.Color := $00000040;
                        uELED2.Color := $00004040;
                        uELED3.Color := clLime;
                   end;
	end;
        with LvlGraph do begin
                Clear;
                for Cnt1 := 0 to High(Nod) do
                        for Cnt2 := 0 to High(Nod[Cnt1].Edg) do
                                if Nod[Cnt1].Edg[Cnt2].Flg and 1 > 0 then Graph.GetEdge(Nod[Cnt1].Com, Nod[Ap.Idx(Nod[Cnt1].Edg[Cnt2].ID)].Com, true);
                NodeStyle.Shape := lgnsEllipse;
                EdgeStyle.Color := clGray;
                EdgeStyle.HighlightColor := clHighLight;
                NodeStyle.Width := 15;
                NodeStyle.GapTop := 8;
                NodeStyle.CaptionPosition := lgncBottom;
                Font.Size := 11;
        end;
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.2. AMonitor - Check current status of Nod and Undo chain and show it in console window
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.AMonitor();
var
	Cnt1, Cnt2, Cnt3: longint;
	AsDs, RowNo: string;
begin
        if GAllowUndo = 1 then begin
		InsertColorStyledText(SCon, LineEnding + '  Current GUndo: ' + IntToStr(GUndo) + LineEnding +
                        '  --------------------------------------------------------------------------------------------------' + LineEnding, clYellow, [], -1);
		for Cnt1 := 0 to High(Undo) do begin
			InsertColorStyledText(SCon, '  Step ' + IntToStr(Cnt1) + ': ', clYellow, [], -1);
			for Cnt2 := 0 to High(Undo[Cnt1]) do begin
				InsertColorStyledText(SCon, '  Node ' + IntToStr(Cnt2) + ' ID' + IntToStr(Undo[Cnt1, Cnt2].ID) + ' "' +
                                        Undo[Cnt1, Cnt2].Com + '"; Flg ' + Copy(ByteToBitStr(Undo[Cnt1, Cnt2].Flg), 6, 3) + '; ', clYellow, [], -1);
				for Cnt3 := 0 to High(Undo[Cnt1, Cnt2].Edg) do begin
					if (Undo[Cnt1, Cnt2].Edg[Cnt3].Flg and 1) > 0 then AsDs := '↓' else AsDs := '↑';
					InsertColorStyledText(SCon, 'Edge #' + IntToStr(Cnt3) + ' ' + AsDs + ' ID' +
                                                IntToStr(Undo[Cnt1, Cnt2].Edg[Cnt3].ID) + '; ', clYellow, [], -1);
				end;
				InsertColorStyledText(SCon, LineEnding, clYellow, [], -1);
			end;
			InsertColorStyledText(SCon, '  --------------------------------------------------------------------------------------------------' +
                                LineEnding, clYellow, [], -1);
			SCon.SelStart := Length(SCon.Text)-1;
		end;
                RowNo := IntToStr(SCon.Lines.Count);
                if Length(RowNo) < 4 then RowNo := StringOfChar('0',(4 - Length(RowNo))) + RowNo;
                InsertColorStyledText(SCon, '  ' + RowNo + ' > ', clWhite,[], -1);
	end
	else begin
                SCon.Lines.Delete(SCon.Lines.Count - 1);
		InsertColorStyledText(SCon, LineEnding + LineEnding + '  Current Nodes count: ' + IntToStr(Length(Nod)) +
                        '  Current Edges count: ' + IntToStr(GEdgeCnt) + LineEnding +
                        '  --------------------------------------------------------------------------------------------------' + LineEnding, clYellow, [], -1);
		for Cnt1 := 0 to High(Nod) do begin
			InsertColorStyledText(SCon, '  Node ' + IntToStr(Cnt1) + ' ID' + IntToStr(Nod[Cnt1].ID) + ' "' + Nod[Cnt1].Com + '"; Flg ' +
                                Copy(ByteToBitStr(Nod[Cnt1].Flg), 6, 3) + '; ', clYellow, [], -1);
			for Cnt2 := 0 to High(Nod[Cnt1].Edg) do begin
				if (Nod[Cnt1].Edg[Cnt2].Flg and 1) > 0 then AsDs := '↓' else AsDs := '↑';
				InsertColorStyledText(SCon, 'Edge #' + IntToStr(Cnt2) + ' ' + AsDs + ' ID' + IntToStr(Nod[Cnt1].Edg[Cnt2].ID) + '; ', clYellow, [], -1);
			end;
			InsertColorStyledText(SCon, LineEnding, clYellow, [], -1);
		end;
		InsertColorStyledText(SCon, '  --------------------------------------------------------------------------------------------------' +
                        LineEnding, clYellow, [], -1);
                RowNo := IntToStr(SCon.Lines.Count);
                if Length(RowNo) < 4 then RowNo := StringOfChar('0',(4 - Length(RowNo))) + RowNo;
                InsertColorStyledText(SCon, '  ' + RowNo + ' > ', clWhite,[], -1);
	end;
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.3. ALog - Log message handling
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.ALog(Msg: integer);
var
	UndoStr, EType, RowNo, CurrNode, TargNode, LtrS0, LtrS1, LtrS2, LtrS3: string;
begin
	if GAllowUndo = 1 then UndoStr := 'Step ' + IntToStr(GUndo) + ': ' else UndoStr := '';
        if ETp.Text = '' then EType := '0' else EType := ETp.Text;
        if Length(Nod) > 0 then CurrNode := IntToStr(Nod[GCurrIdx].ID) else CurrNode := '0';
        if TNodID.Text = '' then TargNode := '(Undefined/New)' else TargNode := IntToStr(Nod[GTargIdx].ID);
        if SCon.Lines.Count > 1 then begin
                SCon.Lines.Delete(SCon.Lines.Count - 1);
                RowNo := IntToStr(SCon.Lines.Count);
                if Length(RowNo) < 4 then RowNo := StringOfChar('0',(4 - Length(RowNo))) + RowNo;
                InsertColorStyledText(SCon, LineEnding + '  ' + RowNo, clWhite,[], -1);
                InsertColorStyledText(SCon, '   '+ FormatDateTime('YYYY/MM/DD hh:mm:ss:zzz', Now), clLime,[], -1);
	end;
        LtrS0 := '';
        LtrS1 := '';
        LtrS2 := '';
        LtrS3 := '';
        if (Length(Nod) - GNodDCnt) <> 1 then LtrS0 := 's';
        if GEdgeCnt <> 1 then LtrS1 := 's';
        if Length(Nod) <> 1 then LtrS2 := 's';
        if Length(Eidon) <> 1 then LtrS3 := 's';
	case Msg of
		// TAesthesiaForm.FormActivate
                00: begin
		    	InsertColorStyledText(SCon, '  SESSION ' + FormatDateTime('YYYY/MM/DD hh:mm:ss:zzz', Now) + LineEnding, clYellow,[], -1);
                        RowNo := IntToStr(SCon.Lines.Count);
                        if Length(RowNo) < 4 then RowNo := StringOfChar('0',(4 - Length(RowNo))) + RowNo;
                        InsertColorStyledText(SCon, '  ' + RowNo + '   ', clWhite,[], -1);
                        InsertColorStyledText(SCon, FormatDateTime('YYYY/MM/DD hh:mm:ss:zzz', Now), clLime,[], -1);
		    	InsertColorStyledText(SCon, '   [00] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
                        InsertColorStyledText(SCon, 'Load knowledge base from IDS file "' + GIDSFile + '".', clLime, [], -1);
		end;
		01: begin
		    	InsertColorStyledText(SCon, '   [01] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'W: ', clAqua, [], -1);
                        InsertColorStyledText(SCon, 'IDS file "' + GIDSFile + '" not found. New IDS file created.', clLime, [], -1);
		end;
		02: begin
		    	InsertColorStyledText(SCon, '   [02] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'W: ', clAqua, [], -1);
		    	InsertColorStyledText(SCon, 'The file "' + GIDSFile + '" is empty. New IDS file created.', clLime, [], -1);
                end;
		03: begin
		    	InsertColorStyledText(SCon, '   [03] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'W: ', clAqua, [], -1);
                    	InsertColorStyledText(SCon, 'The file "' + GIDSFile + '" is not a valid IDS v.0.16 database.' + ' Renamed to "' + GIDSFile +
			'.old" and new IDS file created.', clLime, [], -1);
		end;
		04: begin
		    	InsertColorStyledText(SCon, '   [04] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'W: ', clAqua, [], -1);
                    	InsertColorStyledText(SCon, 'CRC32 check failed for the file "' + GIDSFile + '". Renamed to "' + GIDSFile +
			'.old" and new IDS file created.', clLime, [], -1);
		end;
		05: begin
		    	InsertColorStyledText(SCon, '   [05] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
                    	InsertColorStyledText(SCon, UndoStr + 'Session started with IDS file "' + GIDSFile + '" opened with CRC32 ' + IntToStr(GCRC32) +
			' containing base of ' + IntToStr(Length(Nod) - GNodDCnt) + ' node'+LtrS0+' and ' + IntToStr(GEdgeCnt) + ' edge'+LtrS1+'.', clLime, [], -1);
		end;
		// TAesthesiaForm.CNodIDKeyPress, TAesthesiaForm.CNodIDEdit
		06: begin
		    	InsertColorStyledText(SCon, '   [06] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
                    	InsertColorStyledText(SCon, 'Change the Current Node.', clLime, [], -1);
		end;
		07: begin
		    	InsertColorStyledText(SCon, '   [07] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'W: ', clAqua, [], -1);
                    	InsertColorStyledText(SCon, 'Node ' + CNodID.Text + ' not found. Set to 0.', clLime, [], -1);
		end;
		08: begin
		    	InsertColorStyledText(SCon, '   [08] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'W: ', clAqua, [], -1);
                    	InsertColorStyledText(SCon, 'Node ' + CNodID.Text + ' "' + Nod[Ap.Idx(StrToInt(CNodID.Text))].Com + '" is deleted. Set to 0.', clLime, [], -1);
		end;
		09: begin
		    	InsertColorStyledText(SCon, '   [09] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
                    	InsertColorStyledText(SCon, 'New Current Node ' + CurrNode + ' "' + Nod[GCurrIdx].Com + '".', clLime, [], -1);
		end;
		// TAesthesiaForm.CNodComEdit
                10: begin
		    	InsertColorStyledText(SCon, '   [0A] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
                    	InsertColorStyledText(SCon, 'Change the Current Node comment.', clLime, [], -1);
		end;
		11: begin
		    	InsertColorStyledText(SCon, '   [0B] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
                    	InsertColorStyledText(SCon, UndoStr + 'New comment for the Current Node ' + CurrNode + ': "' + CNodCom.Text + '".', clLime, [], -1);
		end;
		// TAesthesiaForm.TNodIDKeyPress, TAesthesiaForm.TNodIDEdit
		12: begin
		    	InsertColorStyledText(SCon, '   [0C] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
                    	InsertColorStyledText(SCon, 'Change the Target Node.', clLime, [], -1);
		end;
		13: begin
		    	InsertColorStyledText(SCon, '   [0D] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'W: ', clAqua, [], -1);
                    	InsertColorStyledText(SCon, 'Node ' + TargNode + ' not found. Set to none.', clLime, [], -1);
		end;
		14: begin
		    	InsertColorStyledText(SCon, '   [0E] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'W: ', clAqua, [], -1);
                    	InsertColorStyledText(SCon, 'Node ' + TargNode + ' "' + Nod[Ap.Idx(StrToInt(TargNode))].Com + '" is deleted. Set to none.', clLime, [], -1);
		end;
		15: begin
		    	InsertColorStyledText(SCon, '   [0F] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'E: ', clRed, [], -1);
                    	InsertColorStyledText(SCon, 'Cyclic edges are not allowed.', clLime, [], -1);
		end;
		16: begin
		    	InsertColorStyledText(SCon, '   [10] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'W: ', clAqua, [], -1);
                    	InsertColorStyledText(SCon, 'No Target Node specified for the Current Node ' + CNodID.Text + ' "' + CNodCom.Text + '".', clLime, [], -1);
		end;
		17: begin
		    	InsertColorStyledText(SCon, '   [11] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
                    	InsertColorStyledText(SCon, 'New Target Node ' + TargNode + ' "' + Nod[GTargIdx].Com + '".', clLime, [], -1);
		end;
		// TAesthesiaForm.ETpKeyPress, TAesthesiaForm.TETpEdit
		18: begin
		    	InsertColorStyledText(SCon, '   [12] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
                    	InsertColorStyledText(SCon, 'Change edge type.', clLime, [], -1);
		end;
		19: begin
		    	InsertColorStyledText(SCon, '   [13] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'W: ', clAqua, [], -1);
                    	InsertColorStyledText(SCon, 'Wrong edge type ' + EType + ', set to 0 "' + Nod[0].Com + '".', clLime, [], -1);
		end;
		20: begin
		    	InsertColorStyledText(SCon, '   [14] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'W: ', clAqua, [], -1);
                    	InsertColorStyledText(SCon, 'No Target Node specified. New node and edge?', clLime, [], -1);
		end;
		21: begin
		    	InsertColorStyledText(SCon, '   [15] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
                    	InsertColorStyledText(SCon, UndoStr + 'Type of ascending edge from the Current Node ' + CNodID.Text + ' "' + CNodCom.Text +
			'" to the Target Node ' + TargNode + ' "' + GTargComment + '" set to ' + EType + ' "' + Nod[Ap.Idx(StrToInt(EType))].Com + '".', clLime, [], -1);
		end;
		22: begin
		    	InsertColorStyledText(SCon, '   [16] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
                    	InsertColorStyledText(SCon, UndoStr + 'Type of descending edge from the current Node ' + CNodID.Text + ' "' + CNodCom.Text +
			'" to the Target Node ' + TargNode + ' "' + GTargComment + '" set to ' + EType + ' "' + Nod[Ap.Idx(StrToInt(EType))].Com + '".', clLime, [], -1);
		end;
		23: begin
		    	InsertColorStyledText(SCon, '   [17] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'W: ', clAqua, [], -1);
                    	InsertColorStyledText(SCon, 'The Current Node ' + CNodID.Text + ' "' + CNodCom.Text + '" has no edge with the Target Node ' +
                        TargNode + ' "' + GTargComment + '". New edge?', clLime, [], -1);
		end;
		// TAesthesiaForm.TNodComEdit
                24: begin
		    	InsertColorStyledText(SCon, '   [18] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
                    	InsertColorStyledText(SCon, 'Change the Target Node comment.', clLime, [], -1);
		end;
		25: begin
		    	InsertColorStyledText(SCon, '   [19] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
                    	InsertColorStyledText(SCon, UndoStr + 'New comment for the Target Node ' + TargNode + ': "' + GTargComment + '".', clLime, [], -1);
		end;
		26: begin
		    	InsertColorStyledText(SCon, '   [1A] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'W: ', clAqua, [], -1);
                    	InsertColorStyledText(SCon, 'No Target Node defined. New Target Node?', clLime, [], -1);
		end;
		// TAesthesiaForm.AddAscEdgBtnClick, TAesthesiaForm.AddDscEdgBtnClick
		27: begin
		    	InsertColorStyledText(SCon, '   [1B] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
                    	InsertColorStyledText(SCon, 'Add ascending edge of type ' + EType + ' "' + Nod[Ap.Idx(StrToInt(EType))].Com + '" from the Current Node ' +
                        CNodID.Text + ' "' + CNodCom.Text + '" up to the Target Node ' + TargNode + ' "' + GTargComment + '".', clLime, [], -1);
		end;
		28: begin
		    	InsertColorStyledText(SCon, '   [1C] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
                    	InsertColorStyledText(SCon, 'Add descending edge of type ' + EType + ' "' + Nod[Ap.Idx(StrToInt(EType))].Com + '" from the Current Node ' +
                        CNodID.Text + ' "' + CNodCom.Text + '" down to the Target Node ' + TargNode + ' "' + GTargComment + '".', clLime, [], -1);
		end;
		29: begin
		    	InsertColorStyledText(SCon, '   [1D] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'E: ', clRed, [], -1);
                    	InsertColorStyledText(SCon, 'Node 0 cannot have ascending edges.' + ' No edge added.', clLime, [], -1);
		end;
		30: begin
		    	InsertColorStyledText(SCon, '   [1E] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'E: ', clRed, [], -1);
                    	InsertColorStyledText(SCon, 'Duplicate edges are not allowed.' + ' No edge added.', clLime, [], -1);
		end;
		31: begin
		    	InsertColorStyledText(SCon, '   [1F] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'E: ', clRed, [], -1);
                    	InsertColorStyledText(SCon, 'Target Node ' + TargNode + ' doesn''t exist.' + ' No edge added.', clLime, [], -1);
		end;
		32: begin
		    	InsertColorStyledText(SCon, '   [20] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
                    	InsertColorStyledText(SCon, UndoStr + 'New node ' + IntToStr(Nod[GTargIdx].ID) + ' "' + Nod[GTargIdx].Com + '" added.', clLime, [], -1);
		end;
		33: begin
		    	InsertColorStyledText(SCon, '   [21] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
                    	InsertColorStyledText(SCon, UndoStr + 'Ascending edge of type ' + EType + ' "' + Nod[Ap.Idx(StrToInt(EType))].Com + '" from the Current Node ' +
                        CNodID.Text + ' "' + CNodCom.Text + '" up to the Target Node ' + IntToStr(Nod[GTargIdx].ID) + ' "' + GTargComment + '" added.', clLime, [], -1);
		end;
		34: begin
		    	InsertColorStyledText(SCon, '   [22] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
                    	InsertColorStyledText(SCon, UndoStr + 'Descending edge of type ' + EType + ' "' + Nod[Ap.Idx(StrToInt(EType))].Com + '" from the Current Node ' +
                        CNodID.Text + ' "' + CNodCom.Text + '" down to the Target Node ' + IntToStr(Nod[GTargIdx].ID) + ' "' + GTargComment + '" added.', clLime, [], -1);
		end;
		// TAesthesiaForm.DelCNodBtnClick
		35: begin
		    	InsertColorStyledText(SCon, '   [23] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
                    	InsertColorStyledText(SCon, 'Delete the current node ' + CurrNode + ' "' + Nod[GCurrIdx].Com + '".', clLime, [], -1);
		end;
		36: begin
		    	InsertColorStyledText(SCon, '   [24] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'E: ', clRed, [], -1);
                    	InsertColorStyledText(SCon, 'Node 0 cannot be deleted. Deletion failed.', clLime, [], -1);
		end;
		37: begin
		    	InsertColorStyledText(SCon, '   [25] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
                    	InsertColorStyledText(SCon, UndoStr + 'Node ' + IntToStr(Nod[GTargIdx].ID) + ' "' + Nod[GTargIdx].Com + '" deleted.', clLime, [], -1);
		end;
		// TAesthesiaForm.DelCEdgBtnClick
		38: begin
		    	InsertColorStyledText(SCon, '   [26] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
                    	InsertColorStyledText(SCon, 'Delete edge ' + CurrNode + ' "' + Nod[GCurrIdx].Com + '" - ' + TargNode + ' "' + Nod[GTargIdx].Com +
                        '".', clLime, [], -1);
		end;
		39: begin
		    	InsertColorStyledText(SCon, '   [27] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'E: ', clRed, [], -1);
                    	InsertColorStyledText(SCon, 'There is no current Edge to delete.', clLime, [], -1);
		end;
		40: begin
		    	InsertColorStyledText(SCon, '   [29] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'E: ', clRed, [], -1);
                    	InsertColorStyledText(SCon, UndoStr + 'Edge to Node 0 must be the last edge to delete. Deletion failed.', clLime, [], -1);
		end;
		// TAesthesiaForm.CommitBtnClick
		41: begin
		    	InsertColorStyledText(SCon, '   [29] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
                    	InsertColorStyledText(SCon, 'Commit base and clear the history of changes.', clLime, [], -1);
		end;
		42: begin
		    	InsertColorStyledText(SCon, '   [2A] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
                    	InsertColorStyledText(SCon, 'Base commited with ' + IntToStr(Length(Nod)) + ' node'+LtrS2+' and ' + IntToStr(GEdgeCnt) + ' edge'+LtrS1+'. ' +
                        'History of changes cleared.', clLime, [], -1);
		end;
		// TAesthesiaForm.ExitBtnClick
		43: begin
		    	InsertColorStyledText(SCon, '   [2B] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
                    	InsertColorStyledText(SCon, 'Save database and session log.', clLime, [], -1);
		end;
		44: begin
		    	InsertColorStyledText(SCon, '   [2C] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'A: ', clAqua, [], -1);
                    	InsertColorStyledText(SCon, 'Database saved to file "' + GIDSFile + '" with CRC32 ' + IntToStr(GCRC32) + ' containing base of ' +
                        IntToStr(Length (Nod) - GNodDCnt) + ' node'+LtrS0+' and ' + IntToStr(GEdgeCnt) + ' edge'+LtrS1+'.', clLime, [], -1);
		end;
		45: begin
		    	InsertColorStyledText(SCon, '   [2D] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
                    	InsertColorStyledText(SCon, 'Session log saved to file "Pyramidion 016.log", session closed.' + LineEnding, clLime, [], -1);
		end;
		// TAesthesiaForm.UndoBtnClick
		46: begin
		    	InsertColorStyledText(SCon, '   [2E] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
                    	InsertColorStyledText(SCon, 'Undo to the Step ' + IntToStr(GUndo), clLime, [], -1);
		end;
		// TAesthesiaForm.RedoBtnClick
		47: begin
		    	InsertColorStyledText(SCon, '   [2F] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
                    	InsertColorStyledText(SCon, 'Redo to the Step ' + IntToStr(GUndo), clLime, [], -1);
		end;
		// TAesthesiaForm.FormClose
		48: begin
		    	InsertColorStyledText(SCon, '   [30] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
                    	InsertColorStyledText(SCon, 'Session close without saving log and database.', clLime, [], -1);
		end;
		// 'Monitor' event
		49: begin
		    	InsertColorStyledText(SCon, '   [31] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
                    	InsertColorStyledText(SCon, 'Monitor', clLime, [], -1);
		end;
		50: begin
		    	InsertColorStyledText(SCon, '   [32] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
                    	InsertColorStyledText(SCon, 'Monitor query completed.', clLime, [], -1);
		end;
		// Processing console query
		51: begin
		    	InsertColorStyledText(SCon, '   [33] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
                    	InsertColorStyledText(SCon, 'Processing console query.', clLime, [], -1);
		end;
		// Reading Event Strip (Eidetika) from EID file
		52: begin
		    	InsertColorStyledText(SCon, '   [34] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'W: ', clAqua, [], -1);
                    	InsertColorStyledText(SCon, 'EID file "' + LeftStr(GIDSFile, Length(GIDSFile) - 3) + 'eid" not found. New EID file created.', clLime, [], -1);
		end;
		53: begin
		    	InsertColorStyledText(SCon, '   [35] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'W: ', clAqua, [], -1);
                    	InsertColorStyledText(SCon, 'W: The file "' + LeftStr(GIDSFile, Length(GIDSFile) - 3) + 'eid" is empty. New EID file created.', clLime, [], -1);
		end;
		54: begin
		    	InsertColorStyledText(SCon, '   [36] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'W: ', clAqua, [], -1);
                    	InsertColorStyledText(SCon, 'The file "' + LeftStr(GIDSFile, Length(GIDSFile) - 3) + 'eid" is not a valid EID v.0.16 file. Renamed to "' +
                        LeftStr(GIDSFile, Length(GIDSFile) - 3) + 'eid.old" and new EID file created.', clLime, [], -1);
		end;
		55: begin
		    	InsertColorStyledText(SCon, '   [37] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'W: ', clAqua, [], -1);
                    	InsertColorStyledText(SCon, 'CRC32 check failed for the file "' + LeftStr(GIDSFile, Length(GIDSFile) - 3) + 'eid". Renamed to "' +
                        LeftStr(GIDSFile, Length(GIDSFile) - 3) + 'eid.old" and new EID file created.', clLime, [], -1);
		end;
		56: begin
		    	InsertColorStyledText(SCon, '   [38] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
                    	InsertColorStyledText(SCon, UndoStr + 'Eidetika EID file "' + LeftStr(GIDSFile, Length(GIDSFile) - 3) + 'eid" opened containing ' +
                        IntToStr(Length(Eidon)) + ' record'+LtrS3+'.', clLime, [], -1);
		end;
		// Saving Event Strip (Eidetika) to EID file
		57: begin
		    	InsertColorStyledText(SCon, '   [39] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
                        InsertColorStyledText(SCon, 'Events strip saved to file "' + LeftStr(GIDSFile, Length(GIDSFile) - 3) + 'eid" containing ' +
                        IntToStr(Length (Eidon)) + ' record'+LtrS3+'.', clLime, [], -1);
		end;
		// Editing the current node flags
		58: begin
		    	InsertColorStyledText(SCon, '   [3A] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
                        InsertColorStyledText(SCon, 'Edit the current node flags "' + ByteToBitStr(GCNodFlg) + '" .', clLime, [], -1);
		end;
		59: begin
		    	InsertColorStyledText(SCon, '   [3B] ', clGreen, [], -1);
                        InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
                        InsertColorStyledText(SCon, 'Current node flags edited to "' + CNodFlg.Text + '".', clLime, [], -1);
		end;
	end;
        RowNo := IntToStr(SCon.Lines.Count);
        if Length(RowNo) < 4 then RowNo := StringOfChar('0',(4 - Length(RowNo))) + RowNo;
        InsertColorStyledText(SCon, LineEnding + '  ' + RowNo + ' > ', clWhite,[], -1);
        SCon.SelStart := Length(SCon.Lines.Text);
        SConStatLbl.Caption := '   Row: ' + IntToStr(SCon.CaretPos.y) + '  Column: ' + IntToStr(SCon.CaretPos.x);
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.4. SConEdit - Console input done
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	Note: With accessory procedures SConClick, SConKeyUp and SConKeyPress to catch cursor position, arrow keys and #13 'Return' and #27 'Escape' keys
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.SConClick(Sender: TObject);
begin
        SConStatLbl.Caption := '   Row: ' + IntToStr(SCon.CaretPos.y) + '  Column: ' + IntToStr(SCon.CaretPos.x);
end;
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	Note 1: Arrow keys do not generate KeyPress event, only KeyDown and KeyUp
//	Note 2: KeyUp is the only key event processed after caret position update, which is essential when catching arrow keys
//	Note 3: Arrow keys are #37 [Left]; #38 [Up], #39 [Right], and #40 [Down]
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.SConKeyUp(Sender: TObject);
begin
        SConStatLbl.Caption := '   Row: ' + IntToStr(SCon.CaretPos.y) + '  Column: ' + IntToStr(SCon.CaretPos.x);
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.SConKeyPress(Sender: TObject; var Key: char);
var	CurrLine, RowNo: string;
begin
        if Key = #13 then begin
                CurrLine := String(SCon.Lines[SCon.Lines.Count - 1]);
                CurrLine := RightStr(CurrLine, Length(Currline) - 9);
                if Length(CurrLine) > 0 then begin
                        SCon.Lines.Delete(SCon.Lines.Count - 1);
                        RowNo := IntToStr(SCon.Lines.Count);
                        if Length(RowNo) < 4 then RowNo := StringOfChar('0',(4 - Length(RowNo))) + RowNo;
                        InsertColorStyledText(SCon, LineEnding + '  ' + RowNo, clWhite,[], -1);
                        InsertColorStyledText(SCon, '   '+ FormatDateTime('YYYY/MM/DD hh:mm:ss:zzz', Now), clLime,[], -1);
                        InsertColorStyledText(SCon, '   Q: ', clYellow,[], -1);
                        InsertColorStyledText(SCon, CurrLine, clGreen, [], -1);
                        RowNo := IntToStr(SCon.Lines.Count);
                        if Length(RowNo) < 4 then RowNo := StringOfChar('0',(4 - Length(RowNo))) + RowNo;
                        InsertColorStyledText(SCon, LineEnding + '  ' + RowNo + ' > ', clWhite,[], -1);
                        SCon.SelStart := Length(SCon.Lines.Text);
                        GQueryAsked := True;
                        SConEdit;
		end;
                Key := #0;
	end;
	if Key = #27 then begin
                SCon.Lines.Delete(SCon.Lines.Count - 1);
                RowNo := IntToStr(SCon.Lines.Count);
                if Length(RowNo) < 4 then RowNo := StringOfChar('0',(4 - Length(RowNo))) + RowNo;
                InsertColorStyledText(SCon, LineEnding + '  ' + RowNo + ' > ', clWhite,[], -1);
                SCon.SelStart := Length(SCon.Lines.Text);
	end;
        if SCon.CaretPos.Y < (SCon.Lines.Count - 1) then Key := #0;
end;
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.SConEdit();
begin
        if GQueryAsked then begin
                ALog(Ap.Proseychi);
                GQueryAsked := False;
                Redraw;
	end;
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.5. CNodIDKeyPress - Filter for current node ID editing
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.CNodIDKeyPress(Sender: TObject; var Key: char);
begin
	if not (Key in ['0'..'9', #8, #9, #13, #27]) then Key := #0;					// #8 Backspace, #9 Tab, #13 Return, #27 Escape
	if Key = #27 then CNodID.Text := IntToStr(Nod[GCurrIdx].ID);
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.6. CNodIDEdit - Current node ID editing done
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.CNodIDEdit(Sender: TObject);
begin
        if CNodID.Text = '' then CNodID.Text := '0';
        if StrToInt(CNodID.Text) <> Nod[GCurrIdx].ID then begin
	        ALog(06);
                GQueryAsked := True;
        	SConEdit;
	end;
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.7. CNodFlgEdit - Current node comment editing done
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	Note: With accessory procedures CNodFlgEnter, CNodFlgExit to store previous comment
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.CNodFlgEnter(Sender: TObject);
begin
        GCNodFlg := Nod[GCurrIdx].Flg;
        CMouseEnter(CNodFlg);
end;

procedure TAesthesiaForm.CNodFlgKeyPress(Sender: TObject; var Key: char);
begin
	if not (Key in ['0', '1', #8, #9, #13, #27]) then Key := #0;					// #8 Backspace, #9 Tab, #13 Return, #27 Escape
	if Key = #27 then CNodFlg.Text := ByteToBitStr(Nod[GCurrIdx].Flg);
end;

procedure TAesthesiaForm.CNodFlgEditingDone(Sender: TObject);
begin
        CNodFlg.Text := RightStr(CNodFlg.Text, 8);
        if Length(CNodFlg.Text) < 8 then CNodFlg.Text := StringOfChar('0',(4 - Length(CNodFlg.Text))) + CNodFlg.Text;
        Nod[GCurrIdx].Flg := BitStrToByte(CNodFlg.Text);
        ALog(58);
        GQueryAsked := True;
        SConEdit;
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.7. CNodComEdit - Current node comment editing done
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	Note: With accessory procedure CNodComEnter to store previous comment
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.CNodComEnter(Sender: TObject);
begin
	GCurrComment := CNodCom.Text;
	CMouseEnter(CNodCom);
end;
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	Note: These global variables must be set properly - GCurrIdx, GTargIdx
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.CNodComEdit(Sender: TObject);
begin
	if CNodCom.Text <> GCurrComment then begin
		ALog(10);
                GQueryAsked := True;
                SConEdit;
	end;
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.8. TNodIDKeyPress - Filter for target node ID editing
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.TNodIDKeyPress(Sender: TObject; var Key: char);
begin
	if not (Key in ['0'..'9', #8, #9, #13, #27]) then Key := #0;
	if Key = #27 then if GTargIdx > -1 then TNodID.Text := IntToStr(GTargIdx) else TNodID.Text := '';
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.9. TNodIDEdit - Target node ID editing done
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.TNodIDEdit(Sender: TObject);
var
	NewTID, OldTID: longint;
begin
        if TNodID.Text = '' then NewTID := -1 else NewTID := StrToInt(TNodID.Text);
        if GTargIdx = -1 then OldTID := -1 else OldTID := Nod[GTargIdx].ID;
        if NewTID <> OldTID then begin
	        ALog(12);
                GQueryAsked := True;
        	SConEdit;
	end;
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.10. ETpKeyPress - Filter for target edge type editing
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.ETpKeyPress(Sender: TObject; var Key: char);
begin
	if not (Key in ['0'..'9', #8, #9, #13, #27]) then Key := #0;
	if Key = #27 then if GCurrEdge > -1 then ETp.Text := IntToStr(Nod[GCurrIdx].Edg[GCurrEdge].ETp) else ETp.Text := '';
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.11. TETpEdit - Target edge type editing done
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.TETpEdit(Sender: TObject);
begin
        ALog(18);
        GQueryAsked := True;
        SConEdit;
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.13. AddAscEdgBtnClick - Add ascending edge button click
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.AddAscEdgBtnClick(Sender: TObject);
begin
        ALog(27);
        GQueryAsked := True;
        SConEdit;
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.14. AddDscEdgBtnClick - Add descending edge button click
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.AddDscEdgBtnClick(Sender: TObject);
begin
        Alog(28);
        GQueryAsked := True;
        SConEdit;
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.15. DelCNodBtnClick - Delete current node button click
//	Note: These global pointers must be set correctly - GCurrIdx
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.DelCNodBtnClick(Sender: TObject);
begin
        ALog(35);
        GQueryAsked := True;
        SConEdit;
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.16. DelCEdgBtnClick - Delete current edge button click
//	Note: These global pointers must be set correctly - GCurrIdx, GTargIdx, GCurrEdge, GTargEdge
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.DelCEdgBtnClick(Sender: TObject);
begin
        if GCurrEdge > -1 then begin
                ALog(38);
                GQueryAsked := True;
                SConEdit;
	end
	else ALog(39);
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.17. SNodGrdMouseDown - superior nodes grid mouse down
//	Note: calls PopMenu on the right click
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.SNodGrdMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var CurrRow, CurrCol: integer;
begin
	with SNodGrd do begin
        	CurrCol := 0;
                CurrRow := 0;
		MouseToCell(X, Y, CurrCol, CurrRow);
		Row := CurrRow;
		Col := CurrCol;
        end;
	if (Button = mbRight) or (ssRight in Shift) then begin
        	if SNodGrd.Row > 0 then begin
                	SetCurrI.Visible := True;
                        DelCEdgI.Visible := True;
                	DelNodI.Visible := True;
		end
        	else begin
        		SetCurrI.Visible := False;
                        DelCEdgI.Visible := False;
        		DelNodI.Visible := False;
        	end;
		AddSEdgI.Visible := False;
                AddIEdgI.Visible := False;
                AddSNodI.Visible := True;
                AddINodI.Visible := False;
                AddUNodI.Visible := False;
                PopMenu.Popup(X+1028,Y+77);
	end;
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.18. SNodGrdDblClick - superior nodes grid double click
//	Note: switches goEditing property on to allow in-cell editing
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.SNodGrdDblClick(Sender: TObject);
begin
        with SNodGrd do begin
                GTargIdx := Ap.Idx(StrToInt(Cells[1, Row]));
        	GTargComment := Cells[3, Row];
        	if Col = 3 then Options := Options + [goEditing] + [goAlwaysShowEditor];
	end;
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.19. SNodGrdEditingDone - superior nodes grid cell editing done
//	Note: switches goEditing property off to forbid in-cell editing
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.SNodGrdEditingDone(Sender: TObject);
begin
	with SNodGrd do begin
                Options := Options - [goEditing] - [goAlwaysShowEditor];
                if Cells[Col, Row] <> GTargComment then begin
                        ALog(24);
                        GQueryAsked := True;
		end;
		SConEdit;
	end;
        TNodID.Text := '';
        TNodIDEdit(TNodID);
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.20. INodGrdMouseDown - inferior nodes grid mouse down
//	Note: calls PopMenu on the right click
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.INodGrdMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var CurrRow, CurrCol: integer;
begin
	with INodGrd do begin
        	CurrCol := 0;
                CurrRow := 0;
		MouseToCell(X, Y, CurrCol, CurrRow);
		Row := CurrRow;
		Col := CurrCol;
        end;
	if (Button = mbRight) or (ssRight in Shift) then begin
        	if INodGrd.Row > 0 then begin
                	SetCurrI.Visible := True;
                        DelCEdgI.Visible := True;
                	DelNodI.Visible := True;
		end
        	else begin
        		SetCurrI.Visible := False;
                        DelCEdgI.Visible := False;
        		DelNodI.Visible := False;
        	end;
                AddSEdgI.Visible := False;
                AddIEdgI.Visible := False;
                AddSNodI.Visible := False;
                AddINodI.Visible := True;
                AddUNodI.Visible := False;
                PopMenu.Popup(X+1028,Y+340);
	end;
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.21. INodGrdDblClick - inferior nodes grid double click
//	Note: switches goEditing property on to allow in-cell editing
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.INodGrdDblClick(Sender: TObject);
begin
        with INodGrd do begin
                GTargIdx := Ap.Idx(StrToInt(Cells[1, Row]));
        	GTargComment := Cells[3, Row];
        	if Col = 3 then Options := Options + [goEditing] + [goAlwaysShowEditor];
	end;
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.22. INodGrdEditingDone - inferior nodes grid cell editing done
//	Note: switches goEditing property off to forbid in-cell editing
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.INodGrdEditingDone(Sender: TObject);
begin
	with INodGrd do begin
                Options := Options - [goEditing] - [goAlwaysShowEditor];
                if Cells[Col, Row] <> GTargComment then begin
                        ALog(24);
                        GQueryAsked := True;
		end;
		SConEdit;
	end;
        TNodID.Text := '';
        TNodIDEdit(TNodID);
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.23. UNodGrdMouseDown - unlinked nodes grid mouse down
//	Note: calls PopMenu on the right click
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.UNodGrdMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var CurrRow, CurrCol: integer;
begin
	with UNodGrd do begin
        	CurrCol := 0;
                CurrRow := 0;
		MouseToCell(X, Y, CurrCol, CurrRow);
		Row := CurrRow;
		Col := CurrCol;
        end;
	if (Button = mbRight) or (ssRight in Shift) then begin
        	if UNodGrd.Row > 0 then begin
                	SetCurrI.Visible := True;
                        AddSEdgI.Visible := True;
                        AddIEdgI.Visible := True;
                	DelNodI.Visible := True;
		end
        	else begin
        		SetCurrI.Visible := False;
                        AddSEdgI.Visible := False;
                        AddIEdgI.Visible := False;
        		DelNodI.Visible := False;
        	end;
                DelCEdgI.Visible := False;
                AddSNodI.Visible := False;
                AddINodI.Visible := False;
                AddUNodI.Visible := True;
                PopMenu.Popup(X+1028,Y+584);
	end;
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.24. UNodGrdDblClick - unlinked nodes grid double click
//	Note: switches goEditing property on to allow in-cell editing
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.UNodGrdDblClick(Sender: TObject);
begin
        with UNodGrd do begin
                GTargIdx := Ap.Idx(StrToInt(Cells[1, Row]));
        	GTargComment := Cells[3, Row];
        	if Col = 3 then Options := Options + [goEditing] + [goAlwaysShowEditor];
	end;
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.25. UNodGrdEditingDone - unlinked nodes grid cell editing done
//	Note: switches goEditing property off to forbid in-cell editing
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.UNodGrdEditingDone(Sender: TObject);
begin
	with UNodGrd do begin
                Options := Options - [goEditing] - [goAlwaysShowEditor];
                if Cells[Col, Row] <> GTargComment then begin
                        ALog(24);
                        GQueryAsked := True;
		end;
		SConEdit;
	end;
        TNodID.Text := '';
        TNodIDEdit(TNodID);
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.17. SNodGrdKeyPress - Superior nodes grid keypress
//	Note: On 'Enter' (#13) key the target node is set from the current row, and on 'Space' (#32) key the current node is set from the current row.
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//procedure TAesthesiaForm.SNodGrdKeyPress(Sender: TObject; var Key: char);
//begin
//        if SNodGrd.Row > 0 then if Key = #13 then begin
//                TNodID.Text := IntToStr(Ap.Idx(StrToInt(SNodGrd.Cells[1, SNodGrd.Row])));
//                ALog(12);
//                GQueryAsked := True;
//                SConEdit;
//	end
//	else if Key = #32 then begin
//                CNodID.Text := IntToStr(Ap.Idx(StrToInt(SNodGrd.Cells[1, SNodGrd.Row])));
//                ALog(06);
//                GQueryAsked := True;
//                SConEdit;
//	end;
//end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.18. SNodGrdDblClick - Superior nodes grid double click
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//procedure TAesthesiaForm.SNodGrdDblClick();
//begin
//	if SNodGrd.Row > 0 then begin
//                TNodID.Text := IntToStr(Ap.Idx(StrToInt(SNodGrd.Cells[1, SNodGrd.Row])));
//                ALog(12);
//                GQueryAsked := True;
//                SConEdit;
//	end;
//end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.19. INodGrdKeyPress - Inferior nodes grid click
//	Note: On 'Enter' (#13) key the target node is set from the current row, and on 'Space' (#32) key the current node is set from the current row.
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//procedure TAesthesiaForm.INodGrdKeyPress(Sender: TObject; var Key: char);
//begin
//	if INodGrd.Row > 0 then if Key = #13 then begin
//                TNodID.Text := IntToStr(Ap.Idx(StrToInt(INodGrd.Cells[1, INodGrd.Row])));
//                ALog(12);
//                GQueryAsked := True;
//                SConEdit;
//	end
//	else if Key = #32 then begin
//                CNodID.Text := IntToStr(Ap.Idx(StrToInt(INodGrd.Cells[1, INodGrd.Row])));
//                ALog(06);
//                GQueryAsked := True;
//                SConEdit;
//	end;
//end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.20. INodGrdDblClick - Inferior nodes grid double click
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//procedure TAesthesiaForm.INodGrdDblClick();
//begin
//	if INodGrd.Row > 0 then begin
//                TNodID.Text := IntToStr(Ap.Idx(StrToInt(INodGrd.Cells[1, INodGrd.Row])));
//                ALog(12);
//                GQueryAsked := True;
//                SConEdit;
//	end;
//end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.21. UNodGrdClick - Unlinked nodes grid click
//	Note: On 'Enter' (#13) key the target node is set from the current row, and on 'Space' (#32) key the current node is set from the current row.
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//procedure TAesthesiaForm.UNodGrdKeyPress(Sender: TObject; var Key: char);
//begin
//	if UNodGrd.Row > 0 then if Key = #13 then begin
//                TNodID.Text := IntToStr(Ap.Idx(StrToInt(UNodGrd.Cells[1, UNodGrd.Row])));
//                ALog(12);
//                GQueryAsked := True;
//                SConEdit;
//	end
//	else if Key = #32 then begin
//                CNodID.Text := IntToStr(Ap.Idx(StrToInt(UNodGrd.Cells[1, UNodGrd.Row])));
//                ALog(06);
//                GQueryAsked := True;
//                SConEdit;
//	end;
//end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.22. UNodGrdDblClick - Unlinked nodes grid double click
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//procedure TAesthesiaForm.UNodGrdDblClick();
//begin
//	if UNodGrd.Row > 0 then begin
//                TNodID.Text := IntToStr(Ap.Idx(StrToInt(UNodGrd.Cells[1, UNodGrd.Row])));
//                ALog(12);
//                GQueryAsked := True;
//                SConEdit;
//	end;
//end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.23. CommitBtnClick - Commit button click
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.CommitBtnClick(Sender: TObject);
begin
        ALog(41);
        GQueryAsked := True;
        SConEdit;
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.24. ExitBtnClick - Exit button click
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.ExitBtnClick(Sender: TObject);
begin
	ALog(43);
        GQueryAsked := True;
        SConEdit;
	Close;
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.25. UndoBtnClick - Undo last action done (if the base not committed)
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.UndoBtnClick(Sender: TObject);
begin
	if GAllowUndo = 1 then if GUndo > 0 then begin
		GUndo := GUndo-1;
		Nod := Copy(Undo[GUndo]);
		AMonitor;
		GCurrIdx := 0;
		GTargIdx := -1;
		GCurrEdge := -1;
                GTargEdge := -1;
		Redraw;
		ALog(46);
	end;
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.26. RedoBtnClick - Redo last action undone (if the base not committed)
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.RedoBtnClick(Sender: TObject);
begin
	if GAllowUndo = 1 then if GUndo < High(Undo) then begin
		GUndo := GUndo + 1;
		Nod := Copy(Undo[GUndo]);
		AMonitor;
		GCurrIdx := 0;
		GTargIdx := -1;
		GCurrEdge := -1;
                GTargEdge := -1;
		Redraw;
		ALog(47);
	end;
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.27. FormClose - Form close without saving any logs or changes to IDS file
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.FormClose(Sender: TObject);
begin
	ALog(48);
        GQueryAsked := True;
	SConEdit;
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.28. MonitorBtnClick - Types all of the Nod array content to session console
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.MonitorBtnClick(Sender: TObject);
begin
        ALog(49);
        GQueryAsked := True;
        SConEdit;
end;

//==================================================================================================================================================================================
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	0.-1. Color manipulations for the TAesthesiaForm elements
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.CMouseEnter(Sender: TObject);
var
	PyramidionLogoImg: TPortableNetworkGraphic;
	RStream: TResourceStream;
begin
	if (Sender = CNodLbl) or (Sender = CNodIDLbl) or (Sender = CNodID) or (Sender = CNodFlgLbl) or (Sender = CNodFlg) or (Sender = CNodComLbl) or (Sender = CNodCom)
		then CNodLbl.Color := clHighlight;
	if (Sender = CNodIDLbl) or (Sender = CNodID) then CNodIDLbl.Color := clHighlight;
	if (Sender = CNodFlgLbl) or (Sender = CNodFlg) then CNodFlgLbl.Color := clHighlight;
	if (Sender = CNodComLbl) or (Sender = CNodCom) then CNodComLbl.Color := clHighlight;
        if (Sender = CEdgLbl) or (Sender = EdgDirLbl) or (Sender = EdgDir) or (Sender = TNodIDLbl) or (Sender = TNodID) or (Sender = ETpLbl) or (Sender = ETp) or (Sender = ETpTxt)
                then CEdgLbl.Color := clHighlight;
        if (Sender = EdgDirLbl) or (Sender = EdgDir) then EdgDirLbl.Color := clHighlight;
        if (Sender = TNodIDLbl) or (Sender = TNodID) then TNodIDLbl.Color := clHighlight;
        if (Sender = ETpLbl) or (Sender = ETp) or (Sender = ETpTxt) then ETpLbl.Color := clHighlight;
        if (Sender = AddAscEdgBtn) then AddAscEdgBtn.BevelColor := clHighlight;
        if (Sender = AddDscEdgBtn) then AddDscEdgBtn.BevelColor := clHighlight;
        if (Sender = DelCNodBtn) then DelCNodBtn.BevelColor := clHighlight;
        if (Sender = DelCEdgBtn) then DelCEdgBtn.BevelColor := clHighlight;
        if (Sender = CommitBtn) then CommitBtn.BevelColor := clHighlight;
        if (Sender = ExitBtn) then ExitBtn.BevelColor := clHighlight;
        if (Sender = MonitorBtn) then MonitorBtn.BevelColor := clHighlight;
        if (Sender = UndoBtn) then UndoBtn.BevelColor := clHighlight;
        if (Sender = RedoBtn) then RedoBtn.BevelColor := clHighlight;
        if (Sender = UndoLvlBtn) or (Sender = uELED1) or (Sender = uELED2) or (Sender = uELED3) then UndoLvlBtn.BevelColor := clHighlight;
        if (Sender = uELED1) then uELED1.Bright := True;
        if (Sender = uELED2) then uELED2.Bright := True;
        if (Sender = uELED3) then uELED3.Bright := True;
        if (Sender = PyramidionLogo) then begin
        	PyramidionLogoImg := TPortableNetworkGraphic.Create;
        	RStream := TResourceStream.Create(HINSTANCE, 'PyramidionLogo2', RT_RCDATA);
        	PyramidionLogoImg.LoadFromStream(RStream);
        	PyramidionLogo.Picture.Assign(PyramidionLogoImg);
        	RStream.Free;
        	PyramidionLogoImg.Free;
	end;
        if (Sender = SNodPnl) or (Sender = SNodGrd) then SNodPnl.BevelColor := clHighlight;
        if (Sender = SNodPnl) or (Sender = SNodGrd) then SNodLbl.Color := clHighlight;
        if (Sender = SNodGrd) then SNodGrd.Color := clActiveCaption;
        if (Sender = INodPnl) or (Sender = INodGrd) then INodPnl.BevelColor := clHighlight;
        if (Sender = INodPnl) or (Sender = INodGrd) then INodLbl.Color := clHighlight;
        if (Sender = INodGrd) then INodGrd.Color := clActiveCaption;
        if (Sender = UNodPnl) or (Sender = UNodGrd) then UNodPnl.BevelColor := clHighlight;
        if (Sender = UNodPnl) or (Sender = UNodGrd) then UNodLbl.Color := clHighlight;
        if (Sender = UNodGrd) then UNodGrd.Color := clActiveCaption;
        if (Sender = SConPnl) or (Sender = SCon) then SCon.Color := $1A00;
end;
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.CMouseLeave(Sender: TObject);
var
	PyramidionLogoImg: TPortableNetworkGraphic;
	RStream: TResourceStream;
begin
	if (Sender = CNodLbl) or (Sender = CNodIDLbl) or (Sender = CNodID) or (Sender = CNodFlgLbl) or (Sender = CNodFlg) or (Sender = CNodComLbl) or (Sender = CNodCom)
                then CNodLbl.Color := clMenuBar;
	if (Sender = CNodIDLbl) or (Sender = CNodID) then CNodIDLbl.Color := clBtnShadow;
	if (Sender = CNodFlgLbl) or (Sender = CNodFlg) then CNodFlgLbl.Color := clBtnShadow;
	if (Sender = CNodComLbl) or (Sender = CNodCom) then CNodComLbl.Color := clBtnShadow;
        if (Sender = CEdgLbl) or (Sender = EdgDirLbl) or (Sender = EdgDir) or (Sender = TNodIDLbl) or (Sender = TNodID) or (Sender = ETpLbl) or (Sender = ETp) or (Sender = ETpTxt)
                then CEdgLbl.Color := clMenuBar;
        if (Sender = EdgDirLbl) or (Sender = EdgDir) then EdgDirLbl.Color := clBtnShadow;
        if (Sender = TNodIDLbl) or (Sender = TNodID) then TNodIDLbl.Color := clBtnShadow;
        if (Sender = ETpLbl) or (Sender = ETp) or (Sender = ETpTxt) then ETpLbl.Color := clBtnShadow;
        if (Sender = AddAscEdgBtn) then AddAscEdgBtn.BevelColor := clInactiveCaption;
        if (Sender = AddDscEdgBtn) then AddDscEdgBtn.BevelColor := clInactiveCaption;
        if (Sender = DelCNodBtn) then DelCNodBtn.BevelColor := clInactiveCaption;
        if (Sender = DelCEdgBtn) then DelCEdgBtn.BevelColor := clInactiveCaption;
        if (Sender = CommitBtn) then CommitBtn.BevelColor := clInactiveCaption;
        if (Sender = ExitBtn) then ExitBtn.BevelColor := clInactiveCaption;
        if (Sender = MonitorBtn) then MonitorBtn.BevelColor := clInactiveCaption;
        if (Sender = UndoBtn) then UndoBtn.BevelColor := clInactiveCaption;
        if (Sender = RedoBtn) then RedoBtn.BevelColor := clInactiveCaption;
        if (Sender = UndoLvlBtn) or (Sender = uELED1) or (Sender = uELED2) or (Sender = uELED3) then UndoLvlBtn.BevelColor := clInactiveCaption;
        if (Sender = uELED1) then uELED1.Bright := False;
        if (Sender = uELED2) then uELED2.Bright := False;
        if (Sender = uELED3) then uELED3.Bright := False;
        if (Sender = PyramidionLogo) then begin
        	PyramidionLogoImg := TPortableNetworkGraphic.Create;
        	RStream := TResourceStream.Create(HINSTANCE, 'PyramidionLogo1', RT_RCDATA);
        	PyramidionLogoImg.LoadFromStream(RStream);
        	PyramidionLogo.Picture.Assign(PyramidionLogoImg);
        	RStream.Free;
        	PyramidionLogoImg.Free;
	end;
        if (Sender = SNodPnl) or (Sender = SNodGrd) then SNodPnl.BevelColor := clInactiveCaption;
        if (Sender = SNodPnl) or (Sender = SNodGrd) then SNodLbl.Color := clMenuBar;
        if (Sender = SNodGrd) then SNodGrd.Color := clWindow;
        if (Sender = INodPnl) or (Sender = INodGrd) then INodPnl.BevelColor := clInactiveCaption;
        if (Sender = INodPnl) or (Sender = INodGrd) then INodLbl.Color := clMenuBar;
        if (Sender = INodGrd) then INodGrd.Color := clWindow;
        if (Sender = UNodPnl) or (Sender = UNodGrd) then UNodPnl.BevelColor := clInactiveCaption;
        if (Sender = UNodPnl) or (Sender = UNodGrd) then UNodLbl.Color := clMenuBar;
        if (Sender = UNodGrd) then UNodGrd.Color := clWindow;
        if (Sender = SConPnl) or (Sender = SCon) then SCon.Color := clBlack;
end;
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.CMouseDown(Sender: TObject);			//; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
	PyramidionLogoImg: TPortableNetworkGraphic;
	RStream: TResourceStream;
begin
        if (Sender = AddAscEdgBtn) then AddAscEdgBtn.Color := clBtnHighlight;
        if (Sender = AddDscEdgBtn) then AddDscEdgBtn.Color := clBtnHighlight;
        if (Sender = DelCNodBtn) then DelCNodBtn.Color := clBtnHighlight;
        if (Sender = DelCEdgBtn) then DelCEdgBtn.Color := clBtnHighlight;
        if (Sender = CommitBtn) then CommitBtn.Color := clBtnHighlight;
        if (Sender = ExitBtn) then ExitBtn.Color := clBtnHighlight;
        if (Sender = MonitorBtn) then MonitorBtn.Color := clBtnHighlight;
        if (Sender = UndoBtn) then UndoBtn.Color := clBtnHighlight;
        if (Sender = RedoBtn) then RedoBtn.Color := clBtnHighlight;
        if (Sender = uELED1) then uELED1.Bright := True;
        if (Sender = uELED1) then uELED1.Color := $008080FF;
        if (Sender = uELED2) then uELED2.Bright := True;
        if (Sender = uELED2) then uELED2.Color := $0080FFFF;
        if (Sender = uELED3) then uELED3.Bright := True;
        if (Sender = uELED3) then uELED3.Color := $0080FF80;
        if (Sender = PyramidionLogo) then begin
        	PyramidionLogoImg := TPortableNetworkGraphic.Create;
        	RStream := TResourceStream.Create(HINSTANCE, 'PyramidionLogo3', RT_RCDATA);
        	PyramidionLogoImg.LoadFromStream(RStream);
        	PyramidionLogo.Picture.Assign(PyramidionLogoImg);
        	RStream.Free;
        	PyramidionLogoImg.Free;
	end;
end;
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.CMouseUp(Sender: TObject);			//; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
	PyramidionLogoImg: TPortableNetworkGraphic;
	RStream: TResourceStream;
begin
        if (Sender = AddAscEdgBtn) then AddAscEdgBtn.Color := clBtnFace;
        if (Sender = AddDscEdgBtn) then AddDscEdgBtn.Color := clBtnFace;
        if (Sender = DelCNodBtn) then DelCNodBtn.Color := clBtnFace;
        if (Sender = DelCEdgBtn) then DelCEdgBtn.Color := clBtnFace;
        if (Sender = CommitBtn) then CommitBtn.Color := clBtnFace;
        if (Sender = ExitBtn) then ExitBtn.Color := clBtnFace;
        if (Sender = MonitorBtn) then MonitorBtn.Color := clBtnFace;
        if (Sender = UndoBtn) then UndoBtn.Color := clBtnFace;
        if (Sender = RedoBtn) then RedoBtn.Color := clBtnFace;
        if (Sender = uELED1) then uELED1.Bright := False;
        if (Sender = uELED1) then if GAllowUndo = 0 then uELED1.Color := clRed else uELED1.Color := $00000040;
        if (Sender = uELED2) then uELED2.Bright := False;
        if (Sender = uELED2) then if GAllowUndo = 1 then uELED2.Color := clYellow else uELED2.Color := $00004040;
        if (Sender = uELED3) then uELED3.Bright := False;
        if (Sender = uELED3) then if GAllowUndo > 1 then uELED3.Color := clLime else uELED3.Color := $00004000;
        if (Sender = PyramidionLogo) then begin
        	PyramidionLogoImg := TPortableNetworkGraphic.Create;
        	RStream := TResourceStream.Create(HINSTANCE, 'PyramidionLogo2', RT_RCDATA);
        	PyramidionLogoImg.LoadFromStream(RStream);
        	PyramidionLogo.Picture.Assign(PyramidionLogoImg);
        	RStream.Free;
        	PyramidionLogoImg.Free;
	end;
end;
//==================================================================================================================================================================================
end.
