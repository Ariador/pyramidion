
//================================================================================================================================================
//	PYRAMIDION NON-RELATIONAL DATABASE, v.0.19 (alpha version)
//------------------------------------------------------------------------------------------------------------------------------------------------
//	(c) 2023 Igor Voloshin ivoloshin@hotmail.com
//================================================================================================================================================
//	Unit Pyramidion019
//------------------------------------------------------------------------------------------------------------------------------------------------
//	Contains declarations for basic Pyramidion DBMS types, objects and variables, also internal tools (Level -1) and user interface (Level 0).
//	Functions and procedures implemented so far are the following:
//
//	LEVEL -1. INTERNAL PROGRAM TOOLS
//		-1.0.	ByteToBitStr - Convert a byte to '00000000' type string
//		-1.1.	BitStrToByte - Convert '00000000' type string to a byte
//		-1.2.	IDSCRC - Calculate CRC32 for a MemoryStream content (of any origin
//		-1.3.	24 wrapper functions and procedures to manipulate Nod.Flg bits
//		-1.4.	9 wrapper functions and procedures to manipulate Nod.Edg.Flg bits
//		-1.5.   Global variables renewal
//		-1.6.   GlobalsRefresh - Screen refresher for global variables
//
//	LEVEL 0. AESTHESIA - PYRAMIDION INTERFACE
//		0.0.	FormActivate - Initial program actions with the database
//		0.1.	Redraw - Fill all the form fields with the current data
//		0.2.	AMonitor - Check current status of Nod and Undo chain and show it in console window
//		0.3.	ALog - Log message handling
//	4	0.X.	SConClick
//	5	0.X.	SConKeyUp
//	6	0.X.	SConKeyPress to catch cursor position, arrow keys, #13 'Return' and #27 'Escape' keys
//	7	0.4.	SConEdit - Console input done
//	8	0.5.	CNodIDKeyPress - Filter for current node ID editing
//	9	0.6.	CNodIDEdit - Current node ID editing done
//	10	0.X	CNodFlgKeyPress - Current node flags editing input filter
//	11	0.7.	CNodFlgEdit - Current node flags editing done
//	12	0.X.	CNodAdvKeyPress - Current node adventor editing input filter
//	13	0.8.	CNodAdvEdit - Current node adventor editing done
//	14	0.X.	CNodFrmEdit - Current node formula editing done
//	15	0.X.	CNodNmeEdit - Current node name editing done
//	16	0.9.	ETpKeyPress - Current edge type editing input filter
//	17	0.10.	ETpEdit - Current edge type editing done
//	18	0.X.	ManazKeyPress - Current edge checkstring editing input filter
//	19	0.X.	ManazEdit - Current edge checkstring editing done
//	20	0.11.	TNodIDKeyPress - Filter for target node ID editing
//	21	0.12.	TNodIDEdit - Target node ID editing done
//	22	0.X	TNodFlgKeyPress - Target node flags editing input filter
//	23	0.13.	TNodFlgEdit - Target node flags editing done
//	24	0.X.	TNodAdvKeyPress - Target node adventor editing input filter
//	25	0.14.	TNodAdvEdit - Target node adventor editing done
//	26	0.X.	TNodFrmEdit - Target node formula editing done
//	27	0.X.	TNodNmeEdit - Target node name editing done
//	28	0.15.	AddAscEdgBtnClick - Add ascending edge button click
//	29	0.16.	AddDscEdgBtnClick - Add descending edge button click
//	30	0.17.	DelCNodBtnClick - Delete current node button click
//	31	0.18.	DelCEdgBtnClick - Delete current edge button click
//	32	0.19.	CNodLblMouseDown - Current Node label mouse down. Calls PopMenu on the right click
//	33	0.20.	CEdgLblMouseDown - Current Edge label mouse down. Calls PopMenu on the right click
//	34	0.21.	TNodLblMouseDown - Target Node label mouse down. Calls PopMenu on the right click
//	35	0.22.	SNodGrdMouseDown - superior nodes grid mouse down. Calls PopMenu on the right click
//	36	0.23.	SNodGrdDblClick - superior nodes grid double click. Switches goEditing property on to allow in-cell editing
//	37	0.24.	SNodGrdEditingDone - superior nodes grid cell editing done. Switches goEditing property off to forbid in-cell editing
//	38	0.25.	INodGrdMouseDown - inferior nodes grid mouse down. Calls PopMenu on the right click
//	39	0.26.	INodGrdDblClick - inferior nodes grid double click. Switches goEditing property on to allow in-cell editing
//	40	0.27.	INodGrdEditingDone - inferior nodes grid cell editing done. Switches goEditing property off to forbid in-cell editing
//	41	0.28.	UNodGrdMouseDown - unlinked nodes grid mouse down. Calls PopMenu on the right click
//	42	0.29.	UNodGrdDblClick - unlinked nodes grid double click. Switches goEditing property on to allow in-cell editing
//	43	0.30.	UNodGrdEditingDone - unlinked nodes grid cell editing done. Switches goEditing property off to forbid in-cell editing
//	44	0.31.	SetCurrIClick - Popup menu item 'Set as current node' click
//	45	0.32.	SetTargIClick - Popup menu item 'Set as current node' click
//	46	0.33.	AddSEdgIClick - Popup menu item 'Create edge to the current node' click
//	47	0.34.	AddIEdgIClick - Popup menu item 'Create edge from the current node' click
//	48	0.35.	AddNodIClick - Popup menu item 'Add [superior, inferior, unlinked] node' click
//	49	0.36.	DelNodIClick - Popup menu item 'Delete [superior, inferior, unlinked] node' click
//	50	0.37.	SNodGrdKeyPress - Superior nodes grid keypress
//	51	0.38.	SNodGrdDblClick - Superior nodes grid double click
//	52	0.39.	INodGrdKeyPress - Inferior nodes grid keypress
//	53	0.40.	INodGrdDblClick - Inferior nodes grid double click
//	54	0.41.	UNodGrdClick - Unlinked nodes grid keypress
//	55	0.42.	UNodGrdDblClick - Unlinked nodes grid double click
//	56	0.43.	CommitBtnClick - Commit button click
//	57	0.44.	ExitBtnClick - Exit button click
//	58	0.45.	UndoBtnClick - Undo last action done (if the base not committed)
//	59	0.46.	RedoBtnClick - Redo last action undone (if the base not committed)
//	60	0.47.	FormClose - Form close without saving any logs or changes to IDS file
//	61	0.48.	MonitorBtnClick - Types all of the Nod array content to session console
//		COSMETIC PROCEDURES JUST FOR NICE LOOK
//		0.-1.	4 color manipulation procedures for the form elements: CMouseEnter, CMouseLeave, CMouseDown, CMouseUp
//
//	LEVEL 1. APOSTOLIA: DATA INTERFACE (Create, Destroy, Read, Write, Eidetika Read, Eidetika Write, Eidetika Memorize, Proseychi, Idx)
//		1.0.	ACreate - Create IDS base instance;
//		1.1.	ADestroy - Destruct IDS base instance;
//		1.2.	ARead - Read the Pleximo data from external IDS file to memory
//		1.3.	AWrite - Write all the Pleximo data to external IDS file
//		1.4.	AERead - Read the Events Strip (Eidetika) data from external EID file to memory
//		1.5.	AEWrite - Update the Events Strip EID file
//		1.6.	AEMem - Add an event to the Event Strip (Eidetika)
//		1.7.	Proseychi - Process an interface call
//		1.8.	Idx - Return index of a node by its ID (binary search in sorted list)
//
//	LEVEL 2. ANGELIOPHORES: BASIC MECHANICS (Add, Delete, Edit, Change Node, Change Edge Type, Commit, Parse)
//		2.0.	AAdd - Add a node or an edge
//		2.1.	ADelete - Delete an edge
//		2.2.	AEdit - Edit Formula, Adventor or Name of a node
//		2.3.	AChCurrNode - Change current node pointer
//		2.4.	AChTargNode - Change target node pointer
//		2.5.	AChEType - Change edge type
//		2.6.	ACommit - Remove deleted/orphaned nodes/sags from the Nod array, reset changes history for the session
//		2.7.	AParse - Parse strings of any origin to syntactic trees corresonding to the content of the Antikeymena knowledge base
//
//================================================================================================================================================

unit Pyramidion019;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids, ExtCtrls, Buttons,
	LCLProc, LResources, Menus, RichMemo, RichMemoUtils, ueled, ZStream;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	Aesthesia Form Declaration
//------------------------------------------------------------------------------------------------------------------------------------------------
type
	{ TAesthesiaForm }

	TAesthesiaForm = class(TForm)
		GTEFlgLbl:		TLabel;
		GTEFlgTxt:		TEdit;
		GTEFlgType:		TLabel;
		GCurrBIDLbl:		TLabel;
		GCurrBIDType:		TLabel;
		GCurrBIDTxt:		TEdit;
		GCEventLbl:		TLabel;
		GCEventType:		TLabel;
		GCEventTxt:		TEdit;
		GIDSCRCLbl:		TLabel;
		GIDSCRCTxt:		TEdit;
		GIDSCRCType:		TLabel;
		GEIDCRCLbl:		TLabel;
		GEIDCRCType:		TLabel;
		GEIDCRCTxt:		TEdit;
		GIDSFileTxt:		TEdit;
		GTargLbl:		TLabel;
		GEdgeLbl:		TLabel;
		GTargTxt:		TEdit;
		GEdgeTxt:		TEdit;
		GTargType:		TLabel;
		GEdgeType:		TLabel;
		GMnIDLbl:		TLabel;
		GCEFlgLbl:		TLabel;
		GCEFlgTxt:		TEdit;
		GCEFlgType:		TLabel;
		GTpIDLbl:		TLabel;
		GMnIDTxt:		TEdit;
		GTpIDTxt:		TEdit;
		GMnIDType:		TLabel;
		GTpIDType:		TLabel;
		GTEdgLbl:		TLabel;
		GTEdgTxt:		TEdit;
		GTEdgType:		TLabel;
		GTpIdxLbl:		TLabel;
		GTpIdxTxt:		TEdit;
		GTpIdxType:		TLabel;
		GMnFrmLbl:		TLabel;
		GTpNmeLbl:		TLabel;
		GMnFrmTxt:		TEdit;
		GTpNmeTxt:		TEdit;
		GMnFrmType:		TLabel;
		GCEdgLbl:		TLabel;
		GCEdgTxt:		TEdit;
		GCEdgType:		TLabel;
		GMnIdxLbl:		TLabel;
		GMnIdxTxt:		TEdit;
		GMnIdxType:		TLabel;
		GTNmeTxt:		TEdit;
		GTFrmTxt:		TEdit;
		GTAdvTxt:		TEdit;
		GTIDTxt:		TEdit;
		GTIdxTxt:		TEdit;
		GTFlgTxt:		TEdit;
		GCNmeTxt:		TEdit;
		GTNmeLbl:		TLabel;
		GTIdxLbl:		TLabel;
		GTFlgLbl:		TLabel;
		GTFrmLbl:		TLabel;
		GTAdvLbl:		TLabel;
		GTIDLbl:		TLabel;
		GTpNmeType:		TLabel;
		GTNmeType:		TLabel;
		GTFrmType:		TLabel;
		GTAdvType:		TLabel;
		GTIDType:		TLabel;
		GTIdxType:		TLabel;
		GTFlgType:		TLabel;
		PopImage:		TImage;
		//------------------------------------
		NodCntPnl:		TPanel;
		GTopIDLbl:		TLabel;
		GTopIDPlq:		TPanel;
		NodCntLbl:		TLabel;
		NodCntPlq:		TPanel;
		NodDLbl:		TLabel;
		NodDPlq:		TPanel;
		SagCntLbl:		TLabel;
		SagCntPlq:		TPanel;
		SagDLbl:		TLabel;
		SagDPlq:		TPanel;
		//------------------------------------
		BtnPnl:			TPanel;
		AddAscEdgBtn:		TPanel;
		AddDscEdgBtn:		TPanel;
		DelCNodBtn:		TPanel;
		DelCEdgBtn:		TPanel;
		CommitBtn:		TPanel;
		ExitBtn:		TPanel;
		MonitorBtn:		TPanel;
		UndoBtn:		TPanel;
		RedoBtn:		TPanel;
		UndoLvlBtn:		TPanel;
		uELED1:			TuELED;
		uELED2:			TuELED;
		uELED3:			TuELED;
		PyramidionLogo:		TImage;
		RFileOpen:		TOpenDialog;
		RFileSave:		TSaveDialog;
		//------------------------------------
		GlobalVarsPnl:		TPanel;
		GIDSFileLbl:		TLabel;
		GIDSFileType:		TLabel;
		GCNmeType:		TLabel;
		GCFrmType:		TLabel;
		GCAdvType:		TLabel;
		GCAdvTxt:		TEdit;
		GCNmeLbl:		TLabel;
		GCFrmLbl:		TLabel;
		GCFrmTxt:		TEdit;
		GCAdvLbl:		TLabel;
		GCIDType:		TLabel;
		GCIDLbl:		TLabel;
		GCIDTxt:		TEdit;
		GCIdxLbl:		TLabel;
		GCIdxType:		TLabel;
		GCIdxTxt:		TEdit;
		GCFlgLbl:		TLabel;
		GCFlgType:		TLabel;
		GCFlgTxt:		TEdit;
		//------------------------------------
		SNodPnl:		TPanel;
		SNodLbl:		TLabel;
		SNodGrd:		TStringGrid;
		//------------------------------------
		CurrStatePnl:		TPanel;
		CNodLbl:		TLabel;
		CNodIDLbl:		TLabel;
		CNodID:			TEdit;
		CNodFlgLbl:		TLabel;
		CNodFlg:		TEdit;
		CNodFrmLbl:		TLabel;
		CNodFrm:		TEdit;
		CNodAdvLbl:		TLabel;
		CNodAdv:		TEdit;
		CNodNmeLbl:		TLabel;
		CNodNme:		TEdit;
		CEdgLbl:		TLabel;
		EdgDirLbl:		TLabel;
		EdgDir:			TPanel;
		ETpLbl:			TLabel;
		ETp:			TEdit;
		ETpTxt:			TPanel;
		ManazLbl:		TLabel;
		Manaz:			TEdit;
		ManazTxt:		TPanel;
		TNodLbl:		TLabel;
		TNodIDLbl:		TLabel;
		TNodID:			TEdit;
		TNodFlgLbl:		TLabel;
		TNodFlg:		TEdit;
		TNodFrmLbl:		TLabel;
		TNodFrm:		TEdit;
		TNodAdvLbl:		TLabel;
		TNodAdv:		TEdit;
		TNodNmeLbl:		TLabel;
		TNodNme:		TEdit;
		//------------------------------------
		INodPnl:		TPanel;
		INodLbl:		TLabel;
		INodGrd:		TStringGrid;
		//------------------------------------
		UNodPnl:		TPanel;
		UNodLbl:		TLabel;
		UNodGrd:		TStringGrid;
		//------------------------------------
		SConPnl:		TPanel;
		SConLbl:		TLabel;
		SCon:			TRichMemo;
		SConStatLbl:		TLabel;
		//------------------------------------
		PopMenu:		TPopupMenu;
		SetCurrSI:		TMenuItem;
		SetCurrII:		TMenuItem;
		SetCurrUI:		TMenuItem;
		SetTargSI:		TMenuItem;
		SetTargII:		TMenuItem;
		SetTargUI:		TMenuItem;
		AddSEdgI:		TMenuItem;
		AddIEdgI:		TMenuItem;
		AddSNodI:		TMenuItem;
		AddINodI:		TMenuItem;
		AddUNodI:		TMenuItem;
		DelNodSI:		TMenuItem;
		DelNodII:		TMenuItem;
		DelNodUI:		TMenuItem;
	//----------------------------------------------------------------------------------------------------------------------------------------
	//	Interface procedures
	//----------------------------------------------------------------------------------------------------------------------------------------
		procedure	FormActivate(Sender: TObject);
		procedure	Redraw();
		procedure	AMonitor();
		procedure	ALog(Msg: Integer);
		procedure	SConClick(Sender: TObject);
		procedure	SConKeyUp(Sender: TObject);
		procedure	SConKeyPress(Sender: TObject; var Key: Char);
		procedure	SConEdit();
		procedure	CNodIDKeyPress(Sender: TObject; var Key: Char);
		procedure	CNodIDEdit(Sender: TObject);
		procedure	CNodFlgKeyPress(Sender: TObject; var Key: Char);
		procedure	CNodFlgEdit(Sender: TObject);
		procedure	CNodFrmEdit(Sender: TObject);
		procedure	CNodAdvKeyPress(Sender: TObject; var Key: Char);
		procedure	CNodAdvEdit(Sender: TObject);
		procedure	CNodNmeEdit(Sender: TObject);
		procedure	ETpKeyPress(Sender: TObject; var Key: Char);
		procedure	ETpEdit(Sender: TObject);
		procedure	ManazKeyPress(Sender: TObject; var Key: Char);
		procedure	ManazEdit(Sender: TObject);
		procedure	TNodIDKeyPress(Sender: TObject; var Key: Char);
		procedure	TNodIDEdit(Sender: TObject);
		procedure	TNodFlgKeyPress(Sender: TObject; var Key: Char);
		procedure	TNodFlgEdit(Sender: TObject);
		procedure	TNodFrmEdit(Sender: TObject);
		procedure	TNodAdvKeyPress(Sender: TObject; var Key: Char);
		procedure	TNodAdvEdit(Sender: TObject);
		procedure	TNodNmeEdit(Sender: TObject);
		procedure	AddAscEdgBtnClick(Sender: TObject);
		procedure	AddDscEdgBtnClick(Sender: TObject);
		procedure	DelCNodBtnClick(Sender: TObject);
		procedure	DelCEdgBtnClick(Sender: TObject);
		procedure	CNodLblMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
		procedure	CEdgLblMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
		procedure	TNodLblMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
		procedure	SNodGrdMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
		procedure	SNodGrdDblClick(Sender: TObject);
		procedure	SNodGrdEditingDone(Sender: TObject);
		procedure	INodGrdMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
		procedure	INodGrdDblClick(Sender: TObject);
		procedure	INodGrdEditingDone(Sender: TObject);
		procedure	UNodGrdMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
		procedure	UNodGrdDblClick(Sender: TObject);
		procedure	UNodGrdEditingDone(Sender: TObject);
		procedure	SetCurrIClick(Sender: TObject);
		procedure	SetTargIClick(Sender: TObject);
		procedure	AddNodIClick(Sender: TObject);
		procedure	AddSEdgIClick(Sender: TObject);
		procedure	AddIEdgIClick(Sender: TObject);
		procedure	DelNodIClick(Sender: TObject);
		procedure	CommitBtnClick(Sender: TObject);
		procedure	ExitBtnClick(Sender: TObject);
		procedure	UndoBtnClick(Sender: TObject);
		procedure	RedoBtnClick(Sender: TObject);
		procedure	FormClose(Sender: TObject);
		procedure	MonitorBtnClick(Sender: TObject);
	//----------------------------------------------------------------------------------------------------------------------------------------
	//	'Cosmetic' procedures
	//----------------------------------------------------------------------------------------------------------------------------------------
		procedure	CMouseEnter(Sender: TObject);
		procedure	CMouseLeave(Sender: TObject);
		procedure	CMouseDown(Sender: TObject);			//; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
		procedure	CMouseUp(Sender: TObject);			//; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
	private
	public
	end;

var

	//----------------------------------------------------------------------------------------------------------------------------------------
	//	Instance of the main program form
	//----------------------------------------------------------------------------------------------------------------------------------------
	Form:		TAesthesiaForm;		// This is where all the user interface is located

	//----------------------------------------------------------------------------------------------------------------------------------------
	//	Global variables - all the names start with 'G'
	//----------------------------------------------------------------------------------------------------------------------------------------
	GIDSFile:	String;			// Current IDS file in use
	GIDSCRC:	DWord;			// CRC sum of IDS file
	GEIDCRC:	DWord;			// CRC sum of EID file
	GCurrBID:	Word;			// Current IDS base ID (BA&A base BID = 0). Note: in Pyramidion 0.19 upper 2 bytes of a node ID
						// are used as global IDS knowledge base ID, to separate knowledge fields to IDS files
	GTopID:		UInt64;			// Current topmost ID value for the Nod array
	GNodDelCnt:	UInt64;			// Global deleted nodes counter
	GEdgeCnt:	UInt64;			// Global edges counter
	GEdgDelCnt:	UInt64;			// Global deleted edges counter
	//----------------------------------------------------------------------------------------------------------------------------------------
	GCIdx:		UInt64;			// Index of the current node
	GCID:		Int64;			// ID of the current node
	GCFlg:		Byte;			// Current node flags
	GCFrm:		RawByteString;		// Current node formula
	GCAdv:		Int64;			// Current node adventor
	GCNme:		RawByteString;		// Current node name
	//----------------------------------------------------------------------------------------------------------------------------------------
	GTarg:		Boolean;		// True if the target node is set, False otherwise
	GTIdx:		UInt64;			// Index of the target node
	GTID:		Int64;			// ID of the target node
	GTFlg:		Byte;			// Target node flags
	GTFrm:		RawByteString;		// Target node formula
	GTAdv:		Int64;			// Target node adventor
	GTNme:		RawByteString;		// Target node name
	//----------------------------------------------------------------------------------------------------------------------------------------
	GEdge:		Boolean;		// True if the current to target edge is set, False otherwise
	GCEdg:		UInt64;			// Index of the current edge at the current node
	GMnIdx:		UInt64;			// Index of the current edge checkstring node
	GMnID:		Int64;			// Current edge manaz (checkctring node ID) field
	GMnFrm:		RawByteString;		// Current edge check string
	GTEdg:		UInt64;			// Index of the current edge at the target node
	GTpIdx:		UInt64;			// Index of the current edge type node
	GTpID:		Int64;			// Current edge type (property node ID) field
	GTpNme:		RawByteString;		// Current edge type name
	//----------------------------------------------------------------------------------------------------------------------------------------
	GCIDText:	String;			 // Current text in the CNodID field
	GCFlgText:	String;			 // Current text in the CNodFlg field
	GCFrmText:	String;			 // Current text in the CNodFrm field
	GCAdvText:	String;			 // Current text in the CNodAdv field
	GCNmeText:	String;			 // Current text in the CNodNme field
	GTIDText:	String;			 // Current text in the TNodID field
	GTFlgText:	String;			 // Current text in the TNodFlg field
	GTFrmText:	String;			 // Current text in the TNodFrm field
	GTAdvText:	String;			 // Current text in the TNodAdv field
	GTNmeText:	String;			 // Current text in the TNodNme field
	GETpText:	String;			 // Current text in the ETp field
	GMnzText:	String;			 // Current text in the Manaz field
	GCEvent:	Byte;			// Code of the current event
	GQueryAsked:	Boolean;		// Flag of console query
	GAllowUndo:	Byte;			// Switches undo service between 0 (off), 1 (limited to the last commit), 255 (unlimited)
	GUndo:		Int64;			// Current undo/redo array position

	//----------------------------------------------------------------------------------------------------------------------------------------
	//	Edge reference type Declaration
	//----------------------------------------------------------------------------------------------------------------------------------------
	type	AEdge =	record			// Special type is required to edges of a node.
		ID:	Int64;			// Linked Node ID (node at the other end of the edge);
		Flg:	Byte;			// Flags byte. Currently 4 rightmost bits are used, with the following flags:
						// Bit #7: Dir flag ― outgoing edge if 1, incoming if 0
						// Bit #6: ETp flag ― an edge type ID is present if 1, none if 0
						// Bit #5: Mnz flag ― checkstring ID present if 1, none if 0
						// Bit #4: Del flag ― deleted edge if 1, existing if 0
						// Note: for descending nodes ID refers to the node with check string (Manaz Node), for ascending
						// nodes ID refers to the node with property string (Type Node);
		ETp:	Int64;			// Type of the edge encoded in QWord, just referring the ID of the type-representing node.
	end;

	//----------------------------------------------------------------------------------------------------------------------------------------
	//	Nodes Record Declaration
	//----------------------------------------------------------------------------------------------------------------------------------------
	type	ANode =	record			// Pyramidion Node corresponds to orgraph node.
		ID:	Int64;			// Unique Node ID (note: upper 2 bytes represent knowledge base ID, for BA&A ID=0);
		Flg:	Byte;			// Flags byte. Currently 7 rightmost bits are used, with the following flags:
						// Bit #7: Out flag ― an outgoing egde (from the node) present if 1, none if 0
						// Bit #6: Inc flag ― an incoming egde (to the node) present if 1, none if 0
						// Bit #5: Del flag ― deleted node if 1, existing if 0
						// Bit #4: Frm flag ― a non―empty formula field present if 1 (else if 0)
						// Bit #3: Adv flag ― a non―empty adventor field present if 1 (else if 0)
						// Bit #2: Nme flag ― a non―empty nodename field present if 1 (else if 0)
						// Bit #1: Lnk flag ― node is linked to the current node if 1, not if 0 0 *
		Edg:	array of AEdge;		// List of (typed and directed) edges of the node;
		Adv:	Int64;			// Pointer to the adventor field within Eidetika array, able to contain binary data;
		Frm:	RawByteString;		// Formula field for parcing by adventors, contains combinatory symbols and arguments pointers.
		Nme:	RawByteString;		// Node Name field - syntactic sugar for a human operator;
	end;

	//----------------------------------------------------------------------------------------------------------------------------------------
	//	Eidetika Record Declaration
	//----------------------------------------------------------------------------------------------------------------------------------------
	type	AEidon = record			// Record type for the Events Strip (Eidetika). "Ειδον" means "I see".
		TStmp:	Comp;			// Time stamp in the millisecond since 1/1/0001 format (timezone independent unlike TDateTime);
		ChCnt:	DWord;			// Counter of events within milliseconds (because there is no reliable nanoseconds counter in
						// multicore CPUs);
		EvnTp:	Byte;			// Event type (few types are anticipated, so mere a byte used to store type #);
		Args:	RawByteString;		// Arguments of the event (specific to each event type).
	end;

	//----------------------------------------------------------------------------------------------------------------------------------------
	//	Apostolia Class Declaration
	//----------------------------------------------------------------------------------------------------------------------------------------
	type	TApostolia =	class(TObject)
		private
			function	ACreate(): Integer;
			procedure	ADestroy();
			function	ARead(): Integer;
			function	AWrite(): Int64;
			function	AERead(): Integer;
			function	AEWrite(): Int64;
			procedure	AEMem(EventNo: Integer);
		public
			function	Proseychi(): Integer;
			function	Idx(InputID: Int64): Int64;
	end;

	//----------------------------------------------------------------------------------------------------------------------------------------
	//	Angeliophor Class Declaration
	//----------------------------------------------------------------------------------------------------------------------------------------
	type	TAngeliophor =	class(TObject)
		private
			function	AAdd(Mode: Byte): Int64;
			function	ADelete(): Integer;
			function	AEdit(Mode: Byte): Integer;
			function	AChCurrNode(): Integer;
			function	AChTargNode(): Integer;
			function	AChEType(Mode: Byte): Integer;
			function	ACommit(): Integer;
			function	AParse(InputStr: RawByteString): Int64;	// <--- Remake this!
		public
	end;

	var

	//----------------------------------------------------------------------------------------------------------------------------------------
	//	Instances of the main program objects
	//----------------------------------------------------------------------------------------------------------------------------------------
	Ap:	TApostolia;			// All interface calls are processed here;
	Ag:	TAngeliophor;			// All data are processed here;
	Nod:	array of ANode;			// All the Pleximo nodes are placed here;
	Eid:	array of AEidon;		// Eidetika, Events strip. Everything what's happening is fixed here for good;


implementation

{$R *.lfm}
//{$rangeChecks off}

//================================================================================================================================================
// LEVEL -1. INTERNAL PROGRAM TOOLS
//================================================================================================================================================
//------------------------------------------------------------------------------------------------------------------------------------------------
//	-1.0. ByteToBitStr - Convert a byte to '00000000' type string
//------------------------------------------------------------------------------------------------------------------------------------------------
function ByteToBitStr(ByteValue: Byte): String;
begin
	Result := BinStr(Integer(ByteValue), 8);
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	-1.1. BitStrToByte - Convert '00000000' type string to byte
//------------------------------------------------------------------------------------------------------------------------------------------------
function BitStrToByte(StrValue: String): Byte;
var
	Cnt: Byte;
begin
	Result := 0;
	for Cnt := 0 to 7 do Result := Result + (Byte(StrToInt(RightStr(LeftStr(StrValue, 8 - Cnt), 1))) shl Cnt);
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	-1.2. IDSCRC - Calculate CRC32 for a MemoryStream content (of any origin)
//------------------------------------------------------------------------------------------------------------------------------------------------
function IDSCRC(IDS: TMemoryStream): DWord;
var
	CRCTable: array[0..255] of DWord;
	Cnt1, Cnt2, Cnt3, Cnt4, Len: DWord;
	Buf: Byte;
begin
	// Filling CRC Table for the polynomial $EDB88320
	Cnt1 := $EDB88320;
	for Cnt2 := 0 to 255 do begin
		Cnt3 := Cnt2;
		for Cnt4 := 0 to 7 do if (Cnt3 and 1) <> 0 then Cnt3 := Cnt1 xor (Cnt3 shr 1) else Cnt3 := (Cnt3 shr 1);
		CRCTable[Cnt2] := Cnt3;
	end;
	// Calculating CRC32
	IDS.Position := 0;
	Cnt1 := IDS.Size;
	Buf := 0;
	Result := 0;
	for Len := 0 to IDS.Size do begin
		IDS.Read(Buf, 1);
		Result := CRCTable[Byte(Result and $FF) xor Buf] xor (Result shr 8);
	end;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	-1.3. 24 wrapper functions and procedures to manipulate Nod.Flg bits:
//------------------------------------------------------------------------------------------------------------------------------------------------
function OutFlg(NIdx: UInt64): Boolean;
begin
	if NIdx < Length(Nod) then if (Nod[NIdx].Flg and 1) > 0 then Result := True else Result := False;
end;

function IncFlg(NIdx: UInt64): Boolean;
begin
	if NIdx < Length(Nod) then if (Nod[NIdx].Flg and 2) > 0 then Result := True else Result := False;
end;

function DelFlg(NIdx: UInt64): Boolean;
begin
	if NIdx < Length(Nod) then if (Nod[NIdx].Flg and 4) > 0 then Result := True else Result := False;
end;

function FrmFlg(NIdx: UInt64): Boolean;
begin
	if NIdx < Length(Nod) then if (Nod[NIdx].Flg and 8) > 0 then Result := True else Result := False;
end;

function AdvFlg(NIdx: UInt64): Boolean;
begin
	if NIdx < Length(Nod) then if (Nod[NIdx].Flg and 16) > 0 then Result := True else Result := False;
end;

function NmeFlg(NIdx: UInt64): Boolean;
begin
	if NIdx < Length(Nod) then if (Nod[NIdx].Flg and 32) > 0 then Result := True else Result := False;
end;

function LnkFlg(NIdx: UInt64): Boolean;
begin
	if NIdx < Length(Nod) then if (Nod[NIdx].Flg and 64) > 0 then Result := True else Result := False;
end;

function SupFlg(NIdx: UInt64): Boolean;
begin
	if NIdx < Length(Nod) then if (Nod[NIdx].Flg and 128) > 0 then Result := True else Result := False;
end;
//---------------------------------------------------------------------------------------------------------------------------------
procedure SetOutFlg(NIdx: UInt64);
begin
	if NIdx < Length(Nod) then Nod[NIdx].Flg := Nod[NIdx].Flg or 1;
end;

procedure SetIncFlg(NIdx: UInt64);
begin
	if NIdx < Length(Nod) then Nod[NIdx].Flg := Nod[NIdx].Flg or 2;
end;

procedure SetDelFlg(NIdx: UInt64);
begin
	if NIdx < Length(Nod) then Nod[NIdx].Flg := Nod[NIdx].Flg or 4;
end;

procedure SetFrmFlg(NIdx: UInt64);
begin
	if NIdx < Length(Nod) then Nod[NIdx].Flg := Nod[NIdx].Flg or 8;
end;

procedure SetAdvFlg(NIdx: UInt64);
begin
	if NIdx < Length(Nod) then Nod[NIdx].Flg := Nod[NIdx].Flg or 16;
end;

procedure SetNmeFlg(NIdx: UInt64);
begin
	if NIdx < Length(Nod) then Nod[NIdx].Flg := Nod[NIdx].Flg or 32;
end;

procedure SetLnkFlg(NIdx: UInt64);
begin
	if NIdx < Length(Nod) then Nod[NIdx].Flg := Nod[NIdx].Flg or 64;
end;

procedure SetSupFlg(NIdx: UInt64);
begin
	if NIdx < Length(Nod) then Nod[NIdx].Flg := Nod[NIdx].Flg or 128;
end;
//---------------------------------------------------------------------------------------------------------------------------------
procedure ClrOutFlg(NIdx: UInt64);
begin
	if NIdx < Length(Nod) then Nod[NIdx].Flg := Nod[NIdx].Flg and 254;
end;

procedure ClrIncFlg(NIdx: UInt64);
begin
	if NIdx < Length(Nod) then Nod[NIdx].Flg := Nod[NIdx].Flg and 253;
end;

procedure ClrDelFlg(NIdx: UInt64);
begin
	if NIdx < Length(Nod) then Nod[NIdx].Flg := Nod[NIdx].Flg and 251;
end;

procedure ClrFrmFlg(NIdx: UInt64);
begin
	if NIdx < Length(Nod) then Nod[NIdx].Flg := Nod[NIdx].Flg and 247;
end;

procedure ClrAdvFlg(NIdx: UInt64);
begin
	if NIdx < Length(Nod) then Nod[NIdx].Flg := Nod[NIdx].Flg and 239;
end;

procedure ClrNmeFlg(NIdx: UInt64);
begin
	if NIdx < Length(Nod) then Nod[NIdx].Flg := Nod[NIdx].Flg and 223;
end;

procedure ClrLnkFlg(NIdx: UInt64);
begin
	if NIdx < Length(Nod) then Nod[NIdx].Flg := Nod[NIdx].Flg and 191;
end;

procedure ClrSupFlg(NIdx: UInt64);
begin
	if NIdx < Length(Nod) then Nod[NIdx].Flg := Nod[NIdx].Flg and 127;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	-1.4. 15 wrapper functions and procedures to manipulate Nod.Edg.Flg bits:
//------------------------------------------------------------------------------------------------------------------------------------------------
function EOutFlg(NIdx: UInt64; EIdx: UInt64): Boolean;
begin
	if (NIdx < Length(Nod)) and (EIdx < Length(Nod[NIdx].Edg)) then
		if Nod[NIdx].Edg[EIdx].Flg and 1 = 1 then Result := True else Result := False;
end;

function EIncFlg(NIdx: UInt64; EIdx: UInt64): Boolean;
begin
	if (NIdx < Length(Nod)) and (EIdx < Length(Nod[NIdx].Edg)) then
		if Nod[NIdx].Edg[EIdx].Flg and 1 = 0 then Result := True else Result := False;
end;

function EETpFlg(NIdx: Int64; EIdx: Int64): Boolean;
begin
	if (NIdx < Length(Nod)) and (EIdx < Length(Nod[NIdx].Edg)) then
		if Nod[NIdx].Edg[EIdx].Flg and 2 > 0 then Result := True else Result := False;
end;

function EMnzFlg(NIdx: Int64; EIdx: Int64): Boolean;
begin
	if (NIdx < Length(Nod)) and (EIdx < Length(Nod[NIdx].Edg)) then
		if Nod[NIdx].Edg[EIdx].Flg and 4 > 0 then Result := True else Result := False;
end;

function EDelFlg(NIdx: UInt64; EIdx: UInt64): Boolean;
begin
	if (NIdx < Length(Nod)) and (EIdx < Length(Nod[NIdx].Edg)) then
		if Nod[NIdx].Edg[EIdx].Flg and 8 > 0 then Result := True else Result := False;
end;
//---------------------------------------------------------------------------------------------------------------------------------
procedure SetEOutFlg(NIdx: UInt64; EIdx: UInt64);
begin
	if (NIdx < Length(Nod)) and (EIdx < Length(Nod[NIdx].Edg)) then
		Nod[NIdx].Edg[EIdx].Flg := Nod[NIdx].Edg[EIdx].Flg or 1;
end;

procedure SetEIncFlg(NIdx: UInt64; EIdx: UInt64);
begin
	if (NIdx < Length(Nod)) and (EIdx < Length(Nod[NIdx].Edg)) then
		Nod[NIdx].Edg[EIdx].Flg := Nod[NIdx].Edg[EIdx].Flg and 254;
end;

procedure SetEETpFlg(NIdx: Int64; EIdx: Int64);
begin
	if (NIdx < Length(Nod)) and (EIdx < Length(Nod[NIdx].Edg)) then begin
		Nod[NIdx].Edg[EIdx].Flg := Nod[NIdx].Edg[EIdx].Flg or 2;
		Nod[NIdx].Edg[EIdx].Flg := Nod[NIdx].Edg[EIdx].Flg and 251;
	end;
end;

procedure SetEMnzFlg(NIdx: Int64; EIdx: Int64);
begin
	if (NIdx < Length(Nod)) and (EIdx < Length(Nod[NIdx].Edg)) then begin
		Nod[NIdx].Edg[EIdx].Flg := Nod[NIdx].Edg[EIdx].Flg or 4;
		Nod[NIdx].Edg[EIdx].Flg := Nod[NIdx].Edg[EIdx].Flg and 253;
	end;
end;

procedure SetEDelFlg(NIdx: UInt64; EIdx: UInt64);
begin
	if (NIdx < Length(Nod)) and (EIdx < Length(Nod[NIdx].Edg)) then
		Nod[NIdx].Edg[EIdx].Flg := Nod[NIdx].Edg[EIdx].Flg or 8;
end;
//---------------------------------------------------------------------------------------------------------------------------------
procedure ClrEOutFlg(NIdx: UInt64; EIdx: UInt64);
begin
	if (NIdx < Length(Nod)) and (EIdx < Length(Nod[NIdx].Edg)) then
		Nod[NIdx].Edg[EIdx].Flg := Nod[NIdx].Edg[EIdx].Flg and 254;
end;

procedure ClrEIncFlg(NIdx: UInt64; EIdx: UInt64);
begin
	if (NIdx < Length(Nod)) and (EIdx < Length(Nod[NIdx].Edg)) then
		Nod[NIdx].Edg[EIdx].Flg := Nod[NIdx].Edg[EIdx].Flg or 1;
end;

procedure ClrEETpFlg(NIdx: Int64; EIdx: Int64);
begin
	if (NIdx < Length(Nod)) and (EIdx < Length(Nod[NIdx].Edg)) then
		Nod[NIdx].Edg[EIdx].Flg := Nod[NIdx].Edg[EIdx].Flg and 253;
end;

procedure ClrEMnzFlg(NIdx: Int64; EIdx: Int64);
begin
	if (NIdx < Length(Nod)) and (EIdx < Length(Nod[NIdx].Edg)) then
		Nod[NIdx].Edg[EIdx].Flg := Nod[NIdx].Edg[EIdx].Flg and 251;
end;


procedure ClrEDelFlg(NIdx: UInt64; EIdx: UInt64);
begin
	if (NIdx < Length(Nod)) and (EIdx < Length(Nod[NIdx].Edg)) then
		Nod[NIdx].Edg[EIdx].Flg := Nod[NIdx].Edg[EIdx].Flg and 247;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	-1.5. Global variables renewal
//	Note: GCID, GCFlg, GTarg, GTID, GTFlg, GEdge, GCEdg, GTEdg variables and the current edge flags must be set properly
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure GlobalsRenew();
begin
	GCIdx := Ap.Idx(GCID);
	GCFlg := Nod[GCIdx].Flg;
	if FrmFlg(GCIdx) then GCFrm := Nod[GCIdx].Frm;
	if AdvFlg(GCIdx) then GCAdv := Nod[GCIdx].Adv;
	if NmeFlg(GCIdx) then GCNme := Nod[GCIdx].Nme;
	//----------------------------------------------------------------------------------------------------------------------------------------
	if GTarg then begin
		GTIdx := Ap.Idx(GTID);
		GTFlg := Nod[GTIdx].Flg;
		if FrmFlg(GTIdx) then GTFrm := Nod[GTIdx].Frm;
		if AdvFlg(GTIdx) then GTAdv := Nod[GTIdx].Adv;
		if NmeFlg(GTIdx) then GTNme := Nod[GTIdx].Nme;
	end
	else begin
		GTID := -1;
		GTFlg := 0;
		GTFrm := '';
		GTAdv := -1;
		GTNme := '';
		GEdge := False;
		GCEdg := 0;
		GMnIdx := 0;
		GMnID := -1;
		GMnFrm := '';
		GTEdg := 0;
		GTpIdx := 0;
		GTpID := -1;
		GTpNme := '';
	end;
	//----------------------------------------------------------------------------------------------------------------------------------------
	if GEdge then begin
		if EOutFlg(GCIdx, GCEdg) then begin
			GMnID := Nod[GCIdx].Edg[GCEdg].ETp;
			GTpID := Nod[GTIdx].Edg[GTEdg].ETp;
		end
		else begin
			GMnID := Nod[GTIdx].Edg[GTEdg].ETp;
			GTpID := Nod[GCIdx].Edg[GCEdg].ETp;
		end;
		if GMnID = -1 then GMnIdx := 0 else GMnIdx := Ap.Idx(GMnID);
		GMnFrm := Nod[GMnIdx].Frm;
		if GTpID = -1 then GTpIdx := 0 else GTpIdx := Ap.Idx(GTpID);
		GTpNme := Nod[GTpIdx].Nme;
	end
	else begin
		GCEdg := 0;
		GMnIdx := 0;
		GMnID := -1;
		GMnFrm := '';
		GTEdg := 0;
		GTpIdx := 0;
		GTpID := -1;
		GTpNme := '';
	end;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	-1.6. Screen refresher for global variables
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure GlobalsRefresh();
begin
	with Form do begin
		GIDSFileTxt.Text := GIDSFile;		// String;	// Current IDS file in use
		GIDSCRCTxt.Text := IntToStr(GIDSCRC);	// DWord;	// CRC sum of IDS file
		GEIDCRCTxt.Text := IntToStr(GEIDCRC);	// DWord;	// CRC sum of EID file
		GCurrBIDTxt.Text := IntToStr(GCurrBID);	// Word;	// Current IDS base ID (BA&A base BID = 0)
		NodCntPlq.Caption := IntToStr(Length(Nod));		// UInt64; Number of nodes (both actual and deleted)
		GTopIDPlq.Caption := IntToStr(GTopID);	// UInt64;	// Current topmost ID value for the Nod array
		NodDPlq.Caption := IntToStr(GNodDelCnt);// UInt64;	// Global deleted nodes counter
		SagCntPlq.Caption := IntToStr(GEdgeCnt);// UInt64;	// Global edges counter
		SagDPlq.Caption := IntToStr(GEdgDelCnt);// UInt64;	// Global deleted edges counter
		//--------------------------------------------------------------------------------------------------------------------------------
		GCIdxTxt.Text := IntToStr(GCIdx);	// UInt64;	// Index of the current node
		GCIDTxt.Text := IntToStr(GCID);		// Int64;	// ID of the current node
		GCFlgTxt.Text := ByteToBitStr(GCFlg);	// Byte;	// Current node flags
		GCFrmTxt.Text := GCFrm;			// RBString;	// Current node formula
		GCAdvTxt.Text := IntToStr(GCAdv);	// Int64;	// Current node adventor
		GCNmeTxt.Text := GCNme;			// RBString;	// Current node name
		//--------------------------------------------------------------------------------------------------------------------------------
		if GTarg then GTargTxt.Font.Color := clLime else GTargTxt.Font.Color := clRed;
		if GTarg then GTargTxt.Text := 'True' else GTargTxt.Text := 'False';	// Boolean; True if the target node is set
		GTIdxTxt.Text := IntToStr(GTIdx);	// UInt64;	// Index of the target node
		GTIDTxt.Text := IntToStr(GTID);		// Int64;	// ID of the target node
		GTFlgTxt.Text := ByteToBitStr(GTFlg);	// Byte;	// Target node flags
		GTFrmTxt.Text := GTFrm;			// RBString;	// Target node formula
		GTAdvTxt.Text := IntToStr(GTAdv);	// Int64;	// Target node adventor
		GTNmeTxt.Text := GTNme;			// RBString;	// Target node name
		//--------------------------------------------------------------------------------------------------------------------------------
		if GEdge then GEdgeTxt.Font.Color := clLime else GEdgeTxt.Font.Color := clRed;
		if GEdge then GEdgeTxt.Text := 'True' else GEdgeTxt.Text := 'False';	// Boolean; True if the current to target edge is set
		GCEdgTxt.Text := IntToStr(GCEdg);	// UInt64;	// Index of the current edge at the current node
		GMnIdxTxt.Text := IntToStr(GMnIdx);	// UInt64;	// Index of the current edge checkstring node
		GMnIDTxt.Text := IntToStr(GMnID);	// Int64;	// Current edge manaz (checkctring node ID) field
		GMnFrmTxt.Text := GMnFrm;		// RBString;	// Current edge check string
		if GEdge then GCEFlgTxt.Text := ByteToBitStr(Nod[GCIdx].Edg[GCEdg].Flg);
		if GEdge then GTEFlgTxt.Text := ByteToBitStr(Nod[GTIdx].Edg[GTEdg].Flg);
		GTEdgTxt.Text := IntToStr(GTEdg);	// UInt64;	// Index of the current edge at the target node
		GTpIdxTxt.Text := IntToStr(GTpIdx);	// UInt64;	// Index of the current edge type node
		GTpIDTxt.Text := IntToStr(GTpID);	// Int64;	// Current edge type (property node ID) field
		GTpNmeTxt.Text := GTpNme;		// RBString;	// Current edge type name
		//--------------------------------------------------------------------------------------------------------------------------------
		GCEventTxt.Text := IntToStr(GCEvent);	// Byte; Code of the current event
	end;
end;

//================================================================================================================================================
// LEVEL 0. AESTHESIA INTERFACE
//================================================================================================================================================
//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.0. FormActivate - Initial program actions with the database
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.FormActivate(Sender: TObject);
begin
	if RFileOpen.Execute then GIDSFile := RFileOpen.Filename else GIDSFile := 'Pyramidion 019.ids';
	SNodGrd.Cells[0, 0] := 'SNodes';
	INodGrd.Cells[0, 0] := 'INodes';
	UNodGrd.Cells[0, 0] := 'UNodes';

	// Initialize global pointers
	GCID := 0;
	GCIdx := 0;
	GCFlg := 0;
	GCFrm := '';
	GCAdv := 0;
	GCNme := '';
	GTarg := False;
	GTID := -1;
	GTIdx := 0;
	GTFlg := 0;
	GTFrm := '';
	GTAdv := -1;
	GTNme := '';
	GEdge := False;
	GCEdg := 0;
	GMnIdx := 0;
	GMnID := -1;
	GMnFrm := '';
	GTEdg := 0;
	GTpIdx := 0;
	GTpID := -1;
	GTpNme := '';

	ALog(00);
	GQueryAsked := True;
	SConEdit;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.1. Redraw - Fill all the form fields with the current data
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.Redraw();
var
	Cnt1, Cnt2, CIdx: Integer;
	SNod: array of Integer = ();								// Superior nodes
	INod: array of Integer = ();								// Inferior nodes
	UNod: array of Integer = ();								// Unlinked nodes
begin
	if GCIdx = GTIdx then GTarg := False;							// Prevent cyclic references
	NodCntPlq.Caption := IntToStr(Length(Nod) - GNodDelCnt);				// Refresh (non-deleted) nodes count
	NodDPlq.Caption := IntToStr(GNodDelCnt);						// Refresh deleted nodes count
	SagCntPlq.Caption := IntToStr(GEdgeCnt);						// Refresh (non-deleted) edges count
	SagDPlq.Caption := IntToStr(GEdgDelCnt);						// Refresh deleted edges count

	//----------------------------------------------------------------------------------------------------------------------------------------
	//	Accessory arrays filling
	//----------------------------------------------------------------------------------------------------------------------------------------
	if not DelFlg(GCIdx) then if (OutFlg(GCIdx) or IncFlg(GCIdx)) then			// Check if the node not deleted and has edges
		for Cnt1 := 0 to High(Nod[GCIdx].Edg) do
		if not EDelFlg(GCIdx, Cnt1) then						// Check if edge isn't deleted
		if EOutFlg(GCIdx, Cnt1) then begin						// Fill descending nodes array
			SetLength(INod, Length(INod) + 1);
			INod[High(INod)] := Nod[GCIdx].Edg[Cnt1].ID;
			CIdx := Ap.Idx(Nod[GCIdx].Edg[Cnt1].ID);
			SetLnkFlg(CIdx);							// Raise Lnk flag
		end
		else begin									// Fill ascending nodes array
			SetLength(SNod, Length(SNod) + 1);
			SNod[High(SNod)] := Nod[GCIdx].Edg[Cnt1].ID;
			CIdx := Ap.Idx(Nod[GCIdx].Edg[Cnt1].ID);
			SetLnkFlg(CIdx);							// Raise Lnk flag
		end;
	for Cnt1 := 0 to High(Nod) do 	 							// Fill unlinked nodes array <- CAN CAUSE LAGS
		// Note: here Cnt1 could start from 1 not 0, because Node 0 just can't be "unlinked" but
		// for the initial Nod array (only Node 0 present) this should trigger range check error!
		if not LnkFlg(Cnt1) then begin							// Check if Lnk flag is lowered
			if Cnt1 <> GCIdx then							// The current node Lnk flag is lowered, too!
				if not DelFlg(Cnt1) then begin					// Check if the node isn't deleted
					SetLength(UNod, Length(UNod) + 1);
					UNod[High(UNod)] := Nod[Cnt1].ID;
				end;
			if Cnt1 = GTIdx then EdgDir.Caption := 'Undefined';
		end
		else ClrLnkFlg(Cnt1);								// Lower Lnk flag
	//----------------------------------------------------------------------------------------------------------------------------------------
	//	Superior Nodes grid filling
	//----------------------------------------------------------------------------------------------------------------------------------------
	SNodGrd.RowCount := Length(SNod) + 1;
	SNodGrd.Row := 0;
	for Cnt1 := 0 to High(SNod) do begin
		Cnt2 := Ap.Idx(SNod[Cnt1]);
		SNodGrd.Cells[0, Cnt1 + 1] := IntToStr(Cnt1 + 1);
		SNodGrd.Cells[1, Cnt1 + 1] := IntToStr(Nod[Cnt2].ID);
		SNodGrd.Cells[2, Cnt1 + 1] := ByteToBitStr(Nod[Cnt2].Flg);
		if AdvFlg(Cnt2) then SNodGrd.Cells[3, Cnt1 + 1] := IntToStr(Nod[Cnt2].Adv) else SNodGrd.Cells[3, Cnt1 + 1] := 'None';
		if FrmFlg(Cnt2) then SNodGrd.Cells[4, Cnt1 + 1] := Nod[Cnt2].Frm else SNodGrd.Cells[4, Cnt1 + 1] := 'None';
		if NmeFlg(Cnt2) then SNodGrd.Cells[5, Cnt1 + 1] := Nod[Cnt2].Nme else SNodGrd.Cells[5, Cnt1 + 1] := 'None';
	end;
	//----------------------------------------------------------------------------------------------------------------------------------------
	//	Inferior Nodes grid filling
	//----------------------------------------------------------------------------------------------------------------------------------------
	INodGrd.RowCount := Length(INod) + 1;
	INodGrd.Row := 0;
	for Cnt1 := 0 to High(INod) do begin
		Cnt2 := Ap.Idx(INod[Cnt1]);
		INodGrd.Cells[0, Cnt1 + 1] := IntToStr(Cnt1 + 1);
		INodGrd.Cells[1, Cnt1 + 1] := IntToStr(Nod[Cnt2].ID);
		INodGrd.Cells[2, Cnt1 + 1] := ByteToBitStr(Nod[Cnt2].Flg);
		if AdvFlg(Cnt2) then INodGrd.Cells[3, Cnt1 + 1] := IntToStr(Nod[Cnt2].Adv) else INodGrd.Cells[3, Cnt1 + 1] := 'None';
		if FrmFlg(Cnt2) then INodGrd.Cells[4, Cnt1 + 1] := Nod[Cnt2].Frm else INodGrd.Cells[4, Cnt1 + 1] := 'None';
		if NmeFlg(Cnt2) then INodGrd.Cells[5, Cnt1 + 1] := Nod[Cnt2].Nme else INodGrd.Cells[5, Cnt1 + 1] := 'None';
	end;
	//----------------------------------------------------------------------------------------------------------------------------------------
	//	Unlinked Nodes grid filling
	//----------------------------------------------------------------------------------------------------------------------------------------
	UNodGrd.RowCount := Length(UNod) + 1;
	UNodGrd.Row := 0;
	for Cnt1 := 0 to High(UNod) do begin
		Cnt2 := Ap.Idx(UNod[Cnt1]);
		UNodGrd.Cells[0, Cnt1 + 1] := IntToStr(Cnt1 + 1);
		UNodGrd.Cells[1, Cnt1 + 1] := IntToStr(Nod[Cnt2].ID);
		UNodGrd.Cells[2, Cnt1 + 1] := ByteToBitStr(Nod[Cnt2].Flg);
		if AdvFlg(Cnt2) then UNodGrd.Cells[3, Cnt1 + 1] := IntToStr(Nod[Cnt2].Adv) else UNodGrd.Cells[3, Cnt1 + 1] := 'None';
		if FrmFlg(Cnt2) then UNodGrd.Cells[4, Cnt1 + 1] := Nod[Cnt2].Frm else UNodGrd.Cells[4, Cnt1 + 1] := 'None';
		if NmeFlg(Cnt2) then UNodGrd.Cells[5, Cnt1 + 1] := Nod[Cnt2].Nme else UNodGrd.Cells[5, Cnt1 + 1] := 'None';
	end;
	//----------------------------------------------------------------------------------------------------------------------------------------
	//	Refreshing interface fields
	//----------------------------------------------------------------------------------------------------------------------------------------
	//	Fill current node fields
	if GCIdx < Length(Nod) then CNodID.Text := IntToStr(Nod[GCIdx].ID);
	CNodFlg.Caption := ByteToBitStr(GCFlg);
	if AdvFlg(GCIdx) then CNodAdv.Text := IntToStr(GCAdv) else CNodAdv.Text := '';
	if FrmFlg(GCIdx) then CNodFrm.Text := GCFrm else CNodFrm.Text := '';
	if NmeFlg(GCIdx) then CNodNme.Text := GCNme else CNodNme.Text := '';
	// Fill target edge & node fields
	if GTarg then begin
		if GEdge then begin
			if not EDelFlg(GTIdx, GCEdg) then begin
				if EOutFlg(GCIdx, GCEdg) then EdgDir.Caption := 'Outgoing' else EdgDir.Caption := 'Incoming';
				if GMnID = -1 then Manaz.Text := '' else Manaz.Text := IntToStr(GMnID);
				if GMnID = -1 then ManazTxt.Caption := 'Undefined' else ManazTxt.Caption := GMnFrm;
				if GTpID = -1 then ETp.Text := '' else ETp.Text := IntToStr(GTpID);
				if GTpID = -1 then ETpTxt.Caption := 'Undefined' else ETpTxt.Caption := GTpNme;
			end
			else begin
				GCEdg := 0;
				GMnIdx := 0;
				GMnID := -1;
				GMnFrm := '';
				GTEdg := 0;
				GTpIdx := 0;
				GTpID := -1;
				GTpNme := '';
				EdgDir.Caption := 'Undefined';
				Manaz.Text := '';
				ManazTxt.Caption := 'Undefined';
				ETp.Text := '';
				ETpTxt.Caption := 'Undefined';
			end;
		end
		else begin
			GCEdg := 0;
			GMnIdx := 0;
			GMnID := -1;
			GMnFrm := '';
			GTEdg := 0;
			GTpIdx := 0;
			GTpID := -1;
			GTpNme := '';
			EdgDir.Caption := 'Undefined';
			Manaz.Text := '';
			ManazTxt.Caption := 'Undefined';
			ETp.Text := '';
			ETpTxt.Caption := 'Undefined';
		end;
		TNodID.Text := IntToStr(GTID);
		TNodFlg.Text := ByteToBitStr(GTFlg);
		if AdvFlg(GTIdx) then TNodAdv.Text := IntToStr(GTAdv) else TNodAdv.Text := '';
		if FrmFlg(GTIdx) then TNodFrm.Text := GTFrm else TNodFrm.Text := '';
		if NmeFlg(GTIdx) then TNodNme.Text := GTNme else TNodNme.Text := '';
	end
	else begin
		// For the edge
		GEdge := False;
		GCEdg := 0;
		GMnIdx := 0;
		GMnID := -1;
		GMnFrm := '';
		GTEdg := 0;
		GTpIdx := 0;
		GTpID := -1;
		GTpNme := '';
		EdgDir.Caption := 'Undefined';
		Manaz.Text := '';
		ManazTxt.Caption := 'Undefined';
		ETp.Text := '';
		ETpTxt.Caption := 'Undefined';
		// For the target node
		GTIdx := 0;
		GTID := -1;
		GTFlg := 0;
		GTFrm := '';
		GTAdv := -1;
		GTNme := '';
		TNodID.Text := '';
		TNodFlg.Text := '';
		TNodAdv.Text := '';
		TNodFrm.Text := '';
		TNodNme.Text := '';
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
{	with LvlGraph do begin
		Clear;
		for Cnt1 := 0 to High(Nod) do
			for Cnt2 := 0 to High(Nod[Cnt1].Edg) do
				if Nod[Cnt1].Edg[Cnt2].Flg and 1 > 0 then Graph.GetEdge(Nod[Cnt1].Adv,
				Nod[Ap.Idx(Nod[Cnt1].Edg[Cnt2].ID)].Adv, true);
		NodeStyle.Shape := lgnsEllipse;
		EdgeStyle.Color := clGray;
		EdgeStyle.HighlightColor := clHighLight;
		NodeStyle.Width := 15;
		NodeStyle.GapTop := 8;
		NodeStyle.CaptionPosition := lgncBottom;
		Font.Size := 11;
	end;}
	GlobalsRefresh();
	GCIDText := CNodID.Text;		// Current text in the CNodID field
	GCFlgText := CNodFlg.Text;		// Current text in the CNodFlg field
	GCFrmText := CNodFrm.Text;		// Current text in the CNodFrm field
	GCAdvText := CNodAdv.Text;		// Current text in the CNodAdv field
	GCNmeText := CNodNme.Text;		// Current text in the CNodNme field
	GTIDText := TNodID.Text;		// Current text in the TNodID field
	GTFlgText := TNodFlg.Text;		// Current text in the TNodFlg field
	GTFrmText := TNodFrm.Text;		// Current text in the TNodFrm field
	GTAdvText := TNodAdv.Text;		// Current text in the TNodAdv field
	GTNmeText := TNodNme.Text;		// Current text in the TNodNme field
	GETpText := ETp.Text;			// Current text in the ETp field
	GMnzText := Manaz.Text;			// Current text in the Manaz field

end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.2. AMonitor - Check current status of Nod and Undo chain and show it in console window
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.AMonitor();
var
	Cnt1, Cnt2: Int64;
	AsDs, RowNo: String;
begin
	if GAllowUndo = 1 then begin
//		InsertColorStyledText(SCon, LineEnding + 'Current GUndo: ' + IntToStr(GUndo) + LineEnding +
//		'--------------------------------------------------------------------------------------------------' +
//		LineEnding, clYellow, [], -1);
//		for Cnt1 := 0 to High(Undo) do begin
//			InsertColorStyledText(SCon, 'Step ' + IntToStr(Cnt1) + ': ', clYellow, [], -1);
//			for Cnt2 := 0 to High(Undo[Cnt1]) do begin
//				InsertColorStyledText(SCon, 'Node ' + IntToStr(Cnt2) + ' ID' + IntToStr(Undo[Cnt1, Cnt2].ID) + ' "' +
//					Undo[Cnt1, Cnt2].Adv + '"; Flg ' + Copy(ByteToBitStr(Undo[Cnt1, Cnt2].Flg), 6, 3) + '; ', clYellow, [],
//					-1);
//				for Cnt3 := 0 to High(Undo[Cnt1, Cnt2].Edg) do begin
//					if (Undo[Cnt1, Cnt2].Edg[Cnt3].Flg and 1) > 0 then AsDs := '↓' else AsDs := '↑';
//					InsertColorStyledText(SCon, 'Edge #' + IntToStr(Cnt3) + ' ' + AsDs + ' ID' +
//						IntToStr(Undo[Cnt1, Cnt2].Edg[Cnt3].ID) + '; ', clYellow, [], -1);
//				end;
//				InsertColorStyledText(SCon, LineEnding, clYellow, [], -1);
//			end;
//			InsertColorStyledText(SCon,
//			'--------------------------------------------------------------------------------------------------' +
//			LineEnding, clYellow, [], -1);
//			SCon.SelStart := Length(SCon.Text)-1;
//		end;
//		RowNo := IntToStr(SCon.Lines.Count);
//		if Length(RowNo) < 4 then RowNo := StringOfChar('0',(4 - Length(RowNo))) + RowNo;
//		InsertColorStyledText(SCon, '' + RowNo + ' > ', clWhite,[], -1);
	end
	else begin
		SCon.Lines.Delete(SCon.Lines.Count - 1);
		InsertColorStyledText(SCon, LineEnding + LineEnding + 'Current Nodes count: ' + IntToStr(Length(Nod)) +
			'Current Edges count: ' + IntToStr(GEdgeCnt) + LineEnding +
			'--------------------------------------------------------------------------------------------------' + LineEnding,
			clYellow, [], -1);
		for Cnt1 := 0 to High(Nod) do begin
			InsertColorStyledText(SCon, 'Node ' + IntToStr(Cnt1) + ' ID' + IntToStr(Nod[Cnt1].ID) + ' "' + Nod[Cnt1].Nme +
				'" Flg ' + ByteToBitStr(Nod[Cnt1].Flg) + '; ', clYellow, [], -1);
			for Cnt2 := 0 to High(Nod[Cnt1].Edg) do begin
				if EOutFlg(Cnt1, Cnt2) then AsDs := '↓' else AsDs := '↑';
				InsertColorStyledText(SCon, 'Edge #' + IntToStr(Cnt2) + ' ' + AsDs + ' ID' + IntToStr(Nod[Cnt1].Edg[Cnt2].ID) +
				'; ', clYellow, [], -1);
			end;
			InsertColorStyledText(SCon, LineEnding, clYellow, [], -1);
		end;
		InsertColorStyledText(SCon,
			'--------------------------------------------------------------------------------------------------' +
			LineEnding, clYellow, [], -1);
		RowNo := IntToStr(SCon.Lines.Count);
		if Length(RowNo) < 4 then RowNo := StringOfChar('0',(4 - Length(RowNo))) + RowNo;
		InsertColorStyledText(SCon, '' + RowNo + ' > ', clWhite,[], -1);
	end;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.3. ALog - Log message handling
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.ALog(Msg: Integer);
var
	UndoStr, EType, RowNo, CurrNode, TargNode, LtrS0, LtrS1, LtrS2, LtrS3: String;
begin
	if GAllowUndo = 1 then UndoStr := 'Step ' + IntToStr(GUndo) + ': ' else UndoStr := '';
	if GTpID < 0 then EType := 'undefined type' else EType := ('type ' + IntToStr(GTpID) + ' "' + GTpNme + '"');
	if GCNme = '' then CurrNode := IntToStr(GCID) else CurrNode := (IntToStr(GCID) + ' "' + GCNme + '"');
	if GTNme = '' then TargNode := IntToStr(GTID) else TargNode := (IntToStr(GTID) + ' "' + GTNme + '"');
	if not GTarg or (GTID < 0) then TargNode := 'undefined/new';
	if SCon.Lines.Count > 1 then begin
		SCon.Lines.Delete(SCon.Lines.Count - 1);
		RowNo := IntToStr(SCon.Lines.Count);
		if Length(RowNo) < 4 then RowNo := StringOfChar('0',(4 - Length(RowNo))) + RowNo;
		InsertColorStyledText(SCon, LineEnding + '  ' + RowNo, clWhite,[], -1);
		InsertColorStyledText(SCon, '  '+ FormatDateTime('YYYY/MM/DD hh:mm:ss:zzz', Now) + ' ', clLime,[], -1);
	end;
	LtrS0 := '';
	LtrS1 := '';
	LtrS2 := '';
	LtrS3 := '';
	if (Length(Nod) - GNodDelCnt) <> 1 then LtrS0 := 's';
	if GEdgeCnt <> 1 then LtrS1 := 's';
	if Length(Nod) <> 1 then LtrS2 := 's';
	if Length(Eid) <> 1 then LtrS3 := 's';
	case Msg of
		// TAesthesiaForm.FormActivate
		00: begin
			InsertColorStyledText(SCon, '  0001  ' + FormatDateTime('YYYY/MM/DD hh:mm:ss:zzz', Now) + '  SESSION started' +
			LineEnding, clYellow,[], -1);
			RowNo := IntToStr(SCon.Lines.Count);
			if Length(RowNo) < 4 then RowNo := StringOfChar('0',(4 - Length(RowNo))) + RowNo;
			InsertColorStyledText(SCon, '  ' + RowNo + '  ', clWhite,[], -1);
			InsertColorStyledText(SCon, FormatDateTime('YYYY/MM/DD hh:mm:ss:zzz', Now), clLime,[], -1);
			InsertColorStyledText(SCon, ' [00] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
			InsertColorStyledText(SCon, 'Load knowledge base from IDS file "' + GIDSFile + '".', clLime, [], -1);
		end;
		01: begin
			InsertColorStyledText(SCon, '[01] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'W: ', clAqua, [], -1);
			InsertColorStyledText(SCon, 'IDS file "' + GIDSFile + '" not found. New IDS file created.', clLime, [], -1);
		end;
		02: begin
			InsertColorStyledText(SCon, '[02] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'W: ', clAqua, [], -1);
			InsertColorStyledText(SCon, 'The file "' + GIDSFile + '" is empty. New IDS file created.', clLime, [], -1);
		end;
		03: begin
			InsertColorStyledText(SCon, '[03] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'W: ', clAqua, [], -1);
			InsertColorStyledText(SCon, 'The file "' + GIDSFile + '" is not a valid IDS v.0.19 database.' +
			' Renamed to "' + GIDSFile + '.old" and new IDS file created.', clLime, [], -1);
		end;
		04: begin
			InsertColorStyledText(SCon, '[04] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'W: ', clAqua, [], -1);
			InsertColorStyledText(SCon, 'CRC32 check failed for the file "' + GIDSFile + '". Renamed to "' + GIDSFile +
			'.old" and new IDS file created.', clLime, [], -1);
		end;
		05: begin
			InsertColorStyledText(SCon, '[05] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
			InsertColorStyledText(SCon, UndoStr + 'Started with IDS file "' + GIDSFile + '" opened with CRC32 ' + IntToStr(GIDSCRC) +
			' and base of ' + IntToStr(Length(Nod) - GNodDelCnt) + ' node'+LtrS0+' and ' + IntToStr(GEdgeCnt) + ' edge' + LtrS1 +
			'.', clLime,
			[], -1);
		end;
		// TAesthesiaForm.CNodIDKeyPress, TAesthesiaForm.CNodIDEdit - function TAngeliophor.AChCurrNode(): integer *
		06: begin
			InsertColorStyledText(SCon, '[06] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
			InsertColorStyledText(SCon, 'Change the current node.', clLime, [], -1);
		end;
		07: begin
			InsertColorStyledText(SCon, '[07] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'W: ', clAqua, [], -1);
			InsertColorStyledText(SCon, 'Node ' + CurrNode + ' not found. Set to 0.', clLime, [], -1);
		end;
		08: begin
			InsertColorStyledText(SCon, '[08] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'W: ', clAqua, [], -1);
			InsertColorStyledText(SCon, 'Node ' + CurrNode + ' is deleted. Set to 0.',
			clLime, [], -1);
		end;
		09: begin
			InsertColorStyledText(SCon, '[09] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
			InsertColorStyledText(SCon, 'New current node ' + CurrNode + '.', clLime, [], -1);
		end;
		// TAesthesiaForm.CNodAdvEdit
		10: begin
			InsertColorStyledText(SCon, '[0A] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
			InsertColorStyledText(SCon, 'Change the current node adventor.', clLime, [], -1);
		end;
		11: begin
			InsertColorStyledText(SCon, '[0B] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
			InsertColorStyledText(SCon, UndoStr + 'New adventor for the current node ' + CurrNode + ': "' + IntToStr(GCAdv) + '".',
			clLime, [], -1);
		end;
		// TAesthesiaForm.TNodIDKeyPress, TAesthesiaForm.TNodIDEdit - function TAngeliophor.AChTargNode(): integer
		12: begin
			InsertColorStyledText(SCon, '[0C] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
			InsertColorStyledText(SCon, 'Change the target node.', clLime, [], -1);
		end;
		13: begin
			InsertColorStyledText(SCon, '[0D] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'W: ', clAqua, [], -1);
			InsertColorStyledText(SCon, 'Node ' + TargNode + ' not found. Set to none.', clLime, [], -1);
		end;
		14: begin
			InsertColorStyledText(SCon, '[0E] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'W: ', clAqua, [], -1);
			InsertColorStyledText(SCon, 'Node ' + TargNode + ' is deleted. Set to none.', clLime, [], -1);
		end;
		15: begin
			InsertColorStyledText(SCon, '[0F] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'E: ', clRed, [], -1);
			InsertColorStyledText(SCon, 'Cyclic edges are not allowed.', clLime, [], -1);
		end;
		17: begin
			InsertColorStyledText(SCon, '[11] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
			InsertColorStyledText(SCon, 'New target node ' + TargNode + '.', clLime, [], -1);
		end;
		// TAesthesiaForm.ETpKeyPress, TAesthesiaForm.TETpEdit	- function TAngeliophor.AChEType(): integer
		18: begin
			InsertColorStyledText(SCon, '[12] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
			InsertColorStyledText(SCon, 'Change edge type.', clLime, [], -1);
		end;
		19: begin
			InsertColorStyledText(SCon, '[13] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'W: ', clAqua, [], -1);
			InsertColorStyledText(SCon, 'Edge type set to None/Undefined.', clLime, [], -1);
		end;
		21: begin
			InsertColorStyledText(SCon, '[15] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
			InsertColorStyledText(SCon, UndoStr + 'Type of incoming edge to the current node ' + CurrNode +
			' from the target node ' + TargNode + ' set to ' + EType + '.', clLime, [], -1);
		end;
		22: begin
			InsertColorStyledText(SCon, '[16] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
			InsertColorStyledText(SCon, UndoStr + 'Type of outgoing edge from the current node ' + CurrNode +
			' to the target node ' + TargNode + ' set to ' + EType + '.', clLime, [], -1);
		end;
		23: begin
			InsertColorStyledText(SCon, '[17] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'W: ', clAqua, [], -1);
			InsertColorStyledText(SCon, 'The current node ' + CurrNode + ' has no edge with the target node ' +
			TargNode + '. New edge?', clLime, [], -1);
		end;
		// TAesthesiaForm.TNodAdvEdit
		24: begin
			InsertColorStyledText(SCon, '[18] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
			InsertColorStyledText(SCon, 'Change the target node adventor.', clLime, [], -1);
		end;
		25: begin
			InsertColorStyledText(SCon, '[19] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
			InsertColorStyledText(SCon, UndoStr + 'New adventor for the target node ' + TargNode + ': "' + IntToStr(GTAdv) + '".',
			clLime, [], -1);
		end;
		26: begin
			InsertColorStyledText(SCon, '[1A] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'W: ', clAqua, [], -1);
			InsertColorStyledText(SCon, 'No target node specified.', clLime, [], -1);
		end;
		// TAesthesiaForm.AddAscEdgBtnClick, TAesthesiaForm.AddDscEdgBtnClick
		27: begin
			InsertColorStyledText(SCon, '[1B] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
			InsertColorStyledText(SCon, 'Add incoming edge of ' + EType + ' to the current node ' + CurrNode +
			' from the target node ' + TargNode + '.', clLime, [], -1);
		end;
		28: begin
			InsertColorStyledText(SCon, '[1C] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
			InsertColorStyledText(SCon, 'Add outgoing edge of ' + EType + ' from the current node ' + CurrNode +
			' to the target node ' + TargNode + '.', clLime, [], -1);
		end;
		29: begin
			InsertColorStyledText(SCon, '[1D] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'E: ', clRed, [], -1);
			InsertColorStyledText(SCon, 'Node 0 cannot have incoming edges. No edge added.', clLime, [], -1);
		end;
		30: begin
			InsertColorStyledText(SCon, '[1E] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'E: ', clRed, [], -1);
			InsertColorStyledText(SCon, 'Duplicate edges are not allowed. No new edge added.', clLime, [], -1);
		end;
		31: begin
			InsertColorStyledText(SCon, '[1F] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'E: ', clRed, [], -1);
			InsertColorStyledText(SCon, 'Target node ' + TargNode + ' doesn''t exist. No edge added.', clLime, [], -1);
		end;
		32: begin
			InsertColorStyledText(SCon, '[20] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
			InsertColorStyledText(SCon, UndoStr + 'New node ' + TargNode + ' added.',
			clLime, [], -1);
		end;
		33: begin
			InsertColorStyledText(SCon, '[21] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
			InsertColorStyledText(SCon, UndoStr + 'Incoming edge of ' + EType + ' to the current node ' +
			CurrNode + ' from the target node ' + TargNode + ' added.', clLime, [], -1);
		end;
		34: begin
			InsertColorStyledText(SCon, '[22] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
			InsertColorStyledText(SCon, UndoStr + 'Outgoing edge of ' + EType + ' from the current node ' +
			CurrNode + ' to the target node ' + TargNode + ' added.', clLime, [], -1);
		end;
		// TAesthesiaForm.DelCNodBtnClick
		35: begin
			InsertColorStyledText(SCon, '[23] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
			InsertColorStyledText(SCon, 'Delete the current node ' + CurrNode +'.', clLime, [], -1);
		end;
		36: begin
			InsertColorStyledText(SCon, '[24] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'E: ', clRed, [], -1);
			InsertColorStyledText(SCon, 'Node 0 cannot be deleted. Deletion failed.', clLime, [], -1);
		end;
		37: begin
			InsertColorStyledText(SCon, '[25] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
			InsertColorStyledText(SCon, UndoStr + 'Node deleted. New current node ' + CurrNode + '.', clLime, [], -1);
		end;
		// TAesthesiaForm.DelCEdgBtnClick
		38: begin
			InsertColorStyledText(SCon, '[26] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
			InsertColorStyledText(SCon, 'Delete edge ' + CurrNode + ' - ' + TargNode + '.', clLime, [], -1);
		end;
		39: begin
			InsertColorStyledText(SCon, '[27] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'E: ', clRed, [], -1);
			InsertColorStyledText(SCon, 'There is no current Edge to delete.', clLime, [], -1);
		end;
		40: begin
			InsertColorStyledText(SCon, '[29] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'E: ', clRed, [], -1);
			InsertColorStyledText(SCon, UndoStr + 'Edge to Node 0 must be the last edge to delete. Deletion failed.', clLime, [], -1);
		end;
		// TAesthesiaForm.CommitBtnClick	-	function TAngeliophor.ACommit(): integer
		41: begin
			InsertColorStyledText(SCon, '[29] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
			InsertColorStyledText(SCon, 'Commit base and clear the history of changes.', clLime, [], -1);
		end;
		42: begin
			InsertColorStyledText(SCon, '[2A] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
			InsertColorStyledText(SCon, 'Base commited with ' + IntToStr(Length(Nod)) + ' node' + LtrS2 + ' and ' +
			IntToStr(GEdgeCnt) + ' edge' + LtrS1 + '. History of changes cleared.', clLime, [], -1);
		end;
		// TAesthesiaForm.ExitBtnClick
		43: begin
			InsertColorStyledText(SCon, '[2B] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
			InsertColorStyledText(SCon, 'Save database and session log.', clLime, [], -1);
		end;
		44: begin
			InsertColorStyledText(SCon, '[2C] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'A: ', clAqua, [], -1);
			InsertColorStyledText(SCon, 'Database saved to file "' + GIDSFile + '" with CRC32 ' + IntToStr(GIDSCRC) +
			' and base of ' + IntToStr(Length(Nod) - GNodDelCnt) + ' node' + LtrS0 + ' and ' + IntToStr(GEdgeCnt) +
			' edge' + LtrS1 + '.', clLime, [], -1);
		end;
		45: begin
			InsertColorStyledText(SCon, '[2D] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
			InsertColorStyledText(SCon, 'Session log saved to file "' + LeftStr(GIDSFile, Length(GIDSFile) - 3) +
			'log", session closed.' + LineEnding, clLime, [], -1);
		end;
		// TAesthesiaForm.UndoBtnClick
		46: begin
			InsertColorStyledText(SCon, '[2E] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
			InsertColorStyledText(SCon, 'Undo to the step ' + IntToStr(GUndo), clLime, [], -1);
		end;
		// TAesthesiaForm.RedoBtnClick
		47: begin
			InsertColorStyledText(SCon, '[2F] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
			InsertColorStyledText(SCon, 'Redo to the step ' + IntToStr(GUndo), clLime, [], -1);
		end;
		// TAesthesiaForm.FormClose
		48: begin
			InsertColorStyledText(SCon, '[30] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
			InsertColorStyledText(SCon, 'Session close without saving log and database.', clLime, [], -1);
		end;
		// 'Monitor' event
		49: begin
			InsertColorStyledText(SCon, '[31] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
			InsertColorStyledText(SCon, 'Monitor:', clLime, [], -1);
		end;
		50: begin
			InsertColorStyledText(SCon, '[32] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
			InsertColorStyledText(SCon, 'Monitor query completed.', clLime, [], -1);
		end;
		// Processing console query
		51: begin
			InsertColorStyledText(SCon, '[33] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
			InsertColorStyledText(SCon, 'Processing console query.', clLime, [], -1);
		end;
		// Reading Event Strip (Eidetika) from EID file
		52: begin
			InsertColorStyledText(SCon, '[34] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'W: ', clAqua, [], -1);
			InsertColorStyledText(SCon, 'EID file "' + LeftStr(GIDSFile, Length(GIDSFile) - 3) +
			'eid" not found. New EID file created.', clLime, [], -1);
		end;
		53: begin
			InsertColorStyledText(SCon, '[35] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'W: ', clAqua, [], -1);
			InsertColorStyledText(SCon, 'The file "' + LeftStr(GIDSFile, Length(GIDSFile) - 3) +
			'eid" is empty. New EID file created.', clLime, [], -1);
		end;
		54: begin
			InsertColorStyledText(SCon, '[36] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'W: ', clAqua, [], -1);
			InsertColorStyledText(SCon, 'The file "' + LeftStr(GIDSFile, Length(GIDSFile) - 3) +
			'eid" is not a valid EID v.0.19 file. Renamed to "' + LeftStr(GIDSFile, Length(GIDSFile) - 3) +
			'eid.old" and new EID file created.', clLime, [], -1);
		end;
		55: begin
			InsertColorStyledText(SCon, '[37] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'W: ', clAqua, [], -1);
			InsertColorStyledText(SCon, 'CRC32 check failed for the file "' + LeftStr(GIDSFile, Length(GIDSFile) - 3) +
			'eid". Renamed to "' + LeftStr(GIDSFile, Length(GIDSFile) - 3) + 'eid.old" and new EID file created.', clLime, [], -1);
		end;
		56: begin
			InsertColorStyledText(SCon, '[38] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
			InsertColorStyledText(SCon, UndoStr + 'Eidetika EID file "' + LeftStr(GIDSFile, Length(GIDSFile) - 3) +
			'eid" opened with CRC32 ' + IntToStr(GEIDCRC) + ' containing ' + IntToStr(Length(Eid)) + ' record'+LtrS3+'.',
			clLime, [], -1);
		end;
		// Saving Event Strip (Eidetika) to EID file
		57: begin
			InsertColorStyledText(SCon, '[39] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
			InsertColorStyledText(SCon, 'Events strip saved to file "' + LeftStr(GIDSFile, Length(GIDSFile) - 3) +
			'eid" with CRC32 ' + IntToStr(GEIDCRC) + ' containing ' + IntToStr(Length (Eid)) + ' record' + LtrS3 + '.',
			clLime, [], -1);
		end;
		// Editing the current node flags *
		58: begin
			InsertColorStyledText(SCon, '[3A] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
			InsertColorStyledText(SCon, 'Edit the current node flags.', clLime, [], -1);
		end;
		59: begin
			InsertColorStyledText(SCon, '[3B] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
			InsertColorStyledText(SCon, 'Current node flags edited to "' + ByteToBitStr(GCFlg) + '".', clLime, [], -1);
		end;
		// Editing the target node flags
		60: begin
			InsertColorStyledText(SCon, '[3C] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
			InsertColorStyledText(SCon, 'Edit the target node flags.', clLime, [], -1);
		end;
		61: begin
			InsertColorStyledText(SCon, '[3D] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
			InsertColorStyledText(SCon, 'Target node flags edited to "' + ByteToBitStr(GTFlg) + '".', clLime, [], -1);
		end;
		// Adding new node and set it as target node
		62: begin
			InsertColorStyledText(SCon, '[3E] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
			InsertColorStyledText(SCon, 'Add new unlinked node (and set it as target node).', clLime, [], -1);
		end;
		// TAesthesiaForm.CNodFrmEdit
		63: begin
			InsertColorStyledText(SCon, '[3F] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
			InsertColorStyledText(SCon, 'Change the current node formula.', clLime, [], -1);
		end;
		64: begin
			InsertColorStyledText(SCon, '[40] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
			InsertColorStyledText(SCon, UndoStr + 'New formula for the current node: "' + GCFrm + '".', clLime, [],
			-1);
		end;
		// TAesthesiaForm.CNodNmeEdit
		65: begin
			InsertColorStyledText(SCon, '[41] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
			InsertColorStyledText(SCon, 'Change the current node name.', clLime, [], -1);
		end;
		66: begin
			InsertColorStyledText(SCon, '[42] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
			InsertColorStyledText(SCon, UndoStr + 'New name for the current node: "' + GCNme + '".', clLime, [], -1);
		end;
		// TAesthesiaForm.ManazEdit
		67: begin
			InsertColorStyledText(SCon, '[43] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
			InsertColorStyledText(SCon, 'Change the current edge checkstring.', clLime, [], -1);
		end;
		68: begin
			InsertColorStyledText(SCon, '[44] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
			InsertColorStyledText(SCon, UndoStr + 'New checkstring for the current edge: "' + IntToStr(GMnID) + '".', clLime, [], -1);
		end;
		// TAesthesiaForm.TNodFrmEdit
		69: begin
			InsertColorStyledText(SCon, '[45] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
			InsertColorStyledText(SCon, 'Change the target node formula.', clLime, [], -1);
		end;
		70: begin
			InsertColorStyledText(SCon, '[46] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
			InsertColorStyledText(SCon, UndoStr + 'New formula for the target node: "' + GTFrm + '".', clLime, [],
			-1);
		end;
		// TAesthesiaForm.TNodNmeEdit
		71: begin
			InsertColorStyledText(SCon, '[47] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'Q: ', clYellow, [], -1);
			InsertColorStyledText(SCon, 'Change the target node name.', clLime, [], -1);
		end;
		72: begin
			InsertColorStyledText(SCon, '[48] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'A: ', clWhite, [], -1);
			InsertColorStyledText(SCon, UndoStr + 'New name for the target node: "' + GTNme + '".', clLime, [], -1);
		end;
	end;
	RowNo := IntToStr(SCon.Lines.Count);
	if Length(RowNo) < 4 then RowNo := StringOfChar('0',(4 - Length(RowNo))) + RowNo;
	InsertColorStyledText(SCon, LineEnding + '  ' + RowNo + ' > ', clWhite,[], -1);
	SCon.SelStart := Length(SCon.Lines.Text);
	SConStatLbl.Caption := 'Row: ' + IntToStr(SCon.CaretPos.y) + 'Column: ' + IntToStr(SCon.CaretPos.x);
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.4. SConEdit - Console input done
//------------------------------------------------------------------------------------------------------------------------------------------------
//	Note: With accessory procedures SConClick, SConKeyUp and SConKeyPress to catch cursor position, arrow keys and #13 'Ret' & #27 'Esc' keys
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.SConClick(Sender: TObject);
begin
	SConStatLbl.Caption := 'Row: ' + IntToStr(SCon.CaretPos.y) + 'Column: ' + IntToStr(SCon.CaretPos.x);
end;
//------------------------------------------------------------------------------------------------------------------------------------------------
//	Note 1: Arrow keys (#37 [Left]; #38 [Up], #39 [Right], and #40 [Down]) do not generate KeyPress event, only KeyDown and KeyUp
//	Note 2: KeyUp is the only key event processed after caret position update, which is essential when catching arrow keys effect
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.SConKeyUp(Sender: TObject);
begin
	SConStatLbl.Caption := 'Row: ' + IntToStr(SCon.CaretPos.y) + 'Column: ' + IntToStr(SCon.CaretPos.x);
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.SConKeyPress(Sender: TObject; var Key: Char);
var	CurrLine, RowNo: String;
begin
	if Key = #13 then begin
		CurrLine := String(SCon.Lines[SCon.Lines.Count - 1]);
		CurrLine := RightStr(CurrLine, Length(Currline) - 8);
		if Length(CurrLine) > 0 then begin
			SCon.Lines.Delete(SCon.Lines.Count - 1);
			RowNo := IntToStr(SCon.Lines.Count);
			if Length(RowNo) < 4 then RowNo := StringOfChar('0',(4 - Length(RowNo))) + RowNo;
			InsertColorStyledText(SCon, LineEnding + ' ' + RowNo, clWhite,[], -1);
			InsertColorStyledText(SCon, ''+ FormatDateTime('YYYY/MM/DD hh:mm:ss:zzz', Now), clLime,[], -1);
			InsertColorStyledText(SCon, '[FF] ', clGreen, [], -1);
			InsertColorStyledText(SCon, 'Q: ', clYellow,[], -1);
			InsertColorStyledText(SCon, CurrLine, clGreen, [], -1);
			RowNo := IntToStr(SCon.Lines.Count);
			if Length(RowNo) < 4 then RowNo := StringOfChar('0',(4 - Length(RowNo))) + RowNo;
			InsertColorStyledText(SCon, LineEnding + ' ' + RowNo + ' > ', clWhite,[], -1);
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
		InsertColorStyledText(SCon, LineEnding + ' ' + RowNo + ' > ', clWhite,[], -1);
		SCon.SelStart := Length(SCon.Lines.Text);
	end;
	if SCon.CaretPos.Y < (SCon.Lines.Count - 1) then Key := #0;
end;
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.SConEdit();
begin
	if GQueryAsked then begin
		ALog(Ap.Proseychi);
		GQueryAsked := False;
		Redraw;
	end;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.5. CNodIDKeyPress - Current node ID editing input filter *
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.CNodIDKeyPress(Sender: TObject; var Key: Char);
begin
	if not (Key in ['0'..'9', #8, #9, #13, #27]) then Key := #0;					// #8 BackSpace, #9 Tab, #13 Ret, #27 Esc
	if Key = #27 then CNodID.Text := GCIDText;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.6. CNodIDEdit - Current node ID editing done *
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.CNodIDEdit(Sender: TObject);
begin
	if CNodID.Text <> GCIDText then begin
		if CNodID.Text = '' then CNodID.Text := '0';
		if GCID <> StrToInt(CNodID.Text) then begin
			GCID := StrToInt(CNodID.Text);
			ALog(06);
			GQueryAsked := True;
			SConEdit;
		end;
	end;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.X. CNodFlgKeyPress - Current node flags editing input filter *
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.CNodFlgKeyPress(Sender: TObject; var Key: Char);
begin
	if not (Key in ['0', '1', #8, #9, #13, #27]) then Key := #0;					// #8 BackSpace, #9 Tab, #13 Ret, #27 Esc
	if Key = #27 then CNodFlg.Text := ByteToBitStr(GCFlg);
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.7. CNodFlgEdit - Current node flags editing done *
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.CNodFlgEdit(Sender: TObject);
begin
	if CNodFlg.Text <> GCFlgText then begin
		if Length(CNodFlg.Text) < 8 then CNodFlg.Text := StringOfChar('0',(8 - Length(CNodFlg.Text))) + CNodFlg.Text;
		if GCFlg <> BitStrToByte(CNodFlg.Text) then begin
			GCFlg := BitStrToByte(CNodFlg.Text);
			ALog(58);
			GQueryAsked := True;
			SConEdit;
		end;
	end;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.X. CNodAdvKeyPress - Current node adventor editing input filter *
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.CNodAdvKeyPress(Sender: TObject; var Key: Char);
begin
	if not (Key in ['0'..'9', #8, #9, #13, #27]) then Key := #0;					// #8 BackSpace, #9 Tab, #13 Ret, #27 Esc
	if Key = #27 then if AdvFlg(GCIdx) then CNodAdv.Text := GCAdvText else CNodAdv.Text := '';
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.8. CNodAdvEdit - Current node adventor editing done *
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.CNodAdvEdit(Sender: TObject);
begin
	if CNodAdv.Text <> GCAdvText then begin
		if CNodAdv.Text = '' then GCAdv := -1 else GCAdv := StrToInt(CNodAdv.Text);
		ALog(10);
		GQueryAsked := True;
		SConEdit;
	end;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.X. CNodFrmEdit - Current node formula editing done *
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.CNodFrmEdit(Sender: TObject);
begin
	if CNodFrm.Text <> GCFrmText then begin
		if GCFrm <> CNodFrm.Text then begin
			GCFrm := CNodFrm.Text;
			ALog(63);
			GQueryAsked := True;
			SConEdit;
		end;
	end;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.X. CNodNmeEdit - Current node name editing done *
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.CNodNmeEdit(Sender: TObject);
begin
	if CNodNme.Text <> GCNmeText then begin
		if GCNme <> CNodNme.Text then begin
			GCNme := CNodNme.Text;
			ALog(65);
			GQueryAsked := True;
			SConEdit;
		end;
	end;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.9. ETpKeyPress - Current edge type editing input filter *
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.ETpKeyPress(Sender: TObject; var Key: Char);
begin
	if not (Key in ['0'..'9', #8, #9, #13, #27]) then Key := #0;
	if Key = #27 then if GEdge then ETp.Text := GETpText else ETp.Text := '';
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.10. ETpEdit - Current edge type editing done *
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.ETpEdit(Sender: TObject);
begin
	if ETp.Text <> GETpText then begin
		if not GEdge then begin
			ETp.Text := '';
			GETpText := '';
			GTpID := -1;
			ETpTxt.Caption := 'Undefined';
			Exit;
		end;
		if ETp.Text = '' then GTpID := -1 else GTpID := StrToInt(ETp.Text);
		ALog(18);
		GQueryAsked := True;
		SConEdit;
	end;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.X. ManazKeyPress - Current edge Manaz (CkeckString) field editing input filter *
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.ManazKeyPress(Sender: TObject; var Key: Char);
begin
	if not (Key in ['0'..'9', #8, #9, #13, #27]) then Key := #0;
	if Key = #27 then if GEdge then Manaz.Text := GMnzText else Manaz.Text := '';
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.X. ManazEdit - Current edge Manaz (CkeckString) field editing done *
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.ManazEdit(Sender: TObject);
begin
	if Manaz.Text <> GMnzText then begin
		if not GEdge then begin
			Manaz.Text := '';
			GMnzText := '';
			GMnID := -1;
			ManazTxt.Caption := 'Undefined';
			Exit;
		end;
		if Manaz.Text = '' then GMnID := -1 else GMnID := StrToInt(Manaz.Text);
		ALog(67);
		GQueryAsked := True;
		SConEdit;
	end;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.11. TNodIDKeyPress - Filter for target node ID editing *
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.TNodIDKeyPress(Sender: TObject; var Key: Char);
begin
	if not (Key in ['0'..'9', #8, #9, #13, #27]) then Key := #0;
	if Key = #27 then if GTarg then TNodID.Text := GTIDText else TNodID.Text := '';
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.12. TNodIDEdit - Target node ID editing done *
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.TNodIDEdit(Sender: TObject);
begin
	if TNodID.Text <> GTIDText then begin
		if TNodID.Text = '' then TNodID.Text := '-1';
		if GTID <> StrToInt(TNodID.Text) then begin
			GTID := StrToInt(TNodID.Text);
			GTarg := True;
			ALog(12);
			GQueryAsked := True;
			SConEdit;
		end;
	end;
	if TNodID.Text = '-1' then TNodID.Text := '';
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.X. TNodFlgEdit - Target node flags editing input filter *
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.TNodFlgKeyPress(Sender: TObject; var Key: char);
begin
	if not (Key in ['0', '1', #8, #9, #13, #27]) then Key := #0;					// #8 BackSpace, #9 Tab, #13 Ret, #27 Esc
	if Key = #27 then if GTarg then TNodFlg.Text := ByteToBitStr(GTFlg) else TNodFlg.Text := '';
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.13. TNodFlgEdit - Target node flags editing done *
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.TNodFlgEdit(Sender: TObject);
begin
	if TNodFlg.Text <> GTFlgText then begin
		if Length(TNodFlg.Text) < 8 then TNodFlg.Text := StringOfChar('0',(8 - Length(TNodFlg.Text))) + TNodFlg.Text;
		if GTarg then if GTFlg <> BitStrToByte(TNodFlg.Text) then begin
			GTFlg := BitStrToByte(TNodFlg.Text);
			ALog(60);
			GQueryAsked := True;
			SConEdit;
		end;
	end;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.X. TNodAdvKeyPress - Target node adventor editing input filter *
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.TNodAdvKeyPress(Sender: TObject; var Key: Char);
begin
	if not (Key in ['0'..'9', #8, #9, #13, #27]) then Key := #0;
	if Key = #27 then if GTarg then TNodAdv.Text := GTAdvText else TNodAdv.Text := '';
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.14. TNodAdvEdit - Target node adventor editing done *
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.TNodAdvEdit(Sender: TObject);
begin
	if TNodAdv.Text <> GTAdvText then begin
		if TNodAdv.Text = '' then GTAdv := -1 else GTAdv := StrToInt(TNodAdv.Text);
		ALog(24);
		GQueryAsked := True;
		SConEdit;
	end;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.X. TNodFrmEdit - Target node formula editing done *
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.TNodFrmEdit(Sender: TObject);
begin
	if TNodFrm.Text <> GTFrmText then begin
		if not GTarg then TNodFrm.Text := 'None' else if GTFrm <> TNodFrm.Text then begin
			GTFrm := TNodFrm.Text;
			ALog(69);
			GQueryAsked := True;
			SConEdit;
		end;
	end;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.X. TNodNmeEdit - Target node name editing done *
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.TNodNmeEdit(Sender: TObject);
begin
	if TNodNme.Text <> GTNmeText then begin
		if not GTarg then TNodNme.Text := '' else if GTNme <> TNodNme.Text then begin
			GTNme := TNodNme.Text;
			ALog(71);
			GQueryAsked := True;
			SConEdit;
		end;
	end;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.15. AddAscEdgBtnClick - Add ascending edge button click *
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.AddAscEdgBtnClick(Sender: TObject);
begin
	ALog(27);
	GQueryAsked := True;
	SConEdit;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.16. AddDscEdgBtnClick - Add descending edge button click *
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.AddDscEdgBtnClick(Sender: TObject);
begin
	Alog(28);
	GQueryAsked := True;
	SConEdit;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.17. DelCNodBtnClick - Delete current node button click *
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.DelCNodBtnClick(Sender: TObject);
begin
	ALog(35);
	GQueryAsked := True;
	SConEdit;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.18. DelCEdgBtnClick - Delete current edge button click *
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.DelCEdgBtnClick(Sender: TObject);
begin
	if GEdge then begin
		ALog(38);
		GQueryAsked := True;
		SConEdit;
	end
	else ALog(39);
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.19. CNodLblMouseDown - Current Node label mouse down. Calls PopMenu on the right click
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.CNodLblMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	if (Button = mbRight) or (ssRight in Shift) then begin
		SetCurrSI.Visible := False;
		SetTargSI.Visible := False;
		SetCurrII.Visible := False;
		SetTargII.Visible := False;
		SetCurrUI.Visible := False;
		SetTargUI.Visible := False;
		AddSEdgI.Visible := False;
		AddIEdgI.Visible := False;
		AddSNodI.Visible := False;
		AddINodI.Visible := False;
		AddUNodI.Visible := False;
		DelNodSI.Visible := False;
		DelNodII.Visible := False;
		DelNodUI.Visible := False;
		PopMenu.Popup(X+1028,Y+77);
	end;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.20. CEdgLblMouseDown - Current Edge label mouse down. Calls PopMenu on the right click
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.CEdgLblMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	if (Button = mbRight) or (ssRight in Shift) then begin
		SetCurrSI.Visible := False;
		SetTargSI.Visible := False;
		SetCurrII.Visible := False;
		SetTargII.Visible := False;
		SetCurrUI.Visible := False;
		SetTargUI.Visible := False;
		AddSEdgI.Visible := False;
		AddIEdgI.Visible := False;
		AddSNodI.Visible := False;
		AddINodI.Visible := False;
		AddUNodI.Visible := False;
		DelNodSI.Visible := False;
		DelNodII.Visible := False;
		DelNodUI.Visible := False;
		PopMenu.Popup(X+1028,Y+77);
	end;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.21. TNodLblMouseDown - Target Node label mouse down. Calls PopMenu on the right click
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.TNodLblMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	if (Button = mbRight) or (ssRight in Shift) then begin
		SetCurrSI.Visible := False;
		SetTargSI.Visible := False;
		SetCurrII.Visible := False;
		SetTargII.Visible := False;
		SetCurrUI.Visible := False;
		SetTargUI.Visible := False;
		AddSEdgI.Visible := False;
		AddIEdgI.Visible := False;
		AddSNodI.Visible := False;
		AddINodI.Visible := False;
		AddUNodI.Visible := False;
		DelNodSI.Visible := False;
		DelNodII.Visible := False;
		DelNodUI.Visible := False;
		PopMenu.Popup(X+1028,Y+77);
	end;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.22. SNodGrdMouseDown - superior nodes grid mouse down. Calls PopMenu on the right click
//------------------------------------------------------------------------------------------------------------------------------------------------
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
			SetCurrSI.Visible := True;
			SetTargSI.Visible := True;
			DelNodSI.Visible := True;
		end
		else begin
			SetCurrSI.Visible := False;
			SetTargSI.Visible := False;
			DelNodSI.Visible := False;
		end;
		SetCurrII.Visible := False;
		SetTargII.Visible := False;
		SetCurrUI.Visible := False;
		SetTargUI.Visible := False;
		AddSEdgI.Visible := False;
		AddIEdgI.Visible := False;
		AddSNodI.Visible := True;
		AddINodI.Visible := False;
		AddUNodI.Visible := False;
		DelNodII.Visible := False;
		DelNodUI.Visible := False;
		PopMenu.Popup(X+1028,Y+77);
	end;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.23. SNodGrdDblClick - superior nodes grid double click. Switches goEditing property on to allow in-cell editing
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.SNodGrdDblClick(Sender: TObject);
begin
	with SNodGrd do begin
		GTarg := True;
		GTIdx := Ap.Idx(StrToInt(Cells[1, Row]));
		GTID := StrToInt(Cells[1, Row]);
		GTFlg := BitStrToByte(Cells[2, Row]);
		if Cells[3, Row] <> 'None' then GTAdv := StrToInt(Cells[3, Row]) else GTAdv := -1;
		if Cells[4, Row] <> 'None' then GTFrm := Cells[4, Row] else GTFrm := '';
		if Cells[5, Row] <> 'None' then GTNme := Cells[5, Row] else GTNme := '';
		if Col = 5 then Options := Options + [goEditing] + [goAlwaysShowEditor];
	end;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.24. SNodGrdEditingDone - superior nodes grid cell editing done. Switches goEditing property off to forbid in-cell editing
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.SNodGrdEditingDone(Sender: TObject);
begin
	with SNodGrd do begin
		Options := Options - [goEditing] - [goAlwaysShowEditor];
		if Cells[5, Row] <> GTNme then begin
			ALog(24);
			GQueryAsked := True;
		end;
		SConEdit;
	end;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.25. INodGrdMouseDown - inferior nodes grid mouse down. Calls PopMenu on the right click
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.INodGrdMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var CurrRow, CurrCol: Integer;
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
			SetCurrII.Visible := True;
			SetTargII.Visible := True;
			DelNodII.Visible := True;
		end
		else begin
			SetCurrII.Visible := False;
			SetTargII.Visible := False;
			DelNodII.Visible := False;
		end;
		SetCurrSI.Visible := False;
		SetTargSI.Visible := False;
		SetCurrUI.Visible := False;
		SetTargUI.Visible := False;
		AddSEdgI.Visible := False;
		AddIEdgI.Visible := False;
		AddSNodI.Visible := False;
		AddINodI.Visible := True;
		AddUNodI.Visible := False;
		DelNodSI.Visible := False;
		DelNodUI.Visible := False;
		PopMenu.Popup(X+1028,Y+372);
	end;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.26. INodGrdDblClick - inferior nodes grid double click. Switches goEditing property on to allow in-cell editing
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.INodGrdDblClick(Sender: TObject);
begin
	with INodGrd do begin
		GTarg := True;
		GTIdx := Ap.Idx(StrToInt(Cells[1, Row]));
		GTID := StrToInt(Cells[1, Row]);
		GTFlg := BitStrToByte(Cells[2, Row]);
		if Cells[3, Row] <> 'None' then GTAdv := StrToInt(Cells[3, Row]) else GTAdv := -1;
		if Cells[4, Row] <> 'None' then GTFrm := Cells[4, Row] else GTFrm := '';
		if Cells[5, Row] <> 'None' then GTNme := Cells[5, Row] else GTNme := '';
		if Col = 5 then Options := Options + [goEditing] + [goAlwaysShowEditor];
	end;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.27. INodGrdEditingDone - inferior nodes grid cell editing done. Switches goEditing property off to forbid in-cell editing
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.INodGrdEditingDone(Sender: TObject);
begin
	with INodGrd do begin
		Options := Options - [goEditing] - [goAlwaysShowEditor];
		if Cells[5, Row] <> GTNme then begin
			ALog(24);
			GQueryAsked := True;
		end;
		SConEdit;
	end;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.28. UNodGrdMouseDown - unlinked nodes grid mouse down. Calls PopMenu on the right click
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.UNodGrdMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var CurrRow, CurrCol: Integer;
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
			SetCurrUI.Visible := True;
			SetTargUI.Visible := True;
			AddSEdgI.Visible := True;
			AddIEdgI.Visible := True;
			DelNodUI.Visible := True;
		end
		else begin
			SetCurrUI.Visible := False;
			SetTargUI.Visible := False;
			AddSEdgI.Visible := False;
			AddIEdgI.Visible := False;
			DelNodUI.Visible := False;
		end;
		SetCurrSI.Visible := False;
		SetTargSI.Visible := False;
		SetCurrII.Visible := False;
		SetTargII.Visible := False;
		AddSNodI.Visible := False;
		AddINodI.Visible := False;
		AddUNodI.Visible := True;
		DelNodSI.Visible := False;
		DelNodII.Visible := False;
		PopMenu.Popup(X+1028,Y+603);
	end;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.29. UNodGrdDblClick - unlinked nodes grid double click. Switches goEditing property on to allow in-cell editing
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.UNodGrdDblClick(Sender: TObject);
begin
	with UNodGrd do begin
		GTarg := True;
		GTIdx := Ap.Idx(StrToInt(Cells[1, Row]));
		GTID := StrToInt(Cells[1, Row]);
		GTFlg := BitStrToByte(Cells[2, Row]);
		if Cells[3, Row] <> 'None' then GTAdv := StrToInt(Cells[3, Row]) else GTAdv := -1;
		if Cells[4, Row] <> 'None' then GTFrm := Cells[4, Row] else GTFrm := '';
		if Cells[5, Row] <> 'None' then GTNme := Cells[5, Row] else GTNme := '';
		if Col = 5 then Options := Options + [goEditing] + [goAlwaysShowEditor];
	end;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.30. UNodGrdEditingDone - unlinked nodes grid cell editing done. Switches goEditing property off to forbid in-cell editing
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.UNodGrdEditingDone(Sender: TObject);
begin
	with UNodGrd do begin
		Options := Options - [goEditing] - [goAlwaysShowEditor];
		if Cells[5, Row] <> GTNme then begin
			ALog(24);
			GQueryAsked := True;
		end;
		SConEdit;
	end;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.31. SetCurrIClick - Popup menu item 'Set as current node' click
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.SetCurrIClick(Sender: TObject);
begin
	if (Sender = SetCurrSI) then CNodID.Text := SNodGrd.Cells[1, SNodGrd.Row];
	if (Sender = SetCurrII) then CNodID.Text := INodGrd.Cells[1, INodGrd.Row];
	if (Sender = SetCurrUI) then CNodID.Text := UNodGrd.Cells[1, UNodGrd.Row];
	if (Sender = DelNodSI) then CNodID.Text := SNodGrd.Cells[1, SNodGrd.Row];
	if (Sender = DelNodII) then CNodID.Text := INodGrd.Cells[1, INodGrd.Row];
	if (Sender = DelNodUI) then CNodID.Text := UNodGrd.Cells[1, UNodGrd.Row];
	GCID := StrToInt(CNodID.Text);
	if GCID <> Nod[GCIdx].ID then begin
		ALog(06);
		GQueryAsked := True;
		SConEdit;
	end;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.32. SetTargIClick - Popup menu item 'Set as target node' click
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.SetTargIClick(Sender: TObject);
begin
	if (Sender = SetTargSI) then TNodID.Text := SNodGrd.Cells[1, SNodGrd.Row];
	if (Sender = SetTargII) then TNodID.Text := INodGrd.Cells[1, INodGrd.Row];
	if (Sender = SetTargUI) then TNodID.Text := UNodGrd.Cells[1, UNodGrd.Row];
	if (Sender = AddSEdgI) then TNodID.Text := UNodGrd.Cells[1, UNodGrd.Row];
	if (Sender = AddIEdgI) then TNodID.Text := UNodGrd.Cells[1, UNodGrd.Row];
	if GTID <> StrToInt(TNodID.Text) then begin
		GTarg := True;
		GTID := StrToInt(TNodID.Text);
		ALog(12);
		GQueryAsked := True;
		SConEdit;
	end;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.33. AddSEdgIClick - Popup menu item 'Create edge to the current node' click
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.AddSEdgIClick(Sender: TObject);
begin
	SetTargIClick(AddSEdgI);
	AddAscEdgBtnClick(AddSEdgI);
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.34. AddIEdgIClick - Popup menu item 'Create edge from the current node' click
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.AddIEdgIClick(Sender: TObject);
begin
	SetTargIClick(AddIEdgI);
	AddDscEdgBtnClick(AddIEdgI);
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.35. AddNodIClick - Popup menu item 'Add [superior, inferior, unlinked] node' click
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.AddNodIClick(Sender: TObject);
begin
	if (Sender = AddSNodI) then begin								// Add new node and new edge from it
		GTarg := False;
		ALog(27);
	end;
	if (Sender = AddINodI) then begin								// Add new node and new edge to it
		GTarg := False;
		ALog(28);
	end;
	if (Sender = AddUNodI) then begin								// Add new unlinked target node
		GEdge := False;
		ETp.Text := '';
		ETpTxt.Caption := 'Undefined';
		GCIdx := 0;
		GTarg := False;
		ALog(62);
	end;
	GQueryAsked := True;
	SConEdit;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.36. DelNodIClick - Popup menu item 'Delete [superior, inferior, unlinked] node' click
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.DelNodIClick(Sender: TObject);
begin
	SetCurrIClick(Sender);
	DelCNodBtnClick(Sender);
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.43. CommitBtnClick - Commit button click
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.CommitBtnClick(Sender: TObject);
begin
	ALog(41);
	GQueryAsked := True;
	SConEdit;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.44. ExitBtnClick - Exit button click
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.ExitBtnClick(Sender: TObject);
begin
	ALog(43);
	GQueryAsked := True;
	SConEdit;
	Close;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.45. UndoBtnClick - Undo the last action done (if the base not committed)
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.UndoBtnClick(Sender: TObject);
begin
//	if GAllowUndo = 1 then if GUndo > 0 then begin
//		GUndo := GUndo-1;
//		Nod := Copy(Undo[GUndo]);
//		AMonitor;
		GCIdx := 0;
		GTarg := False;
		GEdge := False;
		Redraw;
		ALog(46);
//	end;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.46. RedoBtnClick - Redo the last action undone (if the base not committed)
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.RedoBtnClick(Sender: TObject);
begin
//	if GAllowUndo = 1 then if GUndo < High(Undo) then begin
//		GUndo := GUndo + 1;
//		Nod := Copy(Undo[GUndo]);
//		AMonitor;
		GCIdx := 0;
		GTarg := False;
		GEdge := False;
		Redraw;
		ALog(47);
//	end;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.47. FormClose - Form close without saving any logs or changes to IDS file
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.FormClose(Sender: TObject);
begin
	ALog(48);
	GQueryAsked := True;
	SConEdit;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.48. MonitorBtnClick - Types all of the Nod array content to session console
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.MonitorBtnClick(Sender: TObject);
begin
	ALog(49);
	GQueryAsked := True;
	SConEdit;
end;

//================================================================================================================================================
//------------------------------------------------------------------------------------------------------------------------------------------------
//	0.-1. Color manipulations for the TAesthesiaForm elements
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.CMouseEnter(Sender: TObject);
var
	PyramidionLogoImg: TPortableNetworkGraphic;
	RStream: TResourceStream;
begin
	if (Sender = CNodLbl) or (Sender = CNodIDLbl) or (Sender = CNodID) or (Sender = CNodFlgLbl) or (Sender = CNodFlg) or
	(Sender = CNodFrmLbl) or (Sender = CNodFrm) or (Sender = CNodAdvLbl) or (Sender = CNodAdv) or (Sender = CNodNmeLbl) or
	(Sender = CNodNme) then begin
		CNodLbl.Color := clHighlight;
		CEdgLbl.Color := clMenuBar;
		EdgDirLbl.Color := clBtnShadow;
		ETpLbl.Color := clBtnShadow;
		ManazLbl.Color := clBtnShadow;
		TNodLbl.Color := clMenuBar;
		TNodIDLbl.Color := clBtnShadow;
		TNodFlgLbl.Color := clBtnShadow;
		TNodFrmLbl.Color := clBtnShadow;
		TNodAdvLbl.Color := clBtnShadow;
		TNodNmeLbl.Color := clBtnShadow;
	end;
	if (Sender = CNodIDLbl) or (Sender = CNodID) then CNodIDLbl.Color := clHighlight;
	if (Sender = CNodFlgLbl) or (Sender = CNodFlg) then CNodFlgLbl.Color := clHighlight;
	if (Sender = CNodFrmLbl) or (Sender = CNodFrm) then CNodFrmLbl.Color := clHighlight;
	if (Sender = CNodAdvLbl) or (Sender = CNodAdv) then CNodAdvLbl.Color := clHighlight;
	if (Sender = CNodNmeLbl) or (Sender = CNodNme) then CNodNmeLbl.Color := clHighlight;
	if (Sender = CEdgLbl) or (Sender = EdgDirLbl) or (Sender = EdgDir) or (Sender = ETpLbl) or (Sender = ETp) or (Sender = ETpTxt) or
	(Sender = ManazLbl) or (Sender = Manaz) or (Sender = ManazTxt) then begin
		CNodLbl.Color := clMenuBar;
		CNodIDLbl.Color := clBtnShadow;
		CNodFlgLbl.Color := clBtnShadow;
		CNodFrmLbl.Color := clBtnShadow;
		CNodAdvLbl.Color := clBtnShadow;
		CNodNmeLbl.Color := clBtnShadow;
		CEdgLbl.Color := clHighlight;
		TNodLbl.Color := clMenuBar;
		TNodIDLbl.Color := clBtnShadow;
		TNodFlgLbl.Color := clBtnShadow;
		TNodFrmLbl.Color := clBtnShadow;
		TNodAdvLbl.Color := clBtnShadow;
		TNodNmeLbl.Color := clBtnShadow;
	end;
	if (Sender = EdgDirLbl) or (Sender = EdgDir) then EdgDirLbl.Color := clHighlight;
	if (Sender = ETpLbl) or (Sender = ETp) or (Sender = ETpTxt) then ETpLbl.Color := clHighlight;
	if (Sender = ManazLbl) or (Sender = Manaz) or (Sender = ManazTxt) then ManazLbl.Color := clHighlight;
	if (Sender = TNodLbl) or (Sender = TNodIDLbl) or (Sender = TNodID) or (Sender = TNodFlgLbl) or (Sender = TNodFlg) or
	(Sender = TNodFrmLbl) or (Sender = TNodFrm) or (Sender = TNodAdvLbl) or (Sender = TNodAdv) or (Sender = TNodNmeLbl) or
	(Sender = TNodAdv) then begin
		CNodLbl.Color := clMenuBar;
		CNodIDLbl.Color := clBtnShadow;
		CNodFlgLbl.Color := clBtnShadow;
		CNodFrmLbl.Color := clBtnShadow;
		CNodAdvLbl.Color := clBtnShadow;
		CNodNmeLbl.Color := clBtnShadow;
		CEdgLbl.Color := clMenuBar;
		EdgDirLbl.Color := clBtnShadow;
		ETpLbl.Color := clBtnShadow;
		ManazLbl.Color := clBtnShadow;
		TNodLbl.Color := clHighlight;
	end;
	if (Sender = TNodIDLbl) or (Sender = TNodID) then TNodIDLbl.Color := clHighlight;
	if (Sender = TNodFlgLbl) or (Sender = TNodFlg) then TNodFlgLbl.Color := clHighlight;
	if (Sender = TNodFrmLbl) or (Sender = TNodFrm) then TNodFrmLbl.Color := clHighlight;
	if (Sender = TNodAdvLbl) or (Sender = TNodAdv) then TNodAdvLbl.Color := clHighlight;
	if (Sender = TNodNmeLbl) or (Sender = TNodNme) then TNodNmeLbl.Color := clHighlight;
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
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TAesthesiaForm.CMouseLeave(Sender: TObject);
var
	PyramidionLogoImg: TPortableNetworkGraphic;
	RStream: TResourceStream;
begin
	if (Sender = CNodLbl) or (Sender = CNodIDLbl) or (Sender = CNodID) or (Sender = CNodFlgLbl) or (Sender = CNodFlg) or
	(Sender = CNodFrmLbl) or (Sender = CNodFrm) or (Sender = CNodAdvLbl) or (Sender = CNodAdv) or (Sender = CNodNmeLbl) or
	(Sender = CNodNme) then CNodLbl.Color := clMenuBar;
	if (Sender = CNodIDLbl) or (Sender = CNodID) then CNodIDLbl.Color := clBtnShadow;
	if (Sender = CNodFlgLbl) or (Sender = CNodFlg) then CNodFlgLbl.Color := clBtnShadow;
	if (Sender = CNodFrmLbl) or (Sender = CNodFrm) then CNodFrmLbl.Color := clBtnShadow;
	if (Sender = CNodAdvLbl) or (Sender = CNodAdv) then CNodAdvLbl.Color := clBtnShadow;
	if (Sender = CNodNmeLbl) or (Sender = CNodNme) then CNodNmeLbl.Color := clBtnShadow;
	if (Sender = CEdgLbl) or (Sender = EdgDirLbl) or (Sender = EdgDir) or (Sender = TNodIDLbl) or (Sender = TNodID) or (Sender = ETpLbl) or
	(Sender = ETp) or (Sender = ETpTxt) or (Sender = ManazLbl) or (Sender = Manaz) then CEdgLbl.Color := clMenuBar;
	if (Sender = EdgDirLbl) or (Sender = EdgDir) then EdgDirLbl.Color := clBtnShadow;
	if (Sender = ETpLbl) or (Sender = ETp) or (Sender = ETpTxt) then ETpLbl.Color := clBtnShadow;
	if (Sender = ManazLbl) or (Sender = Manaz) or (Sender = ManazTxt) then ManazLbl.Color := clBtnShadow;
	if (Sender = TNodLbl) or (Sender = TNodIDLbl) or (Sender = TNodID) or (Sender = TNodFlgLbl) or (Sender = TNodFlg) or
	(Sender = TNodFrmLbl) or (Sender = TNodFrm) or (Sender = TNodAdvLbl) or (Sender = TNodAdv) or (Sender = TNodNmeLbl) or
	(Sender = TNodNme) then TNodLbl.Color := clMenuBar;
	if (Sender = TNodIDLbl) or (Sender = TNodID) then TNodIDLbl.Color := clBtnShadow;
	if (Sender = TNodFlgLbl) or (Sender = TNodFlg) then TNodFlgLbl.Color := clBtnShadow;
	if (Sender = TNodFrmLbl) or (Sender = TNodFrm) then TNodFrmLbl.Color := clBtnShadow;
	if (Sender = TNodAdvLbl) or (Sender = TNodAdv) then TNodAdvLbl.Color := clBtnShadow;
	if (Sender = TNodNmeLbl) or (Sender = TNodNme) then TNodNmeLbl.Color := clBtnShadow;
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
//------------------------------------------------------------------------------------------------------------------------------------------------
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
//------------------------------------------------------------------------------------------------------------------------------------------------
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

//================================================================================================================================================
// LEVEL 1. DATA INTERFACE (Create, Destroy, Read, Write, Eidetika Read, Eidetika Write, Eidetika Memorize, Proseychi, Idx)
//================================================================================================================================================
//------------------------------------------------------------------------------------------------------------------------------------------------
//	1.0. ACreate - Create IDS base instance;
//------------------------------------------------------------------------------------------------------------------------------------------------
function TApostolia.ACreate(): Integer;
begin
	Result := ARead;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	1.1. ADestroy - Destruct IDS base instance;
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TApostolia.ADestroy();
begin
	Ap.Free;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	1.2. ARead - Read the Pleximo data from external IDS file to memory
//------------------------------------------------------------------------------------------------------------------------------------------------
//	Results are following:
//	1  IDS file not found, create new initial IDS file instead;
//	2  IDS file is empty, fill it anew with initial IDS data;
//	3  IDS file header is not 'IDS 0.19', backup it & create new IDS file;
//	4  IDS file is corrupted, CRC failed;
//	5  success reading IDS file to Nod array;
//	6  IDS file logical consistency check failed (not yet implemented). This thorough check follows every edge to find whether it's not
//	cyclic or doubled and its ends are matching each other (Asc/Desc and fixed type check).
//	Note: All ints are 64 bit QWords, written in IDS file in reverse order.
//------------------------------------------------------------------------------------------------------------------------------------------------
function TApostolia.ARead(): Integer;
var
	HdrLen: Integer;
	IDS, IDTmp: TMemoryStream;
	UnZ: TDecompressionStream;
	UnZStr: TStringStream;
	Cnt1, Cnt2: QWord;
	const InitStr: RawByteString = #$49 + #$44 + #$53 + #$20 + #$30 + #$2E + #$31 + #$39 + #$04 + #$42 + #$41 + #$26 + #$41 + #$B0 + #$66 +
	#$C8 + #$16 + #$78 + #$9C + #$63 + #$64 + #$40 + #$05 + #$0A + #$5C + #$0C + #$9E + #$79 + #$69 + #$F9 + #$45 + #$B9 + #$89 + #$25 +
	#$99 + #$F9 + #$00 + #$18 + #$31 + #$04 + #$44;
	// Initial Pyramidion019.ids file is just 41 bytes, it's ok to put it here
begin
	//----------------------------------------------------------------------------------------------------------------------------------------
	//	Handling the IDS file with all the necessary checks
	//----------------------------------------------------------------------------------------------------------------------------------------
	Result := 5;										// Initialise Result to success message code
	IDS := TMemoryStream.Create;
	if FileExists(GIDSFile) then IDS.LoadFromFile(GIDSFile)
	else begin										// If no IDS file exist then create new
												// initial IDS file
		Result := 1;									// [01] IDS file not found. Create new one
		IDS.Write(InitStr[1],41);
		IDS.SaveToFile(GIDSFile);
		IDS.Position := 0;
	end;
	If IDS.Size = 0 then begin								// Check file size
		Result := 2;									// [02] IDS file is empty. Replace it
		DeleteFile(GIDSFile);
		IDS.Write(InitStr[1],41);
		IDS.SaveToFile(GIDSFile);
		IDS.Position := 0;
	end;
	Cnt1 := IDS.ReadQWord;
	if Cnt1 <> 4121125918256350281 then begin						// Check for 'IDS 0.19' header
		Result := 3;									// [03] IDS file is not v.0.19. Replace it
		RenameFile(GIDSFile, GIDSFile + '.old');
		IDS.Write(InitStr[1],41);
		IDS.SaveToFile(GIDSFile);
		IDS.Position := 8;
	end;
	HdrLen := IDS.ReadByte;									// Length of the readable IDS file header
	for Cnt1 := 1 to HdrLen do IDS.ReadByte;						// Header string (<=255 ASCII characters)
	Cnt1 := IDS.ReadDWord;									// Check CRC for the file remainder
	IDTmp := TMemoryStream.Create;
	IDTmp.CopyFrom(IDS, IDS.Size - 12 - (HdrLen + 1));					// IDS size minus 'IDS 0.19' header minus
												// recorded CRC minus ASCII header
	Cnt2 := IDSCRC(IDTmp);
	IDTmp.Free;
	If Cnt1 <> Cnt2 then begin
		Result := 4;									// [04] IDS file is corrupted. Replace it
		RenameFile(GIDSFile, GIDSFile + '.old');
		IDS.Write(InitStr[1],41);
		IDS.SaveToFile(GIDSFile);
	end;
	IDS.Position := 8 + (HdrLen + 1);
	GIDSCRC := IDS.ReadDWord;
	//----------------------------------------------------------------------------------------------------------------------------------------
	//	Unpacking data from IDS file
	//----------------------------------------------------------------------------------------------------------------------------------------
	UnZ := TDecompressionStream.Create(IDS);
	UnZStr := TStringStream.Create('');
	UnZStr.CopyFrom(UnZ, 0);
	UnZ.Free;
	UnZStr.Position := 0;
	IDS.Position := 0;
	IDS.CopyFrom(UnZStr, UnZStr.Size);
	UnZStr.Free;
	IDS.Position := 0;
	//----------------------------------------------------------------------------------------------------------------------------------------
	//	Handling the data structure of IDS file:
	//----------------------------------------------------------------------------------------------------------------------------------------
	//	RecCnt (8 bytes)			Records count for Nodes section.
	//	RecCnt times:
	//		Flags (1 byte)			Node Flags field;
	//		EdgCnt (8 bytes)		Edges count;
	//		AEdge (EdgCnt * (17 bytes)	Types & IDs of the edges (absent in initial IDS file);
	//		Adv (8 bytes)			Adventor (absent in initial IDS file);
	//		FrmLen (8 bytes)		Node Formula length;
	//		Frm (FrmLen bytes)		Node Formula;
	//		NmeLen (8 bytes)		Node Name length;
	//		Nme (NmeLen bytes)		Node Name.
	//----------------------------------------------------------------------------------------------------------------------------------------
	//	Get RecordCount in Nodes section
	SetLength(Nod, IDS.ReadQWord);
	GTopID := 0;										// Initialize global Upper Nod ID variable
	GNodDelCnt := 0;									// Initialize global deleted nodes counter
	GEdgeCnt := 0;										// Initialize global edge counter
	GEdgDelCnt := 0;									// Initialize global deleted edges counter
	//----------------------------------------------------------------------------------------------------------------------------------------
	//	Read & convert fields for each Node
	//----------------------------------------------------------------------------------------------------------------------------------------
	for Cnt1 := 0 to High(Nod) do begin
		Nod[Cnt1].ID := IDS.ReadQWord;
		if Nod[Cnt1].ID > GTopID then GTopID := Nod[Cnt1].ID;				// Update global Upper Nod ID variable
		Nod[Cnt1].Flg := IDS.ReadByte;
		if DelFlg(Cnt1) then Inc(GNodDelCnt);						// Update global deleted nodes counter
		if Length(Nod) > 1 then begin							// Check Nod length to define presence of
			SetLength(Nod[Cnt1].Edg, IDS.ReadQWord);				// other nods and sags
			for Cnt2 := 0 to High(Nod[Cnt1].Edg) do begin
				Nod[Cnt1].Edg[Cnt2].ID := IDS.ReadQWord;			// Fill linked node ID field
				Nod[Cnt1].Edg[Cnt2].Flg := IDS.ReadByte;			// Fill edge flags field
				if EDelFlg(Cnt1, Cnt2) then Inc(GEdgDelCnt) else Inc(GEdgeCnt);	// Update global edges and deleted edges counters
				if EOutFlg(Cnt1, Cnt2) and EMnzFlg(Cnt1, Cnt2) then Nod[Cnt1].Edg[Cnt2].ETp := IDS.ReadQWord
					else Nod[Cnt1].Edg[Cnt2].ETp := -1; // Fill edge type field
				if EIncFlg(Cnt1, Cnt2) and EETpFlg(Cnt1, Cnt2) then Nod[Cnt1].Edg[Cnt2].ETp := IDS.ReadQWord
					else Nod[Cnt1].Edg[Cnt2].ETp := -1; // Fill manaz field
			end;
		end;
		if AdvFlg(Cnt1) then Nod[Cnt1].Adv := IDS.ReadQWord else Nod[Cnt1].Adv := 0;	// Check presence of adventor
		if FrmFlg(Cnt1) then for Cnt2 := 1 to IDS.ReadWord do				// Check presence of formula
		Nod[Cnt1].Frm := Nod[Cnt1].Frm + Chr(IDS.ReadByte) else Nod[Cnt1].Frm := '';	// Fill node formula field
		if NmeFlg(Cnt1) then for Cnt2 := 1 to IDS.ReadWord do				// Check presence of name
		Nod[Cnt1].Nme := Nod[Cnt1].Nme + Chr(IDS.ReadByte) else Nod[Cnt1].Nme := '';	// Fill node name field
	end;
	//	Get the current BID (primary base ID is 0)
	GCurrBID := Nod[0].ID and $FFFF000000000000 shr 48;					// The upper 2 node ID bytes are reserved
												// to identify the IDS knowledge base
	GEdgeCnt := GEdgeCnt div 2;								// Each edge has two ends recorded in the
	GEdgDelCnt := GEdgDelCnt div 2;								// Nods array separately
	IDS.Free;
	//	Define here whether allow undo service or not
	GAllowUndo := 0;									// Set initial value to 0 (no undo)
	GUndo := 0;										// Set initial value to 0 (current step)
	//	Refresh global pointers
	GCFlg := Nod[0].Flg;
	GCFrm := Nod[0].Frm;
	GCAdv := Nod[0].Adv;
	GCNme := Nod[0].Nme;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	1.3. AWrite - Write all the Pleximo data to external IDS file
//------------------------------------------------------------------------------------------------------------------------------------------------
function TApostolia.AWrite(): Int64;
var
	IDStr: TStringStream;
	IDS: TMemoryStream;
	IDTmp: TMemoryStream;
	PkZ: TCompressionStream;
	Cnt1, Cnt2: QWord;
begin
	IDStr := TStringStream.Create('');
	//----------------------------------------------------------------------------------------------------------------------------------------
	//	Data structure in IDS file:
	//----------------------------------------------------------------------------------------------------------------------------------------
	//	RecCnt (8 bytes)			Records count for Nodes section.
	//	RecCnt times:
	//		ID (8 bytes)			Node ID;
	//		Flags (1 byte)			Node Flags field;
	//		EdgCnt (8 byte)			Edges count;
	//		AEdge (EdgCnt * (17 bytes)	IDs of the edges;
	//		Adv (8 bytes)			Pointer to Adventor in Eidetika array;
	//		FrmLen (8 bytes)		Formula length;
	//		Frm (FrmLen bytes)		Formula text.
	//		NmeLen (2 bytes)		Node Name length;
	//		Nme (NmeLen bytes)		Node Name text;
	//----------------------------------------------------------------------------------------------------------------------------------------
	//	Write Nodes quantity
	IDStr.WriteQWord(Length(Nod));
	//	Convert & write 1 to 6 fields for each node
	for Cnt1 := 0 to High(Nod) do begin
		IDStr.WriteQWord(Nod[Cnt1].ID);
		IDStr.WriteByte(Nod[Cnt1].Flg);
		if (IncFlg(Cnt1) or OutFlg(Cnt1)) then begin
			IDStr.WriteQWord(Length(Nod[Cnt1].Edg));
			for Cnt2 := 0 to High(Nod[Cnt1].Edg) do begin
				IDStr.WriteQWord(Nod[Cnt1].Edg[Cnt2].ID);
				IDStr.WriteByte(Byte(Nod[Cnt1].Edg[Cnt2].Flg));
				if EOutFlg(Cnt1, Cnt2) and EMnzFlg(Cnt1, Cnt2) then IDStr.WriteQWord(Nod[Cnt1].Edg[Cnt2].ETp);
				if EIncFlg(Cnt1, Cnt2) and EETpFlg(Cnt1, Cnt2) then IDStr.WriteQWord(Nod[Cnt1].Edg[Cnt2].ETp);
			end;
		end;
		if AdvFlg(Cnt1) then IDStr.WriteQWord(Nod[Cnt1].Adv);				// If an adventor exists, write it fo file
		if FrmFlg(Cnt1) then begin							// If a formula exists, write it to file
			IDStr.WriteWord(Length(Nod[Cnt1].Frm));
			for Cnt2 := 1 to Length(Nod[Cnt1].Frm) do IDStr.WriteByte(Byte(Nod[Cnt1].Frm[Cnt2]));
		end;
		if NmeFlg(Cnt1) then begin							// If a name exists, write it to file
			IDStr.WriteWord(Length(Nod[Cnt1].Nme));
			for Cnt2 := 1 to Length(Nod[Cnt1].Nme) do IDStr.WriteByte(Byte(Nod[Cnt1].Nme[Cnt2]));
		end;
	end;
	//----------------------------------------------------------------------------------------------------------------------------------------
	//	Composing IDS file contents (header, CRC32, pack data) and save file
	//----------------------------------------------------------------------------------------------------------------------------------------
	IDStr.Position := 0;
	IDS := TMemoryStream.Create;
	PkZ := TCompressionStream.Create(clDefault, IDS);
	PkZ.CopyFrom(IDStr, IDStr.Size);
	PkZ.Free;
	IDStr.Free;
	IDTmp := TMemoryStream.Create();
	IDTmp.WriteQWord(4121125918256350281);							// Put header 'IDS 0.19'
	IDTmp.WriteByte(4);									// Put ASCII header length
	IDTmp.WriteDWord(1093026114);								// Put 'BA&A' ASCII header
	Result := IDSCRC(IDS);
	GIDSCRC := Result;
	IDTmp.WriteDWord(Result);
	IDS.Position := 0;
	IDTmp.CopyFrom(IDS, IDS.Size);
	IDS.Free;
	if Form.RFileSave.Execute then GIDSFile := Form.RFileSave.Filename;
	if FileExists(GIDSFile + '.bak') then DeleteFile(GIDSFile + '.bak');
	if FileExists(GIDSFile) then RenameFile(GIDSFile, GIDSFile + '.bak');
	IDTmp.SaveToFile(GIDSFile);
	IDTmp.Free;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	1.4. AERead - Read the Events Strip (Eidetika) data from external EID file to memory
//------------------------------------------------------------------------------------------------------------------------------------------------
//	Result value is the following:
//	51	EID file not found, create new initial IDS file instead;
//	52	EID file is empty, fill it anew with initial IDS data;
//	53	EID file header is not 'EID 0.19', backup it & create new IDS file;
//	54	EID file is corrupted, CRC failed;
//	55	success reading EID file to Eid array;
//	Note: All ints are 64 bit QWords, written in EID file in reverse order.
//------------------------------------------------------------------------------------------------------------------------------------------------
function TApostolia.AERead(): Integer;
var
	EIDStream: TMemoryStream;
	EIDFile: String;
	InitStr: TStringStream;
	EIDTmp: TMemoryStream;
	EPkZ: TCompressionStream;
	EUnZ: TDecompressionStream;
	EUnZStr: TStringStream;
	Cnt1, Cnt2, Cnt3: QWord;
begin
	//----------------------------------------------------------------------------------------------------------------------------------------
	//	Handling the EID file with all the necessary checks
	//----------------------------------------------------------------------------------------------------------------------------------------
	//	1. Composing the initial EID record from the current Eid array (2 events are written in by default)
	InitStr := TStringStream.Create('');
	InitStr.WriteQWord(Length(Eid));							// Writing RecCnt field
	for Cnt1 := 0 to High(Eid) do begin
		InitStr.WriteQWord(QWord(Eid[Cnt1].TStmp));					// Writing TStmp field
		InitStr.WriteDWord(Eid[Cnt1].ChCnt);						// Writing ChCnt field
		InitStr.WriteByte(Eid[Cnt1].EvnTp);						// Writing EvnTp field
		InitStr.WriteQWord(Length(Eid[Cnt1].Args));					// Writing ArgLen field
		if Length(Eid[Cnt1].Args) > 0 then						// If an adventor exists, write it down
			for Cnt2 := 1 to Length(Eid[Cnt1].Args) do InitStr.WriteByte(Byte(Eid[Cnt1].Args[Cnt2]));
	end;
	//----------------------------------------------------------------------------------------------------------------------------------------
	//	2. Composing the initial EID file content for the current session
	InitStr.Position := 0;
	EIDStream := TMemoryStream.Create;
	EPkZ := TCompressionStream.Create(clDefault, EIDStream);
	EPkZ.CopyFrom(InitStr, InitStr.Size);
	EPkZ.Free;
	InitStr.Free;
	InitStr := TStringStream.Create('');
	InitStr.WriteQWord(4121125918255368517);					// Put header 'EID 0.19'
	GEIDCRC := IDSCRC(EIDStream);
	InitStr.WriteDWord(GEIDCRC);
	EIDStream.Position := 0;
	InitStr.CopyFrom(EIDStream, EIDStream.Size);					// Initial EID file text is in InitStr stream
	EIDStream.Free;
	InitStr.Position := 0;
	//----------------------------------------------------------------------------------------------------------------------------------------
	//	3. Making 4 checks for the EID file
	Result := 56;									// Initialise Result to [38] Success message
	EIDFile := LeftStr(GIDSFile, Length(GIDSFile) - 3) + 'eid';
	EIDStream := TMemoryStream.Create;
	if FileExists(EIDFile) then EIDStream.LoadFromFile(EIDFile)
	else begin									// If no EID file exist then create new initial EID file
		Result := 52;								// [34] EID file not found. Create new one
		for Cnt1 := 1 to Length(InitStr.DataString) do EIDStream.WriteByte(Byte(InitStr.DataString[Cnt1]));
		EIDStream.SaveToFile(EIDFile);
		EIDStream.Position := 0;
	end;
	If EIDStream.Size = 0 then begin						// Check file size
		Result := 53;								// [35] EID file is empty. Replace it
		DeleteFile(EIDFile);
		for Cnt1 := 1 to Length(InitStr.DataString) do EIDStream.WriteByte(Byte(InitStr.DataString[Cnt1]));
		EIDStream.SaveToFile(EIDFile);
		EIDStream.Position := 0;
	end;
	Cnt1 := EIDStream.ReadQWord;
	if Cnt1 <> 4121125918255368517 then begin					// Check for 'EID19' header
		Result := 54;								// [36] EID file is not v.0.19. Replace it
		RenameFile(EIDFile, EIDFile + '.old');
		for Cnt1 := 1 to Length(InitStr.DataString) do EIDStream.WriteByte(Byte(InitStr.DataString[Cnt1]));
		EIDStream.SaveToFile(EIDFile);
		EIDStream.Position := 8;
	end;
	Cnt1 := EIDStream.ReadDWord;							// Check CRC for the file remainder
	EIDTmp := TMemoryStream.Create;
	EIDTmp.CopyFrom(EIDStream, EIDStream.Size - 12);
	Cnt2 := IDSCRC(EIDTmp);
	EIDTmp.Free;
	If Cnt1 <> Cnt2 then begin
		Result := 55;								// [37] IDS file is corrupted. Replace it
		RenameFile(EIDFile, EIDFile + '.old');
		for Cnt1 := 1 to Length(InitStr.DataString) do EIDStream.WriteByte(Byte(InitStr.DataString[Cnt1]));
		EIDStream.SaveToFile(EIDFile);
	end;
	EIDStream.Position := 8;
	GEIDCRC := EIDStream.ReadDWord;
	//----------------------------------------------------------------------------------------------------------------------------------------
	//	4. Unpacking data from EID file
	EUnZ := TDecompressionStream.Create(EIDStream);
	EUnZStr := TStringStream.Create('');
	EUnZStr.CopyFrom(EUnZ, 0);
	EUnZ.Free;
	EUnZStr.Position := 0;
	EIDStream.Position := 0;
	EIDStream.CopyFrom(EUnZStr, EUnZStr.Size);
	EUnZStr.Free;
	EIDStream.Position := 0;
	//----------------------------------------------------------------------------------------------------------------------------------------
	//	Handling the data structure of EID file:
	//----------------------------------------------------------------------------------------------------------------------------------------
	//	RecCnt:	Int64			Records count
	//	RecCnt times:
	//		TStmp: TTimeStamp;	Time stamp (consists of Time:Int64 + Date:Int64 and is timezone independent unlike TDateTime);
	//		ChCnt: int;		Counter of events within milliseconds (because there is no reliable nanoseconds counter in
	//					multicore CPUs);
	//		EvnTp: byte;		Event type (few types are anticipated, so no more than a byte to store type #);
	//		ArgLen: Int64		Arguments string length
	//		Args: RawByteString;	Arguments of the event (specific to each event type).
	//----------------------------------------------------------------------------------------------------------------------------------------
	//	5. Get RecordCount in Eidon array
	SetLength(Eid, EIDStream.ReadQWord);
	//----------------------------------------------------------------------------------------------------------------------------------------
	//	6. Read & convert 4 to 5 fields for each Eidon record
	for Cnt1 := 0 to High(Eid) do begin
		Eid[Cnt1].TStmp := EIDStream.ReadQWord;
		Eid[Cnt1].ChCnt := EIDStream.ReadDWord;
		Eid[Cnt1].EvnTp := EIDStream.ReadByte;
		Cnt2 := EIDStream.ReadQWord;
		if Cnt2 > 0 then for Cnt3 := 1 to Cnt2 do Eid[Cnt1].Args := Eid[Cnt1].Args + Chr(EIDStream.ReadByte);
	end;
	EIDStream.Free;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	1.5. AEWrite - Update the Events Strip EID file
//------------------------------------------------------------------------------------------------------------------------------------------------
function TApostolia.AEWrite(): Int64;
var
	EIDStr: TStringStream;
	EIDStream: TMemoryStream;
	EIDFile: String;
	EIDTmp: TMemoryStream;
	EPkZ: TCompressionStream;
	Cnt1, Cnt2: QWord;
begin
	EIDStr := TStringStream.Create('');
	//----------------------------------------------------------------------------------------------------------------------------------------
	//	Data structure in EID file:
	//----------------------------------------------------------------------------------------------------------------------------------------
	//	RecCnt:	Int64			Records count
	//	RecCnt times:
	//		TStmp: TTimeStamp;	Time stamp (consists of Time:Int64 + Date:Int64 and is timezone independent unlike TDateTime);
	//		ChCnt: int;			Counter of events within milliseconds (because there is no reliable nanoseconds counter in
	//					multicore CPUs);
	//		EvnTp: byte;		Event type (few types are anticipated, so no more than a byte to store type #);
	//		ArgLen: Int64		Arguments string length
	//		Args: RawByteString;	Arguments of the event (specific to each event type). <--- try to make fixed length field herer!
	//----------------------------------------------------------------------------------------------------------------------------------------
	//	1. Write Eid records quantity
	EIDStr.WriteQWord(Length(Eid));
	//----------------------------------------------------------------------------------------------------------------------------------------
	//	2. Convert & write 4 to 5 fields for each Eidon record
	for Cnt1 := 0 to High(Eid) do begin
		EIDStr.WriteQWord(QWord(Eid[Cnt1].TStmp));
		EIDStr.WriteDWord(Eid[Cnt1].ChCnt);
		EIDStr.WriteByte(Eid[Cnt1].EvnTp);
		EIDStr.WriteQWord(Length(Eid[Cnt1].Args));
		if Length(Eid[Cnt1].Args) > 0 then							// If an adventor exists, write it down
			for Cnt2 := 1 to Length(Eid[Cnt1].Args) do EIDStr.WriteByte(Byte(Eid[Cnt1].Args[Cnt2]));
	end;
	//----------------------------------------------------------------------------------------------------------------------------------------
	//	Composing EID file contents (header, CRC32, pack data) and save file
	//----------------------------------------------------------------------------------------------------------------------------------------
	EIDStr.Position := 0;
	EIDStream := TMemoryStream.Create;
	EPkZ := TCompressionStream.Create(clDefault, EIDStream);
	EPkZ.CopyFrom(EIDStr, EIDStr.Size);
	EPkZ.Free;
	EIDStr.Free;
	EIDTmp := TMemoryStream.Create();
	EIDTmp.WriteQWord(4121125918255368517);								// Put header 'EID 0.19'
	GEIDCRC := IDSCRC(EIDStream);
	EIDTmp.WriteDWord(GEIDCRC);
	Result := GEIDCRC;
	EIDStream.Position := 0;
	EIDTmp.CopyFrom(EIDStream, EIDStream.Size);
	EIDStream.Free;
	EIDFile := LeftStr(GIDSFile, Length(GIDSFile) - 3) + 'eid';
	if FileExists(EIDFile + '.bak') then DeleteFile(EIDFile + '.bak');
	if FileExists(EIDFile) then RenameFile(EIDFile, EIDFile + '.bak');
	EIDTmp.SaveToFile(EIDFile);
	EIDTmp.Free;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	1.6. AEMem - Add an event to the Event Strip (Eidetika)
//------------------------------------------------------------------------------------------------------------------------------------------------
//	Data structure in Eidon array:
//------------------------------------------------------------------------------------------------------------------------------------------------
//	TStmp:	TTimeStamp;		Time stamp (consists of Time:Int64 + Date:Int64 and is timezone independent unlike TDateTime);
//	ChCnt:	int;			Counter of events within milliseconds (because there is no reliable nanoseconds counter in
//					multicore CPUs);
//	EvnTp:	byte;			Event type (few types are anticipated, so no more than a byte to store type #);
//	ArgLen:	Int64			Arguments string length
//	Args:	RawByteString;		Arguments of the event (specific to each event type).
//------------------------------------------------------------------------------------------------------------------------------------------------
procedure TApostolia.AEMem(EventNo: Integer);
var	ConsoleLine: String;
begin
	ConsoleLine := Form.SCon.Lines[Form.SCon.Lines.Count - 2];
	ConsoleLine := RightStr(ConsoleLine, Length(ConsoleLine) - 38);
	SetLength(Eid, Length(Eid) + 1);
	Eid[High(Eid)].TStmp := TimeStampToMSecs(DateTimeToTimeStamp(Now));
	if ((Length(Eid) > 1) and (Eid[High(Eid)].TStmp = Eid[High(Eid) - 1].TStmp))
		then Eid[High(Eid)].ChCnt := Eid[High(Eid) - 1].ChCnt + 1 else Eid[High(Eid)].ChCnt := 0;
	Eid[High(Eid)].EvnTp := byte(EventNo);
	case EventNo of
		-1: Eid[High(Eid)].Args := ConsoleLine;
		5: Eid[High(Eid)].Args := BinStr(Length(Nod), 64) + GIDSFile;
		9: Eid[High(Eid)].Args := BinStr(GCID, 64) + GCNme;
		10: Eid[High(Eid)].Args := BinStr(GCAdv, 64);
		11: Eid[High(Eid)].Args := BinStr(GCID, 64) + BinStr(GCAdv, 64);// Current node adventor edited
		17: Eid[High(Eid)].Args := BinStr(GTID, 64);// New target node set
		18: Eid[High(Eid)].Args := '';
		21, 22: Eid[High(Eid)].Args := BinStr(GCID, 64) + BinStr(GTID, 64) + BinStr(GTpID, 64);// Asc/Dsc edge type change
		23: Eid[High(Eid)].Args := BinStr(GCID, 64) + BinStr(GCAdv, 64) + BinStr(GTID, 64) + GTNme;
		24: Eid[High(Eid)].Args := BinStr(GTAdv, 64);
		25: Eid[High(Eid)].Args := BinStr(GTID, 64) + BinStr(GTAdv, 64);// Trget node adventor edited
		26: Eid[High(Eid)].Args := '';	// No target node specified
		27: Eid[High(Eid)].Args := BinStr(GTID, 64);
		28: Eid[High(Eid)].Args := BinStr(GTID, 64);
		32: Eid[High(Eid)].Args := BinStr(GTID, 64);
		33, 34: Eid[High(Eid)].Args := BinStr(GCID, 64) + BinStr(GTID, 64);
		37: Eid[High(Eid)].Args := BinStr(GTID, 64);
		38: Eid[High(Eid)].Args := BinStr(GCID, 64) + BinStr(Length(GCNme), 32) + GCNme + BinStr(GTID, 64) + GTNme;
		42: Eid[High(Eid)].Args := BinStr(Length(Nod), 64) + BinStr(GEdgeCnt, 64);
		43: Eid[High(Eid)].Args := '';
		44: Eid[High(Eid)].Args := BinStr(GIDSCRC, 32) + BinStr(Length (Nod) - GNodDelCnt, 64) + BinStr(GEdgeCnt, 64) + GIDSFile;
		45: Eid[High(Eid)].Args := '';
		46: Eid[High(Eid)].Args := '[2E] Undoing to the Step ' + BinStr(GUndo, 64);
		47: Eid[High(Eid)].Args := '[2F] Redoing to the Step ' + BinStr(GUndo, 64);
		48, 49, 50: Eid[High(Eid)].Args := '';
		51: Eid[High(Eid)].Args := Consoleline;
		52, 53, 54, 55: Eid[High(Eid)].Args := LeftStr(GIDSFile, Length(GIDSFile) - 3) + 'eid';
		56, 57: Eid[High(Eid)].Args := BinStr(Length(Eid), 64) + LeftStr(GIDSFile, Length(GIDSFile) - 3) + 'eid';
		59: Eid[High(Eid)].Args := BinStr(GCFlg, 8);	// Change curret node flags
		61: Eid[High(Eid)].Args := BinStr(GTFlg, 8);	// Change target node flags
		64: Eid[High(Eid)].Args := BinStr(GCID, 64) + GCFrm;
		66: Eid[High(Eid)].Args := BinStr(GCID, 64) + GCNme;
		68: Eid[High(Eid)].Args := BinStr(GCEdg, 64) + BinStr(GMnID, 64);
		70: Eid[High(Eid)].Args := BinStr(GTID, 64) + GTFrm;
		72: Eid[High(Eid)].Args := BinStr(GTID, 64) + GTNme;
	end;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	1.7. Proseychi - Process an interface call
//------------------------------------------------------------------------------------------------------------------------------------------------
function TApostolia.Proseychi(): Integer;
var
	ConsoleLine: String;
	NodID, CEdg, BackCurrIdx, Back1TargIdx, Back2TargIdx: Int64;
	CurrMsg: Integer;
	LogFile: TFileStream;
begin
	Result := 51;
	ConsoleLine := Form.SCon.Lines[Form.SCon.Lines.Count - 2];
	if RightStr(LeftStr(ConsoleLine, 33), 1) = '[' then CurrMsg := StrToInt('$' + RightStr(LeftStr(ConsoleLine,35),2)) else CurrMsg := -1;
	AEMem(CurrMsg);
	case CurrMsg of
		-1: begin
			ConsoleLine := RightStr(ConsoleLine, Length(ConsoleLine) - 38);
			Form.ALog(51);									// [33] Processing console query
			Result := Ag.AParse(Consoleline);						// Sending Consoleline to parser
			AEMem(Result);
		end;
		00: begin
			Result := ACreate;								// [00] Read Nodes from IDS file
			if Result < 5 then Form.ALog(Result);
			Form.ALog(5);
			AEMem(5);
			Result := AERead;								// Reading Eidetika from EID file
			if Result < 56 then Form.ALog(Result);
			AEMem(56);
		end;
		06: begin										// [06] Current node pointer update
			if Idx(GCID) = -1 then begin							// Check for [07] ID not found
				Form.ALog(07);
				GCID := 0;
			end
			else if DelFlg(GCIdx) then begin						// Check [08] Del flag
				Form.ALog(08);
				GCID := 0;
			end;
			Result := Ag.AChCurrNode();							// Set new current node
			AEMem(Result);
		end;
		10: begin
			Result := Ag.AEdit(10);								// [0A] Current node adventor edit
			AEMem(Result);
		end;
		12: begin										// [0C] Target node pointer update
			if GTarg then if Idx(GTID) = -1 then begin					// Check for [0D] ID not found
				Form.ALog(13);
				GTID := -1;
				GTarg := False;
			end
			else if GTarg then if DelFlg(GTIdx) then begin					// Check [0E] Del flag
				GTNme := Nod[Idx(GTID)].Nme;						// Name is just for ALog procedure
				Form.ALog(14);
				GTID := -1;
				GTarg := False;
			end
			else if GTID = GCID then begin							// Check for [0F] cyclic edge
				Form.ALog(15);
				GTID := -1;
				GTarg := False;
			end;
			Result := Ag.AChTargNode();							// Set new target node
			AEMem(Result);
		end;
		18: begin										// [12] Edge type change
			if Idx(GTpID) = -1 then begin							// Check for [13] edge type ID not found
				Result := 19;
				Exit;
			end;
			Result := Ag.AChEType(1);
			AEMem(Result);
		end;
		24: begin
			Result := Ag.AEdit(24);								// [17] Target node adventor edit
			AEMem(Result);
		end;
		27, 28: begin										// Add [1B] inc. or [1C] out. edge
			if (((CurrMsg = 27) and (GCID = 0)) or ((CurrMsg = 28) and (GTarg and (GTID = 0))))	// Check for [1D] no asc.edges
				 then Result := 29								// allowed for Node 0
			else begin
				if GTarg then if GTpID > -1 then if (Idx(GTpID) = -1) then begin	// Check for [13] edge type ID not found,
					GTpID := -1;							// set to None
					Form.ALog(19);
				end;
				if GTarg and DelFlg(GTIdx) then begin
					Form.ALog(31);							// Check [1F] target node is deleted
					Exit
				end;
				if GTarg and (GTID = GCID) then begin
					Form.ALog(15);							// Check for [0F] cyclic edge error
					Exit;
				end;
				if GTarg then Result := Ag.AAdd(CurrMsg + 2)			    // Edge to the existing target node
				else Result := Ag.AAdd(CurrMsg);					// Edge to the new target node
			end;
			AEMem(Result);
		end;
		35: begin										// [23] Deleting node edge by edge
			if GCID = 0 then begin
				Result := 36;								// [24] Node 0 cannot be deleted
				Exit;
			end
			else for CEdg := High(Nod[GCIdx].Edg) downto 0 do if not EDelFlg(GCIdx, CEdg) then begin
				GCEdg := CEdg;
				GTarg := True;
				GTID := Nod[GCIdx].Edg[GCEdg].ID;
				Ag.AChTargNode();
				BackCurrIdx := GCIdx;
				Back1TargIdx := GTIdx;
				Result := Ag.ADelete;
				if Result = 37 then begin
					Back2TargIdx := GTIdx;
					GTIdx := BackCurrIdx;
					Form.ALog(37);							// [25] Node deleted (current)
					AEMem(37);
					GTIdx := Back2TargIdx;
					Result := 09;
				end;
				if Result = 38 then begin
					Back2TargIdx := GTIdx;
					GTIdx := Back1TargIdx;
					Form.ALog(37);							// [26] Node deleted (target)
					AEMem(37);
					GTIdx := Back2TargIdx;
					Result := 09;
				end;
			end;
			AEMem(Result);
		end;
		38: begin
			BackCurrIdx := GCIdx;
			Back1TargIdx := GTIdx;
			if not GTarg then Result := 39 else Result := Ag.Adelete;			// [26] Deleting edge. Check for [27]
													// There is no current Edge to delete
			if Result = 37 then begin
				Back2TargIdx := GTIdx;
				GTIdx := BackCurrIdx;
				Form.ALog(37);								// [25] Node deleted (current)
				AEMem(37);
				GTIdx := Back2TargIdx;
				Result := 09;
			end;
			if Result = 38 then begin
				Back2TargIdx := GTIdx;
				GTIdx := Back1TargIdx;
				Form.ALog(37);								// [25] Node deleted (target)
				AEMem(37);
				GTIdx := Back2TargIdx;
				Result := 09;
			end;
			AEMem(Result);
		end;
		41: begin
			Result := Ag.ACommit;								// [29] Commiting base
			AEMem(Result);
		end;
		43: begin										// [2B] Save base
			Ap.AWrite;									// Pour Nod array to IDS file
			Form.ALog(44);									// [2C] Database saved
			AEMem(44);
			Ap.AEWrite;									// Pour Eidetika to EID file
			Form.ALog(57);									// [39] Events Strip saved
			AEMem(57);
			if not(FileExists('Pyramidion 019.log')) then FileCreate('Pyramidion 019.log');	// If no log file exist then created new
													// empty log file
			LogFile := TFileStream.Create('Pyramidion 019.log', fmOpenReadWrite);
			Form.ALog(45);									// [2D] Session log saved to file
													// "Pyramidion 019.log", session closed
			AEMem(45);
			if Form.SCon.Lines.Count > 1 then Form.SCon.Lines.Delete(Form.SCon.Lines.Count - 1);
			with LogFile do begin								// Note: on event [2D] session log is
													// saved by the Form itself,
				Seek(0, SoEnd);								// Apostolia just frees Nod instance
				for NodID := 1 to Length(Form.SCon.Caption) do WriteByte(Byte(Form.SCon.Caption[NodID]));
				Destroy;
			end;
			ADestroy;
		end;
		48: begin
			Result := 48;									// [30] Session close without saving log
			AEMem(Result);									// and database
		end;
		49: begin
			Result := 50;									// [31] 'Monitor completed' event
			AEMem(Result);
			Form.AMonitor;
		end;
		58: begin
			Nod[GCIdx].Flg := GCFlg;
			Result := 59;									// [3B] 'Current node flags edited' event
			AEMem(Result);
		end;
		60: begin
			if GTarg then begin								// [3D] 'Target node flags edited' event
				Nod[GTIdx].Flg := GTFlg;
				Result := 61;
			end
			else Result := 26;
			AEMem(Result);
		end;
		62: begin										// [3E] 'Add new unlinked node' event
			NodID := GCID;
			GCID := 0;
			Result := Ag.AAdd(27);								// [20] 'New [unlinked] node created'
			GCID := NodID;
			AEMem(Result);
		end;
		63: begin
			Result := Ag.AEdit(63);								// [3F] Current node formula edit
			AEMem(Result);
		end;
		65: begin
			Result := Ag.AEdit(65);								// [41] Current node name edit
			AEMem(Result);
		end;
		67: begin
			if Idx(GMnID) = -1 then begin							// Check for [13] edge manaz ID not found
				Form.ALog(19);
				AEMem(19);
				GMnID := -1;
			end;
			Result := Ag.AChEType(0);							// [43] Current edge checkstring edit
			AEMem(Result);
		end;
		69: begin
			Result := Ag.AEdit(69);								// [45] Target node formula edit
			AEMem(Result);
		end;
		71: begin
			Result := Ag.AEdit(71);								// [47] Target node name edit
			AEMem(Result);
		end;
	end;
	GlobalsRenew();
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	1.8. Idx - Return index of a node by its ID (binary search in sorted list)
//------------------------------------------------------------------------------------------------------------------------------------------------
function TApostolia.Idx(InputID: Int64): Int64;
var
	Cnt, Lo, Mid, Hi: Int64;
begin
	Result := -1;
	Lo := 0;
	Hi := High(Nod);
	Mid := Hi div 2;
	repeat
		if Nod[Mid].ID >= InputID then Hi := Mid else Lo := Mid;
		Mid := Lo + ((Hi - Lo) div 2);
	until Hi - Lo <= 32;								// 32 is deliberate value, need to recheck it later
	Cnt := Lo - 1;
	if InputID > -1 then repeat Inc(Cnt) until ((Nod[Cnt].ID = InputID) or (Cnt > Hi)) else Result := -1;
	if Cnt > Hi then Result := -1 else Result := Cnt;
end;

//================================================================================================================================================
// LEVEL 2. BASIC PYRAMIDION MECHANICS (Add, Delete, Edit, Change Node, Change Edge Type, Commit, Parse)
//================================================================================================================================================
//------------------------------------------------------------------------------------------------------------------------------------------------
//	2.0. AAdd - Add a node or an edge
//		Input:	Mode:	byte			27 = add ascending edge and new node;
//							28 = add descending edge and new node;
//							29 = add ascending edge to existing node;
//							30 = add descending edge to existing node;
//
//	Note 1:	There is no upper limit check for Nod index here because obviously 64-bit is quite enough for this alpha version. Even the final
//		release version or the Pyramidion DBMS will hardly beat the upper limit of 2^64 Nodes.
//	Note 2: Quick way to handle orphans: link each newly added node to Node 0 (undeletable), so no orphans will ever appear.
//		This also corresponds to the Pyramidion ideology and Circus Lucius concept.
//		Sure this edge to Node 0 is the last one to delete, only when the node itself is to be deleted.
//------------------------------------------------------------------------------------------------------------------------------------------------
function TAngeliophor.AAdd(Mode: Byte): Int64;
var
	Cnt: QWord;
	Swap: Boolean;
	CED: AEdge;
begin
	//----------------------------------------------------------------------------------------------------------------------------------------
	//	Adding new node with descending or ascending edge to the current node
	//----------------------------------------------------------------------------------------------------------------------------------------
	if Mode < 29 then begin
		//--------------------------------------------------------------------------------------------------------------------------------
		// Create new node
		GTIdx := Length(Nod);									// The new node will be the new Targ.Node
		SetLength(Nod, GTIdx + 1);								// Now GTIDx = High(Nod)
		Inc(GTopID);
		Nod[GTIdx].ID := GTopID;
		//--------------------------------------------------------------------------------------------------------------------------------
		// Prevent orphan appearing - any new node must have the incoming edge from the Node 0
		GEdge := True;
		SetLength(Nod[0].Edg, Length(Nod[0].Edg) + 1);						// Add outgoing edge from the Node 0
		Nod[0].Edg[High(Nod[0].Edg)].ID := GTopID;
		SetOutFlg(0);
		GCFlg := Nod[0].Flg;
		SetEOutFlg(0, High(Nod[0].Edg));
		SetLength(Nod[GTIdx].Edg, 1);								// Add incoming edge to the new node
		SetIncFlg(GTIdx);									// Note: Nod[GTIdx].Edg[0].ID, .Flg and
		SetEIncFlg(GTIdx, 0);									// .ETp are set to 0 by default
		Inc(GEdgeCnt);										// Update global pointers
		GTarg := True;
		GTID := Nod[GTIdx].ID;
		GTFlg := Nod[GTIdx].Flg;
		GTAdv := -1;
		GTFrm := '';
		GTNme := '';
		//--------------------------------------------------------------------------------------------------------------------------------
		// Setting the edge between newly added node and the current node
		if GCID > 0 then begin									// Check to avoid duplicate link to Node 0
			GCEdg := Length(Nod[GCIdx].Edg);
			SetLength(Nod[GCIdx].Edg, GCEdg + 1);
			Nod[GCIdx].Edg[GCEdg].ID := GTopID;
			SetLength(Nod[GTIdx].Edg, 2);
			GTEdg := 1;
			Nod[GTIdx].Edg[GTEdg].ID := Nod[GCIdx].ID;
			Inc(GEdgeCnt);									// Update global edge counter
			//------------------------------------------------------------------------------------------------------------------------
			// Setting flags
			if Mode = 27 then begin								// Incoming edge from the new node
				SetOutFlg(GTIdx);
				SetEOutFlg(GTIdx, GTEdg);
				SetIncFlg(GCIdx);
				SetEIncFlg(GCIdx, GCEdg);
			end
			else begin									// Outgoinging edge to the new node
				SetIncFlg(GTIdx);
				SetEIncFlg(GTIdx, GTEdg);
				SetOutFlg(GCIdx);
				SetEOutFlg(GCIdx, GCEdg);
			end;
		end;
		Result := 32;										// [20] New node added

	end
	//----------------------------------------------------------------------------------------------------------------------------------------
	//	Adding ascending or descending edge between the current node and existing target node
	//----------------------------------------------------------------------------------------------------------------------------------------
	else begin
		for Cnt := 0 to High(Nod[GCIdx].Edg) do
			if Nod[GCIdx].Edg[Cnt].ID = Nod[GTIdx].ID then				  // Check for [1E] duplicate edge error
			if not EDelFlg(GCIdx, Cnt) then begin
				Result := 30;
				Exit;
			end;
		GEdge := True;
		GCEdg := Length(Nod[GCIdx].Edg);
		SetLength(Nod[GCIdx].Edg, GCEdg + 1);
		Nod[GCIdx].Edg[GCEdg].ID := Nod[GTIdx].ID;
		Nod[GCIdx].Edg[GCEdg].ETp := -1;
		GTEdg := Length(Nod[GTIdx].Edg);
		SetLength(Nod[GTIdx].Edg, GTEdg + 1);
		Nod[GTIdx].Edg[GTEdg].ID := Nod[GCIdx].ID;
		Nod[GTIdx].Edg[GTEdg].ETp := -1;							// New edge type is Undefined/None
		Inc(GEdgeCnt);										// Update global edge counter
		//--------------------------------------------------------------------------------------------------------------------------------
		// Setting flags
		if Mode = 30 then begin								// Incoming edge to existing node
			SetIncFlg(GTIdx);
			SetEIncFlg(GTIdx, GTEdg);
			SetOutFlg(GCIdx);
			SetEOutFlg(GCIdx, GCEdg);
			Result := 34;								// [21] incoming edge added
		end
		else begin									// Outgoing edge from existing node
			SetOutFlg(GTIdx);
			SetEOutFlg(GTIdx, GTEdg);
			SetIncFlg(GCIdx);
			SetEIncFlg(GCIdx, GCEdg);
			Result := 33;								// [22] outgoing edge added
		end;
		GCFlg := Nod[GCIdx].Flg;
		GTFlg := Nod[GTIdx].Flg;
	end;
	//----------------------------------------------------------------------------------------------------------------------------------------
	//	Sorting newly added edges for the current and target node
	//----------------------------------------------------------------------------------------------------------------------------------------
	//	Note: Simpliest bubble sort is used so far. For big numbers the quick sort algorithm will be implemented.
	//	The size of 'big' is to be defined experimentally.
	//----------------------------------------------------------------------------------------------------------------------------------------
	if Length(Nod[GCIdx].Edg) > 1 then begin
		repeat
		Swap := False;
		for Cnt := 0 to High(Nod[GCIdx].Edg) - 1 do
			if Nod[GCIdx].Edg[Cnt].ID > Nod[GCIdx].Edg[Cnt + 1].ID then begin
				CED := Nod[GCIdx].Edg[Cnt];
				Nod[GCIdx].Edg[Cnt] := Nod[GCIdx].Edg[Cnt + 1];
				Nod[GCIdx].Edg[Cnt + 1] := CED;
				Swap := True;
			end;
		until not Swap;
		if Length(Nod[GTIdx].Edg) > 1 then begin
			repeat
			Swap := False;
			for Cnt := 0 to High(Nod[GTIdx].Edg) - 1 do
				if Nod[GTIdx].Edg[Cnt].ID > Nod[GTIdx].Edg[Cnt + 1].ID then begin
					CED := Nod[GTIdx].Edg[Cnt];
					Nod[GTIdx].Edg[Cnt] := Nod[GTIdx].Edg[Cnt + 1];
					Nod[GTIdx].Edg[Cnt + 1] := CED;
					Swap := True;
				end;
			until not Swap;
		end;
	end;
	//----------------------------------------------------------------------------------------------------------------------------------------
	//	Resetting global edge pointers
	if Length(Nod[GCIdx].Edg) > 1 then begin
		GCEdg := 0;
		repeat Inc(GCEdg) until ((Nod[GCIdx].Edg[GCEdg].ID = Nod[GTIdx].ID) and (not EDelFlg(GCIdx, GCEdg)));
	end;
	if Length(Nod[GTIdx].Edg) > 1 then begin
		GTEdg := 0;
		repeat Inc(GTEdg) until ((Nod[GTIdx].Edg[GTEdg].ID = Nod[GCIdx].ID) and (not EDelFlg(GTIdx, GTEdg)));
	end;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	2.1. ADelete - Delete an edge. With the last edge also delete the node.
//------------------------------------------------------------------------------------------------------------------------------------------------
function TAngeliophor.ADelete(): Integer;
var
	IsEdge, Cnt, IncCnt, OutCnt: QWord;
	CurrNodDel, TargNodDel: Boolean;
begin
	if GTarg then begin
		Result := 9;										// Initialise Result to [09] Success
		//--------------------------------------------------------------------------------------------------------------------------------
		// Check whether current and target nodes have just one undeleted edge left
		IsEdge := 0;
		CurrNodDel := False;						// Check for [29] Edge to Node 0 must be the last edge to delete
		for Cnt:= High(Nod[GCIdx].Edg) downto 0 do if not EDelFlg(GCIdx, Cnt) then Inc(IsEdge);
		if GCEdg = 0 then if IsEdge = 1 then CurrNodDel := True else begin
			Result := 40;
			Exit;
		end;
		IsEdge := 0;
		TargNodDel := False;
		if GTarg then for Cnt:= High(Nod[GTIdx].Edg) downto 0 do if not EDelFlg(GTIdx, Cnt) then Inc(IsEdge);
		if GTEdg = 0 then if GTID > 0 then if IsEdge = 1 then TargNodDel := True else begin
			Result := 40;
			Exit;
		end;
		//--------------------------------------------------------------------------------------------------------------------------------
		// Deleting edge and/or nodes
		if not EDelFlg(GCIdx, GCEdg) then begin
			//------------------------------------------------------------------------------------------------------------------------
			// Deleting edge at the current node side (and the node as well if there no more edges)
			if CurrNodDel then begin
				if GCIdx > 0 then SetDelFlg(GCIdx);
				if GCIdx > 0 then Inc(GNodDelCnt);
				Result := 37;								// [25] Node deleted (current)
			end;
			SetEDelFlg(GCIdx, GCEdg);							// Raise Edg.Del flag
			//------------------------------------------------------------------------------------------------------------------------
			// Deleting edge at the target node side (and the node as well if there no more edges)
			if TargNodDel then begin
				if GTIdx > 0 then SetDelFlg(GTIdx);
				if GTIdx > 0 then Inc(GNodDelCnt);
				Result := 37;								// [25] Node deleted (target)
			end;
			if GTarg then SetEDelFlg(GTIdx, GTEdg);
			//------------------------------------------------------------------------------------------------------------------------
			// Lower Inc or Out flags or both:
			//------------------------------------------------------------------------------------------------------------------------
			// Adjust flags for the current node
			if CurrNodDel then begin
				ClrIncFlg(GCIdx);
				ClrOutFlg(GCIdx);
				SetLength(Nod[GCIdx].Edg, 0);
			end
			else begin
				IncCnt := 0;
				OutCnt := 0;
				for Cnt := 0 to High(Nod[GCIdx].Edg) do if not EDelFlg(GCIdx, Cnt) then
					if EOutFlg(GCIdx, Cnt) then Inc(OutCnt) else Inc(IncCnt);
				if IncCnt = 0 then ClrIncFlg(GCIdx);
				if OutCnt = 0 then ClrOutFlg(GCIdx);
			end;
			//------------------------------------------------------------------------------------------------------------------------
			// Adjust flags for the target node
			if GTarg then if TargNodDel then begin
				ClrIncFlg(GTIdx);
				ClrOutFlg(GTIdx);
				SetLength(Nod[GTIdx].Edg, 0);
			end
			else begin
				IncCnt := 0;
				OutCnt := 0;
				for Cnt := 0 to High(Nod[GTIdx].Edg) do if not EDelFlg(GTIdx, Cnt) then
					if EOutFlg(GTIdx, Cnt) then Inc(OutCnt) else Inc(IncCnt);
				if IncCnt = 0 then ClrIncFlg(GTIdx);
				if OutCnt = 0 then ClrOutFlg(GTIdx);
			end;
			//----------------------------------------------------------------------------------------------------------------------------
			// Adjust globals
			Dec(GEdgeCnt);								// Decrement global edge counter
			Inc(GEdgDelCnt);							// Increment global deleted edge counter
			if CurrNodDel then GCID := 0;						// Set global current node pointer to Node 0
			GCIdx := Ap.Idx(GCID);
			GCFlg := Nod[GCIdx].Flg;
			if FrmFlg(GCIdx) then GCFrm := Nod[GCIdx].Frm else GCFrm := '';
			if AdvFlg(GCIdx) then GCAdv := Nod[GCIdx].Adv else GCAdv := -1;
			if NmeFlg(GCIdx) then GCNme := Nod[GCIdx].Nme else GCNme := '';
		end;
		GTarg := False;
		GTID := -1;
		GTIdx := 0;
		GTID := -1;
		GTFlg := 0;
		GTFrm := '';
		GTAdv := -1;
		GTNme := '';
		GEdge := False;
		GCEdg := 0;
		GMnIdx := 0;
		GMnID := -1;
		GMnFrm := '';
		GTEdg := 0;
		GTpIdx := 0;
		GTpID := -1;
		GTpNme := '';
	end
	else Result := 39;									// [28] No current Edge to delete
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	2.2. AEdit - Edit the adventor, formula or name of a node (Mode corresponds to respective system messages)
//------------------------------------------------------------------------------------------------------------------------------------------------
function TAngeliophor.AEdit(Mode: Byte): Integer;
begin
	case Mode of
	10: begin										// [0A] Current node adventor edit
		if GCAdv > -1 then SetAdvFlg(GCIdx) else ClrAdvFlg(GCIdx);
		Nod[GCIdx].Adv := GCAdv;
		Result := 11;									// [0B] New adventor for the Current Node
	end;
	24: if GTarg then begin									// [17] Target node adventor edit
		if GTAdv > -1 then SetAdvFlg(GTIdx) else ClrAdvFlg(GTIdx);
		Nod[GTIdx].Adv := GTAdv;
		Result := 25;									// [19] New adventor for the Target Node
	end
	else Result := 26;									// [1A] 'No Target Node defined' event
	63: begin										// [3F] Current node formula edit
		if GCFrm <> '' then SetFrmFlg(GCIdx) else ClrFrmFlg(GCIdx);
		Nod[GCIdx].Frm := GCFrm;
		Result := 64;									// [40] New formula for the Current Node
	end;
	65: begin										// [41] Current node name edit
		if GCNme <> '' then SetNmeFlg(GCIdx) else ClrNmeFlg(GCIdx);
		Nod[GCIdx].Nme := GCNme;
		Result := 66;									// [42] New name for the Current Node
	end;
	69: if GTarg then begin									// [45] Target node formula edit
		if GTFrm <> '' then SetFrmFlg(GTIdx) else ClrFrmFlg(GTIdx);
		Nod[GTIdx].Frm := GTFrm;
		Result := 70;									// [46] New formula for the Target Node
	end
	else Result := 26;									// [1A] 'No Target Node defined' event
	71: if GTarg then begin									// [47] Target node name edit
		if GTNme <> '' then SetNmeFlg(GTIdx) else ClrNmeFlg(GTIdx);
		Nod[GTIdx].Nme := GTNme;
		Result := 72;									// [48] New name for the Target Node
	end
	else Result := 26;									// [1A] 'No Target Node defined' event
	end;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	2.3.	AChCurrNode	Change current node pointer (range & deletion checks omitted)
//		Event:		06 [06] 'Q: Change the Current Node.'
//------------------------------------------------------------------------------------------------------------------------------------------------
function TAngeliophor.AChCurrNode(): Integer;
var	 Cnt: Int64;
begin
	GCIdx := Ap.Idx(GCID);
	GCFlg := Nod[GCIdx].Flg;
	GEdge := False;
	if GTarg then if GCID <> GTID then begin				// If the new CNode has an edge with existing TNode
		for Cnt := 0 to High(Nod[GCIdx].Edg) do
			if ((Nod[GCIdx].Edg[Cnt].ID = GTID) and (not EDelFlg(GCIdx, Cnt))) then begin
				GEdge := True;
				GCEdg := Cnt;
				Break;
			end;
		if GEdge then for Cnt := 0 to High(Nod[GTIdx].Edg) do
			if ((Nod[GTIdx].Edg[Cnt].ID = GCID) and (not EDelFlg(GTIdx, Cnt))) then begin
				GTEdg := Cnt;
				Break;
			end;
	end
	else GTarg := False;
	if not GEdge then GTarg := False;
	Result := 09;								// [09] New current node
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	2.4.	AChTargNode	Change target node pointer (range & deletion checks omitted)
//		Event:		12 [0C] 'Q: Change the Target Node.'
//		Output:		16 [10] 'W: No Target Node specified for the Current Node GCID "GCNme".'
//				17 [11] 'A: New Target Node GTID "GTNme".'
//		if (GTID < 0) or (GTID > High(Nod)) then GTID := -1;	// 13 [0D] 'W: Node GTID not found. Set to none.'
//		if DelFlg(NodID) then GTID := -1			// 14 [0E] 'W: Node GTID GTNme is deleted. Set to none.'
//		15 [0F] 'E: Cyclic edges are not allowed.'
//		16 [10] 'W: No Target Node specified for the Current Node GCID GCNme.'
//------------------------------------------------------------------------------------------------------------------------------------------------
function TAngeliophor.AChTargNode(): Integer;
var	Cnt: Int64;
begin
	GEdge := False;
	if GTarg then begin
		GTIdx := Ap.Idx(GTID);						// If the new Target Node has an edge with existing Current Node
		for Cnt := 0 to High(Nod[GCIdx].Edg) do
		if ((Nod[GCIdx].Edg[Cnt].ID = GTID) and (not EDelFlg(GCIdx, Cnt))) then begin
			GEdge := True;
			GCEdg := Cnt;
			Break;
		end;
		if GEdge then for Cnt := 0 to High(Nod[GTIdx].Edg) do
		if ((Nod[GTIdx].Edg[Cnt].ID = Nod[GCIdx].ID) and (not EDelFlg(GTIdx, Cnt))) then begin
			GTEdg := Cnt;
			Break;
		end;
	end
	else begin
		Result := 26;							// [10] No target node specified, exiting procedure
		Exit;
	end;
	Result := 17;								// [11] New target node set
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	2.5.	AChEType	Change current edge manaz/type (range & deletion checks omitted)
//		Event:		18 [12] 'Change edge type.'
//		Output:		19 [13] 'W: Wrong edge type GTpID, set to 0 "Nod[0].Nme".' <<<<< Result of range & deletion check!!!
//				20 [14] 'W: No Target Node specified. New node and edge?'
//				21 [15] 'A: Type of ascending edge from the Current Node GCID GCNme to the Target Node GTID GTNme
//					set to GTpID GTpNme.'
//				22 [16] 'A: Type of descending edge from the current Node GCID GCNNme to the Target Node GTID GTNme
//					set to GTpID GTpNme.'
//				23 [17] 'W: The Current Node GCID GCNme has no edge with the Target Node GTID GTNme.'
//------------------------------------------------------------------------------------------------------------------------------------------------
function TAngeliophor.AChEType(Mode: Byte): Integer;
begin
	if not GTarg then Result := 26								// [1A] No Target Node specified
	else if GEdge then begin
		if Mode = 0 then begin								// Change current edge manaz
			if EIncFlg(GCIdx, GCEdg) then begin
				Nod[GTIdx].Edg[GTEdg].ETp := GMnID;
				if GMnID = -1 then ClrEMnzFlg(GTIdx, GTEdg) else SetEMnzFlg(GTIdx, GTEdg);
				Result := 68;							// [XX] Ascending edge type change
			end
			else begin
				Nod[GCIdx].Edg[GCEdg].ETp := GMnID;
				if GMnID = -1 then ClrEMnzFlg(GCIdx, GCEdg) else SetEMnzFlg(GCIdx, GCEdg);
				Result := 68							// [XX] Descending edge manaz change
			end;
		end
		else begin
			if EIncFlg(GCIdx, GCEdg) then begin					// Change current edge type
				Nod[GCIdx].Edg[GCEdg].ETp := GTpID;
				if GTpID = -1 then ClrEETpFlg(GCIdx, GCEdg) else SetEETpFlg(GCIdx, GCEdg);
				Result := 21;							// [15] Ascending edge type change
			end
			else begin
				Nod[GTIdx].Edg[GTEdg].ETp := GTpID;
				if GTpID = -1 then ClrEETpFlg(GTIdx, GTEdg) else SetEETpFlg(GTIdx, GTEdg);
				Result := 22							// [16] Descending edge manaz change
			end;
		end;
		if GMnID > -1 then begin
			GMnIdx := Ap.Idx(GMnID);
			GMnFrm := Nod[GMnIdx].Frm;
		end
		else begin
			GMnIdx := 0;
			GMnID := -1;
		end;
		if GTpID > -1 then begin
			GTpIdx := Ap.Idx(GTpID);
			GTpNme := Nod[GTpIdx].Nme;
		end
		else begin
		GTpIdx := 0;
		GTpID := -1;
		end;
	end
	else Result := 23;									// [17] Current Node has no edge with Target Node
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	2.6.	ACommit		Remove deleted/orphaned nodes/sags from the Nod array, reset changes history for the session
//		Event:		41 [29] 'Q: Commit base and clear the history of changes.'
//		Input:		none
//		Output:		42 [2A] 'A: Base commited with [Length(Nod)] nodes and [GEdgeCnt)} edges. History of changes cleared.'
//		Used global variables: none
//		Changed global variables: GNodDelCnt, GEdgDelCnt, GCIdx, GCID, GTIdx, GTID, GCEdg, GTEdg
//------------------------------------------------------------------------------------------------------------------------------------------------
function TAngeliophor.ACommit(): Integer;
var
	SrcNod, SrcEdg, TrgNod, TrgEdg: Int64;
	TmpNod: array of ANode = ();
begin
	// If the node is not marked as deleted then copy it
	if Length(Nod) > 1 then for SrcNod := 0 to High(Nod) do if not DelFlg(SrcNod) then begin
		SetLength(TmpNod, Length(TmpNod) + 1);
		TrgNod := High(TmpNod);
		TmpNod[TrgNod] := Nod[SrcNod];
		SetLength(TmpNod[TrgNod].Edg, 0);
		// If the edge is not marked as deleted then copy it
		for SrcEdg := 0 to High(Nod[SrcNod].Edg) do if not EDelFlg(SrcNod, SrcEdg) then begin
			SetLength(TmpNod[TrgNod].Edg, Length(TmpNod[TrgNod].Edg) + 1);
			TrgEdg := High(TmpNod[TrgNod].Edg);
			TmpNod[TrgNod].Edg[TrgEdg] := Nod[SrcNod].Edg[SrcEdg];
		end;
	end;
	SetLength(Nod, 0);
	Nod := Copy(TmpNod);							// Finally, replace the Nod array with the committed copy
	//--------------------
	GNodDelCnt := 0;							// and reset affected global variables
	GEdgDelCnt := 0;
	//--------------------
	GCIdx := 0;
	GCID := 0;
	GCFlg := Nod[0].Flg;
	GCFrm := Nod[0].Frm;
	GCAdv := Nod[0].Adv;
	GCNme := Nod[0].Nme;
	//--------------------
	GTarg := False;
	GTIdx := 0;
	GTID := -1;
	GTFlg := 0;
	GTFrm := '';
	GTAdv := -1;
	GTNme := '';
	//--------------------
	GEdge := False;
	GCEdg := 0;
	GTEdg := 0;
	GMnID := -1;
	GMnIdx := 0;
	GMnFrm := '';
	GTpID := -1;
	GTpIdx := 0;
	GTpNme := '';
	//--------------------
	Result := 42;
end;

//------------------------------------------------------------------------------------------------------------------------------------------------
//	2.7. AParse - Parse strings (of any origin) to abstract syntactic trees corresonding to the content of the Antikeymena knowledge base
//------------------------------------------------------------------------------------------------------------------------------------------------
// >>>>>>>>>> Forgot to put check for deleted nodes/sags! <<<<<<<<<<
function TAngeliophor.AParse(InputStr: RawByteString): Int64;
//var
//	Cnt1, Cnt2, Cnt3, Cnt4: Int64;
//	FStr: RawByteString;
//	FByte: char;
//	AllSymbols: array of ObjRef;
//	PrsSymbols: array of Int64;						// Identified symbols array
//	ArObj: array[0..5] of Int64;						// Array for arithmetic objects IDs, namely:
//										// Arithmetica, Numerus, Comma, Operatio,
//										// Parenthesis Anterioris, Parenthesis Posterioris
//	PrsInput: array of ObjRef;						// Identified objects array
//	SitID: int;								// Situation ID to handle symbol processing rules
//	PointFlg: boolean;							// Digital point flag
//	BrckCnt: int;								// Braqckets counter
begin
//	Result := 51;
//	//------------------------------------------------------------------------------------------------------------------------------------------
//	// Filtering input string for allowed symbols
//	FStr := '';
//	for Cnt1 := 1 to High(InputStr) do begin
//		FByte := InputStr[Cnt1];
//		if not (FByte in ['0'..'9', '.', ',', '(', ')', '+', '-', '/', '*']) then FByte := #0;	// 18 symbols are allowed yet
//		if FByte <> #0 then FStr := FStr + FByte;
//	end;
//	//------------------------------------------------------------------------------------------------------------------------------------------
//	// Linking allowed symbols to symbol nodes
//	Cnt1 := 0;
//	repeat Inc(Cnt1) until ((Nod[Cnt1].Adv = 'Symboli') or (Cnt1 > High(Nod)));
//	SetLength(AllSymbols, 0);
//	for Cnt2 := 0 to High(Nod[Cnt1].Edg) do
//		if (Nod[Cnt1].Edg[Cnt2].ETp = Nod[Cnt1].ID) then begin
//			SetLength(AllSymbols, Length(AllSymbols) + 1);
//			AllSymbols[High(AllSymbols)].ObjID := Nod[Cnt1].Edg[Cnt2].ID;
//			AllSymbols[High(AllSymbols)].ObjVal := Nod[Ap.Idx(Nod[Cnt1].Edg[Cnt2].ID)].Adv;
//		end;
//	SetLength(PrsSymbols, 0);
//	for Cnt1 := 0 to High(FStr) do begin
//		for Cnt2 := 0 to High(AllSymbols) do if FStr[Cnt1] = AllSymbols[Cnt2].ObjVal then begin
//				SetLength(PrsSymbols, Length(PrsSymbols) + 1);
//				PrsSymbols[High(PrsSymbols)] := AllSymbols[Cnt2].ObjID;
//				Break;
//			end;
//		end;
//	//----------------------------------------------------------------------------------------------------------------------------------------
//	// Getting references to arithmetic objects
//	Cnt1 := 0;
//	repeat Inc(Cnt1) until ((Nod[Cnt1].Adv = 'Arithmetica') or (Cnt1 > High(Nod)));
//	ArObj[0] := Ap.Idx(Nod[Cnt1].ID);
//	for Cnt1 := 0 to High(Nod[ArObj[0]].Edg) do if Nod[Ap.Idx(Nod[ArObj[0]].Edg[Cnt1].ID)].Adv = 'Numerus' then begin
//		ArObj[1] := Nod[Ap.Idx(Nod[ArObj[0]].Edg[Cnt1].ID)].ID;
//		Break;
//	end;
//	for Cnt1 := 0 to High(Nod[ArObj[0]].Edg) do if Nod[Ap.Idx(Nod[ArObj[0]].Edg[Cnt1].ID)].Adv = 'Comma' then begin
//		ArObj[2] := Nod[Ap.Idx(Nod[ArObj[0]].Edg[Cnt1].ID)].ID;
//		Break;
//	end;
//	for Cnt1 := 0 to High(Nod[ArObj[0]].Edg) do if Nod[Ap.Idx(Nod[ArObj[0]].Edg[Cnt1].ID)].Adv = 'Operatio' then begin
//		ArObj[3] := Nod[Ap.Idx(Nod[ArObj[0]].Edg[Cnt1].ID)].ID;
//		Break;
//	end;
//	for Cnt1 := 0 to High(Nod[ArObj[0]].Edg) do if Nod[Ap.Idx(Nod[ArObj[0]].Edg[Cnt1].ID)].Adv = 'Parenthesis Anterioris' then begin
//		ArObj[4] := Nod[Ap.Idx(Nod[ArObj[0]].Edg[Cnt1].ID)].ID;
//		Break;
//	end;
//	for Cnt1 := 0 to High(Nod[ArObj[0]].Edg) do if Nod[Ap.Idx(Nod[ArObj[0]].Edg[Cnt1].ID)].Adv = 'Parenthesis Posterioris' then begin
//		ArObj[5] := Nod[Ap.Idx(Nod[ArObj[0]].Edg[Cnt1].ID)].ID;
//		Break;
//	end;
//	//----------------------------------------------------------------------------------------------------------------------------------------
//	// Processing situations and corresponding rules for each arithmetic object
//	SetLength(PrsInput, 0);
//	PointFlg := False;
//	BrckCnt := 0;
//	for Cnt3 := 0 to High(PrsSymbols) do begin
//		// Defining situations IDs
//		//--------------------------------------------------------------------------------------------------------------------------------
//		SitID := 0;
//		if Length(PrsInput) > 0 then
//		if PrsInput[High(PrsInput)].ObjID = ArObj[1] then SitID := 1
//		else if PrsInput[High(PrsInput)].ObjID = ArObj[2] then SitID := 2
//		else if PrsInput[High(PrsInput)].ObjID = ArObj[3] then SitID := 3
//		else if PrsInput[High(PrsInput)].ObjID = ArObj[4] then SitID := 4
//		else if PrsInput[High(PrsInput)].ObjID = ArObj[5] then SitID := 5;
//		Cnt4 := -1;
//		repeat Inc(Cnt4) until ((Nod[Ap.Idx(PrsSymbols[Cnt3])].Edg[Cnt4].ETp = Nod[ArObj[0]].ID) or
//			(Cnt4 = Length(Nod[Ap.Idx(PrsSymbols[Cnt3])].Edg)));				// Look for symbol sag typed as
//													// 'Arithmetica', there must be only one
//		// If current symbol is a digit
//		//--------------------------------------------------------------------------------------------------------------------------------
//		if Nod[Ap.Idx(Nod[Ap.Idx(PrsSymbols[Cnt3])].Edg[Cnt4].ID)].ID = ArObj[1] then begin	// If the current symbol type is 'Numerus'
//			case SitID of
//			0, 3, 4:	begin
//					SetLength(PrsInput, Length(PrsInput) + 1);
//					PrsInput[High(PrsInput)].ObjID := ArObj[1];			// 'Numerus' node ID
//					PrsInput[High(PrsInput)].ObjVal := Nod[Ap.Idx(PrsSymbols[Cnt3])].Adv;
//				end;
//			1, 2:	PrsInput[High(PrsInput)].ObjVal :=PrsInput[High(PrsInput)].ObjVal + Nod[Ap.Idx(PrsSymbols[Cnt3])].Adv;
//			else	Result := -1;
//			end;
//		end;
//		// If current symbol is digital point or comma
//		//--------------------------------------------------------------------------------------------------------------------------------
//		if Nod[Ap.Idx(Nod[Ap.Idx(PrsSymbols[Cnt3])].Edg[Cnt4].ID)].ID = ArObj[2] then begin	// If the current symbol type is 'Comma'
//			PointFlg := False;
//			case SitID of
//			1:	if not PointFlg then begin
//					PrsInput[High(PrsInput)].ObjVal :=PrsInput[High(PrsInput)].ObjVal + Nod[Ap.Idx(PrsSymbols[Cnt3])].Adv;
//					PointFlg := True;
//				end
//				else Result := -1;
//			else	Result := -1;
//			end;
//		end;
//		// If current symbol is an arithmetic operation symbol
//		//--------------------------------------------------------------------------------------------------------------------------------
//		if Nod[Ap.Idx(Nod[Ap.Idx(PrsSymbols[Cnt3])].Edg[Cnt4].ID)].ID = ArObj[3] then begin	// If the curr. symbol type is 'Operatio'
//			case SitID of
//			1, 5:	begin
//					SetLength(PrsInput, Length(PrsInput) + 1);
//					PrsInput[High(PrsInput)].ObjID :=ArObj[3];			// 'Operatio' node ID
//					PrsInput[High(PrsInput)].ObjVal :=Nod[Ap.Idx(PrsSymbols[Cnt3])].Adv;
//				end;
//			else	Result := -1;
//			end;
//		end;
//		// If current symbol is the left bracket
//		//--------------------------------------------------------------------------------------------------------------------------------
//		if Nod[Ap.Idx(Nod[Ap.Idx(PrsSymbols[Cnt3])].Edg[Cnt4].ID)].ID = ArObj[4] then begin	// If the current symbol type is
//													// 'Parenthesis Anterioris'
//			case SitID of
//			0, 3, 4:begin
//					SetLength(PrsInput, Length(PrsInput) + 1);
//					PrsInput[High(PrsInput)].ObjID := ArObj[4];			// 'Parenthesis Anterioris' node ID
//					PrsInput[High(PrsInput)].ObjVal := Nod[Ap.Idx(PrsSymbols[Cnt3])].Adv;
//					Inc(BrckCnt);
//				end;
//			else	Result := -1;
//			end;
//		end;
//		// If current symbol is the right bracket
//		//--------------------------------------------------------------------------------------------------------------------------------
//		if Nod[Ap.Idx(Nod[Ap.Idx(PrsSymbols[Cnt3])].Edg[Cnt4].ID)].ID = ArObj[5] then begin	// If the current symbol type is
//													// 'Parenthesis Posterioris'
//			case SitID of
//			1, 5:	begin
//					SetLength(PrsInput, Length(PrsInput) + 1);
//					PrsInput[High(PrsInput)].ObjID := ArObj[5];			// 'Parenthesis Posterioris' node ID
//					PrsInput[High(PrsInput)].ObjVal := Nod[Ap.Idx(PrsSymbols[Cnt3])].Adv;
//					Dec(BrckCnt);
//				end;
//			else	Result := -1;
//			end;
//		end;
//	end;
//	if BrckCnt <> 0 then Result := -1;								// Check for unpaired brackets}
end;
//================================================================================================================================================

end.

