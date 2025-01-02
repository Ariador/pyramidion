
//==================================================================================================================================================================================
//  PYRAMIDION NON-RELATIONAL DATABASE, v.0.16 (alpha version)
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//  (c) 2020 Igor Voloshin ivoloshin@hotmail.com
//==================================================================================================================================================================================
//  Unit Apostolia016
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//  Contains command line parcer and other Levels -1, 1 and 2 procedures and functions of Desmous DBMS,
//  basic to the Sophos Knowledge Base Management System (Sophos KBMS).
//
//	LEVEL -1. INTERNAL PROGRAM TOOLS
//		-1.1. IDSCRC - Calculate CRC32 for a MemoryStream content (of any origin;
//
//	LEVEL 1. APOSTOLIA: DATA INTERFACE (CDRWPI - Create, Destroy, Read, Write, Proseychi, Idx);
//		1.0. ACreate - Initialise IDS base;
//		1.1. ADestroy - Destruct IDS base;
//		1.2. ARead - Read the Pleximo data from external IDS file to the Nod array;
//		1.3. AWrite - Write all the Pleximo data to external IDS file;
//		1.4. AERead - Read the Eidetika data from external EID file to the Eidov array;
//		1.5. AEWrite - Write all the Eidetika data to wxternal EID file;
//		1.6. AEMem - Add an event to the Eidetika Eidon array;
//		1.7. Proseychi - Process an interface call [public]
//		1.8. Idx - Return index of a node [public]
//
//	LEVEL 2. ANGELIOPHORES: BASIC MECHANICS (ADECP - Add, Delete, Edit, Commit, Parse)
//	      	2.0. AAdd - Add a node or an edge;
//		2.1. ADelete - Delete an edge;
//		2.2. AEdit - Edit comment of a node;
//              2.3. AChNode - Current node pointer update
//		2.4. AChEType - Change edge type
//		2.5. ACommit - Remove deleted nodes and sags from the Nod array, reset changes history for the session;
//		2.6. AParse - Parse strings of any origin to abstract syntactic trees corresonding to the content of the Antikeymena knowledge base
//
//==================================================================================================================================================================================

unit Apostolia016;

{$mode objfpc}

interface

uses
  Classes, CRC, ZStream, SysUtils;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	Edge reference type Declaration
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
type	SID =	record				// Special type is required to sags of a node.
	ID:	longint;			// Linked Node ID (node at the other end of the edge);
	Flg:	byte;				// Flags byte. Currently 2 rightmost bits are used, with the following flags:
						// byte #6 Del: deleted sag if 1 (existing if 0);
						// byte #7 Dir: edge from the node (i.e. descending) if 1, to the node (i.e. ascending) if 0;
	ETp:	longint;			// Type of the edge encoded in longint, just referring the ID of the type-representing node.
						// Types proposed so far (numbers for all types except 0 are preliminary):
						// 0 - no fixed type (Zero or 'free' or 'Informatio' type, reference to Node ID0 'Informatio');
						// 1 - Inheritance - Abstract/Concrete relation;
						// 2 - Division - Whole/Part relation;
						// 3 - Featuring - Object/Property relation;
						// 4 - Constraints to a node (decode with parcer);
						// 5 - Instructions to a node (decode with parcer).
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	Nodes Record Declaration
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
type	ANode =	record				// Pyramidion Node corresponds to orgraph node.
//	BID:	longint;			// Knowledge Base ID (to mount multiple bases); - this is to implement later
	ID:	longint;			// Unique Node ID;
	Flg:	byte;				// Flags byte. Currently 5 rightmost bits are used, with the following flags:
						// Bit #3: Del flag - deleted node if 1 (existing if 0);
						// Bit #4: Lnk flag - linked to the CurID node if 1 (not if 0);
						// Bit #5: Asc flag - an ascending egde (going to the node) present if 1 (none if 0);
						// Bit #6: Dsc flag - a descending egde (going from the node) present if 1 (none if 0);
						// Bit #7: Com flag - a non-empty comment field present if 1 (0 otherwise).
	Edg:	array of SID;			// List of (typed and directed) edges of the node;
	Com:	rawbytestring;			// Comment field able to contain binary data.
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	Eidetika Record Declaration
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
type	AEidon = record				// Record type for the Events Strip (Eidetika). "Ειδον" means "I see".
	TStmp:	comp;				// Time stamp in the millisecond since 1/1/0001 format (timezone independent unlike TDateTime);
	ChCnt:	integer;			// Counter of events within milliseconds (because there is no reliable nanoseconds counter in multicore CPUs);
        EvnTp:	byte;				// Event type (few types are anticipated, so just a byte used to store type #);
  	Args:	rawbytestring;			// Arguments of the event (specific to each event type).
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	Identified objects type Declaration
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
type	ObjRef =	record			// Special type is required to store objects references linked to their content.
  	ObjID:	longint;			// Object Node ID;
  	ObjVal:	rawbytestring;			// Current content of the object.
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	Apostolia Class Declaration
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
type	TApostolia =	class(TObject)
	private
		function	ACreate(): integer;
		procedure	ADestroy();
		function	ARead(): integer;
		function	AWrite(): qword;
		function	AERead(): integer;
		function	AEWrite(): qword;
                procedure	AEMem(EventNo: integer);
	public
		function	Proseychi(): integer;
                function	Idx(InputID: longint): longint;
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	Angeliophor Class Declaration
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
type	TAngeliophor =	class(TObject)
	private
                function	AAdd(Mode: byte; EdgTp: longint; Cmt: rawbytestring): longint;
                function	ADelete(): integer;
                function	AEdit(Mode: byte; Cmt: rawbytestring): integer;
                function	AChNode(Mode: byte; NodID: longint): integer;
                function	AChEType(EType: longint): integer;
		function	ACommit(): integer;
  		function	AParse(InputStr: rawbytestring): integer;
	public
end;

var

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	Instances of the main program objects
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Ap:	TApostolia;			// All interface calls are processed here;
Ag:	TAngeliophor;			// All data are processed here;
Nod:	array of ANode;			// All the Pleximo nodes are placed here;
Eidon:	array of AEidon;		// Eidetika, Events strip. Everything what's happening is fixed here for good;
Undo:	array of array of ANode;	// <--- Remake this

implementation

uses Aesthesia016;

//==================================================================================================================================================================================
//	LEVEL -1. INTERNAL PROGRAM TOOLS
//==================================================================================================================================================================================
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	-1.1. IDSCRC - Calculate CRC32 for a MemoryStream content (of any origin)
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
function IDSCRC(IDS: TMemoryStream): longint;
var
	Len: integer;
	Buf: array[0..127] of byte;									// 128 bytes so far, to increase to 1024 later
begin
	IDS.Position := 0;
	for Len := 0 to 127 do Buf[Len] := 0;
	Result := Crc32(0, nil, 0);									// Initialize CRC bits
	repeat
		Len := IDS.Read(Buf, 128);								// Remember: 128 so far!
		Result := Crc32(Result, @Buf, Len)							// Update CRC hash
	until Len < 128;
end;

//==================================================================================================================================================================================
// LEVEL 1. DATA INTERFACE (CDRWPI - Create, Destroy, Read, Write, Proseychi, Idx)
//==================================================================================================================================================================================
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	1.0. ACreate - Create IDS base instance;
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
function TApostolia.ACreate(): integer;
begin
	Result := ARead;
	// Set global pointers
	GCurrIdx := 0;
	GTargIdx := -1;
	GCurrEdge := -1;
        GTargEdge := -1;
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	1.1. ADestroy - Destruct IDS base instance;
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TApostolia.ADestroy();
begin
	Ap.Free;
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	1.2. ARead - Read the Pleximo data from external IDS file to memory
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	Result value is the following:
//	1      - IDS file not found, create new initial IDS file instead;
//	2      - IDS file is empty, fill it anew with initial IDS data;
//	3      - IDS file header is not 'IDS 0.16', backup it & create new IDS file;
//	4      - IDS file is corrupted, CRC failed;
//	5      - success reading IDS file to Nod array;
//	6      - IDS file logical consistency check failed (not yet implemented). This thorough check follows every edge to find whether it's not cyclic or
//	         doubled and its ends are matching each other (Asc/Desc and fixed type check) and whether no separate parts exist in IDS base.
//	Note: All integers are 64 bit qwords, written in IDS file in reverse order.
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
function TApostolia.ARead(): integer;
var
	IDS: TMemoryStream;
	IDTmp: TMemoryStream;
	UnZ: TDecompressionStream;
	UnZStr: TStringStream;
	InitStr: rawbytestring = #$49 + #$44 + #$53 + #$20 + #$30 + #$2E + #$31 + #$36 + #$3A + #$8F + #$00 + #$CD + #$FF + #$FF + #$FF + #$FF + #$78 + #$9C +
	#$63 + #$64 + #$40 + #$05 + #$8C + #$5C + #$50 + #$86 + #$67 + #$5E + #$5A + #$7E + #$51 + #$6E + #$62 + #$49 + #$66 + #$3E + #$00 + #$16 + #$EC +
	#$04 + #$25; // Initial Pyramidion016.ids file is just 41 byte, it's ok to put it here
	Cnt1, Cnt2: qword;
begin
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	//	Handling the IDS file with all the necessary checks
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	Result := 5;											// Initialise Result to success message code
	IDS := TMemoryStream.Create;
	if FileExists(GIDSFile) then IDS.LoadFromFile(GIDSFile)
	else begin											// If no IDS file exist then create new initial IDS file
		Result := 1;										// IDS file not found. Create new one
		for Cnt1 := 1 to 41 do IDS.WriteByte(Byte(InitStr[Cnt1]));
		IDS.SaveToFile(GIDSFile);
		IDS.Position := 0;
	end;
	If IDS.Size = 0 then begin									// Check file size
		Result := 2;										// IDS file is empty. Replace it
		DeleteFile(GIDSFile);
		for Cnt1 := 1 to 41 do IDS.WriteByte(Byte(InitStr[Cnt1]));
		IDS.SaveToFile(GIDSFile);
		IDS.Position := 0;
	end;
	Cnt1 := IDS.ReadQWord;
	if Cnt1 <> 3904953136142566473 then begin							// Check for 'IDS 0.16' header
		Result := 3;										// IDS file is not v.0.16. Replace it
		RenameFile(GIDSFile, GIDSFile + '.old');
		for Cnt1 := 1 to 41 do IDS.WriteByte(Byte(InitStr[Cnt1]));
		IDS.SaveToFile(GIDSFile);
		IDS.Position := 8;
	end;
	Cnt1 := IDS.ReadQWord;										// Check CRC for the file remainder
	IDTmp := TMemoryStream.Create;
	IDTmp.CopyFrom(IDS, IDS.Size - 16);
	Cnt2 := IDSCRC(IDTmp);
	IDTmp.Free;
	If Cnt1 <> Cnt2 then begin
		Result := 4;										// IDS file is corrupted. Replace it
		RenameFile(GIDSFile, GIDSFile + '.old');
		for Cnt1 := 1 to 41 do IDS.WriteByte(Byte(InitStr[Cnt1]));
		IDS.SaveToFile(GIDSFile);
	end;
	IDS.Position := 8;
	GCRC32 := IDS.ReadQWord;
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	//	Unpacking data from IDS file
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	UnZ := TDecompressionStream.Create(IDS);
	UnZStr := TStringStream.Create('');
	UnZStr.CopyFrom(UnZ, 0);
	UnZ.Free;
	UnZStr.Position := 0;
	IDS.Position := 0;
	IDS.CopyFrom(UnZStr, UnZStr.Size);
	UnZStr.Free;
	IDS.Position := 0;
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	//	Handling the data structure of IDS file:
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	//	RecCnt (8 bytes)		Records count for Nodes section.
	//	RecCnt times:
	//	Flags (1 byte)			Node Flags field;
	//	EdgCnt (8 bytes)		Edges count;
	//	SID (EdgCnt * (17 bytes)	Types & IDs of the edges;
	//	ComLen (8 bytes)		Comment length;
	//	ComTXT (ComLen bytes)		Comment text.
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	//	Get RecordCount in Nodes section
	SetLength(Nod, IDS.ReadQWord);
        GUpperID := 0;                                                                                  // Initialize global Upper Nod ID variable
        GNodDCnt := 0;											// Update global deleted nodes counter
        GEdgeCnt := 0;											// Initialize global edge counter
        GEdgDCnt := 0;											// Initialize global deleted edges counter
        //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	//	Read & convert 1 to 8 fields for each Node
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	for Cnt1 := 0 to High(Nod) do begin
	    	Nod[Cnt1].ID := IDS.ReadQWord;
                if Nod[Cnt1].ID > GUpperID then GUpperID := Nod[Cnt1].ID;                               // Update global Upper Nod ID variable
                Nod[Cnt1].Flg := IDS.ReadByte;
                if Nod[Cnt1].Flg and 16 > 0 then Inc(GNodDCnt);						// Update global deleted nodes counter
		if Length(Nod) > 1 then begin	 	    						// Check Nod length to define presence of other nods and sags
			SetLength(Nod[Cnt1].Edg, IDS.ReadQWord);
			for Cnt2 := 0 to High(Nod[Cnt1].Edg) do begin
				Nod[Cnt1].Edg[Cnt2].ID := IDS.ReadQWord;
				Nod[Cnt1].Edg[Cnt2].Flg := IDS.ReadByte;
                                if Nod[Cnt1].Edg[Cnt2].Flg and 2 > 0 then Inc(GEdgDCnt) else Inc(GEdgeCnt); // Update global edges and deleted edges counters
				Nod[Cnt1].Edg[Cnt2].ETp := IDS.ReadQWord;
			end;
		end;	    				   		 				// Then check presence of comment for this nod
		if (Nod[Cnt1].Flg and 1) > 0 then for Cnt2 := 1 to IDS.ReadQWord do Nod[Cnt1].Com := Nod[Cnt1].Com + Chr(IDS.ReadByte);
	end;
        GEdgeCnt := GEdgeCnt div 2;   	     	      	      	   		    		     	// Because each edge has two ends writted in the Nod array
        GEdgDCnt := GEdgDCnt div 2;   	     	      	      	   		    		     	// Because each edge has two ends writted in the Nod array
	IDS.Free;
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	//	Define here whether allow undo service or not
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	GAllowUndo := 0; // Set initial value to 0 (no undo)
	GUndo := 0;     // Set initial value to 0 (current step)
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	1.3. AWrite - Write all the Pleximo data to external IDS file
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
function TApostolia.AWrite(): qword;
var
	IDStr: TStringStream;
	IDS: TMemoryStream;
	IDTmp: TMemoryStream;
	PkZ: TCompressionStream;
	Cnt1, Cnt2: qword;
begin
	IDStr := TStringStream.Create('');
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	//	Data structure in IDS file:
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	//	RecCnt (8 bytes)		Records count for Nodes section.
	//	RecCnt times:
	//	ID (8 bytes) 			Node ID;
	//	Flags (1 byte)			Node Flags field;
	//	EdgCnt (8 byte)			Edges count;
	//	SID (EdgCnt * (17 bytes)	IDs of the edges;
	//	ComLen (8 bytes)		Comment length;
	//	ComTXT (ComLen bytes)		Comment text.
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	//	Write Nodes quantity
	IDStr.WriteQWord(Length(Nod));
	//	Convert & write 1 to 6 fields for each node
	for Cnt1 := 0 to High(Nod) do begin
                IDStr.WriteQWord(Nod[Cnt1].ID);
                IDStr.WriteByte(Nod[Cnt1].Flg);
		if Length(Nod[Cnt1].Edg) > 0 then IDStr.WriteQWord(Length(Nod[Cnt1].Edg));
		if Length(Nod[Cnt1].Edg) > 0 then for Cnt2 := 0 to High(Nod[Cnt1].Edg) do begin
			IDStr.WriteQWord(Nod[Cnt1].Edg[Cnt2].ID);
			IDStr.WriteByte(Byte(Nod[Cnt1].Edg[Cnt2].Flg));
			IDStr.WriteQWord(Nod[Cnt1].Edg[Cnt2].ETp);
		end;
		if (Nod[Cnt1].Flg and 1) > 0 then begin							// If a comment exists, write it down
			IDStr.WriteQWord(Length(Nod[Cnt1].Com));
			for Cnt2 := 1 to Length(Nod[Cnt1].Com) do IDStr.WriteByte(Byte(Nod[Cnt1].Com[Cnt2]));
		end;
	end;
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	//	Composing IDS file contents (header, CRC32, pack data) and save file
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	IDStr.Position := 0;
	IDS := TMemoryStream.Create;
	PkZ := TCompressionStream.Create(clDefault, IDS);
	PkZ.CopyFrom(IDStr, IDStr.Size);
	PkZ.Free;
	IDStr.Free;
	IDTmp := TMemoryStream.Create();
	IDTmp.WriteQWord(3904953136142566473);								// Put header 'IDS 0.16'
	Result := IDSCRC(IDS);
	GCRC32 := Result;
	IDTmp.WriteQWord(Result);
	IDS.Position := 0;
	IDTmp.CopyFrom(IDS, IDS.Size);
	IDS.Free;
	if Form.RFileSave.Execute then GIDSFile := Form.RFileSave.Filename;
	if FileExists(GIDSFile + '.bak') then DeleteFile(GIDSFile + '.bak');
	if FileExists(GIDSFile) then RenameFile(GIDSFile, GIDSFile + '.bak');
	IDTmp.SaveToFile(GIDSFile);
	IDTmp.Free;
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	1.4. AERead - Read the Events Strip (Eidetika) data from external EID file to memory
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	Result value is the following:
//	51     - EID file not found, create new initial IDS file instead;
//	52     - EID file is empty, fill it anew with initial IDS data;
//	53     - EID file header is not 'EID 0.16', backup it & create new IDS file;
//	54     - EID file is corrupted, CRC failed;
//	55     - success reading EID file to Eid array;
//	Note: All integers are 64 bit qwords, written in EID file in reverse order.
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
function TApostolia.AERead(): integer;
var
	EID: TMemoryStream;
        EIDFile: string;
        InitStr: TStringStream;
	EIDTmp: TMemoryStream;
	EPkZ: TCompressionStream;
	EUnZ: TDecompressionStream;
	EUnZStr: TStringStream;
	Cnt1, Cnt2, Cnt3: qword;
begin
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	//	Handling the IDS file with all the necessary checks
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        //	1. Composing the initial EID record
	InitStr := TStringStream.Create('');
	InitStr.WriteQWord(1);		      								// Writing RecCnt field	(1)
	InitStr.WriteQWord(qword(TimeStampToMSecs(DateTimeToTimeStamp(Now))));				// Writing TStmp field	(Now)
	InitStr.WriteDWord(0);										// Writing ChCnt field	(0)
	InitStr.WriteByte(0);										// Writing EvnTp field	(0)
	InitStr.WriteQWord(0);										// Writing ArgLen field	(0)
        //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        //	2. Composing the initial EID file
        InitStr.Position := 0;
	EID := TMemoryStream.Create;
	EPkZ := TCompressionStream.Create(clDefault, EID);
	EPkZ.CopyFrom(InitStr, InitStr.Size);
	EPkZ.Free;
	InitStr.Free;
	InitStr := TStringStream.Create('');
	InitStr.WriteQWord(3904953136141584709);							// Put header 'EID 0.16'
	Result := IDSCRC(EID);
	InitStr.WriteQWord(Result);
	EID.Position := 0;
	InitStr.CopyFrom(EID, EID.Size);								// Initial EID file text (37 bytes) is in InitStr string stream
	EID.Free;
        //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        //	3. Making 4 checks for the EID file
	Result := 56;											// Initialise Result to [38] Success message
        EIDFile := LeftStr(GIDSFile, Length(GIDSFile) - 3) + 'eid';
	EID := TMemoryStream.Create;
        if FileExists(EIDFile) then EID.LoadFromFile(EIDFile)
	else begin											// If no EID file exist then create new initial EID file
		Result := 52;										// [34] EID file not found. Create new one
		for Cnt1 := 1 to Length(InitStr.DataString) do EID.WriteByte(Byte(InitStr.DataString[Cnt1]));
		EID.SaveToFile(EIDFile);
		EID.Position := 0;
	end;
	If EID.Size = 0 then begin									// Check file size
		Result := 53;										// [35] EID file is empty. Replace it
		DeleteFile(EIDFile);
		for Cnt1 := 1 to Length(InitStr.DataString) do EID.WriteByte(Byte(InitStr.DataString[Cnt1]));
		EID.SaveToFile(EIDFile);
		EID.Position := 0;
	end;
	Cnt1 := EID.ReadQWord;
	if Cnt1 <> 3904953136141584709 then begin							// Check for 'EID 0.16' header
		Result := 54;										// [36] EID file is not v.0.16. Replace it
		RenameFile(EIDFile, EIDFile + '.old');
		for Cnt1 := 1 to Length(InitStr.DataString) do EID.WriteByte(Byte(InitStr.DataString[Cnt1]));
		EID.SaveToFile(EIDFile);
		EID.Position := 8;
	end;
	Cnt1 := EID.ReadQWord;										// Check CRC for the file remainder
	EIDTmp := TMemoryStream.Create;
	EIDTmp.CopyFrom(EID, EID.Size - 16);
	Cnt2 := IDSCRC(EIDTmp);
	EIDTmp.Free;
	If Cnt1 <> Cnt2 then begin
		Result := 55;										// [37] IDS file is corrupted. Replace it
		RenameFile(EIDFile, EIDFile + '.old');
		for Cnt1 := 1 to Length(InitStr.DataString) do EID.WriteByte(Byte(InitStr.DataString[Cnt1]));
		EID.SaveToFile(EIDFile);
	end;
	EID.Position := 16;
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	//	4. Unpacking data from EID file
	EUnZ := TDecompressionStream.Create(EID);
	EUnZStr := TStringStream.Create('');
	EUnZStr.CopyFrom(EUnZ, 0);
	EUnZ.Free;
	EUnZStr.Position := 0;
	EID.Position := 0;
	EID.CopyFrom(EUnZStr, EUnZStr.Size);
	EUnZStr.Free;
	EID.Position := 0;
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	//	Handling the data structure of EID file:
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        //	RecCnt:	longint			Records count
        //	RecCnt times:
        //	TStmp:	TTimeStamp;		Time stamp (consists of Time:longint + Date:longint and is timezone independent unlike TDateTime);
        //	ChCnt:	integer;		Counter of events within milliseconds (because there is no reliable nanoseconds counter in multicore CPUs);
        //      EvnTp:	byte;			Event type (few types are anticipated, so no more than a byte to store type #);
        //	ArgLen:	longint			Arguments string length
        //  	Args:	rawbytestring;		Arguments of the event (specific to each event type).
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	//	5. Get RecordCount in Eidon array
	SetLength(Eidon, EID.ReadQWord);
        //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	//	6. Read & convert 4 to 5 fields for each Eidon record
	for Cnt1 := 0 to High(Eidon) do begin
	    	Eidon[Cnt1].TStmp := EID.ReadQWord;
                Eidon[Cnt1].ChCnt := EID.ReadDWord;
                Eidon[Cnt1].EvnTp := EID.ReadByte;
                Cnt2 := EID.ReadQWord;
		if Cnt2 > 0 then for Cnt3 := 1 to Cnt2 do Eidon[Cnt1].Args := Eidon[Cnt1].Args + Chr(EID.ReadByte);
	end;
	EID.Free;
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	1.5. AEWrite - Update the Events Strip EID file
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
function TApostolia.AEWrite(): qword;
var
	EIDStr: TStringStream;
	EID: TMemoryStream;
        EIDFile: string;
	EIDTmp: TMemoryStream;
	EPkZ: TCompressionStream;
	Cnt1, Cnt2: qword;
begin
	EIDStr := TStringStream.Create('');
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	//	Data structure in EID file:
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        //	RecCnt:	longint			Records count
        //	RecCnt times:
        //	TStmp:	TTimeStamp;		Time stamp (consists of Time:longint + Date:longint and is timezone independent unlike TDateTime);
        //	ChCnt:	integer;		Counter of events within milliseconds (because there is no reliable nanoseconds counter in multicore CPUs);
        //      EvnTp:	byte;			Event type (few types are anticipated, so no more than a byte to store type #);
        //	ArgLen:	longint			Arguments string length
        //  	Args:	rawbytestring;		Arguments of the event (specific to each event type). <--- try to make fixed length field herer!
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	//	1. Write Eidon records quantity
	EIDStr.WriteQWord(Length(Eidon));
        //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	//	2. Convert & write 4 to 5 fields for each Eidon record
	for Cnt1 := 0 to High(Eidon) do begin
                EIDStr.WriteQWord(qword(Eidon[Cnt1].TStmp));
                EIDStr.WriteDWord(Eidon[Cnt1].ChCnt);
                EIDStr.WriteByte(Eidon[Cnt1].EvnTp);
                EIDStr.WriteQWord(Length(Eidon[Cnt1].Args));
		if Length(Eidon[Cnt1].Args) > 0 then begin						// If a comment exists, write it down
			for Cnt2 := 1 to Length(Eidon[Cnt1].Args) do EIDStr.WriteByte(Byte(Eidon[Cnt1].Args[Cnt2]));
		end;
	end;
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	//	Composing IDS file contents (header, CRC32, pack data) and save file
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	EIDStr.Position := 0;
	EID := TMemoryStream.Create;
	EPkZ := TCompressionStream.Create(clDefault, EID);
	EPkZ.CopyFrom(EIDStr, EIDStr.Size);
	EPkZ.Free;
	EIDStr.Free;
	EIDTmp := TMemoryStream.Create();
	EIDTmp.WriteQWord(3904953136141584709);								// Put header 'EID 0.16'
	Result := IDSCRC(EID);
	EIDTmp.WriteQWord(Result);
	EID.Position := 0;
	EIDTmp.CopyFrom(EID, EID.Size);
	EID.Free;
        EIDFile := LeftStr(GIDSFile, Length(GIDSFile) - 3) + 'eid';
        if FileExists(EIDFile + '.bak') then DeleteFile(EIDFile + '.bak');
	if FileExists(EIDFile) then RenameFile(EIDFile, EIDFile + '.bak');
	EIDTmp.SaveToFile(EIDFile);
	EIDTmp.Free;
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	1.6. AEMem - Add an event to the Event Strip (Eidetika)
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	Data structure in Eidon array:
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	TStmp:	TTimeStamp;		Time stamp (consists of Time:longint + Date:longint and is timezone independent unlike TDateTime);
//	ChCnt:	integer;		Counter of events within milliseconds (because there is no reliable nanoseconds counter in multicore CPUs);
//      EvnTp:	byte;			Event type (few types are anticipated, so no more than a byte to store type #);
//	ArgLen:	longint			Arguments string length
//  	Args:	rawbytestring;		Arguments of the event (specific to each event type).
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
procedure TApostolia.AEMem(EventNo: integer);
var	ConsoleLine: string;
        METype, CurrNode, TargNode: longint;
begin
	ConsoleLine := Form.SCon.Lines[Form.SCon.Lines.Count - 2];
        ConsoleLine := RightStr(ConsoleLine, Length(ConsoleLine) - 38);
        if Form.ETp.Text = '' then METype := 0 else METype := StrToInt(Form.ETp.Text);
        if Length(Nod) > 0 then CurrNode := Nod[GCurrIdx].ID else CurrNode := 0;
        if Form.TNodID.Text = '' then TargNode := -1 else TargNode := Nod[GTargIdx].ID;
        SetLength(Eidon, Length(Eidon) + 1);
        Eidon[High(Eidon)].TStmp := TimeStampToMSecs(DateTimeToTimeStamp(Now));
        if ((Length(Eidon) > 1) and (Eidon[High(Eidon)].TStmp = Eidon[High(Eidon) - 1].TStmp))
           	then Eidon[High(Eidon)].ChCnt := Eidon[High(Eidon) - 1].ChCnt + 1 else Eidon[High(Eidon)].ChCnt := 0;
        Eidon[High(Eidon)].EvnTp := byte(EventNo);
        case EventNo of
                -1: Eidon[High(Eidon)].Args := ConsoleLine;
                00: Eidon[High(Eidon)].Args := GIDSFile;
                01: Eidon[High(Eidon)].Args := GIDSFile;
                02: Eidon[High(Eidon)].Args := GIDSFile;
                03: Eidon[High(Eidon)].Args := GIDSFile;
                04: Eidon[High(Eidon)].Args := GIDSFile;
                05: Eidon[High(Eidon)].Args := BinStr(GCRC32, 32) + BinStr(Length(Nod) - GNodDCnt, 64) + BinStr(GEdgeCnt,64) + GIDSFile;
                06: Eidon[High(Eidon)].Args := '';
                07: Eidon[High(Eidon)].Args := BinStr(StrToInt(Form.CNodID.Text), 64);
                08: Eidon[High(Eidon)].Args := BinStr(StrToInt(Form.CNodID.Text), 64) + Nod[Ap.Idx(StrToInt(Form.CNodID.Text))].Com;
                09: Eidon[High(Eidon)].Args := BinStr(CurrNode, 64) + Nod[GCurrIdx].Com;
                10: Eidon[High(Eidon)].Args := '';
                11: Eidon[High(Eidon)].Args := BinStr(CurrNode, 64) + Form.CNodCom.Text;
                12: Eidon[High(Eidon)].Args := '';
                13: Eidon[High(Eidon)].Args := BinStr(TargNode, 64);
                14: Eidon[High(Eidon)].Args := BinStr(TargNode, 64) + Nod[Ap.Idx(TargNode)].Com;
                15: Eidon[High(Eidon)].Args := '';
                16: Eidon[High(Eidon)].Args := BinStr(StrToInt(Form.CNodID.Text), 64) + Form.CNodCom.Text;
                17: Eidon[High(Eidon)].Args := BinStr(TargNode, 64) + Nod[GTargIdx].Com;
                18: Eidon[High(Eidon)].Args := '';
                19: Eidon[High(Eidon)].Args := BinStr(METype, 64) + Nod[0].Com;
                20: Eidon[High(Eidon)].Args := '';
                21: Eidon[High(Eidon)].Args := BinStr(StrToInt(Form.CNodID.Text), 64) + BinStr(Length(Form.CNodCom.Text), 32) + Form.CNodCom.Text + BinStr(TargNode, 64) +
                    BinStr(Length(GTargComment), 32) + GTargComment + BinStr(METype, 64) + Nod[Ap.Idx(METype)].Com;
                22: Eidon[High(Eidon)].Args := BinStr(StrToInt(Form.CNodID.Text), 64) + BinStr(Length(Form.CNodCom.Text), 32) + Form.CNodCom.Text + BinStr(TargNode, 64) +
                    BinStr(Length(GTargComment), 32) + GTargComment + BinStr(METype, 64) + Nod[Ap.Idx(METype)].Com;
                23: Eidon[High(Eidon)].Args := BinStr(StrToInt(Form.CNodID.Text), 64) + BinStr(Length(Form.CNodCom.Text), 32) + Form.CNodCom.Text + BinStr(TargNode, 64) +
                    BinStr(Length(GTargComment), 32) + GTargComment;
                24: Eidon[High(Eidon)].Args := '';
                25: Eidon[High(Eidon)].Args := BinStr(TargNode, 64) + GTargComment;
                26: Eidon[High(Eidon)].Args := '';
		27: Eidon[High(Eidon)].Args := BinStr(METype, 64) + BinStr(Length(Nod[Ap.Idx(METype)].Com), 32) + Nod[Ap.Idx(METype)].Com +
		    BinStr(StrToInt(Form.CNodID.Text), 64) + BinStr(Length(Form.CNodCom.Text), 32) + Form.CNodCom.Text + BinStr(TargNode, 64) + GTargComment;
                28: Eidon[High(Eidon)].Args := BinStr(METype, 64) + BinStr(Length(Nod[Ap.Idx(METype)].Com), 32) + Nod[Ap.Idx(METype)].Com +
                    BinStr(StrToInt(Form.CNodID.Text), 64) + BinStr(Length(Form.CNodCom.Text), 32) + Form.CNodCom.Text + BinStr(TargNode, 64) +
                    BinStr(Length(GTargComment), 32) + GTargComment;
                29: Eidon[High(Eidon)].Args := '';
                30: Eidon[High(Eidon)].Args := '';
                31: Eidon[High(Eidon)].Args := BinStr(TargNode, 64);
                32: Eidon[High(Eidon)].Args := BinStr(Nod[GTargIdx].ID, 64) + Nod[GTargIdx].Com;
                33: Eidon[High(Eidon)].Args := BinStr(METype, 64) + BinStr(Length(Nod[Ap.Idx(METype)].Com), 32) + Nod[Ap.Idx(METype)].Com +
                    BinStr(StrToInt(Form.CNodID.Text), 64) + BinStr(Length(Form.CNodCom.Text), 32) + Form.CNodCom.Text + BinStr(Nod[GTargIdx].ID, 64) + GTargComment;
                34: Eidon[High(Eidon)].Args := BinStr(METype, 64) + BinStr(Length(Nod[Ap.Idx(METype)].Com), 32) + Nod[Ap.Idx(METype)].Com +
                    BinStr(StrToInt(Form.CNodID.Text), 64) + BinStr(Length(Form.CNodCom.Text), 32) + Form.CNodCom.Text + BinStr(Nod[GTargIdx].ID, 64) + GTargComment;
                35: Eidon[High(Eidon)].Args := BinStr(CurrNode, 64) + Nod[GCurrIdx].Com;
                36: Eidon[High(Eidon)].Args := '';
                37: Eidon[High(Eidon)].Args := BinStr(Nod[GTargIdx].ID, 64) + Nod[GTargIdx].Com;
                38: Eidon[High(Eidon)].Args := BinStr(CurrNode, 64) + BinStr(Length(Nod[GCurrIdx].Com), 32) + Nod[GCurrIdx].Com + BinStr(TargNode, 64) + Nod[GTargIdx].Com;
                39: Eidon[High(Eidon)].Args := '';
                40: Eidon[High(Eidon)].Args := '';
                41: Eidon[High(Eidon)].Args := '';
                42: Eidon[High(Eidon)].Args := BinStr(Length(Nod), 64) + BinStr(GEdgeCnt, 64);
                43: Eidon[High(Eidon)].Args := '';
                44: Eidon[High(Eidon)].Args := BinStr(GCRC32, 32) + BinStr(Length (Nod) - GNodDCnt, 64) + BinStr(GEdgeCnt, 64) + GIDSFile;
                45: Eidon[High(Eidon)].Args := '';
                46: Eidon[High(Eidon)].Args := '[2E] Undoing to the Step ' + BinStr(GUndo, 64);
                47: Eidon[High(Eidon)].Args := '[2F] Redoing to the Step ' + BinStr(GUndo, 64);
                48: Eidon[High(Eidon)].Args := '';
                49: Eidon[High(Eidon)].Args := '';
                50: Eidon[High(Eidon)].Args := '';
                51: Eidon[High(Eidon)].Args := Consoleline;
                52: Eidon[High(Eidon)].Args := LeftStr(GIDSFile, Length(GIDSFile) - 3) + 'eid';
                53: Eidon[High(Eidon)].Args := LeftStr(GIDSFile, Length(GIDSFile) - 3) + 'eid';
                54: Eidon[High(Eidon)].Args := LeftStr(GIDSFile, Length(GIDSFile) - 3) + 'eid';
                55: Eidon[High(Eidon)].Args := LeftStr(GIDSFile, Length(GIDSFile) - 3) + 'eid';
                56: Eidon[High(Eidon)].Args := BinStr(Length(Eidon), 64) + LeftStr(GIDSFile, Length(GIDSFile) - 3) + 'eid';
                57: Eidon[High(Eidon)].Args := BinStr(Length(Eidon), 64) + LeftStr(GIDSFile, Length(GIDSFile) - 3) + 'eid';
                58: Eidon[High(Eidon)].Args := BinStr(GCNodFlg, 8);
                59: Eidon[High(Eidon)].Args := BinStr(GCNodFlg, 8) + BinStr(Nod[GCurrIdx].Flg, 8);
        end;
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	1.7. Proseychi - Process an interface call
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
function TApostolia.Proseychi(): integer;
var
	ConsoleLine: string;
        NodID, EType, BackCurrIdx, Back1TargIdx, Back2TargIdx: longint;
        CurrMsg, Mode: integer;
        LogFile: TFileStream;
begin
        Result := 51;
        ConsoleLine := Form.SCon.Lines[Form.SCon.Lines.Count - 2];
        if RightStr(LeftStr(ConsoleLine,36),1) = '[' then CurrMsg := StrToInt('$' + RightStr(LeftStr(ConsoleLine,38),2)) else CurrMsg := -1;
        AEMem(CurrMsg);
	case CurrMsg of
                -1: begin
                        ConsoleLine := RightStr(ConsoleLine, Length(ConsoleLine) - 38);
//			Result := 51;  			     			       			// [33] Processing console query
			Result := Ag.AParse(Consoleline);						// Sending Consoleline to parser
			AEMem(Result);
        	    end;
                00: begin
                        Result := ACreate;  	     	  	     		    			// [00] Base initialisation, reading Nodes from IDS file
                        if Result < 5 then Form.ALog(Result);
                        AEMem(Result);
                        Result := AERead; 		     						// Reading Eidetika from EID file
                        Form.ALog(Result);
                        AEMem(Result);
                        Result := 5;
                        AEMem(Result);
		    end;
		06: begin                                                                               // [06] Current node pointer update
                	if Form.CNodID.Text = '' then Form.CNodID.Text := '0';				// Current Node ID cannot be 'none'
			NodID := StrToInt(Form.CNodID.Text);
                	if NodID > GUpperID then begin	  	       	  				// Check for [07] ID oversize
                		Form.ALog(07);
                                AEMem(07);
                		NodID := 0;
                                Form.CNodID.Text := '0';
                	end
                	else if (Nod[Idx(NodID)].Flg and 16) > 0 then begin				// Check [08] Del flag
                		Form.ALog(08);
                                AEMem(08);
                		NodID := 0;
                                Form.CNodID.Text := '0';
                	end;
                	Result := Ag.AChNode(0,NodID); 	       	    					// Set new global pointer
                        AEMem(Result);
		    end;
                10: begin
                        Result := Ag.AEdit(0, Form.CNodCom.Text);					// [0A] Current node comment edit
                        AEMem(Result);
                    end;
		12: begin     		  								// [0C] Target node pointer update
        		if Form.TNodID.Text = '' then begin
                                NodID := -1;
                                GTargComment := '';
                                Form.ETp.Text := '';
                                Form.ETpTxt.Caption := 'Undefined';
                        end
                        else NodID := StrToInt(Form.TNodID.Text);
                	if NodID > GUpperID then begin	  	       	  				// Check for [0D] ID oversize
                		Form.ALog(13);
                                AEMem(13);
                		NodID := -1;
                                Form.TNodID.Text := '';
                	end
                	else if NodID > -1 then if (Nod[Idx(NodID)].Flg and 16) > 0 then begin		// Check [0E] Del flag
                		Form.ALog(14);
                                AEMem(14);
                		NodID := -1;
                                Form.TNodID.Text := '';
                	end
                        else if NodID = Nod[GCurrIdx].ID then begin	    	    	 		// Check for [0F] cyclic edge
                                Form.ALog(15);
                                AEMem(15);
                                NodID := -1;
                                Form.TNodID.Text := '';
			end;
                	Result := Ag.AChNode(1, NodID);	       	    					// Set new global pointer
                        AEMem(Result);
		    end;
		18: begin      	  									// [12] Edge type change
                        if Form.ETp.Text = '' then EType := -1
			else EType := StrToInt(Form.ETp.Text);
                        if EType > GUpperID then begin	 						// Check for [13] edge type oversize
                                Form.ALog(19);
                                AEMem(19);
                                Form.ETp.Text := '0';
                                EType := 0;
                        end;
                        Result := Ag.AChEType(EType);
                        AEMem(Result);
                        if EType < 0 then Form.ETpTxt.Caption := 'Undefined' else Form.ETpTxt.Caption := Nod[Idx(EType)].Com;
		    end;
                24: begin
                        Result := Ag.AEdit(1, GTargComment);	          	  		// [17] Target node comment edit
                        AEMem(Result);
//                        if Result = 26 then Form.TNodFlg.Caption := '00000001';
                    end;
		27, 28: begin  	       	    			    	       				// [1B] Add [1B] ascending or [1C] descending edge to a new or existing node
                        Result := 0;
                        if (((CurrMsg = 27) and (GCurrIdx = 0)) or ((CurrMsg = 28) and (GTargIdx = 0))) then Result := 29 // Check for [1D] no ascending edges allowed for Node 0
			   else begin
                		if Form.ETp.Text = '' then Form.ETp.Text := '0';
        			EType := StrToInt(Form.ETp.Text);
                                if EType > GUpperID then begin 						// Check for [13] edge type oversize
                                        Form.ALog(19);
                                        AEMem(19);
                                        Form.ETp.Text := '0';
                                        EType := 0;
                                end;
                                if CurrMsg = 27 then Mode := 0 else Mode := 1;
                                if GTargIdx = GCurrIdx then begin
                                        Form.ALog(15); 	    	    	      				// Check for [0F] cyclic edge error
                                        AEMem(15);
				end
				else if (GTargIdx < Length(Nod)) and (Nod[GTargIdx].Flg and 16 > 0) then
                                   begin
                                   	Form.ALog(31);		     			       	    	// Check [1F] Del flag
                                        AEMem(31);
				   end
				else begin
                                   if GTargIdx = -1 then Result := Ag.AAdd(Mode, EType, GTargComment) else Result := Ag.AAdd(Mode + 2, EType, GTargComment);
                                   AEMem(Result);
                                end;
                                if (Result > -1) and (Form.TNodID.Text = '') then
                                   begin
                                   	Form.ALog(32);		       	     	 			// Check for [20] new node
                                        AEMem(32);
				   end;
				if Result = -1 then Result := 30       	     	  			// Check for [1E] duplicate edge error
                                else begin
                                        GTargIdx := Result;
                                        if CurrMsg = 27 then Result := 33 else Result := 34;		// [21] ascending or [22] descending edge added
//                        		GTargComment := Form.TNodCom.Text;
                                end;
                	   end;
                        AEMem(Result);
                    end;
                35: begin   	    		     			       	      	 		// [23] Deleting node edge by edge
                        Result := 09;
                        if GCurrIdx = 0 then Result := 36						// [24] Node 0 cannot be deleted
                        else for GCurrEdge := High(Nod[GCurrIdx].Edg) downto 0 do begin
			     	GTargIdx := Idx(Nod[GCurrIdx].Edg[GCurrEdge].ID);
				GTargEdge := Length(Nod[GTargIdx].Edg);
				repeat Dec(GTargEdge) until Nod[GTargIdx].Edg[GTargEdge].ID = Nod[GCurrIdx].ID;
                                BackCurrIdx := GCurrIdx;
                                Back1TargIdx := GTargIdx;
                                Result := Ag.ADelete;
                                if Result = 37 then begin
                                        Back2TargIdx := GTargIdx;
                                        GTargIdx := BackCurrIdx;
                                        Form.ALog(37);	    				     		// [25] Node deleted (current)
                                        AEMem(37);
                                        GTargIdx := Back2TargIdx;
                                        Result := 09;
                                end;
                                if Result = 38 then begin
                                        Back2TargIdx := GTargIdx;
                                        GTargIdx := Back1TargIdx;
                                        Form.ALog(37);	    				     		// [25] Node deleted (target)
                                        AEMem(37);
                                        GTargIdx := Back2TargIdx;
                                        Result := 09;
                                end;
                        end;
                        AEMem(Result);
		    end;
                38: begin
                        BackCurrIdx := GCurrIdx;
                        Back1TargIdx := GTargIdx;
                        if GTargIdx < 0 then Result := 39 else Result := Ag.Adelete;			// [26] Deleting edge. Check for [27] There is no current Edge to delete
                        if Result = 37 then begin
                                Back2TargIdx := GTargIdx;
                                GTargIdx := BackCurrIdx;
                                Form.ALog(37);		       	      	 				// [25] Node deleted (current)
                                AEMem(37);
                                GTargIdx := Back2TargIdx;
                                Result := 09;
                        end;
                        if Result = 38 then begin
                                Back2TargIdx := GTargIdx;
                                GTargIdx := Back1TargIdx;
                                Form.ALog(37);		 						// [25] Node deleted (target)
                                AEMem(37);
                                GTargIdx := Back2TargIdx;
                                Result := 09;
                        end;
                        AEMem(Result);
		    end;
		41: begin
                        Result := Ag.ACommit;    		 					// [29] Commiting base
                        AEMem(Result);
                    end;
                43: begin     										// [2B] Save base
                        Ap.AWrite;									// Pour Nod array to IDS file
                        Form.ALog(44);									// [2C] Database saved
                        AEMem(44);
                        Ap.AEWrite;									// Pour Eidetika to EID file
                        Form.ALog(57);									// [39] Events Strip saved
                        AEMem(57);
                	if not(FileExists('Pyramidion 016.log')) then FileCreate('Pyramidion 016.log');	// If no log file exist then new empty log file created
        		LogFile := TFileStream.Create('Pyramidion 016.log', fmOpenReadWrite);
                	Form.ALog(45);                                                                  // [2D] Session log saved to file "Pyramidion 016.log", session closed
                        AEMem(45);
                        if Form.SCon.Lines.Count > 1 then Form.SCon.Lines.Delete(Form.SCon.Lines.Count - 1);
                	with LogFile do begin                                                           // Note: on event [2D] session log is saved by the Form itself,
                		Seek(0, SoEnd);                                                         // Apostolia just frees Nod instance
                		for NodID := 1 to Length(Form.SCon.Caption) do WriteByte(Byte(Form.SCon.Caption[NodID]));
                		Destroy;
                	end;
                        ADestroy;
		    end;
		48: begin
                        AEMem(48);
                        Result := 48;	       	  			       				// [30] Session close without saving log and database
                    end;
		49: begin
                        Result := 50;									// [31] 'Monitor completed' event
                        AEMem(Result);
                        Form.AMonitor;
		    end;
                58: begin
                        Result := 59;									// [3B] 'Current node flags edited' event
                        AEMem(Result);
		    end;
	end;
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	1.8. Idx - Return index of a node by its ID (binary search in sorted list)
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
function TApostolia.Idx(InputID: longint): longint;
var
        Cnt, Lo, Mid, Hi: longint;
begin
     	Result := -1;
	Lo := 0;
	Hi := High(Nod);
	Mid := Hi div 2;
        repeat
	  	if Nod[Mid].ID >= InputID then Hi := Mid else Lo := Mid;
	    	Mid := Lo + ((Hi - Lo) div 2);
	until Hi - Lo <= 32;                                                                            // 32 is deliberate value, needs to be set experimentally
      	Cnt := Lo - 1;
	repeat Inc(Cnt) until ((Nod[Cnt].ID = InputID) or (Cnt > Hi));
        if Cnt >= Length(Nod) then Result := -1 else Result := Cnt;
end;

//==================================================================================================================================================================================
//	LEVEL 2. BASIC PYRAMIDION MECHANICS (Add, Delete, Edit, Commit)
//==================================================================================================================================================================================
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	2.0. AAdd - Add a node or an edge
//		Input:
//			Mode:	byte			0 = add ascending edge and new node;
//							1 = add descending edge and new node;
//							2 = add ascending edge to existing node;
//							3 = add descending edge to existing node.
//			EdgTp:	longint			Edge type (ID of the property representing node);
//			Cmt:	rawbytestring		Comment for the new node/edge (name not to mistake for Nod.Com).
//	{Global}	GCurrIdx:	longint		Current node ID (i.e. Nod array index), sent via global variable GCurrIdx.
//	{Global}	GTargIdx:	longint		Target node ID, sent via global variable GTargIdx (equals to -1 if new node is to add).
//
//	Output:	added/target node index			Successful execution or -1 if adding fails.
//
//	Note 1:	There is no upper limit check for Nod index here because obviously 64-bit is quite enough for this alpha version. Even the final release
//		version or the Pyramidion DBMS will hardly beat upper limit of 2^64 Nodes.
//	Note 2: Quick way to handle orphans: link each newly added node to Node 0 (undeletable), so no orphans will ever appear.
//	     	This also corresponds to the Pyramidion ideology and Circus Lucius concept.
//		Sure this edge to Node 0 is the last one to delete, only when the node itself is to be deleted.
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
function TAngeliophor.AAdd(Mode: byte; EdgTp: longint; Cmt: rawbytestring): longint;
var
	TEd, CntE: longint;
	Swap: boolean;
	CED: SID;
begin
        Result := 0;  	       	    	      	      	     						// Initialise Result variable
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	//	Adding new node with descending or ascending edge to the current node
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	if Mode < 2 then begin
                //------------------------------------------------------------------------------------------------------------------------------------------------------------------
                // Create new node
                GTargIdx := Length(Nod);
		SetLength(Nod, GTargIdx + 1);								// Now TID = High(Nod)
                Inc(GUpperID);
                Nod[GTargIdx].ID := GUpperID;
                //------------------------------------------------------------------------------------------------------------------------------------------------------------------
                // Prevent orphan appearing
		SetLength(Nod[0].Edg, Length(Nod[0].Edg) + 1);						// Add descending edge from the Node 0
		Nod[0].Edg[High(Nod[0].Edg)].ID := GUpperID;						// Point to the new node
		Nod[0].Flg := Nod[0].Flg or 2;	 		    					// Raise Dsc flag at the Node 0
		Nod[0].Edg[High(Nod[0].Edg)].Flg := 1;							// Raise Dsc flag at the Node 0 last edge .Flg
                Inc(GEdgeCnt);			    							// Update global edge counter
                //------------------------------------------------------------------------------------------------------------------------------------------------------------------
		// Setting newly added node
                if GCurrIdx > 0 then begin   								// Check to avoid duplicate link to Node 0
			SetLength(Nod[GTargIdx].Edg, 2);						// Note: Nod[GTargIdx].Edg[0].ID, .Flg and .ETp are set to 0 by default
			Nod[GTargIdx].Edg[1].ID := Nod[GCurrIdx].ID;
			Nod[GTargIdx].Edg[1].ETp := EdgTp;
			Nod[GTargIdx].Com := Cmt;
			GCurrEdge := Length(Nod[GCurrIdx].Edg);
			SetLength(Nod[GCurrIdx].Edg, GCurrEdge + 1);
			Nod[GCurrIdx].Edg[GCurrEdge].ID := GUpperID;
                        Nod[GCurrIdx].Edg[GCurrEdge].ETp := EdgTp;
                        Inc(GEdgeCnt);		    							// Update global edge counter
                        //----------------------------------------------------------------------------------------------------------------------------------------------------------
                        // Setting flags
        		if Mode = 0 then begin		    	    					// Ascending edge to new node
        			if Cmt = '' then Nod[GTargIdx].Flg := 6 else Nod[GTargIdx].Flg := 7;		// Raise Asc, Dsc & Com flags at the target (new) node
        			Nod[GTargIdx].Edg[1].Flg := Nod[GTargIdx].Edg[1].Flg or 1;     			// Edge from the target node (raise Edge.Dsc flag at the target side)
        			Nod[GCurrIdx].Flg := Nod[GCurrIdx].Flg or 4;					// Raise Asc flag at the current node
        			Nod[GCurrIdx].Edg[GCurrEdge].Flg := Nod[GCurrIdx].Edg[GCurrEdge].Flg and 254;	// Edge to the current node (lower Edge.Dsc flag at the current side)
        		end
        		else begin				    				     	// Descending edge to new node
        			if Cmt = '' then Nod[GTargIdx].Flg := 4 else Nod[GTargIdx].Flg := 5;		// Raise Asc & Com flags at the target node
        			Nod[GTargIdx].Edg[1].Flg := Nod[GTargIdx].Edg[1].Flg and 254;			// Edge to the target node (lower Edge.Dsc flag at the target side)
        			Nod[GCurrIdx].Flg := Nod[GCurrIdx].Flg or 2;					// Raise Dsc flag at the current node
        			Nod[GCurrIdx].Edg[GCurrEdge].Flg := Nod[GCurrIdx].Edg[GCurrEdge].Flg or 1;     	// Edge from the current node (raise Edge.Dsc flag at the current side)
        		end;
		end
		else begin
                        SetLength(Nod[GTargIdx].Edg, 1);						// Note: Nod[GTargIdx].Edg[0].ID, .Flg and .ETp are set to 0 by default
			Nod[GTargIdx].Edg[0].ID := 0;
			Nod[GTargIdx].Edg[0].ETp := EdgTp;
			Nod[GTargIdx].Com := Cmt;
                        GCurrEdge := High(Nod[0].Edg);
        		Nod[0].Edg[GCurrEdge].ETp := EdgTp;
                        // Setting flags (note: no ascending edges for Node 0)
       			if Cmt = '' then Nod[GTargIdx].Flg := 4 else Nod[GTargIdx].Flg := 5;		// Raise Asc & Com flags at the target node
       			Nod[GTargIdx].Edg[0].Flg := 0;	      	     		       			// Edge to the target node (lower Edge.Dsc flag at the target side)
		end;
		Result := GTargIdx;
	end
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	//	Adding ascending or descending edge between the current node and existing target node
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	else begin
		for TEd := 0 to High(Nod[GCurrIdx].Edg) do
                        if Nod[GCurrIdx].Edg[TEd].ID = Nod[GTargIdx].ID then Result := -1;		// Check for duplicate edge
		if Result = 0 then begin
			TEd := Length(Nod[GTargIdx].Edg);
			SetLength(Nod[GTargIdx].Edg, TEd + 1);
			Nod[GTargIdx].Edg[TEd].ID := Nod[GCurrIdx].ID;
			Nod[GTargIdx].Edg[TEd].ETp := EdgTp;
                        GCurrEdge := Length(Nod[GCurrIdx].Edg);
			SetLength(Nod[GCurrIdx].Edg, GCurrEdge + 1);
			Nod[GCurrIdx].Edg[GCurrEdge].ID := Nod[GTargIdx].ID;
                        Inc(GEdgeCnt);		    							// Update global edge counter
                        //----------------------------------------------------------------------------------------------------------------------------------------------------------
                        // Setting flags
			if Mode = 2 then begin		   		     	       			// Ascending edge to existing node
				Nod[GTargIdx].Flg := Nod[GTargIdx].Flg or 2;				// Raise Dsc flag at the target node
				Nod[GTargIdx].Edg[TEd].Flg := Nod[GTargIdx].Edg[TEd].Flg or 1; 		// Edge to the current node (raise Edge.Dsc flag at the target side)
				Nod[GCurrIdx].Flg := Nod[GCurrIdx].Flg or 4;				// Raise Asc flag at the current node
				Nod[GCurrIdx].Edg[GCurrEdge].Flg := Nod[GCurrIdx].Edg[GCurrEdge].Flg and 254; // Edge to the current node (lower Edge.Dsc flag at the current side)
			end
			else begin				    				     	// Descending edge to existing node
				Nod[GTargIdx].Flg := Nod[GTargIdx].Flg or 4;				// Raise Asc flag at the target node
				Nod[GTargIdx].Edg[TEd].Flg := Nod[GTargIdx].Edg[TEd].Flg and 254;	// Edge to the target node (lower Edge.Dsc flag at the target side)
				Nod[GCurrIdx].Flg := Nod[GCurrIdx].Flg or 2;				// Raise Dsc flag at the current node
				Nod[GCurrIdx].Edg[GCurrEdge].Flg := Nod[GCurrIdx].Edg[GCurrEdge].Flg or 1;    // Edge from the current node (raise Edge.Dsc flag at the current side)
			end;
			Nod[GCurrIdx].Edg[GCurrEdge].ETp := EdgTp;
			Result := GTargIdx;
		end;
	end;
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	//	Sorting newly added edges for the current and target node
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	//	Note: Simpliest bubble sort is used so far. For big numbers the quick sort algorithm will be implemented. The size of 'big' is to be defined experimentally.
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	if Result >= 0 then if Length(Nod[GCurrIdx].Edg) > 1 then begin
		repeat
		Swap := False;
		for CntE := 0 to High(Nod[GCurrIdx].Edg) - 1 do
			if Nod[GCurrIdx].Edg[CntE].ID > Nod[GCurrIdx].Edg[CntE + 1].ID then begin
				CED := Nod[GCurrIdx].Edg[CntE];
				Nod[GCurrIdx].Edg[CntE] := Nod[GCurrIdx].Edg[CntE + 1];
				Nod[GCurrIdx].Edg[CntE + 1] := CED;
				Swap := True;
			end;
		until not Swap;
		if Length(Nod[GTargIdx].Edg) > 1 then begin
			repeat
			Swap := False;
			for CntE := 0 to High(Nod[GTargIdx].Edg) - 1 do
				if Nod[GTargIdx].Edg[CntE].ID > Nod[GTargIdx].Edg[CntE + 1].ID then begin
					CED := Nod[GTargIdx].Edg[CntE];
					Nod[GTargIdx].Edg[CntE] := Nod[GTargIdx].Edg[CntE + 1];
					Nod[GTargIdx].Edg[CntE + 1] := CED;
					Swap := True;
				end;
			until not Swap;
		end;
	end;
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	//	Setting global edge pointers
        if Result >= 0 then if Length(Nod[GCurrIdx].Edg) > 1 then begin
                GCurrEdge := -1;
                repeat Inc(GCurrEdge) until Nod[GCurrIdx].Edg[GCurrEdge].ID = Nod[GTargIdx].ID;
	end
        else GCurrEdge := 0;
        if Result >= 0 then if Length(Nod[GTargIdx].Edg) > 1 then begin
                GTargEdge := 0;
                repeat Inc(GTargEdge) until Nod[GTargIdx].Edg[GTargEdge].ID = Nod[GCurrIdx].ID;
	end
        else GTargEdge := 0;
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	2.1. ADelete - Delete an edge
//		 Input: current and target node/edge indexes sent via global GCurrIdx, GTargIdx, GCurrEdge, GTargEdge variables;
//		 Output: 0 if success, -1 if no edge found (i.e. if GTargIdx = -1).
//
//	Note:	The function marks as deleted targeted edge from Edg arrays of both current and target nodes. To prevent orphaned nodes generation, each node is linked to Node 0.
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
function TAngeliophor.ADelete(): integer;
var
	DelEdge, Cnt, AscCnt, DscCnt: longint;
        CurrNodDel, TargNodDel: boolean;
begin
        if GTargIdx < 0 then Result := 39 else Result := 9;						// Initialise Result to [28] No current Edge to delete or to [09] Success
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	// Check whether current and target nodes have just one undeleted edge left
	DelEdge := 0;
	CurrNodDel := False;	     	    	       	    		       	   			// Check for [29] Edge to Node 0 must be the last edge to delete
	for Cnt:= High(Nod[GCurrIdx].Edg) downto 0 do if Nod[GCurrIdx].Edg[Cnt].Flg and 2 > 0 then Inc(DelEdge);
	if Length(Nod[GCurrIdx].Edg) - DelEdge = 1 then CurrNodDel := True else if ((GCurrEdge = 0) and (GCurrIdx >0)) then Result := 40;
	DelEdge := 0;
	TargNodDel := False;
	for Cnt:= High(Nod[GTargIdx].Edg) downto 0 do if Nod[GTargIdx].Edg[Cnt].Flg and 2 > 0 then Inc(DelEdge);
	if Length(Nod[GTargIdx].Edg) - DelEdge = 1 then TargNodDel := True else if ((GTargEdge = 0) and (GTargIdx >0)) then Result := 40;
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	// Deleting edge and/or nodes
        if ((Result = 9) and (Nod[GCurrIdx].Edg[GCurrEdge].Flg and 2 = 0)) then begin
	        //------------------------------------------------------------------------------------------------------------------------------------------------------------------
		// Deleting edge at the current node side (and the node as well if there no more edges)
	        if CurrNodDel then begin
                        if GCurrIdx > 0 then Nod[GCurrIdx].Flg := Nod[GCurrIdx].Flg or 16;	       	// Raise Del flag at the current node, except for Node 0 of course
			if GCurrIdx > 0 then Inc(GNodDCnt); 		      				// Increment global deleted nodes counter, except for Node 0
                        Result := 37; 	     								// [25] Node deleted
		end;
		Nod[GCurrIdx].Edg[GCurrEdge].Flg := Nod[GCurrIdx].Edg[GCurrEdge].Flg or 2;		// Raise Edg.Del flag
		//------------------------------------------------------------------------------------------------------------------------------------------------------------------
		// Deleting edge at the target node side (and the node as well if there no more edges)
		if TargNodDel then begin
		   	if GTargIdx > 0 then Nod[GTargIdx].Flg := Nod[GTargIdx].Flg or 16;		// Raise Del flag at the target node, except for Node 0 of course
			if GTargIdx > 0 then Inc(GNodDCnt);    	 		      			// Increment global deleted nodes counter, except for Node 0
                        Result := 38; 	     								// [25] Node deleted
		end;
		Nod[GTargIdx].Edg[GTargEdge].Flg := Nod[GTargIdx].Edg[GTargEdge].Flg or 2;		// Raise Edg.Del flag
		//------------------------------------------------------------------------------------------------------------------------------------------------------------------
		// Lower Asc or Dsc flags or both:
		//------------------------------------------------------------------------------------------------------------------------------------------------------------------
		// For the current node
		if CurrNodDel then Nod[GCurrIdx].Flg := Nod[GCurrIdx].Flg and 249
                else begin
		     	AscCnt := 0;
			DscCnt := 0;									// Adjust Asc & Dsc flags
			for Cnt := 0 to High(Nod[GCurrIdx].Edg) do if (Nod[GCurrIdx].Edg[Cnt].Flg and 2) = 0 then if (Nod[GCurrIdx].Edg[Cnt].Flg and 1) > 0
			    	then Inc(DscCnt) else Inc(AscCnt);			      	     	// If Edge.Dsc flag = 1 then it's descending edge
			if AscCnt = 0 then Nod[GCurrIdx].Flg := Nod[GCurrIdx].Flg and 251;		// Lower Asc flag
			if DscCnt = 0 then Nod[GCurrIdx].Flg := Nod[GCurrIdx].Flg and 253;		// Lower Dsc flag
		end;
		//------------------------------------------------------------------------------------------------------------------------------------------------------------------
		// For the target node
		if TargNodDel then Nod[GTargIdx].Flg := Nod[GTargIdx].Flg and 249
		else begin
		     	AscCnt := 0;
			DscCnt := 0;									// Adjust Asc & Dsc flags
			for Cnt := 0 to High(Nod[GTargIdx].Edg) do if (Nod[GTargIdx].Edg[Cnt].Flg and 2) = 0 then if (Nod[GTargIdx].Edg[Cnt].Flg and 1) > 0
		    	    	then Inc(DscCnt) else Inc(AscCnt);    				       	// If Edge.Dsc flag = 1 then it's descending edge
			if AscCnt = 0 then Nod[GTargIdx].Flg := Nod[GTargIdx].Flg and 251;		// Lower Asc flag
			if DscCnt = 0 then Nod[GTargIdx].Flg := Nod[GTargIdx].Flg and 253;		// Lower Dsc flag
		end;
		//------------------------------------------------------------------------------------------------------------------------------------------------------------------
		// Adjust globals
		Dec(GEdgeCnt);	 									// Decrement global edge counter
		Inc(GEdgDCnt);										// Increment global deleted edge counter
		if CurrNodDel then GCurrIdx := 0;	    	    	 				// Set global current node pointer to Node 0
		if CurrNodDel then GCurrEdge := -1;							// Set global current edge pointer to none also
		GTargIdx  := -1;       		   							// Set global target node pointer to none
		GTargEdge := -1;									// Set global target edge pointer to none
        end;
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	2.2. AEdit - Edit Comment of Pleximo element
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	Note: If Mode = 0 then the current node comment is to change, otherwise it's the target node comment to change
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
function TAngeliophor.AEdit(Mode: byte; Cmt: rawbytestring): integer;
var
	CIdx: longint;
begin
        if GTargIdx > -1 then begin
                if Mode = 0 then CIdx := GCurrIdx else CIdx := GTargIdx;
                if Cmt <> '' then Nod[CIdx].Flg := Nod[CIdx].Flg or 1					// Raise Comment flag
                else Nod[CIdx].Flg := Nod[CIdx].Flg and 254;						// Lower Comment flag
                Nod[CIdx].Com := Cmt;
                if Mode = 0 then Result := 11 else Result := 25;
	end
	else Result := 26;
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	2.3. AChNode - Change current node pointer
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
function TAngeliophor.AChNode(Mode: byte; NodID: longint): integer;
var	 Cnt: longint;
begin
	if Mode = 0 then begin
                GCurrIdx := Ap.Idx(NodID);
                GCNodFlg := Nod[GCurrIdx].Flg;
		GTargIdx := -1;
		GCurrEdge := -1;
		GTargEdge := -1;
        	Result := 09;
        end
        else begin
                if NodID > -1 then GTargIdx := Ap.Idx(NodID) else GTargIdx := -1;
		GCurrEdge := -1;
                GTargEdge := -1;
		if NodID > -1 then begin
                        if Length(Nod[GCurrIdx].Edg) > 0 then for Cnt := 0 to High(Nod[GCurrIdx].Edg) do if (Nod[GCurrIdx].Edg[Cnt].ID = NodID) then GCurrEdge := Cnt;
                        if Length(Nod[GTargIdx].Edg) > 0 then for Cnt := 0 to High(Nod[GTargIdx].Edg) do if (Nod[GTargIdx].Edg[Cnt].ID = Nod[GCurrIdx].ID) then GTargEdge := Cnt;
		end;
		if NodID = -1 then Result := 16 else Result := 17;
        end;
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	2.4. AChEType - Change edge type
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
function TAngeliophor.AChEType(EType: longint): integer;
begin
        if GCurrEdge > -1 then begin
                if EType < 0 then EType := 0;
                Nod[GCurrIdx].Edg[GCurrEdge].ETp := EType;
                if (Nod[GCurrIdx].Edg[GCurrEdge].Flg and 1) = 0 then Result := 21 else Result := 22;			// [15] Ascending or [16] descending edge type change
        end
        else if GTargIdx < 0 then Result := 20	     	      	     	    						// [14] empty Target Node ID field
        else Result := 23;			     	      	     	       	       	      	 			// [17] Current Node has no edge with Target Node. New edge?
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	2.5. ACommit - Remove deleted/orphaned nodes/sags from the Nod array, reset changes history for the session
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
function TAngeliophor.ACommit(): integer;
var
	SrcNod, SrcEdg, TrgNod, TrgEdg: longint;
	TmpNod: array of ANode;
begin
        SetLength(TmpNod, 0);
        if Length(Nod) > 1 then for SrcNod := 0 to High(Nod) do if Nod[SrcNod].Flg and 16 = 0 then begin                // If the node is not marked as deleted then copy it
	   	SetLength(TmpNod, Length(TmpNod) + 1);
                TrgNod := High(TmpNod);
                TmpNod[TrgNod] := Nod[SrcNod];
                SetLength(TmpNod[TrgNod].Edg, 0);
                for SrcEdg := 0 to High(Nod[SrcNod].Edg) do if (Nod[SrcNod].Edg[SrcEdg].Flg and 2) = 0 then begin	// If the edge is not marked as deleted then copy it
                        SetLength(TmpNod[TrgNod].Edg, Length(TmpNod[TrgNod].Edg) + 1);
                        TrgEdg := High(TmpNod[TrgNod].Edg);
                        TmpNod[TrgNod].Edg[TrgEdg] := Nod[SrcNod].Edg[SrcEdg];
                end;
        end;
        SetLength(Nod, 0);
        Nod := Copy(TmpNod);	      	     	      			      	       	    	       	    		// Finally, replace the Nod array with the committed copy
        GNodDCnt := 0;
        GEdgDCnt := 0;
        Result := 42;
end;

//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
//	2.6. AParse - Parse strings (of any origin) to abstract syntactic trees corresonding to the content of the Antikeymena knowledge base
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
// >>>>>>>>>> Forgot to put check for deleted nodes/sags! <<<<<<<<<<
function TAngeliophor.AParse(InputStr: rawbytestring): longint;
var
	Cnt1, Cnt2, Cnt3, Cnt4: longint;
        FStr: rawbytestring;
        FByte: char;
        AllSymbols: array of ObjRef;
	PrsSymbols: array of longint;									// Identified symbols array
        ArObj: array[0..5] of longint;									// Array for arithmetic objects IDs, namely:
        	    		   									// Arithmetica, Numerus, Comma, Operatio,
        												// Parenthesis Anterioris, Parenthesis Posterioris
	PrsInput: array of ObjRef;									// Identified objects array
        SitID: integer;	   										// Situation ID to handle symbol processing rules
	PointFlg: boolean;										// Digital point flag
        BrckCnt: integer;										// Braqckets counter
begin
        Result := 51;
        //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        // Filtering input string for allowed symbols
        FStr := '';
        for Cnt1 := 1 to High(InputStr) do begin
		FByte := InputStr[Cnt1];
                if not (FByte in ['0'..'9', '.', ',', '(', ')', '+', '-', '/', '*']) then FByte := #0;	// 18 symbols are allowed yet
                if FByte <> #0 then FStr := FStr + FByte;
        end;
        //--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        // Linking allowed symbols to symbol nodes
        Cnt1 := 0;
//        repeat Inc(Cnt1) until ((Nod[Cnt1].Com = 'Symboli') or (Cnt1 > High(Nod)));
	SetLength(AllSymbols, 0);
//        for Cnt2 := 0 to High(Nod[Cnt1].Edg) do
//                if (Nod[Cnt1].Edg[Cnt2].ETp = Nod[Cnt1].ID) then begin
//                        SetLength(AllSymbols, Length(AllSymbols) + 1);
//                        AllSymbols[High(AllSymbols)].ObjID := Nod[Cnt1].Edg[Cnt2].ID;
//                        AllSymbols[High(AllSymbols)].ObjVal := Nod[Ap.Idx(Nod[Cnt1].Edg[Cnt2].ID)].Com;
//                end;
	SetLength(PrsSymbols, 0);
	for Cnt1 := 0 to High(FStr) do begin
                for Cnt2 := 0 to High(AllSymbols) do if FStr[Cnt1] = AllSymbols[Cnt2].ObjVal then begin
                                SetLength(PrsSymbols, Length(PrsSymbols) + 1);
                                PrsSymbols[High(PrsSymbols)] := AllSymbols[Cnt2].ObjID;
                                Break;
                        end;
                end;
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
        // Getting references to arithmetic objects
        Cnt1 := 0;
        repeat Inc(Cnt1) until ((Nod[Cnt1].Com = 'Arithmetica') or (Cnt1 > High(Nod)));
        ArObj[0] := Ap.Idx(Nod[Cnt1].ID);
        for Cnt1 := 0 to High(Nod[ArObj[0]].Edg) do if Nod[Ap.Idx(Nod[ArObj[0]].Edg[Cnt1].ID)].Com = 'Numerus' then begin
                ArObj[1] := Nod[Ap.Idx(Nod[ArObj[0]].Edg[Cnt1].ID)].ID;
                Break;
        end;
	for Cnt1 := 0 to High(Nod[ArObj[0]].Edg) do if Nod[Ap.Idx(Nod[ArObj[0]].Edg[Cnt1].ID)].Com = 'Comma' then begin
                ArObj[2] := Nod[Ap.Idx(Nod[ArObj[0]].Edg[Cnt1].ID)].ID;
                Break;
	end;
	for Cnt1 := 0 to High(Nod[ArObj[0]].Edg) do if Nod[Ap.Idx(Nod[ArObj[0]].Edg[Cnt1].ID)].Com = 'Operatio' then begin
                ArObj[3] := Nod[Ap.Idx(Nod[ArObj[0]].Edg[Cnt1].ID)].ID;
                Break;
	end;
	for Cnt1 := 0 to High(Nod[ArObj[0]].Edg) do if Nod[Ap.Idx(Nod[ArObj[0]].Edg[Cnt1].ID)].Com = 'Parenthesis Anterioris' then begin
                ArObj[4] := Nod[Ap.Idx(Nod[ArObj[0]].Edg[Cnt1].ID)].ID;
                Break;
	end;
	for Cnt1 := 0 to High(Nod[ArObj[0]].Edg) do if Nod[Ap.Idx(Nod[ArObj[0]].Edg[Cnt1].ID)].Com = 'Parenthesis Posterioris' then begin
                ArObj[5] := Nod[Ap.Idx(Nod[ArObj[0]].Edg[Cnt1].ID)].ID;
                Break;
	end;
	//--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	// Processing situations and corresponding rules for each arithmetic object
	SetLength(PrsInput, 0);
	PointFlg := False;
	BrckCnt := 0;
	for Cnt3 := 0 to High(PrsSymbols) do begin
                // Defining situations IDs
                //------------------------------------------------------------------------------------------------------------------------------------------------------------------
                SitID := 0;
                if Length(PrsInput) > 0 then
                if PrsInput[High(PrsInput)].ObjID = ArObj[1] then SitID := 1
                else if PrsInput[High(PrsInput)].ObjID = ArObj[2] then SitID := 2
                else if PrsInput[High(PrsInput)].ObjID = ArObj[3] then SitID := 3
                else if PrsInput[High(PrsInput)].ObjID = ArObj[4] then SitID := 4
                else if PrsInput[High(PrsInput)].ObjID = ArObj[5] then SitID := 5;
                Cnt4 := -1;
                repeat Inc(Cnt4) until ((Nod[Ap.Idx(PrsSymbols[Cnt3])].Edg[Cnt4].ETp = Nod[ArObj[0]].ID) or
                       (Cnt4 = Length(Nod[Ap.Idx(PrsSymbols[Cnt3])].Edg)));	       			// Look for symbol sag typed as 'Arithmetica', there must be only one
                // If current symbol is a digit
                //------------------------------------------------------------------------------------------------------------------------------------------------------------------
                if Nod[Ap.Idx(Nod[Ap.Idx(PrsSymbols[Cnt3])].Edg[Cnt4].ID)].ID = ArObj[1] then begin	// If the current symbol type is 'Numerus'
                        case SitID of
                        0, 3, 4:	begin
                        		SetLength(PrsInput, Length(PrsInput) + 1);
                        		PrsInput[High(PrsInput)].ObjID := ArObj[1]; 	     	  	// 'Numerus' node ID
                        		PrsInput[High(PrsInput)].ObjVal := Nod[Ap.Idx(PrsSymbols[Cnt3])].Com;
                        	end;
                        1, 2:	PrsInput[High(PrsInput)].ObjVal :=  PrsInput[High(PrsInput)].ObjVal + Nod[Ap.Idx(PrsSymbols[Cnt3])].Com;
                        else	Result := -1;
			end;
		end;
                // If current symbol is digital point or comma
                //------------------------------------------------------------------------------------------------------------------------------------------------------------------
                if Nod[Ap.Idx(Nod[Ap.Idx(PrsSymbols[Cnt3])].Edg[Cnt4].ID)].ID = ArObj[2] then begin	// If the current symbol type is 'Comma'
                        PointFlg := False;
                        case SitID of
			1:	if not PointFlg then begin
                                	PrsInput[High(PrsInput)].ObjVal :=  PrsInput[High(PrsInput)].ObjVal + Nod[Ap.Idx(PrsSymbols[Cnt3])].Com;
                        		PointFlg := True;
                        	end
                        	else Result := -1;
                        else	Result := -1;
			end;
		end;
                // If current symbol is an arithmetic operation symbol
                //------------------------------------------------------------------------------------------------------------------------------------------------------------------
                if Nod[Ap.Idx(Nod[Ap.Idx(PrsSymbols[Cnt3])].Edg[Cnt4].ID)].ID = ArObj[3] then begin	// If the current symbol type is 'Operatio'
                        case SitID of
                        1, 5:	begin
                        		SetLength(PrsInput, Length(PrsInput) + 1);
                        		PrsInput[High(PrsInput)].ObjID :=  ArObj[3];  	    		// 'Operatio' node ID
                        		PrsInput[High(PrsInput)].ObjVal :=  Nod[Ap.Idx(PrsSymbols[Cnt3])].Com;
                        	end;
                        else	Result := -1;
			end;
		end;
                // If current symbol is the left bracket
                //------------------------------------------------------------------------------------------------------------------------------------------------------------------
                if Nod[Ap.Idx(Nod[Ap.Idx(PrsSymbols[Cnt3])].Edg[Cnt4].ID)].ID = ArObj[4] then begin	// If the current symbol type is 'Parenthesis Anterioris'
                        case SitID of
                        0, 3, 4:begin
                                	SetLength(PrsInput, Length(PrsInput) + 1);
                                        PrsInput[High(PrsInput)].ObjID := ArObj[4];    			// 'Parenthesis Anterioris' node ID
                        		PrsInput[High(PrsInput)].ObjVal := Nod[Ap.Idx(PrsSymbols[Cnt3])].Com;
                        		Inc(BrckCnt);
                        	end;
                        else	Result := -1;
			end;
		end;
                // If current symbol is the right bracket
                //------------------------------------------------------------------------------------------------------------------------------------------------------------------
                if Nod[Ap.Idx(Nod[Ap.Idx(PrsSymbols[Cnt3])].Edg[Cnt4].ID)].ID = ArObj[5] then begin	// If the current symbol type is 'Parenthesis Posterioris'
                        case SitID of
                        1, 5:	begin
                                	SetLength(PrsInput, Length(PrsInput) + 1);
                                        PrsInput[High(PrsInput)].ObjID := ArObj[5];   			// 'Parenthesis Posterioris' node ID
                        		PrsInput[High(PrsInput)].ObjVal := Nod[Ap.Idx(PrsSymbols[Cnt3])].Com;
                        		Dec(BrckCnt);
                        	end;
                        else	Result := -1;
			end;
		end;
	end;
        if BrckCnt <> 0 then Result := -1;                                                              // Check for unpaired brackets}
end;
//----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

end.
