
//==================================================================================================================================================
//  ROOT OF PYRAMIDION 0.19 IDS FILE
//--------------------------------------------------------------------------------------------------------------------------------------------------
//  (c) 2023 Igor Voloshin ivoloshin@hotmail.com
//==================================================================================================================================================
//  This is the explanation of the database file format for Pyramidion NoSQL DBMS version 0.19 (alpha). Total length of the initial IDS file
//  makes 41 bytes. The IDS file presence is checked at the Pyramidion program start, and in case of its absence this initial 41-byte file is
//  generated and written to disk for further use.
//
//  #	Size	Value	Field name	Field meaning
//--------------------------------------------------------------------------------------------------------------------------------------------------
//  1.	QWord	$XX	Version		Version header 'IDS 0.19', which corresponds to unsigned 64 bit integer 4121125918256350281, recorded
//					in file in reverse byte order (Intel mode);
//  2.	Byte	$04	HdrLen		Length of the header string limited to 255 ASCII characters;
//  3.	String	'BA&A'	Header		Readable ASCII string header with base name and short description. Initially 'BA&A' (Basic Algorythms &
//					Adventors), which corrersponds to unsigned 32 bit integer 1093026114, recorded in file in reverse
//					byte order (Intel mode);
//  4.	DWord	$YY	CRC32		32-bit checksum for the remainder of the file, represented as unsigned 64-bit integer, initially 382232240;
//
//  The remainder of the IDS file is packed using LZW algorithm
//--------------------------------------------------------------------------------------------------------------------------------------------------
//  5.	QWord	$01	RecCnt		Records count, 64 bit - initially, only Informatio Node exists;
// *6.	QWord	$00	ID		Information Node ID is 0;
//  7.	Byte	$01	Flags		Flags bits initially are b00100000 (see Annex A for flags description);
//  8.!	QWord	$00	EdgeCnt		No ascending edges for the Informatio Node ever, and no descending Edges so far;
//  9.!	17xByte	$00	EdgeRef		No list of edges so far (see Annex B for Edge Reference format);
// 10.!	QWord	$00	Adv		Adventor field - UInt64 pointer to the Eidetika array element with external data,
//					may contain binary data as well - node formulae, images, sounds, video, binary code etc. etc.;
// 11.!	Word	$0000	FrmLen		Lenght of the node Formula field - UInt16, i.e. upto 65535 symbols;
// 12.!	String	''	Frm		Formula field for parcing by adventors, contains combinatory symbols and data pointers. Initially empty.
// 13.	Word	$000A	NmeLen		Lenght of the Name field - UInt16, i.e. upto 65535 symbols;
// 14.	String	'Informatio' Nme	Node Name field - syntactic sugar for a human operator;
//------------
//  Note (*):	Glodal knowledge base identifier (BID) is integrated to the node ID, 2 upper bytes are reserved to identify separate IDS
//		knowledge base file. BID can be extracted from a node ID by logical operations: ID and $FFFF000000000000 shr 48.
//  Note (!):	Fields 8-12 are absent in the initial record, as defined by the corresponding bits of the Flags field (see Annex A).
//
//==================================================================================================================================================
//  Annex A. Node Flags Field Format
//==================================================================================================================================================
//  Each Node has one-byte Flags field to define its structure and behaviour, as follows:
//
//    Bit No.	Bit Meaning								   Bit Mask
//--------------------------------------------------------------------------------------------------------------------------------------------------
//	0	Sup flag ― node is superior to the current node if 1, not if 0		10000000 = 128
//	1	Lnk flag ― node is linked to the current node if 1, not if 0		01000000 =  64
//	2	Nme flag ― a non―empty nodename field present if 1 (else if 0)		00100000 =  32
//	3	Adv flag ― a non―empty adventor field present if 1 (else if 0)		00010000 =  16
//	4	Frm flag ― a non―empty formula field present if 1 (else if 0)		00001000 =   8
//	5	Del flag ― deleted node if 1, existing if 0				00000100 =   4
//	6	Inc flag ― an incoming egde (to the node) present if 1, none if 0	00000010 =   2
//	7	Out flag ― an outgoing egde (from the node) present if 1, none if 0	00000001 =   1
//
//==================================================================================================================================================
//  Annex B. Edge Reference Format
//==================================================================================================================================================
//  Edge reference is 17 bytes record to store linked nodes IDs and also the  type and direction of corresponding edges. Following the idea
//  that edge represents a link or property of some meaning, typisation of edges was introduced to save storage space and to increase the
//  system performance.
//
//  #	Size	Name	Field meaning
//--------------------------------------------------------------------------------------------------------------------------------------------------
//  1.	QWord	ID	The linked (target) node ID reference;
//  2.	Byte	Flg	Edge flags byte. Currently 4 rightmost bits are used:
//			  bit #4 Del: deleted sag if 1, existing if 0;
//			  bit #5 Mnz: manaz ID present if 1, none if 0;
//			  bit #6 ETp: type ID present if 1, none if 0;
//			  bit #7 Dir: edge from the node (i.e. descending) if 1 and edge to the node (i.e. ascending) if 0. Note that for the
//			  ascending edges the next ETp field represents edge type, while for the descending edges ETp field refers to the node
//			  containing the check string in its Frm field, used by angeliofores to define the next move direction;
//  3.	QWord	ETp	Edge type/Manaz is  actually the ID of a node which represents the type, so-called 'property node'.
//
//==================================================================================================================================================
//  Annex C. Raw initial IDS file content
//==================================================================================================================================================
//  Here is the initial 41-bytes IDS file content corresponding to all the abovementioned:
//  #$49 + #$44 + #$53 + #$20 + #$30 + #$2E + #$31 + #$39 + #$04 + #$42 + #$41 + #$26 + #$41 + #$B0 + #$66 + #$C8 + #$16 + #$78 + #$9C + #$63 +
//  #$64 + #$40 + #$05 + #$0A + #$5C + #$0C + #$9E + #$79 + #$69 + #$F9 + #$45 + #$B9 + #$89 + #$25 + #$99 + #$F9 + #$00 + #$18 + #$31 + #$04 +
//  #$44;
//

