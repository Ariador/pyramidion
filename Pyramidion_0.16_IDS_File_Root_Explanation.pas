
//==============================================================================
//  ROOT OF PYRAMIDION 0.16 IDS FILE
//------------------------------------------------------------------------------
//  (c) 2020 Igor Voloshin ivoloshin@hotmail.com
//==============================================================================
//  This is the explanation of the database file format for Pyramidion 0.16
//  Alpha version non-relational database software. Total length of the initial
//  IDS file makes 41 bytes. The IDS file presence is checked at the Pyramidion
//  program start, and in case of its absence this initial 41-byte file is
//  generated and written to disk for further use.
//
//------------------------------------------------------------------------------
//  #	Field Value	Field name	Field meaning
//------------------------------------------------------------------------------
//  1.	QWord qXX	Version		Version header 'IDS 0.16', which
//					corresponds to unsigned 64 bit integer
//					3904953136142566473, written in file in
//					reverse byte order (Intel mode);
//  2.	QWord qYY	CRC32		Checksum for the remainder of the file,
//					represented as unsigned 64-bit integer,
//					initially 18446744072913957889;
//------------------------------------------------------------------------------
//  The remainder of the IDS file is packed using LZW algorithm
//------------------------------------------------------------------------------
//  3.	QWord x01	RecCnt		Records count, 64 bit - initially, only
//					Informatio Node exists;
//  4.	QWord x00	ID		Information Node ID is 0;
//  5.	Byte  x01	Flags		Flags bits initially are b00000001
//					(see Annex A);
// !6.	QWord x00	EdgCnt		No ascending edges for the Informatio
//					Node ever, and no descending Edges
//					so far;
// !6a.	17 Bytes x0	EdgeRef		No list of edges so far (see Annex B
//					for Edge Reference format);
//  7.	QWord x0A	ComLen		Comment length is 10 bytes;
//  8.	'Informatio'	ComText		Comment text is 'Informatio' (Latin for
//					'Information') in Unicode UTF-8 format.
//					The field may contain binary data
//					as well - images, sounds, video,
//					binary code etc.
//------------
//  Note (!):	Fields 6 and 6a are absent in the initial record, as defined by
//		the corresponding bytes of the Flags field (see Annex A).
//
//
//==============================================================================
//  Annex A. Pyramidion Flags Field Format
//==============================================================================
//  Each Node has one-byte Flags field to define its structure and behaviour. Of
//  these 8 bits just rightmost four bits are used so far, as follows:
//
//------------------------------------------------------------------------------
//  Bit No.	Bit Meaning
//------------------------------------------------------------------------------
//	0-2	Reserved
//	3	Del flag: deleted node if 1 (existing if 0)
//	4	Lnk flag: presence of common edge between this node and the
//		current node (used temporarily within Form.Redraw procedure)
//	5	Asc flag: presence of ascending edges for the node (for the 1st
//		'Informatio' node it's always 0)
//	6	Dsc flag: presence of descending edges for the node
//	7	Com flag: presence of the node's comment
//
//
//==============================================================================
//  Annex B. Edge Reference Format
//==============================================================================
//  Ascending and descending edge references are formed of 17 bytes to store
//  linked nodes IDs and also the type and direction of corresponding edges.
//  Following the idea that edge represents a link or property of some meaning,
//  typisation of edges introduced to save storage space and to increase the
//  system performance. Special field is used to implement typisation.
//
//  1. QWord	x00000000	The linked (target) node ID reference;
//
//  2. Byte	x0		Flags byte. Currently 2 rightmost bits are used:
//				byte #6 Del: deleted sag if 1, existing if 0;
//				byte #7 Dir: edge from the node (i.e.
//				descending) if 1, to the node (i.e. ascending)
//				if 0;
//
//  3. QWord	x00000000	Edge type is  actually the ID of a node which
//				represents the type, so-called 'property node'.
//

