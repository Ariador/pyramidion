
//==============================================================================
//  ROOT OF PYRAMIDION 0.16 EID FILE
//------------------------------------------------------------------------------
//  (c) 2020 Igor Voloshin ivoloshin@hotmail.com
//==============================================================================
//  This is the explanation of the memory strip 'Eidetika' file format
//  for Pyramidion 0.16 Alpha version non-relational database software.
//  Total length of the initial EID file makes 37 bytes (records section
//  compressed). The EID file presence is checked at the Pyramidion program
//  start, and in case of its absence this initial file is generated and
//  written to disk for further use.
//
//------------------------------------------------------------------------------
//  #	Field  Value	Field name	Field meaning
//------------------------------------------------------------------------------
//  1.	QWord	qXX	Version		Version header 'EID 0.16', which
//					corresponds to unsigned 64 bit integer
//					3904953136141584709, written in file in
//					reverse byte order (Intel mode);
//  2.	QWord	qYY	CRC32		Checksum for the remainder of the file,
//					represented as unsigned 64-bit integer,
//					calculated within Ap.AERead function;
//------------------------------------------------------------------------------
//  The remainder of the EID file is compressed using LZW algorithm
//------------------------------------------------------------------------------
//  3.	QWord	x01	RecCnt		Records count, 64 bit - initially 1
//  4.	QWord	xTT	TStmp		Consists of Time:longint and Date:
//					longint parts, calculated within
//					Ap.AERead function;
//  5.	DWord	x00	ChCnt		Counter of events within milliseconds,
//					supported programmatically, because
//					there is no reliable nanoseconds counter
//					in multicore CPUs;
//  6.	Byte	x00	EvTp		Event type (few types are anticipated,
//					so no more than a byte to store type #).
//					Event 0 is, logically, creation of the
//					events strip;
//  7.	QWord	x00	ArgLen		Arguments string length, 0 for event 0;
//  8.	String	''	Args		Arguments of the event (specific to each
//					event type). Event 0 has no arguments.
//

