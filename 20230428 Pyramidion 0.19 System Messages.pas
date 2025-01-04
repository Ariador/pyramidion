	// TAesthesiaForm.FormActivate
	00: '[00] Load knowledge base from IDS file "' + GIDSFile + '".';
	01: '[01] W: IDS file "' + GIDSFile + '" not found. New IDS file created.';
	02: '[02] W: The file "' + GIDSFile + '" is empty. New IDS file created.';
	03: '[03] W: The file "' + GIDSFile + '" is not a valid IDS v.0.19 database.' + ' Renamed to "' + GIDSFile +
		'.old" and new IDS file created.';
	04: '[04] W: CRC32 check failed for the file "' + GIDSFile + '". Renamed to "' + GIDSFile + '.old" and new IDS file created.';
	05: '[05] A: Started with IDS file "' + GIDSFile + '" opened with CRC32 ' + IntToStr(GIDSCRC) + ' and base of ' +
		IntToStr(Length(Nod) - GNodDelCnt) + ' node'+LtrS0+' and ' + IntToStr(GEdgeCnt) + ' edge' + LtrS1 + '.';
	// TAesthesiaForm.CNodIDKeyPress, TAesthesiaForm.CNodIDEdit - function TAngeliophor.AChCurrNode(): integer *
	06: '[06] Q: Change the Current Node.';
	07: '[07] W: Node ' + CurrNode + ' not found. Set to 0.';
	08: '[08] W: Node ' + CurrNode + ' is deleted. Set to 0.';
	09: '[09] A: New Current Node ' + CurrNode + ' "' + GCNme + '".';
	// TAesthesiaForm.CNodAdvEdit
	10: '[0A] Q: Change the Current Node adventor.';
	11: '[0B] A: New adventor for the Current Node ' + CurrNode + ': "' + IntToStr(GCAdv) + '".';
	// TAesthesiaForm.TNodIDKeyPress, TAesthesiaForm.TNodIDEdit - function TAngeliophor.AChTargNode(): integer
	12: '[0C] Q: Change the Target Node.';
	13: '[0D] W: Node ' + TargNode + ' not found. Set to none.';
	14: '[0E] W: Node ' + TargNode + ' "' + GTNme + '" is deleted. Set to none.';
	15: '[0F] E: Cyclic edges are not allowed.';
	17: '[11] A: New Target Node ' + TargNode + ' "' + GTNme + '".';
	// TAesthesiaForm.ETpKeyPress, TAesthesiaForm.TETpEdit	-	function TAngeliophor.AChEType(): integer
	18: '[12] Q: Change edge type.';
	19: '[13] W: Wrong edge type ' + EType + ', set to 0 "' + Nod[0].Nme + '".';
	21: '[15] A: Type of ascending edge from the Current Node ' + CurrNode + ' "' + GCNme + '" to the Target Node ' + TargNode +
		' "' + GTNme + '" set to ' + EType + ' "' + GTpNme + '".';
	22: '[16] A: Type of descending edge from the current Node ' + CurrNode + ' "' + GCNme + '" to the Target Node ' + TargNode +
		' "' + GTNme + '" set to ' + EType + ' "' + GTpNme + '".';
	23: '[17] W: The Current Node ' + CurrNode + ' "' + GCNme + '" has no edge with the Target Node ' + TargNode + ' "' + GTNme +
		'". New edge?';
	// TAesthesiaForm.TNodAdvEdit
	24: '[18] Q: Change the Target Node adventor.';
	25: '[19] A: New adventor for the Target Node ' + TargNode + ': "' + IntToStr(GTAdv) + '".';
	26: '[1A] W: No Target Node specified.';
	// TAesthesiaForm.AddAscEdgBtnClick, TAesthesiaForm.AddDscEdgBtnClick
	27: '[1B] Q: Add ascending edge of type ' + EType + ' "' + GTpNme + '" from the Current Node ' + CurrNode + ' "' + GCNme +
		'" up to the Target Node ' + TargNode + ' "' + GTNme + '".';
	28: '[1C] Q: Add descending edge of type ' + EType + ' "' + GTpNme + '" from the Current Node ' + CurrNode + ' "' + GCNme +
		'" down to the Target Node ' + TargNode + ' "' + GTNme + '".';
	29: '[1D] E: Node 0 cannot have ascending edges. No edge added.';
	30: '[1E] E: Duplicate edges are not allowed. No new edge added.';
	31: '[1F] E: Target Node ' + TargNode + ' doesn''t exist. No edge added.';
	32: '[20] A: New node ' + TargNode + ' "' + GTNme + '" added.';
	33: '[21] A: Ascending edge of type ' + EType + ' "' + GTpNme + '" from the Current Node ' + CurrNode + ' "' + GCNme +
		'" up to the Target Node ' + TargNode + ' "' + GTNme + '" added.';
	34: '[22] A: Descending edge of type ' + EType + ' "' + GTpNme + '" from the Current Node ' + CurrNode + ' "' + GCNme +
		'" down to the Target Node ' + TargNode + ' "' + GTNme + '" added.';
	// TAesthesiaForm.DelCNodBtnClick
	35: '[23] Q: Delete the current node ' + CurrNode + ' "' + GCNme + '".';
	36: '[24] E: Node 0 cannot be deleted. Deletion failed.';
	37: '[25] A: Node deleted. New current node 0 "' + GCNme + '".';
	// TAesthesiaForm.DelCEdgBtnClick
	38: '[26] Q: Delete edge ' + CurrNode + ' "' + GCNme + '" - ' + TargNode + ' "' + GTNme + '".';
	39: '[27] E: There is no current Edge to delete.';
	40: '[29] E: Edge to Node 0 must be the last edge to delete. Deletion failed.';
	// TAesthesiaForm.CommitBtnClick	-	function TAngeliophor.ACommit(): integer
	41: '[29] Q: Commit base and clear the history of changes.';
	42: '[2A] A: Base commited with ' + IntToStr(Length(Nod)) + ' node'+LtrS2+' and ' + IntToStr(GEdgeCnt) + ' edge' + LtrS1 + '. ' +
		'History of changes cleared.';
	// TAesthesiaForm.ExitBtnClick
	43: '[2B] Q: Save database and session log.';
	44: '[2C] A: Database saved to file "' + GIDSFile + '" with CRC32 ' + IntToStr(GIDSCRC) + ' and base of ' +
		IntToStr(Length (Nod) - GNodDelCnt) + ' node'+LtrS0+' and ' + IntToStr(GEdgeCnt) + ' edge'+LtrS1+'.';
	45: '[2D] A: Session log saved to file "Pyramidion 019.log", session closed.' + LineEnding;
	// TAesthesiaForm.UndoBtnClick
	46: '[2E] Q: Undo to the Step ' + IntToStr(GUndo);
	// TAesthesiaForm.RedoBtnClick
	47: '[2F] Q: Redo to the Step ' + IntToStr(GUndo);
	// TAesthesiaForm.FormClose
	48: '[30] A: Session close without saving log and database.';
	// 'Monitor' event
	49: '[31] Q: Monitor';
	50: '[32] A: Monitor query completed.';
	// Processing console query
	51: '[33] A: Processing console query.';
	// Reading Event Strip (Eidetika) from EID file
	52: '[34] W: EID file "' + LeftStr(GIDSFile, Length(GIDSFile) - 3) + 'eid" not found. New EID file created.';
	53: '[35] W: The file "' + LeftStr(GIDSFile, Length(GIDSFile) - 3) + 'eid" is empty. New EID file created.';
	54: '[36] W: The file "' + LeftStr(GIDSFile, Length(GIDSFile) - 3) + 'eid" is not a valid EID v.0.19 file. Renamed to "' +
		LeftStr(GIDSFile, Length(GIDSFile) - 3) + 'eid.old" and new EID file created.';
	55: '[37] W: CRC32 check failed for the file "' + LeftStr(GIDSFile, Length(GIDSFile) - 3) + 'eid". Renamed to "' +
		LeftStr(GIDSFile, Length(GIDSFile) - 3) + 'eid.old" and new EID file created.';
	56: '[38] A: Eidetika EID file "' + LeftStr(GIDSFile, Length(GIDSFile) - 3) + 'eid" opened with CRC32 ' + IntToStr(GEIDCRC) +
		' containing ' + IntToStr(Length(Eid)) + ' record'+LtrS3+'.';
	// Saving Event Strip (Eidetika) to EID file
	57: '[39] A: Events strip saved to file "' + LeftStr(GIDSFile, Length(GIDSFile) - 3) + 'eid" with CRC32 ' + IntToStr(GEIDCRC) +
		' containing ' + IntToStr(Length (Eid)) + ' record'+LtrS3+'.';
	// Editing the current node flags *
	58: '[3A] Q: Edit the current node flags.';
	59: '[3B] A: Current node flags edited to "' + CNodFlg.Text + '".';
	// Editing the target node flags
	60: '[3C] Q: Edit the target node flags.';
	61: '[3D] A: Target node flags edited to "' + TNodFlg.Text + '".';
	// Adding new node and set it as target node
	62: '[3E] Q: Add new unlinked node (and set it as target node).';
	// TAesthesiaForm.CNodFrmEdit
	63: '[3F] Q: Change the Current Node formula.';
	64: '[40] A: New formula for the Current Node ' + TargNode + ': "' + GCFrm + '".';
	// TAesthesiaForm.CNodNmeEdit
	65: '[41] Q: Change the Current Node name.';
	66: '[42] A: New name for the Current Node ' + TargNode + ': "' + GCNme + '".';
	// TAesthesiaForm.ManazEdit
	67: '[43] Q: Change the Current Edge checkstring.';
	68: '[44] A: New checkstring for the Current Edge from the Current Node ' + CNodID.Text + ' "' + CNodNme.Text +
		'" to the Target Node ' + TargNode + ' "' + GTNme + ': "' + IntToStr(GMnID) + '".';
	// TAesthesiaForm.TNodFrmEdit
	69: '[45] Q: Change the Target Node formula.';
	70: '[46] A: New formula for the Target Node ' + TargNode + ': "' + GTFrm + '".';
	// TAesthesiaForm.TNodNmeEdit
	71: '[47] Q: Change the Target Node name.';
	72: '[48] A: New name for the Target Node ' + TargNode + ': "' + GTNme + '".';


