set serveroutput on;

declare 

	IS_NOT_MULTIPLE	constant	 number := 0;
	IS_MULTIPLE 	constant	 number := 1;
	
	rscode								number;
	found								number;
	
	objId								number;
	objName								varchar2(255);
	
	objTypeName							varchar2(255);
	objTypeOtherName					varchar2(255);
	objTypeId_Document					number;
	
	attTypeName							varchar2(255);
	attTypeOtherName					varchar2(255);
	attTypeId_PDFConversionExecDate		number;
	
	appException						exception;
	
begin
	
	/* ========================================== */
	/* 属性タイプを作成する               */
	/* ========================================== */
	
	attTypeName := 'PDF変換処理実行日時';
	attTypeOtherName := 'PDF Conversion Exec Date';
	AttributeUtils.createAttributeType(null, null, attTypeName, 3, IS_NOT_MULTIPLE, attTypeId_PDFConversionExecDate, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプを作成できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_PDFConversionExecDate, 'JA', attTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_PDFConversionExecDate, 'EN', attTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;
	
	/* ========================================== */
	/*属性タイプを割り当てる */
	/* ========================================== */
	select id into objTypeId_Document from EIMOBJTYPE where name = 'ドキュメント';
	ObjectAttributeUtils.applyAttributeType(objTypeId_Document, attTypeId_PDFConversionExecDate, rscode);
	
	commit;
	
exception
	when appException then
		dbms_output.put_line('rscode=[ ' || rscode || ' ]');
		rollback;
	when OTHERS then
		dbms_output.put_line('エラー発生: ' || sqlerrm(sqlcode));
		rollback;

end;
/
