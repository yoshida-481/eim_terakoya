set serveroutput on

declare

	--**********
	-- 定数
	--**********
	IS_NOT_MULTIPLE	constant	 number := 0;
	IS_MULTIPLE 	constant	 number := 1;

	--**********
	-- 変数
	--**********
	rscode								number;
	attTypeName							varchar2(255);
	attTypeOtherName					varchar2(255);

	-- ドキュメントオブジェクトタイプID
	objTypeId_Document					number;
	--- スキップステータスタイプID属性ID
	attTypeId_SkipStatus				number;

	appException						exception;

begin

	/* ==================== */
	/* 属性タイプを作成する */
	/* ==================== */

	attTypeName := 'スキップステータスタイプID';
	attTypeOtherName := 'Skip StatusType ID';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_MULTIPLE, attTypeId_SkipStatus, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプを作成できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_SkipStatus, 'JA', attTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_SkipStatus, 'EN', attTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;


	/* ========================================== */
	/* オブジェクトタイプに属性タイプを割り当てる */
	/* ========================================== */

	select id into objTypeId_Document from EIMOBJTYPE where name = 'ドキュメント';
	ObjectAttributeUtils.applyAttributeType(objTypeId_Document, attTypeId_SkipStatus, rscode);
	if rscode <> 0 then
		dbms_output.put_line('ドキュメントオブジェクトタイプに[スキップステータスタイプID]属性タイプを付与できませんでした。');
		raise appException;
	end if;

	commit;
	--rollback;
exception
	when appException then
		dbms_output.put_line('rscode=[ ' || rscode || ' ]');
		rollback;
	when OTHERS then
		dbms_output.put_line('エラー発生: ' || sqlerrm(sqlcode));
		rollback;

end;
/
