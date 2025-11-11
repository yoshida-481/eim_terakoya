set serveroutput on

/* ========================================== */
/* 紙文書電子化オプション                     */
/*   追加設定スクリプト                       */
/* ========================================== */

declare

	IS_NOT_MULTIPLE	constant	 number := 0;
	IS_MULTIPLE 	constant	 number := 1;

	rscode								      number;

	objTypeId_Document          number;
	objTypeId_GroupObjectType		number;

	attTypeId							number;
	attTypeName						varchar2(255);
	attTypeOtherName			varchar2(255);

	attTypeId_CoverFlag		      number;
	attTypeId_CoverCreationFlag	number;

	appException						exception;

begin

	/* ========================================== */
	/* 属性タイプを作成する               */
	/* ========================================== */
	attTypeName := 'スキャン用表紙フラグ';
	attTypeOtherName := 'Cover flag for scanning';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_CoverFlag, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプを作成できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_CoverFlag, 'JA', attTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_CoverFlag, 'EN', attTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;

	attTypeName := 'スキャン用表紙作成可否フラグ';
	attTypeOtherName := 'Cover creation permission flag for scanning';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_NOT_MULTIPLE, attTypeId_CoverCreationFlag, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプを作成できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_CoverCreationFlag, 'JA', attTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_CoverCreationFlag, 'EN', attTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;

	/* ========================================== */
	/* オブジェクトタイプに属性タイプを割り当てる */
	/* ========================================== */
	/* ============================ */
	/* Apply Object Type To Document*/
	/* ============================ */
	select id into objTypeId_Document from EIMOBJTYPE where name = 'ドキュメント';
	ObjectAttributeUtils.applyAttributeType(objTypeId_Document, attTypeId_CoverFlag, rscode);

	/* ==================================== */
	/* Apply Object Type To User Type       */
	/* ==================================== */
	select id into objTypeId_GroupObjectType from EIMOBJTYPE where name = 'グループ';
	ObjectAttributeUtils.applyAttributeType(objTypeId_GroupObjectType, attTypeId_CoverCreationFlag, rscode);


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
