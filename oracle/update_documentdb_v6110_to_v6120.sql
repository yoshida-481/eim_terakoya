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
	objTypeId_Workflow_Setting			number;
	
	attTypeName							varchar2(255);
	attTypeOtherName					varchar2(255);
	attTypeId_BossOnlyDefaultFlag				number;
	
	appException						exception;
	
begin
	
	/* ========================================== */
	/* 属性タイプを作成する               */
	/* ========================================== */
	
	attTypeName := '上長のみ表示デフォルト設定ステータス';
	attTypeOtherName := 'Boss only default setting status';
	AttributeUtils.createAttributeType(null, null, attTypeName, 1, IS_MULTIPLE, attTypeId_BossOnlyDefaultFlag, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプを作成できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_BossOnlyDefaultFlag, 'JA', attTypeName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[JA](' || attTypeName || ')を設定できませんでした。');
		raise appException;
	end if;
	AttributeUtils.addOtherAttributeTypeName(attTypeId_BossOnlyDefaultFlag, 'EN', attTypeOtherName, rscode);
	if rscode <> 0 then
		dbms_output.put_line('「' || attTypeName || '」属性タイプの他言語[EN](' || attTypeOtherName || ')を設定できませんでした。');
		raise appException;
	end if;
	
	
	/* ========================================== */
	/*属性タイプを割り当てる */
	/* ========================================== */
	select id into objTypeId_Workflow_Setting from EIMOBJTYPE where name = 'ワークフロー設定';
	ObjectAttributeUtils.applyAttributeType(objTypeId_Workflow_Setting, attTypeId_BossOnlyDefaultFlag, rscode);
	
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
